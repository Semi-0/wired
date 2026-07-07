import { nodeLabel } from "./model.js";
import { channelValueAt, isWidget, widgetChannels } from "./widgets.js";

export const isPropagator = (node) =>
  node.kind === "propagator" ||
  String(node.label || "").startsWith("app:");

const material = (BABYLON, scene, name, color, emissive = color) => {
  const mat = new BABYLON.StandardMaterial(name, scene);
  mat.diffuseColor = BABYLON.Color3.FromHexString(color);
  mat.emissiveColor = BABYLON.Color3.FromHexString(emissive);
  mat.specularColor = new BABYLON.Color3(0.15, 0.15, 0.15);
  return mat;
};

const labelPlane = (BABYLON, scene, text, name) => {
  const texture = new BABYLON.DynamicTexture(`${name}-texture`, { width: 256, height: 64 }, scene);
  texture.hasAlpha = true;
  const ctx = texture.getContext();
  ctx.clearRect(0, 0, 256, 64);
  ctx.fillStyle = "#ffffff";
  ctx.font = "24px sans-serif";
  ctx.fillText(String(text).slice(0, 26), 12, 40);
  texture.update();

  const plane = BABYLON.MeshBuilder.CreatePlane(name, { width: 1.4, height: 0.35 }, scene);
  const mat = new BABYLON.StandardMaterial(`${name}-material`, scene);
  mat.diffuseTexture = texture;
  mat.emissiveTexture = texture;
  mat.opacityTexture = texture;
  mat.disableLighting = true;
  plane.material = mat;
  plane.billboardMode = BABYLON.Mesh.BILLBOARDMODE_ALL;
  return plane;
};

const pulseHalo = (BABYLON, scene, name) => {
  const halo = BABYLON.MeshBuilder.CreateSphere(name, { diameter: 0.56, segments: 24 }, scene);
  const mat = new BABYLON.StandardMaterial(`${name}-material`, scene);
  mat.diffuseColor = BABYLON.Color3.White();
  mat.emissiveColor = BABYLON.Color3.White();
  mat.alpha = 0;
  mat.disableLighting = true;
  halo.material = mat;
  halo.isPickable = false;
  halo.setEnabled(false);
  return halo;
};

const nodeKind = (node) =>
  isWidget(node) ? "widget" : isPropagator(node) ? "propagator" : "cell";

export const createBabylonGraphView = ({ BABYLON, scene }) => {
  const nodeMeshes = new Map();
  const edgeMeshes = new Map();
  const white = material(BABYLON, scene, "node-white", "#ffffff");
  const widgetMat = material(BABYLON, scene, "widget-dark", "#050505", "#000000");
  const edgeMat = material(BABYLON, scene, "edge-white", "#ffffff");

  const ensureNode = (node) => {
    if (nodeMeshes.has(node.id)) return nodeMeshes.get(node.id);
    const kind = nodeKind(node);
    const mesh = kind === "widget"
      ? BABYLON.MeshBuilder.CreateBox(node.id, { width: 0.95, height: 0.32, depth: 0.08 }, scene)
      : kind === "propagator"
      ? BABYLON.MeshBuilder.CreateCylinder(node.id, {
          diameterTop: 0,
          diameterBottom: 0.38,
          height: 0.42,
          tessellation: 3,
        }, scene)
      : BABYLON.MeshBuilder.CreateSphere(node.id, { diameter: 0.3, segments: 24 }, scene);
    mesh.material = kind === "widget" ? widgetMat : white;
    mesh.metadata = { nodeId: node.id, kind };

    const label = labelPlane(BABYLON, scene, nodeLabel(node), `${node.id}-label`);
    label.parent = mesh;
    label.position.y = kind === "propagator" ? 0.38 : 0.42;

    if (kind === "widget") {
      widgetChannels(node).forEach((channel, index) => {
        const rowHeight = 0.18;
        const top = ((widgetChannels(node).length - 1) * rowHeight) / 2;
        const y = top - index * rowHeight;
        const track = BABYLON.MeshBuilder.CreateBox(
          `${node.id}-track-${index}`,
          { width: 0.74, height: 0.018, depth: 0.018 },
          scene
        );
        track.parent = mesh;
        track.position.set(0, y, 0.075);
        track.material = white;
        const knob = BABYLON.MeshBuilder.CreateSphere(
          `${node.id}-knob-${index}`,
          { diameter: 0.11, segments: 12 },
          scene
        );
        knob.parent = mesh;
        knob.position.set(-0.37, y, 0.13);
        knob.material = white;
        knob.metadata = { nodeId: node.id, kind: "widget-knob", channel: channel.channel };
      });
    } else if (kind === "cell") {
      const halo = pulseHalo(BABYLON, scene, `${node.id}-pulse-halo`);
      halo.parent = mesh;
    }

    nodeMeshes.set(node.id, mesh);
    return mesh;
  };

  const ensureEdge = (edge) => {
    const id = `${edge.from}->${edge.to}`;
    if (edgeMeshes.has(id)) return edgeMeshes.get(id);
    const line = BABYLON.MeshBuilder.CreateLines(id, {
      points: [BABYLON.Vector3.Zero(), BABYLON.Vector3.Zero()],
      updatable: true,
    }, scene);
    line.color = BABYLON.Color3.White();
    const arrow = BABYLON.MeshBuilder.CreateCylinder(`${id}-arrow`, {
      diameterTop: 0,
      diameterBottom: 0.14,
      height: 0.22,
      tessellation: 16,
    }, scene);
    arrow.material = edgeMat;
    edgeMeshes.set(id, { line, arrow });
    return edgeMeshes.get(id);
  };

  const prune = (model) => {
    const nodeIds = new Set(model.graph.nodes.map((node) => node.id));
    for (const [id, mesh] of nodeMeshes) {
      if (!nodeIds.has(id)) {
        mesh.dispose(false, false);
        nodeMeshes.delete(id);
      }
    }
    const edgeIds = new Set(model.graph.edges.map((edge) => `${edge.from}->${edge.to}`));
    for (const [id, edge] of edgeMeshes) {
      if (!edgeIds.has(id)) {
        edge.line.dispose();
        edge.arrow.dispose();
        edgeMeshes.delete(id);
      }
    }
  };

  const renderGraph = (model) => {
    prune(model);
    for (const node of model.graph.nodes) {
      const mesh = ensureNode(node);
      const p = model.layout[node.id] || { x: 0, y: 0, z: 0 };
      mesh.position.set(p.x, p.y, p.z);
      const selected = node.id === model.selectedId;
      const pulse = Math.min(1, Math.max(0, (model.pulses?.[node.id] || 0) / 0.9));
      const bloom = pulse * pulse;
      const scale = selected ? 1.38 : 1;
      mesh.scaling.set(scale, scale, scale);
      const halo = scene.getMeshByName(`${node.id}-pulse-halo`);
      if (halo) {
        halo.setEnabled(bloom > 0.01);
        halo.material.alpha = bloom * 0.56;
        halo.scaling.set(1 + bloom * 1.3, 1 + bloom * 1.3, 1 + bloom * 1.3);
      }
      if (mesh.metadata.kind === "propagator") mesh.rotation.y += 0.012;
      widgetChannels(node).forEach((_, index) => {
        const knob = scene.getMeshByName(`${node.id}-knob-${index}`);
        if (knob) knob.position.x = -0.37 + (channelValueAt(node, index) / 100) * 0.74;
      });
    }

    for (const edge of model.graph.edges) {
      const from = model.layout[edge.from];
      const to = model.layout[edge.to];
      if (!from || !to) continue;
      const start = new BABYLON.Vector3(from.x, from.y, from.z);
      const end = new BABYLON.Vector3(to.x, to.y, to.z);
      const direction = end.subtract(start);
      if (direction.length() < 0.001) continue;
      const tip = end.subtract(direction.normalize().scale(0.18));
      const { line, arrow } = ensureEdge(edge);
      BABYLON.MeshBuilder.CreateLines(null, { points: [start, tip], instance: line });
      arrow.position.copyFrom(tip);
      if (BABYLON.Quaternion.FromUnitVectorsToRef) {
        arrow.rotationQuaternion = BABYLON.Quaternion.FromUnitVectorsToRef(
          BABYLON.Axis.Y,
          direction.normalize(),
          arrow.rotationQuaternion || new BABYLON.Quaternion()
        );
      } else {
        arrow.lookAt(end);
      }
    }
  };

  return {
    nodeMeshes,
    renderGraph,
  };
};
