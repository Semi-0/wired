import * as THREE from "three";
import { nodeLabel } from "./model.js";
import { channelValueAt, isWidget, widgetChannels } from "./widgets.js";

export const isPropagator = (node) =>
  node.kind === "propagator" ||
  String(node.label || "").startsWith("app:");

const makeLabel = (text) => {
  const canvas = document.createElement("canvas");
  canvas.width = 256;
  canvas.height = 64;
  const ctx = canvas.getContext("2d");
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = "#ffffff";
  ctx.font = "24px sans-serif";
  ctx.fillText(String(text).slice(0, 26), 12, 40);
  const texture = new THREE.CanvasTexture(canvas);
  const sprite = new THREE.Sprite(
    new THREE.SpriteMaterial({ map: texture, transparent: true })
  );
  sprite.scale.set(1.4, 0.35, 1);
  return sprite;
};

const makeMaterial = () =>
  new THREE.MeshStandardMaterial({
    color: 0xffffff,
    emissive: 0xffffff,
    emissiveIntensity: 0.65,
    roughness: 0.28,
    metalness: 0.05,
  });

const makeHalo = () =>
  new THREE.Mesh(
    new THREE.SphereGeometry(0.28, 32, 32),
    new THREE.MeshBasicMaterial({
      color: 0xffffff,
      transparent: true,
      opacity: 0,
      depthWrite: false,
      blending: THREE.AdditiveBlending,
    })
  );

const makeEdgeMaterial = () =>
  new THREE.LineBasicMaterial({ color: 0xffffff, transparent: true, opacity: 0.62 });

const makeArrowMaterial = () =>
  new THREE.MeshBasicMaterial({ color: 0xffffff, transparent: true, opacity: 0.86 });

export const createGraphView = ({ group }) => {
  const nodeMeshes = new Map();
  const edgeMeshes = new Map();
  const arrowAxis = new THREE.Vector3(0, 1, 0);

  const ensureNode = (node) => {
    if (nodeMeshes.has(node.id)) return nodeMeshes.get(node.id);
    const geometry = isWidget(node)
      ? new THREE.BoxGeometry(0.95, 0.32, 0.08)
      : isPropagator(node)
      ? new THREE.ConeGeometry(0.22, 0.38, 3)
      : new THREE.SphereGeometry(0.15, 24, 24);
    const mesh = new THREE.Mesh(
      geometry,
      isWidget(node)
        ? new THREE.MeshBasicMaterial({ color: 0x050505 })
        : makeMaterial()
    );
    mesh.userData.nodeId = node.id;
    mesh.userData.kind = isWidget(node) ? "widget" : isPropagator(node) ? "propagator" : "cell";
    const label = makeLabel(nodeLabel(node));
    label.position.set(0, isPropagator(node) ? 0.38 : 0.42, 0);
    if (isWidget(node)) {
      const outline = new THREE.LineSegments(
        new THREE.EdgesGeometry(geometry),
        new THREE.LineBasicMaterial({ color: 0xffffff })
      );
      mesh.add(outline);
      const channels = widgetChannels(node);
      const rowHeight = 0.18;
      const top = ((channels.length - 1) * rowHeight) / 2;
      channels.forEach((channel, index) => {
        const y = top - index * rowHeight;
        const track = new THREE.Mesh(
          new THREE.BoxGeometry(0.74, 0.018, 0.018),
          new THREE.MeshBasicMaterial({ color: 0xffffff })
        );
        track.name = `slider-track-${index}`;
        track.position.set(0, y, 0.075);
        const knob = new THREE.Mesh(
          new THREE.SphereGeometry(0.055, 18, 18),
          new THREE.MeshBasicMaterial({ color: 0xffffff })
        );
        knob.name = `slider-knob-${index}`;
        knob.position.set(-0.37, y, 0.13);
        const channelLabel = makeLabel(String(channel.channel || "value"));
        channelLabel.name = `slider-label-${index}`;
        channelLabel.position.set(-0.52, y - 0.01, 0.13);
        channelLabel.scale.setScalar(0.62);
        mesh.add(track);
        mesh.add(knob);
        mesh.add(channelLabel);
      });
    } else if (!isPropagator(node)) {
      const halo = makeHalo();
      halo.name = "pulse-halo";
      mesh.add(halo);
    }
    mesh.add(label);
    group.add(mesh);
    nodeMeshes.set(node.id, mesh);
    return mesh;
  };

  const ensureEdge = (edge) => {
    const id = `${edge.from}->${edge.to}`;
    if (edgeMeshes.has(id)) return edgeMeshes.get(id);
    const edgeGroup = new THREE.Group();
    const geometry = new THREE.BufferGeometry();
    geometry.setAttribute("position", new THREE.Float32BufferAttribute([0, 0, 0, 0, 0, 0], 3));
    const line = new THREE.Line(
      geometry,
      makeEdgeMaterial()
    );
    line.name = "edge-line";
    edgeGroup.add(line);

    const arrow = new THREE.Mesh(
      new THREE.ConeGeometry(0.07, 0.22, 16),
      makeArrowMaterial()
    );
    arrow.name = "edge-arrow";
    edgeGroup.add(arrow);

    group.add(edgeGroup);
    edgeMeshes.set(id, edgeGroup);
    return edgeGroup;
  };

  const prune = (model) => {
    const nodeIds = new Set(model.graph.nodes.map((node) => node.id));
    for (const [id, mesh] of nodeMeshes) {
      if (!nodeIds.has(id)) {
        group.remove(mesh);
        nodeMeshes.delete(id);
      }
    }
    const edgeIds = new Set(model.graph.edges.map((edge) => `${edge.from}->${edge.to}`));
    for (const [id, edgeGroup] of edgeMeshes) {
      if (!edgeIds.has(id)) {
        group.remove(edgeGroup);
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
      mesh.scale.setScalar((selected ? 1.38 : 1) + bloom * 0.72);
      if (mesh.userData.kind === "widget") {
        mesh.material.color.set(0x050505);
      } else {
        mesh.material.color.set(0xffffff);
        mesh.material.emissive.set(0xffffff);
        mesh.material.emissiveIntensity = (selected ? 1.35 : 0.72) + bloom * 1.6;
      }
      const halo = mesh.getObjectByName("pulse-halo");
      if (halo) {
        halo.visible = bloom > 0.01;
        halo.material.opacity = bloom * 0.56;
        halo.scale.setScalar(1 + bloom * 1.3);
      }
      if (mesh.userData.kind === "propagator") {
        mesh.rotation.y += 0.012;
      }
      widgetChannels(node).forEach((_, index) => {
        const knob = mesh.getObjectByName(`slider-knob-${index}`);
        if (knob) {
          knob.position.x = -0.37 + (channelValueAt(node, index) / 100) * 0.74;
        }
      });
    }

    for (const edge of model.graph.edges) {
      const from = model.layout[edge.from];
      const to = model.layout[edge.to];
      if (!from || !to) continue;
      const edgeGroup = ensureEdge(edge);
      const line = edgeGroup.getObjectByName("edge-line");
      const arrow = edgeGroup.getObjectByName("edge-arrow");
      const direction = new THREE.Vector3(to.x - from.x, to.y - from.y, to.z - from.z);
      const length = direction.length();
      if (length < 0.001) continue;
      direction.normalize();
      const end = new THREE.Vector3(to.x, to.y, to.z).addScaledVector(direction, -0.18);
      const pos = line.geometry.attributes.position;
      pos.setXYZ(0, from.x, from.y, from.z);
      pos.setXYZ(1, end.x, end.y, end.z);
      pos.needsUpdate = true;
      line.geometry.computeBoundingSphere();
      arrow.position.copy(end);
      arrow.quaternion.setFromUnitVectors(arrowAxis, direction);
    }
  };

  return {
    nodeMeshes,
    renderGraph,
  };
};
