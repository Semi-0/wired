import * as THREE from "three";
import { Msg } from "./msg.js";
import { nodeLabel, selectedNode } from "./model.js";

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

const isPropagator = (node) =>
  node.kind === "propagator" ||
  String(node.label || "").startsWith("app:");

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

export const createRenderer = ({ root, selectionEl, dispatch }) => {
  const scene = new THREE.Scene();
  scene.background = new THREE.Color(0x000000);

  const camera = new THREE.PerspectiveCamera(65, 1, 0.01, 1000);

  const renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.xr.enabled = true;
  renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
  root.appendChild(renderer.domElement);

  scene.add(new THREE.AmbientLight(0xffffff, 0.45));
  const keyLight = new THREE.PointLight(0xffffff, 2.4, 80);
  keyLight.position.set(0, 4, 8);
  scene.add(keyLight);
  const group = new THREE.Group();
  scene.add(group);

  const nodeMeshes = new Map();
  const edgeMeshes = new Map();
  const arrowAxis = new THREE.Vector3(0, 1, 0);
  const raycaster = new THREE.Raycaster();
  const pointer = new THREE.Vector2();
  const controls = {
    target: new THREE.Vector3(0, 0, 0),
    radius: 8,
    theta: 0,
    phi: 1.25,
    dragging: false,
    moved: false,
    mode: "rotate",
    lastX: 0,
    lastY: 0,
    userAdjusted: false,
    framedGraphKey: "",
  };

  const updateCamera = () => {
    const sinPhi = Math.sin(controls.phi);
    camera.position.set(
      controls.target.x + controls.radius * sinPhi * Math.sin(controls.theta),
      controls.target.y + controls.radius * Math.cos(controls.phi),
      controls.target.z + controls.radius * sinPhi * Math.cos(controls.theta)
    );
    camera.lookAt(controls.target);
    keyLight.position.copy(camera.position);
  };
  updateCamera();

  const resize = () => {
    const { width, height } = root.getBoundingClientRect();
    renderer.setSize(width, height, false);
    camera.aspect = width / Math.max(height, 1);
    camera.updateProjectionMatrix();
  };
  window.addEventListener("resize", resize);
  resize();

  renderer.domElement.addEventListener("contextmenu", (event) => {
    event.preventDefault();
  });

  renderer.domElement.addEventListener("pointerdown", (event) => {
    controls.dragging = true;
    controls.moved = false;
    controls.mode = event.button === 1 || event.button === 2 || event.shiftKey ? "pan" : "rotate";
    controls.lastX = event.clientX;
    controls.lastY = event.clientY;
    renderer.domElement.setPointerCapture(event.pointerId);
  });

  renderer.domElement.addEventListener("pointermove", (event) => {
    if (!controls.dragging) return;
    const dx = event.clientX - controls.lastX;
    const dy = event.clientY - controls.lastY;
    controls.lastX = event.clientX;
    controls.lastY = event.clientY;
    if (Math.abs(dx) + Math.abs(dy) > 2) {
      controls.moved = true;
      controls.userAdjusted = true;
    }

    if (controls.mode === "pan") {
      const forward = new THREE.Vector3();
      camera.getWorldDirection(forward);
      const right = new THREE.Vector3().crossVectors(forward, camera.up).normalize();
      const up = new THREE.Vector3().crossVectors(right, forward).normalize();
      const scale = controls.radius * 0.0018;
      controls.target.addScaledVector(right, -dx * scale);
      controls.target.addScaledVector(up, dy * scale);
    } else {
      controls.theta -= dx * 0.006;
      controls.phi = Math.max(0.18, Math.min(Math.PI - 0.18, controls.phi + dy * 0.006));
    }
    updateCamera();
  });

  renderer.domElement.addEventListener("pointerup", (event) => {
    controls.dragging = false;
    renderer.domElement.releasePointerCapture(event.pointerId);
  });

  renderer.domElement.addEventListener("wheel", (event) => {
    event.preventDefault();
    controls.userAdjusted = true;
    controls.radius = Math.max(1.4, Math.min(60, controls.radius * Math.exp(event.deltaY * 0.001)));
    updateCamera();
  }, { passive: false });

  const graphKey = (model) =>
    `${model.graph.nodes.map((node) => node.id).sort().join("|")}::${model.graph.edges.length}`;

  const frameGraph = (model) => {
    const key = graphKey(model);
    if (controls.userAdjusted || controls.framedGraphKey === key || model.graph.nodes.length === 0) {
      return;
    }
    const points = model.graph.nodes
      .map((node) => model.layout[node.id])
      .filter(Boolean);
    if (points.length === 0) return;

    const min = new THREE.Vector3(Infinity, Infinity, Infinity);
    const max = new THREE.Vector3(-Infinity, -Infinity, -Infinity);
    for (const p of points) {
      min.min(new THREE.Vector3(p.x, p.y, p.z));
      max.max(new THREE.Vector3(p.x, p.y, p.z));
    }
    const center = new THREE.Vector3().addVectors(min, max).multiplyScalar(0.5);
    const size = new THREE.Vector3().subVectors(max, min);
    const radius = Math.max(size.length() * 1.05, 4.5);

    controls.target.copy(center);
    controls.radius = Math.min(60, Math.max(3.8, radius));
    controls.theta = 0.42;
    controls.phi = 1.18;
    controls.framedGraphKey = key;
    updateCamera();
  };

  const nearestMeshId = (event) => {
    const rect = renderer.domElement.getBoundingClientRect();
    pointer.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    pointer.y = -(((event.clientY - rect.top) / rect.height) * 2 - 1);
    raycaster.setFromCamera(pointer, camera);
    const hits = raycaster.intersectObjects([...nodeMeshes.values()], false);
    return hits[0]?.object?.userData?.nodeId || null;
  };

  renderer.domElement.addEventListener("click", (event) => {
    if (controls.moved) return;
    const id = nearestMeshId(event);
    if (id) dispatch(Msg.SelectNode(id));
  });

  const ensureNode = (node) => {
    if (nodeMeshes.has(node.id)) return nodeMeshes.get(node.id);
    const geometry = isPropagator(node)
      ? new THREE.ConeGeometry(0.22, 0.38, 3)
      : new THREE.SphereGeometry(0.15, 24, 24);
    const mesh = new THREE.Mesh(
      geometry,
      makeMaterial()
    );
    mesh.userData.nodeId = node.id;
    mesh.userData.kind = isPropagator(node) ? "propagator" : "cell";
    const label = makeLabel(nodeLabel(node));
    label.position.set(0, isPropagator(node) ? 0.38 : 0.32, 0);
    if (!isPropagator(node)) {
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

  const render = (model) => {
    prune(model);
    frameGraph(model);
    for (const node of model.graph.nodes) {
      const mesh = ensureNode(node);
      const p = model.layout[node.id] || { x: 0, y: 0, z: 0 };
      mesh.position.set(p.x, p.y, p.z);
      const selected = node.id === model.selectedId;
      const pulse = Math.min(1, Math.max(0, (model.pulses?.[node.id] || 0) / 0.9));
      const bloom = pulse * pulse;
      mesh.scale.setScalar((selected ? 1.38 : 1) + bloom * 0.72);
      mesh.material.color.set(0xffffff);
      mesh.material.emissive.set(0xffffff);
      mesh.material.emissiveIntensity = (selected ? 1.35 : 0.72) + bloom * 1.6;
      const halo = mesh.getObjectByName("pulse-halo");
      if (halo) {
        halo.visible = bloom > 0.01;
        halo.material.opacity = bloom * 0.56;
        halo.scale.setScalar(1 + bloom * 1.3);
      }
      if (mesh.userData.kind === "propagator") {
        mesh.rotation.y += 0.012;
      }
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

    const selected = selectedNode(model);
    selectionEl.textContent = selected
      ? JSON.stringify(selected, null, 2)
      : "no selection";

    renderer.render(scene, camera);
  };

  const detectPinch = (frame, referenceSpace) => {
    if (!frame || !referenceSpace) return null;
    for (const source of renderer.xr.getSession()?.inputSources || []) {
      const hand = source.hand;
      if (!hand) continue;
      const thumb = hand.get("thumb-tip");
      const index = hand.get("index-finger-tip");
      const thumbPose = thumb && frame.getJointPose(thumb, referenceSpace);
      const indexPose = index && frame.getJointPose(index, referenceSpace);
      if (!thumbPose || !indexPose) continue;
      const a = thumbPose.transform.position;
      const b = indexPose.transform.position;
      const dx = a.x - b.x;
      const dy = a.y - b.y;
      const dz = a.z - b.z;
      if (Math.sqrt(dx * dx + dy * dy + dz * dz) < 0.035) {
        return { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2, z: (a.z + b.z) / 2 };
      }
    }
    return null;
  };

  return {
    renderer,
    camera,
    render,
    detectPinch,
    dispose: () => window.removeEventListener("resize", resize),
  };
};
