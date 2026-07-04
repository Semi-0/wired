import * as THREE from "three";
import { createGraphView } from "./graph-view.js";
import { Msg } from "./msg.js";
import { selectedNode } from "./model.js";
import { createSceneKit } from "./scene.js";
import { widgetChannels, widgetId, isWidget } from "./widgets.js";
import { detectPinch as detectXrPinch } from "./xr-input.js";

export const createRenderer = ({ root, selectionEl, dispatch }) => {
  const { scene, camera, renderer, group, controls, resize, updateLight } =
    createSceneKit({ root });
  const graphView = createGraphView({ group });
  const raycaster = new THREE.Raycaster();
  const pointer = new THREE.Vector2();
  const interaction = {
    moved: false,
    userAdjusted: false,
    framedGraphKey: "",
    widgetDrag: null,
    widgetChannel: null,
    pointerId: null,
  };
  let lastModel = null;

  renderer.domElement.addEventListener("contextmenu", (event) => {
    event.preventDefault();
  });

  const nearestHit = (event) => {
    const rect = renderer.domElement.getBoundingClientRect();
    pointer.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    pointer.y = -(((event.clientY - rect.top) / rect.height) * 2 - 1);
    raycaster.setFromCamera(pointer, camera);
    const hits = raycaster.intersectObjects([...graphView.nodeMeshes.values()], false);
    return hits[0] || null;
  };

  const widgetInputValue = (node, event) => {
    const hit = nearestHit(event);
    if (hit?.object?.userData?.nodeId === node?.id) {
      const local = hit.object.worldToLocal(hit.point.clone());
      const x = Math.max(0, Math.min(1, (local.x + 0.37) / 0.74));
      return Math.round(x * 100);
    }
    const rect = renderer.domElement.getBoundingClientRect();
    const x = Math.max(0, Math.min(1, (event.clientX - rect.left) / Math.max(rect.width, 1)));
    return Math.round(x * 100);
  };

  const widgetChannelForEvent = (node, event) => {
    const channels = widgetChannels(node);
    const hit = nearestHit(event);
    if (!hit?.object || channels.length <= 1) return channels[0];
    const local = hit.object.worldToLocal(hit.point.clone());
    const rowHeight = 0.18;
    const top = ((channels.length - 1) * rowHeight) / 2;
    const index = Math.max(0, Math.min(channels.length - 1, Math.round((top - local.y) / rowHeight)));
    return channels[index];
  };

  const sendWidgetInput = (node, event, channelName = null) => {
    const id = widgetId(node);
    const channel = channelName || widgetChannelForEvent(node, event)?.channel || "value";
    if (id) {
      dispatch(Msg.WidgetInput(id, channel, widgetInputValue(node, event)));
    }
  };

  renderer.domElement.addEventListener("pointerdown", (event) => {
    const mesh = nearestHit(event)?.object;
    const node = lastModel?.graph?.nodes?.find((node) => node.id === mesh?.userData?.nodeId);
    const cameraGesture = event.button === 1 || event.button === 2 || event.shiftKey;
    if (isWidget(node) && !cameraGesture) {
      event.preventDefault();
      const channel = widgetChannelForEvent(node, event)?.channel || "value";
      interaction.moved = true;
      interaction.widgetDrag = node.id;
      interaction.widgetChannel = channel;
      interaction.pointerId = event.pointerId;
      controls.enabled = false;
      renderer.domElement.setPointerCapture?.(event.pointerId);
      dispatch(Msg.SelectNode(node.id));
      sendWidgetInput(node, event, channel);
    }
  });

  const movePointerDrag = (event) => {
    if (!interaction.widgetDrag) return;
    if (interaction.pointerId !== null && event.pointerId !== interaction.pointerId) return;
    event.preventDefault();
    const node = lastModel?.graph?.nodes?.find((node) => node.id === interaction.widgetDrag);
    sendWidgetInput(node, event, interaction.widgetChannel);
  };

  const stopPointerDrag = (event) => {
    if (interaction.pointerId !== null && event.pointerId !== interaction.pointerId) return;
    interaction.widgetDrag = null;
    interaction.widgetChannel = null;
    interaction.pointerId = null;
    controls.enabled = true;
    if (renderer.domElement.hasPointerCapture?.(event.pointerId)) {
      renderer.domElement.releasePointerCapture(event.pointerId);
    }
  };

  renderer.domElement.addEventListener("pointermove", movePointerDrag);
  window.addEventListener("pointermove", movePointerDrag);
  window.addEventListener("pointerup", stopPointerDrag);
  window.addEventListener("pointercancel", stopPointerDrag);

  controls.addEventListener("start", () => {
    interaction.moved = false;
  });
  controls.addEventListener("change", () => {
    interaction.moved = true;
    interaction.userAdjusted = true;
    updateLight();
  });

  const graphKey = (model) =>
    `${model.graph.nodes.map((node) => node.id).sort().join("|")}::${model.graph.edges.length}`;

  const frameGraph = (model) => {
    const key = graphKey(model);
    if (interaction.userAdjusted || interaction.framedGraphKey === key || model.graph.nodes.length === 0) {
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
    camera.position.set(center.x + radius * 0.42,
                        center.y + radius * 0.32,
                        center.z + radius);
    interaction.framedGraphKey = key;
    controls.update();
    updateLight();
  };

  renderer.domElement.addEventListener("click", (event) => {
    if (interaction.moved) return;
    const id = nearestHit(event)?.object?.userData?.nodeId || null;
    if (id) dispatch(Msg.SelectNode(id));
  });

  const render = (model) => {
    lastModel = model;
    frameGraph(model);
    graphView.renderGraph(model);

    const selected = selectedNode(model);
    selectionEl.textContent = selected
      ? JSON.stringify(selected, null, 2)
      : "no selection";

    controls.update();
    updateLight();
    renderer.render(scene, camera);
  };

  return {
    renderer,
    camera,
    render,
    detectPinch: (frame, referenceSpace) =>
      detectXrPinch(renderer, frame, referenceSpace),
    dispose: () => {
      window.removeEventListener("resize", resize);
      window.removeEventListener("pointermove", movePointerDrag);
      window.removeEventListener("pointerup", stopPointerDrag);
      window.removeEventListener("pointercancel", stopPointerDrag);
      controls.dispose();
    },
  };
};
