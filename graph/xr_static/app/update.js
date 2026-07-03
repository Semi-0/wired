import { batch, none, runtimeCommand } from "./combinators.js";
import { enterXrEffect } from "./effects.js";
import { reconcileLayout, stepForceLayout } from "./model.js";

const graphFromPayload = (payload) => {
  if (payload?.result?.graph) return payload.result.graph;
  if (payload?.graph) return payload.graph;
  return null;
};

const nodeFingerprint = (node) =>
  JSON.stringify({ value: node.value || null, label: node.label || "" });

const cellPulses = (model, graph) => {
  const previous = Object.fromEntries(
    model.graph.nodes.map((node) => [node.id, nodeFingerprint(node)])
  );
  return Object.fromEntries(
    graph.nodes
      .filter((node) => node.kind === "cell")
      .filter((node) => previous[node.id] !== nodeFingerprint(node))
      .map((node) => [node.id, 0.9])
  );
};

const nearestNode = (model, point) => {
  if (!point) return null;
  let best = null;
  let bestD2 = Infinity;
  for (const node of model.graph.nodes) {
    const p = model.layout[node.id];
    if (!p) continue;
    const dx = p.x - point.x;
    const dy = p.y - point.y;
    const dz = p.z - point.z;
    const d2 = dx * dx + dy * dy + dz * dz;
    if (d2 < bestD2) {
      bestD2 = d2;
      best = node.id;
    }
  }
  return bestD2 < 1.2 ? best : null;
};

const targetForNode = (node) => {
  const alias = Array.isArray(node.aliases) ? node.aliases[0] : null;
  if (alias) return { "cell-id": alias };
  if (node.label) return { label: node.label };
  return { "cell-id": node.id };
};

export const update = (model, msg) => {
  switch (msg.type) {
    case "socket/open":
      return [{ ...model, connected: true, status: "connected" }, none()];

    case "socket/closed":
      return [{ ...model, connected: false, status: "disconnected" }, none()];

    case "socket/error":
      return [{ ...model, status: `socket error: ${msg.error || "unknown"}` }, none()];

    case "socket/message": {
      if (msg.payload?.ok === false) {
        return [{ ...model, status: msg.payload.error || "runtime error" }, none()];
      }
      const graph = graphFromPayload(msg.payload);
      if (!graph) return [model, none()];
      const nodeCount = graph.nodes?.length || 0;
      const edgeCount = graph.edges?.length || 0;
      return [
        {
          ...model,
          graph,
          layout: reconcileLayout(model.layout, graph),
          pulses: { ...model.pulses, ...cellPulses(model, graph) },
          status: `graph ${nodeCount} nodes / ${edgeCount} edges`,
        },
        none(),
      ];
    }

    case "trace/install":
      return [
        { ...model, status: `installing trace ${msg.label}` },
        [runtimeCommand("xr/trace/install", { label: msg.label, direction: "upstream" })],
      ];

    case "graph/extend":
      return [
        { ...model, status: "extending graph" },
        [runtimeCommand("xr/extend-graph", { source: msg.source })],
      ];

    case "cell/send-value": {
      const node = selectedNode(model);
      if (!node) return [{ ...model, status: "select a node first" }, none()];
      return [
        { ...model, status: `sending value to ${node.label || node.id}` },
        [
          runtimeCommand("xr/send-message", {
            target: targetForNode(node),
            message: { kind: "value", value: msg.value },
          }),
        ],
      ];
    }

    case "view/mode":
      if (msg.mode === "xr") {
        if (!model.xr.supported) {
          return [{ ...model, viewMode: "3d", status: "WebXR hardware unavailable" }, none()];
        }
        return [{ ...model, viewMode: "xr", status: "entering XR" }, [enterXrEffect()]];
      }
      return [{ ...model, viewMode: "3d", status: "3D view" }, none()];

    case "tick":
      return [stepForceLayout(model, msg.dt), none()];

    case "select/node":
      return [{ ...model, selectedId: msg.id }, none()];

    case "xr/pinch": {
      const id = nearestNode(model, msg.point);
      return [
        {
          ...model,
          selectedId: id || model.selectedId,
          xr: { ...model.xr, lastPinchAt: msg.at || model.xr.lastPinchAt },
        },
        none(),
      ];
    }

    case "xr/ready":
      return [{ ...model, xr: { ...model.xr, supported: msg.ready } }, none()];

    case "xr/enter":
      if (!model.xr.supported) {
        return [{ ...model, status: "WebXR hardware unavailable" }, none()];
      }
      return [{ ...model, viewMode: "xr", status: "entering XR" }, [enterXrEffect()]];

    case "xr/entered":
      return [
        { ...model, viewMode: "xr", xr: { ...model.xr, active: true }, status: "XR active" },
        none(),
      ];

    case "xr/error":
      return [
        { ...model, viewMode: "3d", status: `XR unavailable: ${msg.error || "unknown"}` },
        none(),
      ];

    default:
      return [model, batch()];
  }
};
