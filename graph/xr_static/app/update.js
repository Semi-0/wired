import { batch, none, runtimeCommand } from "./combinators.js";
import { enterXrEffect } from "./effects.js";
import { graphWithWidgets, reconcileLayout, stepForceLayout, widgetsFromGraph } from "./model.js";

const graphFromPayload = (payload) => {
  if (payload?.result?.graph) return payload.result.graph;
  if (payload?.graph) return payload.graph;
  return null;
};

const widgetId = (ui) => ui?.widgetId || ui?.["widget-id"] || ui?.id;

const setWidgetChannelValue = (widgets, widgetId, channelName, value) =>
  Object.fromEntries(
    Object.entries(widgets || {}).map(([id, widget]) => [
      id,
      id === widgetId
        ? {
            ...widget,
            channels: (widget.channels || []).map((channel) =>
              channel.channel === channelName
                ? { ...channel, current: value }
                : channel
            ),
          }
        : widget,
    ])
  );

const mergeWidgets = (existing, incoming) => {
  const merged = { ...(existing || {}) };
  for (const [id, widget] of Object.entries(incoming || {})) {
    const previous = merged[id];
    const previousByChannel = Object.fromEntries(
      (previous?.channels || []).map((channel) => [channel.channel, channel])
    );
    merged[id] = {
      ...previous,
      ...widget,
      channels: (widget.channels || []).map((channel) => {
        const previousChannel = previousByChannel[channel.channel];
        return {
          ...previousChannel,
          ...channel,
          current:
            previousChannel?.current === null || previousChannel?.current === undefined
              ? channel.current
              : previousChannel.current,
        };
      }),
    };
  }
  return merged;
};

const graphWithWidgetValue = (graph, widgetId, channelName, value) => ({
  ...graph,
  nodes: (graph.nodes || []).map((node) => {
    const id = widgetIdOfNode(node);
    if (id !== widgetId) return node;
    return {
      ...node,
      ui: {
        ...node.ui,
        channels: (node.ui?.channels || []).map((channel) =>
          channel.channel === channelName
            ? { ...channel, current: value }
            : channel
        ),
      },
    };
  }),
});

const widgetIdOfNode = (node) => widgetId(node?.ui);

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

const explicitCellPulses = (graph) => {
  const ids = graph?.["changed-node-ids"] || graph?.changedNodeIds || [];
  return Object.fromEntries(ids.map((id) => [id, 0.9]));
};

const graphPulses = (model, graph) => {
  const explicit = explicitCellPulses(graph);
  return Object.keys(explicit).length > 0 ? explicit : cellPulses(model, graph);
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
      const incomingWidgets = widgetsFromGraph(graph);
      const widgets = mergeWidgets(model.widgets, incomingWidgets);
      const graphWithRegisteredWidgets = graphWithWidgets(graph, widgets);
      const nodeCount = graphWithRegisteredWidgets.nodes?.length || 0;
      const edgeCount = graphWithRegisteredWidgets.edges?.length || 0;
      return [
        {
          ...model,
          graph: graphWithRegisteredWidgets,
          widgets,
          layout: reconcileLayout(model.layout, graphWithRegisteredWidgets),
          pulses: { ...model.pulses, ...graphPulses(model, graphWithRegisteredWidgets) },
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

    case "widget/input":
      {
        const widgets = setWidgetChannelValue(model.widgets, msg.widgetId, msg.channel, msg.value);
        const graph = graphWithWidgetValue(model.graph, msg.widgetId, msg.channel, msg.value);
        return [
          { ...model, widgets, graph, status: `slider ${msg.widgetId}/${msg.channel}: ${msg.value}` },
          [
            runtimeCommand("xr/widget-event", {
              "widget-id": msg.widgetId,
              channel: msg.channel,
              value: msg.value,
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
      const node = model.graph.nodes.find((node) => node.id === id);
      const ui = node?.ui;
      if (ui?.kind === "widget") {
        const id = widgetId(ui);
        const channel = ui.channels?.[0]?.channel || "value";
        const current = Number(ui.channels?.[0]?.current || 0);
        const value = Number.isFinite(current) ? current : 0;
        return [
          {
            ...model,
            selectedId: id || model.selectedId,
            xr: { ...model.xr, lastPinchAt: msg.at || model.xr.lastPinchAt },
          },
          [
            runtimeCommand("xr/widget-event", {
              "widget-id": id,
              channel,
              value,
            }),
          ],
        ];
      }
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
