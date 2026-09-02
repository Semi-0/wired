export const initialModel = () => ({
  connected: false,
  status: "disconnected",
  graph: { nodes: [], edges: [] },
  widgets: {},
  layout: {},
  pulses: {},
  selectedId: null,
  viewMode: "3d",
  xr: { supported: false, active: false, lastPinchAt: 0 },
});

export const nodeLabel = (node) => node.label || node.id;

export const selectedNode = (model) =>
  model.graph.nodes.find((node) => node.id === model.selectedId) || null;

const normalizeWidget = (widget, nodeId = null) => {
  const channels = Array.isArray(widget.channels)
    ? widget.channels
    : Object.values(widget.channels || {});
  return {
    ...widget,
    widgetId: widget.widgetId || widget["widget-id"] || widget.id,
    nodeId: nodeId || widget.nodeId || widget["node-id"] || null,
    channels,
  };
};

export const widgetsFromGraph = (graph) =>
  Object.fromEntries([
    ...(graph.widgets || [])
      .map((widget) => normalizeWidget(widget))
      .filter((widget) => widget.widgetId)
      .map((widget) => [widget.widgetId, widget]),
    ...(graph.nodes || [])
      .filter((node) =>
        (node.kind === "widget" || node.ui?.kind === "widget") &&
        (node.ui?.widgetId || node.ui?.["widget-id"] || node.ui?.id)
      )
      .map((node) => normalizeWidget(node.ui, node.id))
      .filter((widget) => widget.widgetId)
      .map((widget) => [widget.widgetId, widget]),
  ]);

export const graphWithWidgets = (graph, widgets) => {
  const widgetsByNodeId = Object.fromEntries(
    Object.values(widgets || {})
      .filter((widget) => widget.nodeId)
      .map((widget) => [widget.nodeId, widget])
  );
  const nodes = (graph.nodes || []).map((node) =>
    widgetsByNodeId[node.id]
      ? { ...node, ui: widgetsByNodeId[node.id] }
      : node
  );
  const nodeIds = new Set(nodes.map((node) => node.id));
  for (const widget of Object.values(widgets || {})) {
    const id = widget.nodeId || `widget:${widget.widgetId}`;
    if (!id || nodeIds.has(id)) continue;
    nodes.push({
      id,
      label: widget.widgetId,
      kind: "widget",
      ui: widget,
    });
    nodeIds.add(id);
  }
  return { ...graph, nodes };
};

export const reconcileLayout = (layout, graph) => {
  const next = {};
  for (const node of graph.nodes) {
    next[node.id] =
      layout[node.id] ||
      {
        x: (Math.random() - 0.5) * 6,
        y: (Math.random() - 0.5) * 4,
        z: (Math.random() - 0.5) * 6,
        vx: 0,
        vy: 0,
        vz: 0,
      };
  }
  return next;
};

export const stepPulses = (pulses, dt) =>
  Object.fromEntries(
    Object.entries(pulses)
      .map(([id, ttl]) => [id, ttl - dt])
      .filter(([_id, ttl]) => ttl > 0)
  );

export const stepForceLayout = (model, dt) => {
  const ids = model.graph.nodes.map((node) => node.id);
  const layout = Object.fromEntries(
    Object.entries(model.layout).map(([id, p]) => [id, { ...p }])
  );
  const byId = Object.fromEntries(model.graph.nodes.map((node) => [node.id, node]));
  const edgeSet = model.graph.edges.filter((edge) => byId[edge.from] && byId[edge.to]);
  const step = Math.min(dt, 0.032);

  for (let i = 0; i < ids.length; i += 1) {
    const a = layout[ids[i]];
    for (let j = i + 1; j < ids.length; j += 1) {
      const b = layout[ids[j]];
      const dx = a.x - b.x;
      const dy = a.y - b.y;
      const dz = a.z - b.z;
      const d2 = Math.max(dx * dx + dy * dy + dz * dz, 0.04);
      const f = 1.8 / d2;
      const d = Math.sqrt(d2);
      const fx = (dx / d) * f;
      const fy = (dy / d) * f;
      const fz = (dz / d) * f;
      a.vx += fx * step;
      a.vy += fy * step;
      a.vz += fz * step;
      b.vx -= fx * step;
      b.vy -= fy * step;
      b.vz -= fz * step;
    }
  }

  for (const edge of edgeSet) {
    const a = layout[edge.from];
    const b = layout[edge.to];
    const dx = b.x - a.x;
    const dy = b.y - a.y;
    const dz = b.z - a.z;
    const d = Math.max(Math.sqrt(dx * dx + dy * dy + dz * dz), 0.001);
    const f = (d - 1.8) * 0.8;
    const fx = (dx / d) * f;
    const fy = (dy / d) * f;
    const fz = (dz / d) * f;
    a.vx += fx * step;
    a.vy += fy * step;
    a.vz += fz * step;
    b.vx -= fx * step;
    b.vy -= fy * step;
    b.vz -= fz * step;
  }

  for (const id of ids) {
    const p = layout[id];
    p.vx = (p.vx - p.x * 0.08 * step) * 0.92;
    p.vy = (p.vy - p.y * 0.08 * step) * 0.92;
    p.vz = (p.vz - p.z * 0.08 * step) * 0.92;
    p.x += p.vx;
    p.y += p.vy;
    p.z += p.vz;
  }

  return { ...model, layout, pulses: stepPulses(model.pulses, step) };
};
