import { stepForceLayout } from "./model.js";

// Display-only layout: declarations and source identities are never changed.
export const forceCardPositions = (graph) => {
  const nodes = graph.nodes || [];
  if (nodes.length === 0) return {};
  const layout = Object.fromEntries(nodes.map((node, index) => {
    const angle = index * Math.PI * 2 / nodes.length;
    return [node.id, { x: Math.cos(angle) * 3, y: Math.sin(angle) * 3,
      z: 0, vx: 0, vy: 0, vz: 0 }];
  }));
  let model = { graph, layout, viewMode: "2d", pulses: {} };
  for (let step = 0; step < 180; step += 1) {
    model = stepForceLayout(model, 0.016);
  }
  const points = Object.values(model.layout);
  const xs = points.map((point) => point.x);
  const ys = points.map((point) => point.y);
  const minX = Math.min(...xs);
  const maxX = Math.max(...xs);
  const minY = Math.min(...ys);
  const maxY = Math.max(...ys);
  const scale = Math.min(580 / Math.max(1, maxX - minX), 280 / Math.max(1, maxY - minY));
  return Object.fromEntries(Object.entries(model.layout).map(([id, point]) => [id, {
    x: 350 + (point.x - (minX + maxX) / 2) * scale,
    y: 290 + (point.y - (minY + maxY) / 2) * scale,
  }]));
};

export const cardZoom = (current, action) => {
  switch (action) {
    case "+": return Math.min(4, current * 1.25);
    case "−": return Math.max(0.5, current / 1.25);
    case "Fit": return 1;
    default: throw new Error(`Unknown card zoom action: ${action}`);
  }
};
