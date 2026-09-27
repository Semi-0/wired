export const overlaps = (a, b) =>
  a.x < b.x + b.width && a.x + a.width > b.x &&
  a.y < b.y + b.height && a.y + a.height > b.y;

// Work in graph coordinates; measured text and padding reflect the current zoom.
// A crowded label is omitted rather than covering another label or node.
export const placeCardLabels = (nodes, positions, measure, zoom, bounds) => {
  const padding = 4 / zoom;
  const height = 22 / zoom;
  const obstacles = nodes.flatMap((node) => {
    const p = positions[node.id];
    if (!p) return [];
    return [{ x: p.x - 12, y: p.y - 12, width: 24, height: 24 }];
  });
  const labels = [];
  for (const node of nodes) {
    const p = positions[node.id];
    if (!p) continue;
    const text = String(node.label || node.id);
    const width = measure(text) + padding * 2;
    const gap = 14 + padding;
    const candidates = [
      [p.x + gap, p.y - height / 2],
      [p.x - gap - width, p.y - height / 2],
      [p.x - width / 2, p.y - gap - height],
      [p.x - width / 2, p.y + gap],
      [p.x + gap, p.y - gap - height],
      [p.x - gap - width, p.y - gap - height],
      [p.x + gap, p.y + gap],
      [p.x - gap - width, p.y + gap],
    ];
    const box = candidates.map(([x, y]) => ({ x, y, width, height }))
      .find((candidate) => candidate.x >= bounds.x && candidate.y >= bounds.y &&
        candidate.x + width <= bounds.x + bounds.width &&
        candidate.y + height <= bounds.y + bounds.height &&
        !obstacles.some((obstacle) => overlaps(candidate, obstacle)));
    if (box) {
      labels.push({ ...box, id: node.id, text, padding });
      obstacles.push(box);
    }
  }
  return labels;
};
