// Shared geometry keeps card placement and camera framing in agreement.
export const viewLayout = (count, mode, width, height) => {
  if (count === 0) return [];
  const aspect = Math.max(width, 1) / Math.max(height, 1);
  let columns;
  if (mode === "2d") {
    columns = Math.max(1, Math.min(count, Math.round(Math.sqrt(count * aspect * 2.45 / 3.55))));
  } else {
    columns = count;
  }
  const rows = Math.ceil(count / columns);
  return Array.from({ length: count }, (_, index) => ({
    x: ((index % columns) - (columns - 1) / 2) * 3.55,
    y: ((rows - 1) / 2 - Math.floor(index / columns)) * 2.45,
    z: 0,
  }));
};

export const planarFramingRadius = (width, height, viewportWidth, viewportHeight) => {
  const aspect = Math.max(viewportWidth, 1) / Math.max(viewportHeight, 1);
  const halfHeight = Math.max(height / 2, width / (2 * aspect), 2.5) * 1.15;
  return halfHeight / 0.6;
};
