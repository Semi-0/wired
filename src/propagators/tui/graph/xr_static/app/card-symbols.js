export const arrowGeometry = (from, to) => {
  const dx = to.x - from.x;
  const dy = to.y - from.y;
  const distance = Math.hypot(dx, dy);
  if (distance <= 28) return null;
  const ux = dx / distance;
  const uy = dy / distance;
  const start = { x: from.x + ux * 13, y: from.y + uy * 13 };
  const tip = { x: to.x - ux * 13, y: to.y - uy * 13 };
  const length = Math.min(9, (distance - 26) / 2);
  return { start, tip,
    left: { x: tip.x - ux * length - uy * 4, y: tip.y - uy * length + ux * 4 },
    right: { x: tip.x - ux * length + uy * 4, y: tip.y - uy * length - ux * 4 } };
};

export const drawArrow = (ctx, from, to) => {
  const arrow = arrowGeometry(from, to);
  if (arrow === null) return;
  ctx.strokeStyle = "#8a9aa8";
  ctx.fillStyle = "#c0d1df";
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.moveTo(arrow.start.x, arrow.start.y);
  ctx.lineTo(arrow.tip.x, arrow.tip.y);
  ctx.stroke();
  ctx.beginPath();
  ctx.moveTo(arrow.tip.x, arrow.tip.y);
  ctx.lineTo(arrow.left.x, arrow.left.y);
  ctx.lineTo(arrow.right.x, arrow.right.y);
  ctx.closePath();
  ctx.fill();
};

export const drawNode = (ctx, node, point) => {
  ctx.beginPath();
  if (node.kind === "propagator") {
    ctx.fillStyle = "#71d9ef";
    ctx.moveTo(point.x + 11, point.y);
    ctx.lineTo(point.x - 9, point.y - 10);
    ctx.lineTo(point.x - 9, point.y + 10);
    ctx.closePath();
  } else {
    ctx.fillStyle = "#ffffff";
    ctx.arc(point.x, point.y, 10, 0, Math.PI * 2);
  }
  ctx.fill();
};
