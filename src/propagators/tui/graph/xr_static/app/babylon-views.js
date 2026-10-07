import { flattenViewPlanes, viewFingerprint, viewLines } from "./views.js";
import { Msg } from "./msg.js";
import { forceCardPositions, cardZoom } from "./card-layout.js";
import { placeCardLabels } from "./card-labels.js";
import { drawArrow, drawNode } from "./card-symbols.js";
import { viewLayout } from "./view-layout.js";

const panelSize = { width: 3.2, height: 2.1 };
const textureSize = { width: 768, height: 504 };

const drawHierarchy = (ctx, view, forcePositions = null, zoom = 1) => {
  const nodes = view.graph?.nodes || [];
  const edges = view.graph?.edges || [];
  const byId = Object.fromEntries(nodes.map((node) => [node.id, node]));
  const incoming = Object.fromEntries(nodes.map((node) => [node.id, 0]));
  for (const edge of edges) {
    if (byId[edge.to]) incoming[edge.to] = (incoming[edge.to] || 0) + 1;
  }
  const roots = nodes.filter((node) => incoming[node.id] === 0);
  const queue = roots.map((node) => [node.id, 0]);
  const depth = {};
  while (queue.length > 0) {
    const [id, level] = queue.shift();
    if (depth[id] !== undefined && depth[id] <= level) continue;
    depth[id] = level;
    for (const edge of edges.filter((candidate) => candidate.from === id)) {
      queue.push([edge.to, level + 1]);
    }
  }
  const levels = Object.groupBy
    ? Object.groupBy(nodes, (node) => depth[node.id] ?? 0)
    : nodes.reduce((result, node) => {
        const level = depth[node.id] ?? 0;
        result[level] ||= [];
        result[level].push(node);
        return result;
      }, {});
  const positions = {};
  for (const [levelText, levelNodes] of Object.entries(levels)) {
    const level = Number(levelText);
    levelNodes.forEach((node, index) => {
      positions[node.id] = {
        x: 70 + level * 170,
        y: 100 + ((index + 1) * 340) / (levelNodes.length + 1),
      };
    });
  }
  if (forcePositions !== null) {
    Object.assign(positions, forcePositions);
  }
  ctx.strokeStyle = "#6f7782";
  ctx.lineWidth = 3;
  for (const edge of edges) {
    const from = positions[edge.from];
    const to = positions[edge.to];
    if (!from || !to) continue;
    drawArrow(ctx, from, to);
  }
  for (const node of nodes) {
    const point = positions[node.id];
    if (!point) continue;
    drawNode(ctx, node, point);
  }
  ctx.font = `${18 / zoom}px sans-serif`;
  ctx.textBaseline = "middle";
  const bounds = {
    x: 384 + (12 - 384) / zoom, y: 283 + (150 - 283) / zoom,
    width: 744 / zoom, height: 342 / zoom,
  };
  const labels = placeCardLabels(nodes, positions, (text) => {
    const metrics = ctx.measureText(text);
    return Math.max(metrics.width,
      (metrics.actualBoundingBoxLeft || 0) + (metrics.actualBoundingBoxRight || 0));
  }, zoom, bounds);
  for (const label of labels) {
    // Enforce the collision box on actual glyph pixels, not only measurements.
    ctx.save();
    ctx.beginPath();
    ctx.rect(label.x, label.y, label.width, label.height);
    ctx.clip();
    ctx.fillStyle = "#07090c";
    ctx.fillRect(label.x, label.y, label.width, label.height);
    ctx.fillStyle = "#ffffff";
    ctx.fillText(label.text, label.x + label.padding, label.y + label.height / 2);
    ctx.restore();
  }
  ctx.textBaseline = "alphabetic";
  return positions;
};

const paint = (texture, view, zoom, positions) => {
  let targets = [];
  const ctx = texture.getContext();
  ctx.fillStyle = "#07090c";
  ctx.fillRect(0, 0, textureSize.width, textureSize.height);
  ctx.strokeStyle = "#ffffff";
  ctx.lineWidth = 4;
  ctx.strokeRect(3, 3, textureSize.width - 6, textureSize.height - 6);
  ctx.fillStyle = "#ffffff";
  ctx.font = "bold 30px sans-serif";
  ctx.fillText(view.type || "view", 28, 48);
  ["−", "Fit", "+"].forEach((label, index) => {
    ctx.strokeRect(510 + index * 80, 12, 72, 50);
    ctx.fillText(label, 520 + index * 80, 48);
  });
  ctx.save();
  ctx.beginPath();
  ctx.rect(8, 70, 752, 426);
  ctx.clip();
  ctx.translate(384, 283);
  ctx.scale(zoom, zoom);
  ctx.translate(-384, -283);
  ctx.font = "22px ui-monospace, monospace";
  if (view.type === "collection" && view.kind === "graph") {
    const graph = {
      nodes: view.items.map((item, index) => ({ id: item.id, kind: item.kind, label: item.label || String(index + 1) })),
      edges: view.edges,
    };
    const drawn = drawHierarchy(ctx, { graph }, positions, zoom);
    targets = Object.entries(drawn).map(([id, point]) => ({
      id, x: point.x - 24, y: point.y - 24, width: 48, height: 48,
    }));
  } else {
    if (view.selectable) {
      ctx.strokeStyle = "#71d9ef";
      view.items.slice(0, 10).forEach((_item, index) => ctx.strokeRect(24, 102 + index * 32, 720, 32));
    }
    viewLines(view).forEach((line, index) => {
      ctx.fillText(String(line).slice(0, 58), 28, 94 + index * 32);
    });
    if (view.type === "hierarchy" || view.type === "graph") {
      drawHierarchy(ctx, view, positions, zoom);
    } else if (view.type === "collection") {
      targets = view.items.slice(0, 10).map((item, index) => ({
        id: item.id, x: 24, y: 102 + index * 32, width: 720, height: 32,
      }));
    } else {
      targets = [];
    }
  }
  ctx.restore();
  if (view.type === "graph" || view.type === "hierarchy" ||
      (view.type === "collection" && view.kind === "graph")) {
    ctx.fillStyle = "#07090c";
    ctx.fillRect(18, 70, 730, 64);
    ctx.font = "18px sans-serif";
    drawNode(ctx, { kind: "cell" }, { x: 34, y: 92 });
    ctx.fillText("Cell", 52, 98);
    drawNode(ctx, { kind: "propagator" }, { x: 130, y: 92 });
    ctx.fillText("Propagator", 150, 98);
    ctx.fillText("→ direction of flow", 285, 98);
  }
  texture.update();
  return targets;
};

export const createBabylonViewLayer = ({ BABYLON, scene, dispatch }) => {
  const panels = new Map();

  scene.onPointerObservable.add((event) => {
    if (event.type !== BABYLON.PointerEventTypes.POINTERPICK) return;
    const pick = event.pickInfo;
    const panel = panels.get(pick?.pickedMesh?.metadata?.collectionViewId);
    if (!panel) return;
    const uv = pick.getTextureCoordinates();
    if (!uv) return;
    let x = uv.x * textureSize.width;
    let y = (1 - uv.y) * textureSize.height;
    if (y >= 12 && y <= 62 && x >= 510 && x <= 742) {
      const index = Math.floor((x - 510) / 80);
      if (x - (510 + index * 80) <= 72) {
        panel.zoom = cardZoom(panel.zoom, ["−", "Fit", "+"][index]);
        panel.targets = paint(panel.texture, panel.view, panel.zoom, panel.positions);
      }
      return;
    }
    if (!panel.view.selectable || y < 70 || y > 496) return;
    x = 384 + (x - 384) / panel.zoom;
    y = 283 + (y - 283) / panel.zoom;
    const target = panel.targets.find((item) =>
      x >= item.x && x <= item.x + item.width && y >= item.y && y <= item.y + item.height);
    if (target) dispatch(Msg.ViewSelect(panel.view, target.id));
  });

  const createPanel = (view) => {
    const texture = new BABYLON.DynamicTexture(
      `view-${view.id}-texture`, textureSize, scene, false
    );
    const material = new BABYLON.StandardMaterial(`view-${view.id}-material`, scene);
    material.diffuseTexture = texture;
    material.emissiveTexture = texture;
    material.disableLighting = true;
    material.backFaceCulling = false;
    const plane = BABYLON.MeshBuilder.CreatePlane(
      `view-${view.id}`, panelSize, scene
    );
    plane.material = material;
    // Keep juxtaposed cards in one plane: independent billboards can overlap.
    plane.billboardMode = BABYLON.Mesh.BILLBOARDMODE_NONE;
    plane.isPickable = false;
    plane.metadata = { collectionViewId: view.id };
    const panel = { plane, material, texture, fingerprint: null, zoom: 1, positions: null };
    panels.set(view.id, panel);
    return panel;
  };

  const prune = (ids) => {
    for (const [id, panel] of panels) {
      if (ids.has(id)) continue;
      panel.plane.dispose();
      panel.material.dispose();
      panel.texture.dispose();
      panels.delete(id);
    }
  };

  const renderViews = (model, width, height) => {
    const views = flattenViewPlanes(model.views);
    prune(new Set(views.map((view) => view.id)));
    const positions = viewLayout(views.length, model.viewMode, width, height);
    views.forEach((view, index) => {
      const panel = panels.get(view.id) || createPanel(view);
      const fingerprint = viewFingerprint(view);
      if (panel.fingerprint !== fingerprint) {
        if (view.type === "graph") {
          panel.positions = forceCardPositions(view.graph);
        } else if (view.type === "collection" && view.kind === "graph") {
          panel.positions = forceCardPositions({ nodes: view.items, edges: view.edges });
        } else {
          panel.positions = null;
        }
        panel.targets = paint(panel.texture, view, panel.zoom, panel.positions);
        panel.view = view;
        panel.plane.isPickable = true;
        panel.fingerprint = fingerprint;
      }
      const position = positions[index];
      panel.plane.position.set(position.x, position.y, position.z);
    });
  };

  return { renderViews };
};
