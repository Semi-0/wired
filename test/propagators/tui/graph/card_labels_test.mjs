import test from "node:test";
import assert from "node:assert/strict";
import { placeCardLabels, overlaps } from "../../../../src/propagators/tui/graph/xr_static/app/card-labels.js";
import { forceCardPositions } from "../../../../src/propagators/tui/graph/xr_static/app/card-layout.js";

test("crowded cyclic graphs never draw overlapping labels at supported zoom levels", () => {
  const nodes = Array.from({ length: 60 }, (_, i) => ({ id: String(i), label: `propagator-${i}` }));
  const graph = { nodes, edges: nodes.map((node, i) => ({ from: node.id, to: String((i + 1) % nodes.length) })) };
  const original = structuredClone(graph);
  const positions = forceCardPositions(graph);
  for (const zoom of [0.5, 1, 1.25, 2, 4]) {
    const bounds = { x: 384 + (12 - 384) / zoom, y: 283 + (150 - 283) / zoom,
      width: 744 / zoom, height: 342 / zoom };
    const measure = (text) => text.length * 9 / zoom;
    const labels = placeCardLabels(nodes, positions, measure, zoom, bounds);
    assert.ok(labels.length > 0);
    assert.deepEqual(labels, placeCardLabels(nodes, positions, measure, zoom, bounds));
    for (const [index, label] of labels.entries()) {
      assert.ok(label.x >= bounds.x && label.y >= bounds.y);
      assert.ok(label.x + label.width <= bounds.x + bounds.width);
      assert.ok(label.y + label.height <= bounds.y + bounds.height);
      for (const other of labels.slice(index + 1)) assert.equal(overlaps(label, other), false);
      for (const point of Object.values(positions)) {
        assert.equal(overlaps(label, { x: point.x - 12, y: point.y - 12, width: 24, height: 24 }), false);
      }
    }
  }
  assert.deepEqual(graph, original);
});

test("full labels use alternative positions; impossible labels are omitted", () => {
  const nodes = [{ id: "a", label: "a complete propagator name" }, { id: "b", label: "neighbor" }];
  const positions = { a: { x: 320, y: 260 }, b: { x: 360, y: 260 } };
  const bounds = { x: 0, y: 0, width: 768, height: 504 };
  const labels = placeCardLabels(nodes, positions, (text) => text.length * 9, 1, bounds);
  assert.equal(labels.length, 2);
  assert.equal(labels[0].text, nodes[0].label);
  assert.notEqual(labels[0].x, positions.a.x + 18);
  assert.deepEqual(placeCardLabels(nodes, positions, () => 1000, 1, bounds), []);
  assert.deepEqual(placeCardLabels([], {}, () => 0, 1, bounds), []);
});
