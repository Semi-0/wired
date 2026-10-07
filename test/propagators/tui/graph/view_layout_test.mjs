import assert from "node:assert/strict";
import test from "node:test";
import { viewLayout, planarFramingRadius } from "../../../../src/propagators/tui/graph/xr_static/app/view-layout.js";

test("portrait 2D cards form a readable column instead of a tiny horizontal strip", { timeout: 3000 }, () => {
  const positions = viewLayout(7, "2d", 390, 844);
  assert.equal(new Set(positions.map((p) => p.x)).size, 1);
  assert.equal(new Set(positions.map((p) => p.y)).size, 7);
  assert.ok(positions.every((p) => p.z === 0));
  const radius = planarFramingRadius(3.2, 6 * 2.45 + 2.1, 390, 844);
  const halfHeight = radius * 0.6;
  const halfWidth = halfHeight * 390 / 844;
  for (const p of positions) {
    assert.ok(Math.abs(p.x) + 1.6 <= halfWidth);
    assert.ok(Math.abs(p.y) + 1.05 <= halfHeight);
  }
  // Each card is over 90 pixels tall after fitting the whole collection.
  assert.ok(2.1 / (halfHeight * 2) * 844 > 90);
});

test("landscape 2D cards fit a grid without overlapping; 3D retains its row", { timeout: 3000 }, () => {
  const positions = viewLayout(7, "2d", 1200, 800);
  for (let a = 0; a < positions.length; a += 1) {
    for (let b = a + 1; b < positions.length; b += 1) {
      assert.ok(Math.abs(positions[a].x - positions[b].x) > 3.2 ||
        Math.abs(positions[a].y - positions[b].y) > 2.1);
    }
  }
  assert.deepEqual(viewLayout(3, "3d", 390, 844), [
    { x: -3.55, y: 0, z: 0 }, { x: 0, y: 0, z: 0 }, { x: 3.55, y: 0, z: 0 },
  ]);
  assert.deepEqual(viewLayout(0, "2d", 390, 844), []);
});
