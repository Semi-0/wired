import test from "node:test";
import assert from "node:assert/strict";
import { arrowGeometry, drawNode } from "../../../../src/propagators/tui/graph/xr_static/app/card-symbols.js";

test("arrowheads terminate before the target and reverse with dataflow", () => {
  const a = { x: 0, y: 0 };
  const b = { x: 100, y: 0 };
  const forward = arrowGeometry(a, b);
  const backward = arrowGeometry(b, a);
  assert.equal(forward.start.x, 13);
  assert.equal(forward.tip.x, 87);
  assert.ok(forward.left.x < forward.tip.x);
  assert.equal(backward.tip.x, 13);
  assert.ok(backward.left.x > backward.tip.x);
  assert.equal(arrowGeometry(a, a), null);
  const vertical = arrowGeometry(a, { x: 0, y: 100 });
  assert.equal(vertical.tip.y, 87);
  assert.ok(vertical.left.y < vertical.tip.y);
});

test("node kind, not its label, chooses circle or triangle", () => {
  const commands = [];
  const ctx = new Proxy({}, { get: (_target, key) => (...args) => commands.push([key, ...args]) });
  drawNode(ctx, { kind: "propagator", label: "cell" }, { x: 50, y: 50 });
  assert.equal(commands.filter(([name]) => name === "lineTo").length, 2);
  assert.equal(commands.some(([name]) => name === "arc"), false);
  commands.length = 0;
  drawNode(ctx, { kind: "cell", label: "filter" }, { x: 50, y: 50 });
  assert.equal(commands.some(([name]) => name === "arc"), true);
});
