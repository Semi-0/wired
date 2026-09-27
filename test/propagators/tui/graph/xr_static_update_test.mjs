import assert from "node:assert/strict";
import test from "node:test";

import {
  initialModel,
  liftLayoutInto3d,
  stepForceLayout,
} from "../../../../src/propagators/tui/graph/xr_static/app/model.js";
import { displayPosition } from "../../../../src/propagators/tui/graph/xr_static/app/babylon-graph-view.js";
import { update } from "../../../../src/propagators/tui/graph/xr_static/app/update.js";

const widgetId = "slider-panel-0";
const nodeId = `widget:${widgetId}`;

const channel = (current, epoch) => ({ channel: "a", current, epoch });

const widget = (current, epoch) => ({
  id: widgetId,
  widgetId,
  type: "slider-panel",
  channels: [channel(current, epoch)],
});

const widgetNode = (current, epoch) => ({
  id: nodeId,
  label: widgetId,
  kind: "widget",
  ui: {
    kind: "widget",
    widgetId,
    type: "slider-panel",
    channels: [channel(current, epoch)],
  },
});

const graphPayload = (current, epoch) => ({
  ok: true,
  result: {
    graph: {
      graph: true,
      widgets: [widget(current, epoch)],
      nodes: [widgetNode(current, epoch)],
      edges: [],
    },
  },
});

const dispatch = (model, msg) => update(model, msg)[0];

const widgetChannel = (model) => model.widgets[widgetId].channels[0];

const graphWidgetChannel = (model) =>
  model.graph.nodes.find((node) => node.id === nodeId).ui.channels[0];

const assertWidgetState = (model, current, epoch) => {
  assert.deepEqual(widgetChannel(model), channel(current, epoch));
  assert.deepEqual(graphWidgetChannel(model), channel(current, epoch));
};

const runEffects = (effects, env) => {
  for (const effect of effects) {
    effect.run(() => {}, env);
  }
};

const step = (model, msg, env) => {
  const [next, effects] = update(model, msg);
  runEffects(effects, env);
  return next;
};

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

test("stale server widget payload does not snap local slider value back", () => {
  const registered = dispatch(initialModel(), {
    type: "socket/message",
    payload: graphPayload(10, 1),
  });
  assertWidgetState(registered, 10, 1);

  const local = dispatch(registered, {
    type: "widget/input",
    widgetId,
    channel: "a",
    value: 42,
  });
  assertWidgetState(local, 42, 2);

  const staleServerEcho = dispatch(local, {
    type: "socket/message",
    payload: graphPayload(10, 1),
  });
  assertWidgetState(staleServerEcho, 42, 2);

  const acceptedServerEcho = dispatch(staleServerEcho, {
    type: "socket/message",
    payload: graphPayload(42, 2),
  });
  assertWidgetState(acceptedServerEcho, 42, 2);

  const newerServerValue = dispatch(acceptedServerEcho, {
    type: "socket/message",
    payload: graphPayload(50, 3),
  });
  assertWidgetState(newerServerValue, 50, 3);
});

test("rapid slider inputs coalesce to the latest runtime command", async () => {
  const sent = [];
  const env = {
    socket: {
      readyState: 1,
      send: (payload) => sent.push(JSON.parse(payload)),
    },
  };
  let model = dispatch(initialModel(), {
    type: "socket/message",
    payload: graphPayload(10, 1),
  });

  model = step(model, { type: "widget/input", widgetId, channel: "a", value: 20 }, env);
  model = step(model, { type: "widget/input", widgetId, channel: "a", value: 30 }, env);
  model = step(model, { type: "widget/input", widgetId, channel: "a", value: 40 }, env);

  assertWidgetState(model, 40, 4);
  assert.deepEqual(sent, []);

  await sleep(70);

  assert.deepEqual(sent, [
    {
      op: "xr/widget-event",
      "widget-id": widgetId,
      channel: "a",
      value: 40,
    },
  ]);
});

test("view mode transitions are explicit and preserve the graph payload", () => {
  const graph = {
    nodes: [{ id: "a", label: "A" }],
    edges: [],
  };
  const original = {
    ...initialModel(),
    graph,
    layout: { a: { x: 1, y: 2, z: 4, vx: 0, vy: 0, vz: 0 } },
  };
  const twoDimensional = dispatch(original, { type: "view/mode", mode: "2d" });
  const threeDimensional = dispatch(twoDimensional, { type: "view/mode", mode: "3d" });
  const unsupported = dispatch(threeDimensional, { type: "view/mode", mode: "paper" });

  assert.equal(twoDimensional.viewMode, "2d");
  assert.equal(twoDimensional.status, "2D view");
  assert.equal(threeDimensional.viewMode, "3d");
  assert.equal(unsupported.viewMode, "3d");
  assert.equal(unsupported.status, "Unsupported view mode: paper");
  assert.strictEqual(twoDimensional.graph, graph);
  assert.strictEqual(threeDimensional.graph, graph);
});

test("2D force layout is planar and returning to 3D restores depth", () => {
  const planar = stepForceLayout(
    {
      ...initialModel(),
      viewMode: "2d",
      graph: {
        nodes: [{ id: "a" }, { id: "b" }],
        edges: [{ from: "a", to: "b" }],
      },
      layout: {
        a: { x: -1, y: 0, z: 4, vx: 0, vy: 0, vz: 2 },
        b: { x: 1, y: 0, z: -4, vx: 0, vy: 0, vz: -2 },
      },
    },
    0.016
  );

  assert.equal(planar.layout.a.z, 0);
  assert.equal(planar.layout.b.z, 0);
  assert.equal(planar.layout.a.vz, 0);
  assert.equal(planar.layout.b.vz, 0);

  const lifted = liftLayoutInto3d(planar.layout);
  assert.notEqual(lifted.a.z, 0);
  assert.notEqual(lifted.b.z, 0);
  assert.notEqual(lifted.a.z, lifted.b.z);
});

test("renderer projection flattens only the 2D interpretation", () => {
  const point = { x: 1, y: 2, z: 3 };

  assert.deepEqual(displayPosition("2d", point), { x: 1, y: 2, z: 0 });
  assert.deepEqual(displayPosition("3d", point), point);
  assert.deepEqual(displayPosition("xr", point), point);
});
