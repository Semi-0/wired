import assert from "node:assert/strict";
import test from "node:test";

import { initialModel } from "../../graph/xr_static/app/model.js";
import { update } from "../../graph/xr_static/app/update.js";

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
