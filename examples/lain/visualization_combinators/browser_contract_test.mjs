import assert from "node:assert/strict";
import { flattenViewPlanes, viewLines } from "../../../src/propagators/tui/graph/xr_static/app/views.js";
import { initialModel } from "../../../src/propagators/tui/graph/xr_static/app/model.js";
import { Msg } from "../../../src/propagators/tui/graph/xr_static/app/msg.js";
import { update } from "../../../src/propagators/tui/graph/xr_static/app/update.js";
import { forceCardPositions, cardZoom } from "../../../src/propagators/tui/graph/xr_static/app/card-layout.js";
import { createBabylonViewLayer } from "../../../src/propagators/tui/graph/xr_static/app/babylon-views.js";

const view = {
  type: "collection", id: "view", kind: "list", epoch: 1,
  generation: "environment", revision: "revision", selectable: true,
  items: [{ id: "source", value: { kind: "value", value: "7" } }], pending: 1,
};
assert.deepEqual(flattenViewPlanes([{ type: "juxtapose", children: [view] }]), [view]);
assert.deepEqual(viewLines(view), ["list · 1 items · 1 pending", "7"]);
const original = structuredClone(view);
const model = initialModel();
const [next, effects] = update(model, Msg.ViewSelect(view, "source"));
assert.equal(next, model);
const sent = [];
effects[0].run(() => {}, { socket: { readyState: 1, send: (text) => sent.push(JSON.parse(text)) } });
assert.deepEqual(sent, [{
  op: "xr/view-select", "view-id": "view", "item-id": "source",
  epoch: 1, revision: "revision", generation: "environment",
}]);
assert.deepEqual(view, original);
const graph = {
  type: "graph", id: "raw-trace",
  graph: { nodes: [{ id: "a" }, { id: "b" }], edges: [{ from: "a", to: "b" }] },
};
assert.deepEqual(flattenViewPlanes([
  { type: "juxtapose", children: [graph, { type: "juxtapose", children: [view] }] },
]), [graph, view]);
assert.deepEqual(viewLines(graph), ["2 nodes", "1 relationships"]);
const positions = forceCardPositions(graph.graph);
assert.deepEqual(positions, forceCardPositions(graph.graph));
assert.deepEqual(forceCardPositions({ nodes: [], edges: [] }), {});
assert.ok(Object.values(positions).every((p) => Number.isFinite(p.x) && Number.isFinite(p.y)));
assert.notDeepEqual(positions.a, positions.b);
assert.equal(cardZoom(4, "+"), 4);
assert.equal(cardZoom(0.5, "−"), 0.5);
assert.equal(cardZoom(3, "Fit"), 1);
assert.throws(() => cardZoom(1, "unknown"));
assert.match(viewLines({ type: "collection", kind: "list", items: [] }).join(" "), /Select a value/);

// Exercise actual texture picking without a GPU or browser.
const planes = [];
const textures = [];
const picks = [];
const events = [];
const BABYLON = {
  PointerEventTypes: { POINTERPICK: 1 }, Mesh: { BILLBOARDMODE_ALL: 7, BILLBOARDMODE_NONE: 0 },
  DynamicTexture: class {
    constructor() {
      this.scales = [];
      this.ctx = new Proxy({}, { get: (_target, key) => {
        if (key === "scale") return (x) => this.scales.push(x);
        if (key === "measureText") return (text) => ({ width: text.length * 9 });
        return () => {};
      } });
      textures.push(this);
    }
    getContext() { return this.ctx; }
    update() {}
    dispose() {}
  },
  StandardMaterial: class { dispose() {} },
  MeshBuilder: { CreatePlane: () => {
    const plane = { position: { set() {} }, dispose() {} };
    planes.push(plane);
    return plane;
  } },
};
const layer = createBabylonViewLayer({ BABYLON,
  scene: { onPointerObservable: { add: (handler) => picks.push(handler) } },
  dispatch: (event) => events.push(event),
});
layer.renderViews({ views: [graph, view] });
assert.equal(planes[0].billboardMode, BABYLON.Mesh.BILLBOARDMODE_NONE);
assert.equal(planes[1].billboardMode, BABYLON.Mesh.BILLBOARDMODE_NONE);
const pick = (plane, x, y) => picks[0]({ type: 1, pickInfo: {
  pickedMesh: plane, getTextureCoordinates: () => ({ x: x / 768, y: 1 - y / 504 }),
} });
pick(planes[0], 706, 36);
assert.equal(textures[0].scales.at(-1), 1.25);
assert.equal(textures[1].scales.at(-1), 1);
assert.equal(events.length, 0);
pick(planes[1], 100, 120);
assert.deepEqual(events, [Msg.ViewSelect(view, "source")]);
pick(planes[1], 706, 36);
pick(planes[1], 384 + (100 - 384) * 1.25, 283 + (120 - 283) * 1.25);
assert.equal(events.length, 2);
assert.deepEqual(events[1], Msg.ViewSelect(view, "source"));
console.log("24 browser contract assertions passed");
