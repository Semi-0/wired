import assert from "node:assert/strict";
import test from "node:test";
import { configureCameraMode } from "../../../../src/propagators/tui/graph/xr_static/app/camera-mode.js";
import { createBabylonInput } from "../../../../src/propagators/tui/graph/xr_static/app/babylon-input.js";

const BABYLON = {
  Camera: { ORTHOGRAPHIC_CAMERA: 1, PERSPECTIVE_CAMERA: 0 },
  PointerEventTypes: { POINTERDOWN: 1, POINTERMOVE: 2, POINTERUP: 3, POINTERDOUBLETAP: 4 },
};

test("2D locks both orbit axes and uses ordinary drag to pan", { timeout: 3000 }, () => {
  const calls = [];
  const camera = {
    detachControl: () => calls.push("detach"),
    attachControl: (...args) => calls.push(args),
  };
  const canvas = {};
  configureCameraMode(BABYLON, camera, canvas, "2d");
  assert.equal(camera.mode, BABYLON.Camera.ORTHOGRAPHIC_CAMERA);
  assert.equal(camera.lowerAlphaLimit, camera.upperAlphaLimit);
  assert.equal(camera.lowerBetaLimit, camera.upperBetaLimit);
  assert.deepEqual(calls, ["detach", [canvas, true, false, 0]]);
  configureCameraMode(BABYLON, camera, canvas, "3d");
  assert.equal(camera.mode, BABYLON.Camera.PERSPECTIVE_CAMERA);
  assert.equal(camera.lowerAlphaLimit, null);
  assert.equal(camera.upperAlphaLimit, null);
  assert.ok(camera.lowerBetaLimit < camera.upperBetaLimit);
  assert.deepEqual(calls.at(-1), [canvas, true, true, 2]);
});

test("unknown camera mode fails before changing controls", { timeout: 3000 }, () => {
  assert.throws(() => configureCameraMode(BABYLON, {}, {}, "unknown"), /Unsupported camera mode/);
});

const input = (nodes = []) => {
  let pointer;
  const messages = [];
  const mesh = { metadata: { nodeId: "cell" } };
  const controls = createBabylonInput({
    BABYLON,
    scene: { onPointerObservable: { add: (handler) => { pointer = handler; } } },
    canvas: { addEventListener: () => {} },
    camera: { onViewMatrixChangedObservable: { add: () => {} } },
    graphView: { nodeMeshes: new Map([["cell", mesh]]) },
    dispatch: (message) => messages.push(message),
  });
  controls.setModel({ graph: { nodes } });
  return { controls, messages, mesh, pointer: (pickInfo) => pointer({ type: 1, pickInfo }) };
};

test("empty-space taps do not throw or select an absent node", { timeout: 3000 }, () => {
  const { pointer, controls, messages } = input();
  assert.doesNotThrow(() => pointer({ pickedMesh: null }));
  assert.equal(controls.interaction.userAdjusted, true);
  assert.deepEqual(messages, []);
});

test("ordinary node selection still works", { timeout: 3000 }, () => {
  const { pointer, controls, messages, mesh } = input([{ id: "cell", kind: "cell" }]);
  pointer({ pickedMesh: mesh });
  assert.equal(messages.length, 1);
  assert.equal(controls.interaction.widgetDrag, null);
});
