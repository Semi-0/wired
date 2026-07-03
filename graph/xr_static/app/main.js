import { batch } from "./combinators.js";
import { bindControls, connectSocket, animationLoop } from "./effects.js";
import { initialModel } from "./model.js";
import { Msg } from "./msg.js";
import { createRenderer } from "./render.js";
import { update } from "./update.js";

const env = {
  model: initialModel(),
  socket: null,
  enterXr: null,
};

const statusEl = document.getElementById("status");
const viewport = document.getElementById("viewport");
const selectionEl = document.getElementById("selection");
const view3dEl = document.getElementById("view-3d");
const viewXrEl = document.getElementById("view-xr");

let rendererHandle = null;

const runEffects = (effects) => {
  for (const effect of effects) {
    if (effect && effect.run) effect.run(dispatch, env);
  }
};

export const dispatch = (msg) => {
  const [next, effects] = update(env.model, msg);
  env.model = next;
  statusEl.textContent = env.model.status;
  view3dEl.classList.toggle("active", env.model.viewMode === "3d");
  viewXrEl.classList.toggle("active", env.model.viewMode === "xr");
  runEffects(effects);
};

rendererHandle = createRenderer({
  root: viewport,
  selectionEl,
  dispatch,
});

runEffects(batch(
  [connectSocket()],
  [bindControls({
    installTrace: Msg.InstallTrace,
    extendGraph: Msg.ExtendGraph,
    sendValue: Msg.SendValue,
    setViewMode: Msg.SetViewMode,
  })],
  [animationLoop({
    renderer: rendererHandle.renderer,
    render: rendererHandle.render,
    detectPinch: rendererHandle.detectPinch,
  })]
));
