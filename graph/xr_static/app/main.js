import { batch } from "./combinators.js";
import { bindControls, connectSocket, animationLoop } from "./effects.js";
import { initialModel } from "./model.js";
import { Msg } from "./msg.js";
import { createRenderer } from "./babylon-render.js";
import { update } from "./update.js";

const env = {
  model: initialModel(),
  socket: null,
  enterXr: null,
};

const statusEl = document.getElementById("status");
const viewport = document.getElementById("viewport");
const selectionEl = document.getElementById("selection");
const widgetsEl = document.getElementById("widgets");
const view3dEl = document.getElementById("view-3d");
const viewXrEl = document.getElementById("view-xr");

let rendererHandle = null;
let widgetsShapeKey = "";

const rangeValue = (value) => {
  if (value === null || value === undefined || value === "") return "0";
  const n = Number(value);
  return Number.isFinite(n) ? String(n) : "0";
};

const clampSliderValue = (value) => {
  const n = Number(value);
  return Math.max(0, Math.min(100, Number.isFinite(n) ? Math.round(n) : 0));
};

const setSliderDisplay = (slider, value) => {
  const n = clampSliderValue(value);
  slider.dataset.value = String(n);
  slider.style.setProperty("--value", `${n}%`);
  slider.setAttribute("aria-valuenow", String(n));
  const valueEl = slider.querySelector(".slider-value");
  if (valueEl) valueEl.textContent = String(n);
};

const pointerSliderValue = (slider, event) => {
  const rect = slider.getBoundingClientRect();
  const x = Math.max(0, Math.min(1, (event.clientX - rect.left) / Math.max(rect.width, 1)));
  return clampSliderValue(x * 100);
};

const runEffects = (effects) => {
  for (const effect of effects) {
    if (effect && effect.run) effect.run(dispatch, env);
  }
};

const renderWidgets = (model) => {
  const widgets = Object.values(model.widgets || {});
  const shapeKey = JSON.stringify(
    widgets.map((widget) => [
      widget.widgetId,
      (widget.channels || []).map((channel) => channel.channel),
    ])
  );
  if (shapeKey !== widgetsShapeKey) {
    widgetsShapeKey = shapeKey;
    widgetsEl.innerHTML = "";
    for (const widget of widgets) {
      const section = document.createElement("section");
      section.className = "widget-panel";
      const title = document.createElement("h2");
      title.textContent = widget.widgetId;
      section.appendChild(title);
      for (const channel of widget.channels || []) {
        const label = document.createElement("label");
        label.textContent = channel.channel;
        const slider = document.createElement("div");
        slider.className = "slider-control";
        slider.tabIndex = 0;
        slider.setAttribute("role", "slider");
        slider.setAttribute("aria-valuemin", "0");
        slider.setAttribute("aria-valuemax", "100");
        slider.dataset.widgetId = widget.widgetId;
        slider.dataset.channel = channel.channel;
        slider.innerHTML = `
          <span class="slider-track"><span class="slider-fill"></span><span class="slider-knob"></span></span>
          <span class="slider-value">0</span>
        `;
        const sendValue = (value) => {
          setSliderDisplay(slider, value);
          dispatch(Msg.WidgetInput(widget.widgetId, channel.channel, clampSliderValue(value)));
        };
        slider.addEventListener("pointerdown", (event) => {
          event.preventDefault();
          slider.dataset.dragging = "true";
          slider.setPointerCapture?.(event.pointerId);
          sendValue(pointerSliderValue(slider, event));
        });
        slider.addEventListener("pointermove", (event) => {
          if (slider.dataset.dragging === "true") {
            event.preventDefault();
            sendValue(pointerSliderValue(slider, event));
          }
        });
        const stopDrag = (event) => {
          slider.dataset.dragging = "false";
          if (slider.hasPointerCapture?.(event.pointerId)) {
            slider.releasePointerCapture(event.pointerId);
          }
        };
        slider.addEventListener("pointerup", stopDrag);
        slider.addEventListener("pointercancel", stopDrag);
        slider.addEventListener("keydown", (event) => {
          if (event.key === "ArrowLeft" || event.key === "ArrowDown") {
            event.preventDefault();
            sendValue(clampSliderValue(Number(slider.dataset.value || 0) - 1));
          } else if (event.key === "ArrowRight" || event.key === "ArrowUp") {
            event.preventDefault();
            sendValue(clampSliderValue(Number(slider.dataset.value || 0) + 1));
          }
        });
        setSliderDisplay(slider, channel.current);
        label.appendChild(slider);
        section.appendChild(label);
      }
      widgetsEl.appendChild(section);
    }
  }
  for (const widget of widgets) {
    for (const channel of widget.channels || []) {
      const slider = [...widgetsEl.querySelectorAll(".slider-control")]
        .find((node) =>
          node.dataset.widgetId === widget.widgetId &&
          node.dataset.channel === channel.channel
        );
      if (!slider || slider.dataset.dragging === "true") continue;
      setSliderDisplay(slider, rangeValue(channel.current));
    }
  }
};

export const dispatch = (msg) => {
  const [next, effects] = update(env.model, msg);
  env.model = next;
  statusEl.textContent = env.model.status;
  renderWidgets(env.model);
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
    setViewMode: Msg.SetViewMode,
  })],
  [animationLoop({
    renderer: rendererHandle.renderer,
    render: rendererHandle.render,
    detectPinch: rendererHandle.detectPinch,
  })]
));
