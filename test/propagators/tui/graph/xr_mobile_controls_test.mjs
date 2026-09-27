import assert from "node:assert/strict";
import test from "node:test";
import { bindMobileControls } from "../../../../src/propagators/tui/graph/xr_static/app/mobile-controls.js";
import { viewportFramingRadius } from "../../../../src/propagators/tui/graph/xr_static/app/babylon-render.js";

test("portrait camera framing compensates for width without changing desktop framing", () => {
  assert.equal(viewportFramingRadius(8, 1200, 800), 8);
  assert.equal(viewportFramingRadius(8, 400, 800), 16);
  assert.equal(viewportFramingRadius(8, 800, 800), 8);
  assert.equal(Number.isFinite(viewportFramingRadius(8, 0, 0)), true);
});

const element = () => {
  const listeners = new Map();
  const attributes = new Map();
  const classes = new Set();
  return {
    textContent: "",
    focused: false,
    classList: { toggle: (name, enabled) => {
      if (enabled) { classes.add(name); } else { classes.delete(name); }
    }, contains: (name) => classes.has(name) },
    addEventListener: (name, handler) => listeners.set(name, handler),
    emit: (name, event = {}) => listeners.get(name)(event),
    setAttribute: (name, value) => attributes.set(name, value),
    getAttribute: (name) => attributes.get(name),
    contains: () => false,
    focus() { this.focused = true; },
  };
};

const setup = () => {
  const controls = {
    toggle: element(), panel: element(), viewport: element(),
    modes: [element(), element(), element()],
    media: Object.assign(element(), { matches: true }), document: element(),
  };
  bindMobileControls(controls);
  return controls;
};

test("mobile controls start closed and toggle with accessible state", () => {
  const { toggle, panel } = setup();
  assert.equal(toggle.getAttribute("aria-expanded"), "false");
  assert.equal(toggle.textContent, "Views");
  toggle.emit("click");
  assert.equal(toggle.getAttribute("aria-expanded"), "true");
  assert.equal(panel.classList.contains("is-open"), true);
  assert.equal(toggle.textContent, "Close");
  toggle.emit("click");
  assert.equal(panel.classList.contains("is-open"), false);
});

test("each mode closes the menu and returns focus from hidden controls", () => {
  const { toggle, panel, modes } = setup();
  panel.contains = () => true;
  for (const mode of modes) {
    toggle.emit("click");
    toggle.focused = false;
    mode.emit("click");
    assert.equal(toggle.getAttribute("aria-expanded"), "false");
    assert.equal(toggle.focused, true);
  }
});

test("Escape and touching the graph dismiss; other keys do not", () => {
  const { toggle, document, viewport } = setup();
  toggle.emit("click");
  document.emit("keydown", { key: "Tab" });
  assert.equal(toggle.getAttribute("aria-expanded"), "true");
  document.emit("keydown", { key: "Escape" });
  assert.equal(toggle.getAttribute("aria-expanded"), "false");
  toggle.emit("click");
  viewport.emit("pointerdown");
  assert.equal(toggle.getAttribute("aria-expanded"), "false");
});

test("desktop mode selection keeps focus; breakpoint changes reset disclosure", () => {
  const { toggle, panel, modes, media } = setup();
  toggle.emit("click");
  media.matches = false;
  media.emit("change");
  assert.equal(panel.classList.contains("is-open"), false);
  panel.contains = () => true;
  modes[0].emit("click");
  assert.equal(toggle.focused, false);
  media.matches = true;
  media.emit("change");
  assert.equal(toggle.getAttribute("aria-expanded"), "false");
});
