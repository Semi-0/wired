// A non-modal disclosure: the graph remains interactive behind the controls.
export const bindMobileControls = ({ toggle, panel, viewport, modes, media, document }) => {
  const setOpen = (open) => {
    panel.classList.toggle("is-open", open);
    toggle.setAttribute("aria-expanded", String(open));
    if (open) {
      toggle.textContent = "Close";
    } else {
      toggle.textContent = "Views";
    }
  };
  const close = () => {
    if (media.matches && toggle.getAttribute("aria-expanded") === "true") {
      if (panel.contains(document.activeElement)) {
        toggle.focus();
      } else {
        // Leave focus on the graph or the disclosure button.
      }
      setOpen(false);
    } else {
      // Desktop controls and an already closed disclosure need no change.
    }
  };
  toggle.addEventListener("click", () => {
    setOpen(toggle.getAttribute("aria-expanded") !== "true");
  });
  viewport.addEventListener("pointerdown", close);
  for (const mode of modes) {
    mode.addEventListener("click", close);
  }
  document.addEventListener("keydown", (event) => {
    switch (event.key) {
      case "Escape": close(); break;
      default: break;
    }
  });
  media.addEventListener("change", () => setOpen(false));
  setOpen(false);
};
