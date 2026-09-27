import { flattenViewPlanes, summaryText } from "./views.js";
import { Msg } from "./msg.js";

// Only display declarations and send item identities. No semantic transforms.
export const collectionControls = (root, dispatch) => {
  let previous = "";
  return (model) => {
    const views = flattenViewPlanes(model.views).filter((view) => view.type === "collection");
    const fingerprint = JSON.stringify(views);
    if (fingerprint === previous) return;
    previous = fingerprint;
    const sections = views.map((view) => {
      const section = document.createElement("section");
      const title = document.createElement("h2");
      title.textContent = `${view.kind} · ${view.items.length} items`;
      section.appendChild(title);
      if (view.items.length === 0) {
        const hint = document.createElement("p");
        hint.textContent = "No visible items yet. Select a value in the selectable card.";
        section.appendChild(hint);
      }
      for (const item of view.items) {
        let element;
        if (view.selectable) {
          element = document.createElement("button");
          element.type = "button";
          element.addEventListener("click", () => dispatch(Msg.ViewSelect(view, item.id)));
        } else {
          element = document.createElement("div");
        }
        if (view.kind === "graph" && item.label) {
          element.textContent = String(item.label);
        } else {
          element.textContent = summaryText(item.value);
        }
        element.title = item.id;
        section.appendChild(element);
      }
      return section;
    });
    root.replaceChildren(...sections);
  };
};
