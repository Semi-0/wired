export const summaryText = (summary) => {
  if (!summary) return "unavailable";
  if (summary.kind === "nothing") return "nothing";
  if (summary.kind === "contradiction") return "contradiction";
  return String(summary.value ?? summary.current ?? summary.kind ?? "unavailable");
};

export const flattenViewPlanes = (views) =>
  (views || []).flatMap((view) =>
    view.type === "juxtapose"
      ? flattenViewPlanes(view.children || [])
      : [view]
  );

export const viewLines = (view) => {
  switch (view.type) {
    case "collection":
      if ((view.items || []).length === 0) {
        return [`${view.kind} · 0 items · ${view.pending || 0} pending`,
          "No visible items yet.", "Select a value in the selectable card."];
      }
      return [`${view.kind} · ${(view.items || []).length} items · ${view.pending || 0} pending`,
        ...(view.items || []).slice(0, 10).map((item) => summaryText(item.value))];
    case "cell-window":
      return [
        `strongest: ${summaryText(view.strongest)}`,
        `content: ${summaryText(view.content)}`,
      ];
    case "cell-history":
      return (view.samples || []).slice(-10).map((sample) =>
        `${sample["sample/epoch"]}:${sample["sample/tick"]}  ${summaryText(sample["sample/strongest"])}`
      );
    case "graph":
    case "hierarchy":
      return [`${view.graph?.nodes?.length || 0} nodes`, `${view.graph?.edges?.length || 0} relationships`];
    default:
      return [`unsupported view: ${view.type || "unknown"}`];
  }
};

export const viewFingerprint = (view) => JSON.stringify(view);
