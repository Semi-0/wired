export const isWidget = (node) =>
  node.kind === "widget" || node.ui?.kind === "widget";

export const widgetId = (node) =>
  node?.ui?.widgetId || node?.ui?.["widget-id"] || node?.ui?.id;

export const widgetChannels = (node) =>
  Array.isArray(node?.ui?.channels) && node.ui.channels.length > 0
    ? node.ui.channels
    : [{ channel: "value", current: 0 }];

export const channelValueAt = (node, index) => {
  const raw = widgetChannels(node)[index]?.current;
  const n = Number(raw);
  return Number.isFinite(n) ? Math.max(0, Math.min(100, n)) : 0;
};
