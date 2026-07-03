export const none = () => [];

export const batch = (...effects) => effects.flat().filter(Boolean);

export const command = (run) => ({ run });

export const mapEffect = (effect, map) =>
  command((dispatch, env) => effect.run((msg) => dispatch(map(msg)), env));

export const socketSend = (payload) =>
  command((_dispatch, env) => {
    if (env.socket && env.socket.readyState === WebSocket.OPEN) {
      env.socket.send(JSON.stringify(payload));
    }
  });

export const runtimeCommand = (op, payload = {}) =>
  socketSend({ op, ...payload });

export const withSelection = (model, f) => {
  if (!model.selectedId) return [model, none()];
  return f(model.selectedId);
};
