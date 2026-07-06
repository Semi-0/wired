export const none = () => [];

export const batch = (...effects) => effects.flat().filter(Boolean);

export const command = (run) => ({ run });

export const mapEffect = (effect, map) =>
  command((dispatch, env) => effect.run((msg) => dispatch(map(msg)), env));

const socketOpen = (socket) => {
  if (!socket) return false;
  if (typeof WebSocket === "undefined") return socket.readyState === 1;
  return socket.readyState === WebSocket.OPEN;
};

const sendSocketJson = (socket, payload) => {
  if (socketOpen(socket)) {
    socket.send(JSON.stringify(payload));
  }
};

export const socketSend = (payload) =>
  command((_dispatch, env) => {
    sendSocketJson(env.socket, payload);
  });

export const runtimeCommand = (op, payload = {}) =>
  socketSend({ op, ...payload });

export const coalescedRuntimeCommand = (key, op, payload = {}, waitMs = 50) =>
  command((_dispatch, env) => {
    env.coalescedRuntimeCommands ||= {};
    const pending = env.coalescedRuntimeCommands;
    if (pending[key]?.timer) {
      clearTimeout(pending[key].timer);
    }
    pending[key] = {
      payload: { op, ...payload },
      timer: setTimeout(() => {
        const latest = pending[key]?.payload;
        delete pending[key];
        if (latest) sendSocketJson(env.socket, latest);
      }, waitMs),
    };
  });

export const withSelection = (model, f) => {
  if (!model.selectedId) return [model, none()];
  return f(model.selectedId);
};
