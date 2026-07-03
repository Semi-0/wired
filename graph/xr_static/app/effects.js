import { command } from "./combinators.js";
import { Msg } from "./msg.js";

export const connectSocket = () =>
  command((dispatch, env) => {
    const protocol = window.location.protocol === "https:" ? "wss" : "ws";
    const socket = new WebSocket(`${protocol}://${window.location.host}/ws`);
    env.socket = socket;
    socket.addEventListener("open", () => dispatch(Msg.SocketOpen()));
    socket.addEventListener("close", () => dispatch(Msg.SocketClosed()));
    socket.addEventListener("error", () => dispatch(Msg.SocketError("connection failed")));
    socket.addEventListener("message", (event) => {
      try {
        dispatch(Msg.SocketMessage(JSON.parse(event.data)));
      } catch (error) {
        dispatch(Msg.SocketError(error.message));
      }
    });
  });

const parseValue = (raw) => {
  const text = raw.trim();
  if (!text) return "";
  try {
    return JSON.parse(text);
  } catch {
    return raw;
  }
};

export const bindControls = ({ installTrace, extendGraph, sendValue, setViewMode }) =>
  command((dispatch) => {
    document.getElementById("install-trace").addEventListener("click", () => {
      const label = document.getElementById("trace-label").value.trim();
      if (label) dispatch(installTrace(label));
    });
    document.getElementById("extend-graph").addEventListener("click", () => {
      dispatch(extendGraph(document.getElementById("source").value));
    });
    document.getElementById("send-selected").addEventListener("click", () => {
      dispatch(sendValue(parseValue(document.getElementById("message-value").value)));
    });
    document.getElementById("view-3d").addEventListener("click", () => {
      dispatch(setViewMode("3d"));
    });
    document.getElementById("view-xr").addEventListener("click", () => {
      dispatch(setViewMode("xr"));
    });
  });

export const animationLoop = ({ renderer, render, detectPinch }) =>
  command((dispatch, env) => {
    let last = performance.now();
    let referenceSpace = null;

    if (navigator.xr) {
      navigator.xr.isSessionSupported("immersive-vr")
        .then((ready) => dispatch(Msg.XrReady(ready)))
        .catch(() => dispatch(Msg.XrReady(false)));
    }

    env.enterXr = async () => {
      if (!navigator.xr) {
        dispatch(Msg.XrError("navigator.xr is not available"));
        return;
      }
      try {
        const session = await navigator.xr.requestSession("immersive-vr", {
          optionalFeatures: ["local-floor", "hand-tracking"],
        });
        await renderer.xr.setSession(session);
        referenceSpace = await session.requestReferenceSpace("local-floor");
        dispatch(Msg.XrEntered());
      } catch (error) {
        dispatch(Msg.XrError(error.message));
      }
    };

    renderer.setAnimationLoop((time, frame) => {
      const dt = Math.max(0.001, Math.min((time - last) / 1000, 0.05));
      last = time;
      dispatch(Msg.Tick(dt));
      if (frame && referenceSpace) {
        const pinch = detectPinch(frame, referenceSpace);
        if (pinch) dispatch(Msg.Pinch(pinch, time));
      }
      render(env.model);
    });
  });

export const enterXrEffect = () =>
  command((_dispatch, env) => {
    if (env.enterXr) env.enterXr();
  });
