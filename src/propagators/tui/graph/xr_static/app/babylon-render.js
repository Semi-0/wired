import { selectedNode } from "./model.js";
import { createBabylonGraphView } from "./babylon-graph-view.js";
import { createBabylonInput } from "./babylon-input.js";
import { createBabylonViewLayer } from "./babylon-views.js";
import { flattenViewPlanes } from "./views.js";
import { configureCameraMode } from "./camera-mode.js";

const requireBabylon = () => {
  if (!window.BABYLON) {
    throw new Error("Babylon.js did not load");
  }
  return window.BABYLON;
};

const graphKey = (model) =>
  `${model.viewMode}::${model.graph.nodes.map((node) => node.id).sort().join("|")}::${model.graph.edges.length}::${flattenViewPlanes(model.views).length}`;

// Preserve the desktop framing while allowing room along a narrow screen's width.
export const viewportFramingRadius = (radius, width, height) =>
  radius / Math.min(1, Math.max(width, 1) / Math.max(height, 1));

export const createRenderer = ({ root, selectionEl, dispatch }) => {
  const BABYLON = requireBabylon();
  const canvas = document.createElement("canvas");
  canvas.className = "xr-canvas";
  root.appendChild(canvas);

  const engine = new BABYLON.Engine(canvas, true);
  const scene = new BABYLON.Scene(engine);
  scene.clearColor = new BABYLON.Color4(0, 0, 0, 1);

  const camera = new BABYLON.ArcRotateCamera(
    "camera",
    -Math.PI / 3,
    Math.PI / 3,
    8,
    BABYLON.Vector3.Zero(),
    scene
  );
  camera.attachControl(canvas, true);
  camera.wheelPrecision = 45;
  camera.panningSensibility = 90;

  const ambient = new BABYLON.HemisphericLight("ambient", new BABYLON.Vector3(0, 1, 0), scene);
  ambient.intensity = 0.8;
  const key = new BABYLON.PointLight("key", new BABYLON.Vector3(0, 4, 8), scene);
  key.intensity = 1.8;

  const graphView = createBabylonGraphView({ BABYLON, scene });
  const viewLayer = createBabylonViewLayer({ BABYLON, scene, dispatch });
  const input = createBabylonInput({ BABYLON, scene, canvas, graphView, dispatch, camera });
  let framedGraphKey = "";
  let xrExperience = null;
  let cameraMode = null;

  const resize = () => {
    engine.resize();
    framedGraphKey = "";
  };
  window.addEventListener("resize", resize);

  const updateOrthographicBounds = () => {
    const halfHeight = Math.max(camera.radius * 0.6, 2.5);
    const width = Math.max(engine.getRenderWidth(), 1);
    const height = Math.max(engine.getRenderHeight(), 1);
    const halfWidth = halfHeight * (width / height);
    camera.orthoLeft = -halfWidth;
    camera.orthoRight = halfWidth;
    camera.orthoTop = halfHeight;
    camera.orthoBottom = -halfHeight;
  };

  const applyCameraMode = (viewMode) => {
    const nextMode = viewMode === "2d" ? "2d" : "3d";
    if (cameraMode === nextMode) {
      if (nextMode === "2d") updateOrthographicBounds();
      return;
    }
    cameraMode = nextMode;
    framedGraphKey = "";
    input.interaction.userAdjusted = false;
    configureCameraMode(BABYLON, camera, canvas, nextMode);
    if (nextMode === "2d") {
      updateOrthographicBounds();
    } else {
      // Perspective framing uses the existing camera radius.
    }
  };

  const frameGraph = (model) => {
    const key = graphKey(model);
    if (input.interaction.userAdjusted || framedGraphKey === key) return;
    const points = model.graph.nodes
      .map((node) => model.layout[node.id])
      .filter(Boolean)
      .map((p) => new BABYLON.Vector3(p.x, p.y, model.viewMode === "2d" ? 0 : p.z));
    const cards = flattenViewPlanes(model.views).length;
    if (cards > 0) {
      const extent = (cards - 1) * 3.55 / 2 + 1.6;
      points.push(new BABYLON.Vector3(-extent, -1.05, 0),
        new BABYLON.Vector3(extent, 1.05, 0));
      if (model.graph.nodes.length === 0) {
        camera.alpha = -Math.PI / 2;
        camera.beta = Math.PI / 2;
      }
    }
    if (points.length === 0) return;
    const min = points.reduce((a, p) => BABYLON.Vector3.Minimize(a, p), points[0].clone());
    const max = points.reduce((a, p) => BABYLON.Vector3.Maximize(a, p), points[0].clone());
    const center = min.add(max).scale(0.5);
    const radius = Math.max(max.subtract(min).length() * 1.05, 4.5);
    camera.target.copyFrom(center);
    camera.radius = viewportFramingRadius(radius, engine.getRenderWidth(), engine.getRenderHeight());
    if (model.viewMode === "2d") updateOrthographicBounds();
    framedGraphKey = key;
  };

  const enableFeature = (manager, name, options) => {
    try {
      manager.enableFeature(name, "latest", options, true, false);
    } catch (_error) {
      // ponytail: feature availability varies by browser; pointer selection is the fallback.
    }
  };

  const enterXr = async () => {
    xrExperience ||= await scene.createDefaultXRExperienceAsync({
      uiOptions: {
        sessionMode: "immersive-vr",
        referenceSpaceType: "local-floor",
      },
      optionalFeatures: true,
    });
    const manager = xrExperience.baseExperience.featuresManager;
    enableFeature(manager, BABYLON.WebXRFeatureName.HAND_TRACKING, {
      xrInput: xrExperience.input,
    });
    enableFeature(manager, BABYLON.WebXRFeatureName.NEAR_INTERACTION, {
      xrInput: xrExperience.input,
    });
    await xrExperience.baseExperience.enterXRAsync("immersive-vr", "local-floor");
  };

  const render = (model) => {
    input.setModel(model);
    applyCameraMode(model.viewMode);
    frameGraph(model);
    graphView.renderGraph(model);
    viewLayer.renderViews(model);
    const selected = selectedNode(model);
    selectionEl.textContent = selected ? JSON.stringify(selected, null, 2) : "no selection";
    key.position.copyFrom(camera.position);
  };

  return {
    renderer: {
      canvas,
      startAnimationLoop: (dispatch, renderModel) => {
        engine.runRenderLoop(() => {
          renderModel();
          scene.render();
        });
      },
      enterXr,
    },
    render,
    detectPinch: () => null,
    dispose: () => {
      window.removeEventListener("resize", resize);
      engine.dispose();
    },
  };
};
