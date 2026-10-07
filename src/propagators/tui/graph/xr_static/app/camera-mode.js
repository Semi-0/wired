// 2D interaction pans a fixed viewing plane; 3D interaction orbits it.
export const configureCameraMode = (BABYLON, camera, canvas, mode) => {
  if (mode !== "2d" && mode !== "3d") {
    throw new Error(`Unsupported camera mode: ${mode}`);
  }

  // Detaching also clears inertia left over from the previous gesture.
  camera.detachControl();
  if (mode === "2d") {
    camera.mode = BABYLON.Camera.ORTHOGRAPHIC_CAMERA;
    camera.alpha = -Math.PI / 2;
    camera.beta = Math.PI / 2;
    camera.lowerAlphaLimit = camera.alpha;
    camera.upperAlphaLimit = camera.alpha;
    camera.lowerBetaLimit = camera.beta;
    camera.upperBetaLimit = camera.beta;
    camera.attachControl(canvas, true, false, 0);
  } else {
    camera.mode = BABYLON.Camera.PERSPECTIVE_CAMERA;
    camera.lowerAlphaLimit = null;
    camera.upperAlphaLimit = null;
    camera.lowerBetaLimit = 0.01;
    camera.upperBetaLimit = Math.PI - 0.01;
    camera.alpha = -Math.PI / 3;
    camera.beta = Math.PI / 3;
    camera.attachControl(canvas, true, true, 2);
  }
};
