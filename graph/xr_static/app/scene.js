import * as THREE from "three";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";

export const createSceneKit = ({ root }) => {
  const scene = new THREE.Scene();
  scene.background = new THREE.Color(0x000000);

  const camera = new THREE.PerspectiveCamera(65, 1, 0.01, 1000);
  const renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.xr.enabled = true;
  renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));
  root.appendChild(renderer.domElement);

  scene.add(new THREE.AmbientLight(0xffffff, 0.45));
  const keyLight = new THREE.PointLight(0xffffff, 2.4, 80);
  keyLight.position.set(0, 4, 8);
  scene.add(keyLight);

  const group = new THREE.Group();
  scene.add(group);

  const controls = new OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.dampingFactor = 0.08;
  controls.screenSpacePanning = true;
  controls.mouseButtons = {
    LEFT: THREE.MOUSE.ROTATE,
    MIDDLE: THREE.MOUSE.DOLLY,
    RIGHT: THREE.MOUSE.PAN,
  };
  controls.touches = {
    ONE: THREE.TOUCH.ROTATE,
    TWO: THREE.TOUCH.DOLLY_PAN,
  };
  controls.target.set(0, 0, 0);

  const updateLight = () => {
    keyLight.position.copy(camera.position);
  };

  camera.position.set(3.4, 1.7, 7.2);
  controls.update();
  updateLight();

  const resize = () => {
    const { width, height } = root.getBoundingClientRect();
    renderer.setSize(width, height, false);
    camera.aspect = width / Math.max(height, 1);
    camera.updateProjectionMatrix();
  };

  window.addEventListener("resize", resize);
  resize();

  return {
    scene,
    camera,
    renderer,
    group,
    controls,
    resize,
    updateLight,
  };
};
