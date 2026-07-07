import { Msg } from "./msg.js";
import { isWidget, widgetChannels, widgetId } from "./widgets.js";

export const localPoint = (BABYLON, mesh, point) => {
  if (!mesh || !point) return null;
  const inverse = mesh.getWorldMatrix().clone();
  inverse.invert();
  return BABYLON.Vector3.TransformCoordinates(point, inverse);
};

export const widgetInputValue = (local) => {
  if (!local) return 0;
  const x = Math.max(0, Math.min(1, (local.x + 0.37) / 0.74));
  return Math.round(x * 100);
};

export const widgetChannelForLocalPoint = (node, local) => {
  const channels = widgetChannels(node);
  if (!local || channels.length <= 1) return channels[0];
  const rowHeight = 0.18;
  const top = ((channels.length - 1) * rowHeight) / 2;
  const index = Math.max(
    0,
    Math.min(channels.length - 1, Math.round((top - local.y) / rowHeight))
  );
  return channels[index];
};

export const createBabylonInput = ({ BABYLON, scene, canvas, graphView, dispatch, camera }) => {
  const interaction = {
    widgetDrag: null,
    widgetChannel: null,
    userAdjusted: false,
  };
  let lastModel = null;

  const nodeById = (id) => lastModel?.graph?.nodes?.find((node) => node.id === id);

  const selectedMesh = (pickInfo) => {
    const mesh = pickInfo?.pickedMesh;
    const nodeId = mesh?.metadata?.nodeId || mesh?.parent?.metadata?.nodeId;
    return nodeId ? graphView.nodeMeshes.get(nodeId) : null;
  };

  const sendWidgetInput = (node, mesh, pickedPoint, channelName = null) => {
    const id = widgetId(node);
    if (!id) return;
    const local = localPoint(BABYLON, mesh, pickedPoint);
    const channel = channelName || widgetChannelForLocalPoint(node, local)?.channel || "value";
    dispatch(Msg.WidgetInput(id, channel, widgetInputValue(local)));
  };

  scene.onPointerObservable.add((pointerInfo) => {
    const type = pointerInfo.type;
    const pickInfo = pointerInfo.pickInfo;
    if (type === BABYLON.PointerEventTypes.POINTERDOWN) {
      const mesh = selectedMesh(pickInfo);
      const node = nodeById(mesh?.metadata?.nodeId);
      if (isWidget(node)) {
        const local = localPoint(BABYLON, mesh, pickInfo.pickedPoint);
        const channel = widgetChannelForLocalPoint(node, local)?.channel || "value";
        interaction.widgetDrag = node.id;
        interaction.widgetChannel = channel;
        dispatch(Msg.SelectNode(node.id));
        sendWidgetInput(node, mesh, pickInfo.pickedPoint, channel);
      } else if (node) {
        dispatch(Msg.SelectNode(node.id));
      } else {
        interaction.userAdjusted = true;
      }
    }

    if (type === BABYLON.PointerEventTypes.POINTERMOVE && interaction.widgetDrag) {
      const node = nodeById(interaction.widgetDrag);
      const mesh = graphView.nodeMeshes.get(interaction.widgetDrag);
      if (node && mesh) {
        const pick = scene.pick(scene.pointerX, scene.pointerY, (candidate) =>
          candidate === mesh || candidate.parent === mesh
        );
        sendWidgetInput(node, mesh, pick?.pickedPoint, interaction.widgetChannel);
      }
    }

    if (
      type === BABYLON.PointerEventTypes.POINTERUP ||
      type === BABYLON.PointerEventTypes.POINTERDOUBLETAP
    ) {
      interaction.widgetDrag = null;
      interaction.widgetChannel = null;
    }
  });

  canvas.addEventListener("wheel", () => {
    interaction.userAdjusted = true;
  });
  camera.onViewMatrixChangedObservable.add(() => {
    if (scene.pointerX || scene.pointerY) interaction.userAdjusted = true;
  });

  return {
    interaction,
    setModel: (model) => {
      lastModel = model;
    },
  };
};
