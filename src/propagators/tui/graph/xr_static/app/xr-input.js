export const detectPinch = (renderer, frame, referenceSpace) => {
  if (!frame || !referenceSpace) return null;
  for (const source of renderer.xr.getSession()?.inputSources || []) {
    const hand = source.hand;
    if (!hand) continue;
    const thumb = hand.get("thumb-tip");
    const index = hand.get("index-finger-tip");
    const thumbPose = thumb && frame.getJointPose(thumb, referenceSpace);
    const indexPose = index && frame.getJointPose(index, referenceSpace);
    if (!thumbPose || !indexPose) continue;
    const a = thumbPose.transform.position;
    const b = indexPose.transform.position;
    const dx = a.x - b.x;
    const dy = a.y - b.y;
    const dz = a.z - b.z;
    if (Math.sqrt(dx * dx + dy * dy + dz * dz) < 0.035) {
      return {
        x: (a.x + b.x) / 2,
        y: (a.y + b.y) / 2,
        z: (a.z + b.z) / 2,
      };
    }
  }
  return null;
};
