function layout() {
  return {
    name: "Three Columns Left",
    getFrameAssignments: (windows, screenFrame) => {
      const columnWidth = screenFrame.width / 5;
      const sideWindows = windows.length > 2 ? windows.length - 2 : 1;
      const frameHeight = screenFrame.height / sideWindows;
      const frames = windows.map((window, index) => {
        const width =
          index === 1
            ? columnWidth
            : index === 0
            ? columnWidth * 3
            : columnWidth;
        const x =
          index === 1
            ? screenFrame.x
            : index === 0
            ? screenFrame.x + columnWidth
            : screenFrame.x + 4 * columnWidth;
        const y =
          index <= 1
            ? screenFrame.y
            : screenFrame.y + frameHeight * (index - 2);

        const height = index <= 1 ? screenFrame.height : frameHeight;

        const frame = { x, y, width, height };

        return { [window.id]: frame };
      });
      return frames.reduce((frames, frame) => ({ ...frames, ...frame }), {});
    },
  };
}
const thisLayout = layout();
const windows = Array.from({ length: 7 }).map((_, index) => ({ id: index }));
const screenFrame = { x: 0, y: 0, width: 5220, height: 1440 };

const windowLayout = thisLayout.getFrameAssignments(windows, screenFrame);
