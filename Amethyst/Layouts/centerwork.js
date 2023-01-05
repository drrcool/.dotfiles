function layout() {
  return {
    name: "Centerwork",
    getFrameAssignments: (windows, screenFrame) => {
      const windowWidth = (index) => {
        if (index === 0) {
          return unitFrameWidth * 3;
        }
        return unitFrameWidth;
      };
      const windowHeight = (index) => {
        if (index === 0) {
          return screenFrame.height;
        }
        if (index % 2 === 1) {
          return leftSideFrameHeight;
        }
        return rightSideFrameHeight;
      };

      const windowX = (index) => {
        if (index === 0) {
          return unitFrameWidth;
        }
        if (index % 2 === 1) {
          return 0;
        }
        return unitFrameWidth + centerFrameWidth;
      };
      const rowNum = (index) => {
        if (index < 3) {
          return 0;
        }
        if (index % 2 === 1) {
          return (index - 1) / 2;
        }
        return (index - 2) / 2;
      };

      const windowY = (index) => {
        return rowNum(index) * windowHeight(index);
      };
      const unitFrameWidth = screenFrame.width / 5;
      const centerFrameWidth = unitFrameWidth * 3;

      const sideWindowCounts = windows.length > 1 ? windows.length - 1 : 1;
      const leftSideWindowCount = Math.ceil(sideWindowCounts / 2);
      const rightSideWindowCount =
        sideWindowCounts - parseInt(leftSideWindowCount);
      const leftSideFrameHeight =
        (1.0 * screenFrame.height) / parseInt(leftSideWindowCount);
      const rightSideFrameHeight =
        (1.0 * screenFrame.height) / parseInt(rightSideWindowCount);

      const frames = windows.map((window, index) => {
        const width = windowWidth(index);
        const height = windowHeight(index);
        const x = screenFrame.x + windowX(index);
        const y = screenFrame.y + windowHeight(index) * rowNum(index);
        const frame = { x, y, width, height };
        return { [window.id]: frame };
      });
      return frames.reduce((frames, frame) => ({ ...frames, ...frame }), {});
    },
  };
}

const thisLayout = layout();
const windows = Array.from({ length: 4 }).map((_, index) => ({ id: index }));
const screenFrame = { x: 0, y: 0, width: 5220, height: 1440 };
const windowLayout = thisLayout.getFrameAssignments(windows, screenFrame);
