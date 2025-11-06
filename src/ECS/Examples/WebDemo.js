// FFI bindings for WebDemo canvas rendering

// Store the canvas context globally (set from HTML)
let globalCtx = null;

export const setGlobalContext = (ctx) => {
  globalCtx = ctx;
};

export const getCanvasContext = () => {
  if (!globalCtx) {
    const canvas = document.getElementById('canvas');
    if (canvas) {
      globalCtx = canvas.getContext('2d');
    } else {
      throw new Error('Canvas element not found');
    }
  }
  return globalCtx;
};

export const clearCanvas = ctx => width => height => () => {
  ctx.clearRect(0, 0, width, height);
};

export const fillBackground = ctx => color => width => height => () => {
  ctx.fillStyle = color;
  ctx.fillRect(0, 0, width, height);
};

export const drawCircle = ctx => x => y => radius => color => () => {
  ctx.fillStyle = color;
  ctx.beginPath();
  ctx.arc(x, y, radius, 0, Math.PI * 2);
  ctx.fill();
};

export const drawRect = ctx => x => y => width => height => color => () => {
  ctx.fillStyle = color;
  ctx.fillRect(x, y, width, height);
};

export const drawText = ctx => text => x => y => color => () => {
  ctx.fillStyle = color;
  ctx.font = '14px monospace';
  ctx.fillText(text, x, y);
};

export const requestAnimationFrame = callback => () => {
  window.requestAnimationFrame(timestamp => callback(timestamp)());
};

export const createRef = value => () => {
  return { value };
};

export const readRef = ref => () => {
  return ref.value;
};

export const writeRef = ref => value => () => {
  ref.value = value;
};
