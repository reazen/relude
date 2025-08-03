type rafId;

external requestAnimationFrame: (float => unit) => rafId =
  "requestAnimationFrame";

external cancelAnimationFrame: rafId => unit = "cancelAnimationFrame";
