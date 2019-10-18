type rafId;

[@bs.val]
external requestAnimationFrame: (float => unit) => rafId =
  "requestAnimationFrame";

[@bs.val]
external cancelAnimationFrame: rafId => unit = "cancelAnimationFrame";