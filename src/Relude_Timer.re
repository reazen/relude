// TODO: it might make sense to make this a module functor to abstract the low-level startTimer/cancelTimer functions
// For now, I just have a Js-based specialization at the bottom

/**
 * Delays the invocation of a function by `delayMS` milliseconds, and returns a function to cancel
 * the scheduled call.
 */
let delay =
    (
      ~setTimeout: (unit => unit, int) => 'timerId,
      ~clearTimeout: 'timerId => unit,
      ~delayMS: int,
      f: unit => unit,
    )
    : (unit => unit) => {
  let timerId = setTimeout(f, delayMS);
  () => {
    clearTimeout(timerId);
  };
};

/**
 * Repeats a function every `delayMS` milliseconds, and returns a function to cancel the repeat.
 */
let repeat =
    (
      ~setInterval: (unit => unit, int) => 'timerId,
      ~clearInterval: 'timerId => unit,
      ~delayMS: int,
      f: unit => unit,
    )
    : (unit => unit) => {
  let timerId = setInterval(f, delayMS);
  () => {
    clearInterval(timerId);
  };
};

/**
 * Repeats a function every `delayMS` milliseconds, up to `times` times, and returns a function to cancel the
 * repeat.
 */
let repeatTimes =
    (
      ~setInterval: (unit => unit, int) => 'timerId,
      ~clearInterval: 'timerId => unit,
      ~delayMS: int,
      ~times: int,
      f: unit => unit,
    )
    : (unit => unit) => {
  let timerId = ref(None);
  let cancel = () => {
    timerId^ |> Relude_Option.forEach(clearInterval);
  };
  let callCount = ref(0);
  timerId :=
    Some(
      setInterval(
        () => {
          f();
          callCount := callCount^ + 1;
          if (callCount^ == times) {
            cancel();
          };
        },
        delayMS,
      ),
    );
  cancel;
};

// JS-specific implementations - TODO: we should probably move this somewhere else
module Js = {
  let setTimeout = Js.Global.setTimeout;
  let clearTimeout = Js.Global.clearTimeout;
  let setInterval = Js.Global.setInterval;
  let clearInterval = Js.Global.clearInterval;

  let delay: (~delayMS: int, unit => unit, unit) => unit =
    delay(~setTimeout, ~clearTimeout);

  let repeat: (~delayMS: int, unit => unit, unit) => unit =
    repeat(~setInterval, ~clearInterval);

  let repeatTimes: (~delayMS: int, ~times: int, unit => unit, unit) => unit =
    repeatTimes(~setInterval, ~clearInterval);
};