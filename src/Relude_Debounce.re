type debounced = {
  cancel: unit => unit,
  flush: unit => unit,
  isScheduled: unit => bool,
  f: unit => unit,
};

/**
 * debounce takes a `unit => unit` function, and returns a new `unit => unit` function which
 * when called, will only invoke the given input function after a period of inactivity.
 *
 * The `leading` flag can be used to force one invocation of the function the first time the debounced
 * function is called.  If no other calls to the function are made, the function will be invoked again
 * after the delay.  TODO: not sure if this is desirable - maybe it should not be invoked again if it was only
 * called once and invoked on the leading edge.
 *
 * This is useful for waiting for a stream of events to "settle" before invoking some final
 * processing function.  Some examples of debouncing might include:
 *
 * 1. Waiting for a stream of keystrokes to stop before updating a view model, or making an async
 * call.
 * 2. Waiting for window resize events to settle before using the new screen size to make some
 * expensive computation you don't want to run for every incremental change.
 *
 * The setTimeout/cancelTimeout arguments should be supplied with the desired timer implementation,
 * e.g. Js.Global.setTimeout/clearTimeout.
 *
 * The value returned is a record with the following fields:
 *
 * - cancel - a function that can be used to cancel any current-scheduled debounced function invocations
 * - flush - a function that can be used to cancel any scheduled invocations and immediately invoke the function
 * - isScheduled - a function to check if the debounced function is currently scheduled to run in the future
 * - f - the debounced function to be used by the caller
 */
let debounce =
    (
      ~setTimeout: (unit => unit, int) => 'timerId,
      ~clearTimeout: 'timerId => unit,
      ~delayMS: int,
      ~leading: bool=false,
      f: unit => unit,
    )
    : debounced => {
  let timerId = ref(None);

  let cancel = () => {
    timerId^ |> Relude_Option.forEach(timerId => clearTimeout(timerId));
    timerId := None;
  };

  let schedule = () => {
    cancel();
    timerId := Some(setTimeout(f, delayMS));
  };

  let isScheduled = () => {
    timerId^ |> Relude_Option.isSome;
  };

  let flush = () => {
    cancel();
    f();
  };

  let doLeading = ref(leading);

  let debounced = () => {
    if (doLeading^) {
      doLeading := false;
      f();
    };
    schedule();
  };

  {f: debounced, flush, cancel, isScheduled};
};

module Js = {
  let setTimeout = Js.Global.setTimeout;

  let clearTimeout = Js.Global.clearTimeout;

  let debounce = debounce(~setTimeout, ~clearTimeout);
};

// TODO: might make sense to have a requestAnimationFrame version of this?