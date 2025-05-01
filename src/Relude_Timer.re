// TODO: someday we should abstract the use of Js.Global timeout/interval from these functions, but
// for now, there's not an immediately pressing reason to do so.

/**
Delays the invocation of a function by [delayMS] milliseconds, and returns a
function to cancel the scheduled call.
*/
let delay = (~delayMS: int, f: unit => unit): (unit => unit) => {
  let timerId = Js.Global.setTimeout(~f, delayMS);
  () => {
    Js.Global.clearTimeout(timerId);
  };
};

/**
Repeats a function every [delayMS] milliseconds, and returns a function to
cancel the repeat.
*/
let repeat = (~delayMS: int, f: unit => unit): (unit => unit) => {
  let timerId = Js.Global.setInterval(~f, delayMS);
  () => {
    Js.Global.clearInterval(timerId);
  };
};

/**
Repeats a function every [delayMS] milliseconds, up to [times] times, and
returns a function to cancel the repeat.
*/
let repeatTimes =
    (~delayMS: int, ~times: int, f: unit => unit): (unit => unit) => {
  let timerId = ref(None);
  let cancel = () => {
    timerId^ |> Relude_Option.forEach(Js.Global.clearInterval);
  };
  let callCount = ref(0);
  timerId :=
    Some(
      Js.Global.setInterval(
        ~f=
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
