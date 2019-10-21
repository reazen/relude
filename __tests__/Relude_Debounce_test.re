open Jest;
open Expect;

module Timer = Relude_Timer;
module Debounce = Relude_Debounce;

describe("Debounce", () => {
  testAsync("debounce (leading=false)", onDone => {
    Js.log("leading = false");
    let runCount = ref(0);
    let f = () => {
      runCount := runCount^ + 1;
      Js.log("runCount = " ++ string_of_int(runCount^));
    };
    let debounced = Debounce.Js.debounce(~delayMS=200, ~leading=false, f);
    Timer.Js.repeatTimes(~delayMS=50, ~times=4, debounced.f) |> ignore;
    Js.Global.setTimeout(
      () => onDone(expect(runCount^) |> toEqual(1)),
      500,
    )
    |> ignore;
  });

  testAsync("debounce (leading=true)", onDone => {
    Js.log("leading = true");
    let runCount = ref(0);
    let f = () => {
      runCount := runCount^ + 1;
      Js.log("runCount = " ++ string_of_int(runCount^));
    };
    let debounced = Debounce.Js.debounce(~delayMS=200, ~leading=true, f);
    Timer.Js.repeatTimes(~delayMS=50, ~times=4, debounced.f) |> ignore;
    Js.Global.setTimeout(
      () => onDone(expect(runCount^) |> toEqual(2)),
      500,
    )
    |> ignore;
  });
});