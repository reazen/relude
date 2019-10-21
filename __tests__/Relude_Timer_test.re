open Jest;
open Expect;

module Timer = Relude_Timer;

// TODO: these tests are currently time-sensitive and therefore flaky/fragile
// bs-jest has Jest.useFakeTimers, but it doesn't have the `toHaveBeenCalled` matcher implemented

describe("Timer", () => {
  testAsync("delay", onDone => {
    let wasRun = ref(false);
    let f = () => {
      wasRun := true;
    };
    Timer.Js.delay(~delayMS=20, f) |> ignore;
    Js.Global.setTimeout(
      () => onDone(expect(wasRun^) |> toEqual(true)),
      30,
    )
    |> ignore;
  });

  testAsync("repeat", onDone => {
    let runCount = ref(0);
    let f = () => {
      runCount := runCount^ + 1;
    };
    Timer.Js.repeat(~delayMS=50, f) |> ignore;
    Js.Global.setTimeout(
      () => onDone(expect(runCount^) |> toEqual(3)),
      175,
    )
    |> ignore;
  });

  testAsync("repeatTimes", onDone => {
    let runCount = ref(0);
    let f = () => {
      runCount := runCount^ + 1;
    };
    Timer.Js.repeatTimes(~delayMS=50, ~times=3, f) |> ignore;
    Js.Global.setTimeout(
      () => onDone(expect(runCount^) |> toEqual(3)),
      300,
    )
    |> ignore;
  });
});