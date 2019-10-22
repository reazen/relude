open Jest;
open Expect;

module Timer = Relude_Timer;

describe("Timer", () => {
  test("delay", () => {
    Jest.useFakeTimers();

    let wasRun = ref(false);
    let f = () => {
      wasRun := true;
    };

    let wasRun1 = wasRun^;
    Timer.Js.delay(~delayMS=50, f) |> ignore;

    Jest.advanceTimersByTime(40);
    let wasRun2 = wasRun^;

    Jest.advanceTimersByTime(40);
    let wasRun3 = wasRun^;

    expect((wasRun1, wasRun2, wasRun3)) |> toEqual((false, false, true));
  });

  test("repeat", () => {
    Jest.useFakeTimers();

    let runCount = ref(0);

    let f = () => {
      runCount := runCount^ + 1;
    };

    let runCount1 = runCount^; // 0 (0 hits)
    Timer.Js.repeat(~delayMS=50, f) |> ignore;

    Jest.advanceTimersByTime(40); // 40 (0 hits)
    let runCount2 = runCount^;

    Jest.advanceTimersByTime(40); // 80 (1 hit)
    let runCount3 = runCount^;

    Jest.advanceTimersByTime(40); // 120 (2 hits)
    let runCount4 = runCount^;

    Jest.advanceTimersByTime(40); // 160 (3 hits)
    let runCount5 = runCount^;

    Jest.advanceTimersByTime(40); // 200 (4 hits)
    let runCount6 = runCount^;

    expect((runCount1, runCount2, runCount3, runCount4, runCount5, runCount6))
    |> toEqual((0, 0, 1, 2, 3, 4));
  });

  test("repeatTimes", () => {
    Jest.useFakeTimers();

    let runCount = ref(0);

    let f = () => {
      runCount := runCount^ + 1;
    };

    let runCount1 = runCount^; // 0 (0 hits)
    Timer.Js.repeatTimes(~delayMS=50, ~times=3, f) |> ignore;

    Jest.advanceTimersByTime(40); // 40 (0 hits)
    let runCount2 = runCount^;

    Jest.advanceTimersByTime(40); // 80 (1 hit)
    let runCount3 = runCount^;

    Jest.advanceTimersByTime(40); // 120 (2 hits)
    let runCount4 = runCount^;

    Jest.advanceTimersByTime(40); // 160 (3 hits - done)
    let runCount5 = runCount^;

    Jest.advanceTimersByTime(40); // 200 (still 3)
    let runCount6 = runCount^;

    expect((runCount1, runCount2, runCount3, runCount4, runCount5, runCount6))
    |> toEqual((0, 0, 1, 2, 3, 3));
  });
});