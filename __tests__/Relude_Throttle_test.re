open Jest;
open Expect;

module Throttle = Relude.Throttle;

describe("Throttle", () => {
  test("throttle (leading=false)", () => {
    Jest.useFakeTimers();

    let runCount = ref(0);
    let f = () => {
      runCount := runCount^ + 1;
    };

    let runCount1 = runCount^;

    let throttled = Throttle.throttle(~delayMS=200, ~leading=false, f);

    throttled.f();
    throttled.f();
    throttled.f();

    let runCount2 = runCount^;

    Jest.advanceTimersByTime(100);

    throttled.f();
    throttled.f();
    throttled.f();

    let runCount3 = runCount^;

    Jest.advanceTimersByTime(100);

    throttled.f();
    throttled.f();
    throttled.f();

    let runCount4 = runCount^;

    expect((runCount1, runCount2, runCount3, runCount4))
    |> toEqual((0, 1, 1, 2));
  });

  test("throttle (leading=true)", () => {
    Jest.useFakeTimers();

    let runCount = ref(0);
    let f = () => {
      runCount := runCount^ + 1;
    };

    let runCount1 = runCount^;

    let throttled = Throttle.throttle(~delayMS=200, ~leading=true, f);

    throttled.f();
    throttled.f();
    throttled.f();

    let runCount2 = runCount^;

    Jest.advanceTimersByTime(100);

    throttled.f();
    throttled.f();
    throttled.f();

    let runCount3 = runCount^;

    Jest.advanceTimersByTime(100);

    throttled.f();
    throttled.f();
    throttled.f();

    let runCount4 = runCount^;

    expect((runCount1, runCount2, runCount3, runCount4))
    |> toEqual((0, 2, 2, 3)); // Not sure what the "right" behavior is for "leading=true" - maybe we don't even want that flag?
  });
});