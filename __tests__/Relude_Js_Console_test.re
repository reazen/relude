open Jest;
//open Expect;

module Console = Relude_Js_Console;
module IO = Relude_IO;

describe("Js.Console", () => {
  testAsync("log should work", onDone =>
    Console.IO.log("log test")
    |> IO.unsafeRunAsync(fun
    | Ok(_) => onDone(pass)
    | Error(_) => onDone(fail("error"))
    )
  );

  testAsync("warn should work", onDone =>
    Console.IO.warn("log warn test")
    |> IO.unsafeRunAsync(fun
    | Ok(_) => onDone(pass)
    | Error(_) => onDone(fail("error"))
    )
  );
});
