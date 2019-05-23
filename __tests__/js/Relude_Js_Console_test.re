open Jest;

module Console = Relude.Js.Console;
module IO = Relude.IO;

// Skip these b/c they just write log messages, and don't actually assert anything.
Skip.describe("Js.Console", () => {
  testAsync("log should work", onDone =>
    Console.IO.log("log test")
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(pass)
         | Error(_) => onDone(fail("error")),
       )
  );

  testAsync("warn should work", onDone =>
    Console.IO.warn("log warn test")
    |> IO.unsafeRunAsync(
         fun
         | Ok(_) => onDone(pass)
         | Error(_) => onDone(fail("error")),
       )
  );
});