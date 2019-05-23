open Jest;
open Expect;

type error = {message: string};

module IO = Relude.IO;
module OptionT = Relude.OptionT;

module IOE =
  IO.WithError({
    type t = error;
  });

module OptionIOE = OptionT.WithMonad(IOE.Monad);

let ((<$>), (<#>), (>>=)) = OptionIOE.Infix.((<$>), (<#>), (>>=));

describe("OptionT", () => {
  testAsync("make", onDone =>
    OptionIOE.make(IOE.pure(Some(2)))
    |> OptionIOE.map(a => expect(a) |> toEqual(2))
    |> OptionIOE.runOptionT
    |> IO.unsafeRunAsync(
         fun
         | Ok(Some(assertion)) => onDone(assertion)
         | Ok(None) => onDone(fail("none"))
         | Error(_x) => onDone(fail("error")),
       )
  );

  testAsync("map", onDone =>
    OptionIOE.pure(2)
    |> OptionIOE.map(a => a * 2)
    |> OptionIOE.map(a => expect(a) |> toEqual(4))
    |> OptionIOE.runOptionT
    |> IO.unsafeRunAsync(
         fun
         | Ok(Some(assertion)) => onDone(assertion)
         | _ => onDone(fail("fail")),
       )
  );

  testAsync("apply", onDone =>
    OptionIOE.pure(2)
    |> OptionIOE.apply(OptionIOE.pure(a => a * 2))
    |> OptionIOE.map(a => expect(a) |> toEqual(4))
    |> OptionIOE.runOptionT
    |> IO.unsafeRunAsync(
         fun
         | Ok(Some(assertion)) => onDone(assertion)
         | _ => onDone(fail("fail")),
       )
  );

  testAsync("pure", onDone =>
    OptionIOE.pure(pass)
    |> OptionIOE.runOptionT
    |> IO.unsafeRunAsync(
         fun
         | Ok(Some(assertion)) => onDone(assertion)
         | _ => onDone(fail("fail")),
       )
  );

  testAsync("bind", onDone =>
    OptionIOE.pure(2)
    |> OptionIOE.flatMap(a => OptionIOE.pure(a * 2))
    |> OptionIOE.map(a => expect(a) |> toEqual(4))
    |> OptionIOE.runOptionT
    |> IO.unsafeRunAsync(
         fun
         | Ok(Some(assertion)) => onDone(assertion)
         | _ => onDone(fail("fail")),
       )
  );

  testAsync("operators", onDone =>
    OptionIOE.pure(2)
    <#> (a => a * 3)
    >>= (a => OptionIOE.pure(a + 7))
    <#> (a => expect(a) |> toEqual(13))
    |> OptionIOE.runOptionT
    |> IO.unsafeRunAsync(
         fun
         | Ok(Some(assertion)) => onDone(assertion)
         | _ => onDone(fail("fail")),
       )
  );
});