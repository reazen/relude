open Jest;
open Expect;

module NonEmptyList = Relude_NonEmpty.List;
module String = Relude_String;
module Result = Relude_Result;
module Option = Relude_Option;
module Validation = Relude_Validation;

module Error = {
  type t =
    | InvalidName(string)
    | InvalidAge(int)
    | InvalidLanguage(string);

  // This is used below to create the Apply instance for the Validation
  module Type: Bastet.Interface.TYPE with type t = t = {
    type nonrec t = t;
  };
};

type errors = NonEmptyList.t(Error.t);

// Get the Infix operators for our Validation, using a NonEmptyList as our Semigroup for
// collecting errors, and the Error.t type as our error type
module ValidationE =
  Validation.WithErrors(NonEmptyList.SemigroupAny, Error.Type);

// map operator
let (<$>) = ValidationE.Infix.(<$>);

// apply operator
let (<*>) = ValidationE.Infix.(<*>);

module Person = {
  type t = {
    name: string,
    age: int,
    language: string,
  };

  // Think of this as a function of type `a => (b => (c => d))`
  // Given an `a` I can partially apply it and give you a function of type `b => (c => d)`
  // Given a `b` I can partially apply it and give you `c => d`
  // Finally, given a `c` I can give you a `d` (Person)
  //
  // You can map this function over any Functor
  // e.g. for a list
  //
  // ["Andy", "Bob"] |> Relude.List.map(makeWithNoValidation), and you will end up
  // with a `list(b => (c => d))` functions
  //
  // Remember that Validation is just a Functor that happens to have an extra error type that it carries along
  let makeWithNoValidation: (string, int, string) => t =
    (name, age, language) => {name, age, language};

  let validateName: string => Validation.t(string, errors) =
    name =>
      if (name |> String.isWhitespace) {
        VError(NonEmptyList.pure(Error.InvalidName(name)));
      } else {
        VOk(name);
      };

  let validateAge: int => Validation.t(int, errors) =
    age =>
      if (age < 0 || age > 120) {
        VError(NonEmptyList.pure(Error.InvalidAge(age)));
      } else {
        VOk(age);
      };

  let validateLanguage: string => Validation.t(string, errors) =
    lang =>
      if (lang == "English" || lang == "Spanish") {
        VOk(lang);
      } else {
        VError(NonEmptyList.pure(Error.InvalidLanguage(lang)));
      };

  let makeWithValidation: (string, int, string) => Validation.t(t, errors) =
    (name, age, language) => {
      // This is our function `a => (b => (c => d))`
      makeWithNoValidation
      // We map it over a Validation functor, so now we have `Validation.t(b => (c => d), errors)`
      // Notice that this looks like a function wrapped in our functor (e.g. f (x => y)), where x is b and y is (c => d)
      <$> validateName(name)  // map
      // In order to apply a function that is wrapped in our Functor, we use the apply function aka <*>
      // Now we have a `Validation.t(c => d, errors)`
      <*> validateAge(age)  // apply
      // Apply the wrapped function again
      <*> validateLanguage(language); // apply
                                     // You can keep going with <*> if there are more values to validate to construct our `Person`
    };

  let makeWithValidation2: (string, int, string) => Validation.t(t, errors) =
    (name, age, language) => {
      // This is an example that doesn't use the map <$> and apply <*> operators.
      // This looks and is clunky, and that's the main reason the operator version is preferred!
      ValidationE.map(makeWithNoValidation, validateName(name))
      ->ValidationE.apply(validateAge(age))
      ->ValidationE.apply(validateLanguage(language));
    };
};

describe("Validation", () => {
  test("isOk success", () =>
    expect(Validation.VOk(123)->Validation.isOk) |> toBe(true)
  );

  test("isOk error", () =>
    expect(Validation.VError("error")->Validation.isOk) |> toBe(false)
  );

  test("isError success", () =>
    expect(Validation.VError("error")->Validation.isError) |> toBe(true)
  );

  test("isError error", () =>
    expect(Validation.VOk(123)->Validation.isError) |> toBe(false)
  );

  test("tap success", () => {
    let x = ref(0);
    Validation.VOk(123) |> Validation.tap(i => x := i) |> ignore;
    expect(x^) |> toEqual(123);
  });

  test("tap error", () => {
    let x = ref(0);
    Validation.VError(123) |> Validation.tap(i => x := i) |> ignore;
    expect(x^) |> toEqual(0);
  });

  test("mapError success", () => {
    let actual = Validation.VError(123) |> Validation.mapError(i => i + 1);
    expect(actual) |> toEqual(Validation.VError(124));
  });

  test("mapError error", () => {
    let actual = Validation.VOk(123) |> Validation.mapError(i => i + 1);
    expect(actual) |> toEqual(Validation.VOk(123));
  });

  test("mapErrorsNel success", () => {
    let actual =
      Validation.VError(NonEmptyList.pure(123))
      |> Validation.mapErrorsNel(i => i + 1);
    expect(actual) |> toEqual(Validation.VError(NonEmptyList.pure(124)));
  });

  test("mapErrorsNel error", () => {
    let actual = Validation.VOk(123) |> Validation.mapErrorsNel(i => i + 1);
    expect(actual) |> toEqual(Validation.VOk(123));
  });

  test("tapError success", () => {
    let x = ref(0);
    Validation.VError(123) |> Validation.tapError(i => x := i) |> ignore;
    expect(x^) |> toEqual(123);
  });

  test("tapError error", () => {
    let x = ref(0);
    Validation.VOk(123) |> Validation.tapError(i => x := i) |> ignore;
    expect(x^) |> toEqual(0);
  });

  test("bimap success", () => {
    expect(Validation.VOk(123) |> Validation.bimap(i => i + 1, i => i - 1))
    |> toEqual(Validation.VOk(124))
  });

  test("bimap error", () => {
    expect(
      Validation.VError(123) |> Validation.bimap(i => i + 1, i => i - 1),
    )
    |> toEqual(Validation.VError(122))
  });

  test("bitap success", () => {
    let x = ref(0);

    Validation.VOk(123)
    |> Validation.bitap(i => x := i + 1, i => x := i - 1)
    |> ignore;

    expect(x^) |> toEqual(124);
  });

  test("bitap error", () => {
    let x = ref(0);

    Validation.VError(123)
    |> Validation.bitap(i => x := i + 1, i => x := i - 1)
    |> ignore;

    expect(x^) |> toEqual(122);
  });

  test("bind/flatMap success", () => {
    let input = Validation.VOk(123);
    let f = i => Validation.VError(i + 1);
    let expected = Validation.VError(124);

    expect(Validation.bind(input, f)) |> toEqual(expected) |> ignore;
    expect(Validation.flatMap(f, input)) |> toEqual(expected);
  });

  test("bind/flatMap error", () => {
    let input = Validation.VError(123);
    let f = i => Validation.VOk(i + 1);
    let expected = Validation.VError(123);

    expect(Validation.bind(input, f)) |> toEqual(expected) |> ignore;
    expect(Validation.flatMap(f, input)) |> toEqual(expected);
  });

  test("fromResult ok", () => {
    expect(Validation.fromResult(Result.ok(123)))
    |> toEqual(Validation.VOk(123))
  });

  test("fromResult error", () => {
    expect(Validation.fromResult(Result.error("error")))
    |> toEqual(Validation.VError("error"))
  });

  test("toResult ok", () => {
    expect(Validation.toResult(Validation.VOk(123)))
    |> toEqual(Result.ok(123))
  });

  test("toResult error", () => {
    expect(Validation.toResult(Validation.VError("error")))
    |> toEqual(Result.error("error"))
  });

  test("fromOption some", () => {
    expect(Validation.fromOption("error", Option.some(123)))
    |> toEqual(Validation.VOk(123))
  });

  test("fromOption none", () => {
    expect(Validation.fromOption("error", Option.none))
    |> toEqual(Validation.VError("error"))
  });

  test("fold success", () => {
    let input = Validation.VOk(123);
    let errFcn = i => i + 1;
    let okFcn = i => i - 1;
    let expected = 122;

    expect(Validation.fold(errFcn, okFcn, input)) |> toEqual(expected);
  });

  test("fold error", () => {
    let input = Validation.VError(123);
    let errFcn = i => i + 1;
    let okFcn = i => i - 1;
    let expected = 124;

    expect(Validation.fold(errFcn, okFcn, input)) |> toEqual(expected);
  });

  test("flip success", () => {
    expect(Validation.flip(Validation.VOk(123)))
    |> toEqual(Validation.VError(123))
  });

  test("flip error", () => {
    expect(Validation.flip(Validation.VError(123)))
    |> toEqual(Validation.VOk(123))
  });

  test("map5 success", () => {
    expect(
      Validation.map5(
        NonEmptyList.concat,
        (a, b, c, d, e) => a + b + c + d + e,
        Validation.VOk(1),
        Validation.VOk(2),
        Validation.VOk(3),
        Validation.VOk(4),
        Validation.VOk(5),
      ),
    )
    |> toEqual(Validation.VOk(15))
  });

  test("map5 some error", () => {
    expect(
      Validation.map5(
        NonEmptyList.concat,
        (a, b, c, d, e) => a + b + c + d + e,
        Validation.VOk(1),
        Validation.VOk(2),
        Validation.VError(NonEmptyList.pure("error")),
        Validation.VOk(4),
        Validation.VOk(5),
      ),
    )
    |> toEqual(Validation.VError(NonEmptyList.pure("error")))
  });

  test("map5 all error", () => {
    expect(
      Validation.map5(
        NonEmptyList.concat,
        (a, b, c, d, e) => a + b + c + d + e,
        Validation.VError(NonEmptyList.pure("error1")),
        Validation.VError(NonEmptyList.pure("error2")),
        Validation.VError(NonEmptyList.pure("error3")),
        Validation.VError(NonEmptyList.pure("error4")),
        Validation.VError(NonEmptyList.pure("error5")),
      ),
    )
    |> toEqual(
         Validation.VError(
           NonEmptyList.make(
             "error1",
             ["error2", "error3", "error4", "error5"],
           ),
         ),
       )
  });

  testAll(
    "alignWithAppendErrors",
    [
      (
        Validation.ok(42),
        Validation.ok("a"),
        Validation.ok(Relude_Ior_Type.Both(42, "a")),
      ),
      (
        Validation.ok(42),
        Validation.error("fail2"),
        Validation.ok(Relude_Ior_Type.This(42)),
      ),
      (
        Validation.error("fail1"),
        Validation.ok("a"),
        Validation.ok(Relude_Ior_Type.That("a")),
      ),
      (
        Validation.error("fail1"),
        Validation.error("fail2"),
        Validation.error("fail1fail2"),
      ),
    ],
    ((inputA, inputB, expected)) => {
      let actual =
        Validation.alignWithAppendErrors((a, b) => a ++ b, inputA, inputB);
      expect(actual) |> toEqual(expected);
    },
  );

  testAll(
    "alignWithWithAppendErrors",
    [
      (Validation.ok(42), Validation.ok("99"), Validation.ok(141)),
      (Validation.ok(42), Validation.error("fail2"), Validation.ok(42)),
      (Validation.error("fail1"), Validation.ok("99"), Validation.ok(99)),
      (
        Validation.error("fail1"),
        Validation.error("fail2"),
        Validation.error("fail1fail2"),
      ),
    ],
    ((inputA, inputB, expected)) => {
      let f =
        fun
        | Relude_Ior_Type.This(a) => a
        | Relude_Ior_Type.That(b) => int_of_string(b)
        | Relude_Ior_Type.Both(a, b) => a + int_of_string(b);
      let actual =
        Validation.alignWithWithAppendErrors(
          (a, b) => a ++ b,
          f,
          inputA,
          inputB,
        );
      expect(actual) |> toEqual(expected);
    },
  );

  test("makeWithValidation success", () => {
    let validation = Person.makeWithValidation("Andy", 55, "English");
    let expected: Person.t = {name: "Andy", age: 55, language: "English"};
    expect(validation) |> toEqual(Validation.VOk(expected));
  });

  test("makeWithValidation one error", () => {
    let validation = Person.makeWithValidation("Andy", 200, "English");
    let expected: errors = NonEmptyList.pure(Error.InvalidAge(200));
    expect(validation) |> toEqual(Validation.VError(expected));
  });

  test("makeWithValidation two errors", () => {
    let validation = Person.makeWithValidation("Andy", 200, "French");
    let expected: errors =
      NonEmptyList.make(
        Error.InvalidAge(200),
        [Error.InvalidLanguage("French")],
      );
    expect(validation) |> toEqual(Validation.VError(expected));
  });

  test("makeWithValidation all errors", () => {
    let validation = Person.makeWithValidation("", 200, "French");
    let expected: errors =
      NonEmptyList.make(
        Error.InvalidName(""),
        [Error.InvalidAge(200), Error.InvalidLanguage("French")],
      );
    expect(validation) |> toEqual(Validation.VError(expected));
  });

  test("makeWithValidation2 success", () => {
    let validation = Person.makeWithValidation2("Andy", 55, "English");
    let expected: Person.t = {name: "Andy", age: 55, language: "English"};
    expect(validation) |> toEqual(Validation.VOk(expected));
  });

  test("makeWithValidation2 one error", () => {
    let validation = Person.makeWithValidation2("Andy", 200, "English");
    let expected: errors = NonEmptyList.pure(Error.InvalidAge(200));
    expect(validation) |> toEqual(Validation.VError(expected));
  });

  test("makeWithValidation2 two errors", () => {
    let validation = Person.makeWithValidation2("Andy", 200, "French");
    let expected: errors =
      NonEmptyList.make(
        Error.InvalidAge(200),
        [Error.InvalidLanguage("French")],
      );
    expect(validation) |> toEqual(Validation.VError(expected));
  });

  test("makeWithValidation2 all errors", () => {
    let validation = Person.makeWithValidation2("", 200, "French");
    let expected: errors =
      NonEmptyList.make(
        Error.InvalidName(""),
        [Error.InvalidAge(200), Error.InvalidLanguage("French")],
      );
    expect(validation) |> toEqual(Validation.VError(expected));
  });
});
