open Jest;
open Expect;

module NonEmptyList = Relude_NonEmpty.List;
module String = Relude_String;
module Result = Relude_Result;
module Validation = Relude_Validation;

module Error = {
  type t =
    | InvalidName(string)
    | InvalidAge(int)
    | InvalidLanguage(string);

  // This is used below to create the Apply instance for the Validation
  module Type: BsAbstract.Interface.TYPE with type t = t = {
    type nonrec t = t;
  };
};

type errors = NonEmptyList.t(Error.t);

// Get the Infix operators for our Validation, using a NonEmptyList as our Semigroup for
// collecting errors, and the Error.t type as our error type
module Infix = Validation.Infix(NonEmptyList.SemigroupAny, Error.Type);

// map operator
let (<$>) = Infix.Functor.(<$>);

// apply operator
let (<*>) = Infix.Apply.(<*>);

module Person = {
  type t = {
    name: string,
    age: int,
    language: string,
  };

  // Think of this as a function of type `a => (b => (c => d)`
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
};

describe("Validation", () => {
  test("isOk success", () =>
    expect(Validation.VOk(123)->Validation.isOk) |> toBe(true)
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
});