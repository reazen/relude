open Jest;
open Expect;
open! Relude.Globals;

module Validator = {
  type t('a) = string => result('a, string);
};

// The functor we will be using as our metadata type in the FreeAp
module Field = {
  type t('a) = {
    name: string,
    validator: Validator.t('a),
  };

  let make = (name, validator) => {name, validator};

  let map: 'a 'b. ('a => 'b, t('a)) => t('b) =
    (f, {name, validator}) => {
      name,
      validator: validator >> Result.map(f),
    };

  module Functor: BsBastet.Interface.FUNCTOR with type t('a) = t('a) = {
    type nonrec t('a) = t('a);
    let map = map;
  };
};

// Form is our free-applicative type that we'll use to describe the fields that
// we will support for form building.
module Schema = {
  include Free.Applicative.WithFunctor(Field.Functor);

  // A field is the metadata we capture in the freeap for describing a form
  let field: 'a. (string, Validator.t('a)) => t('a) =
    (name, validator) => liftF(Field.make(name, validator));

  // describes an operation that will parse a string to an int
  let int: string => t(int) =
    name =>
      field(name, input =>
        Relude.Int.fromString(input)
        |> Result.fromOption(
             "Invalid input for field " ++ name ++ " (expected int): " ++ input,
           )
      );

  // describes an operation that will parse a string and make sure it's not empty
  let nonEmptyString: string => t(string) =
    name =>
      field(name, input =>
        Some(input)
        |> Relude.Option.keep(String.isNotEmpty)
        |> Relude.Result.fromOption(
             "Invalid input for field "
             ++ name
             ++ " (expected non-empty): "
             ++ input,
           )
      );
};

module User = {
  type t = {
    first: string,
    last: string,
    age: int,
  };

  let make = (first, last, age) => {first, last, age};

  // This is our free applicative that describes metadata about a user
  let schema: Schema.t(t) =
    Schema.Infix.(
      make
      <$> Schema.nonEmptyString("first")
      <*> Schema.nonEmptyString("last")
      <*> Schema.int("age")
    );
};

// This is the target applicative into which we will interpret
module ValidationE = Validation.WithErrors(NonEmptyList.SemigroupAny, String);

// This is a demo of how to interpret the DSL into another applicative, like validation.
// The NT thing is pretty nasty, but I couldn't figure out a simpler way to do it.
let validateUser =
    (first: string, last: string, age: string): ValidationE.t(User.t) => {
  module NT:
    Relude.Interface.NATURAL_TRANSFORMATION with
      type f('a) = Field.t('a) and type g('a) = ValidationE.t('a) = {
    type f('a) = Field.t('a);
    type g('a) = ValidationE.t('a);
    let f = (field: f('a)) =>
      switch (field.name) {
      | "first" => field.validator(first) |> Result.toValidationNel
      | "last" => field.validator(last) |> Result.toValidationNel
      | "age" => field.validator(age) |> Result.toValidationNel
      | _ => Validation.errorNel("Unexpected field: " ++ field.name)
      };
  };
  module SchemaValidate =
    Schema.WithApplicativeAndNT(ValidationE.Applicative, NT);
  SchemaValidate.foldFree(User.schema);
};

describe("Relude.Free_Applicative", () => {
  test("validateUser success", () =>
    expect(validateUser("Andy", "White", "101"))
    |> toEqual(
         Validation.VOk(User.{first: "Andy", last: "White", age: 101}),
       )
  );

  test("validateUser error", () => {
    // TODO: list of errors is reversed just b/c of how this works. Could reverse it somewhere.
    expect(validateUser("", "", "abc"))
    |> toEqual(
         Validation.VError(
           NonEmpty.List.make(
             "Invalid input for field age (expected int): abc",
             [
               "Invalid input for field last (expected non-empty): ",
               "Invalid input for field first (expected non-empty): ",
             ],
           ),
         ),
       )
  });
});
