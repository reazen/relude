# Show

`Show` is the FP answer to the `.toString()` method in OO - converting aribtrary values to strings.  However, the key difference is that you don't get a `toString` function for free when you define a type - you must explicitly implement it by creating an instance of the `Show` typeclass for your type (or tell the compiler to derive an instance of `Show`, which is something that may become available in the future in Relude).  You must explicitly define the `Show` typeclass for each type that needs to be rendered as a string, and if you want to render a value of type `'a` as a string, you must explicitly require an instance of the `Show` typeclass for that type `'a`.

It might seem clunky at first to have to define or derive `Show` for all your types explicitly, but you'll likely find that it will save you from a class of of bugs where values are unintentially converted to strings using implicit calls to `.toString()`.

## Show typeclass (module type)

Note: bs-bastet uses a convention of all-caps for the core typeclass module types.

```reason
module type SHOW = {
  type t;
  let show: t => string;
};
```

The `SHOW` typeclass (module type) basically needs you to specify a type for your value, and a function to convert your value to a string.

## Example instances

```reason
// Int show
module Int {
  module Show: SHOW with type t = int = {
    type t = int;
    let show = string_of_int;
  };
};

// Float show
module Float {
  module Show: SHOW with type t = float = {
    type t = float;
    let show = Js.Float.toString;
  };
};

// String show
module String {
  module Show: SHOW with type t = string = {
    type t = string;
    let show = a => a; // identity - it's already a string!
  };
};
```

## Example basic usage

```reason
let x = Int.Show.show(42); // "42"
let y = String.Show.show("hi"); // "hi"
```

## More advanced instances

Its easy to implement `SHOW` for primitive types like `int` and `string`, but how do we implement it for types like `option('a)` or `result('a, 'e)`?  The answer is to use [module functors](https://v1.realworldocaml.org/v1/en/html/functors.html) - to construct typeclasses from other typeclasses.  Note: this usage of the term `functor` here is different than the `FUNCTOR` typeclass/module type which we will be talking about below - `module functor` is just what OCaml calls its ability to define modules which can be constructed from other modules.

TODO: Option/Result example of show

## Using Show as an abstraction

TODO: example of taking a `SHOW` instance in a function

## Laws

TODO
