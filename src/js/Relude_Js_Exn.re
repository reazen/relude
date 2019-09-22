/**
 * Creates a JS Error with the given string message.
 */
let make: string => Js.Exn.t = [%raw message =>
  {| return new Error(message);  |}
];

/**
 * Creates and throw a JS Error with the given string message.
 */
let throw: string => unit = [%raw message =>
  {| throw new Error(message); |}
];

/**
 * Unsafely Converts an OCaml exn into a Js.Exn.t
 */
let unsafeFromExn: exn => Js.Exn.t =
  exn => {
    let makeUnknownJsExn: exn => Js.Exn.t = [%raw exn =>
      {| return new Error("Unexpected error: " + exn); |}
    ];
    switch (exn) {
    | Js.Exn.Error(jsExn) => jsExn
    | _ => makeUnknownJsExn(exn)
    };
  };

/**
 * Unsafely coerces a Js.Exn.t to an OCaml exn without regard to consequences of such actions.
 */
external unsafeToExn: Js.Exn.t => exn = "%identity";
