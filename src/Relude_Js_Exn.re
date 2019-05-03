let make: string => Js.Exn.t = [%bs.raw
  {| function(message) { return new Error(message); } |}
];

let throw: string => unit = [%bs.raw
  {| function(message) { throw new Error(message); } |}
];

let unsafeFromExn: exn => Js.Exn.t =
  exn => {
    let makeUnknownJsExn: exn => Js.Exn.t = [%bs.raw
      {| function(exn) { return new Error("Unexpected error: " + exn); } |}
    ];
    switch (exn) {
    | Js.Exn.Error(jsExn) => jsExn
    | _ => makeUnknownJsExn(exn)
    };
  };

external unsafeToExn: Js.Exn.t => exn = "%identity";
