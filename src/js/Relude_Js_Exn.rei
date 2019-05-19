let make: string => Js.Exn.t;
let throw: string => unit;
let unsafeFromExn: exn => Js.Exn.t;
external unsafeToExn: Js.Exn.t => exn = "%identity";
