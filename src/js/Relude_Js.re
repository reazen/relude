[@ocaml.text
  {|
[Relude.Js] provides modules to interop with JS platform features. If you open
[Relude.Globals], this module will be aliased as [RJs] to avoid conflicting with
the [Js] module provided by Melange.
|}
];

module Animation = Relude_Js_Animation;
module Console = Relude_Js_Console;
module Exn = Relude_Js_Exn;
module Json = Relude_Js_Json;
module Promise = Relude_Js_Promise;
