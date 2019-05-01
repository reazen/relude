/**
Relude.Globals

This module is intended to be used as an global open (e.g. `open
Relude.Globals`) to bring the most core/common Relude modules, types, and
functions into scope for use in your code.

If you are interested in using a global open for Relude, it's recommended
that you globally open this module `open Relude.Globals`, rather than `open
Relude`, because the core `Relude` module aliases *every* module from Relude
and its purpose is to serve as a root namespace for the library, not as a
module to be globally opened.

Also, separating the concept of a root namespace from a global import module
allows us to fine tune the behavior of the global open, while not disturbing
the core namespace module, and vice-versa.

Finally, you do not need to `open Relude.Globals` in order to use Relude -
this is just a convenience feature for people who want such a thing.

We are intentionally shadowing various stdlib/Belt module names here (like
`Array`/`List`/`String`), because we feel we have provided enough
functionality with the corresponding Relude module that we want to explicitly
shadow other similarly-named modules.

For some other modules, like `Js` we are prefixing our version with `R` for
Relude because the Relude `Js` module is not a replacement for the
BuckleScript `Js` module, it is more an extension to it.

NB: When making changes to this module, keep in mind that you will likely be
affecting other people who have globally opened this module, so only expose
very core/common things, and be careful about accidental shadowing of module
names or functions.
*/
module AsyncData = Relude_AsyncData;
module AsyncResult = Relude_AsyncResult;
module Array = Relude_Array;
module IO = Relude_IO;
module List = Relude_List;
module NonEmpty = Relude_NonEmpty;
module Option = Relude_Option;
module RJs = Relude_Js;
module Result = Relude_Result;
module String = Relude_String;

// Bring in some common operators
// Note: we can't bring in certain types of operators like <$>, >>=, etc.
// because these don't work across different types, because the language does
// not have ad-hoc polymorphism.
let (>>) = Relude_Function.Infix.(>>);
let (<<) = Relude_Function.Infix.(<<);