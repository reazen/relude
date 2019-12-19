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
module Array = Relude_Array;
module ArrayZipper = Relude_ArrayZipper;
module AsyncData = Relude_AsyncData;
module AsyncResult = Relude_AsyncResult;
module Bool = Relude_Bool;
module Float = Relude_Float;
module HList = Relude_HList;
module HMap = Relude_HMap;
module Int = Relude_Int;
module IO = Relude_IO;
module Ior = Relude_Ior;
module List = Relude_List;
module ListZipper = Relude_ListZipper;
module Map = Relude_Map;
module Nea = Relude_NonEmpty.Array;
module Nel = Relude_NonEmpty.List;
module NonEmpty = Relude_NonEmpty;
module NonEmptyArray = Relude_NonEmpty.Array;
module NonEmptyList = Relude_NonEmpty.List;
module Option = Relude_Option;
module OptionT = Relude_OptionT;
module RIO = Relude_RIO;
module RJs = Relude_Js;
module ReaderT = Relude_ReaderT;
module Result = Relude_Result;
module ResultT = Relude_ResultT;
module Sequence = Relude_Sequence;
module SequenceZipper = Relude_SequenceZipper;
module Set = Relude_Set;
module StateT = Relude_StateT;
module String = Relude_String;
module StringMap = Relude_StringMap;
module Tree = Relude_Tree;
module TreeZipper = Relude_TreeZipper;
module Tuple = Relude_Tuple;
module Validation = Relude_Validation;
module Void = Relude_Void;
module WriterLog = Relude_WriterT.WriterLog;
module WriterT = Relude_WriterT;

// Bring in some common operators
// Note: we can't bring in certain types of operators like <$>, >>=, etc.
// because these don't work across different types, because the language does
// not have ad-hoc polymorphism.
let (>>) = Relude_Function.Infix.(>>);
let (<<) = Relude_Function.Infix.(<<);
let id = Relude_Function.id;
let const = Relude_Function.const;
let flip = Relude_Function.flip;
let absurd = Void.absurd;
let inverse = Bool.inverse /* can't use `not` because `refmt` rewrites i*/;