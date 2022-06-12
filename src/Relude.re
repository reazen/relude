[@ocaml.text
  {|
{b Relude} is an alternative standard library (a "prelude") written in Reason
and compiled with the Melange compiler. It builds on the category theory
foundation provided by {{: https://github.com/Risto-Stevcev/bastet} Bastet}, and
provides a larger collection of types and utility function specialized for those
types.

The [Relude] module serves as a namespace containing all other modules in a
convenient hierarchy. This allows you to use [Relude.Array] without shadowing
the [Array] module provided by the OCaml stdlib.

In many cases, however, you {e will} want to shadow the existing stdlib modules
with their Relude equivalents. In this case, you could [open Relude;] but doing
so may shadow more than you want (e.g. the [Js] module provided by Melange will
also be shadowed). See {{: Relude_Globals} [Relude.Globals]} for a better
solution.
|}
];

[@ocaml.text {|{2 Primitives}|}];

module Bool = Relude_Bool;
module Decimal = Relude_Decimal;
module Float = Relude_Float;
module Function = Relude_Function;
module Int = Relude_Int;
module String = Relude_String;
module Tuple = Relude_Tuple;
module Tuple2 = Relude_Tuple2;
module Tuple3 = Relude_Tuple3;
module Tuple4 = Relude_Tuple4;
module Tuple5 = Relude_Tuple5;
module Unit = Relude_Unit;
module Void = Relude_Void;

[@ocaml.text {|{2 Collections}|}];
module Array = Relude_Array;
module ArrayZipper = Relude_ArrayZipper;
module HList = Relude_HList;
module HMap = Relude_HMap;
module List = Relude_List;
module ListZipper = Relude_ListZipper;
module Map = Relude_Map;
module Nea = Relude_NonEmpty.Array;
module Nel = Relude_NonEmpty.List;
module NonEmpty = Relude_NonEmpty;
module NonEmptyArray = Relude_NonEmpty.Array;
module NonEmptyList = Relude_NonEmpty.List;
module Sequence = Relude_Sequence;
module SequenceZipper = Relude_SequenceZipper;
module Set = Relude_Set;
module StringMap = Relude_StringMap;
module Tree = Relude_Tree;
module TreeZipper = Relude_TreeZipper;

[@ocaml.text {|{2 Containers}|}];
module AsyncData = Relude_AsyncData;
module AsyncResult = Relude_AsyncResult;
module Cont = Relude_ContT.Cont;
module ContT = Relude_ContT;
module Free = Relude_Free;
module IO = Relude_IO;
module Ior = Relude_Ior;
module Option = Relude_Option;
module OptionT = Relude_OptionT;
module Reader = Relude_ReaderT.Reader;
module ReaderT = Relude_ReaderT;
module Result = Relude_Result;
module ResultT = Relude_ResultT;
module RIO = Relude_RIO;
module RWST = Relude_RWST;
module State = Relude_StateT.State;
module StateT = Relude_StateT;
module Validation = Relude_Validation;
module Writer = Relude_WriterT.Writer;
module WriterLog = Relude_WriterT.WriterLog;
module WriterT = Relude_WriterT;

[@ocaml.text {|{2 Type Class Helpers}|}];
module Eq = Relude_Eq;
module Extensions = Relude_Extensions;
module Interface = Relude_Interface;
module Ord = Relude_Ord;
module Ordering = Relude_Ordering;

[@ocaml.text {|{2 Utilities and Interop}|}];
module Debounce = Relude_Debounce;
module Identity = Relude_Identity;
module Js = Relude_Js;
module Throttle = Relude_Throttle;
module Timer = Relude_Timer;
module Unsafe = Relude_Unsafe;

[@ocaml.text {|{2 Special Namespace}|}];
module Globals = Relude_Globals;
