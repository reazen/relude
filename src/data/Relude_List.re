/*******************************************************************************
 * Organization of List functions:
 *
 * - Typeclass implementations that don't care about inner 'a
 * - Functions needed by typeclasses
 * - Functions unrelated to typeclasses and indifferent of inner 'a
 * - Freebies from typeclasses
 * - Functions and typeclasses that _do_ care about inner 'a
 ******************************************************************************/

include Relude_List_Types;
include Relude_List_Base;
include Relude_List_Submodules;

module Infix = {
  include BsAbstract.List.Infix;
  include MonadFunctions.ApplyFunctions.Infix;
};
