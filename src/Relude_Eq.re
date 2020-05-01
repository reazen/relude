type eq('a) = ('a, 'a) => bool;

/**
[Eq.by] creates an [eq('b)] function from an [eq('a)] and a function from
['b => 'a]. Effectively, this allows you to say "my complex type can be compared
for equality because I'm already able to compare these simpler parts."

This is the contravariant map for the [eq('a)] function.

{[
  module User = {
    type t = { id: string, name: string };

    // create an equality function for our user type by comparing its id.
    let equals: eq(t) = Eq.by(user => user.id, String.eq);
  }
]}
*/
let by: 'a 'b. ('b => 'a, eq('a)) => eq('b) =
  (bToA, eqA, b1, b2) => eqA(bToA(b1), bToA(b2));

/**
[Eq.cmap] is the contravariant [map] for the equals function. It is an alias for
[Eq.by].
*/
let cmap = by;

module Contravariant:
  BsBastet.Interface.CONTRAVARIANT with type t('a) = eq('a) = {
  type t('a) = eq('a);
  let cmap = cmap;
};

/**
[Eq.invert] produces an equality function from an existing equality function,
but with the returned boolean switched.

{[
  let stringNotEq = Eq.invert(String.eq);
  stringNotEq("hello", "world") == true;
]}
*/
let invert: 'a. eq('a) => eq('a) = (eqA, a1, a2) => !eqA(a1, a2);
