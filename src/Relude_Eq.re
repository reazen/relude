type eq('a) = ('a, 'a) => bool;

/**
 * Creates an eq('b) function from a eq('a) and a function from 'b => 'a
 *
 * Example:
 * ```
 * let userEq: eq(user) = Relude.Eq.by(user => user.id, String.eq);
 * ```
 *
 * This is the contravariant map for the eq('a) function
 */
let by: 'a 'b. ('b => 'a, eq('a)) => eq('b) =
  (bToA, eqA, b1, b2) => eqA(bToA(b1), bToA(b2));

/**
 * Alias for by
 */
let cmap = by;

module Contravariant:
  BsAbstract.Interface.CONTRAVARIANT with type t('a) = eq('a) = {
  type t('a) = eq('a);
  let cmap = cmap;
};

/**
 * Inverts an equality function
 */
let invert: 'a. eq('a) => eq('a) = (eqA, a1, a2) => !eqA(a1, a2);