/**
 * Extensions for any Bifunctor
 */
module BifunctorExtensions = (B: BsAbstract.Interface.BIFUNCTOR) => {
  /**
   * Maps a function over the left-side type.
   */
  let mapLeft: 'a 'b 'c. ('a => 'c, B.t('a, 'b)) => B.t('c, 'b) =
    (aToC, fab) => B.bimap(aToC, b => b, fab);

  /**
   * Maps a function over the right-side type.
   */
  let mapRight: 'a 'b 'd. ('b => 'd, B.t('a, 'b)) => B.t('a, 'd) =
    (bToD, fab) => B.bimap(a => a, bToD, fab);

  /**
   * Alias for `mapRight`
   *
   * Note: this function only makes sense if your error type is on the right of
   * the Bifunctor, like `result('a, 'e)`, or `Relude.IO.t('a, 'e)`.
   */
  let mapError = mapRight;
};

/**
 * Infix operator extensions for any BIFUNCTOR
 */
module BifunctorInfix = (B: BsAbstract.Interface.BIFUNCTOR) => {
  /**
   * Operator version of bimap
   */
  let (<<$>>) = B.bimap;
};
