/**
 * Extensions for any ALT
 */
module AltExtensions = (A: BsAbstract.Interface.ALT) => {
  /**
   * Alternative form of `alt` that uses a named argument for disambiguation
   */
  let orElse: 'a. (~fallback: A.t('a), A.t('a)) => A.t('a) =
    (~fallback, init) => {
      A.alt(init, fallback);
    };
};

/**
 * Infix operator extensions for any ALT
 */
module AltInfix = (A: BsAbstract.Interface.ALT) => {
  /**
   * Operator version for the `alt` function.
   */
  let (<|>) = A.alt;
};