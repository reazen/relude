/**
 * Extensions for any ALT
 */
module AltExtensions = (A: BsAbstract.Interface.ALT) => {};

/**
 * Infix operator extensions for any ALT
 */
module AltInfix = (A: BsAbstract.Interface.ALT) => {
  /**
   * Operator version for the `alt` function.
   */
  let (<|>) = A.alt;
};