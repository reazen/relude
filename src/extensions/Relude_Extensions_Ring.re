/**
 * Extensions for any RING
 */
module RingExtensions = (R: BsAbstract.Interface.RING) => {
  let (-) = R.subtract;
  let negate = v => R.zero - v;
};