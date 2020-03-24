/**
 * Extensions for any RING
 */
module RingExtensions = (R: BsBastet.Interface.RING) => {
  let (-) = R.subtract;
  let negate = v => R.zero - v;
};
