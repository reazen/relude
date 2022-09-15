/**
Extensions for any RING
*/
module RingExtensions = (R: Bastet.Interface.RING) => {
  let (-) = R.subtract;
  let negate = v => R.zero - v;
};
