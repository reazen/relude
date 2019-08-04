open BsAbstract.Interface;

module Make = (R: RING) => {
  let (-) = R.subtract;
  let negate = v => R.zero - v;
};
