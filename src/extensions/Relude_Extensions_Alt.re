module AltExtensions = (A: BsAbstract.Interface.ALT) => {
  //let alt = A.alt;
};

module AltInfix = (A: BsAbstract.Interface.ALT) => {
  let (<|>) = A.alt;
};