module OptionEqExtensions = (E: BsAbstract.Interface.EQ) => {
  let eq: (option(E.t), option(E.t)) => bool =
    Relude_Option_Instances.eqBy(E.eq);
};

module OptionOrdExtensions = (O: BsAbstract.Interface.ORD) => {
  module OptionOrd = Relude_Option_Instances.Ord(O);

  let compare = OptionOrd.compare;
};

module String = {
  include OptionEqExtensions(Relude_String.Eq);
  include OptionOrdExtensions(Relude_String.Ord);
};

module Int = {
  include OptionEqExtensions(Relude_Int.Eq);
  include OptionOrdExtensions(Relude_Int.Ord);
};

module Float = {
  include OptionEqExtensions(Relude_Float.Eq);
  include OptionOrdExtensions(Relude_Float.Ord);
};

module IO = {
  let traverse =
      (type e, f: 'a => Relude_IO.t('b, e), opt: option('a))
      : Relude_IO.t(option('b), e) => {
    module IoE =
      Relude_IO.WithError({
        type t = e;
      });
    module TraverseIO = BsAbstract.Option.Traversable(IoE.Applicative);
    TraverseIO.traverse(f, opt);
  };

  let sequence =
      (type e, opt: option(Relude_IO.t('a, e)))
      : Relude_IO.t(option('a), e) => {
    module IoE =
      Relude_IO.WithError({
        type t = e;
      });
    module TraverseIO = BsAbstract.Option.Traversable(IoE.Applicative);
    TraverseIO.sequence(opt);
  };
};
