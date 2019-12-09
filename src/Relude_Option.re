include Relude_Option_Instances;
include Relude_Option_Base;
include Relude_Option_Specializations;

module Infix = {
  include Relude_Extensions_Semigroupoid.SemigroupoidInfix(Semigroupoid);
  include Relude_Extensions_Functor.FunctorInfix(Functor);
  include Relude_Extensions_Alt.AltInfix(Alt);
  include Relude_Extensions_Apply.ApplyInfix(Apply);
  include Relude_Extensions_Monad.MonadInfix(Monad);

  /**
    Infix operator for `getOrElse()`

    ### Example
    ```re
    Some(3) |? 0 == 3;
    None |? 0 == 0;
    ```
  */
  let (|?) = (opt, default) => getOrElse(default, opt);
};