include Relude_List_Instances;
include Relude_List_Base;
include Relude_List_Specializations;

module Infix = {
  include Relude_Extensions_Functor.FunctorInfix(
            Relude_List_Instances.Functor,
          );
  include Relude_Extensions_Alt.AltInfix(Relude_List_Instances.Alt);
  include Relude_Extensions_Apply.ApplyInfix(Relude_List_Instances.Apply);
  include Relude_Extensions_Monad.MonadInfix(Relude_List_Instances.Monad);
};