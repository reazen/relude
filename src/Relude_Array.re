/**
`Relude.Array` contains typeclass instances and utility functions for the
`array('a)` type.
*/
include Relude_Array_Instances;
include Relude_Array_Base;
include Relude_Array_Specializations;

module Infix = {
  include Relude_Extensions_Functor.FunctorInfix(
            Relude_Array_Instances.Functor,
          );
  include Relude_Extensions_Alt.AltInfix(Relude_Array_Instances.Alt);
  include Relude_Extensions_Apply.ApplyInfix(Relude_Array_Instances.Apply);
  include Relude_Extensions_Monad.MonadInfix(Relude_Array_Instances.Monad);
};
