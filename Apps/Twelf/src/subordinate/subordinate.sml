(*$import Subordinate GlobalStructs LambdaStructs *)
structure Subordinate = 
  Subordinate (structure Global = Global
	       structure IntSyn' = IntSyn
	       structure Whnf = Whnf)

