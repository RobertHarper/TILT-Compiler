(*$import TwelfOrder LambdaStructs RedBlackStructs *)
structure Order =
  Order (structure IntSyn' = IntSyn
	 structure Table = IntRedBlackTree);

