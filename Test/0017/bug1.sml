(*
	The coercion compiler was generating a malformed signature
	with the transparant ascription below.  When compiling with
	-fShowInterface, you saw an sdec for B that had the form:

	**                  B_STR : 
	***                    [hiddenThinModule_INT : 
	****                       [C_STR > c : [+Ea_INT : ...],
	****                        +Ea_INT : c.a_TYC * c.a_TYC -> bool,
	****                        +OopenlblA_INT : [+Ea_INT : ...]]]

	The type of the hidden module's second component (+Ea_INT) contains
	the bogus path c.a_TYC.
*)

structure A :> sig eqtype a end = struct type a = int end

structure B : sig end =
struct
	structure C = A
	type a = C.a
	open A
end
