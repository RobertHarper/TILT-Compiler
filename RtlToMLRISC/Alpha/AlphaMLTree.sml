
(* =========================================================================
 * AlphaMLTree.sml
 * ========================================================================= *)

structure AlphaMLTree = MLTreeF(structure Const = AlphaMLRISCConstant
				structure P	= AlphaMLRISCPseudo
				structure R	= AlphaMLRISCRegion)

structure AlphaMLTreeExtra = MLTreeExtra(structure MLTree = AlphaMLTree)
