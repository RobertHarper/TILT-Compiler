
(* =========================================================================
 * SparcMLTree.sml
 * ========================================================================= *)

structure SparcMLTree = MLTreeF(structure Const = SparcMLRISCConstant
				structure P	= SparcMLRISCPseudo
				structure R	= SparcMLRISCRegion)

structure SparcMLTreeExtra = MLTreeExtra(structure MLTree = SparcMLTree)

