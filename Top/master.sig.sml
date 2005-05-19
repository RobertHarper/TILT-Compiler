signature MASTER =
sig
	val MasterDiag : bool ref
	val MasterVerbose : bool ref
	val MasterVVerbose : bool ref

	type targets = Name.label list

	val make : string list * targets -> unit
	val make_exe : string list * string * targets -> unit	(* project, exe *)
	val make_lib : string list * string * targets -> unit	(* project, lib *)
	val purge : string list * targets -> unit
	val purgeAll : string list * targets -> unit
end
