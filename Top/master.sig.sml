signature MASTER =
sig
    val MasterDiag : bool ref
    val MasterVerbose : bool ref
    val MasterVVerbose : bool ref

    type targets = Name.label list

    val make : string * targets -> unit
    val make_exe : string * string * targets -> unit	(* project, exe *)
    val make_lib : string * string * targets -> unit	(* project, lib *)
    val purge : string * targets -> unit
    val purgeAll : string * targets -> unit
end
