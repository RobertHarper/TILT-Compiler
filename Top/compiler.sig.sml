(* Interface to the compiler. *)
(*
	The compiler has side effects on the file system.
	Parameterized interface files and unit environment files are
	read and written through FileCache.  Unit and interface source
	files are read directly from the file system.  Assembler files
	are written directly to the file system.
*)
signature COMPILER =
sig

    val CompilerDiag : bool ref

    exception Reject of string		(* User code is bad. *)

    structure FileCache :
    sig
	include FILECACHE

	val read_ue : string -> UnitEnvironment.ue
	val write_ue : string * UnitEnvironment.ue -> unit

	val read_info : string -> Info.info
	val write_info : string * Info.info -> unit
    end

    (*
	Invariant: If f : iface, then f names an up-to-date
	parameterized interface file.
    *)
    type file = string
    type iface = file
    type unitname = string
    type imports = unitname list
    (*
	Invariant: If L : precontext, then
	1. No two units have the same name.
	2. L is ordered so that each interface's parameters precede it.
    *)
    type precontext = (unitname * iface) list

    type il_module
    type nil_module
    type rtl_module

    val eq : unitname * precontext * iface * iface -> bool

    (*
	Unit elaboration returns true when a new compiled interface is
	written.  This occurs if the old parameterized interface file
	had a bad magic number or was not equivalent to the new
	interface.
    *)

    val elaborate_srci : {precontext : precontext,
			  imports : imports,
			  ifacename : string,
			  source : file,
			  ifaceTarget : file,
			  ueTarget : file} -> unit

    val elaborate_primu : {precontext : precontext,
			   imports : imports,
			   unitname : unitname,
			   ifaceTarget : file,
			   ueTarget : file} -> il_module * bool

    val elaborate_srcu : {precontext : precontext,
			  imports : imports,
			  unitname : unitname,
			  source : file,
			  ifaceTarget : file,
			  ueTarget : file} -> il_module * bool

    val elaborate_srcu' : {precontext : precontext,
			   imports : imports,
			   unitname : unitname,
			   source : file,
			   interface : iface} -> il_module

    val il_to_nil : unitname * il_module -> nil_module
    val nil_to_rtl : unitname * nil_module -> rtl_module
    val rtl_to_asm : {precontext : precontext,
		      asmTarget : file,
		      ueTarget : file,
		      rtl_module : rtl_module} -> unit

    (* Create initialization code for the given units. *)
    val link : {asmTarget : file, unitnames : unitname list} -> unit

end
