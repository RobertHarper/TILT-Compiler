(* Group IL. *)
(*
	Variables are used to express dependencies.  External language
	identifiers and group file names are kept around to name
	compiler droppings and for printing error messages.

	The translation from the external language to the internal
	language ensures that the input is well-formed, eliminates
	external language features, and examines unit environments to
	determine the dependencies of compiled units and interfaces.
	CHECKU entries are inserted when one group file imports a unit
	provided by a second group file.  The manager ensures the
	unit's imported interface matches its actual interface.
*)

signature GROUP =
sig

    (*
	Error is raised when a group file is malformed, after error
	messages have been printed.
    *)
    exception Error

    type id = ExtSyn.id

    type filename = string
    type pos = filename * int option (* line number *)
    val posString : pos -> string

    type var = Name.var
    type varset = Name.VarSet.set

    (*
	The first filename carried by SRCI, SRCU, PRIMU, and LINK is
	the group file defining the interface or unit.
    *)
    datatype iface =
	SRCI of id * filename * filename * var list
      | COMPI of id * filename * filename * varset

    datatype compunit =
	SRCU of id * filename * filename * var list * var option
      | COMPU of id * filename * filename * varset * var
      | PRIMU of id * filename * var list
      | IMPORTU of id * var
      | CHECKU of {U:var, I:var}

    datatype cmd =
	LINK of filename * filename * varset
      | PACK of
	{lib : filename,
	 imports : varset,	(* units that may only be imported *)
	 units : varset,	(* units to pack *)
	 ifaces : varset,	(* interfaces to pack *)
	 values : (id * ExtSyn.exp) list} (* each exp is a value *)

    datatype entry =
	IFACE of iface
      | UNIT of compunit
      | CMD of cmd

    type group

    (*
	Entries are in group file order.  Variables are associated
	with commands just for position lookup; no entry refers to
	command variables.
    *)
    val list_entries : group -> {entries : (var * entry) list,
				 pos : var -> pos}
    val size : group -> int
    val get_entry : group -> var -> entry
    (*
	A group' contains information that is only needed while
	external language group files are being read.  The fixup
	function supplied to read is applied to new group files right
	after parsing; it is used to add basis library imports.
    *)
    type group'
    val empty_group' : group'
    val get_group : group' -> group
    val add_string_value : group' * id * string -> group'
    val add_bool_value : group' * id * bool -> group'
    val add_int_value : group' * id * int -> group'
    val read : group' * filename * (ExtSyn.groupfile -> ExtSyn.groupfile) ->
	group'	(* May raise Error *)

    val write : filename * ExtSyn.groupfile -> unit

end
