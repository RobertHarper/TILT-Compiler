(* External syntax for groups.  *)
(*
	A group assigns logical names to the files in an SML project
	and describes the dependencies between them.

	A group may comprise multiple group files that refer to one
	another.  The dependencies between group files must form a
	DAG.  The identifiers that may appear free in a group file are
	environment variables, a few predefined variables, identifiers
	from the Basis library (usually), and identifiers from any
	included or imported group files.

	The syntax for constants, comments, and (alphanumeric)
	identifiers is borrowed from SML.
*)

structure ExtSyn =
struct
    type pos = int

    (* Type of extra argument to lexer.  *)
    type lexarg = {errors : unit -> bool,
		   newline : pos -> unit,
		   start : unit -> (string * pos) option,
		   startCom : pos -> unit,
		   nestCom : unit -> unit,
		   endCom : unit -> bool,
		   startStr : pos -> unit,
		   addToStr : string -> unit,
		   finishStr : pos -> string * pos * pos,
		   parseError : string * pos * pos -> unit}

    (*
	An identifier is either a variable x, a compilation unit name
	U, an interface name I, or an environment variable X.  These
	categories are disjoint; for example, it is possible to assign
	the same name to a unit and its interface.  Identifiers may
	not be shadowed within a group file and the same variable or
	compilation unit name may not be defined in two group files
	that are part of the same group.  (Naming a unit is not the
	same as defining it; the distinction is whether or not an
	implementation is provided.  See the following description of
	"import unit".)
    *)
    type id = string

    (*
	The expression langauge has types bool, string, and int.
	String expressions support naming files; for example, "tilt."
	^ $target ^ ".exe".  Int and bool expressions support
	conditional compilation.  Precedence is as in SML.  The parser
	permits parentheses around expressions and after the keyword
	defined.
    *)
    datatype exp =
        EXP_VAR of id			(* $x *)
      | EXP_ENV of id			(* env X *)
      | EXP_STR of string		(* "lit" *)
      | EXP_CAT of exp * exp		(* exp ^ exp *)
      | EXP_INT of int			(* ...,~1,0,1,... *)
      | EXP_BOOL of bool		(* true or false *)
      | EXP_IF of exp * exp * exp	(* if e1 then e2 else e3 *)
      | EXP_NOT of exp			(* not e *)
      | EXP_AND of exp * exp		(* e1 andalso e2 *)
      | EXP_OR of exp * exp		(* e1 orelse e2 *)
      | EXP_SEQ of exp * exp		(* e1 S= e2 *)
      | EXP_BEQ of exp * exp		(* e1 B= e2 *)
      | EXP_IEQ of exp * exp		(* e1 = e2 *)
      | EXP_ILT of exp * exp		(* e1 < e2 *)
      | EXP_ILE of exp * exp		(* e1 <= e2 *)
      | EXP_IGT of exp * exp		(* e1 > e2 *)
      | EXP_IGE of exp * exp		(* e1 >= e2 *)
      | EXP_DEFU of id			(* defined U *)
      | EXP_DEFI of id			(* defined interface I *)
      | EXP_DEFV of id			(* defined $x *)
      | EXP_DEFE of id			(* defined env X *)
      | EXP_MARK of pos * exp

    type imports = id list		(* {U ...} *)

    (*
	An export list specifies additional interfaces and values to
	pack into a library.
    *)
    datatype export =
	EXPORTI of id			(* interface I *)
      | EXPORTV of id			(* val x *)
    type exports = export list		(* {...} *)

    datatype entry =

      (*
	source interface I = file {imports}
	compiled interface I = file and file

	Associated with every source file is an import list that
	specifies the units whose interfaces are needed to elaborate
	the source.  Every free identifier in a source file must be
	declared in one or more of its imports' interfaces.

	A compiled interface is a pair of a parameterized interface
	file and a unit environment file.  A unit environment
	summarizes the context in which a compiled file was generated.
     *)
	SRCI of id * exp * imports
      | COMPI of id * exp * exp

      (*
	source unit U [: I] = file {imports}
	compiled unit U : I = file and file
	primitive unit U {imports}
	import unit U : I

	A group file names a unit either by importing it or by
	defining it.  No unit can be named twice in a given group
	file.  In a set of interdependent group files, no unit can be
	defined twice.  In a set of interdependent group files, a unit
	may be named multiple times provided that if a definition is
	provided, then it precedes the imports; in this case, the
	manager checks that interfaces match up.

	A compiled unit is a pair of a system object file and a unit
	environment.  N.B.  We do not check object files against their
	unit environments or their interfaces.
      *)
      | SRCU of id * id option * exp * imports
      | COMPU of id * id * exp * exp
      | PRIMU of id * imports
      | IMPORTU of id * id

      (*
	include group file
	import group file

	Including or importing a group file makes its
	identifiers---but not those of its subgroups---available to
	subsequent entries.

	The choice between import and include affects subsequent "make
	library" entries; see the following discussion.

	The same group file can be imported/included multiple times;
	each "make library" treats the group file as imported or
	included according to the most recent import/include.
      *)
      | INCLUDE of exp
      | IMPORT of exp

      (* val x = e *)
      | VAL of id * exp

      (*
	make executable file {U ...}

	The manager will bring the specified units up to date and link
	them, together with any units they depend on, into the named
	executable.  Every unit needed by the executable must have an
	implementation; that is, linking against imported units is not
	permitted.  Run-time effects occur in group file order; in
	particular, the order that units are listed in "make
	executable" is irrelevant.
      *)
      | MAKE_EXE of exp * id list

      (*
	make library file [ {exports} ]

	The library implements all units defined in the current and
	included group files, as well as any units they depend on,
	except that it may import, if necessary, but not implement
	units that are imported in the current group file or defined
	by imported group files.

	The manager brings these units and any exported interfaces up
	to date and packages them, together with any interfaces they
	depend on, into the named library.  The library group file
	will define any exported values.

	The manager packs just enough source code for humans to
	understand the library interface.  This is imperfect because
	no source exists for the primitive unit's compiled interface
	and any "compiled interface" entries in the group.
     *)
      | MAKE_LIB of exp * exports

      (*
	#if e entries [#elif e' entries' ...] [#else entries''] #endif
	#error msg
      *)
      | IF of exp * entries * entries
      | ERROR of exp

      | MARK of pos * entry

    withtype entries = entry list

    type groupfile = entries

end
