(* External syntax for project description files.  *)
(*
	A project description assigns logical names to the units and
	interfaces in an SML project and describes the dependencies
	between them.

	A project description may comprise multiple project
	description files that refer to one another.  The dependencies
	between project description files must form a DAG.

	The syntax for constants, comments, and (alphanumeric)
	identifiers is borrowed from SML.

	An identifier is either a variable x, a compilation unit name
	U, an interface name I, or an environment variable X.  These
	categories are disjoint; for example, it is possible to assign
	the same name to a unit and its interface.

	The identifiers that may appear free in a project description
	file are environment variables, a few predefined variables,
	identifiers from the Basis library (usually), and identifiers
	from any included project description files (transitively).

	Variables may not be shadowed.  Compilation unit names may be
	shadowed provided at most one implementation is defined and it
	precedes any other definitions for the unit name.  Interface
	names may be shadowed provided the definitions are equivalent.
	These restrictions ensure that unit and interface names are
	definite references.

	The expression langauge has types bool, string, and int.
	String expressions support naming files; for example,
	$libdir^"/lib".  Int and bool expressions support conditional
	compilation.  Precedence is as in SML.  The parser permits
	parentheses around expressions and after the keyword defined.

	Associated with every source file is a list of the units that
	need to be opened to compile the source.  Every free
	identifier in the source must be declared in one or more of
	these units.

	The location of compiled files is implicit.
*)

structure ExtSyn =
struct
    type pos = Pos.pos

    (* Type of extra argument to lexer.  *)
    type lexarg = {errors : unit -> bool,
		   curpos' : unit -> pos,
		   curpos : unit -> pos * pos,
		   newline : unit -> unit,
		   start : unit -> (string * pos) option,
		   startCom : unit -> unit,
		   nestCom : unit -> unit,
		   endCom : unit -> bool,
		   startStr : unit -> unit,
		   addToStr : string -> unit,
		   finishStr : unit -> string * pos * pos,
		   parseError : string * pos * pos -> unit}

    type label = Name.label

    datatype exp =
        EXP_VAR of label		(* $x *)
      | EXP_ENV of label		(* env X *)
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
      | EXP_DEF of label		(* defined (unit|interface|$|env) U *)
      | EXP_MARK of pos * exp

    type opened = label list		(* {U1 ... Un} *)
    type opopt = opened option
    type asc = label			(* : I *)
    type ascopt = asc option

    datatype ie =
	SRCI of exp * opopt		(* src [opened] *)
      | PRIMI				(* primitive *)
      | PRECOMPI of exp * opened	(* compiled src opened *)
      | COMPI				(* compiled *)

    datatype ue =
	SRCU of exp * opopt * ascopt	(* src [opened] [: I] *)
      | PRIMU of asc			(* primitive : I *)
      | PRECOMPU of exp * opened * asc	(* compiled src opened : I *)
      | COMPU of asc			(* compiled : I *)

    (*
	There are dervied forms for unit entries with ascribed
	interfaces; for example, you can write

		unit U : I = primitive

	instead of

		unit U = primitive : I
    *)
    datatype ent =
	INTERFACE of label * ie		(* interface I = ie *)
      | SC of label * label		(* unit U : I *)
      | UNIT of label * ue		(* unit U = ue *)
      | VAL of label * exp		(* val x = exp *)
      | INCLUDE of exp			(* include file *)
      | LOCAL of exp			(* local file *)
      | IF of exp * ents * ents		(* #if e entries
					   [#elif e' entries' ...]
					   [#else entries''] #endif *)
      | ERROR of exp			(* #error msg *)
      | MARK of pos * ent

    withtype ents = ent list

end
