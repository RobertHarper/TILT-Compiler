(* External syntax for project description files.  *)

structure ExtSyn =
struct
    type pos = Pos.pos

    (* Type of extra argument to lexer.	 *)
    type lexarg =
	{errors : unit -> bool,
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
    |	EXP_ENV of label		(* env X *)
    |	EXP_STR of string		(* "lit" *)
    |	EXP_CAT of exp * exp		(* exp ^ exp *)
    |	EXP_INT of int			(* ...,~1,0,1,... *)
    |	EXP_BOOL of bool		(* true or false *)
    |	EXP_IF of exp * exp * exp	(* if e1 then e2 else e3 *)
    |	EXP_NOT of exp			(* not e *)
    |	EXP_AND of exp * exp		(* e1 andalso e2 *)
    |	EXP_OR of exp * exp		(* e1 orelse e2 *)
    |	EXP_SEQ of exp * exp		(* e1 S= e2 *)
    |	EXP_BEQ of exp * exp		(* e1 B= e2 *)
    |	EXP_IEQ of exp * exp		(* e1 = e2 *)
    |	EXP_ILT of exp * exp		(* e1 < e2 *)
    |	EXP_ILE of exp * exp		(* e1 <= e2 *)
    |	EXP_IGT of exp * exp		(* e1 > e2 *)
    |	EXP_IGE of exp * exp		(* e1 >= e2 *)
    |	EXP_DEF of label		(* defined (unit|interface|$|env) U *)
    |	EXP_MARK of pos * exp

    type units = label list		(* {U1 ... Un} *)
    type units' = units option
    type asc = label			(* : I *)
    type asc' = asc option

    datatype ie =
	SRCI of exp * units'		(* src [opened] *)
    |	PRIMI				(* primitive *)
    |	PRECOMPI of exp * units		(* compiled src opened *)
    |	COMPI				(* compiled *)

    datatype ue =
	SRCU of exp * units' * asc'	(* src [opened] [: I] *)
    |	PRIMU of asc			(* primitive : I *)
    |	PRECOMPU of exp * units * asc	(* compiled src opened : I *)
    |	COMPU of units * asc		(* compiled opened : I *)

    (*
	There are dervied forms for unit entries with ascribed
	interfaces; for example, you can write

	    unit U : I = primitive

	instead of

	    unit U = primitive : I
    *)
    datatype ent =
	INTERFACE of label * ie		(* interface I = ie *)
    |	SC of label * label * bool	(* unit U :[:] I *)
    |	UNIT of label * ue		(* unit U = ue *)
    |	VAL of label * exp		(* val x = exp *)
    |	INCLUDE of exp			(* include file *)
    |	IF of exp * ents * ents		(* #if e entries
					   [#elif e' entries' ...]
					   [#else entries''] #endif *)
    |	ERROR of exp			(* #error msg *)
    |	MARK of pos * ent

    withtype ents = ent list

end
