(* The datatypes for the internal language. *)
structure Il :> IL =
struct

    type var = Name.var
    type label = Name.label
    type labels = Name.label list
    type vpath = Name.vpath
    type prim = Prim.prim
    type ilprim = Prim.ilprim

    datatype path = PATH of var * labels
    datatype arrow = TOTAL | PARTIAL

    datatype exp = OVEREXP of con * bool * exp Util.oneshot (* type, valuable, body *)
                 | SCON    of value
                 | PRIM    of prim * con list * exp list   (* fully applied primitivies only *)
                 | ILPRIM  of ilprim * con list * exp list (* for type-checking reasons *)
                 | ETAPRIM of prim * con list
                 | ETAILPRIM of ilprim * con list
                 | VAR     of var
                 | APP     of exp * exp
                 | EXTERN_APP of con * exp * exp list  (* con is type of function *)
                 | FIX     of bool * arrow * fbnd list
                 | RECORD  of (label * exp) list
                 | RECORD_PROJECT of exp * label * con
                 | SUM_TAIL of int * con * exp
                 | HANDLE  of con * exp * exp
                 | RAISE   of con * exp       (* annotate with the type of the raised expression *)
                 | LET     of bnd list * exp
                 | NEW_STAMP of con
                 | EXN_INJECT of string * exp * exp (* tag expression and value *)
                 | COERCE of exp * con list * exp        (* Coercion value, type arguments, term *)
                 | FOLD   of var list * con * con                 (* Fold coercion   : unrolled type, rolled type (hopefully names) *)
                 | UNFOLD of var list * con * con                 (* Unfold coercion : rolled type, unrolled type (hopefully names) *)
                 | ROLL    of con * exp
                 | UNROLL  of con * con * exp    (* the recursive and non-recursive type *)
                 | INJ     of {sumtype : con,    (* non-special sum tyoe *)
			       field : int,
			       inject : exp option}
                 (* case over sum types of exp with arms and defaults*)
                 | CASE    of {sumtype : con,
			       arg : exp,
			       bound : var,
			       arms : (exp option) list,
			       tipe : con,
			       default : exp option}
                 (* exnarms include: tag exp whose type must be CON_TAG(con) and body : con -> con_result *)
                 | EXN_CASE of {arg : exp,
				arms : (exp * con * exp) list,
				default : exp option,
				tipe : con}
                 | MODULE_PROJECT of mod * label
                 | SEAL    of exp * con

                              (* var = (var : con) : con |-> exp *)
    and  fbnd = FBND    of var * var * con * con * exp

    and flexinfo = FLEXINFO of (Tyvar.stamp * bool * (label * con) list)
	         | INDIRECT_FLEXINFO of flexinfo ref (* <--- this ref is necessary for unification *)

    and      con = CON_VAR           of var
                 | CON_TYVAR         of (context,con,exp) Tyvar.tyvar  (* supports type inference *)
                 | CON_OVAR          of (context,con,exp) Tyvar.ocon   (* supports "overloaded" types *)
                 | CON_FLEXRECORD    of flexinfo ref
                 | CON_INT           of Prim.intsize
                 | CON_UINT          of Prim.intsize
                 | CON_FLOAT         of Prim.floatsize
                 | CON_ARRAY         of con
                 | CON_VECTOR        of con
                 | CON_INTARRAY      of Prim.intsize     (* Array_sz  (uniform rep at size)*)
                 | CON_INTVECTOR     of Prim.intsize 
                 | CON_FLOATARRAY    of Prim.floatsize   (* Array_sz  (uniform rep at size)*)
                 | CON_FLOATVECTOR   of Prim.floatsize 
                 | CON_ANY
                 | CON_REF           of con
                 | CON_TAG           of con
                 | CON_ARROW         of con list * con * bool * (arrow Util.oneshot)
                 | CON_APP           of con * con list
                 | CON_MU            of con
                 | CON_RECORD        of (label * con) list
                 | CON_FUN           of var list * con
                 | CON_SUM           of {names : label list,
					 noncarriers : int,
					 carrier : con,
					 special : int option}
                 | CON_COERCION of var list * con * con
                 | CON_TUPLE_INJECT  of con list
                 | CON_TUPLE_PROJECT of int * con
                 | CON_MODULE_PROJECT of mod * label
    and     kind = KIND
                 | KIND_TUPLE of int
                 | KIND_ARROW of int * kind
    and      mod = MOD_VAR of var
                 | MOD_STRUCTURE of sbnd list
                 | MOD_FUNCTOR of arrow * var * signat * mod * signat
                 | MOD_APP of mod * mod
                 | MOD_PROJECT of mod * label
                 | MOD_SEAL of mod * signat
                 | MOD_LET of var * mod * mod
    and     sbnd = SBND of label * bnd
    and      bnd = BND_EXP of var * exp
                 | BND_CON of var * con
                 | BND_MOD of var * bool * mod (* bool indicates polymorphism encoded with modules;
						  used by phase-splitter *)

    and   signat = SIGNAT_STRUCTURE of sdec list
                 | SIGNAT_FUNCTOR of var * signat * signat * arrow
		 | SIGNAT_VAR of var
	         | SIGNAT_OF of path

    and     sdec = SDEC of label * dec
    and      dec = DEC_EXP       of var * con * exp option  * bool (* true indicates should inline *)
                 | DEC_CON       of var * kind * con option * bool (* true indicates should inline *)
                 | DEC_MOD       of var * bool * signat

    and     ovld = OVLD of (con * exp) list * int option (* types, default *)

    and context_entry =
	CONTEXT_SDEC   of sdec
      | CONTEXT_SIGNAT of label * var * signat
      | CONTEXT_EXTERN of label * var * label * con
      | CONTEXT_FIXITY of label * Fixity.fixity
      | CONTEXT_OVEREXP of label * ovld

    (*
       A context contains
	 (A) An ordered mapping of labels and variables to some classifier information.
	 (B) A table memoizing transformed module signatures.
	 (C) A mapping from top-level labels to paths for the star convention.
	 (D) A mapping from overloaded labels to the overloaded types and expressions.
         (E) A mapping from labels to fixity information.
       The labels of (C) are a superset of the labels of (A).
       The labels of (C) and (D) are disjoint.

       Invariants:

		The entries of varMap, listed in (rev ordering) order,
		form a well-formed elaboration context.

       		If (v,(l,pc)) in varMap, then (l,(v,nil) in labelMap.

		If (l,(v,nil)) in labelMap, then (v,(l',pc)) in varMap.

		(l,(v,labs)) in labelMap, where labs is non-nil, iff
		the label l is visible at the top-level due to the
		star convention and v.labs is the corresponding path.

		dom(overloadMap) and dom(labelMap) are disjoint.

       Comments:

		Equality compilation needs to map constructor
		variables to labels so varMap entries do not have the
		form (v,pc).

		Pattern compilation needs to map multiple bound labels
		to the same variable so there is not a one-to-one
		correspondence between the entries in varMap and the
		entries of the form (l,(v,nil)) in labelMap.

		An extern has two labels: A top-level label that can
		be missing or shadowed and an underlying label that
		does not change.

		The union of dom(overloadMap) and dom(labelMap) is the
		set of top-level labels.
    *)

    (* Should be abstract. *)
    and context = CONTEXT of  {varMap : (label * phrase_class) Name.VarMap.map,
			       ordering : var list,
			       labelMap : vpath Name.LabelMap.map,
			       fixityMap : Fixity.fixity Name.LabelMap.map,
			       overloadMap : ovld Name.LabelMap.map}

    (*
	We write Transformed(decs,s,s') iff
	1. decs |- s = s'.
	2. for every substructure signature s'' in s',
	   a. s'' is selfified
	   b. s'' has the form SIGNAT_STRUCTURE sdecs''
	   c. s'' has no non-binding occurrences of local variables.

	N.B. (2) does not apply to any structures declared in functor
	signatures.

	Invariant:

	If (v,PHRASE_CLASS_MOD (_,_,s,f)) in decs,
	then Transformed(decs,s,f()).

	N.B.  This invariant does not apply to every value of type
	phrase_class; only those in an elaboration context.
    *)
    and phrase_class = PHRASE_CLASS_EXP     of exp * con * exp option * bool
                     | PHRASE_CLASS_CON     of con * kind * con option * bool
                     | PHRASE_CLASS_MOD     of mod * bool * signat * (unit -> signat)
                     | PHRASE_CLASS_SIG     of var * signat
		     | PHRASE_CLASS_EXT     of var * label * con

    withtype value = (con,exp) Prim.value

    type decs = dec list
    type bnds  = bnd list
    type sdecs = sdec list
    type sbnds = sbnd list

    (*
	Invariant: The first component is SOME sbnd iff the second
	component is CONTEXT_SDEC.
    *)
    type decresult = (sbnd option * context_entry) list

    type entries = context_entry list

    type module = context * sbnd * sdec

    type sc_module = context * sdec

    (*
	Parameterized compilation unit interfaces can be written to
	disk.  Instantiated interfaces have more structure than this;
	see LinkIl.  Parameters do not have classifier information.
	The manager uses CRCs of interface files to ensure that if a
	compilation unit's interface changes, then everything that
	depends on that unit gets recompiled.
   *)

    datatype parm =
	PARM of label			(* unit name *)
      | PARM_SIG of label * label	(* unit name, signature name *)
      | PARM_EXT of label * label	(* unit name, extern name *)

    type parms = parm Name.VarMap.map

    type pinterface =
	{parms : parms,
	 entries : entries}

end
