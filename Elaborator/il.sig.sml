(* The datatypes for the internal language. *)
signature IL = 
  sig

    structure Prim : PRIM
    structure Tyvar : TYVAR

    type tag = Name.tag
    type var = Name.var
    type label = Name.label
    type labels = Name.label list
    type prim = Prim.prim
    type ilprim = Prim.ilprim


    datatype path = SIMPLE_PATH   of var 
                  | COMPOUND_PATH of var * labels


    datatype arrow = TOTAL | PARTIAL

    type fixity_table = (label * Fixity.fixity) list 

    type context
    datatype exp = OVEREXP of con * bool * exp Util.oneshot (* type, valuable, body *)
                 | SCON    of value
                 | PRIM    of prim * con list * exp list   (* fully applied primitivies only *)
                 | ILPRIM  of ilprim * con list * exp list (* for type-checking reasons *)
                 | ETAPRIM of prim * con list
                 | ETAILPRIM of ilprim * con list
                 | VAR     of var
                 | APP     of exp * exp list
                 | FIX     of bool * arrow * fbnd list (* bool = false indicates non-recursive *)
                 | RECORD  of (label * exp) list
                 | RECORD_PROJECT of exp * label * con
                 | SUM_TAIL of con * exp
                 | HANDLE  of exp * exp      
                 | RAISE   of con * exp       (* annotate with the type of the raised expression *)
                 | LET     of bnd list * exp
                 | NEW_STAMP of con
                 | EXN_INJECT of string * exp * exp (* name and tag and value *)
                 | ROLL    of con * exp
                 | UNROLL  of con * exp
                 | INJ     of {noncarriers : int,
			       carriers : con list,
			       special : int,
			       inject : exp option}
                 (* case over sum types of exp with arms and defaults*)
                 | CASE    of {noncarriers : int,
			       carriers : con list,
			       arg : exp,
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
                 | CON_TYVAR         of (context,con) Tyvar.tyvar  (* supports type inference *)
                 | CON_OVAR          of (context,con) Tyvar.ocon   (* supports "overloaded" types *)
                 | CON_FLEXRECORD    of flexinfo ref
                 | CON_INT           of Prim.intsize
                 | CON_UINT          of Prim.intsize
                 | CON_FLOAT         of Prim.floatsize
                 | CON_ARRAY         of con
                 | CON_VECTOR        of con
                 | CON_ANY
                 | CON_REF           of con
                 | CON_TAG           of con
	         (* multi-argument functions can be imported; bool indicates closed code *)
                 | CON_ARROW         of con list * con * bool * (arrow Util.oneshot)
                 | CON_APP           of con * con
                 | CON_MUPROJECT     of int * con
                 | CON_RECORD        of (label * con) list
                 | CON_FUN           of var list * con
                 | CON_SUM           of {noncarriers : int,
					 carriers : con list,
					 special : int option}
                 | CON_TUPLE_INJECT  of con list
                 | CON_TUPLE_PROJECT of int * con 
                 | CON_MODULE_PROJECT of mod * label
    and     kind = KIND_TUPLE of int
                 | KIND_ARROW of int * int
                 | KIND_INLINE of kind * con
    and      mod = MOD_VAR of var
                 | MOD_STRUCTURE of sbnd list
                 | MOD_FUNCTOR of var * signat * mod
                 | MOD_APP of mod * mod
                 | MOD_PROJECT of mod * label
                 | MOD_SEAL of mod * signat
                 | MOD_LET of var * mod * mod
    and     sbnd = SBND of label * bnd
    and      bnd = BND_EXP of var * exp
                 | BND_MOD of var * mod
                 | BND_CON of var * con

    and   signat = SIGNAT_STRUCTURE         of path option * sdec list
                 | SIGNAT_FUNCTOR of var * signat * signat * arrow
                 | SIGNAT_INLINE_STRUCTURE  of {self : path option,
						code : sbnd list, (* may be selfified *)
						imp_sig : sdec list,
						abs_sig : sdec list}
    and     sdec = SDEC of label * dec
    and      dec = DEC_EXP       of var * con
                 | DEC_MOD       of var * signat
                 | DEC_CON       of var * kind * con option 
                 | DEC_EXCEPTION of tag * con

    and inline = INLINE_MODSIG of mod * signat
	  | INLINE_EXPCON  of exp * con
	  | INLINE_CONKIND of con * kind
	  | INLINE_OVER    of (con * exp) list

    and context_entry = 
		CONTEXT_INLINE of label * var * inline
	      | CONTEXT_SDEC   of sdec
	      | CONTEXT_SIGNAT of label * var * signat
              | CONTEXT_FIXITY of fixity_table

    withtype value = (con,exp) Prim.value
    and decs = dec list

    type bnds  = bnd list
    type sdecs = sdec list
    type sbnds = sbnd list

end
