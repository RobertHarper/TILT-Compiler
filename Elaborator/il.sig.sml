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
                 | PRIM    of prim * con list (* no polymorphic primitives *)
                 | ILPRIM  of ilprim          (* for type-checking reasons *)
                 | VAR     of var
                 | APP     of exp * exp
                 | FIX     of arrow * fbnd list * var
                 | RECORD  of (label * exp) list
                 | RECORD_PROJECT of exp * label * con
                 | SUM_TAIL of con * exp
                 | HANDLE  of exp * exp      
                 | RAISE   of con * exp       (* annotate with the type of the raised expression *)
                 | LET     of bnd list * exp
                 | NEW_STAMP of con
                 | EXN_INJECT of exp * exp (* tag and value *)
                 | ROLL    of con * exp
                 | UNROLL  of con * exp
                 | TAG     of tag * con       (* used only for evaluation: the result of a NEW_STAMP *)
                 | INJ     of con list * int * exp
                 (* case over sum types of exp with arms and defaults*)
                 | CASE    of con list * exp * (exp option) list * exp option 
                 (* exnarms include: tag exp whose type must be CON_TAG(con) and body : con -> con_result *) 
                 | EXN_CASE of exp * (exp * con * exp) list * exp option
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
                 | CON_ARROW         of con * con * (arrow Util.oneshot)
                 | CON_APP           of con * con
                 | CON_MUPROJECT     of int * con
                 | CON_RECORD        of (label * con) list
                 | CON_FUN           of var list * con
                 | CON_SUM           of int option * con list
                 | CON_TUPLE_INJECT  of con list
                 | CON_TUPLE_PROJECT of int * con 
                 | CON_MODULE_PROJECT of mod * label
    and     kind = KIND_TUPLE of int
                 | KIND_ARROW of int * int
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
                 | SIGNAT_FUNCTOR of var * signat * signat * (arrow Util.oneshot)
    and     sdec = SDEC of label * dec
    and      dec = DEC_EXP       of var * con
                 | DEC_MOD       of var * signat
                 | DEC_CON       of var * kind * con option 
                 | DEC_EXCEPTION of tag * con


    withtype value = (con,exp) Prim.value
    and decs = dec list

    type bnds  = bnd list
    type sdecs = sdec list
    type sbnds = sbnd list

    datatype inline = INLINE_MODSIG of mod * signat
      | INLINE_EXPCON of exp * con
      | INLINE_CONKIND of con * kind
      | INLINE_OVER   of unit -> exp * (context,con) Tyvar.ocon
    datatype context_entry = 
	CONTEXT_INLINE of label * var * inline
      | CONTEXT_SDEC   of sdec
      | CONTEXT_SIGNAT of label * var * signat
      | CONTEXT_FIXITY of fixity_table   (* tracks infix precedence *)

end
