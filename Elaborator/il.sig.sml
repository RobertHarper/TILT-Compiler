(*$import Prelude Fixity Name Prim Tyvar Util *)

(* The datatypes for the internal language. *)
signature IL =
  sig

    type var = Name.var
    type label = Name.label
    type labels = Name.label list
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
                 | SIGNAT_SELF of path * signat option * signat  (* self, optional unselfified sig, selfified sig *)

    and     sdec = SDEC of label * dec
    and      dec = DEC_EXP       of var * con * exp option  * bool (* true indicates should inline *)
                 | DEC_CON       of var * kind * con option * bool (* true indicates should inline *)
                 | DEC_MOD       of var * bool * signat


    and context_entry = 
	CONTEXT_SDEC   of sdec
      | CONTEXT_SIGNAT of label * var * signat
      | CONTEXT_FIXITY of label * Fixity.fixity
      | CONTEXT_OVEREXP of label * (con * exp) list

    (* A context contains 
         (A) A mapping from labels to fixity information,
	 (B) An ordered  mapping of labels and variables to some classifier information.  
	 (C) A mapping from overloaded labels to the overloaded types and expressions.
	 The labels of (B) and (C) are disjoint.
       The underlying structure of (B) consists of 
         (a) An ordered mapping of variables to classifier (phrase_class).
	     Entries in this mapping can never be shadowed so it is
	     illegal to insert a new entry with the same variable.
         (b) An unordered mapping of labels to variables which are in the first mapping.
	     Entries in this mapping can be shadowed so it is possible
	     to insert a new entry with a label that already exists.
       There are 3 additional operations we want to make possible:
         (1) Strutures that have "open" labels signify that their components
	     must be searched when performing label lookup.  Thus, the
	     corresponding internal name might be a path (and not just a variable).
	     To support these "open" lookups, we must extend variables to paths.
	 (2) We want to make obtaining classifier from a label fast.
	 (3) We want to obtain the label that maps onto a particular variable.
	     This is possible if the label has not been shadowed.
       To support this we use the following structures.
         (i)   pathMap: An unordered mapping that takes a paths to a label and a classifier
	 (ii)  ordering: A reversed list of the paths to maintain the ordering required by (a)
	                 since the pathMap in (i) is unordered
	 (iii) labelMap: A mapping that takes a label to a path and a classifier
       Notes:
         (!) Because of the possibility of shadowing, an insertion of a new label
             may require the previously existing label to be changed in both
	     the labelMap and the pathMap.
         (+) The labelMap is redundant and can be reconstructed from pathMap.
	     Thus, it does not need to be written out to disk.
    *)

    and context = CONTEXT of  {fixityMap : Fixity.fixity Name.LabelMap.map,
			       overloadMap : (con * exp) list Name.LabelMap.map,
			       pathMap  : (label * phrase_class) Name.PathMap.map,
			       ordering : path list,
			       labelMap : (path * phrase_class) Name.LabelMap.map} (* reconstructed from pathMap *)

    and phrase_class = PHRASE_CLASS_EXP     of exp * con * exp option * bool
                     | PHRASE_CLASS_CON     of con * kind * con option * bool
                     | PHRASE_CLASS_MOD     of mod * bool * signat
                     | PHRASE_CLASS_SIG     of var * signat

    withtype value = (con,exp) Prim.value
    type decs = dec list

    type bnds  = bnd list
    type sdecs = sdec list
    type sbnds = sbnd list

    type partial_context = context * label Name.VarMap.map  (* A context with free variables *)
    type module = context * partial_context * (sbnd option * context_entry) list

end
