(*$import Prelude Name Symbol Prim Tyvar IL Il *)
(* Utility routines for the internal language. *)
signature ILUTIL =
  sig

    val debug : bool ref
    exception FAILURE of string

    type var = Name.var
    type label = Name.label
    type exp = Il.exp
    type con = Il.con
    type kind = Il.kind
    type mod = Il.mod
    type bnd = Il.bnd
    type dec = Il.dec
    type sdec = Il.sdec
    type sbnd = Il.sbnd
    type signat = Il.signat
    type path = Il.path
    type context = Il.context
    type decs = Il.decs
    type tyvar = (context,con) Tyvar.tyvar

    val fresh_con : context -> con
    val fresh_named_con : context * string -> con




    (* coercions: paths to/from exp/mods/cons *)
    val join_path_labels : path * label list -> path
    val path2mod : path -> mod 
    val path2con : path -> con
    val path2exp : path -> exp
    val mod2path : mod -> path option
    val exp2path : exp -> path option
    val con2path : con -> path option
    val eq_path  : path * path -> bool
    val eq_mpath : mod * mod -> bool
    val eq_cpath : con * con -> bool
    val eq_epath : exp * exp -> bool

    (* error functions *)
    val error_exp : string -> Il.exp -> string -> 'a
    val error_con : string -> Il.con -> string -> 'a
    val error_mod : string -> Il.mod -> string -> 'a
    val error_sig : string -> Il.signat -> string -> 'a

    (* derived forms *)
    val make_lambda : var * con * con * exp -> (exp * con)        (* PARTIAL *)
    val make_total_lambda : var * con * con * exp -> (exp * con)  (* TOTAL *)
    val make_let  : (bnd list * exp) -> exp
    val make_catch : exp * con * exp * con * exp -> exp
    val make_ifthenelse : exp * exp * exp * con -> exp
    val make_seq : (exp * con) list -> exp * con
    val prim_etaexpand : (Prim.prim * con list) -> exp
    val ilprim_etaexpand : (Prim.ilprim * con list) -> exp
    val exp_reduce : exp -> exp option

    val con_bool : con
    val con_eqfun : con -> con
    val con_tuple : con list -> con                 (* the type of tuple values *)
    val con_tuple_inject : Il.con list -> Il.con    (* makes a tuple of types   *)
    val con_record : (Symbol.symbol * con) list -> con
    val exp_tuple : exp list -> exp
    val generate_tuple_label  : int -> label
    val generate_tuple_symbol : int -> Symbol.symbol
    val true_exp : exp
    val false_exp : exp


    (* special labels *)
    val mk_lab   : label
    val km_lab   : label
    val them_lab : label
    val it_lab   : label
    val stamp_lab   : label
    val case_lab : label
    val expose_lab : label
    val functor_arg_lab : label

    (* A type component with a questionable label in a signature can be synthesized
         and not merely matched during signature matching.
       Some internal labels are opened for lookup 
       Some internal labels are non-exported 
       Eq labels are internal, non-exported, and identifiable as eq labels 
    *)
    val to_questionable : label -> label
    val to_open : label -> label
    val to_nonexport : label -> label
    val to_eq: label -> label       
    val to_dt: label -> label       
    val to_cluster: label -> label       

    val is_questionable : label -> bool
    val is_open : label -> bool
    val is_nonexport : label -> bool
    val is_eq : label -> bool
    val is_dt : label -> bool
    val is_cluster : label -> bool


    val prependToInternalLabel : string * label -> label   (* Keeps characteristics *)
    val label2name : label -> string                       (* Lose all characteristics *)

    (* special values *)
    val con_unit : con
    val con_string : con
    val unit_exp : exp
    val fail_tag : Name.tag
    val bind_tag : Name.tag
    val match_tag : Name.tag
    val fail_exp : exp
    val bind_exp : exp
    val match_exp : exp
    val failexn_exp : exp
    val bindexn_exp : exp
    val matchexn_exp : exp

    (* Sort labels into canonical order *)
    val sort_labelpair : (label * 'a) list -> (label * 'a) list 
    val sort_label : label list -> label list 
    val label_issorted : label list -> bool

    (* derefences a constructor variable if possible *)
    val con_deref : con -> con

    (*  ConApply: takes a type and a list of types
                  if the first type is a CON_FUN, performs a substitution (beta reduction). 
		  If the flag is true, then the reduction occurs if each argument is a variable
		  or is used at most once in the function body.  This prevents code explosion. *)
    val ConApply  : bool * con * con list -> con

    (* ConUnroll : takes a singly-recursive mu-type or a projection of
                   a multiply-recursive mu-type and returns the unrolled version *)
    val ConUnroll      : con -> con

    val find_sdec : sdec list * label -> sdec option
    val find_sbnd : sbnd list * label -> sbnd option

    (* ------------ Functions that perform substiutions --------- *)
    type subst

    val empty_subst : subst
    val subst_is_empty : subst -> bool
    val subst_add : subst * subst -> subst  (* Must be disjoint *)
    val list2subst : (var * exp) list * (var * con) list * (var * mod) list -> subst

    val subst_add_expvar : subst * var * exp -> subst
    val subst_add_convar : subst * var * con -> subst
    val subst_add_modvar : subst * var * mod -> subst
    val subst_add_sigvar : subst * var * var -> subst
    val subst_add_exppath : subst * path * exp -> subst
    val subst_add_conpath : subst * path * con -> subst
    val subst_add_modpath : subst * path * mod -> subst

    val exp_subst : exp * subst -> exp
    val con_subst : con * subst -> con
    val mod_subst : mod * subst -> mod
    val sig_subst : signat * subst -> signat

    val con_subst' : con * subst -> int * con  (* int indicates number of substitutions performed *)
    val sig_subst' : signat * subst -> int * signat

    (* ------------ Functions that compute FV --------- *)
    val exp_free : exp -> Name.VarSet.set
    val mod_free : mod -> Name.VarSet.set
    val con_free : con -> Name.VarSet.set
    val sig_free : signat -> Name.VarSet.set
    val sbnd_free : sbnd -> Name.VarSet.set
    val entry_free : Il.context_entry -> Name.VarSet.set

    val findPathsInMod : mod    -> Name.PathSet.set
    val findPathsInSig : signat -> Name.PathSet.set
    val findPathsInCon : con    -> Name.PathSet.set

    (* ----------- Functions that compute object sizes ----------- *)
    val con_size : con -> int
    val mod_size : mod -> int
    val bnd_size : bnd -> int
    val sig_size : signat -> int


    (* ----------- Functions related to type inference ----------- 
       find_tyvars_flexes : given a con, return a list of all unset tyvars with a flag
                            indicating whether it occurred inside a CON_REF or CON_ARRAY
                            and a list of flexinfo refs
			    also performs path compression on chains of CON_TYVARs
    *)
    val find_tyvars_flexes : con -> (bool * tyvar) list * Il.flexinfo ref list


    (* ------------ More Miscellaneous/General Substituter ------- *)
    type handler = (exp -> exp option) * (con -> con option) * 
	           (mod -> mod option) * (sdec -> sdec option) 

    val default_exp_handler  : exp -> exp option
    val default_con_handler  : con -> con option
    val default_mod_handler  : mod -> mod option
    val default_sdec_handler : sdec -> sdec option

    val sig_handle : handler -> signat -> signat
    val exp_handle : handler -> exp -> exp
    val con_handle : handler -> con -> con
    val mod_handle : handler -> mod -> mod
    val bnd_handle : handler -> bnd -> bnd
    val dec_handle : handler -> dec -> dec


      (*
        con_subst_conapps : substitute each application
	remove_modvar_type : given a con, a target mod variable mv, and sdecs, 
	                       return the con after:
	                         subst each tyvar with a lookup of sdecs to the vis type
			         subst each projection from mv to the corresponding
			               vis type found by searching the sdec
			     if all occurrences of the modvar cannot be removed,
				 an exception is generated

        rebind_free_type_var:  given a type and the current context and a variable v,
                    change all free type BUT not constructor variables to a 
		    projection of v.fresh_label().  Also take a list of labels
		    and tyvars and convert the tyvars to projections.  These are
			not considered newly generated.
                    Finally, also return the the list of the newly generated labels
                    and a bool indicating whether the type variable is an eqtype 
		The initial int indicates how many tyvar names to skip.
			       *)

    val con_subst_conapps : (con * (con * con list -> con option)) -> con
    val remove_modvar_type : con * var * Il.sdecs -> con
    val rebind_free_type_var : int * Tyvar.stamp * con * context * var -> 
	                          (tyvar * label * bool) list

    (* Determine whether all overloaded expressions and type metavariables are resolved *)
    val mod_resolved : mod -> bool
    val sig_resolved : signat -> bool


  end;


