(* Utility routines for the internal language. *)
signature ILUTIL =
  sig

    val installHelpers :
	{Context_Lookup_Labels : Il.context * Il.label list -> (Il.path * Il.phrase_class) option,
	 compiling_tiltprim : bool ref,
	 eq_con : Il.context * Il.con * Il.con -> bool
	} -> unit

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
    type phrase_class = Il.phrase_class
    type tyvar = (context,con,exp) Tyvar.tyvar

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
    val error_exp : string -> exp -> string -> 'a
    val error_con : string -> con -> string -> 'a
    val error_mod : string -> mod -> string -> 'a
    val error_sig : string -> signat -> string -> 'a

    (* derived forms *)
    val make_lambda : var * con * con * exp -> (exp * con)        (* PARTIAL *)
    val make_total_lambda : var * con * con * exp -> (exp * con)  (* TOTAL *)
    val make_fold_coercion : var list * con * con -> (exp * con)
    val make_unfold_coercion : var list * con * con -> (exp * con)
    val make_let  : (bnd list * exp) -> exp
    val make_catch : exp * con * exp * con * exp -> exp
    val make_ifthenelse : context -> exp * exp * exp * con -> exp
    val make_seq : (exp * con) list -> exp * con
    val prim_etaexpand : (context * Prim.prim * con list) -> exp
    val ilprim_etaexpand : (context * Prim.ilprim * con list) -> exp

    (* some simple local reductions *)
    val exp_reduce : context * exp -> exp option
    (* just return the original expression if exp_reduce returns NONE *)
    val exp_try_reduce : context * exp -> exp

    val lab_bool : label
    val lab_true : label
    val lab_false : label

    val con_bool : context -> con
    val con_internal_bool : context -> con
    val bool_out : context -> exp
    val bool_in : context -> exp

    val internal_bool_exp : context -> bool -> exp
    val true_exp : context -> exp
    val false_exp : context -> exp
    val vector_eq : context -> mod * signat
    val word8vector_eq : context -> exp * con

    (* Wrap the internal bool type in a coercion to turn it into 
     * the external bool type.  Would probably be better to have eq functions
     * actually return internal bool and wrap the result, but who cares about 
     * generated equality functions anyway.
     * -leaf
     *)
    val to_external_bool       : context -> Prim.prim * con list * exp list -> exp
    val ilto_external_bool     : context -> Prim.ilprim * con list * exp list -> exp
    val to_external_bool_eta   : context -> Prim.prim * con list -> exp
    val ilto_external_bool_eta : context -> Prim.ilprim * con list -> exp

    val con_eqfun : context -> con -> con	(* c |-> (c * c -> bool) *)
    val eqfun_con : context * con -> con	(* (c * c -> bool) |-> c *)
    val con_tuple : con list -> con                 (* the type of tuple values *)
    val con_tuple_inject : con list -> con          (* makes a tuple of types   *)
    val con_record : (Symbol.symbol * con) list -> con
    val exp_tuple : exp list -> exp
    val generate_tuple_label  : int -> label
    val generate_tuple_labels : int -> label list (* generate labels 1..n *)
    val generate_tuple_symbol : int -> Symbol.symbol

    (* special labels *)
    val mk_lab   : label
    val km_lab   : label
    val them_lab : label
    val it_lab   : label
    val stamp_lab   : label
    val case_lab : label
    val expose_lab : label
    val functor_arg_lab : label

    val canonical_tyvar_label : bool (*is_eq*) -> int -> label

    (* special values *)
    val con_unit : con
    val con_string : con
    val unit_exp : exp
    val internal_match_tag : Name.tag	(* Not user's Match *)
    val bind_exn : context -> exp
    val match_exn : context -> exp

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
    val subst_add_sigvar : subst * var * signat -> subst
    val subst_add_exppath : subst * path * exp -> subst
    val subst_add_conpath : subst * path * con -> subst
    val subst_add_modpath : subst * path * mod -> subst

    val exp_subst : exp * subst -> exp
    val con_subst : con * subst -> con
    val mod_subst : mod * subst -> mod
    val sig_subst : signat * subst -> signat
    val sdecs_subst : Il.sdecs * subst -> Il.sdecs
    val sbnds_subst : Il.sbnds * subst -> Il.sbnds
    val entry_subst : Il.context_entry * subst -> Il.context_entry
    val entries_subst : Il.entries * subst -> Il.entries
    val decresult_subst : Il.decresult * subst -> Il.decresult

    val con_subst' : con * subst -> int * con  (* int indicates number of substitutions performed *)
    val sig_subst' : signat * subst -> int * signat

    (* ------------ Functions that compute FV --------- *)
    val exp_free : exp -> Name.VarSet.set
    val mod_free : mod -> Name.VarSet.set
    val con_free : con -> Name.VarSet.set
    val sig_free : signat -> Name.VarSet.set
    val sdec_free : sdec -> Name.VarSet.set
    val sbnd_free : sbnd -> Name.VarSet.set
    val sdecs_free : Il.sdecs -> Name.VarSet.set
    val sbnds_free : Il.sbnds -> Name.VarSet.set
    val ovld_free : Il.ovld -> Name.VarSet.set
    val entry_free : Il.context_entry -> Name.VarSet.set
    val entries_free : Il.entries -> Name.VarSet.set

    val classifier_free : phrase_class -> Name.VarSet.set
    val pc_free         : phrase_class -> Name.VarSet.set

    val findPathsInMod : mod    -> Name.PathSet.set
    val findPathsInSig : signat -> Name.PathSet.set
    val findPathsInCon : con    -> Name.PathSet.set
    val findPathsInExp : exp    -> Name.PathSet.set

    (* ----------- Functions that compute object sizes ----------- *)
    val con_size : con -> int
    val mod_size : mod -> int
    val bnd_size : bnd -> int
    val sig_size : signat -> int

    (* ------------ More Miscellaneous/General Substituter ------- *)
    type handler = (exp -> exp option) * (con -> con option) *
	           (mod -> mod option) * (sdec -> sdec option) *
		   (signat -> signat option)

    val default_exp_handler  : exp -> exp option
    val default_con_handler  : con -> con option
    val default_mod_handler  : mod -> mod option
    val default_sdec_handler : sdec -> sdec option
    val default_sig_handler  : signat -> signat option

    val sig_handle : handler -> signat -> signat
    val exp_handle : handler -> exp -> exp
    val con_handle : handler -> con -> con
    val mod_handle : handler -> mod -> mod
    val bnd_handle : handler -> bnd -> bnd
    val dec_handle : handler -> dec -> dec
    val decresult_handle : handler -> Il.decresult -> Il.decresult
    val sdecs_handle : handler -> Il.sdecs -> Il.sdecs
    val entries_handle : handler -> Il.entries -> Il.entries

      (*
        con_subst_conapps : substitute each application
	remove_modvar_type : given a con, a target mod variable mv, and sdecs,
	                       return the con after:
	                         subst each tyvar with a lookup of sdecs to the vis type
			         subst each projection from mv to the corresponding
			               vis type found by searching the sdec
			     if all occurrences of the modvar cannot be removed,
				 an exception is generated

        rebind_free_type_var: given a type and the current context and a variable v,
                    change all free type BUT not constructor variables to a projection of
                    v.fresh_label() and bind their equality expression holes, when
                    appropriate, to the corresponding equality function projection.
                    Finally, also return the the list of the newly generated labels and a
                    bool indicating whether the type variable is an eqtype.  The initial
                    int indicates how many tyvar names to skip.  *)

    val con_subst_conapps : (con * (con * con list -> con option)) -> con
    val remove_modvar_type : con * var * Il.sdecs -> con
    val rebind_free_type_var : int * Tyvar.stamp * con * context * var ->
	                          (tyvar * label * bool) list

    (* Determine whether all overloaded expressions and type metavariables are resolved *)
    val mod_resolved : mod -> bool
    val sig_resolved : signat -> bool

    (* Help with overload data.
     * ovld_default takes a label to be overloaded, a list of possible expressions, and a failure continuation
     *     and finds a suitable default (a' la' appendix E of the definiton)
     *)
    val ovld_default : label * (con * exp) list * (unit -> int option) -> int option
    val ovld_expand : Il.ovld -> (con * exp * bool) list
    val ovld_collapse : (con * exp * bool) list -> Il.ovld

    (* Help creating type/eqtype sigs *)
    val make_typearg_sdecs : context -> (label * bool) list -> Il.sdecs
    val reduce_typearg_sdecs : exp * Name.vpath * Il.sdecs -> Il.sdecs

    val ok_to_bind : Il.context * Symbol.symbol -> bool

  end;
