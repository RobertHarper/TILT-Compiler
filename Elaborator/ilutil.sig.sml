(*$import IL Il *)
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
    type context = Il.context
    type tag = Il.tag
    type decs = Il.decs
    type tyvar = (context,con) Tyvar.tyvar

    val fresh_con : context -> con
    val fresh_named_con : context * string -> con




    (* coercions: paths to/from exp/mods/cons *)
    val join_path_labels : Il.path * label list -> Il.path
    val path2mod : Il.path -> mod 
    val path2con : Il.path -> con
    val path2exp : Il.path -> exp
    val mod2path : mod -> Il.path option
    val exp2path : exp -> Il.path option
    val con2path : con -> Il.path option
    val eq_path  : Il.path * Il.path -> bool
    val eq_mpath : mod * mod -> bool
    val eq_cpath : con * con -> bool
    val eq_epath : exp * exp -> bool

    (* error functions *)
    val error_exp : string -> Il.exp -> string -> 'a
    val error_con : string -> Il.con -> string -> 'a
    val error_mod : string -> Il.mod -> string -> 'a
    val error_sig : string -> Il.signat -> string -> 'a

    (* derived forms *)
    val make_lambda : var * con * con * exp -> (exp * con)
    val make_total_lambda : var * con * con * exp -> (exp * con) (* no function calls in body *)
    val make_let  : (bnd list * exp) -> exp
    val make_catch : exp * con * exp * con * exp -> exp
    val make_ifthenelse : exp * exp * exp * con -> exp
    val make_seq : (exp * con) list -> exp * con
    val prim_etaexpand : (Prim.prim * con list) -> exp
    val ilprim_etaexpand : (Prim.ilprim * con list) -> exp
    val beta_reduce_mod : mod * mod -> mod option
    val beta_reduce : exp * exp -> exp

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

    (* Some internal labels are opened for lookup *)
    (* Some internal labels are non-exported *)
    (* Eq labels are internal, non-exported, and identifiable as eq labels *)
    val to_open : label -> label
    val to_nonexport : label -> label
    val to_eq: label -> label       
    val to_dt: label -> label       

    val is_open : label -> bool
    val is_nonexport : label -> bool
    val is_eq : label -> bool
    val is_dt : label -> bool

    (* Lose all characteristics *)
    val label2name : label -> string

    (* special values *)
    val con_unit : con
    val con_string : con
    val unit_exp : exp
    val fail_tag : tag
    val bind_tag : tag
    val match_tag : tag
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

    (*  ConApply: takes two types and if they are CON_FUN and CON_TUPLE or CON_VAR,
                  performs a substitution(i.e. beta reduction. 
		  If the flag is true, then the reduction occurs if each argument variable
		  is used at most once in the function body.  This prevents code explosion. *)
    val ConApply       : bool * con * con -> con


    val find_sdec : sdec list * label -> sdec option
    val find_sbnd : sbnd list * label -> sbnd option

    (* ------------ Functions that perform variable substiutions --------- *)
    (* exp_subst_expvar : takes an expression and substitutes 
                          for each expression variable in the list
       exp_subst_convar : takes an expression and substitutes 
                          for each constuctor variable in the list
       exp_subst_modvar : ...
       con_subst_expvar : takes a constructor and substitutes 
                          for each expression variable in the list
       con_subst_convar : ...
       con_subst_modvar : ...
       mod_subst_expvar : takes a module and substitutes 
                          for each expression variable in the list
       mod_subst_convar : ...
       mod_subst_modvar : ... 
       sig_subst_expvar : takes a signature and substitutes 
                          for each expression variable in the list
       sig_subst_convar : ...
       sig_subst_modvar : ... *)

    val exp_subst_expvar : (exp * (var * exp) list) -> exp
    val exp_subst_convar : (exp * (var * con) list) -> exp
    val exp_subst_modvar : (exp * (var * mod) list) -> exp
    val con_subst_expvar : (con * (var * exp) list) -> con
    val con_subst_convar : (con * (var * con) list) -> con
    val con_subst_modvar : (con * (var * mod) list) -> con
    val mod_subst_expvar : (mod * (var * exp) list) -> mod
    val mod_subst_convar : (mod * (var * con) list) -> mod
    val mod_subst_modvar : (mod * (var * mod) list) -> mod
    val sig_subst_expvar : (signat * (var * exp) list) -> signat
    val sig_subst_convar : (signat * (var * con) list) -> signat
    val sig_subst_modvar : (signat * (var * mod) list) -> signat
    val con_subst_conmodvar : (con * (var * con) list * (var * mod) list) -> con
    val mod_subst_conmodvar : (mod * (var * con) list * (var * mod) list) -> mod
    val exp_subst_expconmodvar : (exp * (var * exp) list * (var * con) list * (var * mod) list) -> exp
    val con_subst_expconmodvar : (con * (var * exp) list * (var * con) list * (var * mod) list) -> con
    val kind_subst_expconmodvar : (kind * (var * exp) list * (var * con) list * (var * mod) list) -> kind
    val sig_subst_expconmodvar : (signat * (var * exp) list * (var * con) list * (var * mod) list) -> signat


    (* ------------ Functions that compute free variables ---------  *)
    (* mod_free_expvar: given a module, return all free expression variables  
       con_free_convar: given a con, return all free convars
       con_free_modvar: given a con, return all free modvars
       con_occurs     : given a con and a tyvar, return whether the tyvar occurs in the con
       *)

    val mod_free_expvar : mod -> var list
    val con_free_convar : con -> var list
    val con_free_modvar : con -> var list
    val sig_free_conmodvar : signat -> var list * var list

    (* ----------- Functions that compute object sizes ----------- *)
    val mod_size : mod -> int
    val bnd_size : bnd -> int
    val sig_size : signat -> int


    (* ----------- Functions related to type inference ----------- 
       find_tyvars_flexes : given a con, return a list of all unset tyvars with a flag
                            indicating whether it occurred inside a CON_REF or CON_ARRAY
                            and a list of flexinfo refs
    *)
    val find_tyvars_flexes : con -> (bool * tyvar) list * Il.flexinfo ref list


    (* ------------ More Miscellaneous/General Substituter ------- *)
    type handler = (exp -> exp option) * (con -> con option) * 
	           (mod -> mod option) * (sdec -> sdec option) 
    type proj_handler = (mod * label -> exp option) * (mod * label -> con option) * 
	                (mod * label -> mod option) * (sdec -> sdec option) 

    val default_exp_handler  : exp -> exp option
    val default_con_handler  : con -> con option
    val default_mod_handler  : mod -> mod option
    val default_sdec_handler : sdec -> sdec option

    val default_exp_proj_handler  : mod * label -> exp option
    val default_con_proj_handler  : mod * label -> con option
    val default_mod_proj_handler  : mod * label -> mod option
    val default_sdec_proj_handler : sdec -> sdec option

    val sig_all_handle : handler -> signat -> signat
    val exp_all_handle : handler -> exp -> exp
    val con_all_handle : handler -> con -> con
    val mod_all_handle : handler -> mod -> mod
    val bnd_all_handle : handler -> bnd -> bnd
    val dec_all_handle : handler -> dec -> dec

    val exp_subst_allproj : proj_handler -> exp -> exp
    val con_subst_allproj : proj_handler -> con -> con
    val mod_subst_allproj : proj_handler -> mod -> mod
    val sig_subst_allproj : proj_handler -> signat -> signat

    val con_subst_conapps : (con * (con * con -> con option)) -> con

      (*
	con_subst_var_withproj : given a con c and a list of sdecs and a module m,
	                     transform c by substituting each occurrence of 
			        a variable in the sdecs with a projection from m
	remove_modvar_type : given a con, a target mod variable mv, and sdecs, 
	                       return the con after:
	                         subst each tyvar with a lookup of sdecs to the vis type
			         subst each projection from mv to the corresponding
			               vis type found by searching the sdec
			     if all occurrences of the modvar cannot be removed,
				 an exception is generated
        remove_modvar_signat : same as before except for sigs
        remove_modvar_sdec   : same as before except for sdecs
	add_modvar_type : given a con, a module expression, and sdecs, 
                            return the con after replacing each occurrence of
			    internal variables of sdecs with a projection from
			    the given module expression
	add_modvar_sig : same as before except for sigs

      rebind_free_type_var:  given a type and the current context and a variable v,
                    change all free type BUT not constructor variables to a 
		    projection of v.fresh_label().  Also take a list of labels
		    and tyvars and convert the tyvars to projections.  These are
			not considered newly generated.
                    Finally, also return the the list of the newly generated labels
                    and a bool indicating whether the type variable is an eqtype 
			       *)


    val con_subst_var_withproj    : (con * Il.sdec list * mod) -> con
    val remove_modvar_type : con * var * Il.sdecs -> con
    val remove_modvar_signat : signat * var * Il.sdecs -> signat
    val remove_modvar_sdec : Il.sdec * var * Il.sdecs -> Il.sdec
    val add_modvar_type : con * mod * Il.sdecs -> con
    val add_modvar_sig : signat * mod * Il.sdecs -> signat
    val rebind_free_type_var : Tyvar.stamp * con * context * var -> 
	                          (tyvar * label * bool) list
    val sig_mod_handler : signat * (mod -> mod option) -> signat

    val mod_resolved : mod -> bool
    val sig_resolved : signat -> bool

    (* Travels the first bnd to locate free variables for exps, cons, and mods
       and substitute in the corresponding value give by the bnd list *)
    val subst_var : Il.bnd * (Il.bnd list) -> Il.bnd


    val rename_confun : (Il.var -> bool) * (var list) * con -> con

  end;


