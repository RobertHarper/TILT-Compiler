(* Utility routines for the internal language. *)
signature ILUTIL =
  sig
    structure Il : IL

    val debug : bool ref
    exception FAILURE of string

    type var = Il.Name.var
    type label = Il.Name.label
    type exp = Il.exp
    type con = Il.con
    type kind = Il.kind
    type mod = Il.mod
    type signat = Il.signat
    type context = Il.context
    type context_entry = Il.context_entry
    type tag = Il.tag

    val fresh_con : unit -> con
    val fresh_named_con : string -> con


    (* context extenders and extractors *)
    val add_context_inline : context * label * Il.inline -> context
    val add_context_module : context * label * var * signat -> context
    val add_context_var : context * label * var * con -> context
    val add_context_convar : context * label * var 
                                        * kind * con option -> context
    val add_context_scoped_tyvars : context * Symbol.symbol list -> context
    val add_context_sdecs : context * Il.sdecs -> context
    val add_context_entries : context * context_entry list -> context
    val Context_Get_FixityTable : context -> Il.fixity_table
    val Context_Get_BoundConvars : context -> var list
    val Context_Get_ScopedConvars : context -> Symbol.symbol list


    (* coercions: paths to/from exp/mods/cons *)
    val join_path_labels : Il.path * label list -> Il.path
    val path2mod : Il.path -> mod
    val path2con : Il.path -> con
    val path2exp : Il.path -> exp
    val mod2path : mod -> Il.path

    (* error functions *)
    val error_exp : string -> Il.exp -> string -> 'a
    val error_con : string -> Il.con -> string -> 'a
    val error_mod : string -> Il.mod -> string -> 'a
    val error_sig : string -> Il.signat -> string -> 'a

    (* derived forms *)
    val make_lambda : var * con * con * exp -> (exp * con)
    val make_let  : ((var * exp) list * exp) -> exp
    val make_catch : exp * con * exp -> exp
    val make_ifthenelse : exp * exp * exp -> exp
    val con_bool : con
    val con_tuple : con list -> con
    val con_record : (Symbol.symbol * con) list -> con
    val exp_tuple : exp list -> exp
    val generate_tuple_label  : int -> label
    val generate_tuple_symbol : int -> Symbol.symbol
    val true_exp : exp
    val false_exp : exp

    (* special values *)
    val mk_lab   : label
    val km_lab   : label
    val it_lab   : label
    val case_lab : label
    val expose_lab : label
    val eq_lab   : label
    val functor_arg_lab : label
    val con_unit : con
    val con_string : con
    val unit_exp : exp
    val fail_tag : exp
    val bind_tag : exp
    val match_tag : exp
    val failexn_exp : exp
    val bindexn_exp : exp
    val matchexn_exp : exp

    (* Sort labels into canonical order *)
    val sort_labelpair : (label * 'a) list -> (label * 'a) list 
    val sort_label : label list -> label list 

    (* derefences a constructor variable if possible *)
    val con_deref : con -> con

    (*  con_constrain: given a con, constrain all free tyvars 
       con_useeq: given a con, use_eq all free tyvars
       con_free_tyvar: given a con, return all free tyvars
       con_occurs: given a con and a tyvar, returns whether tyvar occurs in con
       con_subst_var: given a con and a (variable,con) assoc list,
                     returns the con in which each tyvar is substituted 
		        according to the assoc list.
       ConApply: takes two types and if they are CON_FUN and CON_TUPLE,
                  performs a substitution(i.e. beta reduction. *)


    val con_constrain  : con -> unit
    val con_useeq      : con -> unit
    val con_free_tyvar   : con -> con Il.Tyvar.tyvar list
    val con_occurs     : con * var -> bool
    val con_subst_var  : (con * (var * con) list) -> con 
    val ConApply       : con * con -> con


      (*
        exp_subst_var: given an exp and a (variable,exp) assoc list,
                     returns the exp in which each ar is substituted 
		        according to the assoc list. 
        exp_subst_proj: given an exp and a function f, substitute each 
                           expression module projection m.l with x if 
			       the call "f(m,l)" returns "SOME x"; otherwise,
			       leave the projection alone
	con_subst_conapps: given a con and a function f, substitue each CON_APP(c1,c2)
	                     with "x" if "f(c1,c2)" returns "SOME x"; otherwise,
			        leave the CON_APP alone
	con_subst_var_withproj : given a con c and a list of sdecs and a module m,
	                     transform c by substituting each occurrence of 
			        a variable in the sdecs with a projection from m
       make_non_dependent_type : given a con, a variable mv, and sdecs,
                               return c after the following substituions.
                               subst each tyvar with a lookup of sdecs to the vis type
			           subst each projection from mv to the corresponding
				       vis type found by searching the sdec
      rebind_free_type_var:  given a type and the current context and a variable v,
                    change all free type BUT not constructor variables to a projection of v.fresh_label().
                    Also, return the the list of the newly generated labels
                    and a bool indicating whether the type variable is an eqtype 
			       *)
    val exp_subst_var  : (exp * (var * exp) list) -> exp
    val con_subst_var_withproj    : (con * Il.sdec list * mod) -> con
    val exp_subst_proj    : (exp * (mod * label -> exp option)) -> exp
    val con_subst_conapps : (con * (con * con -> con option)) -> con

    val make_non_dependent_type : con * var * Il.sdecs -> con
    val rebind_free_type_var : con * context * var -> (label * bool) list

    (* Travels the first bnd to locate free variables for exps, cons, and mods
       and substitute in the corresponding value give by the bnd list *)
    val subst_var : Il.bnd * (Il.bnd list) -> Il.bnd
  end;


