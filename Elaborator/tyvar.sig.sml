signature TYVAR = 
  sig

    type '1con tyvar     (* type variable *)
    type '1con ocon      (* overloaded type with constraints *)

    val debug : bool ref
    val eq_tyvar     : '1con tyvar * '1con tyvar -> bool
    val tyvar_getvar : '1con tyvar -> Name.var
    val tyvar_deref  : '1con tyvar -> '1con option
    val tyvar_set    : '1con tyvar * '1con -> unit
    val tyvar_constrain : '1con tyvar -> unit
    val tyvar_isconstrained : '1con tyvar -> bool
    val tyvar_use_equal : '1con tyvar -> unit
    val tyvar_is_use_equal : '1con tyvar -> bool

    val ocon_getvar  : '1con ocon -> Name.var
    val ocon_is_inst : '1con ocon -> bool
    val ocon_inst    : '1con ocon -> ('1con * (Name.var * '1con tyvar) list -> '1con) ->'1con ocon
    val ocon_deref   : '1con ocon -> '1con

    (* predicate must work monotypes and be effectless if it returns false*)
    val ocon_check   : '1con ocon * ('1con * '1con -> bool) -> int list option


    val fresh_tyvar : string -> '1con tyvar   (* create an unset tyvar *)
    val fresh_var_tyvar : Name.var -> '1con tyvar  (* create unset tyvar with given var *)
    val fresh_ocon : '1con * (Name.var * '1con list) list -> '1con ocon
  end
      
