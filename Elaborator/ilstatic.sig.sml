(* Static Semantics of the IL. *)
signature ILSTATIC = 
  sig
    structure Il : IL
    val debug : bool ref
 

    val context2decs : Il.context -> Il.decs

    (* unifying arrows *)
    val comp_unify : Il.arrow Util.oneshot * Il.arrow Util.oneshot -> unit

    (* These functions test for constructor equality.
       The primed versions take context rather than decs.
       eq_con: returns true if the types are equal: will set tyvars to ensure equality
       con_unify: a version of eq_con that, upon failure to unify, will print an error message
       soft_eq_con: a version of eq_con that will not set tyvars if the unification failed *)
    val eq_con      : Il.decs * Il.con * Il.con -> bool
    val eq_con'     : Il.context * Il.con * Il.con -> bool
    val con_unify   : Il.decs * string * (string * Il.con) * (string * Il.con) * (unit -> unit) -> unit
    val con_unify'  : Il.context * string * (string * Il.con) * (string * Il.con) * (unit -> unit) -> unit
    val soft_eq_con      : Il.decs * Il.con * Il.con -> bool
    val soft_eq_con'     : Il.context * Il.con * Il.con -> bool

    (* These functions test whether the first constructor is a subtype of the second constructor *)
    val sub_con      : Il.decs * Il.con * Il.con -> bool
    val sub_con'     : Il.context * Il.con * Il.con -> bool

    (* Tests for well-formed constructs. *)
    val Decs_Valid  : Il.decs -> bool
    val Dec_Valid   : Il.decs * Il.dec -> bool
    val Sdecs_Valid : Il.decs * Il.sdecs -> bool
    val Sig_Valid   : Il.decs * Il.signat -> bool

    (* signature/sdec equivalence and subtyping *)
    val eq_sdecs    : Il.decs * Il.sdecs * Il.sdecs -> bool
    val eq_sig      : Il.decs * Il.signat * Il.signat -> bool
    val Sdecs_IsSub : Il.decs * Il.sdecs * Il.sdecs -> bool
    val Sig_IsSub   : Il.decs * Il.signat * Il.signat -> bool

    (* Obtain types and kinds and signatures *)
    val GetExpCon     : Il.decs * Il.exp  -> Il.con
    val GetConKind    : Il.decs * Il.con  -> Il.kind
    val GetModSig     : Il.decs * Il.mod  -> Il.signat
    val GetBndDec     : Il.decs * Il.bnd  -> Il.dec
    val GetBndsDecs   : Il.decs * Il.bnds -> Il.decs 
    val GetSbndsSdecs : Il.decs * Il.sbnds-> Il.sdecs

    (* Valuability *)
    val Module_IsValuable : Il.decs * Il.mod   -> bool
    val Exp_IsValuable    : Il.decs * Il.exp   -> bool
    val Bnds_IsValuable   : Il.decs * Il.bnds  -> bool
    val Sbnds_IsValuable  : Il.decs * Il.sbnds -> bool



  end
