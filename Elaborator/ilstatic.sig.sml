(* Static Semantics of the IL. *)
signature ILSTATIC = 
  sig
    structure Il : IL
    val debug : bool ref
 

    (* ------------ functions that manipulate constructors -------------------- 
       eq_onearrow: returns true if the arrows are equal: will set the oneshot if needed
       eq_con: returns true if the types are equal: will set tyvars to ensure equality
       soft_eq_con: a version of eq_con that will not set tyvars if the unification failed 
       sub_con: tests whether the first constructor is a subtype of the second 
       con_head_normalize: returns an equivalent constructor in head-normal form 
       con_normalize: returns an equivalent constructor in normal form *)
    val eq_onearrow        : Il.arrow Util.oneshot * Il.arrow Util.oneshot -> bool
    val eq_con             : Il.context * Il.con * Il.con -> bool
    val soft_eq_con        : Il.context * Il.con * Il.con -> bool
    val sub_con            : Il.context * Il.con * Il.con -> bool
    val con_head_normalize : Il.context * Il.con -> Il.con
    val con_normalize      : Il.context * Il.con -> Il.con



    (* ---------- Tests for well-formedness ---------------------- *)
    val Decs_Valid  : Il.context * Il.decs -> bool
    val Dec_Valid   : Il.context * Il.dec -> bool
    val Sdecs_Valid : Il.context * Il.sdecs -> bool
    val Sig_Valid   : Il.context * Il.signat -> bool

    (* --------- signature/sdec equivalence and subtyping -------- *)
    val eq_sdecs    : Il.context * Il.sdecs * Il.sdecs -> bool
    val eq_sig      : Il.context * Il.signat * Il.signat -> bool
    val Sdecs_IsSub : Il.context * Il.sdecs * Il.sdecs -> bool
    val Sig_IsSub   : Il.context * Il.signat * Il.signat -> bool

    (* ---------- Obtain types and kinds and signatures ---------- *)
    val GetExpCon     : Il.context * Il.exp  -> Il.con
    val GetConKind    : Il.context * Il.con  -> Il.kind
    val GetModSig     : Il.context * Il.mod  -> Il.signat
    val GetBndDec     : Il.context * Il.bnd  -> Il.dec
    val GetBndsDecs   : Il.context * Il.bnds -> Il.decs 
    val GetSbndsSdecs : Il.context * Il.sbnds-> Il.sdecs

    (* ---------- Valuability ------------------------------------- *)
    val Module_IsValuable : Il.context * Il.mod   -> bool
    val Exp_IsValuable    : Il.context * Il.exp   -> bool
    val Bnds_IsValuable   : Il.context * Il.bnds  -> bool
    val Sbnds_IsValuable  : Il.context * Il.sbnds -> bool

    (* ---- Selfification needed before inserting into contexts ----- *)
    val SelfifySig : Il.path * Il.signat -> Il.signat
    val SelfifyDec : Il.dec -> Il.dec


  end
