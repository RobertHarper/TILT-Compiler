(* Static Semantics of the IL. *)
signature ILSTATIC = 
  sig
    structure Il : IL
    val debug : bool ref
 
    (* The exception NOTFOUND is returned on any lookup that fails. *)
    exception NOTFOUND of string

    val context2decs : Il.context -> Il.decs

    (* Side-effecting: a failure to unify will set typevars *)
    val eq_con    : Il.con * Il.con * Il.decs -> bool


    (* Side-effecting: a failure to unify will set typevars *)
    val con_unify : Il.context * string * Il.con * string * Il.con * string -> unit
    val con_unify': Il.context * string * Il.con * string * (unit -> 'a) 
                                        * Il.con * string * (unit -> 'a) -> unit
    val comp_unify : Il.arrow Il.Util.oneshot * Il.arrow Il.Util.oneshot -> unit

    (* Possibly side-effecting: if unified, tyvars are set; otherwise
         it will be as though the tyvars were unchanged *)
    val soft_eq_con : Il.con * Il.con * Il.decs -> bool

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
