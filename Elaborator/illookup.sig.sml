(* The translation from AST to IL. *)
signature ILLOOKUP = 
  sig
    structure Il : IL

    val debug : bool ref

    (* Lookup routines *)
    datatype class = CLASS_EXP of Il.con
                   | CLASS_CON of Il.kind
                   | CLASS_MOD of Il.signat
                   | CLASS_SIG
                   | CLASS_OVEREXP

    datatype phrase_class = PHRASE_CLASS_EXP  of Il.exp * Il.con
                          | PHRASE_CLASS_CON  of Il.con * Il.kind
                          | PHRASE_CLASS_MOD  of Il.mod * Il.signat
                          | PHRASE_CLASS_SIG  of Il.signat
                          | PHRASE_CLASS_OVEREXP of unit -> (int list -> Il.exp) * Il.con Il.Tyvar.ocon

    val Signat_Lookup  : Il.decs * (Il.path * Il.signat) * Il.label list 
                                                            -> Il.label list * class
    val Context_Lookup : Il.context * Il.label list -> phrase_class
    val modsig_lookup : Il.context * Il.label list -> (Il.path * Il.mod * Il.signat) option
 

  end;
