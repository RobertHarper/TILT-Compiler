signature NAMEBLAST =
  sig

    (* Since variables can alpha-vary and must in fact alpha-vary so that
       the same variable is not bound twice, the persistence of variables
       poses a special problem given separate compilation.

       The following scheme is used.  Variables are stored to disk by
       their underlying integer.  When read back in, a special function
       is provided by the client to convert this back to the variable.
       By sending in the a new conversion function per module, alpha-varying
       within modules can be done.

     *)

    (* blasting routines *)
    val blastOutVar   : Blaster.outstream -> Name.var -> unit
    val blastInVar    : (int -> Name.var) -> Blaster.instream -> Name.var
    val blastInLabel  : Blaster.instream -> Name.label
    val blastOutLabel : Blaster.outstream -> Name.label -> unit
    val blastInTag    : Blaster.instream -> Name.tag
    val blastOutTag   : Blaster.outstream -> Name.tag -> unit

    val blastOutVarmap   : Blaster.outstream -> 'a Blaster.blastout -> 'a Name.VarMap.map -> unit
    val blastInVarmap    : (int -> Name.var) -> Blaster.instream -> 'a Blaster.blastin -> 'a Name.VarMap.map
    val blastOutPathmap  : Blaster.outstream -> 'a Blaster.blastout -> 'a Name.PathMap.map -> unit
    val blastInPathmap   : (int -> Name.var) -> Blaster.instream -> 'a Blaster.blastin -> 'a Name.PathMap.map
    val blastOutLabelmap : Blaster.outstream -> 'a Blaster.blastout -> 'a Name.LabelMap.map -> unit
    val blastInLabelmap  : Blaster.instream -> 'a Blaster.blastin -> 'a Name.LabelMap.map
    val blastOutTagmap   : Blaster.outstream -> 'a Blaster.blastout -> 'a Name.TagMap.map -> unit
    val blastInTagmap    : Blaster.instream -> 'a Blaster.blastin -> 'a Name.TagMap.map


  end
