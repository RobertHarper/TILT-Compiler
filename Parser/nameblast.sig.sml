(*$import NAME BinIO Blaster *)

signature NAMEBLAST = 
  sig

    (* blasting routines *)
    val blastInVar    : BinIO.instream -> Name.var
    val blastOutVar   : BinIO.outstream -> Name.var -> unit
    val blastInLabel  : BinIO.instream -> Name.label
    val blastOutLabel : BinIO.outstream -> Name.label -> unit
    val blastInTag    : BinIO.instream -> Name.tag
    val blastOutTag   : BinIO.outstream -> Name.tag -> unit

    val blastOutVarmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.VarMap.map -> unit
    val blastInVarmap    : BinIO.instream -> 'a Blaster.blastin -> 'a Name.VarMap.map
    val blastOutLabelmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.LabelMap.map -> unit
    val blastInLabelmap    : BinIO.instream -> 'a Blaster.blastin -> 'a Name.LabelMap.map
    val blastOutTagmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.TagMap.map -> unit
    val blastInTagmap    : BinIO.instream -> 'a Blaster.blastin -> 'a Name.TagMap.map


  end
