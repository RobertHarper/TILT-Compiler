(*$import Prelude TopLevel NAME BinIO Blaster Name *)

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
    val blastOutVar   : BinIO.outstream -> Name.var -> unit
    val blastInVar    : (int -> Name.var) -> BinIO.instream -> Name.var
    val blastInLabel  : BinIO.instream -> Name.label
    val blastOutLabel : BinIO.outstream -> Name.label -> unit
    val blastInTag    : BinIO.instream -> Name.tag
    val blastOutTag   : BinIO.outstream -> Name.tag -> unit

    val blastOutVarmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.VarMap.map -> unit
    val blastInVarmap    : (int -> Name.var) -> BinIO.instream -> 'a Blaster.blastin -> 'a Name.VarMap.map
    val blastOutPathmap  : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.PathMap.map -> unit
    val blastInPathmap   : (int -> Name.var) -> BinIO.instream -> 'a Blaster.blastin -> 'a Name.PathMap.map
    val blastOutLabelmap : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.LabelMap.map -> unit
    val blastInLabelmap  : BinIO.instream -> 'a Blaster.blastin -> 'a Name.LabelMap.map
    val blastOutTagmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.TagMap.map -> unit
    val blastInTagmap    : BinIO.instream -> 'a Blaster.blastin -> 'a Name.TagMap.map


  end
