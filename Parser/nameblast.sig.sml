(*$import NAME BinIO Blaster Name *)

signature NAMEBLAST = 
  sig

    (* Since variables alpha-vary, blastInVar may read in a variable v already in use
       as determined by the construct_var function.  In this case, a new variable v'
       is instead generated.  A mapping (from v to v') is maintained and used so that
       if v is read in again, v' is used rather than create yet another variable.
       (1) The map is initially empty.  
       (2) When a variable v is blasted in,
           (a) if a mapping from v -> v' exists, we return v'
	   (b) otherwise, if v is already in use (as determined by construct_var),
	         then create a new variable v' and insert the entry v -> v'
		 else else insert the entry v -> v
       This map is maintained with the module and must be flushed for the approptiate 
       alpha-varying to occur.  For example, suppose there were several partial
       contexts each of which individually do not rebind variable but which may use
       the same variables as a result of separate compilation.  Then, the appropriate
       time to flush the map is right before each partial context is read in.
     *)

    val resetVarMap : unit -> unit

    (* blasting routines *)
    val blastInVar    : BinIO.instream -> Name.var
    val blastOutVar   : BinIO.outstream -> Name.var -> unit
    val blastInLabel  : BinIO.instream -> Name.label
    val blastOutLabel : BinIO.outstream -> Name.label -> unit
    val blastInTag    : BinIO.instream -> Name.tag
    val blastOutTag   : BinIO.outstream -> Name.tag -> unit

    val blastOutVarmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.VarMap.map -> unit
    val blastInVarmap    : BinIO.instream -> 'a Blaster.blastin -> 'a Name.VarMap.map
    val blastOutPathmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.PathMap.map -> unit
    val blastInPathmap    : BinIO.instream -> 'a Blaster.blastin -> 'a Name.PathMap.map
    val blastOutLabelmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.LabelMap.map -> unit
    val blastInLabelmap    : BinIO.instream -> 'a Blaster.blastin -> 'a Name.LabelMap.map
    val blastOutTagmap   : BinIO.outstream -> 'a Blaster.blastout -> 'a Name.TagMap.map -> unit
    val blastInTagmap    : BinIO.instream -> 'a Blaster.blastin -> 'a Name.TagMap.map


  end
