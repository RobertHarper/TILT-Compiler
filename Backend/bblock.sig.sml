(* Definitions of annotated instructions and basic blocks, and some
   simple dataflow operations on them.*)

signature BBLOCK =
sig
   structure Machineutils : MACHINEUTILS

   structure Machine : MACHINE
   sharing Machineutils.Machine = Machine

   (* Annotations on an instruction *)

   datatype 'a annotated = NO_ANN of 'a
                         | LIVE of Machineutils.Regset.set * 'a

   (* definition of a basic block:
	   instrs: Instructions are kept in REVERSE order

	   def: the set of variables definitely assigned values in B
	        prior to any uses of those variables in B

	   use: the set of variables whose values may be
	        used in B prior to any definition of the variable.

	   truelabel: true if the label of this basic block must
	   appear in the code, i.e. it is the target of branch or jump etc.

	   succs: successors in the control-flow graph 

   The sets def/use are defined according section 10.6, live-variable analysis,
   of Aho, Sethi, Ullman.

   *)

    datatype bblock = BLOCK of {instrs    : Machine.instruction annotated list ref,
 			        def       : Machineutils.Regset.set,
                                use       : Machineutils.Regset.set,
 			        in_live   : Machineutils.Regset.set ref,
 			        out_live  : Machineutils.Regset.set ref,
 			        truelabel : bool,
 			        succs     : Machine.loclabel list ref}

   (* moved from annotate.sig.sml to here, since these
      need to be global.*)

   val stripAnnot     : 'a annotated -> 'a
   val msAnnotation   : 'a annotated -> string

   val live : Machine.instruction annotated -> Machineutils.Regset.set
   val defUse : Machine.instruction -> Machine.register list * Machine.register list
   val blockDefUse : bblock -> bblock
   val liveVars : bblock Machineutils.Labelmap.map -> Machine.loclabel -> 
                        bblock Machineutils.Labelmap.map

end

