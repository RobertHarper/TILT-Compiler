functor Bblock(structure Machineutils : MACHINEUTILS) : BBLOCK =
struct
   structure Machineutils = Machineutils
   structure Machine = Machineutils.Machine

   open Machineutils Machineutils.Machine
   (* Annotations on an instruction *)
   datatype 'a annotated = NO_ANN of 'a
                         | LIVE of Regset.set * 'a

   (* All the information for a single basic block *)
   datatype bblock = BLOCK of {instrs : instruction annotated list ref,
			       def       : Regset.set,
                               use       : Regset.set,
			       in_live   : Regset.set ref,
			       out_live  : Regset.set ref,
			       truelabel : bool,
			       succs     : loclabel list ref}

   (* utility functions *)

   val error = fn s => Util.error "bblock.sml" s

   fun listToSet lst = Regset.addList(Regset.empty, lst)

   (* Find the union of a list of sets of registers *)

   fun unionLists l =
     let fun sum(nil,accum) = accum
	   | sum(h::t,accum) = sum(t,Regset.union(h,accum))
     in sum(l, Regset.empty)
     end

   val defUse = Machineutils.Machine.defUse

   fun live (LIVE (s,_)) = s
     | live (NO_ANN _) = Regset.empty

   (* Remove all annotations from an instruction *)
   fun stripAnnot (NO_ANN x) = x
     | stripAnnot (LIVE (_, a)) = a

   (* Turn a liveness annotation into a string, to be shown as a
      comment *)
   fun msAnnotation (NO_ANN _) = ""
     | msAnnotation (LIVE (live,a)) =
       let
	 fun loop [] = ""
	   | loop ((R v)::ds) = ("$" ^ (Int.toString v) ^ " ") ^ (loop ds)
           | loop ((F v)::ds) = ("$f" ^ (Int.toString v) ^ " ") ^ (loop ds)
       in loop (Regset.listItems live)
       end

   val (op -) = Regset.difference
   val \/ = Regset.union
   infix 4 \/

   fun blockDefUse (BLOCK{instrs,
			   def,
			   use,
			   in_live,
			   out_live,
	                   truelabel,
			   succs}) =
       let fun loop ([],use,def) = (use,def)
	     | loop (h :: t,use, def) =
	     	    (* We don't want special registers to appear in def/use sets *)
		 let val (def_list, use_list) = defUse (stripAnnot h)
		     val instr_def = listToSet def_list - (listToSet special_regs)
		     val instr_use = listToSet use_list - (listToSet special_regs)
	            (* variables used before being defined *)
	             val use' = use \/ (instr_use - def)
		    (* variables assigned before being used *)
		     val def' = def \/ (instr_def - use')
		 in loop(t,use',def')
		 end
	   val (use,def) = loop (rev (!instrs),Regset.empty,Regset.empty)
       in BLOCK{instrs=instrs,
		 def=def,
		 use=use,
		 in_live=ref Regset.empty,
		 out_live=ref Regset.empty,
		 truelabel=truelabel,
		 succs = succs}
       end


   (* Define in_live and out_live for every basic block using
      the blocks' def and use values. *)
   fun findLiveTemps block_map first_label =
     let
       fun getInLive (BLOCK{in_live,...}) = ! in_live

       fun getBlock blk =
	   case Labelmap.find(block_map, blk) of
	       SOME value => value
	     | NONE => error ("findLiveTemps: getblock" ^
			      (msLoclabel blk))

       fun memberLabel [] _ = false
         | memberLabel (l::rest) l' = 
	   if (eqLLabs l l') then true else memberLabel rest l'

       (* Aho/Sethi/Ullman suggests traversing the blocks in the
          OPPOSITE order from a depth-first-search through the
	  control-flow graph. *)
       val blocklabel_list =
	 let 
	   fun revDFS [] block_names = block_names
	     | revDFS (blk :: rest) block_names =
	       if (memberLabel block_names blk) then
		 revDFS rest block_names
	       else
		 let val (BLOCK{succs,...}) = getBlock blk
		 in
		   revDFS (!succs @ rest) (blk :: block_names)
		 end
	 in
(*	   revDFS (first_label :: 
		   (map (fn (l,_) => l) (Labelmap.listItems block_map))) [] *)
	   revDFS [first_label] []
	 end

       (* Flag: have we gone an entire pass through the function
	        without changing any block's live-variable values? *)
       val unchanged = ref true

       (* Make one pass through the entire function *)
       fun loop [] = ()
         | loop (blk :: rest) =
	   let 
	     val (BLOCK{def, use, in_live, out_live, succs, ...}) = 
	       getBlock blk
	     val block_liveins = map (fn l => getInLive (getBlock l)) (!succs)
             val out_live' = unionLists block_liveins
	     val in_live'  = 
	       Regset.union (use, Regset.difference(out_live', def))
	   in
	     unchanged := ((!unchanged) andalso 
			   (Regset.equal(!in_live, in_live')));
	     out_live := out_live';
	     in_live  := in_live';
	     loop rest
	   end

       (* Repeat loop() until a fixpoint is reached *)
       fun outerLoop () =      
	 (unchanged := true;
	  loop blocklabel_list;
	  if (not (! unchanged)) then outerLoop () else ())
     in
       outerLoop ()
     end

   (* This function ignores special regs, which are *always* live *)

   (* Use the in_live and out_live values determined from findLiveTemps()
      to annotate individual instructions. *)
   fun liveVars (block_map : bblock Machineutils.Labelmap.map) first_label =
     let 
	                      
       (* Scan backwards to compute which variables are live after each instruction in
	  the program.   Takes instructions in REVERSE order, returns 
	  instructions in REVERSE order. 

	  Taken from Aho, Sethi, Ullman, live-variable analysis (eq. 10.11) *)

       fun loop (out,[]) = []
	 | loop (out,instr :: instrs) =
	      let val instr = stripAnnot instr
		  val (def_list, use_list) = defUse instr
		  val instr_def = listToSet def_list - (listToSet special_regs)
		  val instr_use = listToSet use_list - (listToSet special_regs)
	          val in' = instr_use \/ (out - instr_def)
              in LIVE(out,instr) :: loop(in',instrs)
	      end

       fun annotateBlock (_, BLOCK{instrs, in_live, out_live, ...}) =
	    instrs := loop (! out_live, !instrs)

      (* compute def/use and zap bblock in_live/out_line information *)

       val block_map' = Labelmap.map blockDefUse block_map
     in
       (* find live vars at block boundaries *)
       findLiveTemps block_map' first_label;

       (* use this to annotate individual instructions *)            
       Labelmap.appi annotateBlock block_map' ;

       block_map'
     end
end

 






