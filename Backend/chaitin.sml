(* Graph-coloring register allocator.
       Notes on constructing the interference graph for procedures.
       
       A procedure comes to us in two states:
             (1) either arguments, return, and result registers are pre-colored,
	         and assigned to specific physical registers.
	     (2) or they are unconstrained.

       We assume that register allocation has already been done on all procedures
       called in the body of this procedure.

       If the procedure is pre-colored, then 
             (1) insert moves at the beginning of the procedure from the physical
	         registers holding the arguments and return address to the
		 pseudo-registers (formals)
	     (2) insert moves from the result pseudo-registers to the
	         result physical registers at the end of the procedure.
		 End it with a return.

		 Change all returns to to jump to this post-lude code.

       When constructing the interference graph, do the following at each
       procedure call:
                   insert conflicts between (A) the physical arguments, return
	           address, result registers, and caller-save registeers
                   and (B) every variable live across the procedure call. 
		   Also insert conflicts between
		   each pseudo-register argument and return.

		   This makes certain that the procedure call does not
		   destroy any registers used by the procedure.

       When coloring the graph, you can't ``remove'' any physical registers.

       When rewriting the procedure,
               At procedure call, move the actual parameters and return
	       address to the desired physical registers.
	       
	       After the procedure call, move the results to the desired
	       physical registers.

       We can only permit callee save registers that are TRACE_YES or TRACE_NO.
*)

functor Chaitin(val commentHeader: string
		structure Printutils : PRINTUTILS
		structure Bblock : BBLOCK
		structure Callconv : CALLCONV
		structure Trackstorage : TRACKSTORAGE
		structure Ifgraph : IFGRAPH
		structure Color : COLOR
		structure Tracetable : TRACETABLE

		sharing Bblock = Printutils.Bblock
		sharing Callconv.Machine = Bblock.Machineutils.Machine
		sharing Trackstorage.Machineutils = Bblock.Machineutils
		sharing type Ifgraph.node = Bblock.Machineutils.Machine.register
		sharing Color.Trackstorage = Trackstorage
		sharing Color.Ifgraph = Ifgraph
	        sharing Tracetable = Printutils.Tracetable
		  ) : PROCALLOC =
struct

   open Bblock 
   open Callconv Color
   open Printutils Tracetable 
   open Machineutils
   open Machine


   structure Bblock = Bblock
   structure Machine = Machine
   structure Machineutils = Machineutils
   structure Tracetable = Tracetable

   val debug = ref false
   val msgs = ref false
   val delete_moves = ref true

   fun msg (x : string) = if !msgs then print x else ()

   (* time various phases *)

   fun precolor_time f = Stats.timer("precolor",f)
   fun replace_calls_time f = Stats.timer("replace calls",f)
   fun annotate_time f =      Stats.timer("annotate",f)
   fun build_graph_time f =   Stats.timer("build graph",f)
   fun summarize_time f =     Stats.timer("summarize info",f)
   fun rewrite_time f =       Stats.timer("rewrite prog",f)



   val Rdiscard = Rat


(* ------------------------------------------------------------ *)



   val error = fn s => Util.error "chaitin.sml" s

   (* this was nonstandard -- why ? *)

   val zip = Listops.zip
   fun sieve_regs (IN_REG r :: t) = r :: sieve_regs t
     | sieve_regs (_ :: t) = sieve_regs t
     | sieve_regs nil = nil

   fun mvregs (src : register, dest : register) =
       case (src,dest)
       of (R _,R _) => BASE(MOVI (src,dest))
        | (F _,F _) => BASE(MOVF (src,dest))
        | _ => error "mvregs: type of regs doesn't match"

   fun mv2reg (loc : assign,dest : register) : instruction =
     case loc
     of IN_REG r => mvregs(r,dest)
      | ON_STACK s => BASE(POP(dest,s))
      | _ => error "mv2reg: not a valid argument source"

   fun mv (src : assign,dest : assign) : instruction list =
     case src
     of IN_REG r => 
	   (case dest 
	    of IN_REG r' => [mvregs(r,r')]
	     | ON_STACK s => [BASE(PUSH(r,s))]
	     | _ => error "mv 1")
      | ON_STACK s => 
	   (case dest
	    of IN_REG r' => [BASE(POP(r',s))]
	     | ON_STACK s' => (print "Warning! mv: mem-to-mem transfer"; [])
	     | _ => error "mv 2")
      | _ => error "m: not a valid argument source"

   (* given two lists of sources and destination, make
      alist of moves *)

   fun mvlist (srcs,dests) =
         foldr (op @) nil (map mv (zip srcs dests)) 

       (* Bias info *)

       val biases : (register * int) list Regmap.map ref = ref Regmap.empty

       fun getBias pseudoreg =  
	        case Regmap.find(! biases, pseudoreg) of
		     NONE => nil
		   | SOME l => l

       fun addBias pseudoreg (physicalreg, bias) =
	 let 
	   fun loop [] = [(physicalreg, bias)]
	     | loop ((p as (reg,bias'))::rest) = 
	       if (eqRegs physicalreg reg) then
		 (reg, bias + bias') :: rest
	       else
		 p :: (loop rest)
	 in
	   if (! debug) then
	     (emitString "adding bias of ";
	      print_int bias;
	      emitString " for ";
	      print_list (print_pair print_reg print_reg) 
	           [(pseudoreg, physicalreg)]) else ();
	   case Regmap.find (! biases, pseudoreg) of
	     NONE => 
	       biases := Regmap.insert(!biases,pseudoreg,[(physicalreg, bias)])
	   | SOME lst =>
	       biases := Regmap.insert(!biases, pseudoreg, loop lst);

	   ()
	 end

       fun addPossibleBias (pseudoreg_src,pseudoreg_target, bias) =
	   case Regmap.find(!biases,  pseudoreg_src) of
	       NONE => ()
	     | SOME lst => app (fn p => addBias pseudoreg_target p) (map (fn (x,y) => (x,bias)) lst)


  (* getCallInstrRegs: calculate the physical registers used by a call *)
     
       fun getCallInstrRegs (getSignature : loclabel -> procsig) call =
	(case call
	 of (CALL{func = DIRECT (I llabel), ...}) =>
	   (case getSignature llabel of
	      PROCSIG{regs_destroyed = regs_destroyed, 
		      regs_modified = regs_modified,
		      arg_ra_pos = SOME (arg_pos),
		      res_ra_pos = SOME res_pos,
		      ...} => 
	                {regs_destroyed = ! regs_destroyed, 
			 regs_modified = ! regs_modified, 
			 arg_pos = arg_pos, 
			 res_pos = res_pos, 
			 C_call = false}
		    | _ => error "call to nonallocated function!")
           | (CALL{func = INDIRECT _, args, results, ...})=>
		 let 
		   val ACTUALS{args=arg_pos, results=res_pos} =
		     unknown_ml false (FORMALS{args=args,results=results})
                 in {regs_destroyed = indirect_caller_saved_regs,
		     regs_modified = (indirect_callee_saved_regs @
				      indirect_caller_saved_regs),
	             arg_pos = arg_pos,
		     res_pos = res_pos,
		     C_call = false}
		 end
           | (CALL{func = DIRECT (MLE _), args,results, ...}) =>
	          error "getCallInstRegs: ML extern in call"
	   | (CALL{func = DIRECT (CE (_,NONE)), args, results, 
				 argregs, resregs, destroys, ...}) =>
		  let 
		    val ACTUALS{args=arg_pos_default,
				results=res_pos_default} = std_c false 
		      (FORMALS{args=args,results=results})
		  in {regs_destroyed = (case destroys of
					  NONE => C_caller_saved_regs
					| SOME regs => regs),
		  regs_modified = (case destroys of
				     NONE => C_caller_saved_regs
				   | SOME regs => regs),
		  arg_pos = (case argregs of
			       NONE => arg_pos_default
			     | SOME regs => map IN_REG regs),
		  res_pos = (case resregs of
			       NONE => res_pos_default
			     | SOME regs => map IN_REG regs),
		  C_call = true}
              end
	   | (CALL{func = DIRECT (CE (_,SOME sra)), args, results, 
				 argregs, resregs, destroys, ...}) =>
		  let 
		    val ACTUALS{args=arg_pos_default,
				results=res_pos_default} = std_c false 
		      (FORMALS{args=args,results=results})
		    val temp = (case destroys of
				   NONE => sra::C_caller_saved_regs
				 | SOME regs => sra::regs)
		  in {regs_destroyed = temp,
		      regs_modified = temp,
		      arg_pos = (case argregs of
				   NONE => arg_pos_default
				 | SOME regs => map IN_REG regs),
		      res_pos = (case resregs of
				   NONE => res_pos_default
				 | SOME regs => map IN_REG regs),
		      C_call = true}
		  end
	  | _ => error "getCallInstRegs: not a call")

    
   (* if a function's arguments, results, and return address register are
      already assigned, then rewrite procedure as described in comments at
      the top of the file.*)

   fun pre_color (name : loclabel, arg_ra_pos,res_ra_pos,blocklabels,args,res, block_map) = 
      case (arg_ra_pos,res_ra_pos)
      of (SOME(actual_args), SOME actual_results) =>
	  let val postlude_label = freshCodeLabel()
	    val block_labels = blocklabels
	    
	      (* An empty prelude block has been created for us already *)
	      local
		(* extract current name of first block *)
		val prelude_label = (case blocklabels of
				       (a::_) => a
				     | nil => error "empty procedure!")
		(* - Move actual parameters into pseudo-registers.*)
		val prelude_instr = map mv2reg (zip actual_args args)
		val def_prelude = listToSet args
		val use_prelude = Regset.empty
	      in
		val block_map = 
		  case prelude_instr of
		    [] => block_map
		  | _ => let
			   val BLOCK{succs = oldsuccs, instrs, ...} = 
			       (case Labelmap.find(block_map,prelude_label) of
				    SOME bl => bl | NONE => error "missing block")
			   val _ = (case (!instrs) of
				      [] => ()
				    | _ => error "Preamble not empty")
			   val newbl = BLOCK{instrs=ref (rev (map NO_ANN prelude_instr)),
					       def=def_prelude,
					       use=use_prelude,
					       in_live = ref Regset.empty,
					       out_live = ref Regset.empty,
					       truelabel = false,
					       succs = ref (!oldsuccs)}
			 in
			   Labelmap.insert(block_map,prelude_label,newbl)
			 end
	      end
	    

              (* make sure actual return address has been restored.
	         The caller expects to use it to reload the gp.*)
	      val postlude_instr =
		   mvlist (map IN_REG res,actual_results) @
		   [BASE(POP_RET NONE),  (* return address offset not known yet *)
		    BASE(RTL (RETURN {results=res}))]
	      val def_postlude = Regset.empty
	      val use_postlude = listToSet res
	      val postlude = BLOCK{instrs=ref (rev (map NO_ANN postlude_instr)),
				   def=def_postlude,
				   use=use_postlude,
				   in_live = ref Regset.empty,
				   out_live = ref Regset.empty,
				   truelabel = true,
				   succs = ref []}


	      (* turn returns into jumps to postlude code, except for the
	         last instruction in the last block.  That can fall through
		 to postlude code*)
	      fun replace_returns last (label,BLOCK{instrs,succs,...}) =
		   let fun usual () = 
		           map (fn (NO_ANN(BASE(RTL (RETURN _)))) =>
					 (succs := postlude_label :: !succs;
					 NO_ANN(BASE(BR(I postlude_label))))
					  
			       | NO_ANN i => NO_ANN i
			       | _ => error "replace returns: annotated instruction!")
			     (!instrs)
		       val instrs' =
			   case Labelmap.Key.compare(last,label) of
			       EQUAL =>
		              (case !instrs
			       of (NO_ANN (BASE(RTL (RETURN _))) :: t) => 
				   (succs := postlude_label :: !succs;
				    t)
			        | _ => usual ())
			    | _ => usual ()
		   in instrs := instrs'
		   end

	      val _ = Labelmap.appi (replace_returns (hd (rev blocklabels))) block_map
	      (* add postlude *)
	      val block_map = Labelmap.insert(block_map,postlude_label, postlude)
	      val block_labels = block_labels @ [postlude_label]


(*
	      (* if prelude changed, re-insert it *)
	      val (block_map,block_labels) =
		case prelude of
		  NONE => (block_map,prelude_label :: restoflabels @ [postlude_label])
		| (SOME (tmp,prelude)) =>
		    let 
		      val orig_block = Labelmap.find(block_map,prelude_label)
		      val bl_map1 = Labelmap.insert(block_map,tmp,orig_block)
		      val bl_map2 = Labelmap.insert(bl_map1,prelude_label,prelude)
		    in (bl_map2,prelude_label :: tmp :: restoflabels @ [postlude_label])
		    end
*)




         in (block_labels,block_map)
	end
	| _  => (blocklabels,block_map)

    (* time pre-coloring phase *)

     val pre_color = precolor_time pre_color

     val (hasRpv,Rpv_virt) = (case Rpv of 
				NONE  => (false, Rat)
			      | (SOME x) => (true,x))


   (* replace_calls: rewrite calls so that moves to/from physical
      registers are explicitly in the code.   Also calculate
      the set of general ML registers that may be destroyed by
      calls.   These need to be marked as used, so that the correct
      code for callee-save may be generated.*)

   local
       val _ = ()
   in
     fun replace_calls (getSignature : loclabel -> procsig) (blockmap,tracemap) =
       let val max_on_stack = ref 0
	   val max_C_args = ref 0
	   val regs_destroyed = ref Regset.empty
	   val regs_modified = ref Regset.empty
	   fun check_arg (ON_STACK (THIS_FRAME_ARG i)) =
	              max_on_stack := Int.max (!max_on_stack,i+1)
	     | check_arg _ = ()
	   fun expand_call (call as CALL{args,func,results,
					  tailcall,destroys,...}) : instruction list =
	           let val {arg_pos,res_pos,
			    C_call,regs_destroyed=destroyed_by_call,
			    regs_modified=modified_by_call,
			    ...} = getCallInstrRegs getSignature call
		       val arg_regs = sieve_regs arg_pos
		       val result_regs = sieve_regs res_pos
		      (* update maximum num of args passed on stack *)
		       val _ = app check_arg arg_pos
		       val _ = (case func of
				  DIRECT (CE _) => max_C_args := Int.max(!max_C_args, length args)
				| _ => ())
		      (* make sure to load address of called routine into
		         Rpv if Rpv exists.   The caller may want to reset
			 certain globals with it(like gp on the Alpha). *)
		       val code : instruction list =
			   mvlist(map IN_REG args,arg_pos) @
			   (case func of
			      DIRECT (I label) => if hasRpv then [BASE(LADDR(Rpv_virt,I label))] else []
			    | DIRECT (CE (label,NONE)) => if hasRpv then [BASE(LADDR(Rpv_virt,CE (label,NONE)))] 
							  else []
			    | DIRECT (CE (label,SOME sra)) => 
				if hasRpv then [BASE(LADDR(Rpv_virt,CE (label,SOME sra)))] else []
			    | INDIRECT reg => [mvregs(reg,Rpv_virt)]
			    | _ => error "replace_calls: pv move")
			      @ (BASE(RTL(CALL{func=func,
					       args=arg_regs,
					       results=result_regs,
					       argregs=SOME arg_regs,
					       resregs=SOME result_regs,
					       destroys=destroys,
					       tailcall=tailcall})) ::
				 mvlist(res_pos,map IN_REG results))

		       (* if it is a C call, save/restore dedicated registers
			  that fall into the C caller save set.*)

		       val code' = 
			if C_call then
			    let 
				val save_across_C_reglist = save_across_C
				val save_across_C_assignlist = 
				    map IN_REG save_across_C_reglist
				val tmps = 
				let exception SpecialRegNoTrace
				    val table = 
					[(Rheap, TRACE_NO),
					 (Rhlimit, TRACE_NO),
					 (Rexnptr, TRACE_YES)]
				    fun lookup (R v) [] = 
					(print "R"; print (Int.toString v);
					 print "\n";
					 raise SpecialRegNoTrace)
				      | lookup r ((a,b)::c) =
					if (eqRegs a r) then b else (lookup r c)
				      | lookup _ _ = error "no match in lookup"
				    fun doit r = 
					let val newreg = freshIreg()
					    val _ = tracemap := 
						Regmap.insert(!tracemap,newreg, 
							      lookup r table)
					in IN_REG newreg
					end
				in map doit save_across_C_reglist
				end
			   in mvlist (save_across_C_assignlist,tmps) @
			      code @
			      mvlist (tmps,save_across_C_assignlist)
			   end
                        else code

		        (* update the set of registers destroyed by calls.
			   This includes (1) actual argument, result, return
			   registers and (2) registers in the destroy set.*)

		       val _ =
			   let 
			       val actuals = (arg_regs @ result_regs)
			   in 
			       regs_destroyed := Regset.union(!regs_destroyed,listToSet actuals);
			       regs_destroyed := Regset.union(!regs_destroyed,listToSet destroyed_by_call);
			       regs_modified  := Regset.union(!regs_modified ,listToSet actuals);
			       regs_modified  := Regset.union(!regs_modified ,listToSet modified_by_call)
			   end
		   in code'	                  
		   end
	     | expand_call _ = error "expand_call: unhandled cases"
		   
	   val nonlocal_return = ref false
           fun scan_instrs (h::t) =
	     (case (stripAnnot h : instruction) of
	         BASE(RTL (call as CALL _)) => 
		   (map NO_ANN (expand_call call)) @ scan_instrs t
	       | BASE(RTL HANDLER_ENTRY) =>
		      (
		       (* here is the crucial change that actually affects exn handling *)
		       (*
			regs_destroyed := 
			Regset.union(indirect_callee_saved_regs,
			         !regs_destroyed); *)
			nonlocal_return := true;
		       h :: scan_instrs t)
	       | _ => h :: scan_instrs t)
             | scan_instrs nil = nil
       in 
	   Labelmap.app (fn (BLOCK{instrs,...})
			 => instrs := rev(scan_instrs (rev(!instrs)))) blockmap;
	   (!max_on_stack, !max_C_args, 
	    Regset.listItems (Regset.intersection(listToSet general_regs,!regs_destroyed)),
	    Regset.listItems (Regset.intersection(listToSet general_regs,!regs_modified)))
       end (* replace_calls *)

       val replace_calls = (fn arg1 => replace_calls_time (replace_calls arg1))
   end (* local *)

   (* print an interference graph *)

   fun print_graph g = 
	 let
	   val nodes = Ifgraph.nodes g
	   fun print_node n = 
	     (print_reg n;
	      emitString ":  ";
	      print_list print_reg (Ifgraph.edges g n))
	 in
	   app print_node nodes;
	   TextIO.flushOut TextIO.stdOut
	 end


   (* Build interference graph.    Physical registers appear in the graph.*)

       fun buildGraph (getSignature,name,block_map, 
		       args,res,callee_saved) : Ifgraph.graph =
	 let val igraph = Ifgraph.empty ()
	     val insert_node = Ifgraph.insert_node igraph
	     val insert_edge = Ifgraph.insert_edge igraph

	   fun addConflict n n' =
	        (if (! debug) then
		  (emitString "adding edge between ";
		   print_reg n';
		   emitString " and ";
		   print_reg n;
		   emitString "\n")
		 else ();
		 insert_edge(n', n))
	       
	   fun processInstr instr =
	     let
	       val (def_regs,_) =  Bblock.defUse (stripAnnot instr)
	       val live_regs = Bblock.live instr
	       fun process_defined reg = 
		       Regset.app (addConflict reg) live_regs
             in (* add conflicts between pseudo-regs live after
		   instruction and pseudo-regs defined by 
		   instruction *)
		app process_defined def_regs;

		(* don't add nodes for pseudo-regs def'd 
		   if there are no registers live after the
		   instruction.   Those regs are dead, and
	           wil be assigned to Rzero/Fzero *)
(* 
	        app insert_node def_regs;
*)
	        case (stripAnnot instr) of
		 (BASE(RTL(call as (CALL{func, args, results, ...})))) =>
		   let
		     val {regs_destroyed, ...} =
		        getCallInstrRegs getSignature call

		     (* add conflicts between registers destroyed
		        by call and variables live after call.*)

		   in Regset.app process_defined (listToSet regs_destroyed)
		   end
                | _ => ()
	     end
	   
	   fun processBlock (BLOCK{in_live, instrs, ...}) =
	      (app processInstr (!instrs))

       (* add_list:
	  (1) add every variable in a list to the interference graph.
      and (2) add conflicts between every pair of variables that
	      can be chosen from the list.*)

	   fun add_list l =
	     let fun varLoop _ [] = ()
	           | varLoop v (x::xs) = 
		         (addConflict v x;
			  varLoop v xs;
			  varLoop x xs)
	     in case l
		of [a] => insert_node a  (* special case of singleton list *)
		 | h::t => varLoop h t
		 | [] => ()
	     end

	 in
	  (* not needed since the graph is pre-colored for 
	     moves to and from argumsnts, results and return.:

	     build full graph on (return::args)
	     add_list (return :: args @ res);
	   *)

           (* return pseudo-reg cannot be placed in callee-saved registers,
	      since the last thing we do before returning is restore the
	      callee-saved registers.*)
 
	   (* if the functions arguments/results are not pre-colored,
	      but are to be allocated.    they can't be placed in callee-saved
	      registers either. This restriction isn't implemented right
	      now.*)

	   Labelmap.app processBlock block_map;
	   if (! debug) then print_graph igraph else ();
	   igraph
	 end

         val buildGraph = fn arg => build_graph_time buildGraph arg

   (* generate tables of information for the garbage collector *)

   datatype callsite_info = CALLSITE of {label : loclabel,live : Regset.set}


    (* rewrite a block to use physical registers *)

   fun allocateBlock (mapping,stackframe_size,fixStackOffset,tailcallImpossible,name) callee_save_slots
                     (BLOCK{in_live, use, def, instrs,...}) =
	   let
	     val _ = if (! debug) then emitString "AllocateBlock\n" else ()
             val pop = fn (d,l) => pop(d,fixStackOffset l)
	     val push = fn (s,l) => push(s,fixStackOffset l)
	       
	     (* track live variables at each call site *)

	     val callsite_info = ref []
	     fun add_info i = callsite_info := CALLSITE i :: (!callsite_info)

	     exception DEAD
             exception GETREGBUG

	     fun getreg r =
		 if isPhysical r
		 then IN_REG r
		 else (case Regmap.find (mapping,r) of
		         NONE => (print ("Warning! getreg: missing register binding: nulling instruction"^ 
					msReg r ^" (dead variable ?)\n"); 
				  raise GETREGBUG)
		       | SOME assign => assign)
	     fun getreg_posdead r =
		 if isPhysical r
		 then IN_REG r
		 else (case Regmap.find (mapping,r) of
		         NONE => raise DEAD
		       | SOME assign => assign)


	     val _ = if (! debug) then
	       (emitString "in_live\n";
		print_set (! in_live);
		emitString "mapping\n";
		print_map mapping) else ()

	     fun putInRegs src_regs dst_regs =
	       let
		 val temps = [Rat, Fat, Rat2, Fat2]
		 val free_temps = Regset.listItems
		   (Regset.difference(listToSet temps, 
				      listToSet src_regs))

		 fun pickTemp _ [] = error "pickTemp"
		   | pickTemp (R _) ((ireg as R _) :: rest) = (ireg, rest)
		   | pickTemp (ireg as R _) (_ :: rest) = pickTemp ireg rest
		   | pickTemp (F _) ((freg as F _) :: rest) = (freg, rest)
		   | pickTemp (freg as F _) (_ :: rest) = pickTemp freg rest

		 fun allocSources _ [] precode localmap = (precode, localmap)
                   | allocSources temps_left (src :: rest) precode localmap = 
		     (case getreg src of
			IN_REG r => 
			  allocSources temps_left rest precode
			     (Regmap.insert(localmap, src, r))
		      | ON_STACK offset =>
			  let
			    val (this_temp, temps_left) = 
			      pickTemp src temps_left
			  in
			    allocSources temps_left rest
			      (pop(this_temp,offset) :: precode)
			      (Regmap.insert(localmap, src, this_temp))
			  end
		      | _ => error "putInRegs: allocSources")

		 fun allocDests _ [] postcode localmap = (postcode, localmap)
                   | allocDests temps_left (dst :: rest) postcode localmap = 
		     ((case getreg_posdead dst
		      of IN_REG r => 
			  allocDests temps_left rest postcode
			     (Regmap.insert(localmap, dst, r))
		      | ON_STACK offset =>
			  let
			    val (this_temp, temps_left) = 
			      pickTemp dst temps_left
			  in
			    allocDests temps_left rest
			      ((push(this_temp, offset)) :: postcode)
			      (Regmap.insert(localmap, dst, this_temp))
			  end
		      | _ => error "putInRegs: allocDests")
			handle DEAD => (emitInstr "" (BASE (COMMENT ("allocdest: dead reg" ^
   					   (msReg dst))));
                                        (allocDests temps_left rest postcode localmap)))

		 val (precode, srcmap) = 
		   allocSources free_temps src_regs [] (Regmap.empty)

		 val (postcode, dstmap) = 
		   allocDests temps dst_regs [] (Regmap.empty)
	       in
		 {precode = precode,
		  srcmap = srcmap,
		  dstmap = dstmap,
		  postcode = postcode}
	       end

	     fun allocateInstr (NO_ANN _) _ = error "allocateInstr: unannotated"
	       | allocateInstr (LIVE (live,instr)) next_label =
	       (case instr of
		  BASE(ILABEL _) => [instr]
		  | BASE(RTL (CALL{func, args, results, 
				   tailcall, ...})) =>
		    (let val return_label = (case next_label of
					       NONE => freshCodeLabel()
					     | SOME l => l)
			 (* Store live variables for GC *)
		      val ra_sloc = fixStackOffset RETADD_POS
		      val maybe_overflow_args = tailcallImpossible()
		      val _ = 
			case (maybe_overflow_args,func, tailcall) of
			  (true,_,_) =>
			    add_info {label=return_label,live=live} 
			    | (_,DIRECT (CE _), _) => 
			    add_info {label=return_label,live=live} 
			    | (_,_, true) => ()
			    | _ => add_info {label=return_label,live=live}
			    
		      fun stack_fixup_code1 () = 
			(map (fn (reg,sloc) => pop(reg,sloc)) callee_save_slots) 
		      val stack_fixup_code2 = [increase_stackptr stackframe_size]
			
		      fun ld2reg (IN_REG r, r') = [mvregs(r, r')]
			| ld2reg (ON_STACK offset, r') = [pop(r', offset)]
			| ld2reg _ = error "allocateInstr/ld2reg: bad assignment"
			
		      val no_moddef_info = { regs_modified = [] : register list,
					     regs_destroyed = [] : register list,
					     args = [] : register list}
		      (* Actual call *)
		      val br_instrs : instruction list = 
			case (func, tailcall) of
			  (DIRECT (I label), false) => 
			    ([BASE(BSR (I label, NONE, no_moddef_info)),
			      BASE(ILABEL return_label)] @
			     (std_return_code NONE))
			| (DIRECT (CE (label,NONE)), false) =>
			    ([BASE(BSR (CE (label,NONE), NONE, no_moddef_info)),
			      BASE(ILABEL return_label)] @
			     (std_return_code NONE))
			| (DIRECT (CE (label,SOME sra)), false) =>
			    ([BASE(BSR (CE (label,SOME sra), SOME sra, no_moddef_info)),
			      BASE(ILABEL return_label)] @
			     (std_return_code(SOME sra)))
			| (DIRECT (MLE _), _) => error "br_instrs: ML extern"
			| (INDIRECT _, false) =>
			    ([BASE (JSR(true, Rpv_virt, 1, [])),
			      BASE(ILABEL return_label)] @
			     (std_return_code NONE))
			    
			| (DIRECT (I label), true) =>
			    if (not maybe_overflow_args)
			      then
				(stack_fixup_code1 ()) @ 
				[BASE(POP_RET(SOME(ra_sloc)))] @
				stack_fixup_code2 @
				[BASE(BR (I label))]
			    else 
			      [BASE(BSR (I label, NONE, no_moddef_info)),
			       BASE(ILABEL return_label),
			       BASE(POP_RET(SOME(ra_sloc)))] @
			      (stack_fixup_code1 ()) @ 
			      stack_fixup_code2 @
			      [BASE(RET (false, 1))]
			      
			| (DIRECT (CE (label,NONE)), true) =>
			      if (not maybe_overflow_args)
				then
				  (stack_fixup_code1 ()) @ 
				  [BASE(POP_RET(SOME(ra_sloc)))] @
				  stack_fixup_code2 @
				  [BASE(BR (CE (label,NONE)))]
			      else
				[BASE(BSR(CE (label,NONE), NONE, no_moddef_info)),
				 BASE(ILABEL return_label),
				 BASE(POP_RET(SOME(ra_sloc)))] @
				(stack_fixup_code1 ()) @ 
				stack_fixup_code2 @
				[BASE(RET (false, 1))]
				       
				       
			| (DIRECT (CE (label,SOME sra)), true) =>
				if (not maybe_overflow_args)
				  then
				    (stack_fixup_code1 ()) @ 
				    [BASE(POP(sra,ra_sloc))] @
				    stack_fixup_code2 @
				    [BASE(BR (CE (label, SOME sra)))]
				else
				  [BASE(BSR (CE (label, SOME sra), SOME sra, no_moddef_info)),
				   BASE(ILABEL return_label),
				   BASE(POP_RET(SOME(ra_sloc)))] @
				  (stack_fixup_code1 ()) @ 
				  stack_fixup_code2 @
				  [BASE(RET (false, 1))]
				  
			| (INDIRECT _, true) => 
				  if (not maybe_overflow_args)
				    then
				      (stack_fixup_code1 ()) @ 
				      [BASE(POP_RET(SOME(ra_sloc)))] @
				      stack_fixup_code2 @
				      [BASE(JSR (false, Rpv_virt, 1, []))]
				  else
				    [BASE (JSR (true, Rpv_virt, 1, [])),
				     BASE(ILABEL return_label),
				     BASE(POP_RET(SOME(ra_sloc)))] @
				    (stack_fixup_code1 ()) @ 
				    stack_fixup_code2 @
				    [BASE(RET(false, 1))]
		     in br_instrs
		     end) (* allocateCall *)
				 | BASE(RTL(RETURN{results})) =>
		       let val callee_restore_code = 
			 map (fn (reg,sloc) => pop(reg,sloc)) callee_save_slots
			   val res : instruction list = callee_restore_code @ 
			     [increase_stackptr stackframe_size,
			      BASE(RET(false, 1))]
		       in  res
		       end
		       | BASE(RTL HANDLER_ENTRY) => []
		       | BASE(RTL (SAVE_CS _)) => []
		       | BASE(GC_CALLSITE llabel) => (add_info{label=llabel,live=live}; [])
		       | BASE(MOVI (src,dest)) =>
		       let val dest' = getreg_posdead dest
			 val src' = getreg src
		       in
			 case (src',dest') of
			   (IN_REG r,IN_REG r') => 
			     if !delete_moves andalso eqRegs r r' 
			     then []
			     else [BASE(MOVI(r,r'))]
		      | (ON_STACK l,IN_REG r') => [pop(r',l)]
		      | (IN_REG r,ON_STACK l') => [push(r,l')]
		      | (ON_STACK l,ON_STACK l') =>
			       [pop(Rat,l),push(Rat,l')]
		      | _ => error "allocateInstr: MOVI"
		    end
		 | BASE(MOVF (src,dest)) =>
		    let val dest' = getreg_posdead dest
		        val src' = getreg src
		    in
		      case (src', dest') of
		        (IN_REG r,IN_REG r') =>
			     if !delete_moves andalso eqRegs r r' 
			     then []
			     else [BASE(MOVF(r,r'))]
		      | (ON_STACK l,IN_REG r') => [pop(r',l)]
		      | (IN_REG r,ON_STACK l') => [push(r,l')]
		      | (ON_STACK l,ON_STACK l') =>
			       [pop(Fat,l),push(Fat,l')]
		      | _ => error "allocateInstr: MOVF"
		    end
                 | _ =>
			let val (def,use) = defUse instr
			    val {precode, srcmap, dstmap, postcode} = putInRegs use def
			    (* fs: find source *)
			    fun fs r = if isPhysical r then r else 
				(case Regmap.find(srcmap,r) of
				     SOME r => r
				   | NONE => error "fs failed")
			    fun fd (r : register) : register = 
				if (isPhysical r) then r else 
					(case (Regmap.find(dstmap,r)) of
					  NONE => raise DEAD
                                        | (SOME r) => r)

		            val instr' =
			       case instr of
				  BASE(JSR(link, Raddr, hint, labels)) =>
					  BASE(JSR(link, fs Raddr,hint, labels))
				| BASE(RET args) => BASE(RET args)
				| BASE(PUSH(src,al)) => push(fs src,al)
				| BASE(POP(dst,al)) => pop(fd dst,al)
				| i => translate_to_real_reg(i,fs,fd)

			in precode @ (instr' :: postcode)
			end)
	                (* allocateInstr *)
	       
	     fun instructionLoop [] = []
               | instructionLoop (instr :: rest) = 
	       ((let val next_label = (case rest of
					 (LIVE(_,BASE(ILABEL l)))::_ => SOME l
				       | _ => NONE)
		     val tmp = allocateInstr instr next_label
		 in  tmp @ (instructionLoop rest)
		 end) 
		   handle DEAD => (emitInstr "" (BASE (COMMENT ("dead instr" ^
								(msInstruction "" (stripAnnot instr)))));
				   (instructionLoop rest))
			| GETREGBUG => (print "GETREGBUG while processing procedure ";
					print (msLoclabel name);
					print ": ";
					print (msInstruction "" (stripAnnot instr));
					raise GETREGBUG)
			| e => (error "UNK ERROR while processing: ";
				print (msInstruction "" (stripAnnot instr));
				raise e))
		   
	     val instrs_in = rev (! instrs)

	     val _ = if (! debug) then
	       (emitString "block in:\n";
		app ((emitInstr "") o stripAnnot) instrs_in) else ()

	     val instrs_out = 
	       instructionLoop instrs_in
	       handle e => (print "exception in allocateBlock ignored\n";
			    print "No vblock code will be generated.\n";
			    [])

	     val _ = if (! debug) then
	       (emitString "block out:\n";
		app (emitInstr "") instrs_out) else ()

	   in
	     instrs := map NO_ANN (rev instrs_out);
	     !callsite_info
	   end (* allocateBlock *)

           val allocateBlock = fn arg1 => rewrite_time (allocateBlock arg1)

       (* arguments: 
	       . name of site
	       . summary of storage information
	       . mapping from pseudo-regs to locations
	       . mapping from pseudo-regs to traceability info
	       . list of call sites
        *)

       fun getCallInfo (name as (Rtl.LOCAL_CODE v)) summary mapping tracemap 
				(l : callsite_info list)=
	let 
	     val rawname = Name.var2string v
	     val Trackstorage.SUMMARY
	            {registers_used,
		     stackframe_size,
		     callee_save_slots,
		     fixStackOffset,...} = summary

	     fun loop nil = nil
	       | loop (CALLSITE{label,live} :: rest)  =
		   let

		       fun get_stackloc spos =
			 sloc2int (fixStackOffset spos)

		       (* trace information for callee-saved registers spilled
			  to stack at beginning of procedure.*)

		       val callee_save_spilled_info =
			   let 
			     fun loop nil = nil
			       | loop ((reg as R _,sloc)::t) =
			       (sloc2int sloc,Tracetable.TRACE_CALLEE reg) :: (loop t)
			       | loop (_ :: t) = loop t
			   in loop callee_save_slots
			   end

		       (* trace information for callee-saved registers not
			  touched by this procedure.*)
 
		       val callee_save_regs_info =
			   let val (op -) = Regset.difference

			       (* assume that all registers not used by this
				  procedure are callee-save.*)
 
			       val implicitly_saved =
				  Regset.listItems (listToSet general_iregs - 
						    listToSet registers_used)
			   in map (fn r => (r,Tracetable.TRACE_CALLEE r))
			          implicitly_saved
			   end
	       
		       (* take live psuedo-regs *)

		       val live = Regset.listItems live

		       (* map them to physical locations.   Ignore any
			  physical registers encountered in the live set.
			 Any physical registers which are live MUST hold
			 arguments to the called function --- they'll be traced 
			 inside the call if necessary.

			 Ignore floating pointer registers, since they're never
			 traceable.*)

		       (* calculate the real stack location of a register holding
			  trace information.*)
 
 		       fun stack_trace_location x =
			   case x
			     of TRACE_STACK loc => TRACE_STACK (fixStackOffset loc)
			   | TRACE_STACK_REC (loc,i) =>
			       TRACE_STACK_REC (fixStackOffset loc,i)
			   | _ => x
			   

		       val physical_locations = 
		          let fun loop nil = nil
			        | loop (h::t) =
			        let fun fail x =
				   error("getCallInfo: "^msReg h^x)
				in if isPhysical h then loop t
				   else if isFloat h then loop t
				   else 
				     case (Regmap.find(mapping,h),
					   Regmap.find(tracemap,h)) of
				       (SOME loc,SOME trace) => 
					 let val trace = stack_trace_location trace
					 in  (loc,trace) :: loop t
					 end
				     | (SOME _,NONE) => fail "missing trace info"
				     | (NONE,SOME _) => fail "missing phys loc"
				     | _ => fail "missing both"
				end
			  in loop live
			  end

		       fun split ((loc,trace) :: t,regs,stack) =
			    (case loc
			     of IN_REG x =>    split(t,(x,trace)::regs,stack)
			      | ON_STACK loc => split(t,regs,(get_stackloc loc,
							      trace)::stack)
			      | _ => error "getCallInfo: split")
			 | split (nil, regs, stack) = (regs, stack)

		       val (regtrace,stacktrace) = 
			              split (physical_locations,nil,nil)

		       val regtrace = regtrace @ callee_save_regs_info
(*
		       fun tr2s TRACE_NO                  = "no"
			 | tr2s TRACE_YES                 = "yes"
			 | tr2s TRACE_UNSET               = "unset"
			 | tr2s (TRACE_CALLEE  r)         = "callee"
			 | tr2s (TRACE_STACK sloc) = "stack"
			 | tr2s (TRACE_STACK_REC (sloc,i)) = "stack_rec"
			 | tr2s _          = 	"<ignoring trace>"
			   
		       val _ = (print "chaitin.sml processing label = ";
				print (Machineutils.Machine.msLoclabel label); print "\n";
				print "regtrace:\n";
				app (fn (v,t) => (print (msReg v); print " => ";
						  print (tr2s t); print "\n")) regtrace;
				print "\n\n")
*)
		       val stacktrace = stacktrace @ callee_save_spilled_info
		   in
		       (Tracetable.CALLINFO
			{calllabel = Tracetable.CALLLABEL label,
			 framesize = stackframe_size,
			 retaddpos = sloc2int (fixStackOffset RETADD_POS),
			 regtrace  = regtrace,
			 stacktrace = stacktrace}) :: loop rest
		   end (* loop *)
	 in loop (l : callsite_info list)
	 end (* getCallInfo *)

       fun initBias block_map =
	  let fun processInstr i =
	        let fun domv (src,dest) =
		    case (isPhysical src,isPhysical dest)
		    of (true,true) => ()
		     | (true,false) => addBias dest (src,1)
		     | (false,true) => addBias src (dest,1)
		     | (false,false) => addPossibleBias (src,dest,1)
	        in case stripAnnot i
		  of BASE(MOVI (src,dest)) => domv(src,dest)
		   | BASE(MOVF (src,dest)) => domv(src,dest)
		   | _ => ()
	        end
	      fun processBlock (BLOCK{instrs,...}) =
		      app processInstr (!instrs)
         in biases := Regmap.empty;
	    Labelmap.app processBlock block_map
	 end


       fun createPostamble (block_map,name,arg_ra_pos,
			     Trackstorage.SUMMARY{callee_save_slots, fixStackOffset,
						  stackframe_size,...}) =
	   (case arg_ra_pos of
	      NONE => error "createPostamble"
	    | SOME (arg_pos) => 
		let val BLOCK {instrs,...} = (case Labelmap.find(block_map,name) of
						  SOME bl => bl
						| _ => error "missing block")
		  val i_in = !instrs
		  fun realize_retadd (BASE(POP_RET NONE)) =  SOME(BASE(POP_RET(SOME(fixStackOffset RETADD_POS))))
		    | realize_retadd (BASE(POP_RET (SOME _))) =  error "createPostamble: already realized!!!" 
		    | realize_retadd _ = NONE
		  fun loop [] = error "createPostamble: no reload" 
		    | loop (a::b) = 
		    let val temp = 
		      case a of
			(NO_ANN i) => (case (realize_retadd (i)) of
					 NONE => NONE | (SOME ii) => SOME (NO_ANN ii))
	  	             | (LIVE (rs,i)) => (case (realize_retadd (i)) of
						   NONE => NONE | (SOME ii) => SOME(LIVE(rs,ii)))
		    in (case temp of
			  (SOME res) => res::b
			| NONE => a::(loop b))
		    end
		  val i_out = loop i_in 
		in instrs := i_out
		end)
		

   fun createPreamble (block_map,name,arg_ra_pos,
			     Trackstorage.SUMMARY{callee_save_slots,
						  stackframe_size,fixStackOffset,...}) =
	   (case arg_ra_pos of
	      NONE => error "createPreamble"
	    | SOME (arg_pos) => 
		let 
		  val BLOCK {instrs,...} = (case Labelmap.find(block_map,name) of
						SOME bl => bl | NONE => error "missing block")
		  val reversed_i = ((std_entry_code()) @
				    [decrease_stackptr stackframe_size,
				     BASE(PUSH_RET(SOME(fixStackOffset RETADD_POS)))] @
				    (map (fn (r, i) => push(r, i)) callee_save_slots))
		  val ordered_i = rev (map NO_ANN reversed_i)
		in instrs := !instrs @ ordered_i
		end)
		
   fun allocateProc1 (blah as
		      {getSignature : loclabel -> procsig,
		       external_name : Rtl.label option,
		       name         : loclabel,
		      block_map    : bblock Labelmap.map,
		      procsig = procsig as PROCSIG{arg_ra_pos=orig_args,
						   res_ra_pos,
						   allocated,
						   regs_destroyed,
						   regs_modified,
						   framesize,
						   ra_offset,
						   callee_saved,
						   blocklabels,
						   args, res} : procsig,
		       stack_resident : Machine.stacklocation
		                        Machineutils.Regmap.map,
		       tracemap     : Tracetable.trace Regmap.map}) = 

     let
       val _ = msg "\tentered allocateproc1\n"
       val local_tracemap = ref tracemap
       (* map arguments on stack to proper location *)

       val arg_ra_pos = 
	   case orig_args
	   of NONE => NONE
	    | (SOME l) =>
	        SOME(map (fn (ON_STACK (THIS_FRAME_ARG i)) =>
	                          ON_STACK(CALLER_FRAME_ARG i)
	                      | a => a) l)

      val _ = if !debug then
	           (emitString commentHeader;
		    emitString " before precoloring procedure\n";
	            dumpProc(name,external_name,procsig,
			     block_map,blocklabels, true))
            else ()

       val (block_labels,block_map) = 
	    pre_color(name,arg_ra_pos,res_ra_pos,blocklabels,
		      args,res,block_map)

      val _ = if !debug then
	           (emitString commentHeader;
		    emitString " result of precoloring procedure\n";
	            dumpProc(name,external_name,procsig,
			     block_map,block_labels, true))
            else ()

      val (max_passed_args, max_C_args, regs_destroyed, regs_modified) =
	       (msg "\treplacing calls\n";
		replace_calls getSignature (block_map, local_tracemap))

       val _ = if !debug then
	           (emitString commentHeader;
		    emitString " dumping procedure after expanding calls\n";
		    dumpProc (name,external_name,procsig, block_map, block_labels, !debug);
		    emitString commentHeader;
		    emitString " done expanding\n")
	       else ()
       val _ = msg  "\tannotating\n"
	   
       val block_map = (annotate_time (Bblock.liveVars block_map)) name
	   
       val _ = 
	   if (! debug) then 
	       (emitString commentHeader;
		emitString " dumping procedure after annotation\n";
		dumpProc (name, external_name,procsig, block_map, block_labels, !debug);
		emitString commentHeader;
		emitString " done annotation\n")
	   else ()
       val _ = msg "\tleaving allocateproc1\n"
     in
	 ({getSignature = getSignature,
	   name = name,
	   external_name = external_name,
	   block_map = block_map,
	   procsig = PROCSIG{arg_ra_pos=orig_args,
			     res_ra_pos = res_ra_pos,
			     allocated = allocated,
			     regs_destroyed = ref regs_destroyed,
			     regs_modified = ref regs_modified,
			     framesize = framesize,
			     ra_offset = ra_offset,
			     callee_saved = callee_saved,
			     blocklabels = blocklabels,
			     args = args, res = res},
	   stack_resident = stack_resident,
	   tracemap = !local_tracemap},
	  arg_ra_pos, max_passed_args, max_C_args, block_labels)
     end (* allocateProc1 *)



   fun allocateProc2 ({getSignature : loclabel -> procsig,
		       external_name : Rtl.label option,
		       name         : loclabel,
		       block_map    : bblock Labelmap.map,
		       procsig as PROCSIG{arg_ra_pos=orig_args,
						    res_ra_pos,
						    allocated,
						    regs_destroyed,
						    regs_modified,
						    framesize,
						    ra_offset,
						    callee_saved,
						    blocklabels,
						    args, res} : procsig,
		       stack_resident : Machine.stacklocation
		                        Machineutils.Regmap.map,
		       tracemap     : Tracetable.trace Regmap.map},
		      arg_ra_pos : assign list option, 
		      max_passed_args, max_C_args, block_labels) = 

       let

	   (* if there is a HANDLER_ENTRY in here, add modified to destroyed for saving purposes *)
	   local
	       val has_exn_flag = ref false
	       fun has_exn [] = false
		 | has_exn ((BASE(RTL HANDLER_ENTRY))::_) = true
		 | has_exn (_::rest) = has_exn rest
	       val instr_blocks = 
		   Labelmap.app (fn (BLOCK{instrs,...}) => 
				 if (has_exn (map stripAnnot (!instrs)) )
				     then (has_exn_flag := true) else ()) block_map
	   in
	       val _ = if (!has_exn_flag)
			   then regs_destroyed := (!regs_destroyed) @ (!regs_modified)
		       else ()
	   end

       val _ = if !msgs then
	         print ("processing procedure "^msLoclabel name^"\n")
	       else ()

       (* initialize information on storage *)

       val storage_info = 
	 Trackstorage.newInfo {callee_saves = callee_saved,
			       stack_resident = stack_resident,
			       max_on_stack = max_passed_args,
			       max_C_args = max_C_args,
			       regs_destroyed = (!regs_destroyed)}

       val stackOffset = Trackstorage.stackOffset storage_info
       val _ = initBias block_map
       val _ = msg "\tbuilding interference graph\n"
       val igraph = buildGraph(getSignature,name,block_map,
			       args,res,callee_saved)
       val _ = if (!msgs) then Ifgraph.print_stats igraph else ()
       val _ = msg "\tcoloring interference graph\n"
       val mapping = color(igraph,storage_info, stack_resident, getBias)


       val _ = msg "\tsummarizing storage information\n"

       val summarize = summarize_time Trackstorage.summarize 
       val summary as (Trackstorage.SUMMARY{fixStackOffset,
					    tailcallImpossible,
					    stackframe_size,
					    registers_used,
					    callee_save_slots,...}) = 
	                summarize storage_info

        val new_procsig =
	       let 
		 val framesize  = SOME stackframe_size
		 val ra_offset = SOME(sloc2int (fixStackOffset RETADD_POS))
		 val callee_save_regs = (map #1 callee_save_slots)
	         val regs_destroyed  = ref (Regset.listItems (Regset.difference(listToSet registers_used,
										listToSet callee_save_regs)))
	         val regs_modified  = ref (Regset.listItems (Regset.union(listToSet registers_used,
									 listToSet (!regs_modified))))
	       in
		  PROCSIG {arg_ra_pos   = orig_args,
			   res_ra_pos   = res_ra_pos,
			   allocated    = true,
			   regs_destroyed = regs_destroyed,
			   regs_modified = regs_modified,
			   blocklabels  = block_labels,
			   framesize    = framesize,
			   ra_offset = ra_offset,
			   callee_saved = callee_save_regs,
			   args         = args,
			   res          = res}
	       end

        val _ = msg "\trewriting module\n"

	 (* rewrite module, saving information about variables live
	    at every call site. *)

	 val callsite_info = 
		map (fn bblock => 
		        allocateBlock (mapping,
				       stackframe_size,
				       fixStackOffset,
				       tailcallImpossible,
				       name) callee_save_slots bblock)
		(Labelmap.listItems block_map)

	 val callsite_info = foldr (op @) nil callsite_info 

	   (* modify the preamble and postamble code *)
	 val _ = createPostamble(block_map,hd(rev block_labels),arg_ra_pos,summary)
         val _ = createPreamble (block_map,name,arg_ra_pos,summary)

	 val callinfo = getCallInfo name summary mapping tracemap callsite_info

	 val _ = msg "\tleaving allocateproc2\n"
     in	(new_procsig, 
	 block_map, 
	 block_labels,
	 Tracetable.MakeTable callinfo)
     end (* allocateProc2 *)

(*   val allocateProc = allocateProcOrig  *)

  fun allocateProc arg = allocateProc2 (allocateProc1 arg) 

end; (* Chaitin *)
