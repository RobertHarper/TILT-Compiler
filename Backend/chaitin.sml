(*$import Prelude TopLevel Core Int Rtl PRINTUTILS BBLOCK CALLCONV TRACKSTORAGE IFGRAPH COLOR TRACETABLE MACHINEUTILS INTRAPROC Stats TextIO Util Listops List UtilError *)
(* Graph-coloring register allocator.
       Notes on constructing the interference graph for procedures.
       
       Since we use a standard calling convention, we must associate
       the formals of the procedure and actuals dictated by the calling convention
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
		structure Color : COLOR
		structure Printutils : PRINTUTILS
		structure Machineutils : MACHINEUTILS
		structure Callconv : CALLCONV

		sharing Printutils.Bblock.Machine 
		      = Printutils.Machine 
		      = Callconv.Machine 

		sharing Printutils.Tracetable = Printutils.Bblock.Tracetable

		  ) :> PROCALLOC where Bblock = Printutils.Bblock 
                                 where Tracetable = Printutils.Tracetable =
struct
   val makeTable = Stats.tt "MakeTable"
   open Rtl
   open Callconv Color Printutils Machineutils
   open Tracetable 
   open Core
   open Machine
   open Bblock

   structure Bblock = Bblock
   structure Machine = Machine
   structure Machineutils = Machineutils
   structure Tracetable = Tracetable

   val debug = Stats.ff("ChaitinDebug")
   val msgs = Stats.ff("ChaitinMsgs")
   val delete_moves = ref true

   fun msg (x : string) = if !msgs then print x else ()
       
   fun print_reglist regs = app (fn r => (print(msReg r); print "  ")) regs

   val doTimer = Stats.ff("DoChaitinTimer")
   fun subtimer (str,f) = if !doTimer
			      then Stats.subtimer(str,f)
			  else f


(* ------------------------------------------------------------ *)

   val error = fn s => Util.error "chaitin.sml" s

   (* this was nonstandard -- why ? *)

   val zip = Listops.zip
   fun sieve_regs (IN_REG r :: t) = r :: sieve_regs t
     | sieve_regs (_ :: t) = sieve_regs t
     | sieve_regs nil = nil

   fun assigns2regs [] = []
     | assigns2regs ((IN_REG r) :: rest) = r :: (assigns2regs rest)
     | assigns2regs (_ :: rest) = assigns2regs rest

   fun mv (src : assign,dest : assign) : instruction list =
     case (src,dest) of
	 (IN_REG r, IN_REG r') => [BASE(MOVE (r,r'))]
       | (IN_REG r, ON_STACK s) => [BASE(PUSH(r,s))]
       | (ON_STACK s, IN_REG r') => [BASE(POP(r',s))]
       | (ON_STACK s, ON_STACK s') => (error "Warning! mv: mem-to-mem transfer")
       | _ => error "m: not a valid argument source or dest"

   fun mv2reg (loc : assign,dest : register) : instruction =
     case loc of
	 IN_REG r => BASE(MOVE (r,dest))
       | ON_STACK s => BASE(POP(dest,s))
       | _ => error "mv2reg: not a valid argument source"

   (* given two lists of sources and destination, make
      alist of moves *)

   fun mvlist (srcs,dests) =
         List.concat (map mv (zip srcs dests))

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
     
       fun getLinkageDestroyedModified (UNKNOWN_PROCSIG{linkage,regs_destroyed,regs_modified,...}) = 
	   (linkage, regs_destroyed, regs_modified)
	 | getLinkageDestroyedModified (KNOWN_PROCSIG{linkage,regs_destroyed,regs_modified,...}) = 
	   (linkage, regs_destroyed, regs_modified)
       fun getCallInstrRegs (getSignature : label -> procsig) call =
	(case call of
	  (CALL{func = DIRECT (label as (LOCAL_CODE _),_), ...}) =>
	    let val (LINKAGE{argCaller = arg_pos,
			     resCaller = res_pos,
			     ...},
		     regs_destroyed,
		     regs_modified) = getLinkageDestroyedModified (getSignature label)
	    in  {regs_destroyed = regs_destroyed, 
		 regs_modified = regs_modified, 
		 arg_pos = arg_pos, 
		 res_pos = res_pos, 
		 C_call = false}
	    end
	| (CALL{func = INDIRECT _, args : register list, results, ...})=>
	     let 
		 val LINKAGE{argCaller=arg_pos, resCaller=res_pos,...} =
		     unknown_ml (FORMALS{args=args,results=results})
	     in {regs_destroyed = indirect_caller_saved_regs,
		 regs_modified = Regset.union(indirect_callee_saved_regs,
					      indirect_caller_saved_regs),
		 arg_pos = arg_pos,
		 res_pos = res_pos,
		 C_call = false}
	     end

       | (CALL{calltype = ML_NORMAL,
	       func = DIRECT (_, _), args, results,  
	       argregs, resregs, destroys, ...}) =>
	     let val LINKAGE{argCaller=arg_pos_default, resCaller=res_pos_default,...} = 
		     unknown_ml (FORMALS{args=args,results=results})
	     in {regs_destroyed = (case destroys of
				       NONE => indirect_caller_saved_regs
				     | SOME regs => listToSet regs),
		 regs_modified = (case destroys of
				      NONE => indirect_caller_saved_regs
				    | SOME regs => listToSet regs),
		 arg_pos = (case argregs of
				NONE => arg_pos_default
			      | SOME regs => map IN_REG regs),
		 res_pos = (case resregs of
				NONE => res_pos_default
			      | SOME regs => map IN_REG regs),
		 C_call = false}
	     end

	   | (CALL{calltype = C_NORMAL,
		   func = DIRECT (_, _), args, results,  
				 argregs, resregs, destroys, ...}) =>
	       let val LINKAGE{argCaller=arg_pos_default, resCaller=res_pos_default, ...} = 
			std_c (FORMALS{args=args,results=results})
	       in {regs_destroyed = (case destroys of
					  NONE => C_caller_saved_regs
					| SOME regs => listToSet regs),
		  regs_modified = (case destroys of
				     NONE => C_caller_saved_regs
				   | SOME regs => listToSet regs),
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

   fun pre_color (name : label, actual_args, actual_results, blocklabels, args, res, block_map) = 
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
		    BASE(RTL (RETURN {results=assigns2regs actual_results}))]
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
					 NO_ANN(BASE(BR postlude_label)))
					  
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

     val pre_color = subtimer("chaitin_preColor", pre_color)

     val (hasRpv,Rpv_virt) = (case Rpv of 
				NONE  => (false, Rat)
			      | (SOME x) => (true,x))


   (* replace_calls: rewrite calls so that moves to/from physical
      registers are explicitly in the code.   Also calculate
      the set of general ML registers that may be destroyed by
      calls.   These need to be marked as used, so that the correct
      code for callee-save may be generated.*)

     fun replace_calls (getSignature : label -> procsig, blockmap) =
       let val max_on_stack = ref 0
	   val max_C_args = ref 0
	   val regs_destroyed = ref Regset.empty
	   val regs_modified = ref Regset.empty
	   fun check_arg (ON_STACK (THIS_FRAME_ARG4 i)) =
	              max_on_stack := Int.max (!max_on_stack,i+1)
	     | check_arg (ON_STACK (THIS_FRAME_ARG8 i)) =
	              max_on_stack := Int.max (!max_on_stack,i+2)
	     | check_arg _ = ()
	   fun expand_call (call as CALL{calltype,args,func,results,
					  destroys,...}) : instruction list =
	           let val {arg_pos,res_pos,
			    C_call,regs_destroyed=destroyed_by_call,
			    regs_modified=modified_by_call,
			    ...} = getCallInstrRegs getSignature call
		       val arg_regs = sieve_regs arg_pos
		       val result_regs = sieve_regs res_pos
		      (* update maximum num of args passed on stack *)
		       val _ = app check_arg arg_pos
		       val _ = (case calltype of
				    C_NORMAL =>
					max_C_args := Int.max(!max_C_args, length args)
				  | _ => ())
		      (* make sure to load address of called routine into
		         Rpv if Rpv exists.   The caller may want to reset
			 certain globals with it(like gp on the Alpha). *)
		       val code : instruction list =
			   mvlist(map IN_REG args,arg_pos) @
			   (case func of
			      DIRECT (LOCAL_CODE label, _) => 
 (* CS: I don't think this is necessary (on the alpha), as the translation of
    the call (BSR (i.e., alpha jsr) to a label) puts the address in $pv
    automatically *)
				  if hasRpv then [BASE(LADDR(Rpv_virt,LOCAL_CODE label))] 
				  else []
(* new code *)              | DIRECT (ML_EXTERN_LABEL label, _) => 
				  if hasRpv then [BASE(LADDR(Rpv_virt,ML_EXTERN_LABEL label))] 
				  else []
                            | DIRECT (C_EXTERN_LABEL label, _) =>
				  if hasRpv then [BASE(LADDR(Rpv_virt,C_EXTERN_LABEL label))] 
				  else []
			    | INDIRECT reg => 
				  if hasRpv then [BASE(MOVE(reg,Rpv_virt))] else [])
			      @ (BASE(RTL(CALL{calltype=calltype,
					       func=func,
					       args=arg_regs,
					       results=result_regs,
					       argregs=SOME arg_regs,
					       resregs=SOME result_regs,
					       destroys=destroys})) ::
				 mvlist(res_pos,map IN_REG results))
				 

		        (* update the set of registers destroyed by calls.
			   This includes (1) actual argument, result, return
			   registers and (2) registers in the destroy set.*)

		       val _ =
			   let 
			       val actuals = (arg_regs @ result_regs)
			       val actuals_set = listToSet actuals
			   in 
			       regs_destroyed := Regset.union(!regs_destroyed, actuals_set);
			       regs_destroyed := Regset.union(!regs_destroyed, destroyed_by_call);
			       regs_modified  := Regset.union(!regs_modified , actuals_set);
			       regs_modified  := Regset.union(!regs_modified , modified_by_call)
			   end
		   in code	                  
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

     val replace_calls = fn arg => subtimer("chaitin_replaceCalls", replace_calls) arg

   (* print an interference graph *)

   fun print_graph g = 
	 let
	   val nodes = Ifgraph.nodes g
	   fun print_node n = 
	     (print_reg n;
	      emitString ":  ";
	      print_list print_reg (Regset.listItems (Ifgraph.edges g n)))
	 in
	   Regset.app print_node nodes;
	   TextIO.flushOut TextIO.stdOut
	 end


   (* Build interference graph.    Physical registers appear in the graph.*)

       fun buildGraph (getSignature,name,block_map, 
		       args,res,callee_saved) : Ifgraph.graph =
	 let val igraph = Ifgraph.empty ()
	     val insert_node = Ifgraph.insert_node igraph
	     val insert_edge = Ifgraph.insert_edge igraph
	     val insert_edges = Ifgraph.insert_edges igraph

	   fun processInstr instr =
	     let
	       val (def_regs,_) = Bblock.defUse (stripAnnot instr)
	       val live_regs = Bblock.live instr
             in 

		(* don't add nodes for pseudo-regs def'd 
		   if there are no registers live after the
		   instruction.   Those regs are dead, and
	           wil be assigned to Rzero/Fzero 
		   add conflicts between pseudo-regs live after
		   instruction and pseudo-regs defined by 
		   instruction *)

		 app insert_node def_regs;
		 app (fn def => (insert_edges ([def],live_regs))) def_regs;

		case (stripAnnot instr) of
		 (BASE(RTL(call as (CALL{func, args, results, ...})))) =>
		     let
			 val {regs_destroyed, ...} = getCallInstrRegs getSignature call
(*
			 val regs_destroyed' = Regset.filter isPhysical regs_destroyed
			 val _ = (print "regs_destroyed = "; print (Int.toString (length regs_destroyed));
				  print "regs_destroyed' = "; print (Int.toString (length regs_destroyed'));
				  print "live_regs = "; print (Int.toString (Regset.numItems live_regs));
				  print "\n")
*)
		         (* add conflicts between registers destroyed
			  by call and variables live after call.*)
		     in	 insert_edges (Regset.listItems regs_destroyed,live_regs)
		     end
                | _ => ()
	     end
	   
	   fun processBlock (BLOCK{in_live, instrs, ...},n) =
	       let val _ = msg ("processBlock #" ^ (Int.toString n) ^ ": " ^
				(Int.toString (length (!instrs))) ^ "instrs \n")
		   val instructions = rev (!instrs)
		   val _ = (case instructions of
				firstInstr::_ => Regset.app insert_node (Bblock.live firstInstr)
		              | _ => ())
		   val _ = app processInstr instructions
	       in  n+1
	       end

	 in
           (* return pseudo-reg cannot be placed in callee-saved registers,
	      since the last thing we do before returning is restore the
	      callee-saved registers.*)
 
	   (* if the functions arguments/results are not pre-colored,
	      but are to be allocated.    they can't be placed in callee-saved
	      registers either. This restriction isn't implemented right
	      now.*)
	     msg("There are " ^ (Int.toString (Labelmap.numItems block_map)) 
		 ^ " blocks in the blockmap\n");

	   Labelmap.foldl processBlock 0 block_map;
	   if (! debug) then print_graph igraph else ();
	   igraph
	 end

       val buildGraph = fn arg => subtimer("chaitin_buildGraph", buildGraph) arg

   (* generate tables of information for the garbage collector *)

   datatype callsite_info = CALLSITE of {label : label,live : Regset.set}


    (* rewrite a block to use physical registers *)

   fun allocateBlock (mapping,stackframe_size,fixStackOffset,name) callee_save_slots
                     (BLOCK{in_live, use, def, instrs,...}) =
	   let
	     val _ = if (! debug) then emitString "AllocateBlock\n" else ()
             val pop = fn (d,l) => pop(d,fixStackOffset l)
	     val push = fn (s,l,t) => push(s,fixStackOffset l,t)
	       
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
	         (* Prefer to use Rat2 because the translation to
                    assembler may try to use Rat.  We catch this and
                    compensate, but it's less efficient and should be
                    avoided if possible.  *)
		 val itemps = [Rat2, Rat]
		 val ftemps = [Fat2, Fat]
		 val pre_itemps = Listops.list_diff_eq(eqRegs',itemps, src_regs)
		 val pre_ftemps = Listops.list_diff_eq(eqRegs',ftemps, src_regs)
		 val pre_temps = (pre_itemps,pre_ftemps)
		 val push_temp = hd itemps
		 val post_itemps = tl itemps
		 val post_ftemps = ftemps
		 val post_temps = (post_itemps,post_ftemps)
		 val push' = fn (s,l) => push(s,l,push_temp)

		 fun pickTemp r (itemps,ftemps) = 
		     (case (r,itemps,ftemps) of
			  (R _, [], _) => error "pickTemp"
			| (R _, ir::rest, _) => (ir, (rest, ftemps))
			| (F _, _, []) => error "pickTemp"
			| (F _, _, fr::rest) => (fr, (itemps, rest))
		     handle e => (print "pickTemp failed on "; print (msReg r); print " with candidates";
				  print "itemps are "; print_reglist itemps; print "\n";
				  print "ftemps are "; print_reglist ftemps; print "\n";
				  print "src_regs are "; print_reglist src_regs; print "\n";
				  print "pre_itemps are "; print_reglist pre_itemps; print "\n";
				  print "pre_ftemps are "; print_reglist pre_ftemps; print "\n";
				  raise e))
		 fun nextItemp (nil,_) = NONE
		   | nextItemp (r::_, _) = SOME r
		 fun allocAny mover temps [] precode localmap = (List.concat precode, localmap, nextItemp temps)
                   | allocAny mover temps (src :: rest) precode localmap = 
		     (case getreg src of
			IN_REG r => 
			    let val localmap = Regmap.insert(localmap, src, r)
			    in  allocAny mover temps rest precode localmap
			    end
		      | ON_STACK offset =>
			  let
			    val (this_temp, temps) = pickTemp src temps
			    val precode = mover(this_temp,offset) :: precode
			    val localmap = Regmap.insert(localmap, src, this_temp)
			  in
			    allocAny mover temps rest precode localmap
			  end
		      | _ => error "putInRegs: allocAny")

		 val (precode, srcmap, srctmp) = 
		     allocAny pop pre_temps src_regs [] (Regmap.empty)

		 val (postcode, dstmap, _) = 
		     allocAny push' post_temps dst_regs [] (Regmap.empty)
	       in
		 {precode = precode,
		  srcmap = srcmap,
		  srctmp = srctmp,
		  dstmap = dstmap,
		  postcode = postcode}
	       end
	   handle e => (print "Error while calling putInRegs with\n";
			print "src_regs = "; print_reglist src_regs; print "\n";
			print "dst_regs = "; print_reglist dst_regs; print "\n";
			raise e)

	     fun allocateInstr (NO_ANN _) _ = error "allocateInstr: unannotated"
	       | allocateInstr (LIVE (live,instr)) next_label =
	       (case instr of
		  BASE(ILABEL _) => [instr]
		| BASE(RTL (JMP(Raddr,rtllabs))) => 
		      let val fixup_code = List.concat (map (fn (reg,sloc) => pop(reg,sloc)) callee_save_slots)
			  val (def,use) = defUse instr
			  val {precode, srcmap, srctmp, dstmap, postcode} = putInRegs use def
			  val jump_reg = (if isPhysical Raddr then Raddr else 
					      (case Regmap.find(srcmap,Raddr) of
						   SOME r => r
						 | NONE => error "can't translate JMP"))
			  val mov_instr = BASE(MOVE(jump_reg, Rpv_virt))
			  val jump_instr = BASE(JSR(false,jump_reg,1,rtllabs))
		      in  mov_instr :: fixup_code @ [jump_instr]
		      end
		  | BASE(RTL (CALL{func, args, results, calltype, ...})) =>
		    (let val return_label = (case next_label of
					       NONE => freshCodeLabel()
					     | SOME l => l)
			 (* Store live variables for GC *)
		      val ra_sloc = fixStackOffset RETADD_POS

		         (* the results are not live yet during the call *)
(*
		      val _ = (print "CHAITIN CALL: live(before) = ";
			       Regset.app (fn r => (print (msReg r); print "  ")) live;
			       print "\n")
		      val _ = (print "        CALL: results = ";
			       app (fn r => (print (msReg r); print "  ")) results;
			       print "\n")
*)
		      val live = Regset.difference(live,listToSet results)
(*
		      val _ = (print "CHAITIN CALL: live(after) = ";
			       Regset.app (fn r => (print (msReg r); print "  ")) live;
			       print "\n")
*)
			    
		      fun stack_fixup_code1 () = 
			(List.concat (map (fn (reg,sloc) => pop(reg,sloc)) callee_save_slots))
		      val stack_fixup_code2 = deallocate_stack_frame stackframe_size
			
		      val no_moddef_info = { regs_modified = [] : register list,
					     regs_destroyed = [] : register list,
					     args = [] : register list}

		      val _ = 
			  (case calltype of
			       ML_TAIL _ => 
				   (if (length args > length Machineutils.indirect_int_args)
				       then error "too many args in tailcall: checked? in toalpha/tosparc"
				    else ())
					
			     | _ => add_info {label=return_label,live=live})
				  
		      val br_instrs : instruction list = 
			case (calltype, func) of

			  (C_NORMAL, DIRECT (label as (C_EXTERN_LABEL _), sraOpt)) =>
			      [BASE(BSR (C_EXTERN_LABEL "save_regs_MLtoC", NONE, no_moddef_info)),
			       BASE(BSR (label, sraOpt, no_moddef_info)),
			       BASE(ILABEL return_label)] @
			      (std_return_code sraOpt) @
			      [BASE(BSR (C_EXTERN_LABEL "load_regs_MLtoC", NONE, no_moddef_info))] @
			      (std_return_code NONE)

			| (C_NORMAL, DIRECT (l, _)) => 
			      (print "C_NORMAL call non-C_EXTERN_LABEL"; print (msLabel l); print "\n";
			       error "C_NORMAL call non-C_EXTERN_LABEL")
			| (C_NORMAL, _) => error "C_NORMAL call but not DIRECT"

			| (ML_NORMAL, DIRECT (label, sraOpt)) =>
			      ([BASE(BSR (label, sraOpt, no_moddef_info)),
				BASE(ILABEL return_label)] @
			       (std_return_code sraOpt))

			| (ML_NORMAL, INDIRECT r) =>
			      let val (def,use) = defUse instr
				  val {precode, srcmap, srctmp, dstmap, postcode} = putInRegs use def
				  val reg = if isPhysical r then r else 
				  (case Regmap.find(srcmap,r) of
				       SOME r => r
				     | NONE => error "fs failed")
			      in  precode @
				  ([BASE (JSR(true, reg, 1, [])),
				    BASE(ILABEL return_label)] @
				   (std_return_code NONE))
			      end
			| (ML_TAIL _, DIRECT (label,_)) =>
				(stack_fixup_code1 ()) @ 
				[BASE(POP_RET(SOME(ra_sloc)))] @
				stack_fixup_code2 @
				[BASE(BR label)]
				  
			| (ML_TAIL _, INDIRECT r) => 
			      let val (def,use) = defUse instr
				  val {precode, srcmap, srctmp, dstmap, postcode} = putInRegs use def
				  val reg = if isPhysical r then r else 
				  (case Regmap.find(srcmap,r) of
				       SOME r => r
				     | NONE => error "fs failed")
			      in  precode @
				  (stack_fixup_code1 ()) @ 
				  [BASE(POP_RET(SOME(ra_sloc)))] @
				  stack_fixup_code2 @
				  [BASE (JSR (false, reg, 1, []))]
			      end
		     in br_instrs
		     end) (* allocateCall *)
		  
		  | BASE(RTL(RETURN{results})) =>
		       let val callee_restore_code =
			   List.concat (map (fn (reg,sloc) => pop(reg,sloc)) callee_save_slots)
			   val res : instruction list = callee_restore_code @ 
			       (deallocate_stack_frame stackframe_size) @
			       [BASE(RET(false, 1))]
		       in  res
		       end
		  | BASE(RTL HANDLER_ENTRY) => []
		  | BASE(RTL (SAVE_CS _)) => []
		  | BASE(GC_CALLSITE llabel) => (add_info{label=llabel,live=live}; [])
		  | BASE(MOVE (src,dest)) =>
		       let val dest' = getreg_posdead dest
			   val src' = getreg src
		       in
			   case (src',dest') of
			       (IN_REG r,IN_REG r') => 
				   if !delete_moves andalso eqRegs r r' 
				       then []
				   else [BASE(MOVE(r,r'))]
			     | (ON_STACK l,IN_REG r') => pop(r',l)
			     | (IN_REG r,ON_STACK l') => push(r,l',Rat)
			     | (ON_STACK l,ON_STACK l') =>
				       pop(Rat,l) @ push(Rat,l',Rat2)
			     | _ => error "allocateInstr: MOVE"
		       end
                 | _ =>
			let val (def,use) = defUse instr
			    val {precode, srcmap, srctmp, dstmap, postcode} = putInRegs use def
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
					  [BASE(JSR(link, fs Raddr,hint, labels))]
				| BASE(RET args) => [BASE(RET args)]
				| BASE(PUSH(src,al)) =>
					  let val tmp = (case srctmp
							   of NONE => error "no free temporary for PUSH"
							    | SOME t => t)
					  in  push(fs src,al,tmp)
					  end
				| BASE(POP(dst,al)) => pop(fd dst,al)
				| i => [translate_to_real_reg(i,fs,fd)]

			in List.concat [precode, instr', postcode]
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
		   handle DEAD => 
		            let val str = msInstruction ("", (stripAnnot instr))
			    in  (emitInstr ("", (BASE (ICOMMENT ("dead instr" ^ str))));
				 (instructionLoop rest))
			    end
			| GETREGBUG => (print "GETREGBUG in ";
					print (msLabel name);
					print ": ";
					print (msInstruction ("", stripAnnot instr));
					raise GETREGBUG)
			| e => (error "UNK ERROR while processing: ";
				print (msInstruction ("", stripAnnot instr));
				raise e))
		   
	     val instrs_in = rev (! instrs)

	     val _ = if (! debug) then
	       (emitString "block in:\n";
		app (fn i => emitInstr ("", stripAnnot i)) instrs_in) else ()

	     val instrs_out = 
	       instructionLoop instrs_in
	       handle e =>
		   (case e
		      of UtilError.BUG _ => raise e
		       | _ => if !debug then raise e
			      else (print "exception in allocateBlock ignored\n";
				    print "No vblock code will be generated.\n";
				    []))

	     val _ = if (! debug) then
	       (emitString "block out:\n";
		app (fn i => emitInstr ("", i)) instrs_out) else ()

	   in
	     instrs := map NO_ANN (rev instrs_out);
	     !callsite_info
	   end (* allocateBlock *)

   val allocateBlock = subtimer("chaitin_rewrite", allocateBlock)

       (* arguments: 
	       . name of site
	       . summary of storage information
	       . mapping from pseudo-regs to locations
	       . mapping from pseudo-regs to traceability info
	       . list of call sites
        *)

       fun getCallInfo name summary mapping tracemap 
				(l : callsite_info list)=
	let 
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
			   case x of
			       TRACE_STACK loc => TRACE_STACK (fixStackOffset loc)
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
				       (SOME loc,SOME (_,trace)) => 
					 let  
					 (* val _ = (print  (msReg h); print " ==> "; 
						      print (msAssign loc); print "\n") *)
					     val trace = stack_trace_location trace
					 in  (loc,trace) :: loop t
					 end
				     | (SOME _,NONE) => fail "missing trace info"
				     | (NONE,SOME _) => fail "missing phys loc"
				     | _ => fail "missing both"
				end
			  in loop live
			  end

		       fun split ((loc,trace) :: t,regs,stack) =
			    (case loc of
				IN_REG x =>    split(t,(x,trace)::regs,stack)
			      | ON_STACK loc => split(t,regs,(get_stackloc loc,
							      trace)::stack)
			      | _ => error "getCallInfo: split")
			 | split (nil, regs, stack) = (regs, stack)

		       val (regtrace,stacktrace) = 
			              split (physical_locations,nil,nil)

		       val regtrace = regtrace @ callee_save_regs_info

(*
		       val _ = (print "chaitin.sml processing label = ";
				print (msLabel label); print "\n";
				print "regtrace:\n";
				app (fn (v,t) => (print (msReg v); print " => ";
						  print (msTrace t); print "\n")) regtrace;
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
	        in case stripAnnot i of
		     BASE(MOVE (src,dest)) => domv(src,dest)
		   | _ => ()
	        end
	      fun processBlock (BLOCK{instrs,...}) =
		      app processInstr (!instrs)
         in biases := Regmap.empty;
	    Labelmap.app processBlock block_map
	 end


       fun createPostamble (block_map,name,arg_pos,
			     Trackstorage.SUMMARY{callee_save_slots, fixStackOffset,
						  stackframe_size,...}) =
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
	   end
		

   fun createPreamble (block_map,name,arg_pos,
			     Trackstorage.SUMMARY{callee_save_slots,
						  stackframe_size,prevframe_maxoffset,
						  fixStackOffset,...}) =
       let 
	   val BLOCK {instrs,...} = (case Labelmap.find(block_map,name) of
					 SOME bl => bl | NONE => error "missing block")
	   val reversed_i = ((std_entry_code()) @
			     (allocate_stack_frame (stackframe_size, prevframe_maxoffset)) @
			     [BASE(PUSH_RET(SOME(fixStackOffset RETADD_POS)))] @
			     (List.concat (map (fn (r, i) => push(r, i, Rat)) callee_save_slots)))
	   val ordered_i = rev (map NO_ANN reversed_i)
       in instrs := !instrs @ ordered_i
       end
		
   fun allocateProc ({getSignature : label -> procsig,
		       name         : label,
		       block_map    : bblock Labelmap.map,
		       procsig = 
		         procsig as 
			   KNOWN_PROCSIG{linkage = linkage as LINKAGE{argCallee=arg_ra_pos,
								      resCallee=res_ra_pos,
								      ...},
					 regs_destroyed,
					 regs_modified,
					 framesize,
					 ra_offset,
					 callee_saved,
					 blocklabels,
					 argFormal = args, 
					 resFormal = res},
		       stack_resident : stacklocation Regmap.map,
		       tracemap     : (register option * Tracetable.trace) Regmap.map}) = 

     let
       val _ = msg "\tentered allocateproc1\n"

       val _ = if !debug then
	           (emitString commentHeader;
		    emitString " before precoloring procedure\n";
	            dumpProc(name,procsig,
			     block_map,blocklabels, true))
            else ()

       val (block_labels,block_map) = 
	    pre_color(name,arg_ra_pos,res_ra_pos,blocklabels,
		      args,res,block_map)

      val _ = if !debug then
	           (emitString commentHeader;
		    emitString " result of precoloring procedure\n";
	            dumpProc(name,procsig,
			     block_map,block_labels, true))
            else ()

      val (max_passed_args, max_C_args, regs_destroyed, regs_modified) =
	       (msg "\treplacing calls\n";
		replace_calls (getSignature, block_map))

       val _ = if !debug then
	           (emitString commentHeader;
		    emitString " dumping procedure after expanding calls\n";
		    dumpProc (name,procsig, block_map, block_labels, !debug);
		    emitString commentHeader;
		    emitString " done expanding\n")
	       else ()
       val _ = msg  "\tannotating\n"
	   
       val block_map = subtimer("chaitin_annotate", Bblock.liveVars tracemap block_map) name
	   
       val _ = 
	   if (! debug) then 
	       (emitString commentHeader;
		emitString " dumping procedure after annotation\n";
		dumpProc (name,procsig, block_map, block_labels, !debug);
		emitString commentHeader;
		emitString " done annotation\n")
	   else ()

       val _ = msg("processing procedure "^msLabel name^"\n")

       (* if there is a HANDLER_ENTRY in here, add modified to destroyed for saving purposes *)
       local
	   val has_exn_flag = ref false
	   fun has_exn [] = false
	     | has_exn ((BASE(RTL HANDLER_ENTRY))::_) = true
	     | has_exn (_::rest) = has_exn rest
	   val instr_blocks = Labelmap.app (fn (BLOCK{instrs,...}) => 
					    if (has_exn (map stripAnnot (!instrs)) )
						then (has_exn_flag := true) else ())
	       block_map
       in  val regs_modified = listToSet regs_modified
	   val regs_destroyed = listToSet regs_destroyed
	   val regs_destroyed = if (!has_exn_flag)
				    then Regset.union(regs_destroyed, regs_modified)
				else regs_destroyed
       end

       (* initialize information on storage *)
       val storage_info = 
	   subtimer("chaitin_newinfo", 
	 Trackstorage.newInfo)
	                      {callee_saves = Regset.listItems callee_saved,
			       stack_resident = stack_resident,
			       max_on_stack = max_passed_args,
			       max_C_args = max_C_args,
			       regs_destroyed = Regset.listItems regs_destroyed}
       val stackOffset = Trackstorage.stackOffset storage_info
       val _ =  subtimer("chaitin_initbias",  initBias)  block_map
       val _ = msg "\tbuilding interference graph\n"

       val igraph = buildGraph (getSignature,name,block_map,
				args,res,callee_saved)
       val _ = if (!msgs) then Ifgraph.print_stats igraph else ()

       val _ = msg "\tcoloring interference graph\n"
       val mapping = subtimer("chaitin_color",color)(igraph,storage_info, stack_resident, getBias)


       val _ = msg "\tsummarizing storage information\n"

       val summarize = subtimer("chaitin_summarize", Trackstorage.summarize)
       val summary as (Trackstorage.SUMMARY{fixStackOffset,
					    stackframe_size,
					    registers_used,
					    callee_save_slots,...}) = 
	   subtimer("chaitin_summarize", summarize )  storage_info

        val new_procsig =
	       let 
		 val framesize  = stackframe_size
		 val ra_offset = sloc2int (fixStackOffset RETADD_POS)
		 val callee_save_regs = listToSet (map #1 callee_save_slots)
		 val registers_used_set = listToSet registers_used
	         val regs_destroyed  = Regset.difference(registers_used_set, callee_save_regs)
	         val regs_modified  = Regset.union(regs_modified, registers_used_set)
	       in
		   KNOWN_PROCSIG {linkage = linkage,
				  regs_destroyed = regs_destroyed,
				  regs_modified = regs_modified,
				  blocklabels  = block_labels,
				  framesize    = framesize,
				  ra_offset = ra_offset,
				  callee_saved = callee_save_regs,
				  argFormal    = args,
				  resFormal    = res}
	       end

        val _ = msg "\trewriting module\n"

	 (* rewrite module, saving information about variables live
	    at every call site. *)

	 val callsite_info = 
		 subtimer("chaitin_rewrite", 
			       map (fn bblock => 
		        allocateBlock (mapping,
				       stackframe_size,
				       fixStackOffset,
				       name) callee_save_slots bblock))
		(Labelmap.listItems block_map)

	 val callsite_info = List.concat callsite_info 

	   (* modify the preamble and postamble code *)
	 val _ = createPostamble(block_map,hd(rev block_labels),arg_ra_pos,summary)
         val _ = createPreamble (block_map,name,arg_ra_pos,summary)

	 val _ = msg "\tmaking trace table\n"
	 val callinfo = subtimer("chaitin_getcallinfo",
				       getCallInfo name summary mapping tracemap)  callsite_info

	 fun MakeTable callinfo =
	     if !makeTable then Tracetable.MakeTable callinfo
	     else []

	 val tables = subtimer("chaitin_maketable", MakeTable) callinfo
     in	(new_procsig, 
	 block_map, 
	 block_labels,
	 tables)
     end (* allocateProc *)


end; (* Chaitin *)
