(*$import PRINTUTILS CALLCONV INTRAPROC RECURSION TOASM MACHINEUTILS Stats RTLTOASM Int32 *)
functor Rtltoasm (val commentHeader : string
		  structure Machineutils : MACHINEUTILS
		  structure Callconv : CALLCONV
		  structure Printutils : PRINTUTILS
		  structure Procalloc : PROCALLOC
		  structure Recursion : RECURSION
		  structure Toasm : TOASM

		  sharing Printutils.Machine 
		      = Machineutils.Machine
		      = Callconv.Machine
		      = Procalloc.Machine
		      = Procalloc.Bblock.Machine
		      = Procalloc.Tracetable.Machine
		      = Toasm.Machine
(* should not be needed *) = Printutils.Bblock.Machine = Printutils.Tracetable.Machine


		  sharing Printutils.Bblock 
		    = Procalloc.Bblock
		    = Toasm.Bblock

		  sharing Printutils.Tracetable
		    = Procalloc.Tracetable 
		    = Toasm.Tracetable)

		     :> RTLTOASM  =
struct


   open Callconv Printutils Machineutils Rtl
   open Machine

   fun flatten arg = foldr (op @) [] arg
   val debug       = ref false
   val knowns      = ref false
   val msgs        = ref false

   fun msg (x: string) = if !msgs then print x else ()
   val error = fn s => Util.error "rtltoasm.sml" s


   fun filter p [] = []
     | filter p (x::xs) = if (p x) then x :: (filter p xs) else filter p xs

   fun xlocal_label (Rtl.LOCAL_DATA v) = LOCAL_DATA v
     | xlocal_label (Rtl.LOCAL_CODE v) = LOCAL_CODE v

   fun xlabel (Rtl.ML_EXTERN_LABEL s) = MLE s
     | xlabel (Rtl.C_EXTERN_LABEL s) = CE(s,NONE)
     | xlabel (Rtl.LOCAL_LABEL ll) = I(xlocal_label ll)

   fun xalign Rtl.LONG = LONG
     | xalign Rtl.ODDLONG = ODDLONG
     | xalign Rtl.QUAD = QUAD
     | xalign Rtl.OCTA = OCTA
     | xalign Rtl.ODDOCTA = ODDOCTA

   fun xdata (Rtl.COMMENT arg) = COMMENT arg
     | xdata (Rtl.STRING arg) = STRING arg
     | xdata (Rtl.INT32 arg) = INT32 arg
     | xdata (Rtl.INT_FLOATSIZE arg) = INT_FLOATSIZE arg
     | xdata (Rtl.FLOAT arg) = FLOAT arg
     | xdata (Rtl.DATA arg) = DATA (xlabel arg)
     | xdata (Rtl.ARRAYI arg) = ARRAYI arg
     | xdata (Rtl.ARRAYF arg) = ARRAYF arg
     | xdata (Rtl.ARRAYP (i,Rtl.PTR arg)) = ARRAYP(i,PTR (xlabel arg))
     | xdata (Rtl.ARRAYP (i,Rtl.TAG arg)) = ARRAYP(i,TAG arg)
     | xdata (Rtl.ALIGN arg) = ALIGN (xalign arg)
     | xdata (Rtl.DLABEL arg) = DLABEL (xlabel arg)

(* ----------------------------------------------------------------- *)

   fun allocateModule (prog as Rtl.MODULE{procs, data, main, 
					  mutable_objects,
					  mutable_variables}) =
     let
      
       local 
	   val table = map (fn PROC{name,external_name,...} => (xlocal_label name,Util.mapopt xlabel external_name)) procs
       in  fun name2external name = 
	   (case (Listops.assoc_eq(fn(x,y) => eqLLabs x y,name,table)) of
		NONE => error "bad name"
	      | SOME res => res)
       end
       val {callee_map, rtl_scc, ...} = Recursion.procGroups prog
       local
	   val recursive_components =
	       map (map Toasm.translateLocalLabel) rtl_scc
	       
	   val _ = if (!debug)
		       then (print "***** There are ";
			     print (Int.toString (length procs));
			     print " procedures  and  ";
			     print (Int.toString (length recursive_components));
			     print " recursive components with the largest being ";
			     print (Int.toString (foldr Int.max 0
						  (map length recursive_components)));
			     print "\n")
		   else ()
	   fun memberLabel [] _ = false
	     | memberLabel (l::rest) l' =
	       (eqLLabs l l') orelse (memberLabel rest l')

	   fun getComponent groups proc =
	       let
		   fun loop [] = error "getComponent: procedure not found"
		     | loop (lst :: lsts) =
	       if (memberLabel lst proc) then
		   lst
	       else
		   loop lsts
	       in
		   loop groups
	       end
	   
	   fun sameComponent groups proc1 proc2 = 
	       memberLabel (getComponent groups proc1) proc2
       in
	   val is_mutual_recursive = sameComponent recursive_components
	   val names = map (fn (Rtl.PROC{name,external_name,...}) => 
			    (Toasm.translateLocalLabel name,external_name)) procs
	   val component_names = recursive_components
       end

       val Labelmap = ref (Labelmap.empty) : 
	 procsig Labelmap.map ref

       fun getSig proc_name = (case (Labelmap.find (!Labelmap, proc_name)) of
				   SOME s => s
				 | _ => error "getSig")

       fun existsSig proc_name = 
	   case Labelmap.find (!Labelmap,proc_name) of
	       SOME _ => true
	     | _ => false

       fun setSig proc_name proc_sig = 
	 Labelmap := Labelmap.insert(!Labelmap, proc_name, proc_sig)

       val indirect_regs_destroyed = indirect_caller_saved_regs

       fun makeUnknownSig (args,results) = 
	   let val ACTUALS{args=actual_args,
			   results=actual_results} =
	     Callconv.unknown_ml false (FORMALS{args=args,
						results=results})
           in PROCSIG
	       {arg_ra_pos = SOME actual_args,
		res_ra_pos = SOME actual_results,
		allocated = false,
		blocklabels = [],
		regs_destroyed = ref indirect_regs_destroyed,
		regs_modified = ref general_regs,
		args = args,
		res = results,
		framesize = NONE,
		ra_offset = NONE,
		callee_saved = indirect_callee_saved_regs}
            end
	   
       fun initSigs [] = ()
         | initSigs (proc::rest)=
	   let 
	     val (Rtl.PROC{name, args, results, return, known, ...}) = proc
	     val name = Toasm.translateLocalLabel name
	     val args = (map Toasm.translateIReg (#1 args)) @
	                (map Toasm.translateFReg (#2 args))
	     val results  = (map Toasm.translateIReg (#1 results)) @
	                (map Toasm.translateFReg (#2 results))
             val return = Toasm.translateIReg return

	     val sign = if known andalso (! knowns) andalso (length args <= 15) then
		         PROCSIG 
			      {arg_ra_pos = NONE,
			       res_ra_pos = NONE,
			       allocated = false,
			       regs_destroyed = ref [],
			       regs_modified = ref [],
			       blocklabels = [],
			       args = args,
			       res = results,
			       framesize = NONE,
			       ra_offset = NONE,
			       callee_saved = []}
		       else makeUnknownSig(args,results)
           in if existsSig name then
	           error ("function names not unique: "; 
			  msLoclabel name^" occurs twice")
	      else setSig name sign;
	     initSigs rest
	   end

       exception Lookup
       fun lookup [] _ = raise Lookup
	 | lookup ((x,y)::ls) x' = if (x = x') then y else lookup ls x'
	   
       fun findRtlProc p [] = error "findRtlProc"
	 | findRtlProc p ((p' as Rtl.PROC{name,...})::rest) =
	 if (Rtl.eq_locallabel (p, name)) then
	   p'
	 else
	   findRtlProc p rest




       fun allocateProc1 (name : loclabel) =
	 let 
	     val _ = if (!debug)
			 then (print "allocateProc 1 entered\n")
		     else ()
	   val (psig as (PROCSIG{allocated, regs_destroyed, ...})) = 
	     getSig name
	 in

	   if allocated then
	       (SOME psig, NONE, [])
	   else
	       let
		 val callees = 
		   map Toasm.translateLocalLabel 
		       (callee_map (Toasm.untranslateLocalLabel name))

		 val recursive_callees = 
		   filter (is_mutual_recursive name) callees

		 val nonrecursive_callees =
		   filter (not o (is_mutual_recursive name)) callees

		 val _ = 
		   if (! debug) then
		     (emitString commentHeader;
		      emitString ("  Allocating " ^ 
				  (msLoclabel name) ^ "\n");
		      emitString commentHeader;
		      emitString "nonrecursive: ";
		      print_list print_lab nonrecursive_callees;
		      emitString commentHeader;
		      emitString "recursive: ";
		      print_list print_lab recursive_callees) else ()

		 val code_labels_listlist = Listops.mapcount allocateComponent (map (fn x => [x]) nonrecursive_callees)

	     val _ = if (!debug)
			 then (print "allocateProc 1 : 1\n")
		     else ()

		 val code_labels = flatten code_labels_listlist
		 val _ = app (fn name =>
			         let val PROCSIG{args,res,
						 allocated,...} = getSig name
				 in if allocated
				    then ()
				    else setSig name(makeUnknownSig(args,res))
				 end) recursive_callees

		 val _ = msg ((msLoclabel name) ^ "\n")


		 val _ = msg "\ttranslating\n"

		 val (known, (blocklabels, block_map, tracemap,stack_resident)) =
		   let
		     val (rtlproc as Rtl.PROC{known,...})= 
		       findRtlProc (Toasm.untranslateLocalLabel name) procs
		   in
		     (known andalso (! knowns), 
		      Stats.subtimer("toasm_translateproc",Toasm.translateProc) rtlproc)
		   end

	     val _ = if (!debug)
			 then (print "allocateProc 1 : 2\n")
		     else ()


		 (* Add blocklabels and recursive-updated regs_destroyed *)
		 val psig = case psig of
		   (PROCSIG{arg_ra_pos, res_ra_pos, framesize, ra_offset,
				      callee_saved, allocated,
				      regs_destroyed, regs_modified,
				      args, res, ...}) =>
		    PROCSIG {arg_ra_pos = arg_ra_pos,
				       res_ra_pos = res_ra_pos,
				       framesize  = framesize,
				       ra_offset = ra_offset,
				       callee_saved=callee_saved,
				       allocated  = false,
				       regs_destroyed  = regs_destroyed,
				       regs_modified = regs_modified,
				       blocklabels = blocklabels,
				       args       = args,
				       res        = res}

		 val _ = msg "\tabout to dump initial version\n"

	         val _ = 
		     if !debug
		     then (emitString commentHeader;
			   emitString (" dumping initial version of procedure");
			   dumpProc(name,name2external name,psig, block_map, blocklabels, true))
		     else ()

		 val _ = msg "\tallocating\n"

	     val _ = if (!debug)
			 then (print "allocateProc 1 : 3\n")
		     else ()

		 val temp = Procalloc.allocateProc1
		     {getSignature = getSig,
		      external_name = name2external name,
		      name      = name,
		      block_map = block_map,
		      tracemap  = tracemap,
		      procsig   = psig,
		      stack_resident = stack_resident}
		     handle e => (print "exception from Procalloc\n";
				  raise e)

		 val _ = if (!debug)
			     then (print "allocateProc 1 exitting\n")
			 else ()
	       in
		   (NONE, SOME (name, temp),code_labels)
	       end
	 end (* allocateProc1 *)
     
       and allocateProc2 (name, res_of_allocateproc1) =
	   let
	       val _ = if (!debug)
			     then (print "allocateProc 2 entered\n")
		       else ()
	       val (new_sig, new_block_map, new_block_labels, gc_data) =
		   Stats.subtimer("chaitin_allocproc2",
				  Procalloc.allocateProc2) res_of_allocateproc1
	       fun doer (l,acc) = 
		 let
		   val (Bblock.BLOCK{instrs,in_live,out_live,succs,def,use,truelabel,...}) =
		       (case (Labelmap.find (new_block_map, l)) of
			    SOME b => b | NONE => error "missing block")
		     val instrs = !instrs
		     fun folder (annote_instr,acc) = 
			 (case (Bblock.stripAnnot annote_instr) of
			    (BASE(LADDR(_,I (LOCAL_CODE l)))) => (I (LOCAL_CODE l)) :: acc
			  | (BASE(LADDR(_,MLE s))) => (MLE s) :: acc
			  | (BASE(LADDR(_,CE x))) => (CE x) :: acc
			  | _ => acc)
		 in foldl folder acc instrs
		 end
	       val rev_code_label_list = foldl doer [] new_block_labels
	       val code_label_list : label list = rev rev_code_label_list
	   in
	       setSig name new_sig;
	       msg "\tdumping\n";
	       if !debug then
		   (emitString commentHeader;
		    emitString(" dumping final version of procedure "))
	       else ();
		dumpProc (name, name2external name,
			  new_sig, new_block_map, 
			  new_block_labels, !debug);
		dumpDatalist gc_data;
		if !debug then
		    (emitString commentHeader;
		     emitString(" done procedure "))
		else ();
	       (if (!debug)
			     then (print "allocateProc 2 exitting\n")
			 else ());
		(new_sig, code_label_list)
	   end
	   
       and allocateComponent (count,chunk) = 
	 let 
	     val _ = if (!debug)
			 then (print "allocating component #"; 
			       print (Int.toString count); print "\n")
		     else ()
	     val temps = map allocateProc1 chunk
	     local
		 fun helper (PROCSIG{regs_modified,...}) = regs_modified
	     in
		 fun get_modsetref (SOME psig,NONE,_) = helper psig
		   | get_modsetref (NONE,SOME(_,({getSignature,external_name,name,block_map,
						  procsig,stack_resident,tracemap},
						  _,_,_,_)),_) = helper procsig
		   | get_modsetref _ = error "allocateproc in allocatecomponent"
	     end
	     fun set_modset target_val arg = (get_modsetref arg) := target_val
	     val all_modified = foldr (op @) []
		 (map (op !) (map get_modsetref temps))

	     val _ = map (set_modset all_modified) temps

	     fun final_alloc arg = 
		 case arg of
		   (SOME psig, NONE, cls) => ((psig,[]),cls)
		 | (NONE, SOME x, cls) => (Stats.subtimer("toasm_allocProc2",allocateProc2) x,cls)
		 | _ => error "allocateproc in allocatecomponent"
	     val code_labels_listlist = map (fn ((_,a),b) => (a @ b)) (map final_alloc temps)
	     val code_labels = flatten code_labels_listlist
	     val _ = if (!debug)
			 then (print "done allocating component #"; 
			       print (Int.toString count); print "\n")
		     else ()
	 in
	   code_labels
	 end


       val main_loc_label = Toasm.translateLocalLabel main
       val main' = Machine.msLoclabel main_loc_label
       val client_entry = main'^"_client_entry"
       val sml_global = main'^"_SML_GLOBALS_BEGIN_VAL"
       val end_sml_global = main'^"_SML_GLOBALS_END_VAL"

       

       val _ = app emitString programHeader;
       val _ = emitString ("\t.globl "^main'^"\n");
       val _ = dumpDatalist (Tracetable.MakeTableHeader main');

       val _ = initSigs procs;

       val _ = app emitString textStart;
       val _ = emitString ("\t.globl "^main'^"_CODE_END_VAL\n");
       val _ = emitString ("\t.globl "^main'^"_CODE_BEGIN_VAL\n");
       val _ = emitString (""^main'^"_CODE_BEGIN_VAL:\n");
       val code_labels_listlist = Listops.mapcount allocateComponent component_names;
       val code_labels = flatten code_labels_listlist
     in (* allocateProg *)
       app emitString textStart;
       emitString (main'^"_CODE_END_VAL:\n");
       dumpDatalist (Tracetable.MakeTableTrailer main');


       app emitString dataStart;
       emitString ("\t.globl "^sml_global^"\n");
       emitString ("\t.globl "^end_sml_global^"\n");
       emitString (sml_global^":\n");

       dumpCodeLabel code_labels;
       dumpDatalist (map xdata data);
       app emitString dataStart;
       emitString ("\n"^end_sml_global^":   ");
       emitString commentHeader;
       emitString " filler next\n";
       emitString ("\t.long 0\n\n");
       dumpDatalist (Tracetable.MakeGlobalTable 
		          (main', 
			   map (fn (l, rep) => (xlabel l,Toasm.translateRep rep))
			        mutable_variables));

       dumpDatalist (Tracetable.MakeMutableTable (main', 
						  map xlabel mutable_objects));

       ()
     end (* allocateProg *)
       handle e => (Printutils.closeOutput (); raise e)

     fun dumpEntryTables nl =
	 let val count = length nl
	     val nl' = map msLoclabel (map Toasm.translateLocalLabel nl)
             fun mktable(name,suffix) =
	       let 
		 val temps = map (fn s => s ^ suffix) nl'
		 val _ = app (fn s => emitString(extern_decl s)) temps
	       in
		 DLABEL (MLE name) ::
		 map (fn s => DATA(MLE s)) temps
	       end
	     val gc_table_begin =    mktable("GCTABLE_BEGIN_VAL","_GCTABLE_BEGIN_VAL")
	     val gc_table_end =	     mktable("GCTABLE_END_VAL","_GCTABLE_END_VAL")
	     val sml_globals =       mktable("SML_GLOBALS_BEGIN_VAL","_SML_GLOBALS_BEGIN_VAL")
	     val end_sml_globals =   mktable("SML_GLOBALS_END_VAL","_SML_GLOBALS_END_VAL")
	     val globaltable_begin = mktable("GLOBAL_TABLE_BEGIN_VAL","_GLOBAL_TABLE_BEGIN_VAL")
	     val globaltable_end =   mktable("GLOBAL_TABLE_END_VAL","_GLOBAL_TABLE_END_VAL")
	     val muttable_begin =    mktable("MUTABLE_TABLE_BEGIN_VAL","_MUTABLE_TABLE_BEGIN_VAL")
	     val muttable_end =      mktable("MUTABLE_TABLE_END_VAL","_MUTABLE_TABLE_END_VAL")
	     val codetable_begin =   mktable("CODE_BEGIN_VAL","_CODE_BEGIN_VAL")
	     val codetable_end =     mktable("CODE_END_VAL","_CODE_END_VAL")
             val entrytable =        mktable("client_entry","")
	     val count = [DLABEL (MLE "module_count"),
			  INT32 (TilWord32.fromInt count)]
         in  
	    dumpDatalist count;
	    dumpDatalist gc_table_begin;
	    dumpDatalist gc_table_end;
	    dumpDatalist sml_globals;
	    dumpDatalist end_sml_globals;
	    dumpDatalist globaltable_begin;
	    dumpDatalist globaltable_end;
	    dumpDatalist muttable_begin;
	    dumpDatalist muttable_end;
	    dumpDatalist codetable_begin;
	    dumpDatalist codetable_end;
	    dumpDatalist entrytable
         end

end 
