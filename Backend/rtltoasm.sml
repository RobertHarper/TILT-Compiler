functor Rtltoasm (val commentHeader : string
		  structure Printutils : PRINTUTILS
		  structure Callconv : CALLCONV
		  structure Procalloc : PROCALLOC
		  structure Recursion : RECURSION
		  structure Toasm : TOASM
		  sharing Printutils.Machineutils.Machine
		    = Callconv.Machine
		    
		  sharing Printutils.Bblock 
		    = Procalloc.Bblock
		    = Toasm.Bblock

		  sharing Printutils.Tracetable
		    = Procalloc.Tracetable 
		    = Toasm.Tracetable 

		  sharing Recursion.Rtl = Printutils.Machineutils.Machine.Rtl 
		    ) : RTLTOASM  =
struct

   structure Rtl = Printutils.Machineutils.Machine.Rtl

   open Callconv Printutils Printutils.Machineutils Rtl
   open Machine

   fun flatten arg = foldr (op @) [] arg
   val debug       = ref false
   val knowns      = ref false
   val msgs        = ref false

   fun msg (x: string) = if !msgs then print x else ()
   val error = fn s => Util.error "rtltoasm.sml" s


   fun filter p [] = []
     | filter p (x::xs) = if (p x) then x :: (filter p xs) else filter p xs


(* ----------------------------------------------------------------- *)

   fun allocateModule (prog as Rtl.MODULE{procs, data, main, 
					  mutable_objects,
					  mutable_variables}) =
     let
       val {callee_map, rtl_scc, ...} = Recursion.procGroups prog
       local
	   val recursive_components =
	       map (map Toasm.translateLocalLabel) rtl_scc
	       
	   val _ = print ("***** There are " ^ (Int.toString (length procs)) 
	       ^ " procedures  and  ")
	   val _ = print ((Int.toString (length recursive_components))
			  ^ " recursive components with the largest being " ^
			  (Int.toString (foldr Int.max 0
					   (map length recursive_components))) ^
			  "\n")

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
	   val names = map (fn (Rtl.PROC{name,...}) => 
			    Toasm.translateLocalLabel name) procs
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

		 val code_labels_listlist = map allocateComponent (map (fn x => [x]) nonrecursive_callees)
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
		      Toasm.translateProc rtlproc)
		   end


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
			   dumpProc(name,psig, block_map, blocklabels, true))
		     else ()

		 val _ = msg "\tallocating\n"

		 val temp = Procalloc.allocateProc1
		     {getSignature = getSig,
		      name      = name,
		      block_map = block_map,
		      tracemap  = tracemap,
		      procsig   = psig,
		      stack_resident = stack_resident}
		     handle e => (print "exception from Procalloc\n";
				  raise e)

	       in
		   (NONE, SOME (name, temp),code_labels)
	       end
	 end (* allocateProc1 *)
     
       and allocateProc2 (name, res_of_allocateproc1) =
	   let
	       val (new_sig, new_block_map, new_block_labels, gc_data) =
		   Procalloc.allocateProc2 res_of_allocateproc1
	       fun doer l = 
		 let
		   val (Bblock.BLOCK{instrs,in_live,out_live,succs,def,use,truelabel,...}) =
		       (case (Labelmap.find (new_block_map, l)) of
			    SOME b => b | NONE => error "missing block")
		   fun filter [] = []
		     | filter ((BASE(LADDR(_,I (LOCAL_CODE l))))::rest) = (I (LOCAL_CODE l)) :: (filter rest)
		     | filter ((BASE(LADDR(_,MLE s)))::rest) = (MLE s) :: (filter rest)
		     | filter ((BASE(LADDR(_,CE x)))::rest) = (CE x) :: (filter rest)
		     | filter (a::rest) = filter rest
		 in filter (map Bblock.stripAnnot (!instrs))
		 end
	       val code_label_listlist = map doer new_block_labels
	       val code_label_list : label list = flatten code_label_listlist
	   in
	       setSig name new_sig;
	       msg "\tdumping\n";
	       if !debug then
		   (emitString commentHeader;
		    emitString(" dumping final version of procedure "))
	       else ();
		dumpProc (name, new_sig, new_block_map, 
			     new_block_labels, !debug);
		dumpDatalist gc_data;
		if !debug then
		    (emitString commentHeader;
		     emitString(" done procedure "))
		else ();
		(new_sig, code_label_list)
	   end
	   
       and allocateComponent chunk = 
	 let 
	     val temps = map allocateProc1 chunk
	     local
		 fun helper (PROCSIG{regs_modified,...}) = regs_modified
	     in
		 fun get_modsetref (SOME psig,NONE,_) = helper psig
		   | get_modsetref (NONE,SOME(_,({getSignature,name,block_map,
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
		 | (NONE, SOME x, cls) => (allocateProc2 x,cls)
		 | _ => error "allocateproc in allocatecomponent"
	     val code_labels_listlist = map (fn ((_,a),b) => (a @ b)) (map final_alloc temps)
	     val code_labels = flatten code_labels_listlist
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
       val _ = Printutils.reset_times();
       val _ = Procalloc.reset_times();
       val _ = app emitString textStart;
       val _ = emitString ("\t.globl "^main'^"_CODE_END_VAL\n");
       val _ = emitString ("\t.globl "^main'^"_CODE_BEGIN_VAL\n");
       val _ = emitString (""^main'^"_CODE_BEGIN_VAL:\n");
       val code_labels_listlist = map allocateComponent component_names;
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
       dumpData data;
       app emitString dataStart;
       emitString ("\n"^end_sml_global^":   ");
       emitString commentHeader;
       emitString " filler next\n";
       emitString ("\t.long 0\n\n");
       dumpDatalist (Tracetable.MakeGlobalTable 
		          (main', 
			   map (fn (v, rep) => (v,Toasm.translateRep rep))
			        mutable_variables));

       dumpDatalist (Tracetable.MakeMutableTable (main', 
						  mutable_objects));
       print "Done compiling.\n";
       Procalloc.print_times();
       Printutils.print_times();
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
		 DLABEL (ML_EXTERN_LABEL name) ::
		 map (fn s => DATA(ML_EXTERN_LABEL s)) temps
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
	     val count = [DLABEL (ML_EXTERN_LABEL "module_count"),
			  INT32 (TilWord32.fromInt count)]
         in dumpDatalist count;
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
