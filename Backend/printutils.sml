(*$import BBLOCK MACHINEUTILS TRACETABLE PRINTUTILS TextIO Util Listops *)
functor Printutils(val commentHeader : string
		   structure Machineutils : MACHINEUTILS
                   structure Tracetable : TRACETABLE
		   structure Bblock : BBLOCK
		   sharing Tracetable = Bblock.Tracetable)
		      :> PRINTUTILS where Bblock = Bblock
                                    where Machine = Bblock.Machine
				    where Tracetable = Tracetable =
struct
   structure Bblock = Bblock
   structure Machine = Bblock.Machine
   structure Machineutils = Machineutils
   structure Tracetable = Tracetable

   open Machineutils Machine Bblock
   open Core

   val show_labels = ref true
   val debug = ref false


   val error = fn s => Util.error "printutils.sml" s

   val output_stream = ref NONE : TextIO.outstream option ref

   fun emitString msg =
     (case (! output_stream) of
	NONE => error "output stream not open"
      | SOME s => ((* print msg; *)
		   TextIO.output (s, msg)))

   fun emitInstr cmt instr = emitString (msInstruction cmt instr)

   fun emitData arg = 
     let 
       val list = msData arg
       fun doer (0,_) = ()
	 | doer (count,s) = (emitString s; doer (count-1,s))
     in app doer list
     end

   val print_reg = emitString o msReg

   val print_lab = emitString o msLabel

   fun print_pos (IN_REG r) = print_reg r
     | print_pos (ON_STACK s) = (emitString "STACK:"; 
			      emitString (msStackLocation s))
     | print_pos (HINT r) = (emitString "HINT("; print_reg r; emitString ")")
     | print_pos UNKNOWN = emitString "UNKNOWN"


   fun print_asn (tempname,pos) = (emitString "["; print_reg tempname; 
				emitString ","; print_pos pos; emitString "]")

   fun print_move (t,(x,y)) = (print_reg t; emitString ":"; print_pos x;
			    emitString "->"; print_pos y)

   fun print_list f [] = emitString "\n"
     | print_list f (x::xs) = (f x; emitString " "; print_list f xs)

   fun print_set set = print_list print_reg (Regset.listItems set)

   fun print_map map = print_list print_asn (Regmap.listItemsi map)

   fun print_pair f g (a,b) = (emitString "["; f a; emitString ",";
			    g b; emitString "]")

   fun print_int (i:int) = emitString (Int.toString i)

   fun print_trace Tracetable.TRACE_YES = emitString "*YES*"
     | print_trace Tracetable.TRACE_NO = emitString "*NO*"
     | print_trace (Tracetable.TRACE_CALLEE r) = 
       (emitString "*CALLEE["; print_reg r; emitString "]*")
     | print_trace Tracetable.TRACE_UNSET = emitString "*UNSET*"
     | print_trace (Tracetable.TRACE_STACK _) = emitString "*STACK*"
     | print_trace (Tracetable.TRACE_STACK_REC _) = emitString "*STACK_REC*"
     | print_trace (Tracetable.TRACE_GLOBAL _) = emitString "*GLOBAL*"
     | print_trace (Tracetable.TRACE_GLOBAL_REC _) = emitString "*GLOBAL_REC*"
     | print_trace (Tracetable.TRACE_IMPOSSIBLE) = emitString "*IMPOSSIBLE*"

       
   fun openOutput outfilename =
       (if (!debug) then (print "about to open_out "; print outfilename; print "\n") else ();
	output_stream := SOME (TextIO.openOut outfilename))

   fun openAppend outfilename =
       (if (!debug) then (print "about to open_append"; print outfilename; print "\n") else ();
	output_stream := SOME (TextIO.openAppend outfilename))

   fun closeOutput () = 
     (case (! output_stream) of
	NONE => error "Can't close output stream; it's not open."
      | SOME s => ((TextIO.closeOut s) handle _ => error "error closing output stream"; 
		   output_stream := NONE))

   (* OUTPUT PROGRAM *)

   fun dumpBlocks debug proc_name 
                  (psig as (PROCSIG{framesize, arg_ra_pos, 
				    ra_offset,
				    ...}))
                  block_map blocklabels  =
     let 
       fun myEmitInstr i = emitInstr (msAnnotation i) 
	 (stripAnnot i)

       fun dumpBlock l = 
	 let
	   val (BLOCK{instrs,in_live,out_live,succs,def,use,truelabel,...}) =
	       (case Labelmap.find (block_map, l) of
		    SOME value => value
		  | NONE => (print "dumpblock: ";
			     print (msLabel l);
			     error "dumpblock"))
	 in
	   if debug then
	     (emitString commentHeader;
	      emitString " LIVE_IN : "; 
	      print_list print_reg (Regset.listItems (!in_live)))
	   else ();
	     
	   if (! show_labels orelse truelabel) then
	       (print_lab l;
		emitString ":\n")
	   else
	     ();

	   if (eqLabs proc_name l) then
	     emitString(makeAsmHeader psig)
	   else ();


	   app myEmitInstr (rev (! instrs));

	   if debug then
	     (emitString commentHeader;
	      emitString " LIVE_OUT: ";
	      print_list print_reg (Regset.listItems (!out_live));
	      emitString commentHeader;
	      emitString " DEF     : ";
	      print_list print_reg (Regset.listItems def);
	      emitString commentHeader;
	      emitString " USE     : ";
	      print_list print_reg (Regset.listItems use);
	      emitString commentHeader;
	      emitString " SUCCS   : ";
	      print_list print_lab (! succs);
	      emitString "\n")
	   else ()
		   
	 end
       val res = app dumpBlock blocklabels
     in
	res
     end

   exception Zip
       fun zip [] [] = []
         | zip (x::xs) (y::ys) = (x,y) :: (zip xs ys)
	 | zip _ [] = raise Zip
	 | zip [] _ = raise Zip


   fun dumpCodeLabel cls = 
       let open Machine
	   fun member (elem,list) = Listops.member_eq(Rtl.eq_label,elem,list)
	   fun unique [] = []
	     | unique (a::rest) = if (member(a,rest)) then unique rest else a::(unique rest)
	   val ucls = unique cls
     in
       app (emitString o CodeLabelDecl) ucls
     end

   fun dumpProc (name, 
		 psig as PROCSIG{args, res, regs_destroyed, regs_modified,
				 arg_ra_pos, res_ra_pos, ...}, 
		 
		 block_map,
		 block_labels,
		 debug) =
     (app emitString textStart; 
      app emitString (procedureHeader name);
      emitString commentHeader;

      emitString " arguments : ";
      (case arg_ra_pos of
	 SOME (arg_pos) => print_list print_asn (zip args arg_pos)
       | NONE => print_list print_reg args);
      emitString commentHeader;
      emitString (" results    : ");

      (case res_ra_pos of 
	 SOME res_pos => print_list print_asn (zip res res_pos)
       | NONE => print_list print_reg res);

      emitString commentHeader;
      emitString (" destroys   : ");
      emitString (msRegList (! regs_destroyed));
      emitString "\n";
      emitString commentHeader;
      emitString (" modifies   : ");
      emitString (msRegList (! regs_modified));
      emitString "\n";

      (case block_labels of
        (first_label::_) => 
		let val (BLOCK{instrs,...}) = (case (Labelmap.find(block_map, first_label)) of
						   SOME bl => bl
						 | _ => error "missing block")
	        in  case (rev (!instrs)) of
		      first_instr::_ => (case (stripAnnot first_instr) of
					BASE(ILABEL l) =>  
					emitString (".globl " ^ (msLabel l) ^ "\n")
				      | _ => ())
		    | _ => ()
                end
      | _ => ());


      dumpBlocks debug name psig block_map block_labels;
      app emitString (procedureTrailer (msLabel name));
      emitString "\n")

   fun dumpData data =
     (app emitString dataStart;
      Array.app emitData data)
(*      app emitString textStart; emitString "\n" *)

   fun dumpDatalist data =
     (app emitString dataStart;
      app emitData data)
(*      app emitString textStart; emitString "\n" *)


end 
