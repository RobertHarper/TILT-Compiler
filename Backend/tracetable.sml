(*$import Prelude TopLevel TilWord32 Rtl Core TRACETABLE Int Util List Pprtl Stats *)

(* This is how the compiler tells the runtime about how to determine all roots
   from the registers and from the stack.  The runtime, at GC, will walk the 
   stack.  For each frame encountered, it will look up information generated
   by this module and determine whether each stack location is live or not.
   In addition, it will also compute the liveness of the registers.
   
   The runtime will assume that the return address of all stack frames
   are stored in slot 0 of the stack frame.

   Each entry generated will have the following format.
   4 bytes for the return address for indexing into the entry.
-- 4 bytes OPTIONALLY for an id so that one can easily match
       entries to the corresponding GC point in the code.
   4 bytes/1 word used to represent table entry size, frame size, byte section size
      measured in words:
        the lowest 10 bits represent table entry size
        the next higher 10 bits represent frame size
        the next higher 10 bits represent byte section size
        the highest 2 bits represent the quadword offset of return address
      Note that |word section| = |table entry| - |byte section| - |fixed fields|
   2 words for register states.  The 2 words are defined so that:
         (1) Bits (from LSB) n of the first and second word, respectively, specify for register n
                 10 is TRACE_YES
		 00 is TRACE_NO
		 11 is TRACE_CALLEE  - can arise only for the matching register
		 01 is TRACE_SPECIAL - look up in the word section
         (2) This weird mapping allows bit operations that quickly give the status without space waste.
   Variable number of words used to indicate the state of the stack slots
        (1) Each word can describe up to 16 stack slots.  LSB bits describe lowest stack slots.
            There may be unused bits at the upper end of the last word.
        (2) As before, each two bits is interpreted as
                 10 is TRACE_YES
		 00 is TRACE_NO
		 11 is TRACE_CALLEE  - look up in byte section for stack slot
		 01 is TRACE_SPECIAL - look up in the word section
   Variable number of words for the "byte" section.  There may be extra bytes for alignment.
     Optional special data for large return addresses.
     Special data for the stack slots.
   Variable number of words for the "word" section.
     Special data for the stack slots.
     Special data for the registers.
*)

functor Tracetable(val little_endian : bool) :> TRACETABLE =
  struct

    open Rtl

    datatype calllabel = CALLLABEL of Core.label
    datatype trace     = TRACE_YES 
                       | TRACE_NO
                       | TRACE_UNSET   (* unset variable; handle specially for gener GC *)
      (* traceability depends on traceability of reg at this moment *)
		       | TRACE_CALLEE     of Core.register
      (* should be resolved to actual stack locations in the end; word-sized *)
		       | TRACE_STACK      of Core.stacklocation
      (* stack pos, rec pos *)
		       | TRACE_STACK_REC  of Core.stacklocation * int list
		       | TRACE_LABEL     of Core.label
		       | TRACE_LABEL_REC of Core.label * int list
		       | TRACE_GLOBAL     of label
		       | TRACE_GLOBAL_REC of label * int list

      (* trace status should never be needed.  A bug if it is.*)
		       | TRACE_IMPOSSIBLE  

    datatype callinfo = CALLINFO of 
      {calllabel  : calllabel, 
       framesize  : int,
       retaddpos  : int,
       regtrace   : (Core.register * trace) list,
       stacktrace : (int * trace) list                   
       }

    val call_entrysize = 16;
    val ShowDebug      = Stats.ff "TraceDebug"
    val ShowDiag       = Stats.ff "TraceDiag"
    val TagEntry       = Stats.ff "TraceTagEntry"


(* ---------------------------------------------------------------------- *)
    val error = fn s => Util.error "tracetable.sml" s

    structure W = TilWord32
    val i2w = W.fromInt
    val w2i = W.toInt
    val wone = W.one
    val wzero = W.zero
    (* positive shift disp is left shift *)
    fun bitshift(v,disp) = if (disp >= 0) 
			       then W.lshift(v,disp)
			   else W.rshiftl(v,~disp)
    local
      val count = ref 5000
    in
      fun makeintid() = 
	(count := !count + 1; !count)
    end

    fun layout sz 0 [] = nil
      | layout sz n [] = (INT32 (wzero))::(layout sz (n-1) nil)
      | layout sz 0 ls = (INT32 (wzero))::(layout sz (sz-1) ls)
      | layout sz n (a::rest) = (INT32 a)::(layout sz (n-1) rest)
	
    local
	fun bot2w acc 16 []         = [acc]
	  | bot2w acc 16 arg        = acc::(bot2w (i2w 0) 0 arg)
	  | bot2w acc _  []         = [acc]
	  | bot2w acc pos (a::rest) = 
	    bot2w (W.orb(acc,bitshift(i2w a,2*pos))) (pos+1) rest
	fun bit2w acc 32 []         = [acc]
	  | bit2w acc 32 arg        = acc::(bit2w (i2w 0) 0 arg)
	  | bit2w acc _  []         = [acc]
	  | bit2w acc pos (a::rest) = 
	    bit2w (W.orb(acc,bitshift(i2w a,pos))) (pos+1) rest
    in
	fun botlist2wordlist arg = bot2w (i2w 0) 0 arg
	fun bitlist2wordlist arg = bit2w (i2w 0) 0 arg
    end

    fun fourbyte_2word (a,b,c,d) = 
        let
          val lowshort  = a + b * 256
          val highshort = c + d * 256
          val s16 = i2w(256 * 256)
          val w = W.orb(W.umult(s16,i2w highshort),i2w lowshort)
	in w
	end

    fun bytelist2wordlist []      = nil
      | bytelist2wordlist [a]     = bytelist2wordlist[a,0,0,0]
      | bytelist2wordlist [a,b]   = bytelist2wordlist[a,b,0,0]
      | bytelist2wordlist [a,b,c] = bytelist2wordlist[a,b,c,0]
      | bytelist2wordlist (a::b::c::d::rest) = 
        let val w = if (little_endian) 
		      then fourbyte_2word(a,b,c,d)
		    else fourbyte_2word(d,c,b,a)
        in w::(bytelist2wordlist rest)
        end
  

    local
	type byte = int
	val bytes = ref ([] : byte list);
	val words = ref ([] : data list);
    in
      (* yes, they are stored backwards *)
	fun addbyte(b) = if (b > 255 orelse b < 0)
			   then error "NOT A BYTE"
			 else bytes := (b :: (!bytes))
	fun addbyte_at_beginning(b) = if (b > 255 orelse b < 0)
					then error "NOT A BYTE"
				      else bytes := ((!bytes) @ [b])
	fun getbytes() = let val x = (map INT32 (bytelist2wordlist(rev(!bytes))))
			 in case x of 
			     [] => []
			   | _ => (COMMENT "bytedata")::x
			 end
	fun clearbytes() = bytes := []
	fun addword_int(q) = words := ((INT32 q) :: (!words))
	fun addword_label(q) = words := ((DATA q) :: (!words))
	fun getwords() = let val x = rev(!words)
			 in case x of
			     [] => []
			   | _ => (COMMENT "worddata")::x
			 end
	fun clearwords() = words := []
    end


    val Count_no = ref 0;
    val Count_yes = ref 0;
    val Count_unset_reg = ref 0;
    val Count_unset_stack = ref 0;
    val Count_stack_rec = ref 0;
    val Count_callee = ref 0;
    val Count_label_rec = ref 0;
    val Count_global_rec = ref 0;
    fun inc x = x := (!x + 1)

    (* these number must match up with the macros in stack.h *)
    (* the lower 2 bits are used for other things so we only have 30 bits *)
    local
(*
	val bits = [6,8,8,8] (* must sum to <= 30 *)
	val pows = [1,64,64*256,64*256*256]
	val maxes  = [63,255,255,255]
*)
	val bits = [14,8,8] (* must sum to <= 30 *)
	val pows = [1,16384,16384*256]
	val maxes  = [16383,255,255]
	fun local_error indices = 
	     (print ("indices2int: ");
	      app (fn m => (print (Int.toString m);
			    print "  ")) indices;
	      print "\n")
	fun loop [] _ _ _ = 0
	  | loop (index::indexRest) (pow::powRest) (max::maxRest) error = 
	    if (index + 1 > max)
		then error "index too large"
	    else (index + 1) * pow + (loop indexRest powRest maxRest error)
	  | loop _ _ _ error = error "too many indices"

	(* indices2int : int list -> int.  Uses low 30 bits. *)
	fun indices2int indices =
	    (loop indices pows maxes
	     (fn s => (local_error indices; error s)))
    in
	datatype special_type = STACK | LABEL | GLOBAL | UNSET

	(* indices2word : int list -> W.word.  Uses 32 bits.  *)
	fun indices2word (ty,indices) =
	    let val tbits = i2w (case ty of
				     STACK => 0
				   | LABEL => 1
				   | GLOBAL => 2
				   | UNSET => 3)
		val ibits = i2w (indices2int indices)
	    in  W.orb (W.lshift (ibits, 2), tbits)
	    end
    end

    fun tr2bot TRACE_NO                  = (inc Count_no; 0)
      | tr2bot TRACE_YES                 = (inc Count_yes; 1)
      | tr2bot (TRACE_CALLEE  r)         = 
	(inc Count_callee; addbyte (Core.regNum r); 2)
      | tr2bot (TRACE_UNSET) = 
	(inc Count_unset_stack; 
	addword_int (i2w (~1)); addword_int (i2w (~1)); 3) 
      | tr2bot (TRACE_STACK sloc) = tr2bot(TRACE_STACK_REC (sloc, []))
      | tr2bot (TRACE_LABEL lab) = tr2bot (TRACE_LABEL_REC (lab, []))
      | tr2bot (TRACE_GLOBAL lab) = tr2bot (TRACE_GLOBAL_REC (lab, []))
      | tr2bot (TRACE_STACK_REC (sloc,indices)) =
	(inc Count_stack_rec; 
	 addword_int (indices2word (STACK, indices));
	 addword_int (i2w (Core.sloc2int sloc));
	 3)
      | tr2bot (TRACE_LABEL_REC (lab,indices)) =
	(inc Count_label_rec; 
	 addword_int (indices2word (LABEL, indices));
	 addword_label lab;
	 3)
      | tr2bot (TRACE_GLOBAL_REC (lab,indices)) =
	(inc Count_label_rec; 
	 addword_int (indices2word (GLOBAL, indices));
	 addword_label lab;
	 3)
      | tr2bot TRACE_IMPOSSIBLE          = 
	error "cannot get a trace impossible while making table"


    fun regtr2bits _ TRACE_NO                  = (inc Count_no; (0,0))
      | regtr2bits _ TRACE_YES                 = (inc Count_yes; (1,0))
      | regtr2bits cr (TRACE_CALLEE  r)        = 
	if (cr = Core.regNum r) then
	    (inc Count_callee; (1,1))
	else (print ("WARN ERROR WRONG WRONG WRONG: regtr2bot: non matching TRACE_CALLEE: " ^
		    (Int.toString cr) ^ " != " ^ (Int.toString (Core.regNum r)) ^ "\n");
	      (1,0))
      | regtr2bits _ (TRACE_UNSET) = 
	(inc Count_unset_reg; addword_int (i2w (~1)); addword_int (i2w (~1)); (0,1))
      | regtr2bits _ (tr as TRACE_STACK sloc) = (tr2bot tr; (0,1))
      | regtr2bits _ (tr as TRACE_STACK_REC _) = (tr2bot tr; (0,1))
      | regtr2bits _ (tr as TRACE_GLOBAL _)        = (tr2bot tr; (0,1))
      | regtr2bits _ (tr as TRACE_GLOBAL_REC _) =  (tr2bot tr; (0,1))
      | regtr2bits _ (tr as TRACE_LABEL _)        = (tr2bot tr; (0,1))
      | regtr2bits _ (tr as TRACE_LABEL_REC _) =  (tr2bot tr; (0,1))
      | regtr2bits _ TRACE_IMPOSSIBLE          = 
	error "cannot get a trace impossible while making table"


    val msReg = Core.msReg
    val msSloc = Core.msStackLocation
    val msLabel = Pprtl.label2s
    fun msIndices i = (foldr (fn (i,s) => "." ^ Int.toString i ^ s) "" i)

    fun msTrace TRACE_NO                   = "no"
      | msTrace TRACE_YES                  = "yes"
      | msTrace TRACE_UNSET                = "unset"
      | msTrace (TRACE_CALLEE  r)          = ("callee " ^ msReg r)
      | msTrace (TRACE_STACK sloc)         = ("stack " ^ msSloc sloc)
      | msTrace (TRACE_STACK_REC (sloc,i)) = ("stack_rec " ^ msSloc sloc ^ msIndices i)
      | msTrace (TRACE_LABEL lab)          = ("label " ^ msLabel lab)
      | msTrace (TRACE_LABEL_REC (lab,i))  = ("label_rec " ^ msLabel lab ^ msIndices i)
      | msTrace (TRACE_GLOBAL lab)         = ("global " ^ msLabel lab)
      | msTrace (TRACE_GLOBAL_REC (lab,i)) = ("global_rec " ^ msLabel lab ^ msIndices i)
      | msTrace _                          = "<ignoring trace>"

    fun do_callinfo (CALLINFO {calllabel=CALLLABEL lab,framesize,retaddpos,
			       regtrace,stacktrace}) =
	let
	    fun printPair f (x, t) = (print "\t";
				      print (f x);
				      print ":";
				      print (msTrace t);
				      print "\n")
	    fun printPairs f L = app (printPair f) L
	    val _ = (if !ShowDiag then
			 (print "\n------------------------\n";
			  print (Pprtl.label2s lab); print ":\n";
			  print "framesize:"; print (Int.toString framesize); print "\n";
			  print "retaddpos:"; print (Int.toString retaddpos); print "\n";
			  print "regtrace:\n"; printPairs Core.msReg regtrace;
			  print "stacktrace:\n"; printPairs Int.toString stacktrace;
			  print "------------------------\n")
		     else ())

	    val _ = if (framesize mod 4 = 0) then ()
		    else error "framesize not a multiple of 4"

	    fun stacklookup n = 
		case (List.find (fn (v,t) => (n = v)) stacktrace) of
		    NONE => TRACE_NO
		  | (SOME (v,t)) => t
	    fun reglookup n = 
		let fun mapper (v,t) = if (n = Core.regNum v)
					   then SOME t
				       else NONE
		    val matches = List.mapPartial mapper regtrace
		in  (case matches of
			 [] => TRACE_NO
		       | [t] => ((* print "trace for reg "; print (Int.toString n);
				 print " = "; print (msTrace t); *)
				 t)
		       | _ => (print "multiple traces found for register";
			       print (Int.toString n);
			       print ":: ";
			       app (fn t => (print (msTrace t); print "  ")) matches;
			       print "\n"; hd matches))
		end
	    val _ = clearwords()
	    val _ = clearbytes()
	    local
		fun stackloop n = 
		    if (n >= framesize) then nil
		    else
			let val tr = stacklookup n
			    val _ = (if (!ShowDiag) then
					 print ("Stack  " ^ (Int.toString n) ^ ":" ^
						(msTrace tr) ^ "\n")
				     else ())
			in  tr2bot tr :: (stackloop (n+4))
			end
		fun regloop n = 
		    if (n = 32) then (nil,nil)
		    else 
			let val tr = reglookup n
			    val _ = (if (!ShowDiag) then
					 print ("Reg  " ^ (Int.toString n) ^ ":" ^
						(msTrace tr) ^ "\n")
				     else ())
			    val (a,b) = regtr2bits n tr
			    val (ar,br) = regloop (n+1)
			in (a::ar,b::br)
			end
	    in
		(* the order here is IMPORTANT *)
		val (yes,no,callee,spec) = (!Count_yes,!Count_no,!Count_callee,
					    !Count_unset_reg +
					    !Count_unset_stack +
					    !Count_label_rec +
					    !Count_stack_rec +
					    !Count_global_rec)
		val stackbots = stackloop 0
		val (n_yes,n_no,n_callee,n_spec) = (!Count_yes,!Count_no,!Count_callee,
					    !Count_unset_reg +
					    !Count_unset_stack +
					    !Count_label_rec +
					    !Count_stack_rec +
					    !Count_global_rec)
		val sum = (n_spec + n_yes + n_no + n_callee) - 
		    (spec + yes + no + callee)
		val cursize = (sum + 15) div 16
		val newsize = ((sum - (n_no - no)) + 8) div 8
		val _ = if (sum = (framesize div 4)) then ()
			else (print "OOPS: framesize =  "; print (Int.toString framesize); 
			print "  and  sum count = "; print (Int.toString sum); print "\n")
		val _ = if (!ShowDiag) then
		    (print ("no,yes,callee,spec: " ^
			    (Int.toString (n_no - no)) ^ ", " ^
			    (Int.toString (n_yes - yes)) ^ ", " ^
			    (Int.toString (n_callee - callee)) ^ ", " ^
			    (Int.toString (n_spec - spec)) ^ "      " ^
			    (Int.toString sum) ^ "    " ^
			    (Int.toString (cursize - newsize)) ^
			    (if (cursize < newsize) then "   BAD\n" else 
				 if (cursize = newsize) then "   SAME\n" else "   WIN\n")))
			else ()
		val stacktracewords = botlist2wordlist stackbots
		val (regtracebits_a, regtracebits_b) = regloop 0
		val regtracewords = (bitlist2wordlist regtracebits_a) @
				     (bitlist2wordlist regtracebits_b)
	    end
	    val labeldata       = DATA lab
	    val (bytedata,quad_ra_offset_word) =
	      if ((retaddpos >= 0) andalso
		  ((retaddpos mod 4) = 0))
		then 
		  if (retaddpos < 31 * 4)
		    then (getbytes(),i2w (retaddpos div 4))
		  else if (retaddpos <= 255 * 4)
			 then (addbyte_at_beginning(retaddpos div 4);
			       (getbytes(),i2w 15))
		       else error 
			 ("illegal retadd stack pos too large = " ^
			  (Int.toString retaddpos))
	      else error
		("illegal retadd negative or non mult of 4 = " ^
		 (Int.toString retaddpos))
	    val specdata = bytedata @ (getwords())
	    val calldata = 
		(map INT32 regtracewords) @ 
		[COMMENT "stacktrace"] @
		(map INT32 stacktracewords) @ 
		specdata
	    fun datalength arg = 
		foldr ((op +): (int*int) -> int) 0 
		    (map 
		     (fn (INT32 _) => 1 
		   | (DATA _) => 1
		   | (COMMENT _) => 0
		   | _ => error "datalength") arg)

	    val framesizeword =  (i2w (framesize div 4))
	    val entrysizeword = if (!TagEntry) then (i2w (3 + (datalength calldata)))
				else (i2w (2 + (datalength calldata)))
	    val bytestuffsizeword = (i2w (datalength bytedata))
	    val sizedata = 
		let
		    fun local_error (what, v) =
			(print what; print " = "; print (W.toDecimalString v); print "\n";
			 error ("giant frame: " ^ what ^ " too big"))
		    val _ = if (W.ugte(entrysizeword,i2w 5124))
				then local_error ("entrysizeword", entrysizeword) else ()
		    val _ = if (W.ugte(framesizeword,i2w 512))
				then local_error ("framesizeword", framesizeword) else ()
		    val _ = if (W.ugte(bytestuffsizeword,i2w 512))
				then local_error ("bytestuffsizeword", bytestuffsizeword) else ()
		    val t1 = bitshift(entrysizeword,0)
		    val t2 = bitshift(framesizeword,9)
		    val t3 = bitshift(bytestuffsizeword,18)
		    val t4 = bitshift(quad_ra_offset_word,27)
		in
		    INT32 (W.orb(W.orb(t1,t2),W.orb(t3,t4)))
		end

	    val templist = sizedata :: calldata
	in
	    if (!TagEntry) then
		((COMMENT "-------- label,id,sizes,reg")::
		 labeldata :: (INT32 (i2w (makeintid()))) :: templist)
	    else
		((COMMENT "-------- label,sizes,reg")::
		 labeldata :: templist)
	end
    
    fun MakeTableHeader name = 
	[COMMENT "gcinfo",DLABEL(ML_EXTERN_LABEL (name^"_GCTABLE_BEGIN_VAL"))]
    fun MakeTable (calllist) = List.concat (map do_callinfo calllist)
    fun MakeTableTrailer name = 
	(if !ShowDebug
	     then (print ("\nTraceability Table Summary: \n");
		   print ("  Count_unset_reg: " ^ (Int.toString (!Count_unset_reg)) ^ "\n");
		   print ("  Count_unset_stack: " ^ (Int.toString (!Count_unset_stack)) ^ "\n");
		   print ("  Count_no: " ^ (Int.toString (!Count_no)) ^ "\n");
		   print ("  Count_yes: " ^ (Int.toString (!Count_yes)) ^ "\n");
		   print ("  Count_callee: " ^ (Int.toString (!Count_callee)) ^ "\n");
		   print ("  Count_stack_rec: " ^ (Int.toString (!Count_stack_rec)) ^ "\n");
		   print ("  Count_label_rec: " ^ (Int.toString (!Count_label_rec)) ^ "\n");
		   print ("  Count_global_rec: " ^ (Int.toString (!Count_global_rec)) ^ "\n");
		   print "\n")
	 else ();
	 [COMMENT "endgcinfo with filler for alignment",
	  DLABEL(ML_EXTERN_LABEL (name^"_GCTABLE_END_VAL")),
	  INT32 wzero])


    fun MakeMutableTable (name,arg) = 
      let
	  fun do_lab_trace(lab,trace) = 
	      let 
		  val _ = clearbytes()
		  val _ = clearwords()
		  val botword = botlist2wordlist [tr2bot trace]
		  fun comfilter [] = []
		    | comfilter ((COMMENT _)::rest) = comfilter rest
		    | comfilter (a::rest) = a::(comfilter rest)
		  val specdata = 
		      case (comfilter (getbytes() @ getwords())) of
			  [] => [COMMENT "filler",INT32 (i2w 0),INT32(i2w 0)]
			| [a] => [COMMENT "bytestuff",a,
				  COMMENT "filler",INT32(i2w 0)]
			| [a,b] => [COMMENT "wordstuff",a,b]
			| _ => error "global table entry wrong"
	      in (COMMENT "-----global label and bot----") ::
		  (DATA lab) :: (map INT32 botword) @ specdata
	      end
      in
	[DLABEL(ML_EXTERN_LABEL (name^"_MUTABLE_TABLE_BEGIN_VAL"))]
	@ (List.concat (map do_lab_trace arg))
	@ [COMMENT "filler for alignment of global_table",
	   DLABEL(ML_EXTERN_LABEL (name^"_MUTABLE_TABLE_END_VAL")),
	   INT32 wzero]
      end


  end
