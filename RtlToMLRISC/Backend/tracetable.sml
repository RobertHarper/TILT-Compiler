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
OLD   4 bytes to indicate the size(in 4 bytes) of this entry.   
OLD   4 bytes to indicate the size(in 4 bytes) of the "byte section"
OLD   4 bytes to indicate the size(in bytes) the size of the stack frame
OLD   4 bytes to indiate return address position in frame
   4 bytes/1 word used to represent table entry size, frame size, byte section size
      measured in words:
        the lowest 10 bits represent table entry size
        the next higher 10 bits represent frame size
        the next higher 10 bits represent byte section size
        the highest 2 bits represent the octaword offset of return address
      Note that |word section| = |table entry| - |byte section| - |fixed fields|
   8 bytes to indicate the states of the registers
      the two words are defined so that:
         (1) least significant bit corresponds to register 0 and so on
         (2)  if you define x_n and y_n to be bit n of word 1 and of word 2, respectively,
                 then 10 is TRACE_YES, 00 is TRACE_no, 11 is TRACE_CALLEE, 01 is other.
              note that for registers, a TRACE_CALLEE can arise only for the matching register
         (3) this weird mapping is used so that bit operations will quickly give you
             the status without space waste
   varaible number of words used to indicate the state of the stack slots
        (1) 16 pairs of two bits are used in each word from least to most significant
              order to indicate the state of the stack slots
        (2) padding is used to the next word boundary
        (3) 00 -> TRACE_NO; 01-> TRACE_YES; 02 -> TRACE_CALLEE ?; 03 -> TRACE_?
        (4) Note that here the reg on which TRACE_CALLEE is variable and
            so one must look the info up in the byte data section
   variable number of bytes to indicate "byte" and "word" data sections.
     The "byte" data starts and extends with padding to a multiple of 4 bytes.
     Then comes the "word" data.  This variable-sized data comes in the order of all
     the stack slots first and then all the register.
*)
functor Tracetable(val little_endian    : bool 
		   structure Rtl: RTL)
  : TRACETABLE = 
  struct

    structure Machine = struct
      structure Rtl = Rtl

      datatype register = R of int
                        | F of int
      type loclabel = Rtl.local_label
      datatype stacklocation = CALLER_FRAME_ARG of int
                             | THIS_FRAME_ARG of int
                             | SPILLED_INT of int
                             | SPILLED_FP of int
                             | ACTUAL4 of int
                             | ACTUAL8 of int
                             | RETADD_POS

      fun regNum (R n) = n
	| regNum (F n) = n

      fun sloc2int (ACTUAL4 x) = x
	| sloc2int (ACTUAL8 x) = x
	| sloc2int _ = raise Match

      val makeAsmLabel =
	let 
	  fun loop [] = ""
	    | loop (#"'" :: rest) = "PRIME" ^ (loop rest)
	    | loop (#"!" :: rest) = "BANG" ^ (loop rest)
	    | loop (#"%" :: rest) = "PERCENT" ^ (loop rest)
	    | loop (#"&" :: rest) = "AND" ^ (loop rest)
	    | loop (#"$" :: rest) = "DOLLAR" ^ (loop rest)
	    | loop (#"#" :: rest) = "HASH" ^ (loop rest)
	    | loop (#"+" :: rest) = "PLUS" ^ (loop rest)
	    | loop (#"-" :: rest) = "MINUS" ^ (loop rest)
	    | loop (#"/" :: rest) = "SLASH" ^ (loop rest)
	    | loop (#":" :: rest) = "COLON" ^ (loop rest)
	    | loop (#"<" :: rest) = "LT" ^ (loop rest)
	    | loop (#"=" :: rest) = "EQ" ^ (loop rest)
	    | loop (#">" :: rest) = "GT" ^ (loop rest)
	    | loop (#"?" :: rest) = "QUEST" ^ (loop rest)
	    | loop (#"@" :: rest) = "AT" ^ (loop rest)
	    | loop (#"\\" :: rest) = "BACKSLASH" ^ (loop rest)
	    | loop (#"~" :: rest) = "TILDE" ^ (loop rest)
	    | loop (#"`" :: rest) = "ANTIQUOTE" ^ (loop rest)
	    | loop (#"^" :: rest) = "HAT" ^ (loop rest)
	    | loop (#"|" :: rest) = "BAR" ^ (loop rest)
	    | loop (#"*" :: rest) = "STAR" ^ (loop rest)
	    | loop (s :: rest) = (str s) ^ (loop rest)
	in
	  loop o explode
	end

      fun msLoclabel (Rtl.LOCAL_CODE v) = "LC" ^ (makeAsmLabel (Name.var2string v))
	| msLoclabel (Rtl.LOCAL_DATA v) = "LD" ^ (makeAsmLabel (Name.var2string v))
    end

    open Machine
    open Rtl

    datatype calllabel = CALLLABEL of Machine.loclabel
    datatype trace     = TRACE_YES 
                       | TRACE_NO
                       | TRACE_UNSET   (* unset variable; handle specially for gener GC *)
      (* traceability depends on traceability of reg at this moment *)
		       | TRACE_CALLEE     of Machine.register
      (* should be resolved to actual stack locations in the end; word-sized *)
		       | TRACE_STACK      of Machine.stacklocation
      (* stack pos, rec pos *)
		       | TRACE_STACK_REC  of Machine.stacklocation * int list
		       | TRACE_GLOBAL     of Rtl.label
		       | TRACE_GLOBAL_REC of Rtl.label * int list

      (* trace status should never be needed.  A bug if it is.*)
		       | TRACE_IMPOSSIBLE  

    datatype callinfo = CALLINFO of 
      {calllabel  : calllabel, 
       framesize  : int,
       retaddpos  : int,
       regtrace   : (Machine.register * trace) list,
       stacktrace : (int * trace) list                   
       }

    val call_entrysize = 16;
    val ShowDebug      = ref false;
    val ShowDiag       = ref false;
    val TagEntry = ref false;


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
	val words = ref ([] : Rtl.data list);
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
			   | _ => (Rtl.COMMENT "bytedata")::x
			 end
	fun clearbytes() = bytes := []
	fun addword_int(q) = words := ((INT32 q) :: (!words))
	fun addword_label(q) = words := ((DATA q) :: (!words))
	fun getwords() = let val x = rev(!words)
			 in case x of
			     [] => []
			   | _ => (Rtl.COMMENT "worddata")::x
			 end
	fun clearwords() = words := []
    end


    val Count_no = ref 0;
    val Count_yes = ref 0;
    val Count_unset_reg = ref 0;
    val Count_unset_stack = ref 0;
    val Count_stack = ref 0;
    val Count_stack_rec = ref 0;
    val Count_callee = ref 0;
    val Count_global = ref 0;
    val Count_global_rec = ref 0;
    fun inc x = x := (!x + 1)


    fun tr2bot TRACE_NO                  = (inc Count_no; 0)
      | tr2bot TRACE_YES                 = (inc Count_yes; 1)
      | tr2bot (TRACE_CALLEE  r)         = 
	(inc Count_callee; addbyte (Machine.regNum r); 2)
      | tr2bot (TRACE_UNSET) = 
	(inc Count_unset_stack; 
	addword_int (i2w (~1)); addword_int (i2w (~1)); 3) 
      | tr2bot (TRACE_STACK sloc) = 
	(inc Count_stack; addword_int (i2w 0); addword_int (i2w (sloc2int sloc)); 3) 
      | tr2bot (TRACE_STACK_REC (sloc,[i])) = 
        let val pos = sloc2int sloc
	in  (if (!ShowDebug)
	     then (print ("trace_stack_rec  pos,i,val" ^ 
			  (Int.toString pos) ^ "," ^ (Int.toString i) ^ "," ^
			  (Int.toString (2+2*i)) ^ "\n"))
	 else ();
	     (inc Count_stack_rec; addword_int (i2w (2+2*i)); 
	      addword_int (i2w pos); 3))
        end
      | tr2bot (TRACE_GLOBAL lab)        = 
	(inc Count_global; addword_int (i2w 1); addword_label lab; 3)
      | tr2bot (TRACE_GLOBAL_REC (lab,[i]))= 
	(inc Count_global_rec; addword_int (i2w (3+2*i)); 
	 addword_label lab; 3)
      | tr2bot (TRACE_STACK_REC (lab,_)) = error "TRACE_STACK_REC with list of length > 1 not done"
      | tr2bot (TRACE_GLOBAL_REC (lab,_)) = error "TRACE_GLOBAL_REC with list of length > 1 not done"
      | tr2bot TRACE_IMPOSSIBLE          = 
	error "cannot get a trace impossible while making table"


    fun regtr2bits _ TRACE_NO                  = (inc Count_no; (0,0))
      | regtr2bits _ TRACE_YES                 = (inc Count_yes; (1,0))
      | regtr2bits cr (TRACE_CALLEE  r)        = 
	if (cr = Machine.regNum r) then
	    (inc Count_callee; (1,1))
	else error "regtr2bot: non matching TRACE_CALLEE"
      | regtr2bits _ (TRACE_UNSET) = 
	(inc Count_unset_reg; addword_int (i2w (~1)); addword_int (i2w (~1)); (0,1))
      | regtr2bits _ (TRACE_STACK sloc) = 
	(inc Count_stack; addword_int (i2w 0); addword_int (i2w (sloc2int sloc)); (0,1))
      | regtr2bits _ (TRACE_STACK_REC (sloc, [i])) = 
        let val pos = sloc2int sloc
	in (if (!ShowDebug)
	     then (print ("trace_stack_rec  pos,i,val" ^ 
			  (Int.toString pos) ^ "," ^ (Int.toString i) ^ "," ^
			  (Int.toString (2+2*i)) ^ "\n"))
	 else ();
	     (inc Count_stack_rec; addword_int (i2w (2+2*i)); 
	      addword_int (i2w pos); (0,1)))
        end
      | regtr2bits _ (TRACE_GLOBAL lab)        = 
	(inc Count_global; addword_int (i2w 1); addword_label lab; (0,1))
      | regtr2bits _ (TRACE_GLOBAL_REC (lab,[i]))= 
	(inc Count_global_rec; addword_int (i2w (3+2*i)); 
	 addword_label lab; (0,1))
      | regtr2bits _ (TRACE_STACK_REC (lab,_)) = error "TRACE_STACK_REC with list of length > 1 not done"
      | regtr2bits _ (TRACE_GLOBAL_REC (lab,_)) = error "TRACE_GLOBAL_REC with list of length > 1 not done"
      | regtr2bits _ TRACE_IMPOSSIBLE          = 
	error "cannot get a trace impossible while making table"

    fun tr2s TRACE_NO                  = "no"
      | tr2s TRACE_YES                 = "yes"
      | tr2s TRACE_UNSET               = "unset"
      | tr2s (TRACE_CALLEE  r)         = "callee"
      | tr2s (TRACE_STACK sloc) = "stack"
      | tr2s (TRACE_STACK_REC (sloc,i)) = "stack_rec"
      | tr2s _          = 	"<ignoring trace>"

    val trace2string = tr2s	

    fun do_callinfo (CALLINFO {calllabel=CALLLABEL lab,framesize,retaddpos,
			       regtrace,stacktrace}) =
	let
	    val _ = if (framesize mod 4 = 0) then ()
		    else error "framesize not a multiple of 4"

	    fun stacklookup n = 
		case (List.find (fn (v,t) => (n = v)) stacktrace) of
		    NONE => TRACE_NO
		  | (SOME (v,t)) => t
	    fun reglookup n = 
		let fun mapper (v,t) = if (n = Machine.regNum v)
					   then SOME t
				       else NONE
		    val matches = List.mapPartial mapper regtrace
		in  (case matches of
			 [] => TRACE_NO
		       | [t] => ((* print "trace for reg "; print (Int.toString n);
				 print " = "; print (tr2s t); *)
				 t)
		       | _ => (print "multiple traces found for register";
			       print (Int.toString n);
			       print ":: ";
			       app (fn t => (print (tr2s t); print "  ")) matches;
			       print "\n"; hd matches))
		end
(*
	    fun reglookup n = 
		case (List.find (fn (v,t) => (n = Machine.regNum v)) regtrace) of
		    NONE => TRACE_NO
		  | (SOME (v,t)) => t
*)
	    val _ = clearwords()
	    val _ = clearbytes()
	    local
		fun stackloop n = 
		    if (n >= framesize) then nil
		    else
		      (if (!ShowDiag) then
			 print ("Stack  " ^ (Int.toString n) ^ ":" ^
				(tr2s (stacklookup n)) ^ "\n")
		       else ();
			 (tr2bot (stacklookup n)) :: (stackloop (n+4)))
		fun regloop n = 
		    if (n = 32) then (nil,nil)
		    else 
			let val (a,b) = regtr2bits n (reglookup n)
			    val (ar,br) = regloop (n+1)
			in (a::ar,b::br)
			end
	    in
		(* the order here is IMPORTANT *)
		val (yes,no,callee,spec) = (!Count_yes,!Count_no,!Count_callee,
					    !Count_unset_reg +
					    !Count_unset_stack +
					    !Count_stack +
					    !Count_stack_rec +
					    !Count_global +
					    !Count_global_rec)
		val stackbots = stackloop 0
		val (n_yes,n_no,n_callee,n_spec) = (!Count_yes,!Count_no,!Count_callee,
					    !Count_unset_reg +
					    !Count_unset_stack +
					    !Count_stack +
					    !Count_stack_rec +
					    !Count_global +
					    !Count_global_rec)
		val sum = (n_spec + n_yes + n_no + n_callee) - 
		    (spec + yes + no + callee)
		val cursize = (sum + 15) div 16
		val newsize = ((sum - (n_no - no)) + 8) div 8
		val _ = if (sum = (framesize div 4)) then ()
			else (print "OOPS: framesize =  "; print (Int.toString framesize); 
			print "  and  sum count = "; print (Int.toString sum); print "\n")
		val _ = if (!ShowDiag) then
		    (print "\n------------------------\n"; 
		     print (msLoclabel lab); print ":\n";
		     app (fn (v,t) => (print (Int.toString v);
				       print (tr2s t); 
				       print "\n")) stacktrace;
		     print "------------------------\n")
		    else ()
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
	    val labeldata       = DATA (LOCAL_LABEL lab)
	    val (bytedata,octa_ra_offset_word) =
	      if ((retaddpos >= 0) andalso
		  ((retaddpos mod 8) = 0))
		then 
		  if (retaddpos <= 14 * 8)
		    then (getbytes(),i2w (retaddpos div 8))
		  else if (retaddpos <= 255 * 8)
			 then (addbyte_at_beginning(retaddpos div 8);
			       (getbytes(),i2w 15))
		       else error 
			 ("illegal retadd stack pos too large = " ^
			  (Int.toString retaddpos))
	      else error
		("illegal retadd negative or non mult of 8 = " ^
		 (Int.toString retaddpos))
	    val specdata = bytedata @ (getwords())
	    val calldata = 
		(map INT32 regtracewords) @ 
		[Rtl.COMMENT "stacktrace"] @
		(map INT32 stacktracewords) @ 
		specdata
	    fun datalength arg = 
		foldr ((op +): (int*int) -> int) 0 
		    (map 
		     (fn (INT32 _) => 1 
		   | (DATA _) => 1
		   | (Rtl.COMMENT _) => 0
		   | _ => error "datalength") arg)

	    val framesizeword =  (i2w (framesize div 4))
	    val entrysizeword = if (!TagEntry) then (i2w (3 + (datalength calldata)))
				else (i2w (2 + (datalength calldata)))
	    val bytestuffsizeword = (i2w (datalength bytedata))
	    val sizedata = 
		let 
		    val _ = if (W.ugte(entrysizeword,i2w 1024) orelse
				W.ugte(framesizeword,i2w 512) orelse 
				W.ugte(bytestuffsizeword,i2w 512))
				then error "sizes too big" else ()
		    val t1 = bitshift(entrysizeword,0)
		    val t2 = bitshift(framesizeword,10)
		    val t3 = bitshift(bytestuffsizeword,19)
		    val t4 = bitshift(octa_ra_offset_word,28)
		in
		    INT32 (W.orb(W.orb(t1,t2),W.orb(t3,t4)))
		end

	    val templist = sizedata :: calldata
	in
	    if (!TagEntry) then
		((Rtl.COMMENT "-------- label,id,sizes,reg")::
		 labeldata :: (INT32 (i2w (makeintid()))) :: templist)
	    else
		((Rtl.COMMENT "-------- label,sizes,reg")::
		 labeldata :: templist)
	end
    
    fun MakeTableHeader name = 
	[Rtl.COMMENT "gcinfo",DLABEL(ML_EXTERN_LABEL (name^"_GCTABLE_BEGIN_VAL"))]
    fun MakeTable (calllist) = foldr (op @) nil (map do_callinfo calllist) 
    fun MakeTableTrailer name = 
	(if !ShowDebug
	     then (print ("\nTraceability Table Summary: \n");
		   print ("  Count_unset_reg: " ^ (Int.toString (!Count_unset_reg)) ^ "\n");
		   print ("  Count_unset_stack: " ^ (Int.toString (!Count_unset_stack)) ^ "\n");
		   print ("  Count_no: " ^ (Int.toString (!Count_no)) ^ "\n");
		   print ("  Count_yes: " ^ (Int.toString (!Count_yes)) ^ "\n");
		   print ("  Count_stack: " ^ (Int.toString (!Count_stack)) ^ "\n");
		   print ("  Count_stack_rec: " ^ (Int.toString (!Count_stack_rec)) ^ "\n");
		   print ("  Count_callee: " ^ (Int.toString (!Count_callee)) ^ "\n");
		   print ("  Count_global: " ^ (Int.toString (!Count_global)) ^ "\n");
		   print ("  Count_global_rec: " ^ (Int.toString (!Count_global_rec)) ^ "\n");
		   print "\n")
	 else ();
	 [Rtl.COMMENT "endgcinfo with filler for alignment",
	  DLABEL(ML_EXTERN_LABEL (name^"_GCTABLE_END_VAL")),
	  INT32 wzero])


    fun MakeGlobalTable (name,arg) = 
      let
	  fun do_lab_trace(lab,trace) = 
	      let 
		  val _ = clearbytes()
		  val _ = clearwords()
		  val botword = botlist2wordlist [tr2bot trace]
		  fun comfilter [] = []
		    | comfilter ((Rtl.COMMENT _)::rest) = comfilter rest
		    | comfilter (a::rest) = a::(comfilter rest)
		  val specdata = 
		      case (comfilter (getbytes() @ getwords())) of
			  [] => [Rtl.COMMENT "filler",INT32 (i2w 0),INT32(i2w 0)]
			| [a] => [Rtl.COMMENT "bytestuff",a,
				  Rtl.COMMENT "filler",INT32(i2w 0)]
			| [a,b] => [Rtl.COMMENT "wordstuff",a,b]
			| _ => error "global table entry wrong"
	      in (Rtl.COMMENT "-----global label and bot----") ::
		  (DATA lab) :: (map INT32 botword) @ specdata
	      end
      in
	[DLABEL(ML_EXTERN_LABEL (name^"_GLOBAL_TABLE_BEGIN_VAL"))]
	@ (foldr (op @) nil (map do_lab_trace arg))
	@ [Rtl.COMMENT "filler for alignment of global_table",
	   DLABEL(ML_EXTERN_LABEL (name^"_GLOBAL_TABLE_END_VAL")),
	   INT32 wzero]
      end


    fun MakeMutableTable (name,arg) = 
	[DLABEL(ML_EXTERN_LABEL (name^"_MUTABLE_TABLE_BEGIN_VAL"))]
	@ (map (fn (lab) => DATA(lab)) arg)
	@ [Rtl.COMMENT "filler for alignment for mutable_table",
	   DLABEL(ML_EXTERN_LABEL (name^"_MUTABLE_TABLE_END_VAL")),
	   INT32 wzero]

  end
