functor Rtlinterp(structure Pprtl : PPRTL
		  structure Rtl : RTL
		  structure Rtltags : RTLTAGS
		  structure Heap : RTLHEAP
		  structure Registerset : REGISTERSET
		  sharing type Heap.iword = Registerset.iword
		  sharing type Heap.quad_val = Registerset.quad_val
		  sharing type Heap.instr = Rtl.instr = Registerset.instr
			  and type Registerset.regi = Rtl.regi
			      and type Registerset.regf = Rtl.regf
				  and type Heap.w32 = Word32.word
		   sharing Rtl = Pprtl.Rtl = Rtltags.Rtl
                   structure Operations : OPERATIONS
                   sharing type Operations.cmp = Rtl.cmp
 		) 
 : RTLINTERP  = 
  struct
    
    open Rtl Pprtl;
    open Registerset
    structure O = Operations

    val debug = ref false
    val error = fn s => Util.error "rtlinterp.sml: " s
    type quad_val = Registerset.quad_val;
    structure H = Heap
    structure R = Registerset

    val Overflow_exnval   = 12321;
    val DivideZero_exnval = 21012;

    val getival = R.lookupval_ireg;
    val getfval = R.lookupval_freg;
    fun getlowival ri = case (R.lookupval_ireg ri) of (a,b) => b
    val setireg = R.update_ireg;
    val getireg = R.lookup_ireg;
    val setfreg = R.update_freg;
    val getfreg = R.lookup_freg;

    structure W = TilWord32
    val i2w = W.fromInt
    val w2i = W.toInt
    val wzero = W.zero

    val outstream = ref (NONE : TextIO.outstream option);
    val print_trace = ref false;
    val print_dump = ref false;
    val print_dump_callret = ref false;
    val bp = ref 0
    val window = ref 15

    val pc = ref 0;
    val codebegin = ref 0;
    val actualstart = 256

    fun crash (msg : string) =
	(print "*** CRASHED ***\n";
	 print msg; 
	 print "\n";
	 print "pc = ";
	 print (Int.toString (!pc));
	 print "   ";
	 Pprtl.pp_Instr (H.lookupinstr(i2w (!pc)));
	 print "\n";
	 print "REGISTER STATE\n";
	 R.showreg_nonzero();
	 Heap.showheap (i2w (!pc - 4 * !window),!window*2);
	 error "")
	 
    val cur_module = ref ([MODULE{procs=[],
			     data=(Array.array(0,(STRING ("")))),
			     main=(named_code_label ""),
			     mutable_objects = nil,
			     mutable_variables = nil}]);
    fun ea_to_val(EA(ri,disp),sc) = 
      let 
	val _ = if (sc = 0) then crash "ea_to_val(_,0)" else ()
	val res = W.uplus(getlowival(ri),i2w(disp))
	val check = if ((W.umod(res,i2w sc)) = wzero) then ()
	  else crash
	    ("Unaligned effective address " ^ 
	     (W.toDecimalString res) ^ " " ^ (Int.toString sc))
      in res
      end
    fun label_to_address [] label = 
      crash ("label_to_address" ^ (label2s label))
      | label_to_address ((l,location)::rest) label = 
	if (eq_label(l,label)) 
	  then location else (label_to_address rest label)
    fun address_to_label [] add = crash ("address_to_label" ^ (Int.toString add))
      | address_to_label ((l,location)::rest) add = 
	if (location = add) then l else (address_to_label rest add)
    local 
      val flag = ref false
    in
      fun pc_unset_changed() = flag := false
      fun pc_set_changed() = flag := true
      fun pc_look_changed() = !flag
    end
    exception RTL_HALTED
    val haltpc = 2000000
    fun pc_abs_change w = (if (W.equal(w,i2w haltpc))
			       then raise RTL_HALTED
			   else pc := w2i w; pc_set_changed())

    val label_table = ref ([] : (label * int) list)
    val init_label_table = [(C_EXTERN_LABEL "ml_output", 1000000),
			    (C_EXTERN_LABEL "ml_input", 1000004),
			    (C_EXTERN_LABEL "ml_lookahead", 1000008),
			    (C_EXTERN_LABEL "ml_open_in", 1000012),
			    (C_EXTERN_LABEL "ml_open_out", 1000016),
			    (C_EXTERN_LABEL "ml_close_in", 1000020),
			    (C_EXTERN_LABEL "ml_close_out", 1000024),
			    (C_EXTERN_LABEL "ml_flush_out", 1000028),
			    (C_EXTERN_LABEL "ml_end_of_stream", 1000032),
			    (C_EXTERN_LABEL "ml_spill_vararg", 1000036)
			    ]
    fun reset_label_table() = label_table := init_label_table

    fun pc_label_change label = 
      (pc := (label_to_address (!label_table) label); pc_set_changed())

    fun find_proc(label) = 
      let fun loop ([],t) = loop2 t
	    | loop ((p as PROC({name,...})) :: rest,t) =
	      if (eq_label(LOCAL_LABEL name,label)) then p else loop(rest,t)
          and loop2 [] = crash ("find_proc " ^ (label2s label))
	    | loop2 (MODULE {procs,...} :: t) = loop(procs,t)
      in loop2 (!cur_module)
      end

(* -------------- normal call stack and exn stack ------------------ *)
    type callblob = {func : reg_or_label, return : regi option, 
		     args : regi list * regf list,
		     results : regi list * regf list,
		     tailcall : bool,
		     save : save}
    type frame = callblob * proc * (quad_val list * quad_val list)
	         * (quad_val list * quad_val list)

    val stack = ref ([] : frame list)
    fun push_stack(s) = stack := (s:: (!stack))
    fun pop_stack() = case (!stack) of
      a::b => (stack := b; a)
    | _ => (crash "pop_stack")
    fun peek_stack() = case (!stack) of
      a::b => a
    | _ => (crash "peek_stack")

fun copy_all_stack() = !stack
fun replace_all_stack(arg) = stack := arg

    type exn_frame = frame list;

    val exn_stack = ref ([] : exn_frame list)

    fun push_exn_stack(s) = exn_stack := (s:: (!exn_stack))

    fun pop_exn_stack() = 
	case (!exn_stack)
	of a::b => (exn_stack := b; a)
          | _ => crash "pop_exn_stack"
	
(* --------------------- code and data layout ----------------- *)
   
(* Since these functions are laying out code using a ref cell for current location *)
(* it is imperative that their execution is sequentialized explicitly *)
    fun RTL_layout modules =
      let 
	val _ = reset_label_table()
	val location = ref 0 : int ref;
	fun inc_location() = location := (!location + 4)
	fun add_location(delta) = location := (!location + delta)
	fun inst_layout pos code =
	  let val codesize = Array.length code
	  in if (pos >= codesize) 
		then ()
	     else let val i = Array.sub(code,pos)
		  in  (H.storeinstr(i2w(!location),i);
		       (case i of
			  (ILABEL l) => (label_table :=
					 ((LOCAL_LABEL l,!location)::
					  (!label_table));
					 (inst_layout (pos+1) code))
			| _          => (inc_location();
					 (inst_layout (pos+1) code))))
		  end
	  end

        (* it's important that the label for a PROC be put first on the
           label table list ... that way when we map addresses back to
           procedure names we find the procedure name.   There really
           should be two separate tables. *)

	fun proc_layout (PROC {name,code,...}) =
	  let 
	    val curlocation = !location
	  in  inst_layout 0 code;
	      label_table := (LOCAL_LABEL name,curlocation) :: (!label_table)
	  end
	fun code_layout [] = ()
	  | code_layout (p::procs) = 
		let val _ = proc_layout p
		in  (code_layout procs)
		end
	fun dataitem_layout wh (COMMENT _) _ = 0
	  | dataitem_layout wh (INT32(w)) _ = (H.storelong(i2w wh,w); 4)
	  | dataitem_layout wh (INT_FLOATSIZE(w)) _ = 
	    (H.storequad(i2w wh,LONGS(WORD 0w0,WORD w)); 8)
	  | dataitem_layout wh (FLOAT(strf)) _ = 
	    (H.storefloat(i2w wh,O.str_to_real strf); 8)
	  | dataitem_layout wh (ARRAYI(0,_)) _ = 0
	  | dataitem_layout wh (ARRAYI(n,v)) f = 
	    let val sz = dataitem_layout wh (INT32 (v)) f
	    in  sz + (dataitem_layout (wh+sz) (ARRAYI(n-1,v)) f)
	    end
	  | dataitem_layout wh (ARRAYF(0,_)) _ = 0
	  | dataitem_layout wh (ARRAYF(n,v)) f = 
	    let val sz = dataitem_layout wh (FLOAT (v)) f
	    in  sz + (dataitem_layout (wh+sz) (ARRAYF(n-1,v)) f)
	    end
	  | dataitem_layout wh (DATA(l)) false = 4 
	  | dataitem_layout wh (DATA(l)) true = 
	    dataitem_layout wh 
	    (INT32 (i2w(label_to_address (!label_table) l))) true
	  | dataitem_layout wh (ARRAYP(0,_)) _ = 0
	  | dataitem_layout wh (ARRAYP(n,l)) false = 4 * n
	  | dataitem_layout wh (ARRAYP(n,l)) true = 
	    let val data = 
		     case l
		     of TAG i => INT32 (i)
		      | PTR l => INT32(i2w(label_to_address (!label_table) l))
		val sz = dataitem_layout wh data true
	    in  sz + (dataitem_layout (wh+sz) (ARRAYP(n-1,l)) true)
	    end
	  | dataitem_layout wh (DLABEL _) _ = 0
	  | dataitem_layout wh (ALIGN (LONG)) _ = (4 - (wh mod 4)) mod 4
	  | dataitem_layout wh (ALIGN (QUAD)) _ = (8 - (wh mod 8)) mod 8
	  | dataitem_layout wh (ALIGN (OCTA)) _ = (16 - (wh mod 16)) mod 16
	  | dataitem_layout wh (ALIGN (ODDLONG)) _ = 
	      let val t = wh mod 8
	      in if t=4 then 0
		 else if t<4 then 4-t
		 else 12-t  (* (8-t)+4 --- align to quadword, add 4*)
	      end
	  | dataitem_layout wh (ALIGN (ODDOCTA)) _ = 
	      let val t = wh mod 16
              in if t=12 then 0
                 else if t<12 then 12-t
                 else 12+(16-t)   (* align to octaword, add 12 *)
              end
	  | dataitem_layout wh (STRING (s)) _ = 
		let 
		  val moresize = (4 - (size s) mod 4) mod 4;
		  val more = String.substring("\000\000\000",0,moresize);
		  fun loop wh [] = 0
		    | loop wh (a::more) = 
		      (H.storebyte(i2w wh,ord a); 
		       1 + (loop (wh+1) more))
		in loop wh (explode (s ^ more))
                end
	fun data_layout pos dataarray finalFlag = 
	  let val sz = Array.length dataarray
	  in if (pos >= sz) 
		then ()
	     else let 
		    val d = Array.sub(dataarray,pos)
		    val curlocation = !location
		    val size = dataitem_layout curlocation d finalFlag
		    val _ = add_location(size)
		  in 
		    ((case d of
			(DLABEL (l)) => 
			  if (finalFlag) then ()
			    else (label_table := 
				  (l,curlocation)::(!label_table))
		      | _  	     => ());
			data_layout (pos + 1) dataarray finalFlag)
		  end
	  end
	fun read [] = () 
	  | read ((a,aa)::b) = 
	     (print ((label2s a) ^ " " 
		     ^ (Int.toString aa) ^ "\n"); read b);

	val _ = location := actualstart
	val _ = app (fn MODULE{data,...} => data_layout 0 data false) modules
        val _ = let val temp = !location + 7
		in location := (temp div 8) * 8
		end;
	val _ = codebegin := !location

	val _ = app (fn MODULE{procs,...} => code_layout procs) modules
        val _ = let val temp = !location + 7
		in location := (temp div 8) * 8
		end;

	val savelocation = !location
	val _ = location := actualstart
	val _ = app (fn MODULE{data,...} => data_layout 0 data true) modules
	val _ = location := savelocation
	val _ = if (!print_dump)
		  then (print "*****************ALL LABELS LAID OUT -- START ********\n";
			read (!label_table);	
			print "*****************ALL LABELS LAID OUT -- END **********\n")
		else ()
      in  !location
      end;

(* ------------------------- a few helpers -------------------- *)
fun sv2val(IMM i) = (wzero,i2w i)
  | sv2val(REG ri) = lookupval_ireg(ri);

fun trap() = 
    if O.get_exn() = O.OKAY then () else error "hardware traps unimplemented"



    fun lpack (a,b) = LONGS(WORD a, WORD b);
    val four = (wzero,i2w 4)
    val eight = (wzero,i2w 8)

(* ----------------- the big enchilada  --------------------------- *)

    (* ------ metastep handles call to predefine C code in a single step ---------- *)    
    val instream_table : (int * TextIO.instream) list ref = ref([(0,TextIO.stdIn)])
    val outstream_table : (int * TextIO.outstream) list ref = 
      ref([(2,TextIO.stdErr),(1,TextIO.stdOut)])
    val stream_counter = ref 3
    fun new_stream_id() = (stream_counter := !stream_counter + 1;
			   !stream_counter)
    fun add_in (i,s) = instream_table := (i,s) :: (!instream_table)
    fun add_out (i,s) = outstream_table := (i,s) :: (!outstream_table)
    exception FindInt
    fun find_int(i:int,args) = 
      let fun f((j,x)::rest) = if (i=j) then x else f rest
	    | f [] = raise FindInt
      in
	f args
      end
    fun find_in i = (find_int(w2i i,!instream_table) 
		     handle FindInt =>
		       crash "bad instream")
    fun find_out i = (find_int(w2i i,!outstream_table) 
		      handle FindInt =>
			crash "bad outstream")

    fun destring (r) :string =
      let 
	val tag = H.lookuplong(W.uminus(r,0w4))
	val size = w2i(W.rshiftl(tag,3))
	val numword = (size+3) div 4;
	fun loop i = if (i >= numword) then ""
	  else 
	    let val lon = H.lookuplong(W.uplus(r,i2w(4*i)))
		val disps = [0,8,16,24]
		fun doer disp = chr(w2i(W.andb(W.rshiftl(lon,disp),i2w 255)))
		val temp = implode(map doer disps)
	    in temp ^ (loop (i + 1))
	    end
      in
	String.substring(loop 0,0,size)
      end
    val heapptr = SREGI HEAPPTR
    fun alloc_string(str : string) = 
      let 
	val len = size str
	val s = str ^ "\000\000\000\000";
	val numword = (len+3) div 4;
	val totalnumword = numword + 1;
	val curheap = getlowival(heapptr)
	val newheap = W.uplus(curheap,i2w(4 * totalnumword))
	(* number of bytes in string to copy *)
	val numbytes = numword * 4
	(* base of array *)
        val base = W.uplus(curheap,i2w 4)
        fun fill i =
	       if i<numbytes then
		     (H.storebyte(W.uplus(base,i2w i),ord(String.sub(s,i)));
		      fill(i+1))
	       else ()
	val arraytag = Rtltags.intarraytag (i2w numword)
	val _ = H.storelong(curheap,arraytag)
	val _ = fill 0
	val finalheap = W.uplus(newheap,i2w (3 * 4));
        val recordtag = case (Rtltags.recordtag([NOTRACE_INT,TRACE])) of
	    [{static=recordtag,...}] => recordtag
	  | _ => error "recordtag misbehaving"
	val _ = H.storelong(W.uplus(newheap,i2w 0),recordtag)
	val _ = H.storelong(W.uplus(newheap,i2w 4),i2w len)
	val _ = H.storelong(W.uplus(newheap,i2w 8),W.uplus(curheap,i2w 4))
	val _ = setireg(heapptr,lpack(wzero,finalheap));
      in
	W.uplus(newheap,i2w 4)
      end
    val unit = lpack(wzero,wzero)

    local
      val buffer = ref ""
    in
      fun clear_output() = buffer := ""
      fun add_output(s) = buffer := ((!buffer) ^ s)
      fun get_output() = !buffer
    end


    fun metastep({func=(C_EXTERN_LABEL name),
		  return,args,results,tailcall,save}) = 
      (case (name,args,results) of
	 ("ml_output",([ri,rj],[]),([dest],[])) =>
	   let
	     fun getregnum (REGI (v,_)) = Name.var2int v
	       | getregnum (SREGI HEAPPTR) = 0
	       | getregnum (SREGI HEAPLIMIT) = 1
	       | getregnum (SREGI STACKPTR) = 2
	       | getregnum (SREGI EXNPTR) = 3
	       | getregnum (SREGI EXNARG) = 4
	     val rinum = getregnum ri
	     val rjnum = getregnum rj
	     val (rival,rjval) = (getlowival(ri),getlowival(rj))
	     val msg = destring rjval
	     val stream = find_out rival
	     val _ = if (rival = (i2w 1))
		       then add_output(msg)
		     else ()
	   in TextIO.output(stream,msg);
	      setireg(dest,unit)
	   end
       | ("ml_input",([ri,rj],[]),([dest],[])) => 
	   let val (rival,rjval) = (getlowival(ri),getlowival(rj))
	     val res = alloc_string(TextIO.inputN(find_in rival,w2i rjval))
	   in setireg(dest,lpack(wzero,res))
	   end
       | ("ml_lookahead",([ri],[]),([dest],[])) => 
	   let val rival = getlowival(ri)
	       val res = alloc_string(case (TextIO.lookahead (find_in rival)) of
					  SOME c => implode[c]
					| NONE => "")
	   in setireg(dest,lpack(wzero,res))
	   end
       | ("ml_open_in",([ri],[]),([ret],[])) => 
	   let val rival = getlowival(ri)
	     val i = new_stream_id()
	     val st = TextIO.openIn(destring rival)
	   in add_in(i,st);
	     setireg(ret,lpack(wzero,i2w i))
	   end
       | ("ml_open_out",([ri],[]),([ret],[])) => 
	   let val rival = getlowival(ri)
	     val i = new_stream_id()
	     val st = TextIO.openOut(destring rival)
	   in add_out(i,st);
	     setireg(ret,lpack(wzero,i2w i))
	   end
       | ("ml_close_in",([ri],[]),([dest],[])) => 
	        (TextIO.closeIn (find_in (getlowival ri));
		 setireg(dest,unit))
       | ("ml_close_out",([ri],[]),([dest],[])) => 
	        (TextIO.closeOut (find_out (getlowival ri));
		 setireg(dest,unit))
       | ("ml_flush_out",([ri],[]),([dest],[])) => 
	         (TextIO.flushOut (find_out (getlowival ri));
		  setireg(dest,unit))
       | ("ml_end_of_stream",([ri],[]),([ret],[])) => 
	   let val id = find_in (getlowival ri)
	     val res = if (TextIO.endOfStream id) then 1 else 0
	   in  setireg(ret,lpack(wzero,i2w res))
	   end
       | ("ml_spill_vararg",_,_) => error "extern: spill_vararg"
       | _ => crash ("no such extern " ^ name))
      | metastep _ = error "no such metastep"

    fun step (LI(v,ri)) =  setireg(ri,LONGS(WORD wzero, WORD v))
      | step (LADDR(lab,offset,ri)) = 
          setireg(ri,lpack(wzero,i2w(offset + (label_to_address (!label_table) lab))))
      | step (LEA(ea,ri)) = setireg(ri,lpack(wzero,ea_to_val(ea,1)))
      | step (CMV(cmp,a,b,dest)) = 
	let 
	  val lowa = getlowival a;
	  val res = (O.cmpis_to_fun cmp)(lowa,wzero)
	in  if res then setireg(dest,lpack(sv2val(b))) else ()
	end
      | step (MV(rsrc,rdest)) = 
	(setireg(rdest,getireg(rsrc))
	 handle Div => (print "MV caused a div\n"; raise Div))
      | step (FMV(rsrc,rdest)) = update_freg(rdest,getfreg(rsrc))
      | step (ADD(a,b,dest)) = setireg(dest,lpack(O.plusop(getival(a),sv2val(b),false)))
      | step (SUB(a,b,dest)) = setireg(dest,lpack(O.minusop(getival(a),sv2val(b),false)))
      | step (MUL(a,b,dest)) = setireg(dest,lpack(O.multop(getival(a),sv2val(b),false)))
      | step (DIV(a,b,dest)) = setireg(dest,lpack(O.divop(getival(a),sv2val(b),false)))
      | step (MOD(a,b,dest)) = 
	let val q = O.divop(getival(a),sv2val(b),false)
	  val res = O.minusop(getival(a),O.multop(q,sv2val(b),false),false)
	in setireg(dest,lpack(res))
	end
      | step (ADDT(a,b,dest)) = setireg(dest,lpack(O.plusop(getival(a),sv2val(b),true)))
      | step (SUBT(a,b,dest)) = setireg(dest,lpack(O.minusop(getival(a),sv2val(b),true)))
      | step (MULT(a,b,dest)) = setireg(dest,lpack(O.multop(getival(a),sv2val(b),true)))
      | step (DIVT(a,b,dest)) = setireg(dest,lpack(O.divop(getival(a),sv2val(b),true)))
      | step (MODT(a,b,dest)) = 
	let val q = O.divop(getival(a),sv2val(b),true)
	  val res = O.minusop(getival(a),O.multop(q,sv2val(b),true),true)
	in setireg(dest,lpack(res))
	end
      | step (S4ADD(a,b,dest)) = setireg(dest,lpack(O.plusop(O.multop(getival(a),four,false),
							     sv2val(b),false)))
      | step (S8ADD(a,b,dest)) = setireg(dest,lpack(O.plusop(O.multop(getival(a),eight,false),
							     sv2val(b),false)))
      | step (S4SUB(a,b,dest)) = setireg(dest,lpack(O.minusop(O.multop(getival(a),four,false),
							     sv2val(b),false)))
      | step (S8SUB(a,b,dest)) = setireg(dest,lpack(O.minusop(O.multop(getival(a),eight,false),
							     sv2val(b),false)))
      | step (CMPUI(cmp,a,b,dest)) = 
	let 
	  val lowa = getlowival a;
	  val lowb = case (sv2val(b)) of (x,y) => y
	  val res = (O.cmpiu_to_fun cmp)(lowa,lowb)
	in  setireg(dest,lpack(O.bool_to_ireg_val(res)))
	end
      | step (CMPSI(cmp,a,b,dest)) = 
	let 
	  val lowa = getlowival a;
	  val lowb = case (sv2val(b)) of (x,y) => y
	  val res = (O.cmpis_to_fun cmp)(lowa,lowb)
	in  setireg(dest,lpack(O.bool_to_ireg_val(res)))
	end
      | step (NOTB(a,dest)) = setireg(dest,lpack(O.notop(getival(a))))
      | step (ANDB(a,b,dest)) = setireg(dest,lpack(O.andop(getival(a),sv2val(b))))
      | step (ORB(a,b,dest)) = setireg(dest,lpack(O.orop(getival(a),sv2val(b))))
      | step (XORB(a,b,dest)) = setireg(dest,lpack(O.xorop(getival(a),sv2val(b))))
      | step (SRA(a,b,dest)) = setireg(dest,lpack(O.sraop(getival(a),sv2val(b))))
      | step (SRL(a,b,dest)) = setireg(dest,lpack(O.srlop(getival(a),sv2val(b))))
      | step (SLL(a,b,dest)) = setireg(dest,lpack(O.sllop(getival(a),sv2val(b))))
      | step (FADDD(a,b,dest)) = update_freg(dest,QFLOAT ((getfval a + getfval b) handle Overflow => (O.raise_overflow(); 1.0)))
      | step (FSUBD(a,b,dest)) = update_freg(dest,QFLOAT ((getfval a - getfval b) handle Overflow => (O.raise_overflow(); 1.0)))
      | step (FMULD(a,b,dest)) = update_freg(dest,QFLOAT((getfval a * getfval b) handle Overflow => (O.raise_overflow(); 1.0)))
      | step (FDIVD(a,b,dest)) = update_freg(dest,QFLOAT((getfval a / getfval b) handle Div => (O.raise_dividezero(); 1.0)))

      | step (FABSD(a,dest)) = update_freg(dest,QFLOAT(abs(getfval(a))))
      | step (FNEGD(a,dest)) = update_freg(dest,QFLOAT(~(getfval(a))))
      | step (CVT_INT2REAL(rf,dest)) = 
	let val lons = 
	    case lookup_ireg(rf) of
		LONGS(WORD a, WORD b) => (a,b)
	      | _ => error "step: FCVTI"
	in update_freg(dest,QFLOAT(O.long_to_real(lons)))
	end
      | step (SQRT(a,dest)) = update_freg(dest,QFLOAT(Math.sqrt(getfval(a))))
      | step (CVT_REAL2INT(a,dest)) = update_ireg(dest,lpack(wzero,i2w(floor(getfval(a)))))
      | step (SIN(a,dest)) = update_freg(dest,QFLOAT(Math.sin(getfval(a))))
      | step (COS(a,dest)) = update_freg(dest,QFLOAT(Math.cos(getfval(a))))
      | step (ARCTAN(a,dest)) = update_freg(dest,QFLOAT(Math.atan(getfval(a))))
      | step (EXP(a,dest)) = update_freg(dest,QFLOAT(Math.exp(getfval(a))))
      | step (LN(a,dest)) = update_freg(dest,QFLOAT(Math.ln(getfval(a))))

      | step (CMPF(cmp,a,b,dest)) = 
	let val res = (O.cmpf_to_fun cmp)(getfval(a),getfval(b))
	in  update_ireg(dest,lpack (O.bool_to_freg_val(res)))
	end
      | step (BR(label)) = pc_label_change (LOCAL_LABEL label)
      | step (BCNDI2 _) = error "unimplemented: BCNDI2"
      | step (BCNDF2 _) = error "unimplemented: BCNDF2"
      | step (BCNDI(cmp,ri,label,predict)) = 
	let val (a,b) = (getlowival(ri),wzero)
        in
	if ((O.cmpis_to_fun cmp) (a,b))
	  then pc_label_change (LOCAL_LABEL label)
	else ()
	end
      | step (BCNDF(cmp,ri,label,predict)) = 
	if ((O.cmpf_to_fun cmp) (getfval(ri),0.0))
	  then pc_label_change (LOCAL_LABEL label)
	else ()
      | step (JMP(ri,_)) = pc_abs_change (getlowival ri)
      | step (LOAD32I(ea,ri)) = 
	setireg(ri,LONGS(WORD wzero,WORD (H.lookuplong(ea_to_val(ea,4)))))
      | step (STORE32I(ea,ri)) = H.storelong(ea_to_val(ea,4),getlowival(ri))
      | step (LOADQF(ea,rf)) = update_freg(rf,H.lookupquad(ea_to_val(ea,8)))
      | step (STOREQF(ea,ri)) = H.storequad(ea_to_val(ea,8),getfreg(ri))
      | step (NEEDMUTATE(addr)) = ()
      | step (NEEDGC(sz)) = ()
      | step (FLOAT_ALLOC(sz,v,dest,ptag)) = error "float_alloc not done for rtlinterp"
      | step (INT_ALLOC(sz,v,dest,ptag)) = error "int_alloc not done for rtlinterp"
      | step (PTR_ALLOC(sz,v,dest,ptag)) = error "ptr_alloc not done for rtlinterp"
      | step (IALIGN _) = error "alignment not done for rtlinterp"
      | step (ILABEL _) = ()
      
(* we can ignore tail calls for now, they have branches to the right place 
 except the old return address was overwritten so we have to restore it *)
      | step (CALL (blob as {func,return=local_return,args,
		results,tailcall,save=SAVE save_caller_without})) = 
	let 
	  val call_add = (case func of
			      REG' r => getlowival(r)
			    | LABEL' l => i2w(label_to_address (!label_table) (l)))
	  fun regular_call () =
	      let val call_label = 
		  case func of
		      REG' r => address_to_label (!label_table) (w2i call_add)
		    | LABEL' l => l
		  val callee as PROC{args=formal,return,save=SAVE save_callee,...} = 
		      find_proc(call_label)
		  val save_caller = save_caller_without
		  val newblob = blob
		      
		  val save_caller' = R.register_save(save_caller)
		  val save_callee' = R.register_save save_callee
		  val _ = R.register_parmove args formal
		  val ret_add = lpack(wzero,i2w(!pc + 4))
		  val _ = setireg(return, ret_add)
		  val _ = push_stack(newblob,callee,save_caller',save_callee')
		  val _ = pc_abs_change call_add
	      in ()
	      end
	in if (W.ugte(call_add,0w1000000))
	       then let
		     val _ =  metastep{func = address_to_label (!label_table) (w2i call_add),
				 return=local_return,
				 args=args,
				 results = results,
				 tailcall = tailcall,
				 save = SAVE save_caller_without}
		    in  ()
		    end
	   else regular_call()
	end
      | step (RETURN ret_reg) = 
	let 
	  val ret_add = getlowival(ret_reg)
          val (caller,self,saved_val,save_callee') = pop_stack()
	  val {save=save_abs,results=result_formal,...} = caller
	  val SAVE save = save_abs;
	  val PROC{results=result_actual,save=SAVE save_callee,...} = self
	  val retvals = R.register_save result_actual
	  val _ = R.register_restore save_callee save_callee'
	  val _ = R.register_restore save saved_val
	  val _ = R.register_restore result_formal retvals
	  val _ = pc_abs_change ret_add
(*
	  val pr = pp_Save' o SAVE
          val _ = print ("moving " ^ (pr save_callee) ^ " back\n");
          val _ = print ("moving " ^ (pr result_actual) ^ " to " ^
			(pr result_formal) ^ "    ret_add is "
			^ (Int.toString (w2i ret_add)) ^ "\n")
          val _ = print ("moving " ^ (pr save) ^ " back\n");
*)
        in ()
        end
      | step (SOFT_VBARRIER _) = trap()
      | step (SOFT_ZBARRIER _) = trap()
      | step (HARD_VBARRIER _) = trap()
      | step (HARD_ZBARRIER _) = trap()
     | step HANDLER_ENTRY = ()
      | step (SAVE_CS _) =
	      (push_exn_stack (copy_all_stack()))
      | step END_SAVE = 
	      (pop_exn_stack(); ())
      | step RESTORE_CS = 
	      (replace_all_stack (pop_exn_stack()))
      | step HALT = raise RTL_HALTED


    val instrcount = ref 0;
    val heapstart = ref 0;
    fun showheap() =
      let
	fun foo(cur,limit) = 
	  H.showheap(i2w cur,(limit-cur+4)div 8);
      in 
	(foo(!heapstart,(w2i(getlowival(heapptr))) + 20);
	 print("\n");
	 foo(actualstart,!codebegin))
      end
    
    fun run() = 
      let 
	val curpc = !pc
	val instr = H.lookupinstr(i2w curpc);
	val iscallret =  (case instr of
			      CALL _ => true
			    | RETURN _ => true
			    | _ => false)
	val dumpregs = (!print_dump andalso !instrcount >= !bp) 
	    orelse (!print_dump_callret andalso iscallret)
	(* NOTE that step assumes that the pc had not been advanced yet in CALL *)
	val isexn = case instr of
	    SAVE_CS _ => true
	  | END_SAVE => true
	  | RESTORE_CS => true
	  | _ => false
	val _ = if (!print_trace)
		    (*	  orelse isexn orelse iscallret) *)
		    then
			(print ("\n" ^ (Int.toString (!instrcount)) ^ 
				"  PC=" ^ (Int.toString (curpc)) ^ "  " ^ 
				(R.iword2str (INSTR instr)) ^ 
				(if isexn then "EXN instr" else "") ^ "\n"))
		else ()
        val stop = ((step instr; instrcount := (!instrcount) + 1; false)
	   handle RTL_HALTED => true
		| e => crash "exn raised in RTL interp")

	val _ = if (dumpregs)
(* orelse isexn orelse iscallret) *)
(*	               (iscallret andalso (!instrcount > 1000))) *)
then 
	  (print "-----------------------------------\n";
	   R.showreg_nonzero();
	   print "-----------------------------------\n"
	   (* showheap() *)
	   )
		else ()
	val _ = if (pc_look_changed()) then () else (pc := curpc + 4)
	val _ = pc_unset_changed()
	val _ = if ((!instrcount mod 10000) = 0) 
		  then print (Int.toString(!instrcount) ^ " instructions executed.\n") else ()
      in if stop then (raise RTL_HALTED) else run()
      end;
      
    val badvar = Name.fresh_named_var "rtlinterp_badvar"
    fun do_module (callval,tf) ((name : string,MODULE{procs,data,main,...})) = 
      let 
	val proc as PROC({args,results,return=main_return,...}) = find_proc(LOCAL_LABEL main)
	val _ = setireg(main_return,lpack(wzero,i2w haltpc))
	val _ = R.register_restore args callval;
	local
	  val badreg : regi = REGI(badvar,NOTRACE_INT);
	  val badcall = {func=((REG' badreg) : reg_or_label),
			 return=NONE,
			 args=(nil,nil),results=(nil,nil),
			 tailcall=false,save=((SAVE(nil,nil)) : save)} 
	    : callblob
	  val stopframe = (badcall,proc,(nil,nil),(nil,nil));
	in
	  val _ = push_stack(stopframe)
	end
        val _ = (pc := (label_to_address (!label_table) (LOCAL_LABEL main)))
	val _ = clear_output()
        val answer = run()
		handle RTL_HALTED => R.register_save results;
      in  
        answer
      end;

    fun RTL_interp (modules, callval, tf) = 
      let 
        val _ = (H.reset_heap(); R.reset_register(); cur_module := map #2 modules;
		 instrcount := 0; O.reset_exn())
        val heapbase = RTL_layout (map #2 modules)

        (* it is valid (but uninteresting) to interpret 0 modules *)

	val _ = heapstart := heapbase
        val _ = (R.update_ireg (heapptr,
				R.LONGS (R.WORD (i2w 0),
					 R.WORD(i2w heapbase)));
	         clear_output();
		 print_dump := tf)
	val _ = if (!print_dump)
	  then (print "INITIAL STATE\n"; R.showreg())
		else ()
	val answers = map (do_module (callval,tf)) modules
	val _ = print ("\nRtl interpreter: " ^ (Int.toString (!instrcount)) ^ 
		       " instructions executed\n")

        val _ = print "\nOutput was:-------------BEGIN--------------\n"
	val _ = print (get_output())
	val _ = 
	  case (!outstream) of
	    NONE => ()
	  | SOME s => TextIO.output(s,get_output())
        val _ = print "Output was:-----------END------------------\n\n"
	val _ = cur_module := nil
      in  
        answers
      end;


end;
