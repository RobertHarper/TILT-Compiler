(* empty records translate to 256; no allocation *)
functor Tortl(structure Rtl : RTL
	      structure Pprtl : PPRTL 
	      structure Rtltags : RTLTAGS 
	      structure Nil : NIL
	      structure NilUtil : NILUTIL
	      structure Ppnil : PPNIL
	      sharing Ppnil.Nil = NilUtil.Nil = Nil
	      sharing Pprtl.Rtltags = Rtltags
	      sharing Rtl = Pprtl.Rtl)
    : TORTL =
struct

val debug = ref false

    exception XXX
fun simplify_type _ = raise XXX
   (* Module-level declarations *)

    structure Rtl = Rtl
    structure Nil = Nil
    open Util
    open Nil
    open NilUtil
    open Rtl
    open Name
    open Rtltags 
    open Pprtl 
    type label = Rtl.label


    val exncounter_label = ML_EXTERN_LABEL "exncounter"
    val error = fn s => (error "tortl.sml" s)
    structure W32 = TilWord32
    structure W64 = TilWord64


   (* ------------------ Overall Data Structures ------------------------------ *)
      
   type translate_params = { HeapProfile : int option, do_write_list : bool, 
                             codeAlign : Rtl.align, FullConditionalBranch : bool, 
                             elim_tail_call : bool, recognize_constants : bool }
   val cur_params = ref { HeapProfile = NONE : int option, 
			 do_write_list = true,
			 codeAlign = Rtl.OCTA, FullConditionalBranch = false, 
			 elim_tail_call = true, recognize_constants = true}
    
   (* A NIL variable is represented at the RTL level as one of:
     (1) a local RTL register
     (2) a global variable 
     (3) a label (either local or extern)
     (4) a code label
     In addition, in some cases, we know that the value of the variable is:
     (1) a constant integer
     (2) a constant real
     (3) a constant record which is always laid out at the given label
           Note that it is the responsibility of the caller of the constructor
	     to lay out the record.
           Components are identified by the variable names.
  	   Perform a lookup to determine if the subcomponents are constants or not.
   *)

   datatype reg = I of Rtl.regi | F of Rtl.regf
   datatype var_loc = VREGISTER of reg 
		    | VGLOBAL of label * rep
		    | VLABEL of label
		    | VCODE of label
   and var_val = VINT of TilWord32.word
               | VREAL of string
               | VRECORD of label * var list
   type var_rep = var_loc * var_val option * con
   type convar_rep = var_loc * kind
   datatype loc_or_val = VAR_LOC of var_loc
                       | VAR_VAL of var_val
   type varmap = (var,var_rep) HashTable.hash_table
   type convarmap = (var,convar_rep) HashTable.hash_table
(* XXX add empty record to data segment *)
   val unitlabel = LOCAL_LABEL(LOCAL_DATA(fresh_named_var "emptyrecord"))
   val unitval = (VAR_VAL(VRECORD(unitlabel,[])), Prim_c(Record_c[],[]))

  (* ----- Global data structures ------------------------------
   dl: list of data for module
   pl: list of procedures for the entire module
   gvarmap: how a top-level NIL variable is represented at the RTL level
   gconvarmap: how a top-level NIL type variable is represented at the RTL level
   varmap: how a NIL variable is represented at the RTL level
   convarmap: how a NIL typevariable is represented at the RTL level
   mutable_objects : objects in global data area that can point to heap objects
                     (e.g.) pointer arrays
   mutable_variables : global variables that may contain pointers to heap objects
   ------------------------------------------------------------- *)

   exception NotFound
   val dl : Rtl.data list ref = ref nil
   val pl : Rtl.proc list ref = ref nil
   val gvarmap: varmap ref = ref (mk_var_hash_table(128,NotFound))
   val gconvarmap: convarmap ref = ref (mk_var_hash_table(128,NotFound))
   val varmap: varmap ref = ref (mk_var_hash_table(128,NotFound))
   val convarmap: convarmap ref = ref (mk_var_hash_table(128,NotFound))
   val mutable_objects : label list ref = ref nil
   val mutable_variables : (label * rep) list ref = ref nil
   fun add_proc p = pl := p :: !pl
   datatype work = FunWork of (var * function)
                 | ConFunWork of (var * (var * kind) list * con * kind)
   val worklist : work list ref = ref nil
   fun addWork more = (print "addwork called\n";
		       worklist := (more :: (!worklist)))
   fun getWork() = (case (!worklist) of
			 [] => NONE
		       | (a::b) => (worklist := b; SOME a))

  (* ---- Looking up and adding new variables --------------- *)
    
  fun getconvarrep v = 
      (case HashTable.find (!convarmap) v of
	   NONE => (case HashTable.find (!gconvarmap) v of
			NONE => error ("getconvarrep: variable "^(var2string v)^" not found")
		      | SOME convar_rep => convar_rep)
	 | SOME convar_rep => convar_rep)
  fun getrep v = 
      (case HashTable.find (!varmap) v of
	   NONE => (case HashTable.find (!gvarmap) v of
			NONE => 
			    (HashTable.appi (fn (v,_) => (pp_var v; print "\n")) (!varmap);
			     print "!varmap has "; print (Int.toString (HashTable.numItems (!varmap)));
			     print " items\n!convarmap has "; 
			     print (Int.toString (HashTable.numItems (!convarmap)));
			     print " items\n";
			     error ("getrep: variable "^(var2string v)^" not found"))
		      | SOME rep => rep)
	 | SOME rep => rep)


  fun alloc_named_code_label s = LOCAL_CODE(fresh_named_var s)
  fun alloc_named_data_label s = LOCAL_DATA(fresh_named_var s)
  fun alloc_named_local_code_label s = LOCAL_LABEL(alloc_named_code_label s)
  fun alloc_named_local_data_label s = LOCAL_LABEL(alloc_named_data_label s)
  fun alloc_code_label () = LOCAL_CODE(fresh_var())
  fun alloc_data_label () = LOCAL_DATA(fresh_var())
  fun alloc_local_code_label () = LOCAL_LABEL(alloc_code_label())
  fun alloc_local_data_label () = LOCAL_LABEL(alloc_data_label())

  val codeAlign = ref (Rtl.OCTA)
  fun do_code_align() = () (* add_instr(IALIGN (!codeAlign)) *)

  (* Takes a constructor and returns the RTL representation.
     The head-normal form must be statically known. That is, this constructor
     must not involve any computation to determine the RTL representation. *)
   fun con2rep con : rep = 
       let fun primcon2rep (pcon,clist) = 
	   case (pcon,clist) of
	       (Int_c _,_) => NOTRACE_INT
	     | (Float_c Prim.F32,_) => error "32-bit floats not supported"
	     | (Float_c Prim.F64,_) => NOTRACE_REAL
	     | (((BoxFloat_c _) | Exn_c | Array_c | Vector_c | Ref_c | Exntag_c | 
		 (Sum_c _) | (Record_c _) | (Vararg_c _)),_) => TRACE
       in case con of
	   Prim_c(pcon,clist) => primcon2rep(pcon,clist)
	 | Mu_c (vc_seq,v) => TRACE (* XXX con2rep(NilUtil.muExpand(vc_seq,v)) *)
	 | AllArrow_c (Open,_,_,_,_,_) => error "no open lambdas allowed by this stage"
	 | AllArrow_c(Closure,_,_,_,_,_) => TRACE
	 | AllArrow_c(Code,_,_,_,_,_) => NOTRACE_CODE
	 | Var_c v => (case (#1(getconvarrep v)) of
			   (VREGISTER (I r)) => COMPUTE(Var_p r)
			 | (VREGISTER (F _)) => error "constructor in float reg"
			 | (VGLOBAL (l,r)) => COMPUTE(Label_p l)
			 | (VLABEL l) => COMPUTE(Label_p l)
			 | (VCODE _) => NOTRACE_CODE)
	 | (Proj_c (Var_c v,l)) => 
	       (case (getconvarrep v) of
		    (vl,Record_k fields_seq) => 
			let fun loop acc [] = error "bad Proj_c"
			      | loop acc (((l',_),_)::rest) = 
				if (eq_label(l,l')) then acc else loop (acc+1) rest
			    val fields_list = sequence2list fields_seq
			    val i = loop 0 fields_list
			in  case vl of
			    (VREGISTER (I ir)) => COMPUTE(Projvar_p(ir,i))
			  | (VREGISTER (F _)) => error "constructor in float reg"
			  | (VGLOBAL (l,r)) => COMPUTE(Projlabel_p(l,i))
			  | (VLABEL l) => COMPUTE(Projlabel_p(l,i))
			  | (VCODE _) => error "constructor is a code pointer"
			end
		  | _ => error "Proj_c from constructuctor of non-record kind")
	 | (Proj_c _) => error "Proj_c from non-variable not in head-normal form"
	 | (Let_c _) => error "Let_c not in head-normal form"
	 | (Crecord_c _) => error "Crecord_c not a type"
	 | (Closure_c _) => error "Closure_c not a type"
	 | (App_c _) => error "App_c not in head-normal form"
	 | (Annotate_c (_,c)) => con2rep c
       end

   fun varloc2rep varloc =
       (case varloc of
	    VLABEL _ => LABEL
	  | VCODE _ => NOTRACE_CODE
	  | VREGISTER reg =>
		(case reg of
		     F (REGF(_,frep)) => frep
		   | I (REGI(_,irep)) => irep
		   | I (SREGI _) => error "tracetable_value on SREG")
	  | VGLOBAL (_,rep) => rep)
	    
  fun varval2rep varval =
      (case varval of
	   VINT _ => NOTRACE_INT
	 | VREAL _ => NOTRACE_REAL
	 | VRECORD _ => TRACE)

  fun valloc2rep (VAR_VAL varval) = varval2rep varval
    | valloc2rep (VAR_LOC varloc) = varloc2rep varloc

  (* ----- Data structures for the current function being compiled 
   il: list of instructions for the current function being compiled
   localreg{i/f} : computing SAVE sets for RTL interpreter 
   top: label at top of current function (past prelude code)
   currentfun: the current function being compiled
   argreg{i/f}: the argument registers  
   ---> If top and currentfun and NONE, then we are at top-level 
        and the register lists will also be empty.   <--- *)

   local
       val istop : bool ref = ref false
       val top : local_label ref = ref (alloc_code_label())
       val currentfun : local_label ref = ref (alloc_code_label())
       val resultreg : reg ref = ref (F(REGF(fresh_named_var "badreg",NOTRACE_REAL)))
       val il : Rtl.instr list ref = ref nil
       val localregi : regi list ref = ref nil
       val localregf : regf list ref = ref nil
       val argregi : regi list ref = ref nil
       val argregf : regf list ref = ref nil
   in  
       fun istoplevel() = !istop
       fun getTop() = !top
       fun getCurrentFun() = !currentfun
       fun getResult() = !resultreg
       fun getLocals() = (!localregi, !localregf)
       fun getArgI() = !argregi
       fun getArgF() = !argregf

       fun add_data d = dl := d :: !dl
       fun add_instr i = il := i :: !il

       fun alloc_regi (traceflag) = 
	   let val r = REGI(fresh_var(),traceflag)
	   in localregi := r :: (!localregi);
	       r
	   end
       
       fun alloc_regf () = 
	   let val r = REGF(fresh_var(),NOTRACE_REAL)
	   in localregf := r :: (!localregf);
	       r
	   end
       
       fun alloc_named_regi v traceflag = 
	   let val r = REGI (v,traceflag)
	   in localregi := r :: (!localregi);
	       r
	   end
       
       fun alloc_named_regf v = 
	   let val r = REGF (v,NOTRACE_REAL)
	   in localregf := r :: (!localregf);
	       r
	   end
       
       fun alloc_reg c = 
	   case c of (* might need to normalize *)
	       Nil.Prim_c(Float_c _,[]) => F(alloc_regf())
	     | _ => I(alloc_regi (con2rep c))
		   
       fun alloc_named_reg (c,v : var) = 
	   case c of
	       Nil.Prim_c(Float_c _,[]) => F(alloc_named_regf v)
	     | _ => I(alloc_named_regi v (con2rep c))
		   
       fun insert map (key,v) = (case (HashTable.find (map) key) of
				     NONE => HashTable.insert (map) (key,v)
				   | SOME _ => error "hash table already contains entry")

       fun promote_maps() = 
	   (print "promote maps called\n";
	    HashTable.appi (fn (k,v) => (insert (!gvarmap) (k,v))) (!varmap);
	    HashTable.appi (fn (k,v) => (insert (!gconvarmap) (k,v))) (!convarmap))

       fun reset_state (is_top,name,(iargs,fargs),result,return) =
	   (dl := nil; 
	    pl := nil; 
	    if (!istop)  (* if we were just in global state, promote its locals *)
		then promote_maps()
	    else ();
	    varmap := mk_var_hash_table(128,NotFound); 
	    convarmap := mk_var_hash_table(128,NotFound); 
	    istop := is_top;
	    mutable_objects := nil;
	    mutable_variables := nil; 
	    
	    currentfun := LOCAL_CODE name;
	    top := alloc_code_label();
	    il := nil; 
	    do_code_align();
	    add_instr(ILABEL (!top));
	    resultreg := result;
	    argregi := iargs;
	    argregf := fargs;
	    localregi := (case result of
			      I ir => ir :: return :: iargs
			    | _ => return :: iargs);
	    localregf := (case result of
			      F fr => fr :: fargs
			    | _ => fargs))
       fun get_state() = {name = !currentfun,
			  revcode = !il}
   end
	   

  (* Context: is the context around an expression the identity
   context, or not the identity context.   The identity context
   carries the register containing the return address for the
   function with it.*)
  
  datatype context = ID of regi | NOTID
    

(* ---------  Helper Functions ------------------------------------------- *)
    val w2i = Word32.toInt
    val i2w = Word32.fromInt;

    local
	val counter = ref 10000
    in
	val HeapProfile = ref false
	fun GetHeapProfileCounter() = !counter
	fun SetHeapProfileCounter(x) = counter := x
	fun MakeProfileTag() = 
	    (counter := (!counter) + 1;
	     if (!counter > 65535) 
		 then error "counter must be stored in 16 bits for heap profile"
	     else ();
		 i2w((!counter) - 1))
    end

  fun in_imm_range_w (w : Word32.word) = in_imm_range (w2i w)
  fun in_imm_range_vl (VAR_VAL(VINT w)) = if in_imm_range_w w then SOME (w2i w) else NONE
    | in_imm_range_vl _ = NONE
  fun in_ea_range scale (VAR_VAL(VINT i)) = 
      if in_ea_disp_range(w2i i * scale)
	  then SOME (w2i i * scale)
      else NONE
    | in_ea_range _ _ = NONE



(*  fun add_code_label(label,s) = add_instr(ILABEL label,s)
  fun add_aligned_code_label(label,s as (_,{params = {codeAlign, ...}, ...}):state) : state = 
      add_instr(ILABEL label, s) (* add_instr(IALIGN codeAlign),s) *) 
      *)



  fun alloc_named_data_label v = LOCAL_DATA v
  fun alloc_named_code_label v = LOCAL_CODE v
 
  (* printing functions *)
  fun regi2s (Rtl.REGI (v,_)) = var2string v
    | regi2s (Rtl.SREGI HEAPPTR) = "HEAPPTR"
    | regi2s (Rtl.SREGI HEAPLIMIT) = "HEAPLIMIT"
    | regi2s (Rtl.SREGI STACKPTR) = "STACKPTR"
    | regi2s (Rtl.SREGI EXNPTR) = "EXNPTR"
    | regi2s (Rtl.SREGI EXNARG) = "EXNARG"

  fun regf2s (Rtl.REGF (v,_)) = var2string v
  fun reg2s (I x) = regi2s x
    | reg2s (F x) = regf2s x

  (* coercing/getting registers *)
    
  fun coercef (r : reg) : regf =
    case r
      of F r => r
    | I r => error("coercef: expected float register, found int register "^regi2s r)
	
  fun coercei str (r : reg) : regi =
    case r
      of I r => r
       | F r => (print "coercei: "; print (str : string) ; print "\n";
		 error("coercei: expected int register, found float register "^regf2s r))
				      

  (* functions for manipulating RTL representations of NIL variables *)
  fun add_var     (v,reg,con)       = insert (!varmap) (v,(VREGISTER reg, NONE, con))
  fun add_var_val (v,reg,con,value) = insert (!varmap) (v,(VREGISTER reg, SOME value, con))
  fun add_convar  (v,regi,kind)     = insert (!convarmap) (v,(VREGISTER (I regi),kind))
  fun add_convar_label (v,l,kind)   = insert (!convarmap) (v,(VLABEL l,kind))
  fun gadd_convar_label (v,l,kind)   = insert (!gconvarmap) (v,(VLABEL l,kind))

  fun add_global     (v,p,con)        = insert (!varmap) (v,(VGLOBAL p, NONE,con))
  fun add_global_val (v,p,con,value)  = insert (!varmap) (v,(VGLOBAL p, SOME value,con))

  fun add_label (v,lab, con) = insert (!varmap) (v,(VLABEL lab, NONE, con))
  fun add_loclabel (LOCAL_CODE v,lab, con) = insert (!varmap) (v,(VCODE lab, NONE, con))
    | add_loclabel (LOCAL_DATA v,lab, con) = insert (!varmap) (v,(VLABEL lab, NONE, con))
  fun gadd_loclabel (LOCAL_CODE v,lab, con) = insert (!gvarmap) (v,(VCODE lab, NONE, con))
    | gadd_loclabel (LOCAL_DATA v,lab, con) = insert (!gvarmap) (v,(VLABEL lab, NONE, con))

  fun add_varloc (v,VREGISTER r,con) = add_var(v,r,con)
    | add_varloc (v,VGLOBAL lr, con) = add_global(v,lr,con)
    | add_varloc (v,VLABEL l, con) = add_label(v,l,con)
    | add_varloc (v,VCODE l, con) = insert (!varmap) (v,(VCODE l, NONE, con))

  fun add_varmap (v,rep,con) = insert (!varmap) (v,(rep,NONE,con))
    
  val heapptr  = SREGI HEAPPTR
  val stackptr = SREGI STACKPTR
  val exnptr   = SREGI EXNPTR
  val exnarg   = SREGI EXNARG

  (* -------------------------- End of Helper Functions ------------------------------- *)


  (* --------- Tag Operation Hlper Functions -------------------- *)

  (* for reals, the len reg measures quads *)

  fun mk_realarraytag(len,tag) = 
    (add_instr(SLL(len,IMM (real_len_offset),tag));
     add_instr(ORB(tag,IMM (w2i realarray),tag)))
    
  fun mk_intarraytag(len,tag) = 
    (add_instr(SLL(len,IMM (int_len_offset),tag));
     add_instr(ORB(tag,IMM (w2i intarray),tag)))
    
  fun mk_ptrarraytag(len,tag) = 
    (add_instr(SLL(len,IMM (int_len_offset),tag));
     add_instr(ORB(tag,IMM (w2i ptrarray),tag)))
    
  fun mk_recordtag(flags) = recordtag(flags);
    
  (* storing a tag *)
    
  fun store_tag_zero tag =
    let val tmp = alloc_regi(NOTRACE_INT)
      in add_instr(LI(tag,tmp));
	 add_instr(STORE32I(EA(heapptr,0),tmp)) (* store tag *)
    end
  
  fun store_tag_disp (disp,tag) =
    let val tmp = alloc_regi(NOTRACE_INT)
    in add_instr(LI(tag,tmp));
      add_instr(STORE32I(EA(heapptr,disp),tmp)) (* store tag *)
    end

  (* ------------- functions for loading NIL values into RTL registers ----------- *)


  (* ---------------------------------------------------------------------------
   the following are functions that load integer registers or float registers
   or load an sv; for int/float regs, one can optionally specify a dest register *)

    (* --- given the RTL representation of a variable, load the
           variable into the optional register i *)
    fun load_ireg_loc (loc : var_loc, poss_dest : regi option) =
      let 
	  fun pickdest rep = (case poss_dest of
				  NONE => alloc_regi rep
				| SOME d => d)
      in case loc of
	  (VLABEL l) => 
	       let val reg = pickdest LABEL
	       in  add_instr(LADDR(l,0,reg));
		   reg
	       end
	| (VCODE l) => 
	       let val reg = pickdest NOTRACE_CODE
	       in  add_instr(LADDR(l,0,reg));
		   reg
	       end
	 | VGLOBAL(l,NOTRACE_REAL) => error "load_ireg called with (VGLOBAL real)"
	 | VGLOBAL(label,rep) =>
	       let val addr = alloc_regi(LABEL)
		   val reg = pickdest rep
	       in  add_instr(LADDR(label,0,addr));
		   add_instr(LOAD32I(EA(addr,0),reg));
		   reg
	       end
	 | VREGISTER (I r) => (case poss_dest of
				   NONE => r
				 | SOME d => (add_instr(MV(r,d)); d))
	 | VREGISTER (F r) => error "moving from float to int register"
      end
    
    fun load_ireg_val (value : var_val, poss_dest : regi option) : regi =
      let 
	  fun pickdest rep = (case poss_dest of
				  NONE => alloc_regi rep
				| SOME d => d)
      in case value of
	  VINT i => let val r = pickdest NOTRACE_INT
			in  add_instr (LI(i,r));
			    r
			end
	| VREAL r => error ("load_ireg: VREAL" ^ r)
	| VRECORD(label,_) => 
	      let val addr = alloc_regi(LABEL)
		  val reg = pickdest TRACE
	      in  add_instr(LADDR(label,0,addr));
		  add_instr(LOAD32I(EA(addr,0),reg));
		  reg
	      end
      end


    fun mk_named_float_data (r : string, label : label) =
	(add_data(ALIGN (ODDLONG));
	 add_data(INT32 (realarraytag (i2w 1)));
	 add_data(DLABEL (label));
	 add_data(FLOAT (r)))
	
    fun mk_float_data ( r : string) : label =
	let 
	    val label = LOCAL_LABEL(alloc_data_label())
	in mk_named_float_data (r,label);
	    label
	end
  
    fun load_freg_loc (rep : var_loc, poss_dest : regf option) : regf =
      let 
	  fun pickdest ()  = (case poss_dest of
				  NONE => alloc_regf()
				| SOME d => d)
      in case rep of
	  (VREGISTER (F r)) => (case poss_dest of
				      NONE => r
				    | SOME d => (add_instr(FMV(r,d)); d))
	| (VREGISTER (I r)) => error "moving from integer register to float register"
	| (VGLOBAL (l,NOTRACE_REAL)) =>
	      let val addr = alloc_regi(LABEL)
		  val dest = pickdest()
	      in  add_instr(LADDR(l,0,addr));
		  add_instr(LOADQF(EA(addr,0),dest));
		  dest
	      end
	(* this depends on globals being quadword aligned *)
	| (VGLOBAL (l,NOTRACE_INT)) => error "moving from global integer to float register"
	| (VGLOBAL (l,_)) => error "load_freg: got VGLOBAL(_,non-int and non-float)"
	| (VLABEL _) => error "load_freg: got VLABEL"
	| (VCODE _) => error "load_freg: got VCODE"
      end

    fun load_freg_val (rep : var_val, poss_dest : regf option) : regf =
      let 
	  fun pickdest ()  = (case poss_dest of
				  NONE => alloc_regf()
				| SOME d => d)
      in case rep of
	  (VINT i) => error "load_freg: got VINT"
	| (VREAL r) => let val label = mk_float_data r
			  val addr = alloc_regi LABEL
			  val r = pickdest()
		      in  add_instr(LADDR(label,0,addr));
			  add_instr(LOADQF(EA(addr,0),r));
			  r
		      end
	| (VRECORD _) => error "load_freg: got VRECORD"
      end

    fun load_reg_loc (rep : var_loc, destopt : reg option) : reg = 
	(case rep of
	     ((VREGISTER (F _)) | (VGLOBAL (_, NOTRACE_REAL))) =>
		 (case destopt of
		      NONE => F(load_freg_loc(rep, NONE))
		    | SOME (I ir) => error "load_reg on a FLOAT rep and a SOME(I _)"
		    | SOME (F fr) => F(load_freg_loc(rep, SOME fr)))
	   | _ =>
		 (case destopt of
		      NONE => I(load_ireg_loc(rep, NONE))
		    | SOME (F fr) => error "load_freg on a FLOAT rep and a SOME(F _)"
		    | SOME (I ir) => I(load_ireg_loc(rep, SOME ir))))

    fun load_reg_val (rep : var_val, destopt : reg option) : reg = 
	(case rep of
	     VREAL _ => 
		 (case destopt of
		      NONE => F(load_freg_val(rep, NONE))
		    | SOME (I ir) => error "load_reg_val on a FLOAT rep and a SOME(I _)"
		    | SOME (F fr) => F(load_freg_val(rep, SOME fr)))
	   | _ =>
		 (case destopt of
		      NONE => I(load_ireg_val(rep, NONE))
		    | SOME (F fr) => error "load_freg_val on a FLOAT rep and a SOME(F _)"
		    | SOME (I ir) => I(load_ireg_val(rep, SOME ir))))

    fun load_ireg((_,SOME v,_) : var_rep, poss_dest) = load_ireg_val(v,poss_dest)
      | load_ireg((l,_,_) : var_rep, poss_dest) = load_ireg_loc(l,poss_dest)
    fun load_freg((_,SOME v,_) : var_rep, poss_dest) = load_freg_val(v,poss_dest)
      | load_freg((l,_,_) : var_rep, poss_dest) = load_freg_loc(l,poss_dest)
    fun load_reg((_,SOME v,_) : var_rep, poss_dest) = load_reg_val(v,poss_dest)
      | load_reg((l,_,_) : var_rep, poss_dest) = load_reg_loc(l,poss_dest)

    fun load_ireg_locval(VAR_LOC vl, poss_dest) = load_ireg_loc(vl,poss_dest)
      | load_ireg_locval(VAR_VAL vv, poss_dest) = load_ireg_val(vv,poss_dest)
    fun load_freg_locval(VAR_LOC vl, poss_dest) = load_freg_loc(vl,poss_dest)
      | load_freg_locval(VAR_VAL vv, poss_dest) = load_freg_val(vv,poss_dest)
    fun load_reg_locval(VAR_LOC vl, poss_dest) = load_reg_loc(vl,poss_dest)
      | load_reg_locval(VAR_VAL vv, poss_dest) = load_reg_val(vv,poss_dest)
    fun load_ireg_sv(vl as (VAR_VAL(VINT i))) =
	if (in_imm_range_w i) then IMM(TilWord32.toInt i) else REG(load_ireg_locval(vl,NONE))
      | load_ireg_sv vl = REG(load_ireg_locval(vl,NONE))

  (*  
    and load_fvalue (v : U.value, poss_dest : regf option) : regf  =
      let 
	fun pickdest(reg) = 
	  case poss_dest of
	    NONE => reg
	  | SOME d => d
      in
	      case v of
		U.Var_e v => load_freg(v, getrep v,poss_dest)
	     | U.Real_e r => 
		 let val label = mk_float_data r
		   val addr = alloc_regi LABEL
		   val r = pickdest(alloc_regf())
		 in  add_instr(LADDR(label,0,addr));
		   add_instr(LOADQF(EA(addr,0),r));
		   r
		 end
	     | U.MLextern_e s => error "load_fvalue: unexpected ML extern"
	     | U.Cextern_e s => error "load_fvalue: unexpected C extern"
      end

  in
    fun load_value_regi    (v : U.value)    =  load_ivalue(v,NONE)
    fun load_value_to_regi (v : U.value, d) = 
      if (eqregi(load_ivalue(v,SOME d),d)) 
	then () else error "load_value_to_regi corrupt"
    fun load_value_regf    (v : U.value)    =  load_fvalue(v,NONE)
    fun load_value_to_regf (v : U.value, d) = 
	  if (eqregf(load_fvalue(v,SOME d),d)) 
	      then () else error "load_value_to_regf corrupt"
    fun load_sv (v : U.value) : sv =
      let fun foo i = 
	if in_imm_range_w i andalso
	  i2w(w2i i) = i  (* bug in SML/NJ ! *)
	  then Rtl.IMM(w2i i)
	else Rtl.REG(load_value_regi v)
      in case v
	of U.Int_e i => foo i
      | U.Tag_e i => foo i
      | _ => Rtl.REG (load_value_regi v)
      end
    val load_freg = load_freg
    val load_ireg = load_ireg

    fun get_fn_addr (v : U.value) : reg_or_label =
	case v
	    of U.Var_e var => 
		(case getrep var of
		     VREGISTER (I r) => REG' r
		   | VLABEL l => (print "WARNING: should not have code label as label\n"; LABEL' l)
		   | VCODE l => LABEL' l
		   | VGLOBAL (l,rep) => REG'(load_ivalue(v))
		   | _ =>  error ("get_fn_addr/var "^var2string var^"is not an reg or label"))
	  | U.MLextern_e (s,_) => LABEL'(ML_EXTERN_LABEL s)
	  | U.Cextern_e (s,_) => LABEL'(C_EXTERN_LABEL s)
	  | _ => error "get_fn_addr: non-function value in application"

  end
*)
  (* --------- end of load(sv/int/float) ------------------------------------------ *)


  (* --------- Common Code Sequence Helper Functions ------------------------------ *)

  (* by possibly adding 4 to heapptr, make it odd-quadword aligned;
   if addition took place, store a skip tag first *)
  fun align_odd_word () =
    let val tmp0 = alloc_regi(NOTRACE_INT)
      val tmp1 = alloc_regi(TRACE)
    in add_instr(LI(Rtltags.skiptag,tmp0));
	 add_instr(STORE32I(EA(heapptr,0),tmp0));  (* store a skiptag *)
	 add_instr(ANDB(heapptr,IMM 4,tmp0));
	 add_instr(ADD(heapptr,IMM 4,tmp1));
	 add_instr(CMV(EQ,tmp0,REG tmp1,heapptr))
    end
  (* by possibly adding 4 to heapptr, make it even-quadword aligned;
   if addition took place, store a skip tag first *)
  fun align_even_word () =
    let val tmp0 = alloc_regi(NOTRACE_INT)
      val tmp1 = alloc_regi(NOTRACE_INT)
    in add_instr(LI(Rtltags.skiptag,tmp0));
      add_instr(STORE32I(EA(heapptr,0),tmp0)); (* store a skiptag *)
      add_instr(ANDB(heapptr,IMM 4,tmp0));
      add_instr(ADD(heapptr,IMM 4,tmp1));
      add_instr(CMV(NE,tmp0,REG tmp1,heapptr))
    end
  

  fun add (reg,i : int,dest) =
    if in_imm_range i then
      add_instr(ADD(reg,IMM i,dest))
    else if in_ea_disp_range i then
	add_instr(LEA(EA(reg,i),dest))
         else let val size = alloc_regi(NOTRACE_INT)
              in add_instr(LI (i2w i,size));
		  add_instr(ADD(reg,REG size,dest))
	      end
  
  fun alloc_global (v : var,
		    con : con,
		    lv : loc_or_val) =
    let 
      val label = LOCAL_LABEL(alloc_named_data_label v)
      val addr = alloc_regi LABEL
      val rtl_rep = con2rep con
      val _ = (case rtl_rep of
		   (TRACE | COMPUTE _) => 
		       mutable_variables := (label,rtl_rep) :: !mutable_variables
		 | _ => ())
    in 
	add_global(v,(label,rtl_rep),con);
	add_data(ALIGN (QUAD));
	add_data(DLABEL (label));
	case lv of
	    VAR_LOC loc => let val reg = load_reg_loc(loc, NONE)
			   in  add_instr(LADDR(label,0,addr));
			       (case reg of
				    I r => (add_data(INT32(i2w 0));
					    add_instr(STORE32I(EA(addr,0),r)))
				  | F r => (add_data(FLOAT "0.0");
					    add_instr(STOREQF(EA(addr,0),r))))
			   end
	  | VAR_VAL (VINT w32) => add_data(INT32 w32)
	  | VAR_VAL (VREAL s) => add_data(FLOAT s)
	  | VAR_VAL (VRECORD (l,_)) => add_data(DATA l)
    end

  fun boxFloat regf : regi = 
      let val dest = alloc_regi TRACE
	  val _ = if (not (!HeapProfile))
		      then (add_instr(NEEDGC(IMM 4));
			    align_odd_word();
			    store_tag_zero(realarraytag (i2w 1));
			    add_instr(STOREQF(EA(heapptr,4),regf)); (* allocation *)
			    add(heapptr,4,dest);
			    add(heapptr,12,heapptr))
		  else
		      (add_instr(NEEDGC(IMM 5));
		       align_even_word();
		       store_tag_disp(0,MakeProfileTag());
		       store_tag_disp(4,realarraytag (i2w 1));
		       add_instr(STOREQF(EA(heapptr,8),regf)); (* allocation *)
		       add(heapptr,8,dest);
		       add(heapptr,16,heapptr))
      in  dest
      end


 (* code for allocating an fp array at run-time given a list of var_locs: return an ireg *)
       
  fun fparray (val_locs : loc_or_val list) : regi = 
    let 
      val res = alloc_regi TRACE
      val len = length val_locs
      fun scan (nil,_) = ()
	| scan (h::t,offset) =
	  let val src = load_freg_locval (h, NONE)
	  in  add_instr(STOREQF(EA(res,offset),src));
	      scan(t,offset+8)
	  end
    in 
       if (not (!HeapProfile))
	 then 
	   (add_instr(NEEDGC(IMM((if len = 0 then 1 else 2*len)+2)));   
	    align_odd_word();
	    store_tag_zero(realarraytag(i2w len));
	    add_instr(ADD(heapptr,IMM 4,res)))
       else (add_instr(NEEDGC(IMM((if len = 0 then 1 else 2*len)+3)));
	     align_even_word();
	     store_tag_disp(0,MakeProfileTag());
	     store_tag_disp(4,realarraytag(i2w len));
	     add_instr(ADD(heapptr,IMM 8,res)));
       scan (val_locs,0);
       if (len = 0)
	   then add_instr(ADD(res,IMM 4,heapptr))
       else
	   add_instr(ADD(res,IMM (8*len),heapptr));
       res
    end
  

	      
  local
    fun shuffle_regs (src : 'a list,dest : 'a list, 
		      eqreg : ('a * 'a) -> bool,
		      alloc : 'a -> 'a, 
		      mover: ('a * 'a) -> instr) =
      let fun isdest r = Listops.member_eq(eqreg,r, dest)
	
	local
	  (* given two lists, zip the lists, and remove any pairs 
	   whose two components are equal.*)
	  fun sieve2 (a,b) =
	    let fun f(h::t,h'::t') =
	      let val (nt,nt') = f (t,t')
	      in if eqreg(h,h') then (nt,nt')
		 else (h::nt,h'::nt')
	      end
		  | f(nil,nil) = (nil,nil)
		  | f _ = error "sieve2"
		      in f (a,b)
		      end
	in  (* remove assignments of register to self *)
	  val (src,dest) = sieve2 (src,dest)
	end
      
	(* compute which registers are both a source and a dest *)
	val needtmp = map (fn r => (r,isdest r)) src
	  
	(* create temp regs for these, and copy sources into them.*)
	val tmps = map (fn (r,true) =>
			let val r' = alloc(r)
			in  add_instr(mover(r,r'));
			  (r,SOME r')
			end
      |  (r,_) => (r,NONE)) needtmp
	  
	fun merge (nil,nil) = ()
	  | merge ((h',tmp)::t',h::t) =
	  (case tmp
	     of NONE =>
	       if eqreg(h',h) then ()
			   else add_instr(mover(h',h))
		     | SOME r =>
			     add_instr(mover(r,h));
			     merge(t',t))
	  | merge _ = error "shuffle_regs/merge"
      in merge (tmps,dest)
      end
  in
    fun shuffle_fregs (src : regf list,dest : regf list) =
      shuffle_regs(src, dest, eqregf, fn (arg) => alloc_regf(), FMV)
    fun shuffle_iregs (src : regi list,dest : regi list) =
      shuffle_regs(src, dest, eqregi, 
		   (fn (REGI(_,rep)) => alloc_regi(rep)
		 | (SREGI EXNARG) => alloc_regi(TRACE)
		 | (SREGI EXNPTR) => alloc_regi(TRACE)
		 | (SREGI _) => alloc_regi(NOTRACE_INT))
		   , MV)
  end



  fun xbnd (bnd : bnd) : unit = 
      let
	  val x = 5
      in
	  case bnd of
	      Con_b (v,k,c) => let val (ir,k) = xcon(v,c)
			       in  add_convar(v,ir,k)
			       end
	    | Exp_b (v,c,e) => let val (loc_or_val,_) = xexp(v,e,SOME c,NOTID)
			       in if istoplevel()
				      then alloc_global(v,c,loc_or_val)
				  else 
				      case loc_or_val of
					  VAR_LOC(var_loc) => add_varloc(v,var_loc,c)
					| VAR_VAL(var_val) => 
					      let val reg = load_reg_val(var_val, NONE)
					      in  add_var_val(v,reg,c,var_val)
					      end
			     end
	    | Fixopen_b (var_fun_set : (var,function) Nil.set) => error "no open functions permitted"
	    | Fixcode_b (var_fun_set : (var,function) Nil.set) => 
		  let 
		      fun adder(v,f as Function(effect,recur,vklist,vclist,vflist,b,c)) = 
			  let val funcon = AllArrow_c(Code,effect,vklist,map #2 vclist,
						      TilWord32.fromInt (length vflist),c)
			      val ll = LOCAL_CODE v
			      val _ = gadd_loclabel(ll, LOCAL_LABEL ll, funcon)
			  in addWork(FunWork(v,f))
			  end
		  in app adder (sequence2list var_fun_set)
		  end
	    | Fixclosure_b var_varconexpset => 
		  let val var_vcelist = sequence2list var_varconexpset
		      val cregsi = map (fn (v,{code,cenv,venv,tipe}) =>
					let val (cregi,_) = xcon(fresh_named_var "cenv", cenv)
					    val _ = add_var(v,I cregi,tipe)
					in cregi 
					end) var_vcelist
		      fun loadcl (n,(v,_),cregi) = 
			  let val code_lv = #1(xexp(fresh_named_var "codereg",Var_e v,NONE,NOTID))
			      val vls = [code_lv,
					 VAR_LOC(VREGISTER (I cregi)),
					 VAR_VAL(VINT 0w0)]
			      val reps = [NOTRACE_CODE, TRACE, TRACE]
			  in  make_record(NONE,reps,vls)
			  end
		      val clregsi = Listops.map2count loadcl (var_vcelist, cregsi)
		      val eregs = map (fn (_,{code,cenv,venv,tipe}) =>
				       #1(xexp'(fresh_named_var "venv", venv, NONE, NOTID))) var_vcelist
		      fun dowrite (clregi, I eregi) = STORE32I(EA(clregi,8), eregi)
			| dowrite _ = error "closure or venv not an int register"
		      val _ = Listops.map2 dowrite (clregsi,eregs)
		  in  ()
		  end
      end
  
  and xconbnd (cbnd : conbnd) : unit = 
      let
	  val x = 5
      in
	  case cbnd of
	      Con_cb (v,k,c) => let val (ir,k) = xcon(v,c)
				in  add_convar(v,ir,k)
				end
	    | Code_cb (conwork as (name,vklist,c,k)) => 
		  let val funkind = Arrow_k(Code,vklist,k)
		      val l = LOCAL_LABEL(LOCAL_CODE name)
		      val _ = gadd_convar_label(name,l,funkind)
		  in  addWork (ConFunWork conwork)
		  end
	    | Open_cb _ => error "open Fun_cb"
      end

  and xconst (arg_v : (con,exp) Prim.value) 
                   : loc_or_val * con =
      let
	  open Prim
	  open TilWord64
	  fun xvector (c,a : exp Array.array) : loc_or_val * con =
	      let 
		  val label = alloc_named_local_data_label "string"
		  val sz = Array.length a
		  val tagword = W32.orb(W32.lshift(W32.fromInt sz,int_len_offset),intarray)
		  val _ = add_data(INT32 tagword)
		  val _ = add_data(DLABEL label)
		  fun layout segsize packager = 
		  let fun loop index 0 acc = (packager (rev acc); loop index segsize [])
			| loop (index : int) remain acc =
			  if (index < sz)
			      then loop (index+1) (remain-1) ((Array.sub(a,index))::acc)
			  else packager (rev acc)
		  in  loop 0 segsize []
		  end
		  fun char_packager vals = 
		      let fun getword(Const_e(uint(_,w))) = TilWord64.toUnsignedHalf w
			    | getword _ = error "bad string"
			  val (a,b,c,d) = (case (map getword vals) of
					       [a,b,c,d] => (a,b,c,d)
					     | [a,b,c] => (a,b,c,W32.zero)
					     | [a,b] => (a,b,W32.zero,W32.zero)
					     | [a] => (a,W32.zero,W32.zero,W32.zero)
					     | _ => error "bad string")
			  val b = W32.lshift(b,8)
			  val c = W32.lshift(c,16)
			  val d = W32.lshift(d,24)
		      in  add_data(INT32 (W32.orb(W32.orb(a,b),W32.orb(c,d))))
		      end
		  val _ = (case (Array.sub(a,0)) of
			       Const_e(uint (W8, _)) => layout 4 char_packager
			     | _ => error "xvector not fully done")
	      in  (VAR_LOC(VLABEL label), Prim_c(Vector_c, [c]))
	      end
      in
	  (case arg_v of
	       ((uint (ws as (W8 | W16 | W32),w64)) |
		(int (ws as (W8 | W16 | W32),w64))) =>  
	       let val w32 = TilWord64.toUnsignedHalf w64
	       in  (VAR_VAL(VINT w32), Prim_c(Int_c ws, []))
	       end
	      | ((uint (W64, _)) | (int (W64, _))) => error "64-bit ints not done"
	      | (float (F64, s)) => (VAR_VAL(VREAL s), Prim_c(Float_c F64, []))
	      | (float (F32, _)) => error "32 bit floats not done"
	      | (vector (c,a)) => xvector(c,a)
	      | (array _ | refcell _) => 
		    error "array/vector/refcell constants not implemented"
	      | (tag(t,c)) => let val i = TilWord32.fromInt (tag2int t)
			      in  (VAR_VAL(VINT i), Prim_c(Int_c W32, []))
			      end)
      end


  and xexp' (name : var, (* Purely for debugging and generation of useful names *)
	     e : exp,    (* The expression being translated *)
	     copt : con option,    (* The type of the expression being translated *)
	     context     (* The evaluation context this expression is in *)
	     ) : reg * con =
      (case xexp(name,e,copt,context) of
	   (VAR_LOC var_loc, c) => (load_reg_loc(var_loc,NONE),c)
	 | (VAR_VAL var_val, c) => (load_reg_val(var_val,NONE),c))

  and xexp (name  : var, (* Purely for debugging and generation of useful names *)
	    arg_e : exp,         (* The expression being translated *)
	    copt  : con option,  (* The type of the expression being translated *)
	    context     (* The evaluation context this expression is in *)
	    ) : loc_or_val * con =
      let 
	  fun pickdesti rep = alloc_named_regi name rep
	  fun pickdestf () = alloc_named_regf name
      in
	  case arg_e of
	      Var_e v => (case (getrep v) of
			      (_,SOME value,c) => (VAR_VAL value,c)
			    | (l,_,c) => (VAR_LOC l, c))
	    | Const_e v => xconst v
	    | Let_e (_, bnds, body) => (app xbnd bnds;
					xexp(fresh_var(),body,copt,context))
	    | Prim_e (NilPrimOp nilprim, clist, elist) => xnilprim(nilprim,clist,elist,context)
	    | Prim_e (PrimOp prim, clist, elist) => xprim(prim,clist,elist,context)
	    | Switch_e sw => xswitch(name,sw,copt,context)
	    | App_e (openness, f, clist, elist, eflist) => (* assume the environment is passed in first *)
		  let 
		      local
			  val cregsi = map (fn c => #1(xcon(fresh_named_var "call_carg", c))) clist
			  val eregs = map (fn e => #1(xexp'(fresh_named_var "call_e", e, NONE, NOTID))) elist
			  val efregs = map (fn e => #1(xexp'(fresh_named_var "call_ef", e, NONE, NOTID))) eflist
			  val (selfcall,fun_reglabel,funcon,cregsi',eregs') = 
			      (case (openness,f) of
				   (Code,Var_e expvar) =>
				       let 
					   val (var_loc,_,funcon) = getrep expvar
				       in  ((case (getCurrentFun()) of
						 LOCAL_CODE v => eq_var(expvar,v)
					       | _ => false),
					    (case var_loc of
						 (VREGISTER (I r)) => REG' r
					       | (VCODE l) => LABEL' l
					       | _ => error "bad VAR_LOC for function"),
					    funcon, [], [])
				       end
				 | (Code,_) => error "ill-formed application"
				 | (Closure,cl) =>
				       let val (clreg,funcon) = xexp'(fresh_var(),cl,NONE,NOTID)
					   val clregi = (case clreg of
							     I ir => ir
							   | F _ => error "closure compiled to float reg")
					   val funregi = alloc_named_regi (fresh_named_var "funreg") NOTRACE_CODE
					   val cregi =  alloc_named_regi (fresh_named_var "creg") TRACE
					   val eregi =  alloc_named_regi (fresh_named_var "ereg") TRACE
					   val _ = (add_instr(LOAD32I(EA(clregi,0),funregi));
						    add_instr(LOAD32I(EA(clregi,4),cregi));
						    add_instr(LOAD32I(EA(clregi,8),eregi)))
				       in  (false, REG' funregi, funcon, [cregi], [I eregi])
				       end
				 | (Open,_) => error "no open apps allowed")
		      in
			  val selfcall = selfcall
			  val fun_reglabel = fun_reglabel
			  val cregsi = cregsi' @ cregsi
			  val eregs = eregs' @ eregs
			  val efregs = efregs
			  val rescon = (case (copt,funcon) of
					    (SOME c,_) => c
					  | (NONE,AllArrow_c(_,_,_,_,_,rescon)) => rescon
					  | _ => error "cannot compute type of result of call")
		      end
		      val iregs = cregsi @ (map (coercei "call") eregs)
		      val fregs = map coercef efregs
		      val dest = getResult()
		      val results = (case dest of
					 F fr => ([],[fr])
				       | I ir => ([ir],[]))
		      fun do_call return_opt = 
			  add_instr(CALL{func=fun_reglabel,
					 return=return_opt,
					 args=(iregs,fregs),
					 results=results,
					 tailcall=(case return_opt of
						       SOME _ => true
						     | NONE => false),
					 save=SAVE(getLocals())})
		  in  (case (context,#elim_tail_call(!cur_params)) of
			   ((NOTID,_) | (_, false)) => do_call NONE
			 | (ID r,true) =>  
			       if (selfcall)
				   then (shuffle_iregs(iregs,getArgI());
					 shuffle_fregs(fregs,getArgF());
					 add_instr(BR (getTop())))
			       else do_call (SOME r));
		      (VAR_LOC(VREGISTER dest), rescon)
		  end

	    | Raise_e (exp, con) =>
		  let val ir = (case (xexp'(name,exp,SOME(Prim_c(Exn_c,[])),NOTID)) of
				    (I ir,_) => ir
				  | (F _,_) => error "exception cannot translate to an freg")
		      val newpc = alloc_regi LABEL
		  in  add_instr(LOAD32I(EA(exnptr,0),newpc));
		      add_instr(MV (ir,exnarg));
		      add_instr RESTORE_CS;
		      add_instr (JMP(newpc,nil));
		      (VAR_LOC(VREGISTER (I ir)), Prim_c(Exn_c,[]))   (* <---  the r should not be used *)
		  end
            (* --- We rely on the runtime to unwind the stack so we don't need to save the
	           free variables of the continuation of this expression. *)
	    | Handle_e (exp, Function(effect,recur,
				      [],[(exnvar,exncon)], [], handler_body, hcon)) => 
		  let
		      (* compute free variables that need to be saved for handler *)

		      local
			  val fvhandler = NilUtil.freeExpVarInExp handler_body
			  fun loop [] (irep,ir,fr) = (irep,ir,fr)
			    | loop (v::rest) (irep,ir,fr) = 
			      let val var = Var_e v
			      in
				  (case (#1(getrep v)) of
				       VREGISTER (I (r as (REGI (_,rep)))) => loop rest 
					   (rep::irep,r::ir,fr)
				     | VREGISTER (I (SREGI _)) => error "SREGI free in handler!!!"
				     | VREGISTER (F r) => loop rest (irep,ir,r::fr)
				     | _ => loop rest (irep,ir,fr))
			      end
		      in
			  val (local_int_reps,local_iregs, local_fregs) = loop fvhandler ([],[],[])
		      end



		      val bl = alloc_code_label()
		      val hl = alloc_named_code_label (fresh_named_var "handler")
		      val hlreg = alloc_regi LABEL
		      val nl = alloc_named_code_label (fresh_named_var "after_handler")


		      val reps = (LABEL :: LABEL :: TRACE :: local_int_reps)

		      val fpbase = (* --- save the floating point values, if any *)
			  (case local_fregs of
			       [] => NONE
			     | _ => SOME (fparray(map (VAR_LOC o VREGISTER o F) local_fregs)))

		      (* --- create the exn record and set the exnptr to it to install it *)
		      val int_vallocs = (map (VAR_LOC o VREGISTER o I) 
					 ([hlreg, stackptr,exnptr] @ local_iregs))
		      val _ = (add_instr(LADDR(LOCAL_LABEL hl,0,hlreg));
			       make_record(SOME exnptr,reps, int_vallocs))
			  
                      (* --- compute the body; restore the exnpointer; branch to after handler *)
		      val (reg,arg_c) = xexp'(name,exp,copt,context)
		      val _ = (add_instr(LOAD32I(EA(exnptr,8),exnptr));
			       add_instr(BR bl));

		      (* --- now the code for handler --- *)
		      val xr = alloc_named_regi exnvar TRACE;
		      val _ = add_var (exnvar,I xr,exncon)
		      val _ = (add_instr(ILABEL hl);
			       add_instr(MV(exnarg,xr)))

		      (* --- restore the int registers --- *)
		      val _ = let 
				  val int_regs =
				      (case fpbase of
					   SOME r => [stackptr, exnptr, r] 
					 | NONE => [stackptr, exnptr]) @ local_iregs
				  fun f (h :: t,offset) =
				      (if eqregi(h,exnptr) then ()
				       else add_instr(LOAD32I(EA(exnptr,offset),h));
					   f (t,offset+4))
				    | f (nil,offset) = ()
			      in f (int_regs,4)
			      end

		      (* --- restore the float registers --- *)			  
		      val _ = let fun f (base, h :: t,offset) = 
			                    (add_instr(LOADQF(EA(base,offset),h));
					     f (base, t,offset+8))
				    | f (base,nil,offset) = ()
			      in (case fpbase of
				      SOME base => f (base,local_fregs,0)
				    | NONE => ())
			      end
			  
                      (* --- restore exnptr; compute the handler; move result into same register
                             as result reg of expression; add after label; and fall-through *)
		      val _ = add_instr(LOAD32I(EA(exnptr,8),exnptr))
		      val (hreg,_) = xexp'(name,exp,SOME hcon,context)
		      val _ = (case (hreg,reg) of
				   (I hreg,I reg) => add_instr(MV(hreg,reg))
				 | (F hreg,F reg) => add_instr(FMV(hreg,reg))
				 | _ => error "hreg/ireg mismatch in handler")
		      val _ = add_instr(ILABEL bl)
		  in 
		      (* for debugging, should check that arg_c and hcon are the same *)
		      (VAR_LOC(VREGISTER reg), arg_c)
		  end
	    | Handle_e _ => error "ill-formed handler"
      end


      (* The trick is to notice that for certain args, the comparison and computation
         can be folded into one instruction. *)
      and zero_one (r : regi, copt : con option, zeroexp, oneexp, context) = 
	  let val thenl = alloc_code_label()
	      val elsel = alloc_code_label()
	      val afterl = alloc_code_label()
	      val _ = add_instr(BCNDI(NE,r,elsel,false))
	      val (zero,zcon) = xexp'(fresh_named_var "zero_result", zeroexp, copt, context)
	      val result = zero
	      val _ = add_instr(BR afterl)
	      val _ = add_instr(ILABEL elsel)
	      val (one,ocon) = xexp'(fresh_named_var "nonzero_result", oneexp, copt, context)
	      val _ = (case (zero,one) of 
			   (I zz, I oo) => add_instr(MV (oo,zz))
			 | (F zz, F oo) => add_instr(FMV (oo,zz))
			 | _ => error "zero_one: different arms have results in float and int registers")
	      val _ = add_instr(ILABEL afterl)
	  in (VAR_LOC (VREGISTER result), zcon)
	  end


  and xswitch (name : var,  (* Purely for debugging and generation of useful names *)
	       sw : switch, (* The switch expression being translated *)
	       arg_c : con option, (* The type of the switch expression *)
	       context      (* The evaluation context this expression is in *)
	       ) : loc_or_val * con =
      let
	  val rescon = ref NONE
	  val dest = ref NONE
	  fun mv (r,c) = (case (r,!dest) of
			      (_,NONE) => (dest := (SOME r); rescon := (SOME c))
			    | (I ir1, SOME(I ir2)) => add_instr(MV(ir1,ir2))
			    | (F fr1, SOME(F fr2)) => add_instr(FMV(fr1,fr2))
			    | _ => error "register mismatch")
	  fun get_body (Function(_,_,[],[],[],e,c)) = e
	    | get_body _ = error "get_body of xswitch got a function with arguments"
	  fun no_match() = (case (!rescon) of
				SOME c => (mv(xexp'(fresh_var(),Raise_e(NilUtil.match_exn,c),
						    NONE, NOTID)))
			      | NONE => error "empty switch statement")

      in
	  case sw of
	      Intsw_e {info, arg, arms, default} => 
		  let val r = (case (xexp'(fresh_named_var "intsw_arg",arg,
					   SOME(Prim_c(Int_c info,[])),NOTID)) of
				   (I ireg,_) => ireg
				 | (F _,_) => error "intsw argument in float register")
		  in  case (arms,default) of
		      ([(0w0,z)],SOME e) => zero_one(r, arg_c, get_body z, e, context)
		    | ([(0w0,z),(0w1,one)],NONE) => zero_one(r, arg_c, get_body z, get_body one, context)
		    | _ => (* general case *)
			  let 
			      val afterl = alloc_code_label()
			      fun scan(lab,[]) = (add_instr(ILABEL lab);
						  case default of
						      NONE => no_match()
						    | SOME e => mv(xexp'(fresh_var(),e,arg_c,context)))
				| scan(lab,(i,Function(_,_,_,_,_,body,con))::rest) = 
				  let val next = alloc_code_label()
				      val test = alloc_regi(NOTRACE_INT)
				  in  add_instr(ILABEL lab);
				      (if in_imm_range(w2i i) then
					  add_instr(CMPSI(EQ,r,IMM(w2i i),test))
				      else 
					  let val tmp = alloc_regi(NOTRACE_INT)
					  in add_instr(LI(i,tmp));
					      add_instr(CMPSI(EQ,r,REG tmp,test))
					  end);
				      add_instr(BCNDI(EQ,test,next,true));
				      mv(xexp'(fresh_var(),body,SOME con,context));
				      add_instr(BR afterl);
				      scan(next,rest)
				  end
			  in  scan(alloc_code_label(),arms);
			      add_instr(ILABEL afterl);
			      case (!dest,!rescon) of
				  (SOME r,SOME c) => (VAR_LOC(VREGISTER r),c)
				| _ => error "no arms"
			  end
		  end
	    | Exncase_e {info, arg, arms, default} => 
		  let val exnarg = (case (xexp'(fresh_named_var "exntsw_arg",arg,NONE,NOTID)) of
					(I ireg,_) => ireg
				      | (F _,_) => error "exnsw argument in float register")
		      val exntag = alloc_regi(NOTRACE_INT)
		      val _ = add_instr(LOAD32I(EA(exnarg,0),exntag))
		      val afterl = alloc_code_label()
		      fun scan(lab,[]) = (add_instr(ILABEL lab);
					  case default of
					      NONE => no_match()
					    | SOME e => mv(xexp'(fresh_var(),e,arg_c,context)))
			| scan(lab,(armtag,Function(_,_,_,[(v,c)],_,body,con))::rest) = 
			  let val next = alloc_code_label()
			      val test = alloc_regi(NOTRACE_INT)
			      val carried = alloc_reg c
			      val carriedi = (case carried of
						  I ir => ir
						| _ => error "carried value is an unboxed float")
			      val _ = add_var(v,carried,c)
			      val _ = add_instr(ILABEL lab);
			      val armtagi = (case (#1(xexp'(fresh_var(),armtag,NONE,NOTID))) of
						 I ir => ir | _ => error "armtag is a float")
			  in  add_instr(CMPSI(EQ,exntag,REG armtagi,test));
			      add_instr(BCNDI(EQ,test,next,true));
			      add_instr(LOAD32I(EA(exnarg,4),carriedi));
			      mv(xexp'(fresh_var(),body,SOME con,context));
			      add_instr(BR afterl);
			      scan(next,rest)
			  end
			| scan(lab,_) = error "ill-typed exnsw_e Function"
		  in  scan(alloc_code_label(),arms);
		      add_instr(ILABEL afterl);
		      case (!dest,!rescon) of
			  (SOME r,SOME c) => (VAR_LOC(VREGISTER r),c)
			| _ => error "no arms"
		  end 
	    | Sumsw_e {info = (tagcount,cons), arg, arms, default} => 
		  let val r = (case (xexp'(fresh_named_var "sumsw_arg",arg,NONE,NOTID)) of
				   (I ireg,_) => ireg
				 | (F _,_) => error "intsw argument in float register")
		      val afterl = alloc_code_label()
		      fun scan(lab,[]) = (add_instr(ILABEL lab);
					  case default of
					      NONE => no_match()
					    | SOME e => mv(xexp'(fresh_var(),e,arg_c,context)))
			| scan(lab,(i,Function(_,_,_,[(v,spcon)],_,body,con))::rest) = 
			  let val next = alloc_code_label()
			      val test = alloc_regi(NOTRACE_INT)
			      val _ = add_instr(ILABEL lab)
			      val tag = if (TilWord32.ult(i,tagcount))
					    then r
					else let val tag = alloc_regi(NOTRACE_INT)
						 val _ = add_var(v,I r,spcon)
					     in  (add_instr(LOAD32I(EA(r,0),tag));
						  tag)
					     end
			  in  if in_imm_range(w2i i) then
			      add_instr(CMPSI(EQ,tag,IMM(w2i i),test))
			      else 
				  let val tmp = alloc_regi(NOTRACE_INT)
				  in add_instr(LI(i,tmp));
				      add_instr(CMPSI(EQ,tag,REG tmp,test))
				  end;
			      add_instr(BCNDI(EQ,test,next,true));
			      mv(xexp'(fresh_var(),body,SOME con,context));
			      add_instr(BR afterl);
			      scan(next,rest)
			  end
			| scan(lab,(i,_)::rest) = error "bad function for sum switch"
		  in  scan(alloc_code_label(),arms);
		      add_instr(ILABEL afterl);
		      case (!dest,!rescon) of
			  (SOME r,SOME c) => (VAR_LOC(VREGISTER r),c)
			| _ => error "no arms"
		  end
      end

  (* First, we assume that datatypes are translated by 
     rearranging the non value-carrying components to the beginning.
     To support efficient representations, we let m and n denote
     the length of the maximal initial segment of non value-carrying components 
     and n the rest of the components.  Note that this m is
     NOT the number(k) of non-value-carrying constructors in the datatype.
     It is possible for m > k. There are then 6 cases to consider:

         1. m=0, n=0    we use the unit representation
         2. m<>0, n=0   we use tags to represent the datatype
         3. m=0, n=1    we use the argument type to represent the datatype
         4. m<>0, n=1   we use tag or pointer to represent the datatype
         5. m<>0, n>1   we use tag or tagged sum to represent the datatype
         6. m=0, n>1    we use tagged sum to represent the datatype
  *)
  and xsum ({tagcount : TilWord32.word,field : TilWord32.word},
	    clist,elist,context) : loc_or_val * con = 
      let
	  open Prim
	  val field' = TilWord64.fromInt(TilWord32.toInt field)
	  val varlocs = map (fn e => #1(xexp(fresh_var(),e,NONE,NOTID))) elist
	  val sumcon = Prim_c(Sum_c{tagcount=tagcount,known=NONE}, clist)
	  val nontagcount = length clist
	  fun xtagsum() = 
	      let val tag_vl = VAR_VAL(VINT (TilWord32.uminus(field,TilWord32.fromInt nontagcount)))
		  val reps = map valloc2rep varlocs
		  val ir = make_record(NONE,reps,tag_vl::varlocs)
	      in VAR_LOC(VREGISTER(I ir))
	      end
	  val varloc =
	      case (TilWord32.toInt tagcount,nontagcount) of
		  (0,0) => #1(xexp(fresh_var(),Prim_e(NilPrimOp(Nil.record []),[], []),NONE,context))
		| (_,0) => #1(xexp(fresh_var(),Const_e(uint(W32,field')),NONE,context))
		| (0,1) => hd varlocs
		| (_,1) => if (TilWord32.equal(field,tagcount))
			       then hd varlocs
			   else #1(xexp(fresh_var(),Const_e(uint(W32,field')),NONE,context))
		| (_,_) => if (TilWord32.ult(field,tagcount))
				    then #1(xexp(fresh_var(),Const_e(uint(W32,field')),NONE,context))
			   else xtagsum()
      in (varloc, sumcon) 
      end


  and xnilprim(nilprim,clist,elist,context) : loc_or_val * con = 
      (case nilprim of 
	   Nil.record labels => 
	       let val vallocs_types = map (fn e => xexp(fresh_var(), e,NONE,NOTID)) elist
		   val types = map #2 vallocs_types
		   val vallocs = map #1 vallocs_types
		   val c = Prim_c(Record_c labels, types)
		   val reps = map valloc2rep vallocs
		   val desti = make_record(NONE,reps,vallocs)
	       in  (VAR_LOC(VREGISTER(I desti)), c)
	       end
	 | select label => (case (clist,elist) of 
				([Prim_c(Record_c labels,cons)],[e]) => 
				    let fun loop [] _ n = error "bad select"
					  | loop _ [] n = error "bad select"
					  | loop (l1::lrest) (c1::crest) n = if (eq_label(l1,label))
										 then (n,c1)
									     else loop lrest crest (n+1)
					val (which,con) = loop labels cons 0
					val (I addr,_) = xexp'(fresh_var(),e,NONE,NOTID)
					val desti = (case (alloc_reg con) of
							 I ir => ir
						       | _ => error "records cannot have floats")
					val _ = add_instr(LOAD32I(EA(addr,which * 4), desti))
				    in  (VAR_LOC(VREGISTER(I desti)), con)
				    end)
	 | inject_record injinfo => xsum(injinfo,clist,elist,context)
	 | inject (injinfo as {tagcount, field}) => 
	       if (TilWord32.ult(field,tagcount))
		   then xsum(injinfo,clist,elist,context)
	       else let 					    
			val v = fresh_named_var "slowrecord"
			fun make_proj cons n = Prim_e(NilPrimOp(select (NilUtil.generate_tuple_label n)),
						      cons,[Var_e v])
			val elist' = 
			    (case elist of
				 [e] => let val (I ir,c) = xexp'(fresh_var(),e,NONE,NOTID)
					    val _ = add_var(v,I ir,c)
					    val (count,cons) = 
						(case c of
						     Prim_c(Record_c ll,cons) => (length ll,cons)
						   | _ => error "inject not a record not supported")
					in (Listops.map0count (make_proj cons) count)
					end
			       | _ => error "bad inject")
		    in xnilprim(inject_record injinfo, clist,elist',context)
		    end
	 | project_sum_record {tagcount, sumtype, field} => 
	       let val index = TilWord32.toInt(TilWord32.uminus(sumtype, tagcount))
		   val field' = TilWord32.toInt field
		   val base = (case elist of
				   [e] => coercei "" (#1(xexp'(fresh_var(),e,NONE,NOTID)))
				 | _ => error "bad project_sum_record")
		   val record_con = (List.nth(clist,index)
				     handle _ => error "bad project_sum_record")
		   val field_con = (case record_con of
					Prim_c(Record_c _,cons) => List.nth(cons,field')
				      | _ => error "bad project_sum_record")
		   val desti = alloc_regi (con2rep field_con)
		   val _ = add_instr(LOAD32I(EA(base,4*field'),desti))
	       in (VAR_LOC(VREGISTER(I desti)), field_con)
	       end
	 | project_sum {tagcount, sumtype} => 
	       let val index = TilWord32.toInt(TilWord32.uminus(sumtype, tagcount))
		   val record_con = (List.nth(clist,index)
				     handle _ => error "bad project_sum")
		   val field_cons = (case record_con of
					Prim_c(Record_c _,cons) => cons
				      | _ => error "bad project_sum_record")
		   val labels = Listops.mapcount (fn (n,_) => NilUtil.generate_tuple_label n) field_cons
		   fun make_e (n,_) = Prim_e(NilPrimOp(project_sum_record{tagcount = tagcount,
									  sumtype = sumtype,
									  field = TilWord32.fromInt n}),
					     clist,elist)
		   val elist' = Listops.mapcount make_e field_cons
	       in  xnilprim(Nil.record labels,field_cons,elist',context)
	       end
	 | box_float Prim.F64 => 
	       let val (F fr,_) = (case elist of
				       [e] => xexp'(fresh_var(),e,NONE,NOTID)
				     | _ => error "boxfloat did not get one arg")
		   val ir = boxFloat fr
	       in (VAR_LOC(VREGISTER(I ir)), Prim_c(BoxFloat_c Prim.F64,[]))
	       end
	 | unbox_float Prim.F64 => 
	       let val (I ir,_) = (case elist of
				       [e] => xexp'(fresh_var(),e,NONE,NOTID)
				     | _ => error "unbox_float did not get one arg")
		   val fr = alloc_regf()
		   val _ = add_instr(LOADQF(EA(ir,0),fr))
	       in (VAR_LOC(VREGISTER(F fr)), Prim_c(Float_c Prim.F64,[]))
	       end
	 | box_float Prim.F32 => error "32-bit floats not done"
	 | unbox_float Prim.F32 => error "32-bit floats not done"
	 | roll => (case (clist,elist) of 
			([c],[e]) => let val (r,_) = xexp'(fresh_var(),e,NONE,context)
				     in (VAR_LOC(VREGISTER r), c)
				     end
		      | _ => error "roll given bad arguments")
	 | unroll => (case (clist,elist) of 
			([c],[e]) => let val (r,_) = xexp'(fresh_var(),e,NONE,context)
					 val c' = (case c of
						       Mu_c(vcseq,v) => NilUtil.muExpand(vcseq,v)
						     | _ => error "not a mu type decorating an unroll")
				     in (VAR_LOC(VREGISTER r), c')
				     end
		      | _ => error "roll given bad arguments")
	 | make_exntag => (case clist of
			       [c] => let val c' = Prim_c(Exntag_c,[c])
					  val desti = alloc_regi NOTRACE_INT
					  val addr = alloc_regi TRACE
					  val tmp = alloc_regi NOTRACE_INT
					  val _ = (add_instr(LADDR(exncounter_label,0,addr));
						   add_instr(LOAD32I(EA(addr,0),desti));
						   add_instr(ADD(desti,IMM 1,tmp));
						   add_instr(STORE32I(EA(addr,0),tmp)))
				      in  (VAR_LOC(VREGISTER (I desti)), c')
				      end
			     | _ => error "bad make_exntag")
	 | inj_exn => (case (clist,elist) of
			   ([c],[e1,e2]) => let val desti = alloc_regi NOTRACE_INT
						val (vl1,_) = xexp(fresh_var(),e1,NONE,NOTID)
						val (vl2,_) = xexp(fresh_var(),e2,NONE,NOTID)
						val vallocs = [vl1,vl2]
						val reps = map valloc2rep vallocs
					    in  (VAR_LOC(VREGISTER (I (make_record(NONE,reps,vallocs)))),
						 Prim_c(Exn_c,[]))
					    end
			 | _ => error "bad make_exntag")
	 | make_vararg _ => raise XXX     
	 | make_onearg _ => raise XXX     
	 | peq => error "peq not done")


  and xprim(prim,clist,elist,context) : loc_or_val * con = 
      let 
	  open Prim
	  val vlcon_list = map (fn e => xexp(fresh_var(),e,NONE,NOTID)) elist
	  val vl_list = map #1 vlcon_list
	  val int32 = Prim_c(Int_c W32, []) 
	  val float64 = Prim_c(Float_c F64, []) 
	  fun xtt int_tt = INT_TT
	    | xtt real_tt = REAL_TT
	    | xtt both_tt = BOTH_TT
	  fun commute (v1 as (VAR_VAL(VINT i)),v2) = 
	      if in_imm_range_w i then (v2,v1) else (v1,v2)
	    | commute arg = arg
	  (* ----------- integer comparisons ----------------------- *)
	  fun flip EQ = EQ
	    | flip LE = GE
	    | flip LT = GT
	    | flip GE = LE
	    | flip GT = LT
	    | flip NE = NE
	    | flip LBS = error "flip: LBS shouldn't be here"
	    | flip LBC = error "flip: LBC shouldn't be here"
	  fun swap (a,v1 as VAR_VAL(VINT i),v2) = 
	      if in_imm_range_w i 
		  then (flip a,v2,v1)
	      else (a,v1,v2)
	    | swap arg = arg
          fun stdcmp2i signed oper =
              (case vl_list of
                   [vl1,vl2] => 
                       let 
                           val (oper,vl1,vl2) = swap(oper,vl1,vl2)
                           val a' = load_ireg_locval(vl1,NONE)
                           val b' = load_ireg_sv vl2
                           val dest = alloc_regi NOTRACE_INT
                           val cmp = if signed then CMPSI else CMPUI
                           val _ =  add_instr(cmp(oper,a',b',dest))
                       in (VAR_LOC(VREGISTER(I dest)),bool_con)
                       end
                 | _ => error "need exactly 2 arguments for this primitive")
          val stdcmp2si = stdcmp2i true
          val stdcmp2ui = stdcmp2i false

          (* ----------- floatint point comparison ---------------- *)
	  fun cmpf oper =
	      (case vl_list of
		   [vl1,vl2] => 
		       let val a' = load_freg_locval(vl1,NONE)
			   val b' = load_freg_locval(vl2,NONE)
			   val dest = alloc_regi NOTRACE_INT
			   val _ =  add_instr(CMPF(oper,a',b',dest))
		       in (VAR_LOC(VREGISTER(I dest)),bool_con)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")

	  (* ----------- unary integer operations ----------------- *)
	  fun op1i oper : loc_or_val * con =
	      (case vl_list of
		   [vl1] => 
		       let val a' = load_ireg_locval(vl1,NONE)
			   val dest = alloc_regi NOTRACE_INT
			   val _ = add_instr(oper(a',dest))
		       in (VAR_LOC(VREGISTER(I dest)), int32)
		       end
		 | _ => error "need exactly 1 arguments for this primitive")

	  (* ----------- binary integer operations ----------------- *)
	  fun op2i comflag oper : loc_or_val * con =
	      (case vl_list of
		   [vl1,vl2] => 
		       let (* commute values if the first arg is a small imm *)
			   val (vl1,vl2) = if comflag then commute(vl1,vl2) else (vl1,vl2)
			   val a' = load_ireg_locval(vl1,NONE)
			   val b' = load_ireg_sv vl2
			   val dest = alloc_regi NOTRACE_INT
			   val _ = add_instr(oper(a',b',dest))
		       in (VAR_LOC(VREGISTER(I dest)), int32)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
	  val commutesop2i = op2i true
	  val stdop2i = op2i false
	  fun add_ibar b1 b2 = (add_instr (b1 INT_TT); add_instr (b2 INT_TT))
	  fun trapZero result = (add_ibar SOFT_ZBARRIER HARD_ZBARRIER; result)
	  fun trapOver result = (add_ibar SOFT_VBARRIER HARD_VBARRIER; result)
	  (* ----------- binary and unary float operations ----------------- *)
	  fun op2f oper : loc_or_val * con =
	      (case vl_list of
		   [vl1,vl2] => 
		       let val a' = load_freg_locval(vl1,NONE)
			   val b' = load_freg_locval(vl2,NONE)
			   val dest = alloc_regf()
			   val _ = add_instr(oper(a',b',dest))
		       in (VAR_LOC(VREGISTER(F dest)), float64)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
	  fun op1f oper : loc_or_val * con =
	      (case vl_list of
		   [vl] => 
		       let val a' = load_freg_locval(vl,NONE)
			   val dest = alloc_regf()
			   val _ = add_instr(oper(a',dest))
		       in (VAR_LOC(VREGISTER(F dest)), float64)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
      in (case prim of
	      soft_vtrap tt => (add_instr(SOFT_VBARRIER(xtt tt)); unitval)
	    | soft_ztrap tt => (add_instr(SOFT_ZBARRIER(xtt tt)); unitval)
	    | hard_vtrap tt => (add_instr(HARD_VBARRIER(xtt tt)); unitval)
	    | hard_ztrap tt => (add_instr(HARD_ZBARRIER(xtt tt)); unitval)
	       
	    | mk_ref => (case (vl_list,clist) of
			     ([vl],[c]) => xarray(c,VAR_VAL(VINT 0w1),vl)
			   | _ => error "mk_ref given bad arguments")
	    | deref =>  (case (vl_list,clist) of
			     ([vl],[c]) => xsub(c,vl,VAR_VAL(VINT 0w0))
			   | _ => error "deref given bad arguments")
	    | setref =>  (case (vl_list,clist) of
			      ([vl1,vl2],[c]) => xupdate(c,vl1,VAR_VAL(VINT 0w0),vl2)
			    | _ => error "setref given bad arguments")
	    | eq_ref => (case (clist,vl_list) of
			     ([c],[vl1,vl2]) => xeqarray(c,vl1,vl2)
			   | _ => error "eq_ref given bad arguments")

	    | float2int => (case vl_list of
				[vl] => let val src = load_freg_locval(vl,NONE)
					    val dest = alloc_regi NOTRACE_INT
					    val _ = add_instr(CVT_REAL2INT(src,dest))
					in (VAR_LOC(VREGISTER(I dest)), int32)
					end)
	    | int2float => (case vl_list of
				[vl] => let val src = load_ireg_locval(vl,NONE)
					    val dest = alloc_regf()
					    val _ = add_instr(CVT_INT2REAL(src,dest))
					in (VAR_LOC(VREGISTER(F dest)), float64)
					end)
	    | int2uint => raise XXX
	    | uint2int => raise XXX

	    | neg_float fs => op1f FNEGD
	    | abs_float fs => op1f FABSD
	    | plus_float fs => op2f FADDD
	    | minus_float fs => op2f FSUBD
	    | mul_float fs => op2f FMULD
	    | div_float fs => op2f FDIVD
	    | less_float fs => cmpf LT
	    | greater_float fs => cmpf GT
	    | lesseq_float fs => cmpf LE
	    | greatereq_float fs => cmpf GE
	    | eq_float fs => cmpf EQ
	    | neq_float fs => cmpf NE

	    | plus_int W32 =>  trapOver(commutesop2i ADDT)
	    | mul_int W32 =>   trapOver(commutesop2i MULT)
	    | minus_int W32 => trapOver(stdop2i SUBT)
	    | div_int W32 =>   trapZero(stdop2i DIVT)
	    | mod_int W32 =>   trapZero(stdop2i MODT)
	    | quot_int W32 =>  error "quotient not implemented!"
	    | rem_int W32 =>   error "remainder not implemented!"
	    | plus_uint W32 =>  (commutesop2i ADDT)
	    | mul_uint W32 =>   (commutesop2i MULT)
	    | minus_uint W32 => (stdop2i SUBT)
	    | div_uint W32 =>   (stdop2i DIVT)
	    | mod_uint W32 =>   (stdop2i MODT)
	    | less_int W32 => stdcmp2si LT
	    | greater_int W32 => stdcmp2si GT
	    | lesseq_int W32 => stdcmp2si LE
	    | greatereq_int W32 => stdcmp2si GE
	    | less_uint W32 => stdcmp2ui LT
	    | greater_uint W32 => stdcmp2ui GT
	    | lesseq_uint W32 => stdcmp2ui LE
	    | greatereq_uint W32 => stdcmp2ui GE
	    | eq_int W32 => stdcmp2ui EQ
	    | neq_int W32 => stdcmp2ui NE

	    | neg_int is => error "neg_int not done"
	    | abs_int is => error "abs_int not done"

	    | not_int W32 => op1i NOTB
	    | and_int W32 => commutesop2i ANDB
	    | or_int W32 => commutesop2i ORB
	    | lshift_int W32 => stdop2i SLL
	    | rshift_int W32 => stdop2i SRA
	    | rshift_uint W32 => stdop2i SRL

	    | ((plus_int _) | (mul_int _) | (minus_int _) | (div_int _) | 
	       (mod_int _) | (quot_int _) | (rem_int _) | 
	       (plus_uint _) | (mul_uint _) | (minus_uint _) |
	       (div_uint _) | (mod_uint _) |
	       (less_int _) | (greater_int _) | (lesseq_int _) | (greatereq_int _) |
	       (less_uint _) | (greater_uint _) | (lesseq_uint _) | (greatereq_uint _) |
	       (eq_int _) | (neq_int _) | (not_int _) | (and_int _) | (or_int _) | 
	       (lshift_int _) | (rshift_int _) | (rshift_uint _)
	       ) => error "non 32-bit math not done"


	     | (length1 _) => (case (vl_list,clist) of
				   ([vl],[c]) => xlength(c,vl)
				 | _ => error "length/vlength given bad arguments")
	     | (sub1 _) => (case (vl_list,clist) of
				([vl1,vl2],[c]) => xsub(c,vl1,vl2)
			      | _ => error "sub/vsub given bad arguments")
	     | (floatsub1 _) => (case (vl_list,clist) of
				  ([vl1,vl2],[c]) => (VAR_LOC(VREGISTER (F (xfloatsub(vl1,vl2)))),
						      float64)
				| _ => error "floatsub")
	     | (intsub1 _) => (case (vl_list,clist) of
				   ([vl1,vl2],[]) => (VAR_LOC(VREGISTER(I (xintptrsub(int32,vl1,vl2)))),
						      int32)
				 | _ => error "intsub")
	     | (ptrsub1 _) => (case (vl_list,clist) of
				   ([vl1,vl2],[c]) => (VAR_LOC(VREGISTER(I (xintptrsub(c,vl1,vl2)))), c)
				 | _ => error "ptrsub")
	     | update1 => (case (vl_list,clist) of
				      ([vl1,vl2,vl3],[c]) => xupdate(c,vl1,vl2,vl3)
				    | _ => error "update given bad arguments")
	     | intupdate1 => (case (vl_list,clist) of
				      ([vl1,vl2,vl3],[c]) => xintupdate(c,vl1,vl2,vl3)
				    | _ => error "update given bad arguments")
	     | ptrupdate1 => (case (vl_list,clist) of
				([vl1,vl2,vl3],[c]) => xptrupdate(c,vl1,vl2,vl3)
			      | _ => error "update given bad arguments")
	     | floatupdate1 => (case (vl_list,clist) of
				  ([vl1,vl2,vl3],[c]) => xfloatupdate(vl1,vl2,vl3)
				| _ => error "update given bad arguments")
	     | (array1 _) => (case (vl_list,clist) of
				  ([vl1,vl2],[c]) => xarray(c,vl1,vl2)
				| _ => error "array/vector given bad arguments")

	     | array_eq true =>  (case (clist,vl_list) of
				  ([c],[vl1,vl2]) => xeqarray(c,vl1,vl2)
				| _ => error "array_eq given bad arguments")
	     | array_eq false => raise XXX (* not the same as array equality *)
	     | output =>
		   let 
		       val outvar = fresh_var()
		       val argvar = fresh_var()
		       val label = C_EXTERN_LABEL "ml_output"
		       val ir_arg = load_ireg_locval((case vl_list of
							  [vl] => vl
							| _ => error "bad output"), NONE)
		       val string_con = Prim_c(Vector_c,[Prim_c(Int_c W8,[])])
		       val tipe = AllArrow_c(Code,Partial,[],[Prim_c(Int_c W32,[]),string_con],0w0,unit_con)
		       val _ = add_varloc(outvar,VCODE label,tipe)
		       val _ = add_var(argvar,I ir_arg, string_con)
		       val elist' = [Const_e (Prim.int(Prim.W8,TilWord64.one)), Var_e argvar]
		   in xexp(fresh_var(),App_e(Code,Var_e outvar,[],elist',[]),NONE,context)
		   end
	     | input => raise XXX)
      end


  (* ---------------- array and vector operations --------------------
     --- the array/vector type may or may not be statically known ---- *)
  and xfloatsub(vl1 : loc_or_val, vl2 : loc_or_val) : regf = 
      let
	  val a' = load_ireg_locval(vl1,NONE)
	  val destf = alloc_regf()
	  val _ =  (case (in_ea_range 8 vl2) of
			SOME i => add_instr(LOADQF(EA(a',i),destf))
		      | NONE =>
			    let val b' = load_ireg_locval(vl2,NONE)
				val addr = alloc_regi (LOCATIVE)
			    in  add_instr(S8ADD(b',REG a',addr));
				add_instr(LOADQF(EA(addr,0),destf))
			    end)
      in  destf
      end

  and xintptrsub(c, vl1 : loc_or_val, vl2 : loc_or_val) : regi =
      let
	  val a' = load_ireg_locval(vl1,NONE)
	  val desti = alloc_regi(con2rep c)
	  val _ = (case (in_ea_range 4 vl2) of
		       SOME i => add_instr(LOAD32I(EA(a',i),desti))
		     | NONE => let val b' = load_ireg_locval(vl2,NONE)
				   val addr = alloc_regi (LOCATIVE)
			       in  add_instr(S4ADD(b',REG a',addr));
				   add_instr(LOAD32I(EA(addr,0),desti))
			       end)
      in desti
      end

  and xsub(c, vl1 : loc_or_val, vl2 : loc_or_val) : loc_or_val * con =
      let
	  fun floatcase() = xfloatsub(vl1,vl2)
	  fun nonfloatcase() = xintptrsub(c,vl1,vl2)
	  val r = (case (simplify_type c) of
		       (true,Prim_c(Float_c F64,[])) => I(boxFloat (floatcase()))
		     | (true,_) => I(nonfloatcase())
		     | (false,c') => let val (r,_) = xcon(fresh_var(),c')
					 val tmp = alloc_regi NOTRACE_INT
					 val desti = alloc_regi(con2rep c)
					 val afterl = alloc_code_label()
					 val floatl = alloc_code_label()
					 val _ = (add_instr(CMPUI(EQ, r, IMM 7, tmp));
						  add_instr(BCNDI(NE,tmp,floatl,false)))
					 val desti = nonfloatcase()
					 val _ = add_instr(BR afterl)
					 val _ = add_instr(ILABEL floatl)
					 val destf = floatcase()
					 val boxi = boxFloat destf
					 val _ = (add_instr(MV(boxi,desti));
						  add_instr(ILABEL afterl))
				     in  I(desti)
				     end)
      in (VAR_LOC(VREGISTER r), c)
      end

  and xfloatupdate(vl1 : loc_or_val, vl2 : loc_or_val, vl3 : loc_or_val) : loc_or_val * con =
      let val a' = load_ireg_locval(vl1,NONE)
	  val argf = load_freg_locval(vl3,NONE)
      in (case (in_ea_range 8 vl2) of
	      SOME i => add_instr(STOREQF(EA(a',i),argf))
	    | NONE => let val b' = load_ireg_locval(vl2,NONE)
			  val addr = alloc_regi (LOCATIVE)
		      in  add_instr(S8ADD(b',REG a',addr));
			  add_instr(STOREQF(EA(addr,0),argf))
		      end);
	  unitval
      end

  and xintupdate(c, vl1 : loc_or_val, vl2 : loc_or_val, vl3 : loc_or_val) : loc_or_val * con =
      let
	  val a' = load_ireg_locval(vl1,NONE)
	  val argi = load_ireg_locval(vl3,NONE)
      in  (case (in_ea_range 4 vl2) of
	       SOME i => add_instr(STORE32I(EA(a',i),argi))
	     | NONE => let val b' = load_ireg_locval(vl2,NONE)
			   val addr = alloc_regi (LOCATIVE)
		       in  add_instr(S4ADD(b',REG a',addr));
			   add_instr(STORE32I(EA(addr,0),argi))
		       end);
	  unitval
      end

  and xptrupdate(c, vl1 : loc_or_val, vl2 : loc_or_val, vl3 : loc_or_val) : loc_or_val * con =
      let
	  val a' = load_ireg_locval(vl1,NONE)
	  val argi = load_ireg_locval(vl3,NONE)
	  val addr = alloc_regi (LOCATIVE)
	  val wloc = alloc_regi (LOCATIVE)
      in  if (#do_write_list(!cur_params))
	      then (add_instr(NEEDMUTATE wloc);
		    (case (in_imm_range_vl vl2) of
			 SOME i' => add_instr(ADD(a',IMM (i'),addr))
		       | NONE => let val t = load_ireg_locval(vl2,NONE)
				 in  add_instr(ADD(a',REG t,addr))
				 end);
			 add_instr(STORE32I(EA(wloc,0),addr)))
	  else ();
	  xintupdate(c, vl1, vl2, vl3)
      end

  and xupdate(c, vl1 : loc_or_val, vl2 : loc_or_val, vl3 : loc_or_val) : loc_or_val * con =
      let
	  val r = (case (simplify_type c) of
		       (true,Prim_c(Float_c Prim.F64,[])) => (xfloatupdate(vl1,vl2,vl3); ())
		     | (true,Prim_c(Float_c Prim.F32,[])) => error "32-bit floats not done"
		     | (true,Prim_c(Int_c _,[])) => (xintupdate(c,vl1,vl2,vl3); ())
		     | (true,_) => (xptrupdate(c,vl1,vl2,vl3); ())
		     | (false,c') => let val (r,_) = xcon(fresh_var(),c')
					 val tmp = alloc_regi NOTRACE_INT
					 val afterl = alloc_code_label()
					 val floatl = alloc_code_label()
					 val intl = alloc_code_label()
					 val _ = (add_instr(CMPUI(EQ, r, IMM 7, tmp));
						  add_instr(BCNDI(NE,tmp,floatl,false)))
					 val _ = (add_instr(CMPUI(LE, r, IMM 4, tmp));
						  add_instr(BCNDI(NE,tmp,intl,false)))
					 val _ = xptrupdate(c,vl1,vl2,vl3)
					 val _ = add_instr(BR afterl)
					 val _ = add_instr(ILABEL intl)
					 val _ = xintupdate(c,vl1,vl2,vl3)
					 val _ = add_instr(BR afterl)
					 val _ = add_instr(ILABEL floatl)
					 val _ = xfloatupdate(vl1,vl2,vl3)
					 val _ = add_instr(ILABEL afterl)
				     in  ()
				     end)
      in unitval
      end

  and xeqarray(c, vl1 : loc_or_val, vl2 : loc_or_val) : loc_or_val * con =
      let val ir1 = load_ireg_locval(vl1,NONE)
	  val ir2 = load_ireg_locval(vl2,NONE)
	  val desti = alloc_regi NOTRACE_INT
	  val _ = add_instr(CMPUI(EQ,ir1,REG ir2,desti))
      in  (VAR_LOC(VREGISTER (I desti)),NilUtil.bool_con)
      end

  and xlength(c, vl : loc_or_val) : loc_or_val * con =
      let val dest = alloc_regi NOTRACE_INT
	  val src = load_ireg_locval(vl,NONE)
	  val _ = add_instr(LOAD32I(EA(src,~4),dest))
	  val _ = (case (simplify_type c) of
		       (true,Prim_c(Float_c Prim.F64,[])) => add_instr(SRL(dest,IMM real_len_offset,dest))
		     | (true,_) => add_instr(SRL(dest,IMM int_len_offset,dest))
		     | (false,c') => let val (r,_) = xcon(fresh_var(),c')
					 val tmp = alloc_regi NOTRACE_INT
					 val afterl = alloc_code_label()
					 val floatl = alloc_code_label()
				     in (add_instr(CMPUI(EQ, r, IMM 7, tmp));
					 add_instr(BCNDI(NE,tmp,floatl,false));
					 add_instr(SRL(tmp,IMM real_len_offset,dest));
					 add_instr(BR afterl);
					 add_instr(ILABEL floatl);
					 add_instr(SRL(tmp,IMM int_len_offset,dest));
					 add_instr(ILABEL afterl))
				     end)
      in  (VAR_LOC (VREGISTER (I dest)), Prim_c(Int_c Prim.W32, []))
      end


  and xarray(c, vl1 : loc_or_val, vl2 : loc_or_val) : loc_or_val * con =
    let val dest = alloc_regi TRACE
	val ptag = if (!HeapProfile) then MakeProfileTag() else (i2w 0)
	val after       = alloc_code_label()
	val bottom      = alloc_code_label();
	val top         = alloc_code_label()
	val small_alloc = alloc_code_label()
	val tag = alloc_regi(NOTRACE_INT)
	val gctemp  = alloc_regi(NOTRACE_INT)
	val cmptemp = alloc_regi(NOTRACE_INT)
	val i       = alloc_regi(NOTRACE_INT)
	val tmp     = alloc_regi(LOCATIVE)
	val len     = load_ireg_locval(vl1,NONE)
	val skiptag = alloc_regi(NOTRACE_INT)
	fun floatcase() = 
	    let 
		val v = load_freg_locval(vl2,NONE)
	    (* store object tag and profile tag with alignment so that
	     raw data is octaligned;  then loop through and initialize *)
	    in 
	       (* if array is too large, call runtime to allocate *)
	       add_instr(LI(i2w 4096,cmptemp));
	       add_instr(CMPUI(LE, len, REG cmptemp, cmptemp));
	       add_instr(BCNDI(NE,cmptemp,small_alloc,true));
	       add_instr(FLOAT_ALLOC(len,v,dest,ptag));
	       add_instr(BR after);
	     
	       (* inline allocation code - start by doing the tag stuff*)
	       do_code_align();
	       add_instr(ILABEL small_alloc);
	       add_instr(ADD(len,REG len, gctemp));
	       if (not (!HeapProfile))
		   then
		       (add_instr(ADD(gctemp,IMM 3, gctemp));
			add_instr(NEEDGC(REG gctemp));
			align_odd_word();
			mk_realarraytag(len,tag);
			add_instr(STORE32I(EA(heapptr,0),tag)); (* store tag *)
			add_instr(ADD(heapptr,IMM 4,dest)))
	       else
		   (add_instr(ADD(gctemp,IMM 4, gctemp)));
		   add_instr(NEEDGC(REG gctemp));
		   align_even_word();
		   store_tag_disp(0,ptag);                 (* store profile tag *)
		   mk_realarraytag(len,tag);               
		   add_instr(STORE32I(EA(heapptr,4),tag)); (* store tag *)
		   add_instr(ADD(heapptr,IMM 8,dest));
		   
		   (* now use a loop to initialize the data portion *)
		   add_instr(S8ADD(len,REG dest,heapptr));
		   add_instr(LI(Rtltags.skiptag, skiptag));
		   add_instr(STORE32I(EA(heapptr,0),skiptag));
		   add_instr(ADD(heapptr,IMM 4,heapptr));
		   add_instr(SUB(len,IMM 1,i));      (* init val *)
		   add_instr(BR bottom);             (* enter loop from bottom *)
		   do_code_align();
		   add_instr(ILABEL top);            (* loop start *)
		   add_instr(S8ADD(i,REG dest,tmp));
		   add_instr(STOREQF(EA(tmp,0),v));  (* initialize value *)
		   add_instr(SUB(i,IMM 1,i));
		   add_instr(ILABEL bottom);
		   add_instr(BCNDI(GE,i,top,true));
		   do_code_align();
		   add_instr(ILABEL after)
	    end (* end of floatcase *)
	local
	     val v = load_ireg_locval(vl2,NONE)
	     val len = load_ireg_locval(vl1,NONE)
	     fun general_init_case(gctemp_imm) = 
	       (if (not (!HeapProfile))
		  then
		    (add_instr(STORE32I(EA(heapptr,0),tag)); (* allocation *)
		     add_instr(ADD(heapptr,IMM 4,dest)))
		else
		  (store_tag_disp(0,ptag);
		   add_instr(STORE32I(EA(heapptr,4),tag)); (* allocation *)
		   add_instr(ADD(heapptr,IMM 8,dest)));
		  
		  (* gctemp's contents reflects the profile tag already *)
		  (case gctemp_imm of
		     NONE => add_instr(S4ADD(gctemp,REG heapptr,heapptr))
		   | (SOME n) => add_instr(ADD(heapptr, IMM (4*n), heapptr)));
		   add_instr(LI(Rtltags.skiptag, skiptag));
		   add_instr(STORE32I(EA(heapptr,0),skiptag));
		   add_instr(ADD(heapptr,IMM 4,heapptr));
		   add_instr(SUB(len,IMM 1,i));  (* init val and enter loop from bot *)
		   add_instr(BR bottom);
		   do_code_align();
		   add_instr(ILABEL top);        (* top of loop *)
		   add_instr(S4ADD(i,REG dest,tmp));
		   add_instr(STORE32I(EA(tmp,0),v)); (* allocation *)
		   add_instr(SUB(i,IMM 1,i));
		   add_instr(ILABEL bottom);
		   add_instr(BCNDI(GE,i,top,true));
		   do_code_align();
		   add_instr(ILABEL after))
	     fun general_intcase () = 
		    (add_instr(LI(i2w 4096,cmptemp));
		     add_instr(CMPUI(LE, len, REG cmptemp, cmptemp));
		     add_instr(BCNDI(NE,cmptemp,small_alloc,true));
		     add_instr(INT_ALLOC(len,v,dest,ptag));
		     add_instr(BR after);
		     do_code_align();
		     add_instr(ILABEL small_alloc);
		     add_instr(CMPUI(EQ,len,IMM 0, gctemp));
		     add_instr(ADD(gctemp,IMM 1, gctemp));
		     add_instr(ADD(gctemp,REG len, gctemp));
		     if (!HeapProfile)
			 then add_instr(ADD(gctemp, IMM 1, gctemp))
		     else ();
			 add_instr(NEEDGC(REG gctemp));
			 mk_intarraytag(len,tag);
			 general_init_case(NONE))
	     fun general_ptrcase() = 
		 (add_instr(LI((i2w 4096),cmptemp));
		  add_instr(CMPUI(LE, len, REG cmptemp, cmptemp));
		  add_instr(BCNDI(NE,cmptemp,small_alloc,true));
		  add_instr(PTR_ALLOC(len,v,dest,ptag));
		  add_instr(BR after);
		  do_code_align();
		  add_instr(ILABEL small_alloc);
		  add_instr(CMPUI(EQ,len,IMM 0, gctemp));
		  add_instr(ADD(gctemp,IMM 1, gctemp));
		  add_instr(ADD(gctemp,REG len, gctemp));
		  if (!HeapProfile)
		      then add_instr(ADD(gctemp, IMM 1, gctemp))
		  else ();
		      add_instr(NEEDGC(REG gctemp));
		      mk_ptrarraytag(len,tag);
		      general_init_case(NONE))
	   in
	       fun intcase() = 
		   case vl1 of
		       (VAR_VAL(VINT log_size)) => 
			   let val temp1 = if (!HeapProfile) then 1 else 0
			       val total_size = temp1 + 1 + 
				   (if (log_size = 0w0) then 1 else (w2i log_size))
			   in
			       if (in_imm_range(total_size)) then
				   (add_instr(NEEDGC(IMM total_size));
				    mk_intarraytag(len,tag);
				    general_init_case(SOME total_size))
			       else general_intcase()
			   end
		     | _ => general_intcase()
	       fun ptrcase() = 
		   case vl1 of
		       (VAR_VAL(VINT log_size)) => 
			   let val temp1 = if (!HeapProfile) then 1 else 0
			       val total_size = temp1 + 1 + 
				   (if (log_size = 0w0) then 1 else (w2i log_size))
			   in
			       if (in_imm_range(total_size)) then
				   (add_instr(NEEDGC(IMM total_size));
				    mk_ptrarraytag(len,tag);
				    if (log_size = 0w1) 
					then 
					    (if (not (!HeapProfile))
						 then
						     (add_instr(STORE32I(EA(heapptr,0),tag)); (* tag *)
						      add_instr(STORE32I(EA(heapptr,4),v));   (* data *)
						      add_instr(ADD(heapptr,IMM 4, dest));
						      add_instr(ADD(heapptr,IMM 8, heapptr)))
					     else
						 (store_tag_disp(0,ptag);
						  add_instr(STORE32I(EA(heapptr,4),tag)); 
						  add_instr(STORE32I(EA(heapptr,8),v)); 
						  add_instr(ADD(heapptr,IMM 8,dest));
						  add_instr(ADD(heapptr,IMM 12, heapptr))))
				    else general_init_case(SOME total_size))
			       else general_ptrcase()
			   end
		     | _ => general_ptrcase()
	   end
	val _ = (case (simplify_type c) of
		       (true,Prim_c(Float_c Prim.F64,[])) => floatcase()
		     | (true,Prim_c(Int_c (Prim.W8 | Prim.W16 | Prim.W32),[])) => intcase()
		     | (true,_) => ptrcase()
		     | (false,c') => let val (r,_) = xcon(fresh_var(),c')
					 val tmp = alloc_regi NOTRACE_INT
					 val afterl = alloc_code_label()
					 val floatl = alloc_code_label()
					 val intl = alloc_code_label()
					 val _ = (add_instr(CMPUI(EQ, r, IMM 7, tmp));
						  add_instr(BCNDI(NE,tmp,floatl,false)))
					 val _ = (add_instr(CMPUI(LE, r, IMM 4, tmp));
						  add_instr(BCNDI(NE,tmp,intl,false)))
					 val _ = ptrcase()
					 val _ = add_instr(BR afterl)
					 val _ = add_instr(ILABEL intl)
					 val _ = intcase()
					 val _ = add_instr(BR afterl)
					 val _ = add_instr(ILABEL floatl)
					 val _ = floatcase()
					 val _ = add_instr(ILABEL afterl)
				     in  ()
				     end)
    in  (VAR_LOC (VREGISTER (I dest)), Prim_c(Int_c Prim.W32, []))
    end


   (* ------------------- translate constructors ------------------------ *)

  and xcon (name : var, (* Purely for debugging and generation of useful names *)
	    arg_con : con     (* The expression being translated *)
	    ) : regi * kind = 
      let fun mk_ptr i = (load_ireg_val(VINT (TilWord32.fromInt i), NONE), Type_k Runtime)
	  fun mk_sum_help (isrec,indices,cons) = 
	      let val indices' = map (fn i => VAR_VAL(VINT (TilWord32.fromInt i))) indices
		  val con_kinds = map (fn c => xcon(fresh_named_var "xcon_sum",c)) cons
		  val cons' = map (fn c => VAR_LOC(VREGISTER(I(#1(xcon(fresh_named_var "xcon_sum",c)))))) cons
		  val reps = (map (fn _ => NOTRACE_INT) indices') @ (map (fn _ => TRACE) cons')
		  val kind = if isrec
				 then let fun help(n,(_,k)) = ((NilUtil.generate_tuple_label n,
								fresh_var()),k)
					  val temp = Listops.mapcount help con_kinds
				      in  Record_k(list2sequence temp)
				      end
			     else Type_k Runtime
	      in (make_record(NONE,reps, indices' @ cons'), kind)
	      end
	  fun mk_sum' (indices,cons) = mk_sum_help(false,indices,cons)
	  fun mk_sum (index,cons) = mk_sum_help(false,[index],cons)
	  open Prim
      in
	  (case arg_con of
	       Prim_c(Int_c W8, []) => mk_ptr 0
	     | Prim_c(Int_c W16, []) => mk_ptr 1
	     | Prim_c(Int_c W32, []) => mk_ptr 2
	     | Prim_c(Int_c W64, []) => mk_ptr 3
	     | Prim_c(Float_c F32, []) => mk_ptr 6
	     | Prim_c(Float_c F64, []) => mk_ptr 7
	     | Prim_c(BoxFloat_c F32, []) => mk_ptr 10
	     | Prim_c(BoxFloat_c F64, []) => mk_ptr 11
	     | Prim_c(Exn_c,[]) => mk_ptr 12
	     | Prim_c(Exntag_c,[]) => mk_ptr 13
	     | Prim_c(Array_c,[c]) => mk_sum(0,[c])
	     | Prim_c(Vector_c,[c]) => mk_sum(1,[c])
	     | Prim_c(Ref_c,[c]) => mk_sum(2,[c])
	     | Prim_c(Sum_c {known,tagcount},cons) => mk_sum'([4,(case known of
								      NONE => ~1
								    | SOME w => TilWord32.toInt w), 
							       TilWord32.toInt tagcount],cons)
	     | Prim_c(Record_c _,cons) => mk_sum_help(true,[5],cons)
	     | Prim_c(Vararg_c _,cons) => mk_sum(6,cons)
	     | Prim_c _ => error "ill-formed primitive type"
	     | Mu_c (vcset,v) => let val vclist = sequence2list vcset
				     val cons = map #2 vclist
				     fun loop [] _ = error "ill-formed Mu_c"
				       | loop ((v',_)::vrest) n = if (eq_var(v,v')) 
							     then n else loop vrest (n+1)
				     val which = loop vclist 0
				 in mk_sum'([7,which],cons)
				 end
	     | AllArrow_c (Open,_,_,_,_,_) => error "open Arrow_c"
	     | AllArrow_c (Closure,_,_,clist,numfloat,c) => 
		   mk_sum_help(false,[9,TilWord32.toInt numfloat],c::clist)
	     | AllArrow_c (Code,_,_,clist,numfloat,c) => 
		   mk_sum_help(false,[10,TilWord32.toInt numfloat],c::clist)
	     | Var_c v => let val (var_loc,k) = getconvarrep v
			      val ir = load_ireg_loc(var_loc,NONE)
			  in (ir,k)
			  end
	     | Let_c (letsort, cbnds, c) => (app xconbnd cbnds;
					     xcon(fresh_var(),c))
	     | Crecord_c lclist => 
		   let val vars = map (fn (l,_) => fresh_named_var (label2string l)) lclist
		       fun doer (v,(l,c)) = (l,v,xcon(v,c))
		       val lvregikind = Listops.map2 doer (vars,lclist)
		       val lvkList = map (fn (l,v,(_,k)) => ((l,v),k)) lvregikind
		       val kind = Record_k(list2sequence lvkList)
		       val vl = map (fn (_,_,(ir,_)) => VAR_LOC(VREGISTER(I ir))) lvregikind
		       val reps = map (fn _ => TRACE) vl
		       val ir = make_record(NONE,reps,vl)
		   in  (ir,kind)
		   end
	     | Proj_c (c, l) => 
		   let val (ir,k) = xcon(fresh_named_var "proj_c",c)
		       fun loop [] _ = error "ill-formed Mu_c"
			 | loop (((l',_),k)::vrest) n = if (eq_label(l,l')) 
							    then (n,k) else loop vrest (n+1)
		       val (which,fieldk) = (case k of 
						 Record_k lvk_seq => loop (sequence2list lvk_seq) 0
					       | _ => error "bad kind to proj_c from")
		       val dest = alloc_regi TRACE
		       val _ = add_instr(LOAD32I(EA(ir,4 * which),dest))
		   in (dest, fieldk)
		   end
	     | Closure_c (c1,c2) => mk_sum(8,[c1,c2])
	     | App_c (c,clist) => (* pass in env argument first *)
		   let val (clregi,k) = xcon(fresh_named_var "closure",c)
		       val resk = (case k of
				       Arrow_k(_,_,resk) => resk
				     | _ => error "bad kind to App_c")
		       val cregsi = map (fn c => #1(xcon(fresh_named_var "clos_arg",c))) clist
		       val coderegi = alloc_regi NOTRACE_CODE
		       val envregi = alloc_regi TRACE
		       val desti = alloc_regi TRACE
		       val _ = add_instr(CALL{func=REG' coderegi,
					      return=NONE,
					      args=(envregi :: cregsi,[]),
					      results=([desti],[]),
					      tailcall=false,
					      save=SAVE(getLocals())})
		   in (desti,resk)
		   end
	     | Annotate_c (_,c) => xcon(name,c))
      end

  

  (* -- create a record given a list of tagwords for the fields,
   the first n fields which are already in integer registers,
   and the rest of the fields, which are values *)

  and make_record (destopt, _ , []) = load_ireg_val(VINT 0w256,NONE)
    | make_record (destopt, reps : rep list, vl : loc_or_val list) = 
    let 
	val tagwords = mk_recordtag reps
	val dest = (case destopt of
			NONE => alloc_regi TRACE
		      | SOME d => d)
	val tagwords = 
	    if (not (!HeapProfile))
		then tagwords
	    else ({dynamic=nil,static=MakeProfileTag()}) :: tagwords
	val words_alloced = length vl+length tagwords
	fun scan_vals (offset,nil) = offset
	  | scan_vals (offset,vl::vls) =
	    let val r = load_ireg_locval(vl,NONE)
	    in  add_instr(STORE32I(EA(heapptr,offset),r)); (* allocation *)
		scan_vals(offset+4,vls)
	    end
	fun do_dynamic (r,{bitpos,path}) =
	    let val tipe = 
		case path of
		    Var_p regi => regi
		  | Projvar_p (regi,i) =>
			let val tipe = alloc_regi TRACE
			in add_instr(LOAD32I(EA(regi,i*4),tipe));
			    tipe
			end
		  | Label_p label =>
			let val addr = alloc_regi LABEL
			    val tipe = alloc_regi TRACE
			in add_instr(LADDR(label,0,addr));
			    add_instr(LOAD32I(EA(addr,0),tipe));
			    tipe
			end
		  | Projlabel_p(label,i) =>
			let val addr = alloc_regi LABEL
			    val addr' = alloc_regi LABEL
			    val tipe = alloc_regi TRACE
			in add_instr(LADDR(label,0,addr));
			    add_instr(LOAD32I(EA(addr,0),addr'));
			    add_instr(LOAD32I(EA(addr',i*4),tipe));
			    tipe
			end
		  | Notneeded_p => error "record: Notneeded_p hit"
		val tmp1 = alloc_regi NOTRACE_INT
		val tmp2 = alloc_regi NOTRACE_INT
	    in add_instr(LI(i2w 0,tmp1));
		add_instr(CMV(NE,tipe,IMM 1,tmp1));
		add_instr(SLL(tmp1,IMM (bitpos-1),tmp2));
		add_instr(ORB(tmp2,REG r,r))
	    end
	
      fun scantags (offset,nil : Rtltags.tags) = offset
	| scantags (offset,({static,dynamic}::vl) : Rtltags.tags) =
	  let val r = alloc_regi(NOTRACE_INT)
	  in 
	      add_instr (LI(static,r));
	      app (fn a => do_dynamic(r,a)) dynamic;
	      add_instr(STORE32I(EA(heapptr,offset),r)); (* allocation *)
	      scantags(offset+4,vl)
	  end
      val _ = add_instr(NEEDGC(IMM(words_alloced)));
      val offset = 0
      val offset = scantags(offset,tagwords);
      val offset = scan_vals (offset, vl)
      val _ = (add(heapptr,4 * length tagwords,dest);
	       add(heapptr,4 * words_alloced,heapptr))
    in   dest
    end
  


  local 
      fun doconfun is_top (name,vklist,body,kind) = 
	  let 
	      val iargs = map (fn (v,k) => let val r = alloc_named_regi v TRACE
					   in  (add_convar(v,r,k); r)
					   end) vklist
	      val args = (iargs,[])
	      val resulti = alloc_regi TRACE
	      val results = ([resulti],[])
	      val return = alloc_regi(LABEL)
	      val _ = reset_state(is_top,name, args, I resulti,return)
	      val {name,revcode} = get_state()
	      val {name,revcode} = get_state()
	      val (ir,k) = xcon(fresh_named_var "result",body)
	      val mvinstr = MV(ir,resulti)
	      val revcode = (RETURN return)::mvinstr::revcode
	      val p = PROC{name=name,
			   return=return,
			   args=args,
			   results=results,
			   code=Array.fromList(rev revcode),
			   known=false,
			   save=SAVE(nil,nil),
			   vars=NONE}
	  in add_proc p
	  end
     fun dofun_help is_top (v,Function(effect,recur,vklist,vclist,vflist,body,con)) = 
	  let 
	      val cargs = map (fn (v,k) => 
			       let val r = alloc_named_regi v TRACE
				   val _ = add_convar(v,r,k)
			       in  r
			       end) vklist
	      val eiargs = map (fn (v,c) => 
				let val r = alloc_named_reg(c,v)
				    val _ = add_var(v,r,c); 
				in case r of
				    I ir => ir
				  | _ => error "can't have float register for general arguments"
				end) vclist
	      val efargs = map (fn v => let val fr = alloc_named_regf v
					    val _ = add_var(v,F fr,Prim_c(Float_c Prim.F64,[]))
					in  fr
					end) vflist
	      val args = (cargs @ eiargs, efargs)
	      val result = alloc_reg con
	      val results = (case result of
				 I ir => ([ir],[])
			       | F fr => ([],[fr]))
	      val return = alloc_regi(LABEL)
	      val _ = reset_state(is_top, v, args, result,return)
	      val (r,c) = xexp'(fresh_named_var "result",body,
				SOME con, ID return)
	      val {name,revcode} = get_state()
	      val mvinstr = (case (r,result) of
				 (I ir1,I ir2) => MV(ir1,ir2)
			       | (F fr1,F fr2) => FMV(fr1,fr2)
			       | _ => error "register mismatch")
	      val revcode = (RETURN return)::mvinstr::revcode
	      val p = PROC{name=name,
			   return=return,
			   args=args,
			   results=results,
			   code=Array.fromList(rev revcode),
			   known=false,
			   save=SAVE(nil,nil),
			   vars=NONE}
	  in  p
	  end
     fun dofun arg = add_proc(dofun_help false arg)
  in
      fun dofun_top arg = dofun_help true arg
      fun worklist_loop () = 
	  (case getWork() of
	       NONE => ()
	     | SOME (FunWork vf) => (print "calling dofun\n";
				     dofun vf; worklist_loop())
	     | SOME (ConFunWork vvkck) => (print "calling doconfun\n";
					   doconfun false vvkck; worklist_loop()))
  end

   fun translate trans_params ({bnds : bnd list,
				name_c : var,
				name_r : var,
				type_r : con,
				knd_c : kind}) = 
         let 
	     val exp = Let_e(Sequential,bnds,Var_e name_r)
	     val con = type_r
	     val _ = cur_params := trans_params
	     val _ = (case trans_params of
			  { HeapProfile = SOME c, ...} => (HeapProfile := true;
							   SetHeapProfileCounter c)
			| {HeapProfile = NONE, ...} => HeapProfile := false)

		 
	    (* translate the expression as a function taking no arguments *)
	     val mainName = fresh_named_var "main"
	     val _ = gvarmap := mk_var_hash_table(128,NotFound)
	     val _ = gconvarmap := mk_var_hash_table(128,NotFound)
		 (* --- need to add locations of certain vars with labels for exports *)
	     val PROC{name,return,args,results,code,known,save,vars} =
		 dofun_top (mainName,Function(Partial,Nonleaf,[],[],[],exp,con))
	     val p' = PROC{name=name,
			   return=return,
			   args=args,
			   results=([],[]),
			   code=code,
			   known=known,
			   save=save,
			   vars=vars}
	     val _ = add_proc p'
	     val _ = worklist_loop()

	     val module = MODULE {procs = rev (!pl),
				  data = Array.fromList (rev(!dl)),
				  main=LOCAL_CODE mainName,
				  mutable_objects = !mutable_objects,
				  mutable_variables = !mutable_variables}
	 in module
	 end

end;
