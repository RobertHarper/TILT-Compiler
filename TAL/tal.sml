(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* x86tal.mli
 * 
 * This is a fairly complete specification of the abstract syntax for
 * a typed version of 32-bit (flat-model) iNTEL 80386, Pentium, Pentium Pro,
 * and/or Pentium II assembly language.  
 *
 * TODO:  1. floating point
 *
 * Ported to Standard ML by Leaf Petersen (2003)
 *
 *)

(**********************************************************************)
(* Miscellanous stuff *)

structure Tal = 
struct
(*  structure N = Numtypes*)

  structure LabelSet = Name.LabelSet
  structure LabelMap = Name.LabelMap
  val error = fn s => Util.error "tal.sml" s

  type int8 = TilWord32.word
  type int16 = TilWord32.word
  type int32 = TilWord32.word

  type f32 = string
  type f64 = string 
  val int32_to_int = TilWord32.toInt

  (* TAL doesn't have a label/var distinction, unfortunately.  *)
  type identifier = Name.label
  type label = Name.label


  datatype mode = Abs | Rel

  datatype scale = datatype Lil.size
(*
  fun scale_to_int32 s =
    case s of
      B1 => N.i32_1
    | B2 => N.i32_2
    | B4 => N.i32_4
    | B8 => N.i32_8

  fun scale_to_int s = N.int32_to_int(scale_to_int32 s)
*)
  datatype reg = Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | Virt of identifier

  (* For part word stuff: e = 32bit, x = 16bit, h = "high" 8bit, l = low 8bit *)
  datatype reg_part = RPe | RPx | RPh | RPl

  (**********************************************************************)
  (* Kinds *)

  (* Kbyte <= Ktype, Kmemi <= Kmem *)
  datatype rkind = 
    Kbyte of scale	    (* describes types of 8/16/32/64 bit values *)
    | Ktype   		    (* describes types of all values *)
    | Kmemi of int32        (* types for memory of size i *)
    | Kmem                  (* types for memory or heap blocks *)
    | Kstack  		    (* describes types of the stack & pointers into it *)
    | Kint                  (* integer kind *)
    | Kbool                 (* boolean kind *) 
    | Karrow of kind * kind (* functions from constructors to constructors *)
    | Kprod of kind list    (* tuples of constructors *)
    | Kname                 (* "names" used for alias information *)
    | Kcap                  (* capabilities for alias information *)
    | Kms                   (* machine states *)
    (* ----- LX kinds ----- *)
    | Ksum of kind list
    | Kvar of identifier
    | Kmu of (identifier * kind) list * identifier 
  (* ----- end LX ----- *)
    
  (* LX *)
  withtype kind =
    { 
     rkind : rkind,                                (* the raw kind *)
     freekindvars : LabelSet.set option ref,         (* free kind vars *)
     kabbrev : identifier option ref               (* is this kind an abbreviation *)
     } 

  type kmu_schema = (identifier * kind) list 


    (* helper functions for creating kinds *)
  fun defkind (k : rkind) : kind = { rkind = k, freekindvars = ref NONE, kabbrev = ref NONE }

  fun defprimkind (k : rkind) : kind = { rkind = k, freekindvars = ref (SOME (LabelSet.empty)), kabbrev = ref NONE}

  (* kind constructors *)
  fun kbyte s = defprimkind (Kbyte s)
  val ktype = defprimkind Ktype
  fun kmemi i = defprimkind (Kmemi i)
  val kmem = defprimkind(Kmem)
  val kstack = defprimkind Kstack
  val kint = defprimkind Kint
  val kbool = defprimkind Kbool
  fun karrow k1 k2 = defkind (Karrow (k1,k2))
  fun kprod ks = defkind (Kprod ks)
  val kunit = defprimkind(Kprod [])
  fun ksum ks = defkind (Ksum ks)
  fun kvar i = {rkind=Kvar i, freekindvars = ref (SOME (LabelSet.singleton i)), kabbrev= ref NONE}
  fun kmu i l = defkind (Kmu (i,l))
  val kname = defprimkind Kname
  val kcap = defprimkind Kcap
  val kms = defprimkind Kms

  val k1byte = kbyte B1 (* 32 bit values *)
  val k2byte = kbyte B2 (* 64 bit values *)
  val k4byte = kbyte B4 (* 32 bit values *)
  val k8byte = kbyte B8 (* 64 bit values *)

  fun kbyte s = 
    (case s
       of B1 => k1byte
	| B2 => k2byte
	| B4 => k4byte
	| B8 => k8byte)
    
  (**********************************************************************)
  (* Type Constructors *)
    
  (* primitive constructors *)
  datatype primcon = 
    PCbytes of scale        (* : Kbyte s *)
    | PCfloat32             (* : Kbyte Byte4 *)
    | PCfloat64             (* : Kbyte Byte8 *)
    | PCjunk of int32       (* : Uninitialized junk on the stack or in memory *)
    | PCjunkbytes of scale  (* : junk of kind Kbyte s *)
    | PCint of int32        (* : Kint  *)
    | PCtrue                (* : Kbool *)
    | PCfalse               (* : Kbool *)
  (* fields in tuples & arrays have variances:
   * readonly, writeonly, readwrite *)
  datatype variance = Read | Write | ReadWrite;;

  (* arithmetic and logical operators *)
  datatype log =
    Cadd    (* 32 bit add *)
    | Csub  (* 32 bit sub *)
    | Cmuls (* 32 bit signed mul *)
    | Cmulu (* 32 bit unsigned mul *)
    | Cand  (* logical and *)
    | Cor   (* logical or *)
    | Cimp  (* logical implication *)
    | Ciff  (* logical bi-implication *)
    | Cnot  (* logical not *)
    | Clts  (* signed less than *)
    | Cltu  (* unsigned less than *)
    | Cltes (* signed less than or equal to *)  
    | Clteu (* unsigned less than or equal to *)
    
  (* alias information *)
  datatype alias_info = Unique | MayAlias;;

  (* floating point stack: 8 fp register tags (ST(0),ST(1),...,ST(7)) *)
  datatype  fpreg   = FPempty | FPfull | FPany
  type fpstack = fpreg * fpreg * fpreg * fpreg * fpreg * fpreg * fpreg * fpreg


  datatype con_state = NotNorm | Normalized | WeakHead

  (* free kind and con vars of rcon *)

    
  (* Get around withtype scoping by explicitly parameterizing *)
  type ('con,'ccinfo) pre_machine_state = 
    {
     ms_eax : 'con option,
     ms_ebx : 'con option,
     ms_ecx : 'con option,
     ms_edx : 'con option,
     ms_esi : 'con option,
     ms_edi : 'con option,
     ms_ebp : 'con option,
     ms_esp : 'con option,
     ms_fpstack : fpstack,
     ms_cc : 'ccinfo ref,
     ms_save_cc : 'ccinfo,
     ms_cap : 'con
     }

  datatype rcon = 
    (* the language portion of con's *)
    Cvar of identifier
    | Clam of identifier * kind * con
    | Capp of con * con
    | Ctuple of con list
    | Cproj of int32 * con
    (* ---- LX ---- *)
    | Cinj of int32 * con * kind
    | Ccase of con * identifier * con list
    | Cfold of kind * con
    | Cpr of identifier * (identifier * identifier * kind * identifier * kind * con) list
       (* primitive recursion
	  a set of mututally recursive functions, indexed by the third identifier *)
    | Cvoid of kind
    (* -- end LX -- *)
    (* the "type" portion of con's *)
    | Clab of label                     (* a generative, globally-scoped 
					 * type.  See Glew & Morrisett's
					 * MTAL paper for description. *)
    | Cprim of primcon  (* see above *)
    | Crec of (identifier * kind * con) list  (* mutually recursive con's *)
    | Cforall of identifier * kind * con      (* forall-polymorphism *)
    | Cexist of identifier * kind * con * con (* E[i:k such that c1].c2 *)
    | Ccode of con                            (* Kms -> K4byte *)
    | Cms of (con,ccinfo) pre_machine_state 
                                            (* describes an entry point in 
					     * a hunk of code.  The machine
					     * state is described below and
					     * is in essence a pre-condition
					     * that must be satisfied before
					     * control may be transferred to
					     * this code. *)
    | Cmsjoin of con * con
	(* constructor on the right overwrites register bindings on the left *)
    | Chptr of int32 list*con option*(con*variance) option
	(* A (potential) pointer to a heap-allocated object
	 * (i.e., a Cprod, Csum, Carray, etc.)
	 * In reality, an object with this type can be any one of
	 * the integers listed (in the range [0..4095]) or a pointer
	 * to a heap-allocated object of kind Kmem described by the
	 * con option.  The con*variance option is a potential "tag"
	 * as described in Glew's ICFP paper.  NEAL:  fill in some 
	 * details here.  
	 *)
    | Cfield of con*variance             (* a "field" in a struct type *)
    | Cprod of con list                  (* a struct -- made of fields *)
    | Csum of con list                    (* a disjoint union -- the cons must
					 * be Cprod's where the first field
					 * is a unique singleton int. *)
    | Carray of con*con       		(* Kint,Kmem -> Kmem *)
    | Csing of con             		(* Kint -> K4byte *)
    (* the "stack" portion of con's *)
    | Csptr of con                        (* Kstack -> K4byte *)
    | Cempty
    | Ccons of con * con                  (* Kmem/Kbyte,Kstack -> Kstack *)
    | Cappend of con * con                (* Kstack,Kstack -> Kstack *)
    (* the arithmetic and logical portion of the con's *)
(*    | Clog of log * con list
    | Cif of con * con (* if c1:bool then c2 else void *)
*)    (* the alias information of con's *)
    (* Alias and capability portion of con's:
   For various reasons, we need to be able to track certain aliasing
   relationships among values.  To do so, we use "names" (identifiers
   of kind Kname) to statically name a dynamic location.  If both Eax and
   Ebx have type Cname(x) where x is a name, then we know that Eax and
   Ebx contain the same value.  We separately keep track of the type
   of x in a data structure called the capability.  For instance, the
   capability might say that at a control-flow point, x has type 
   ^*[B4,B4] indicating that Eax and Ebx contain the same pointer to
   a pair of 4-byte integers.  In addition, the capability tracks
   whether x is a "unique" location or whether x may have aliases.
   Unique names' types can change.  For instance, if x is unique, then
   we can write Ebx into the first field of Eax and x will now have
   the (recursive) type ^*[Cname(x),B4].  (Allocation returns unique
   names and intialization changes the type of the name.)  In this
   respect, unique names are like linear types, but the level of 
   indirection allows us to have multiple copies of the value.  If x
   is not unique, then it may have aliases and thus we cannot change
   the type of x arbitrarily.  However, we can refine the type of x
   (if for instance, x is a disjoint union of some sort.)  The ability
   to refine the type of a location (as opposed to a register/stack slot/
   etc.) allows us to simultaneously refine the type of a bunch of copies
   of some object.  If in the example above, we try to call a function
   where Eax and Ebx are supposed to have type ^*[B4,B4] (and not Cname(x))
   then if x is unique, the call will fail.  This prevents one from hiding
   an alias to a supposedly unique location  That is, when x is unique,
   a value of type Cname(x) cannot just be coerced to ^*[B4,B4].  However
   if x may alias, then a value of type Cname(x) may be coerced to 
   the type that x has, thereby "forgetting" the aliasing relationships
   with other values.  
 *)
    | Cname of con
    | Ccap of (alias_info*con) LabelMap.map
    | Cjoin of con list
    | Ctagof of con (* :Kname -> K4byte the tag of a sum value *)
(*    (* Cyclone *)
    | Ctmpl of con * con option
    * (identifier * con) list * (identifier * con) list
    | Ctptr of identifier
    | Ctrgn of con * con option
    * (identifier * (identifier * con) list * (identifier * con) list) list
    (* End Cyclone *)
    (* an explicit substitution applied to a con *)
    | Csubst of con * esubst
    (* Enil - empty substitution, Es replace var with con, Eo left-to-composition*)
    | Cr of rep_item (* type of a type representation *)
    (* the type hack portion*)
    | Ctypeof of identifier
    *)
(*  and esubst = Enil | Es of identifier * con | Eo of esubst * esubst*)
  and ccinfo =
    CCnoinfo
    | CCcmp of con*con
    | CCtest of con*con
(*  and rep_item = RCon of con | RKind of kind | RLabel of identifier*)
    
  withtype con = 
    { 
     rcon     : rcon ref,   (* "raw" constructor *)
     con_state : con_state ref,
     freevars : (LabelSet.set * LabelSet.set) option ref,
     (*     hash : int ref; *)
     abbrev : identifier option ref  (* is this con an abbreviation *)
     } 

  (* free kind and con vars of rcon *)

  type machine_state = (con,ccinfo) pre_machine_state

  fun invalid_arg s = Util.error "tal.sml" ("Invalid arg: "^s)
  fun string_of_int i = Int.toString i
  fun string_of_int32 i = TilWord32.toDecimalString i
  fun string_of_int16 i = TilWord32.toDecimalString i
  fun string_of_int8 i = TilWord32.toDecimalString i
  fun id_to_string v = Name.label2string v
  fun lbl_to_string v = Name.label2string v

    (* floating point registers and stack *)
  fun get_fpreg fps i = 
    let val (a,b,c,d,e,f,g,h) = fps in 
      case i of
	0 => a
      | 1 => b
      | 2 => c
      | 3 => d
      | 4 => e
      | 5 => f
      | 6 => g
      | 7 => h
      | _ => (invalid_arg "get_fpreg: bad reg"; a)
    end

  fun set_fpreg fps i fpr = 
    let val (a,b,c,d,e,f,g,h) = fps in 
      case i of
	0 => (fpr,b,c,d,e,f,g,h)
      | 1 => (a,fpr,c,d,e,f,g,h)
      | 2 => (a,b,fpr,d,e,f,g,h)
      | 3 => (a,b,c,fpr,e,f,g,h)
      | 4 => (a,b,c,d,fpr,f,g,h)
      | 5 => (a,b,c,d,e,fpr,g,h)
      | 6 => (a,b,c,d,e,f,fpr,h)
      | 7 => (a,b,c,d,e,f,g,fpr)
      | _ => (invalid_arg "set_fpreg: bad reg"; fps)
    end

  val fpstack_empty = 
    (FPempty,FPempty,FPempty,FPempty,FPempty,FPempty,FPempty,FPempty)

  fun fpstack_isempty     fps   = fps = fpstack_empty
  fun fpstack_isempty_reg fps i = get_fpreg fps i = FPempty
  fun fpstack_isfull_reg  fps i = get_fpreg fps i = FPfull
    
  fun fpstack_equal fps1 fps2   = fps1 = fps2;;
    
  fun fpstack_leq fps1 fps2 = 
    let 
      fun aux i =
	if i < 8 then 
	  let 
	    val st1 = get_fpreg fps1 i 
	    val st2 = get_fpreg fps2 i 
	  in
	    if st2 = FPany then aux (i+1)
	    else if st1 = st2 then aux (i+1) else false
	  end
	else true 
    in aux 0
    end

  fun fpstack_get_fpreg fps i =
    if 0 <= i andalso i <=7 then get_fpreg fps i 
    else invalid_arg ("fpstack_get_fpreg: arg = " ^ (string_of_int i))

  fun fpstack_set_fpreg fps i fpr =
    if 0 <= i andalso i <= 7 then set_fpreg fps i fpr
    else invalid_arg ("fpstack_set_fpreg: arg = " ^ (string_of_int i))

  (* create stack with height i, i must be in the range 0..8 *)
  fun fpstack_create i =
    if 0 < i andalso i <= 8 then
      let fun aux fps j =
	if j < 0 then fps
	else aux (set_fpreg fps j FPfull) (j-1) 
      in aux fpstack_empty (i-1)
      end
    else
      invalid_arg 
      ("fpstack_create: cannot create stack with height "^(string_of_int i))
      

  (* initialize fp register, i must be in the range 0..7 *)
  fun fpstack_init_reg fps i = 
    if 0 <= i andalso i <= 7 then set_fpreg fps i FPfull
    else invalid_arg ("fpstack_init_reg: cannot initialize reg"^(string_of_int i))


  fun fpstack_hide_reg fps i =
    if 0 <= i andalso i <= 7 then set_fpreg fps i FPany
    else invalid_arg ("fpstack_hide_reg: cannot hide reg"^(string_of_int i))

  (* initialize fp register, i must be in the range 0..7 *)
  fun fpstack_free_reg fps i = 
    if 0 <= i andalso i <= 7 then set_fpreg fps i FPempty
    else invalid_arg ("fpstack_free_reg: cannot free reg"^(string_of_int i))


  (* true if ST(i) a valid reference in the fp stack *) 
  fun fpstack_inrange fps i = get_fpreg fps i = FPfull;;
    
  (* The following check overflow/underflow and raise FPstack_error "overflow"
   * or FPstack_error "underflow".  Overflow occurs when attempting to
   * push onto a slot that is already full. Underflow occurs when attempting
   * a pop of an empty register. (NOTE: INTEL manual appears to have an error
   * in its description of stack pop, which doesn't allow you to pop the last
   * element off the stack.  See Intel Arch. Software Developer's Manual,
   * volume 1,  page 7-20.  The manual is unclear, but it seems to state that
   * underflow is detected by checking to see if the result position of the top
   * pointer is empty instead of checking to see if the start position of the
   * top pointer is empty.  Here we check to see if the start position of the
   * top pointer is empty.  This way, we can pop the last element and move
   * the pointer to a position where it points to an empty slot.)
   * 
   * Negative numbers push; Positive numbers pop
   * Argument index must be in the range -2..2
  *)
  fun fpstack_adjust error fps i = 
    let 
      fun overflow  () = (error "overflow"; fps)
      fun underflow () = (error "underflow"; fps)
      val (a,b,c,d,e,f,g,h) = fps 
    in
      case i of
	0  => fps
      | ~1 => 
	if h = FPfull then overflow()
	else (FPfull,a,b,c,d,e,f,g)
      | ~2 =>
	if h = FPfull orelse g = FPfull then overflow()
	else (FPfull,FPfull,a,b,c,d,e,f)
      | 1  =>
        if a = FPempty then underflow()
	else (b,c,d,e,f,g,h,FPempty)
      | 2  => 
        if a = FPempty orelse b = FPempty then underflow()
	else (c,d,e,f,g,h,FPempty,FPempty)
      | _ => invalid_arg ("fpstack_adjust: cannot adjust by"^string_of_int i)
    end


  (* Bump the top-of-stack pointer by a max of 2 in either direction.  
   * Has the effect of rotating empty or filled stack slots with respect to
   * the top of the stack.
   * Positive int arguments rotate in the direction of popping, negative in the
   * direction of pushing. 
   * Do *not* fill or empty stack slots while rotating.  
   * Do *not* check for overflow or underflow.
   *)
  fun fpstack_rotate fps i =
    let val (a,b,c,d,e,f,g,h) = fps 
    in
      case i of
	0  => fps
      | ~1 => (h,a,b,c,d,e,f,g)
      | ~2 => (g,h,a,b,c,d,e,f)
      | 1  => (b,c,d,e,f,g,h,a)
      | 2  => (c,d,e,f,g,h,a,b)
      | _ =>  invalid_arg ("fpstack_rotate: cannot rotate by"^string_of_int i)
    end


  fun prim_string pc = 
    (case pc of
       PCbytes B1 => "b1"
     | PCbytes B2 => "b2"
     | PCbytes B4 => "b4"
     | PCbytes B8 => "b8"
     | PCfloat32 => "f4"
     | PCfloat64 => "f8"
     | PCjunk i => "junk"^(string_of_int(int32_to_int i))
     | PCjunkbytes B1 => "1junk"
     | PCjunkbytes B2 => "2junk"
     | PCjunkbytes B4 => "4junk"
     | PCjunkbytes B8 => "8junk"
     | PCint i => "int"^(string_of_int(int32_to_int i))
     | PCtrue => "true"
     | PCfalse => "false")

  fun rcon_string r = 
    (case r of
       Cvar x => "var("^(id_to_string x)^")"
     | Clam(x,k,c) => "lam"
     | Capp(c1,c2) => "app"
     | Ctuple(cs) => "tuple"
     | Cproj(i,c) => "proj"
     | Cinj(i,c,k) => "inj"
     | Ccase(c,x,cs) => "case"
     | Cfold(k,c) => "fold"
     | Cpr(x,ys) => "pr"
     | Cvoid k => "void"
     | Clab x => "lab("^(lbl_to_string x)^")"
     | Cprim pc => "prim("^(prim_string pc)^")"
     | Crec xkcs => "rec"
     | Cforall(x,k,c) => "forall"
     | Cexist(x,k,c1,c2) => "exist"
     | Ccode c => "code"
     | Cms ms => "ms"
     | Cmsjoin(c1,c2) => "msjoin"
     | Chptr(is,copt,cvopt) => "hptr"
     | Cfield(c,v) => "field"
     | Cprod cs => "prod"
     | Csum cs => "sum"
     | Carray(c1,c2) => "array"
     | Csing c => "sing"
     | Csptr c => "sptr"
     | Cempty => "empty"
     | Ccons(c1,c2) => "cons"
     | Cappend(c1,c2) => "append"
     | Cname c => "name"
     | Ccap d => "cap"
     | Cjoin cs => "join"
     | Ctagof c => "tagof"
	 )


  fun defcon (rc : rcon) : con =
    let 
      val con = { rcon = ref rc, con_state = ref NotNorm, freevars = ref NONE, 
		 abbrev = ref NONE
		 }
    in con
    end

  val empty_id_set = LabelSet.empty
  val empty_freevars = SOME(empty_id_set,empty_id_set)
  fun prcon (rc : rcon) : con = 
    let val con = defcon rc 
    in 
      #con_state con :=  Normalized;
      #freevars con :=  empty_freevars;
      con
    end

  fun wcon rc = 
    let val con = defcon rc in
      #con_state con := WeakHead;
      con
    end

  val cempty_cap = wcon (Ccap (LabelMap.empty))
  (* Machine states *)

  val ms_empty : machine_state =
    { ms_eax = NONE, ms_ebx = NONE, ms_ecx = NONE, ms_edx = NONE,
     ms_esi = NONE, ms_edi = NONE, ms_ebp = NONE, ms_esp = NONE,
     ms_fpstack=fpstack_empty,
     ms_cc=ref CCnoinfo, ms_save_cc=CCnoinfo, ms_cap=cempty_cap}

  fun ms_get_cap (ms : machine_state) = #ms_cap ms
  fun ms_get_reg (ms : machine_state) (r :reg) = 
    let 
      val copt = 
	case r of
	  Eax => #ms_eax ms
	| Ebx => #ms_ebx ms
	| Ecx => #ms_ecx ms
	| Edx => #ms_edx ms
	| Esi => #ms_esi ms
	| Edi => #ms_edi ms
	| Ebp => #ms_ebp ms
	| Esp => #ms_esp ms
	| _ => error "virtual registers unimplemented"
    in copt
    end 


  fun set_ms_eax { ms_eax = _  , ms_ebx , ms_ecx , ms_edx , ms_esi , ms_edi , ms_ebp , ms_esp , ms_fpstack , ms_cc , ms_save_cc , ms_cap } ms_eax =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_ebx { ms_eax , ms_ebx = _  , ms_ecx , ms_edx , ms_esi , ms_edi , ms_ebp , ms_esp , ms_fpstack , ms_cc , ms_save_cc , ms_cap } ms_ebx =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_ecx { ms_eax , ms_ebx , ms_ecx = _  , ms_edx , ms_esi , ms_edi , ms_ebp , ms_esp , ms_fpstack , ms_cc , ms_save_cc , ms_cap } ms_ecx =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_edx { ms_eax , ms_ebx , ms_ecx , ms_edx = _  , ms_esi , ms_edi , ms_ebp , ms_esp , ms_fpstack , ms_cc , ms_save_cc , ms_cap } ms_edx =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_esi { ms_eax , ms_ebx , ms_ecx , ms_edx , ms_esi = _  , ms_edi , ms_ebp , ms_esp , ms_fpstack , ms_cc , ms_save_cc , ms_cap } ms_esi =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_edi { ms_eax , ms_ebx , ms_ecx , ms_edx , ms_esi , ms_edi = _  , ms_ebp , ms_esp , ms_fpstack , ms_cc , ms_save_cc , ms_cap } ms_edi =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_ebp { ms_eax , ms_ebx , ms_ecx , ms_edx , ms_esi , ms_edi , ms_ebp = _  , ms_esp , ms_fpstack , ms_cc , ms_save_cc , ms_cap } ms_ebp =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_esp { ms_eax , ms_ebx , ms_ecx , ms_edx , ms_esi , ms_edi , ms_ebp , ms_esp = _  , ms_fpstack , ms_cc , ms_save_cc , ms_cap } ms_esp =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_fpstack { ms_eax , ms_ebx , ms_ecx , ms_edx , ms_esi , ms_edi , ms_ebp , ms_esp , ms_fpstack = _  , ms_cc , ms_save_cc , ms_cap } ms_fpstack =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_cc { ms_eax , ms_ebx , ms_ecx , ms_edx , ms_esi , ms_edi , ms_ebp , ms_esp , ms_fpstack , ms_cc = _  , ms_save_cc , ms_cap } ms_cc =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_save_cc { ms_eax , ms_ebx , ms_ecx , ms_edx , ms_esi , ms_edi , ms_ebp , ms_esp , ms_fpstack , ms_cc , ms_save_cc = _  , ms_cap } ms_save_cc =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun set_ms_cap { ms_eax , ms_ebx , ms_ecx , ms_edx , ms_esi , ms_edi , ms_ebp , ms_esp , ms_fpstack , ms_cc , ms_save_cc , ms_cap = _  } ms_cap =
    {
     ms_eax = ms_eax,
     ms_ebx = ms_ebx,
     ms_ecx = ms_ecx,
     ms_edx = ms_edx,
     ms_esi = ms_esi,
     ms_edi = ms_edi,
     ms_ebp = ms_ebp,
     ms_esp = ms_esp,
     ms_fpstack = ms_fpstack,
     ms_cc = ms_cc,
     ms_save_cc = ms_save_cc,
     ms_cap = ms_cap
     }

  fun ms_set_reg ms r c = 
    (case r of
       Eax => set_ms_eax ms (SOME c)
     | Ebx => set_ms_ebx ms (SOME c)
     | Ecx => set_ms_ecx ms (SOME c)
     | Edx => set_ms_edx ms (SOME c)
     | Esi => set_ms_esi ms (SOME c)
     | Edi => set_ms_edi ms (SOME c)
     | Ebp => set_ms_ebp ms (SOME c)
     | Esp => set_ms_esp ms (SOME c)
     | _ => error "virtual registers unimplemented")

  fun ms_set_regs ms rcs = 
    List.foldl (fn ((r,c),ms) => ms_set_reg ms r c) ms rcs

  fun ms_del_reg ms r = 
    (case r of
       Eax => set_ms_eax ms NONE
     | Ebx => set_ms_ebx ms NONE
     | Ecx => set_ms_ecx ms NONE
     | Edx => set_ms_edx ms NONE
     | Esi => set_ms_esi ms NONE
     | Edi => set_ms_edi ms NONE
     | Ebp => set_ms_ebp ms NONE
     | Esp => set_ms_esp ms NONE
     | _ => error "virtual registers unimplemented")

  fun ms_del_regs ms rl = List.foldl (fn (rl,ms) => ms_del_reg ms rl) ms rl


  fun ms_fold_reg f (ms : machine_state) a = 
    let 
      fun fopt r copt a = 
	case copt 
	  of NONE => a
	   | SOME c => f r c a 
      val a = fopt Esp (#ms_esp ms) a 
      val a = fopt Ebp (#ms_ebp ms) a 
      val a = fopt Edi (#ms_edi ms) a 
      val a = fopt Esi (#ms_esi ms) a 
      val a = fopt Edx (#ms_edx ms) a 
      val a = fopt Ecx (#ms_ecx ms) a 
      val a = fopt Ebx (#ms_ebx ms) a 
      val a = fopt Eax (#ms_eax ms) a 
    in a
    end


  fun ms_get_fpstack (ms : machine_state) = #ms_fpstack ms
  fun ms_get_cc (ms : machine_state) = !(#ms_cc ms)
  fun ms_restore_cc (ms : machine_state) = (#ms_cc ms) := #ms_save_cc ms


  (* "merges" to machine states -- this is only one possible definition but
   * it seems reasonable to me (JGM):  the registers from ms2 overwrite any
   * bindings of registers in ms1.  The cc, and save_cc fields are
   * overwritten only if they contain information.  The capability fields
   * are joined.  The latter is the only one that concerns me as it means
   * that for sure ms_join is not idempotent...
   *)
  fun ms_join (ms1 : machine_state) (ms2 : machine_state) : machine_state = 
    let 
      fun jopt copt1 copt2 = 
	case copt2 of
	  NONE => copt1
	| SOME _ => copt2 
      val eax = jopt (#ms_eax ms1) (#ms_eax ms2)
      val ebx = jopt (#ms_ebx ms1) (#ms_ebx ms2)
      val ecx = jopt (#ms_ecx ms1) (#ms_ecx ms2)
      val edx = jopt (#ms_edx ms1) (#ms_edx ms2)
      val esi = jopt (#ms_esi ms1) (#ms_esi ms2)
      val edi = jopt (#ms_edi ms1) (#ms_edi ms2)
      val ebp = jopt (#ms_ebp ms1) (#ms_ebp ms2)
      val esp = jopt (#ms_esp ms1) (#ms_esp ms2)
      val fps = 
	if fpstack_isempty (#ms_fpstack ms2) then #ms_fpstack ms1
	else #ms_fpstack ms2 
      val cc = case !(#ms_cc ms2) of CCnoinfo => #ms_cc ms1 | _ => #ms_cc ms2 
      val save_cc = 
	case #ms_save_cc ms2 of CCnoinfo => #ms_save_cc ms1 | _ => #ms_save_cc ms2
      val cap = defcon(Cjoin [#ms_cap ms1,#ms_cap ms2])
    in
      { ms_eax = eax, ms_ebx = ebx, ms_ecx = ecx, ms_edx = edx,
       ms_esi = esi, ms_edi = edi, ms_ebp = ebp, ms_esp = esp,
       ms_fpstack = fps,
       ms_cc      = cc,
       ms_save_cc = save_cc,
       ms_cap     = cap
       } 
    end


  fun pcbytes s = prcon (Cprim (PCbytes s))
  val cbyte8 = pcbytes B8
  val cbyte4 = pcbytes B4
  val cbyte2 = pcbytes B2
  val cbyte1 = pcbytes B1
  fun pcbytes s = 
    (case s
       of B1 => cbyte1
	| B2 => cbyte2
	| B4 => cbyte4
	| B8 => cbyte8)

  val pcfloat32 = prcon (Cprim PCfloat32)
  val pcfloat64 = prcon (Cprim PCfloat64)

  fun pcjunk i = prcon (Cprim (PCjunk i))
  val pcjunk4  = prcon (Cprim (PCjunkbytes B4))
  fun pcjunkbytes sc  = prcon (Cprim (PCjunkbytes sc))
  fun pcint  i = prcon (Cprim (PCint i))
  val pctrue   = prcon (Cprim PCtrue)
  val pcfalse  = prcon (Cprim PCfalse)

  fun cvar v =
  let 
    val con = defcon (Cvar v) 
    val () = 
      (case #freevars con of
	 (ref NONE) =>  (#con_state con :=  Normalized;
			 #freevars con := SOME(empty_id_set, LabelSet.singleton v))
       | _ => ())
  in
    con
  end

  fun clam v k c = wcon (Clam (v,k,c))
  fun capp c1 c2 = defcon (Capp (c1,c2))
  fun ctuple cs = wcon (Ctuple cs)
  fun cproj c i = defcon (Cproj (i,c))
    (* ---- LX ---- *)
  fun cinj i c k = wcon (Cinj(i,c,k))
  fun ccase c i cs = defcon (Ccase (c,i,cs))
  fun cfold k c = wcon (Cfold (k,c))
  fun cpr j l = wcon (Cpr (j,l))
  fun cvoid k = wcon (Cvoid k)
    (* -- end LX -- *)
  fun clab l = prcon (Clab l)
  fun crec vkcs = wcon (Crec vkcs)
  fun cforall i k c = wcon (Cforall (i,k,c))
  fun cexist i k c = wcon (Cexist (i,k,pctrue,c))
  fun cexistp i k c1 c2 = wcon (Cexist (i,k,c1,c2))
  fun cms ms = wcon (Cms ms)
  fun cmsjoin c1 c2 = defcon (Cmsjoin(c1,c2))
  fun ccode c = wcon (Ccode c)
  fun ccode_ms ms = ccode (cms ms)
  fun ccode_l rcs = ccode_ms (ms_set_regs ms_empty rcs)
  fun ccode_l_fps rcs fps = 
    ccode_ms (set_ms_fpstack (ms_set_regs ms_empty rcs) fps)
  fun ccode_l_cap rcs cap = 
    ccode_ms (set_ms_cap (ms_set_regs ms_empty rcs) cap)
  fun ccode_l_cap_fps rcs cap fps =
    ccode_ms (set_ms_fpstack (set_ms_cap (ms_set_regs ms_empty rcs) cap) fps)
  fun chptr is co tco = wcon (Chptr (is,co,tco))
  fun cptr c = wcon (Chptr ([],SOME c,NONE))
  fun cfield c v = wcon (Cfield (c,v))
  fun cprod cs = defcon (Cprod cs)
  fun cprod_b cs = cptr (cprod cs)
  fun csum cs = wcon (Csum cs)
  fun carray cl ce = wcon (Carray (cl,ce))
  fun csing c = wcon (Csing c)
  fun carray_s v ce =
    let 
      val cv = cvar v 
    in
      cexist v kint (cprod_b [cfield (csing cv) Read,carray cv ce])
    end

  fun csptr c = wcon (Csptr c)
  val cempty = prcon Cempty
  fun ccons c1 c2 = wcon (Ccons (c1,c2))
  fun cappend c1 c2 = 
    (case (#rcon c1,#rcon c2)
       of (ref Cempty,_) => c2
	| (_,ref Cempty) => c1
	| _ => defcon (Cappend (c1,c2)))

    
  (**********************************************************************)
  (* Instructions *)
  datatype annotate = (* Added by Dan *)
    Con        of con
    | AReg       of reg
    | StackTail  of reg * int
    | StackSlice of reg * int * int * con


  (* various coercions that only affect the type of a value/reg/path/etc *)
  datatype coercion =
    Pack of con * con  	 (* abstract a type: first con is hidden,
			  second con is existential *)
    | Tapp of annotate     (* instantiate type var *)
    | Roll of con        	 (* introduce recursive type *)
    | Unroll             	 (* eliminate recursive type *)
    | Tosum of con       	 (* coerce record/tag to a sum *)
    | Fromsum            	 (* coerce a unary-sum to a record *)
    | RollTosum of con   	 (* combined Tosum followed by Roll *)
    | Toarray of int32*int*con
    (* coerce record to an array/vector
     (offset,depth,element type) *)
    | Slot of int32*int32  (* coerce stack slot to junk *)
    | Subsume of con       (* subsumption *)
    | Forgetname           (* used to forget aliasing information for MayAlias
			    * names. *)
    | Prove                (* used to prove precondition on code. ie: eliminate
			    * Cif form *)

  type 'a coerce = 'a * coercion list (* (r,[c1;...;cn]) is c1(...cn(r)...) *)

  (* Operands for most instructions *)
  datatype genop =
    Immed of int32
    | Reg of reg
    | Addr of label
    | Prjr of reg coerce * int32 * (scale * reg) option
    | Prjl of label coerce * int32 * (scale * reg) option

    (* Note: Above/Below are unsigned, Greater/Less are signed *)
  datatype condition = 
    Above | AboveEq | Below | BelowEq | Eq | Greater | GreaterEq | Less
    | LessEq | NotEq | NotOverflow | NotSign | Overflow | ParityEven
    | ParityOdd | Sign

  fun negate_condition c = 
    (case c of
       Above => BelowEq
     | AboveEq => Below
     | Below => AboveEq
     | BelowEq => Above
     | Eq => NotEq
     | Greater => LessEq
     | GreaterEq => Less
     | Less => GreaterEq
     | LessEq => Greater
     | NotEq => Eq
     | NotOverflow => Overflow
     | NotSign => Sign
     | Overflow => NotOverflow
     | ParityEven => ParityOdd
     | ParityOdd => ParityEven
     | Sign => NotSign)

  datatype arithbin = Adc | Add | And | Imul2 | Or | Sbb | Sub | Xor
  datatype arithun = Dec | Inc | Neg | Not
  datatype arithmd = Div | Idiv | Imul1 | Mul
  datatype arithsr = Rcl | Rcr | Rol | Ror | Sal | Sar | Shl | Shr

  datatype conv = Cbw | Cdq | Cwd | Cwde

  datatype mallocarg = 
    Mbytes of scale
    | Mprod of mallocarg list
    | Mbytearray of scale * int32

  (* Operations that never take arguments *)
  datatype fpnoargs = 
    F2xm1 | Fabs | Fchs | Fclex | Fnclex | Fcompp | Fucompp | Fcos | Fdecstp 
    | Fincstp | Finit | Fninit  | Fld1 | Fldz | Fldpi | Fldl2e | Fldl2t 
    | Fldlg2 | Fldln2 | Fnop | Fpatan | Fprem | Fprem1 | Fptan | Frndint 
    | Fscale | Fsin | Fsincos | Fsqrt | Ftst | Fwait | Fxam | Fxtract 
    | Fyl2x | Fyl2xp1

  (* Floating point instruction argument configurations *)
  datatype fpargs =
    FPstack of int           (* ST(i) *)
    | FPstack2 of bool * int   (* true => ST, ST(i); false => ST(i),ST *)
    | FPgenop of scale * genop (* Explicit memory or register operand *)
    
  (* Operations that take arguments *)
  datatype fpsomeargs =
    (* generic binary instructions *)
    Fadd | Fcom | Fdiv | Fdivr | Fmul | Fsub | Fsubr | Fucom | Fxch
    (* integer instructions *)
    | Fiadd | Ficom | Ficomp | Fidiv | Fidivr | Fimul | Fisub | Fisubr
    (* instructions that pop an argument *)
    | Faddp | Fcomp | Fdivp | Fdivrp | Fmulp | Fsubp | Fsubrp | Fucomp
    (* unary load and store instructions *)
    | Fst | Fstp | Fist | Fistp | Fld | Fild
    (* change fp register tag to empty *)
    | Ffree
    (* comparison operations that write condition codes to main unit *)
    (* implemented only on the pentium pro and better processors *)
    | Fcomi | Fcomip | Fucomi | Fucomip
    (* Store Status Word *)
    | Fstsw | Fnstsw

  (* This is a subset of the x86 32-bit instructions that we might want to
   * cover.  Does not include floating-point support yet.
   *)
  datatype instruction = 
    ArithBin of arithbin * genop * genop
	                          (* binary arithmetic operation *)
    | ArithUn of arithun * genop  (* unary arithmetic operation *)
    | ArithMD of arithmd * genop  (* multiply/division *)
    | ArithSR of arithsr * genop * int32 option (* NONE = ECX, shift/rotate *)
    | Bswap of reg                (* toggle endianess *)
    | Call of genop coerce        (* push return addr, jump to label *)
    | Clc                      	  (* clear carry flag *)
    | Cmc                      	  (* toggle carry flag *)
    | Cmovcc of condition * reg * genop coerce
	                          (* conditional move *)
    | Cmp of (genop coerce) * (genop coerce) (* compare *)
    | Conv of conv                (* various 8/16/32 -> 16/32/64 ops *)
    | Imul3 of reg * genop * int32(* signed multiply 3 arg form *)
    | Int of int8               	(* interrupt:  system call *)
    | Into                        (* interrupt if overflow set *)
    | Jcc of condition * label coerce * instruction list option
	                          (* jump on condition *)
	                          (* instructions are no-ops but coerce the context *)
    | Jecxz of label coerce * instruction list option
                                  (* jump if ECX is zero *)
	                          (* instructions are no-ops but coerce the context *)
    | Jmp of genop coerce      	  (* jump *)
    | Lahf                     	  (* move flags into Eax (exc. overflow) *)
    | Lea of reg * genop          (* move effective address into register *)
    | Loopd of label coerce * bool option
                                  (* decrement ECX and if result nonzero jump
				   if bool present jump only if
				     nonzero Z flag equals the boolean *)
    | Mov of genop * (genop coerce)
                                  (* move, load, store *)
    | Movpart of bool * genop * reg_part * genop * reg_part
	                          (* Move/zero extend/sign/extend/trunc part word
                                   to another part word.  One genop must be a
                                   register. *)
    | Nop                      	  (* no-op *)
    | Pop of genop             	  (* stack pop *)
    | Popad                    	  (* pop all registers (32-bit) *)
    | Popfd                    	  (* pop eflags *)
    | Push of genop coerce     	  (* push onto stack *)
    | Pushad                   	  (* push all registers (32-bit) *)
    | Pushfd                   	  (* push eflags *)
    | Rdtsc                       (* Read time stamp counter in EDX:EAX *)
    | Retn of int32 option        (* return "near" (i.e., doesn't touch CS) *)
    | Sahf                     	  (* move ah into flags (exc. overflow) *)
    | Setcc of condition * genop  (* set dest=1/0 if condition is true/false *)
    | Shld of genop * reg * int32 option (* NONE = ECX, shift 64 *)
    | Shrd of genop * reg * int32 option (* NONE = ECX, shift 64 *)
    | Stc                      	  (* set carry flag *)
    | Test of genop * genop   	  (* test *)
    | Xchg of genop * reg         (* exchange *)
    (* operations specific to x86tal *)
    | Coerce of genop coerce 
    (* coerce an object.  The object's location is a "path" off of a register*)
    | CoerceName of identifier coerce
    (* coerce an object indirectly through a name *)
    | Comment of string
    | Fallthru of con list  
    (* only valid when preceeding a label L.  effectively, 
     * a jmp L[c1,...,cn]. *)
    | Malloc of identifier * int32 * (mallocarg option)
    (* Malloc(x,i,m) allocates an object of i bytes returning the pointer
     * in eax, and wrecking all other registers but ebx.  Upon return,
     * x is added as a Unique pointer to an object described by mallocarg.
     * If the mallocarg is not present, then we assume a tuple of 32-bit
     * words (i.e., i/4).  Upon return, eax has type Cname(x) so that 
     * it may be initialized.  
     *)
    | Unpack of identifier * reg * genop coerce
    (* effectively a move *)
    | Sunpack of identifier * genop
    (* mov to and from a stack slot *)
    | Nameobj of identifier * genop
    (* genop specifies a value in a register or memory.  We introduce
     * a new Kname (identifier) and replace the type of the object with 
     * Cname(identifier).  The identifier is assigned MayAlias in the 
     * capability.   Used to refine the type of something where we may
     * make multiple copies of it.
     *)
    | ForgetUnique of identifier
    (* ForgetUnique(x) assumes x is a unique Kname in the current capability.
     * Changes x to MayAlias which allows objects of type Cname(x) to be
     * coerced to the type that the capability assigns x.
     *)
    | RemoveName of identifier
    (* Removes the name from the current capability. Note that Cname(x) 
     * will still be a valid constructor -- you just won't be able to do
     * much with it.  *)
    (* Floating Point Instructions *)
    | FPnoargs of fpnoargs
    | FPsomeargs of fpsomeargs * fpargs
    (* LX Instructions *)
    | Letprod of identifier list * con 
    | Letroll of identifier * con
    | Vcase of int32 * con * identifier * genop coerce 
  (* end LX *)

  type code_block = label * con option * instruction vector

  (**********************************************************************)
  (* Static Data *)
    
  (* There are some other things we could add here such as exnnames *)

  datatype data_item =
    Dlabel of label coerce
    | Dbytes of string
    | D2bytes of int16
    | D4bytes of int32 coerce
    | Dfloat32 of f32
    | Dfloat64 of f64
    | Djunk
    | Dup
    | Ddown



  (* A datablock is one of the following:
   *   Exnname:
   *     Dexnname
   *     Cannot coerce
   *   Tuple:
   *     (Dlabel | Dbytes | D2bytes | D4bytes | Djunk)^*
   *     Which may then be coerced.
   *)

  (* int represents alignment in bytes -- defaults to 4. *)
  type data_block = label * int32 * con option * (data_item list) coerce

  (**********************************************************************)
  (* Compilation Units *)

  type kind_abbrev = identifier * kind     (* LX *)
  type con_abbrev = identifier * con

  datatype int_con_def = AbsCon | BoundCon of con | ConcCon of con

  type int_con = label * kind * int_con_def

  type tal_int =
    { 
     int_abbrevs : con_abbrev vector,
     int_kindabbrevs : kind_abbrev vector,  (* LX *)
     int_cons : int_con vector,
     int_vals : (label * con) vector
     }

  type tal_int_type = {
    it_cons : int_con list,
    it_vals : (label * con) list
  } 

  type con_block = (label * kind * con)

  type tal_imp = 
    { 
     imp_abbrevs : con_abbrev vector,
     imp_kindabbrevs : kind_abbrev vector, (* LX *)
     con_blocks : con_block vector,
     code_blocks : code_block vector,
     data_blocks : data_block vector
     } 


  datatype int_ref = 
    Int_filename of string
    | Int_data of string * tal_int

  type tal_pre_mod =
    { 
     import_refs : int_ref vector,
     export_refs : int_ref vector,
     pre_imp     : tal_imp
     } 

  type tal_mod =
    { 
     imports : tal_int vector,
     exports : tal_int vector,
     imp     : tal_imp
     } 

end
(* EOF: x86tal.ml *)
