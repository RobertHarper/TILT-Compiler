structure TalTranslationDefs :> TALTRANSLATIONDEFS = 
struct

  structure Dec = Deconstruct.Dec
  structure LU = LilUtil
  structure TA = TalAbbrev

  val error = fn s => Util.error "taltranslationdefs.sml" s

  structure K = 
    struct
      val T32 = Tal.k4byte
      val T64 = Tal.k8byte
      val TM = Tal.kmem
      fun Type s = Tal.kbyte s
      fun Arrow a b = Tal.karrow a b
      val Nat = Tal.kint
    end  (* structure K *)


  structure C = 
    struct


      val var = Tal.cvar
      val nat = Tal.pcint
      val unit = Tal.ctuple []
      val lam = Tal.clam
      fun lambda k c = 
	let
	  val a = Name.fresh_named_var "a"
	in lam a k (c a)
	end

      val app = Tal.capp
      fun appn c1 [] = c1
	| appn c1 (c2::cs) = appn (app c1 c2) cs
	
      val pair = fn c1 => fn c2 => Tal.ctuple [c1,c2]
      val pi1 = fn c => Tal.cproj c 0w0
      val pi2 = fn c => Tal.cproj c 0w1
      fun pr (j,(a,k),k',r,body) = Tal.cpr r [(j,a,k,r,k',body)]
      val sumcase = Tal.ccase
      fun inj i k c = Tal.cinj i c k
      val fold = Tal.cfold
      val ptr = Tal.cptr

    end  (* structure C *)

  structure T = 
    struct

      (* Abbrev will memoize on the variable, but may get reset.
       * Choose a single unique name for the abbreviation,
       * and then re-abbreviate it whenever it is used.
       *)
      fun abbrev s cmaker = 
	let
	  val a = Name.fresh_named_var s
	  fun f () = 
	    let
	      val c = cmaker ()
	      val () = TA.C.abbreviate a c
	    in C.var a
	    end
	in f
	end

      fun sz_abbrev name constr = 
	let
	  val item8  = abbrev (name ^"8") (constr Lil.B1)
	  val item16 = abbrev (name^"16") (constr Lil.B2)
	  val item32 = abbrev (name^"32") (constr Lil.B4)
	  val item64 = abbrev (name^"64") (constr Lil.B8)
	  fun eta_constr sz = 
	    (case sz
	       of Lil.B1 => item8()
		| Lil.B2 => item16()
		| Lil.B4 => item32()
		| Lil.B8 => item64())
	in eta_constr
	end


      fun array_con sz () = 
	let 
	  val array_size_var = Name.fresh_named_var "?sz"
	  val array_arg_var = Name.fresh_named_var "t"
	  val cv = Tal.cvar array_size_var 
	  val c = Tal.cvar array_arg_var
	  val ce = Tal.cfield c Tal.ReadWrite 
	  val body = 
	    Tal.cexist array_size_var Tal.kint 
	    (Tal.cprod_b [Tal.cfield (Tal.csing cv) Tal.Read,
			  Tal.cfield (Tal.cprod_b [Tal.carray cv ce]) Tal.Read])
	in C.lam array_arg_var (K.Type sz) body
	end

      fun eta_array sz = sz_abbrev "Array" array_con sz
      fun array sz c =  C.app (eta_array sz) c

      val ref_con = fn () => C.lambda (K.T32) (fn a => Tal.cprod_b [Tal.cfield (C.var a) Tal.ReadWrite])
      val eta_ref = abbrev "Ref" ref_con
      fun reft c = C.app (eta_ref()) c

      local
	fun counttags (w : TilWord32.word) : TilWord32.word list = 
	  let
	    fun loop (x,acc) = if x = 0w0 then 0w0::acc else loop(x-0w1,x::acc)
	  in loop (w,[])
	  end
      in
	fun sum w arms = 
	  let
	    
	    val tags = counttags w
	    val c = case arms 
		      of [] => NONE
		       | [arm] => SOME arm
		       | arms => SOME (Tal.csum arms)
	  in Tal.chptr tags c NONE
	  end

	(* Represent known sums by the rep type itself. Could get rid of
	 * these from the LIL, actually. *)
	fun ksum which tagcount arms = 
	  let
	    fun tag i = Tal.csing (Tal.pcint i)
	    val tags = counttags tagcount
	    val tags = map C.nat tags
	    val l = tags @ arms
	  in LU.wnth which l
	  end
      end

      fun tuple fields = Tal.cprod (map (fn c => Tal.cfield c Tal.Read) fields)
      fun unit () = C.ptr (tuple [])
      fun exists (a,k) c = Tal.cexist a k c
      fun forall (a,k) c = Tal.cforall a k c

      val dyntag_con = 
	fn () => C.lambda K.T32 (fn a => Tal.chptr [] (SOME (tuple[]))(SOME (C.var a,Tal.Read)))
      val eta_dyntag = abbrev "Dyntag" dyntag_con

      fun dyntag c = C.app (eta_dyntag()) c


      fun exn_body c = 
	C.ptr (tuple [dyntag c,c])

      val exn_con = fn () => 
	let
	  val a = Name.fresh_named_var "exn_vt"
	in exists (a,K.T32) (exn_body (C.var a))
	end
      val exn = abbrev "Exn" exn_con



      (*Who cares?  Let's do right to left for C's sake.
       *)
      fun stackargs cs = 
	let
	  fun loop ([],acc) = acc
	    | loop (c::cs,acc) = loop (cs,Tal.ccons c acc)
	in loop (cs,Tal.cempty)
	end
      fun stackargcons c cstack = Tal.cappend cstack c

      val s_tvar = Name.fresh_named_var "s"
      val s_tvar1 = Name.fresh_named_var "s1"
      val s_tvar2 = Name.fresh_named_var "s2"
	
      val cs_tvar1 = Name.fresh_named_var "cs_t1"
      val cs_tvar2 = Name.fresh_named_var "cs_t2"
      val cs_tvar3 = Name.fresh_named_var "cs_t3"
      val cs_tvar4 = Name.fresh_named_var "cs_t4"
      val cs_tvar5 = Name.fresh_named_var "cs_t5"
      val cs_tvar6 = Name.fresh_named_var "cs_t6"

      val s_con = Tal.cvar s_tvar
      val s_con1 = Tal.cvar s_tvar1
      val s_con2 = Tal.cvar s_tvar2

      val cs_con1 = Tal.cvar cs_tvar1
      val cs_con2 = Tal.cvar cs_tvar2
      val cs_con3 = Tal.cvar cs_tvar3
      val cs_con4 = Tal.cvar cs_tvar4
      val cs_con5 = Tal.cvar cs_tvar5
      val cs_con6 = Tal.cvar cs_tvar6

      (* This is currently wrong because the LIL reorders the 64/32 parameters
       * which C conventions don't do. Fix needs to be in Lil.
       *)
      local
	val eargs = Name.fresh_named_var "eargs"
	val fargs = Name.fresh_named_var "fargs"
	val ret = Name.fresh_named_var "ret"


	fun externarrow_con sz () = 
	  let
	    val ms_entry = Tal.ms_empty
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebx (C.var cs_tvar1)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Esi (C.var cs_tvar2)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edi (C.var cs_tvar3)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebp (C.var cs_tvar4)

	    val below_args = (C.var s_tvar) 
	    val below_retn = Tal.cappend (C.var eargs) (Tal.cappend (C.var fargs) below_args)
	    val on_retn    = below_args

	    val ms_return = Tal.ms_set_reg ms_entry Tal.Esp (Tal.csptr on_retn)
	    val ms_return = 
	      (case sz
		 of Lil.B8 => Tal.set_ms_fpstack ms_return (Tal.fpstack_init_reg (Tal.ms_get_fpstack ms_return) 0)
		  | _ => Tal.ms_set_reg ms_return Tal.Eax (C.var ret))

	    val entry_stack = Tal.ccons (Tal.ccode_ms ms_return) below_retn
	    val ms_entry  = Tal.ms_set_reg ms_entry Tal.Esp (Tal.csptr entry_stack)

	    val body = 
	      forall(cs_tvar1, Tal.k4byte)
	      (forall(cs_tvar2, Tal.k4byte)
              (forall(cs_tvar3, Tal.k4byte)
              (forall(cs_tvar4, Tal.k4byte)
	      (forall(s_tvar, Tal.kstack) 
	       (Tal.ccode_ms(ms_entry))))))
	    val res = 
	      C.lam eargs Tal.kstack
	      (C.lam fargs Tal.kstack
	       (C.lam ret (K.Type sz) 
		body))
	  in res
	  end
      in
	fun eta_externarrow sz = sz_abbrev "Externarrow" externarrow_con sz
	fun externarrow sz args fargs rtype = C.appn (eta_externarrow sz) [stackargs args,stackargs fargs,rtype]
      end

      local
	val from = Name.fresh_named_var "qfrom"
	val to = Name.fresh_named_var "qto"

	val coercion_con = fn () => 
	  let
	    val ms_entry = Tal.ms_empty
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Eax (C.var from)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebx (C.var cs_tvar1)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ecx (C.var cs_tvar2)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edx (C.var cs_tvar3)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Esi (C.var cs_tvar4)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebp (C.var cs_tvar5)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edi (C.var cs_tvar6)

	    val stack = Tal.cvar s_tvar

	    val ms_return = Tal.ms_set_reg ms_entry Tal.Esp (Tal.csptr stack)
	    val ms_return = Tal.ms_set_reg ms_return Tal.Eax (C.var to)

	    val entry_stack = Tal.ccons (Tal.ccode_ms ms_return) stack
	    val ms_entry  = Tal.ms_set_reg ms_entry Tal.Esp (Tal.csptr entry_stack)

	    val body = 
	      forall(cs_tvar1, Tal.k4byte)
	      (forall(cs_tvar2, Tal.k4byte)
              (forall(cs_tvar3, Tal.k4byte)
              (forall(cs_tvar4, Tal.k4byte)
              (forall(cs_tvar5, Tal.k4byte)
              (forall(cs_tvar6, Tal.k4byte)
	      (forall(s_tvar, Tal.kstack)
	       (Tal.ccode_ms(ms_entry))))))))
	    val res = 
	      C.lam from Tal.k4byte
	      (C.lam to Tal.k4byte
	       body)
	  in res
	  end

      in
	val eta_coercion = abbrev "Coercion" coercion_con

	fun coercion from to = C.appn (eta_coercion()) [from,to]

      end

      val handler_con = fn () =>
	let
	  val ms = Tal.ms_empty
	  val ms = Tal.ms_set_reg ms Tal.Eax (exn())
	  val ms = Tal.ms_set_reg ms Tal.Esp (Tal.csptr(C.var s_tvar))
	in C.lam s_tvar Tal.kstack (Tal.ccode_ms ms)
	end
      val eta_handler = abbrev "Handler" handler_con
      fun handler s = C.app (eta_handler()) s

      val handler_stackptr_con = fn () => 
	let
	  val outside_handler = handler (C.var s_tvar2)
	  val shandler = Tal.ccons outside_handler (C.var s_tvar2)
	in C.lam s_tvar2 Tal.kstack (Tal.csptr shandler)
	end
      val eta_handler_stackptr = abbrev "HndlrSPtr" handler_stackptr_con
      fun handler_stackptr s = C.app (eta_handler_stackptr()) s

      local
	val eargs = Name.fresh_named_var "eargs"
	val fargs = Name.fresh_named_var "fargs"
	val ret = Name.fresh_named_var "ret"


	val code_con = fn () => 
	  let
	    val ms_entry = Tal.ms_empty
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebx (C.var cs_tvar1)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Esi (C.var cs_tvar2)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edi (C.var cs_tvar3)
	    val outside_handler = handler (C.var s_tvar2)
	    val shandler = Tal.ccons outside_handler (C.var s_tvar2)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebp (Tal.csptr shandler)

	    val below_args = Tal.cappend (C.var s_tvar1) shandler
	    val below_retn = Tal.cappend (C.var eargs) (Tal.cappend (C.var fargs) below_args)
	    val on_retn    = below_args

	    val ms_return = Tal.ms_set_reg ms_entry Tal.Esp (Tal.csptr on_retn)
	    val ms_return = Tal.ms_set_reg ms_return Tal.Eax (C.var ret)

	    val entry_stack = Tal.ccons (Tal.ccode_ms ms_return) below_retn
	    val ms_entry  = Tal.ms_set_reg ms_entry Tal.Esp (Tal.csptr entry_stack)

	    val body = 
	      forall(cs_tvar1, Tal.k4byte)
	      (forall(cs_tvar2, Tal.k4byte)
              (forall(cs_tvar3, Tal.k4byte)
	      (forall(s_tvar1, Tal.kstack)
	      (forall(s_tvar2, Tal.kstack) 
	       (Tal.ccode_ms(ms_entry))))))
	    val res = 
	      C.lam eargs Tal.kstack
	      (C.lam fargs Tal.kstack
	       (C.lam ret Tal.k4byte 
		body))
	  in res
	  end
	val eta_code = abbrev "Code" code_con
      in
	fun code args fargs rettype = C.appn (eta_code()) [args,fargs,rettype]
      end

      (* TAL provides a general mu form which provides similary expressiveness
       * (modulo the baked in syntax and the kind polymorphism) as the LIL Rec_c, 
       * albeit in a slighly hacky way.
       *
       * So:  Lil.Rec_c[k](\(r::k -> T32).cbody)(carg) => ((Tal.Mu [r,k->T32,cbody]).0)(carg)
       * Or:  Lil.Rec_c[k] => \(self::((k->T32)->(k->T32)).((Tal.Mu [r,k->T32,self r]).0)
       *)

      (* eta expand the Rec constructor (as much as possible).  This is probably
       * more generality than we need to translate the LIL, but... *)
      fun eta_rek k = 
	let
	  val k1 = K.Arrow k K.T32
	  val k = K.Arrow k k
	  val r = Name.fresh_named_var "r"
	in
	  C.lambda k (fn self => Tal.cproj(Tal.crec [(r,k1,C.app (C.var self) (C.var r))]) 0w0)
	end

      (* Translate a partially applied Rec type to a partially applied mu, avoiding
       * the unnecessary eta expansion above. *)
      fun partial_rek ((r,k),c) = Tal.cproj(Tal.crec [(r,k,c)]) 0w0

      (* Translate a fully applied Rec type to a mu *)
      fun rek clam carg = C.app (partial_rek clam) carg

      fun tag c = Tal.csing c
      val tag_con = fn () => C.lambda K.Nat (fn a => tag (C.var a))
      val eta_tag = abbrev "Tag" tag_con

      local
	fun boxed_con sz () = C.lambda (K.Type sz) (fn a => Tal.cfield (C.var a) Tal.Read)
      in
	fun eta_boxed sz = sz_abbrev "Boxed" boxed_con sz
	fun boxed sz c = Tal.cfield c Tal.Read
      end

      local
	fun embed_con sz () = C.lambda (K.Type sz) (fn a => C.var a)
      in
	fun eta_embed sz = sz_abbrev "Embed" embed_con sz
	fun embed sz c = c
      end


      fun primcon p = 
	let
	  
	  val res = 
	    (case p
	       of Lil.Int_c s    => Tal.pcbytes s
		| Lil.Float_c    => Tal.pcfloat64
		| Lil.Void_c     => Tal.cvoid K.T32
		| Lil.Tag_c      => eta_tag()
		| Lil.Boxed_c sz => eta_boxed sz
		| Lil.Embed_c sz => eta_embed sz
		| Lil.Array_c sz => eta_array sz
		| Lil.Ref_c      => eta_ref()
		| Lil.Coercion_c => eta_coercion()
		| Lil.Dyntag_c   => eta_dyntag()

		(* Closure converion eliminates Arrow_c *)
		| Lil.Arrow_c    => error "Shouldn't see arrows"

		(* Some things don't really make sense to eta expand in TAL,
		 * because 1: no universal kinds, and 2: primcons don't 
		 * take type lists as arguments, so it's complicated or 
		 * impossible to translate between the idioms.
		 *)


		| Lil.Code_c     => error "Code_c cannot be eta-expanded in TAL"
		| Lil.Sum_c      => error "Sum_c cannot be eta-expanded in TAL"
		| Lil.KSum_c     => error "KSum_c cannot be eta-expanded in TAL"
		| Lil.Tuple_c    => error "Tuple_c cannot be eta-expanded in TAL"
		| Lil.Exists_c   => error "Exists_c cannot be eta-expanded in TAL"
		| Lil.Forall_c   => error "Forall_c cannot be eta-expanded in TAL"
		| Lil.Rec_c      => error "Rec_c cannot be eta-expanded in TAL"
		| Lil.ExternArrow_c s =>  error "ExternArrow_c cannot be eta-expanded in TAL"
		 )
	in res
	end
    end (* structure T *)

  structure E =
    struct
      fun mk_tal_lbl l = 
	let
	  val s = Name.label2string l
	  val s = Core.makeAsmLabel s
	  val smb = Symbol.labSymbol s
	  val l = Name.symbol_label smb
	in l
	end

      val l_overflow = mk_tal_lbl (Name.internal_label "l_overflow")
      val l_unit = mk_tal_lbl (Name.internal_label "l_unit")
    end

end  (* TalTranslationDefs *)