structure TalTranslationDefs :> TALTRANSLATIONDEFS = 
struct

  structure Dec = Deconstruct.Dec
  structure LU = LilUtil
  structure LD = LilDefs
  structure TA = TalAbbrev

  val error = fn s => Util.error "taltranslationdefs.sml" s

  structure K = 
    struct
      val T32 = Tal.k4byte
      val T64 = Tal.k8byte
      val TM = Tal.kmem
      fun Type s = Tal.kbyte s
      fun Arrow a b = Tal.karrow a b
      fun Pair a b = Tal.kprod [a,b]
      val Unit = Tal.kprod []
      val Nat = Tal.kint
      fun Binsum k1 k2 = Tal.ksum [k1,k2]
      val var = Tal.kvar
    end  (* structure K *)


  structure C = 
    struct


      val var = Tal.cvar
      val nat = Tal.pcint
      val unit = Tal.ctuple []
      val lam = Tal.clam
      fun lambda k c = 
	let
	  val a = Name.fresh_internal_label "a"
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
	  val a = Name.fresh_internal_label s
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

      local
	val array_size_var = Name.fresh_internal_label "?sz"
	val array_arg_var = Name.fresh_internal_label "t"
	val cv = Tal.cvar array_size_var 
	val c = Tal.cvar array_arg_var
      in
      fun array_elts_con sz () = 
	let
	  val ce = Tal.cfield c Tal.ReadWrite 

	  val elts = Tal.cprod_b [Tal.carray cv ce]
	in
	  C.lam array_size_var Tal.kint
	  (C.lam array_arg_var (K.Type sz) elts)
	end

      val eta_array_elts = sz_abbrev "ArrElts" array_elts_con 
      fun array_elts sz i c =  C.appn (eta_array_elts sz) [i,c]

      fun unpacked_array_con sz () = 
	let 
	  val body = 
	    (Tal.cprod_b [Tal.cfield (Tal.csing cv) Tal.Read,
			  Tal.cfield (array_elts sz cv c) Tal.Read])
	in 
	  C.lam array_size_var Tal.kint 
	  (C.lam array_arg_var (K.Type sz) body)
	end

      val eta_unpacked_array = sz_abbrev "UnpckdArr" unpacked_array_con 
      fun unpacked_array sz i c =  C.appn (eta_unpacked_array sz) [i,c]

      fun array_con sz () = 
	let 
	  val body = 
	    Tal.cexist array_size_var Tal.kint (unpacked_array sz cv c)
	in C.lam array_arg_var (K.Type sz) body
	end

      val eta_array = sz_abbrev "Array" array_con 
      fun array sz c =  C.app (eta_array sz) c
      end

      val string_con = fn () => array Tal.B1 Tal.cbyte1
      val eta_string = abbrev "Strng" string_con
      fun stringt() = eta_string()

      val ref_con = fn () => C.lambda (K.T32) (fn a => Tal.cprod [Tal.cfield (C.var a) Tal.ReadWrite])
      val eta_ref = abbrev "Ref" ref_con
      fun reft c = C.app (eta_ref()) c

      local
	fun counttags (w : TilWord32.word) : TilWord32.word list = 
	  let
	    fun loop (x,acc) = if x = 0w0 then acc else loop(x-0w1,(x-0w1)::acc)
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
	  in 
	    if which < tagcount then 
	      Tal.chptr [LU.wnth which tags] NONE NONE
	    else
	      (case arms
		 of [t] => Tal.chptr [] (SOME t) NONE
		  | _ => Tal.chptr [] (SOME (Tal.csum [LU.wnth (which-tagcount) arms])) NONE)
	  end
      end

      fun tuple fields = Tal.cprod (map (fn c => Tal.cfield c Tal.Read) fields)
      fun het_tuple fields32 fields64 = 
	let 
	  val fields = fields32 @ fields64
	in tuple fields
	end
      fun het_tuple_ptr fields32 fields64 = C.ptr (het_tuple fields32 fields64)

      fun unit () = C.ptr (tuple [])
      fun exists (a,k) c = Tal.cexist a k c
      fun forall (a,k) c = Tal.cforall a k c

      val dyntag_con = 
	fn () => C.lambda K.T32 (fn a => Tal.chptr [] (SOME (tuple[]))(SOME (Tal.cfield (C.var a) Tal.Read,Tal.ReadWrite)))
      val eta_dyntag = abbrev "Dt" dyntag_con

      fun dyntag c = C.app (eta_dyntag()) c


      val dyntag_con' = 
	fn () => C.lambda Tal.kmem (fn a => Tal.chptr [] (SOME (tuple[]))(SOME (C.var a,Tal.ReadWrite)))
      val eta_dyntag' = abbrev "Dt2" dyntag_con'

      fun dyntag' c = C.app (eta_dyntag'()) c

      fun exn_body' c = 
	C.ptr (Tal.cprod [Tal.cfield (dyntag' c) Tal.Read,c,Tal.cfield (stringt()) Tal.Read])



      fun exn_body c = 
	C.ptr (tuple [dyntag c,c,stringt()])

      val exn_con = fn () => 
	let
	  val a = Name.fresh_internal_label "exn_vt"
	in exists (a,K.T32) (exn_body (C.var a))
	end
      val exn = abbrev "Exn" exn_con



      (*Who cares?  Let's do right to left for C's sake.
       *)
      fun stackargs cs = 
	(case cs 
	   of [] => Tal.cempty
	    | c::cs => Tal.ccons c (stackargs cs))

(*      fun stackargs cs = 
	let
	  fun loop ([],acc) = acc
	    | loop (c::cs,acc) = loop (cs,Tal.ccons c acc)
	in loop (cs,Tal.cempty)
	end
*)
      fun stackargcons c cstack = Tal.ccons c cstack
(*      fun stackargcons c cstack = Tal.cappend cstack (Tal.ccons c Tal.cempty)*)

      val list2stack_con = fn () => 
	let
	  val a = Name.fresh_internal_label "a"
	  val a_c = C.var a
	  val j = Name.fresh_internal_label "j"
	  val j_k = K.var j
	  val r = Name.fresh_internal_label "r"
	  val r_c = C.var r
	  val b = Name.fresh_internal_label "b"
	  val b_c = C.var b
	  val k = K.Binsum K.Unit (K.Pair Tal.ktype j_k)
	  val body = C.sumcase a_c b [Tal.cempty,Tal.ccons (C.pi1 b_c) (C.app r_c (C.pi2 b_c))]
	  val f = C.pr (j,(a,k),Tal.kstack,r,body)
	in f
	end
      val list2stack_eta = abbrev "list2stack" list2stack_con
      fun list2stack l = C.app (list2stack_eta()) l

      val s_tvar = Name.fresh_internal_label "s"
      val s_tvar1 = Name.fresh_internal_label "s1"
      val s_tvar2 = Name.fresh_internal_label "s2"
	
      val cs_tvar1 = Name.fresh_internal_label "cs_t1"
      val cs_tvar2 = Name.fresh_internal_label "cs_t2"
      val cs_tvar3 = Name.fresh_internal_label "cs_t3"
      val cs_tvar4 = Name.fresh_internal_label "cs_t4"
      val cs_tvar5 = Name.fresh_internal_label "cs_t5"
      val cs_tvar6 = Name.fresh_internal_label "cs_t6"

      val handler_fv_tvar = Name.fresh_internal_label "henv"

      val s_con = Tal.cvar s_tvar
      val s_con1 = Tal.cvar s_tvar1
      val s_con2 = Tal.cvar s_tvar2

      val cs_con1 = Tal.cvar cs_tvar1
      val cs_con2 = Tal.cvar cs_tvar2
      val cs_con3 = Tal.cvar cs_tvar3
      val cs_con4 = Tal.cvar cs_tvar4
      val cs_con5 = Tal.cvar cs_tvar5
      val cs_con6 = Tal.cvar cs_tvar6

      val handler_fv_con = Tal.cvar handler_fv_tvar

      local
	val eargs = Name.fresh_internal_label "eargs"
	val ret = Name.fresh_internal_label "ret"


	fun externarrow_con sz () = 
	  let
	    val ms_entry = Tal.ms_empty
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Eax (Tal.pcjunk4)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebx (C.var cs_tvar1)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ecx (Tal.pcjunk4)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edx (Tal.pcjunk4)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Esi (C.var cs_tvar2)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edi (C.var cs_tvar3)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebp (C.var cs_tvar4)

	    val below_args = (C.var s_tvar) 
	    val below_retn = Tal.cappend (C.var eargs) (below_args)

	    val ms_return = Tal.ms_set_reg ms_entry Tal.Esp (Tal.csptr below_retn)
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
	       (C.lam ret (K.Type sz) 
		body)
	  in res
	  end
      in
	val eta_externarrow = sz_abbrev "Externarrow" externarrow_con 
	fun externarrow sz args rtype = C.appn (eta_externarrow sz) [stackargs args,rtype]
      end

      local
	val from = Name.fresh_internal_label "qfrom"
	val to = Name.fresh_internal_label "qto"

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

      val handler_code_con = fn () =>
	let
	  val ms = Tal.ms_empty
	  val ms = Tal.ms_set_reg ms Tal.Eax (exn())
	  val ms = Tal.ms_set_reg ms Tal.Ebx (handler_fv_con)
	  val ms = Tal.ms_set_reg ms Tal.Esp (Tal.csptr(s_con2))

	  val ms = Tal.ms_set_reg ms Tal.Ecx (Tal.pcjunk4)
	  val ms = Tal.ms_set_reg ms Tal.Edx (Tal.pcjunk4)
	  val ms = Tal.ms_set_reg ms Tal.Esi (Tal.pcjunk4)
	  val ms = Tal.ms_set_reg ms Tal.Edi (Tal.pcjunk4)
	  val ms = Tal.ms_set_reg ms Tal.Ebp (Tal.pcjunk4)

	in 
	  C.lam s_tvar2 Tal.kstack 
	  (C.lam handler_fv_tvar K.T32 
	    (Tal.ccode_ms ms))
	end
      val eta_handler_code = abbrev "HndlrCd" handler_code_con
      fun handler_code s fv_v = C.appn (eta_handler_code()) [s,fv_v]

      val handler_frame_con = fn () => 
	let
	  val t = tuple [handler_code s_con2 handler_fv_con,
			 Tal.csptr s_con2,
			 handler_fv_con]
	  val t = C.ptr t
	  val t = exists (handler_fv_tvar,K.T32) t
	in 
	  C.lam s_tvar2 Tal.kstack t
	end
      val eta_handler_frame = abbrev "HndlrFrm" handler_frame_con
      fun handler_frame s = C.app (eta_handler_frame()) s


      local
	val eargs = Name.fresh_internal_label "eargs"
	val fargs = Name.fresh_internal_label "fargs"
	val ret = Name.fresh_internal_label "ret"


	val code_con = fn () => 
	  let
	    val ms_entry = Tal.ms_empty
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebx (C.var cs_tvar1)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Esi (C.var cs_tvar2)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edi (C.var cs_tvar3)

	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebp (handler_frame s_con2)

	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Eax (Tal.pcjunk4)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ecx (Tal.pcjunk4)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edx (Tal.pcjunk4)

	    val below_args = Tal.cappend (C.var s_tvar1) s_con2
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

      (*Return type *)
      local
	val ret = Name.fresh_internal_label "ret"


	val ret_con = fn () => 
	  let
	    val ms_entry = Tal.ms_empty
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebx (C.var cs_tvar1)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Esi (C.var cs_tvar2)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edi (C.var cs_tvar3)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebp (handler_frame s_con2)

	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ecx (Tal.pcjunk4)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edx (Tal.pcjunk4)

	    val below_args = Tal.cappend (C.var s_tvar1) s_con2
	    val on_retn    = below_args

	    val ms_return = Tal.ms_set_reg ms_entry Tal.Esp (Tal.csptr on_retn)
	    val ms_return = Tal.ms_set_reg ms_return Tal.Eax (C.var ret)

	    val body = Tal.ccode_ms(ms_return)
	    val res = 
	      C.lam cs_tvar1 Tal.k4byte
	      (C.lam cs_tvar2 Tal.k4byte
	       (C.lam cs_tvar3 Tal.k4byte
		(C.lam s_tvar1 Tal.kstack
		 (C.lam s_tvar2 Tal.kstack
		 (C.lam ret Tal.k4byte body)))))
	  in res
	  end
	val eta_ret = abbrev "RType" ret_con
      in
	fun rtype rettype = C.appn (eta_ret()) [cs_con1,cs_con2,cs_con3,s_con1,s_con2,rettype]
      end

(*
      local
	val eaxt = Name.fresh_named_var "eaxt"
	val ebxt = Name.fresh_named_var "ebxt"
	val ecxt = Name.fresh_named_var "ecxt"
	val edxt = Name.fresh_named_var "edxt"
	val esit = Name.fresh_named_var "esit"
	val edit = Name.fresh_named_var "edit"
	val ebpt = Name.fresh_named_var "ebpt"
	val espt = Name.fresh_named_var "espt"


	val code_con = fn () => 
	  let
	    val ms_entry = Tal.ms_empty
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Eax (C.var eaxt)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebx (C.var ebxt)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ecx (C.var ecxt)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edx (C.var edxt)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Esi (C.var esit)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Edi (C.var edit)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Ebp (C.var ebpt)
	    val ms_entry = Tal.ms_set_reg ms_entry Tal.Esp (C.var espt)
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
*)
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
	  val r = Name.fresh_internal_label "r"
	in
	  C.lambda k (fn self => Tal.cproj(Tal.crec [(r,k1,C.app (C.var self) (C.var r))]) 0w0)
	end

      (* Translate a partially applied Rec type to a partially applied mu, avoiding
       * the unnecessary eta expansion above. *)
      fun partial_rek ((r,k),c) = Tal.cproj(Tal.crec [(r,K.Arrow k K.T32,c)]) 0w0

      (* Translate a fully applied Rec type to a mu *)
      fun rek clam carg = C.app (partial_rek clam) carg

      fun tag c = Tal.csing c
      val tag_con = fn () => C.lambda K.Nat (fn a => tag (C.var a))
      val eta_tag = abbrev "Tag" tag_con

      local
	fun boxed_con sz () = C.lambda (K.Type sz) (fn a => Tal.cprod ([Tal.cfield (C.var a) Tal.Read]))
      in
	val eta_boxed = sz_abbrev "Boxed" boxed_con 
	fun boxed sz c = Tal.cfield c Tal.Read
      end

      local
	fun embed_con sz () = C.lambda (K.Type sz) (fn a => Tal.pcbytes Tal.B4)
      in
	val eta_embed = sz_abbrev "Embed" embed_con 
	fun embed sz c = Tal.pcbytes Tal.B4
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
		| Lil.Array_c sz => error "Arrays can't be eta-expanded in TAL (only array-ptrs)"
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


      val new_dyntag_con = 
	let
	  val a = Name.fresh_named_var "a"
	  val a_v = LD.C.var a
	  val body = LD.T.externarrow' Lil.B4 [] (LD.T.dyntag a_v)
	in LD.C.lambda (a,LD.K.T32()) body
	end

      val new_int8array_con = 
	let
	  val a_c = LD.T.intt Lil.B1
	  val len_t = LD.C.inl (LD.K.T32or64()) (LD.T.intt Lil.B4)
	  val arg_t = LD.C.inl (LD.K.T32or64()) (LD.T.embed Lil.B1 a_c)
	  val body = LD.T.externarrow' Lil.B4 [len_t,arg_t] (LD.T.arrayptr Lil.B1 a_c)
	in body
	end

      val new_int32array_con = 
	let
	  val a_c = LD.T.intt Lil.B4
	  val len_t = LD.C.inl (LD.K.T32or64()) (LD.T.intt Lil.B4)
	  val arg_t = LD.C.inl (LD.K.T32or64()) a_c
	  val body = LD.T.externarrow' Lil.B4 [len_t,arg_t] (LD.T.arrayptr Lil.B4 a_c)
	in body
	end

      val new_floatarray_con = 
	let
	  val a_c = LD.T.float64()
	  val len_t = LD.C.inl (LD.K.T32or64()) (LD.T.intt Lil.B4)
	  val arg_t = LD.C.inr (LD.K.T32or64()) a_c
	  val body = LD.T.externarrow' Lil.B4 [len_t,arg_t] (LD.T.arrayptr Lil.B8 a_c)
	in body
	end

      val new_polyarray32_con = 
	let
	  val a = Name.fresh_named_var "a"
	  val a_v = LD.C.var a
	  val len_t = LD.C.inl (LD.K.T32or64()) (LD.T.intt Lil.B4)
	  val arg_t = LD.C.inl (LD.K.T32or64()) a_v
	  val body = LD.T.externarrow' Lil.B4 [len_t,arg_t] (LD.T.arrayptr Lil.B4 a_v)
	in LD.C.lambda (a,LD.K.T32()) body
	end

      val new_dyntag_type = LD.T.forall' (LD.K.T32()) new_dyntag_con
      val new_ptrarray_type = LD.T.forall' (LD.K.T32()) new_polyarray32_con
      val new_intarray_type = (fn sz =>
			       (case sz
				  of Lil.B1 => new_int8array_con
				   | Lil.B4 => new_int32array_con
				   | _ => error "Bad int array size"))
      val new_floatarray_type = new_floatarray_con

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
      val l_array_zero = mk_tal_lbl (Name.internal_label "l_array_zero")
      val l_wordarray_zero = mk_tal_lbl (Name.internal_label "l_wordarray_zero")
      val l_floatarray_zero = mk_tal_lbl (Name.internal_label "l_floatarray_zero")
      val l_raise_subscript = mk_tal_lbl (Name.internal_label "l_raise_subscript")
      val l_div_zero = mk_tal_lbl (Name.internal_label "l_div_zero")


      fun extern_label s = 
	let
	  val smb = Symbol.varSymbol s
	  val l = Name.symbol_label smb
	in l
	end
      val l_newdyntag = extern_label "new_dyntag"
      val l_int8array = extern_label "new_int8array"
      val l_int32array = extern_label "new_int32array"
      val l_intarray = fn sz => 
	(case sz
	   of Lil.B1 => l_int8array
	    | Lil.B4 => l_int32array
	    | _ => error "Bad size to l_intarray")
      val l_ptrarray = extern_label "new_ptrarray"
      val l_floatarray = extern_label "new_floatarray"

    end

end  (* TalTranslationDefs *)