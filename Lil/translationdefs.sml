structure TranslationDefs :> TRANSLATIONDEFS =
  struct
    structure LD = LilDefs
    structure LU = LilUtil
    structure Dec = Deconstruct.Dec
    structure R = Reduce
    structure LO = Listops
    structure LR = LilRename

    open Lil

    fun error s = Util.error "translationdefs.sml" s

    val simplify = Stats.tt "TD_simplify_defs"
    val debug  = Stats.ff "TranslationDefsDebug"
    val betaexp = Stats.counter "TD:if_xxx beta expansions"
    val inline_interp = Stats.ff "TDInlineInterp"

    fun debugdo f = if !debug then f() else ()

    fun countflat f = LO.map0count f (!flattenThreshold +1)
    fun countbig f  = f (!flattenThreshold + 1)
    fun countall f g = (countflat f)@[countbig g]

    val Tupleidx'  = fn i => LU.i2w (if i > !flattenThreshold then !flattenThreshold +1 else i)
    val Flatidx    = fn i => LU.i2w i
    val Bigidx     = fn () => LU.i2w (!flattenThreshold + 1)

    val Tupleidx  : w32 = 0w0
    val BFloatidx : w32 = 0w1
    val Ptridx    : w32 = 0w2
    val Otheridx  : w32 = 0w3

    fun mkstatic f = 
      let
	val fv = f()
      in fn () => fv
      end

    local

      (* Tmilr == 1 + T32 + T32 * T32 + .... + T32 list (All lists < flattenThreshold represented directly) *)

      fun Tmilr' () = 
	let
	  val flats = countflat (fn i => LD.K.ntuple (LO.copy (i,LD.K.T32())))
	in LD.K.sum (flats@[LD.K.tlist()])
	end
      val Tmilr = mkstatic Tmilr'

      fun Tmil' () : kind = 
	LD.K.sum [Tmilr(),
		  LD.K.unit(),
		  LD.K.TM(),
		  LD.K.T32()]
      val Tmil = mkstatic Tmil'

      (* interpr :: Tmilr -> T32  *)
      fun interpr_case (c : Lil.con) : Lil.con = 
	let
	  val flatarm = fn i => (LU.i2w i,fn c => LD.COps.ntuple2tlist i c)
	  val bigarm  = fn i => (LU.i2w i,fn l => l)
	  val arms = countall flatarm bigarm
	in
	  LD.T.tuple (LD.C.sumcase c arms NONE)
	end

      fun interpr_fn () = 
	let
	  val arg = Name.fresh_named_var "interpr_arg"
	in LD.C.lambda  (arg,Tmilr()) (interpr_case (mk_con (Var_c arg)))
	end

      val interpr_fn = mkstatic interpr_fn

      fun interpr (c : Lil.con) : Lil.con = 
	if !inline_interp then
	  interpr_case c
	else 
	  (case (Dec.C.inj' c,!simplify)
	     of (SOME _,true) => interpr_case c
	      | _ => LD.C.app (interpr_fn ()) c)

      fun interp_case (c : Lil.con) : Lil.con = 
	LD.C.sumcase c
	[ 
	 (Otheridx,fn t => t)
	 ] 
	(SOME(LD.T.ptr (LD.C.sumcase c [ 
					(Tupleidx,fn l => interpr l),
					(BFloatidx,fn _ => LD.T.boxed_float ()),
					(Ptridx,fn t => t),
					(Otheridx,fn t => (LD.T.boxed B4 t))
					] NONE)))
				      


      fun interp_fn () = 
	let
	  val arg = Name.fresh_named_var "interp_arg"
	in LD.C.lambda  (arg,Tmil()) (interp_case (mk_con (Var_c arg)))
	end
      val interp_fn = mkstatic interp_fn

      fun interp (c : Lil.con) : Lil.con = 
	if !inline_interp then
	  interp_case c
	else 
	  (case (Dec.C.inj' c,!simplify)
	     of (SOME _,true) => interp_case c
	      | _ => LD.C.app (interp_fn()) c)

    in
      val Tmilr = Tmilr
      val Tmil = Tmil
      val interpr = fn c => LD.T.ptr (interpr c)
      val interp = interp

      fun Bigtuple l  = LD.C.inj (LU.i2w (!flattenThreshold + 1)) (Tmilr()) (LD.C.tlist l)
      fun Flattuple l = LD.C.inj (LU.i2w (List.length l)) (Tmilr()) (LD.C.ntuple l)
	
      (* : T32 list -> Tmilr *)
      fun Tuple' l = if List.length l > !flattenThreshold 
		       then Bigtuple l
		     else Flattuple l
		       
      val TupleRep   = fn l => LD.C.inj Tupleidx (Tmil ()) (Tuple' l)
      val BFloatRep  = fn () => LD.C.inj BFloatidx (Tmil()) (LD.C.star())
      val PtrRep     = LD.C.inj Ptridx (Tmil()) 
      val OtherRep   = LD.C.inj Otheridx (Tmil())


      (* :: Tmilr -> T32 -> T32 *)
      fun VarargTuplearrowarg arg = 
	let
	  val flatarm = fn i => (Flatidx i,fn l => (LD.COps.ntuple2tlist i l))
	  val bigarm  = fn i => (Bigidx(),fn t => LD.C.tlist [LD.T.tupleptr t])
	  val arms= countall flatarm bigarm
	in LD.C.sumcase arg arms NONE
	end

      fun VarargTuplearrowargfn' () = 
	let
	  val argv = Name.fresh_named_var "rarg_c"
	  val argc = LD.C.var argv
	in 
	  LD.C.lambda (argv,Tmilr())
	  (VarargTuplearrowarg argc )
	end
      val VarargTuplearrowargfn = mkstatic  VarargTuplearrowargfn'

      (* :: Tmilr -> T32 -> T32 *)
      fun VarargTuplecasedef arg rest = 
	let
	  val flatarm = fn i => (Flatidx i,fn l => (LD.COps.ntuple2tlist i l))
	  val bigarm  = fn i => (Bigidx(),fn t => LD.C.tlist [LD.T.tupleptr t])
	  val arms= countall flatarm bigarm
	in LD.T.arrow (LD.C.sumcase arg arms NONE) (LD.C.nill (LD.K.T64())) rest
	end

      fun VarargTuplecasefn' () = 
	let
	  val argv = Name.fresh_named_var "rarg_c"
	  val argc = LD.C.var argv
	  val restv = Name.fresh_named_var "res_t"
	  val rest = LD.C.var restv
	in 
	  LD.C.nlambda [(argv,Tmilr()),(restv,LD.K.T32())]
	  (VarargTuplecasedef argc rest)
	end

      val VarargTuplecasefn = mkstatic VarargTuplecasefn'

      (* :: Tmil -> T32 -> T32 *)
      fun VarargTuple' argc rest =
	(case (Dec.C.inj' argc,!simplify)
	   of (SOME _,true) => VarargTuplecasedef argc (rest)
	    | _ => LD.C.appn (VarargTuplecasefn()) [argc,rest])

      (* :: Tmil -> Tmil -> T32 *)
      fun VarargTuple argc resc = VarargTuple' argc (interp resc)

      (* :: Tmil ->  T32  -> T32 *)
      fun Varargcasedef argc rest = 
	let
	  val BFloata = fn _ => (LD.C.tlist [LD.T.ptr (LD.T.boxed_float ())])
	  val Ptra = fn t => (LD.C.tlist [LD.T.ptr t])
	  val Otherwisea = fn t => (LD.C.tlist [t])
	  val Tuplea = fn t => (LD.C.app (VarargTuplearrowargfn ()) t )
	  val branch = LD.C.sumcase argc [(Tupleidx,Tuplea),(BFloatidx,BFloata),(Ptridx,Ptra),(Otheridx,Otherwisea)] NONE
	in LD.T.arrow branch (LD.C.nill (LD.K.T64())) rest
	end

      (* :: Tmil -> T32 -> T32 *)
      fun Varargdef' () = 
	let
	  val argv = Name.fresh_named_var "arg_c"
	  val argc = LD.C.var argv
	  val restv = Name.fresh_named_var "res_t"
	  val rest = LD.C.var restv
	in 
	  LD.C.nlambda [(argv,Tmil()),(restv,LD.K.T32())]
	  (Varargcasedef argc rest)
	end

      val Varargdef = mkstatic Varargdef'

      (* If argc is known, then we can reduce the sum and make
       * this much smaller.  Otherwise, we are much happier 
       * leaving the beta redex around so that we only have one copy
       * of the argument types and their interpretations.
       *)
      fun Vararg' argc rest = 
	(case (Dec.C.inj' argc,!simplify)
	   of (SOME _,true) => Varargcasedef argc (rest)
	    | _ => LD.C.appn (Varargdef()) [argc,rest])

      fun Vararg argc resc = Vararg' argc (interp resc)

      fun IfTaglike arg ift ifnott = 
	let
	  val ift = fn _ => ift
	in LD.C.sumcase arg [(Otheridx,ift)] (SOME ifnott)
	end
(*
	let
	  val ift = fn _ => ift
	in
	  LD.C.sumcase arg [(Otheridx,ift)] (SOME ifnott)
	end
*)
      fun Array arg = 
	let
	  val Tuplea = fn a => LD.T.array Lil.B4 (interpr a)
	  val BFloata = fn _ => LD.T.array Lil.B8 (LD.T.float())
	  val Ptra = fn t => LD.T.array Lil.B4 (LD.T.ptr t)
	  val Otherwisea = fn t => LD.T.array Lil.B4 t
	in LD.C.sumcase arg [(Tupleidx,Tuplea),(BFloatidx,BFloata),(Ptridx,Ptra),(Otheridx,Otherwisea)] NONE
	end
      fun Arrayptr arg = LD.T.ptr (Array arg)
    end

    local
      val unit_arm = fn _ => LD.T.unit ()
      val void_arm = fn _ => LD.T.void ()

      fun proof' arm arg w = LD.C.sumcase arg [(w,arm)] (SOME (void_arm()))
      fun proof c w = proof' unit_arm c w
	
     fun tuple_sum c = 
       let
	 val arms = countall (fn i => proof c (LU.i2w i)) (fn i => proof c (LU.i2w i))
       in LD.T.sum' 0w0 arms
       end
       
     (* : Tmil -> T32 *)
     fun tuple_sum_fn' () =
       let
	 val a = Name.fresh_named_var "a_r"
       in LD.C.lambda (a,Tmilr()) (tuple_sum (mk_con (Var_c a)))
       end
     
     val tuple_sum_fn =
       let
	 val t = tuple_sum_fn'()
       in fn () => t
       end

     fun mk_tuple_sum c = 
       (case (Dec.C.inj' c,!simplify)
	  of (SOME _,true) => tuple_sum c
	   | _ => LD.C.app (tuple_sum_fn()) c)


     (**********  KNOWN Rtuple ********)
     fun tuple_sum_i i c = 
       let
	 val arms = countall (fn i => proof c (LU.i2w i)) (fn i => proof c (LU.i2w i))
       in LD.T.ksum i (LD.C.nat 0w0) (LD.C.tlist arms)
       end

     (* : Nat -> Tmil -> T32 *)
     fun tuple_sum_fn_i' () =
       let
	 val i = Name.fresh_named_var "i"
	 val a = Name.fresh_named_var "a_r"
       in LD.C.nlambda [(i,LD.K.nat()),(a,Tmilr())] (tuple_sum_i (mk_con (Var_c i)) (mk_con (Var_c a)))
       end
     
     val tuple_sum_fn_i =
       let
	 val t = tuple_sum_fn_i'()
       in fn () => t
       end

     fun mk_tuple_sum_i i c = 
       (case (Dec.C.inj' c,!simplify)
	  of (SOME _,true) => tuple_sum_i i c
	   | _ => LD.C.appn (tuple_sum_fn_i()) [i,c])

     fun top_sum c = 
       let
	 val tuple_case = proof' (fn cl => mk_tuple_sum cl) c Tupleidx 
	 val bfloat_case = proof c BFloatidx
	 val ptr_case    = proof c Ptridx
	 val other_case  = proof c Otheridx
       in LD.T.sum' (0w0) [tuple_case,bfloat_case,ptr_case,other_case]
       end
     
     (* : Tmil -> T32 *)
     fun top_sum_fn' () =
       let
	 val a = Name.fresh_named_var "a_r"
       in LD.C.lambda (a,Tmil()) (top_sum (mk_con (Var_c a)))
       end

     val top_sum_fn =
       let
	 val t = top_sum_fn'()
       in fn () => t
       end

     fun mk_top_sum c = 
       (case (Dec.C.inj' c,!simplify)
	  of (SOME _,true) => top_sum c
	   | _ => LD.C.app (top_sum_fn()) c)

    (****** KNOWN R()  **************)
     fun top_sum_i i c = 
       let
	 val tuple_case = proof' (fn cl => mk_tuple_sum cl) c Tupleidx 
	 val bfloat_case = proof c BFloatidx
	 val ptr_case    = proof c Ptridx
	 val other_case  = proof c Otheridx
       in LD.T.ksum i (LD.C.nat 0w0) (LD.C.tlist [tuple_case,bfloat_case,ptr_case,other_case])
       end
     
     (* : Tmil -> T32 *)
     fun top_sum_fn_i' () =
       let
	 val i = Name.fresh_named_var "i"
	 val a = Name.fresh_named_var "a_r"
       in LD.C.nlambda [(i,LD.K.nat()),(a,Tmil())] (top_sum_i (mk_con (Var_c i)) (mk_con (Var_c a)))
       end

     val top_sum_fn_i =
       let
	 val t = top_sum_fn_i' ()
       in fn () => t
       end

     fun mk_top_sum_i i c = 
       (case (Dec.C.inj' c,!simplify)
	  of (SOME _,true) => top_sum_i i c
	   | _ => LD.C.appn (top_sum_fn_i()) [i,c])

    in
      fun Rtuple (c : con) : con = mk_tuple_sum c
      fun R (c : con) : con = mk_top_sum c

      fun Rtuple_i (i : con) (c : con) : con = mk_tuple_sum_i i c
      fun R_i (i : con) (c : con) : con = mk_top_sum_i i c
    end

    fun tuple i R_c  = 
      let
	val (_,sum_args) = Dec.C.sum R_c
	val r_sum = Dec.C.hd sum_args
	val arm = Tupleidx' i
	    
	val r_exp = P.bind (LD.E.unit()) (LD.E.inj_nontag_from_sumtype arm r_sum)
	val pexp   = P.bind r_exp (LD.E.inj_nontag_from_sumtype Tupleidx R_c)
      in pexp
      end
      
    fun bfloat R_c    = P.bind (LD.E.unit()) (LD.E.inj_nontag_from_sumtype BFloatidx R_c)
    fun ptr R_c       = P.bind (LD.E.unit()) (LD.E.inj_nontag_from_sumtype Ptridx R_c)
    fun otherwise R_c = P.bind (LD.E.unit()) (LD.E.inj_nontag_from_sumtype Otheridx R_c)
      
    val tuple' = fn a => fn b => P.SV32.to_op (tuple a b)
    val bfloat' = fn a => P.SV32.to_op (bfloat a)
    val ptr' = fn a => P.SV32.to_op (ptr a)
    val otherwise' = fn a => P.SV32.to_op (otherwise a)
    local

      fun if_xxx which (mkrtype : Lil.con -> Lil.con) (srep : Lil.con) (drep : Lil.sv32 ) (yes : Lil.con -> Lil.sv32 P.pexp) (no: Lil.con -> Lil.sv32 P.pexp) = 
	let
	  fun mkswitch srep drep = 
	    let
	      fun arm iw = 
		let
		  val ksum = R_i (LD.C.nat iw) srep
		  val x = Name.fresh_named_var "x"
		  val x_i = LD.E.project ksum (Var_32 x)
		  val e = fn _ => if which = iw then yes srep else no srep
		  val exp = P.bind x_i 
		    (fn x_i =>LD.E.vcase iw srep e x_i)
		  val exp = P.Lili.to_exp exp
		  val exp = LR.renameEVarsExp exp
		in (iw,x,exp)
		end
	      
	      val switch = 
		Sumcase {arg = drep,
			 arms = [arm Tupleidx, arm BFloatidx, arm Ptridx, arm Otheridx],
			 default = NONE,
			 rtype = mkrtype srep}
	    in P.ret (Switch switch )
	    end

	  fun mkfn () = 
	    let
	      val srepv = Name.fresh_named_var "if_srep"
	      val srep = mk_con (Var_c srepv)
	      val repv = Name.fresh_named_var "if_rep"
	      val rep = Lil.Var_32 repv
	      val rept = R srep
		
	      val body = mkswitch srep rep 

	      val f = 
		Lil.Function {tFormals = [(srepv,Tmil())],
			      eFormals = [(repv,rept)],
			      fFormals = [],
			      rtype    = mkrtype srep,
			      body = P.Lili.op_to_exp body}
	      val fv = Name.fresh_named_var "if_xxx" 
	    in P.Bind.fixcode' (P.ret [(fv,f)]) (P.ret (Var_32 fv))
	    end

	in
	  (case LD.E.letpath srep
	     of SOME srep => P.bind srep (fn srep => mkswitch srep drep)
	      | NONE => (betaexp();
			 P.bind (mkfn ()) (fn f => LD.E.allapp' f [srep] [drep] [])))
	end
    in
      val ifboxfloat = fn x => (debugdo(fn() => print "Calling ifboxflat\n");if_xxx BFloatidx x)
      val iftaglike  = fn x => (debugdo(fn() => print "Calling iftaglike\n");if_xxx Otheridx x)

      fun mk_project_dynamic_fn () = 
	let
	  val srepv = Name.fresh_named_var "carrier_type_srep"
	  val srep = mk_con (Var_c srepv)
	  val repv = Name.fresh_named_var "carrier_type_rep"
	  val rep = Lil.Var_32 repv
	  val rept = R srep
	  val sumargv = Name.fresh_named_var "sumarg"
	  val sumarg = Lil.Var_32 sumargv
	  val sumt = IfTaglike srep (LD.T.ptr (LD.T.tuple' [interp srep])) (interp srep)

	  val body = iftaglike interp srep rep
	    (fn _ => LD.E.select (LU.i2w 0) sumarg)
	    (fn _ => P.ret sumarg)
	  val f = 
	    Lil.Function {tFormals = [(srepv,Tmil())],
			  eFormals = [(repv,rept),(sumargv,sumt)],
			  fFormals = [],
			  rtype    = interp srep,
			  body = P.Lili.op_to_exp body}
	  val fv = Name.fresh_named_var "project_dynamic"
	in P.Bind.fixcode' (P.ret [(fv,f)]) (P.ret (Var_32 fv))
	end

      fun mk_inject_dynamic_fn () = 
	let
	  val srepv = Name.fresh_named_var "carrier_type_srep"
	  val srep = mk_con (Var_c srepv)
	  val repv = Name.fresh_named_var "carrier_type_rep"
	  val rep = Lil.Var_32 repv
	  val rept = R srep
	  val injargv = Name.fresh_named_var "injarg"
	  val injarg = Lil.Var_32 injargv
	  val mkrtype = fn injt => IfTaglike srep (LD.T.ptr (LD.T.tuple' [interp injt])) (interp injt)

	  val body = iftaglike mkrtype srep rep
	    (fn _ => LD.E.tuple [injarg])
	    (fn _ => P.ret injarg)
	  val f = 
	    Lil.Function {tFormals = [(srepv,Tmil())],
			  eFormals = [(repv,rept),(injargv,interp srep)],
			  fFormals = [],
			  rtype    = mkrtype srep,
			  body = P.Lili.op_to_exp body}
	  val fv = Name.fresh_named_var "inject_dynamic"
	in P.Bind.fixcode' (P.ret [(fv,f)]) (P.ret (Var_32 fv))
	end

      fun mk_inject_dynamic_app injfn srep rep arg = 
	LD.E.allapp' injfn [srep] [rep,arg] []
      fun mk_project_dynamic_app injfn srep rep arg = 
	LD.E.allapp' injfn [srep] [rep,arg] []
    end

    local (* mk_var/onearg *)

      fun flatlam tlist rtype f = 
	let
	  val vars = map (fn _ => Name.fresh_named_var "vararg_eta") tlist
	  val vts = LO.zip vars tlist
	  val args = map (Lil.Var_32) vars
	  val tup = LD.E.tuple args
	  val body = P.map (fn sv => Lil.App (f,[sv],[])) tup
	  val body = P.Lili.op_to_exp body
	  val lam = LD.E.lambda vts rtype body
	in lam
	end

      fun unflatlam tlist rtype f = 
	let
	  val argv = Name.fresh_named_var "onearg_tupv"
	  val argt = LD.T.tupleptr' tlist
	  val projs = LD.EOps.project_all_tuple_fields (Var_32 argv) (List.length tlist)
	  val body = P.map (fn svs => Lil.App (f,svs,[])) projs
	  val body = P.Lili.op_to_exp body
	  val lam = LD.E.lambda [(argv,argt)] rtype body
	in lam
	end

      fun tuple_case switchtype mklam srepc rtype drep f =
	let
	  fun mk_arm mkbody i = 
	    let
	      val iw = LU.i2w i
	      val ksum = Rtuple_i (LD.C.nat iw) srepc
	      val x = Name.fresh_named_var "tuplerep"
	      val x_i = LD.E.project ksum (Var_32 x)
	      val a = P.bind x_i (fn x_i => LD.E.letinj iw (srepc,x_i))
	      val e = P.bind a (fn a => mkbody i a)
	      val e = P.Lili.to_exp e
	    in (iw,x,e)
	    end

	  (*XXX: We should probably reorder these to check the common cases first.
	   * Tuples of size 1, for example, should almost never appear.
	   *)
	  val flatarmn = mk_arm (fn i => fn a => mklam (LD.COps.project_i_fields i a) rtype f)
	  val bigarm = mk_arm (fn i => fn a => P.ret f)

	  val arms = countall flatarmn bigarm
	  val switch = 
	    Sumcase {arg = drep,
		     arms = arms,
		     default = NONE,
		     rtype = switchtype}
	in Switch switch
	end

      fun tuple_arm tuple_case argc rtype drep f = 
	let
	  val iw = Tupleidx
	  val ksum = R_i (LD.C.nat iw) argc
	  val x = Name.fresh_named_var "drep"
	  val x_i = LD.E.project ksum (Var_32 x)
	  val oper = P.bind x_i 
	    (fn x_i => P.bind (LD.E.letinj iw (argc,x_i))
	     (fn a => P.ret (tuple_case a rtype x_i f)))
	  val exp = P.Lili.op_to_exp oper
	in (iw,x,exp)
	end


      fun arg_case switchtype tuple_arm argc rtype drep f = 
	let
	  fun other_arm iw = 
	    let
	      val ksum = R_i (LD.C.nat iw) argc
	      val x = Name.fresh_named_var "drep"
	      val x_i = LD.E.project ksum (Var_32 x)
	      val oper = P.bind x_i 
		(fn x_i => P.bind (LD.E.letinj iw (argc,x_i))
		 (fn a => P.ret f))
	      val exp = P.Lili.to_exp oper
	    in (iw,x,exp)
	    end
	  
	  (* XXX: This is unfortunate.  If we used union types
	   * instead of sum types, we could refine the type 
	   * in the default branch sufficiently to only have to check 
	   * whether or not we were in the first case.  Well, at least
	   * we could if we could show that void + void + void == void.
	   * As it is, the best we can do is order the cases according to 
	   * how likely we think they are.
	   *)
	  val arms =
	    [tuple_arm argc rtype drep f,
	     other_arm Otheridx,
	     other_arm Ptridx,
	     other_arm BFloatidx]
	  val switch = 
	    Sumcase {arg = drep,
		     arms = arms,
		     default = NONE,
		     rtype = switchtype}
	in Switch switch
	end

      (* Note: It may be desirable to factor out the tuple case into
       * another function so that partial inlining of just the top level
       * case can happen.
       *)
      fun mk_arg_fn name mkfrom mkto mk_case = 
	let
	  val argcv = Name.fresh_named_var "argc"
	  val argc = Lil.mk_con (Lil.Var_c argcv)
	  val restv = Name.fresh_named_var "rtype"
	  val rest = Lil.mk_con (Lil.Var_c restv)
	  val repv = Name.fresh_named_var "argcrep"
	  val repsv = Lil.Var_32 repv
	  val fv = Name.fresh_named_var "f"
	  val f = Lil.Var_32 fv
	  val ftype = mkfrom argc rest
	  val rtype = mkto argc rest
	  val argf = 
	    Lil.Function {tFormals = [(argcv,Tmil()),(restv,LD.K.T32())],
			  eFormals = [(repv,R(argc)),(fv,ftype)],
			  fFormals = [],
			  rtype    = rtype,
			  body = LD.E.op2exp (mk_case argc rest repsv f)}
	  val argv = Name.fresh_named_var name
	in P.Bind.fixcode' (P.ret [(argv,argf)]) (P.ret (Var_32 argv))
	end

      fun Arrow argc rtype = LD.T.arrow' [(interp argc)] [] rtype
      fun ArrowTuplecasedef argc rtype = LD.T.arrow' [(interpr argc)] [] rtype

      fun tuple_vararg_case srepc rtype drep f = tuple_case (VarargTuple' srepc rtype) flatlam srepc rtype drep f
      fun tuple_onearg_case srepc rtype drep f = tuple_case (ArrowTuplecasedef srepc rtype) unflatlam srepc rtype drep f

      fun tuple_vararg_arm argc rtype drep f = tuple_arm tuple_vararg_case argc rtype drep f
      fun tuple_onearg_arm argc rtype drep f = tuple_arm tuple_onearg_case argc rtype drep f
	
      fun vararg_case argc rtype drep f = arg_case (Vararg' argc rtype) tuple_vararg_arm argc rtype drep f
      fun onearg_case argc rtype drep f = arg_case (Arrow argc rtype) tuple_onearg_arm argc rtype drep f
    in (* mk_vararg *)
      fun mk_vararg_case argc resc drep f = P.ret (vararg_case argc (interp resc) drep f)
      fun mk_onearg_case argc resc drep f = P.ret (onearg_case argc (interp resc) drep f)

      fun mk_vararg_fn () = mk_arg_fn "vararg"  Arrow Vararg' vararg_case
      fun mk_onearg_fn () = mk_arg_fn "onearg"  Vararg' Arrow onearg_case


      fun mk_vararg_app varargsv argc resc drep f = LD.E.allapp' varargsv [argc,interp resc] [drep,f] []
      fun mk_onearg_app oneargsv argc resc drep f = LD.E.allapp' oneargsv [argc,interp resc] [drep,f] []
    end 
  

    (* Array stuff *)
    structure Array = 
      struct

	
	fun ifboxfloat (mkrtype : Lil.con -> Lil.con) (srep : Lil.con) (drep : Lil.sv32 ) (args : Lil.sv32 list) (argtypes : (Lil.con -> Lil.con) list) (yes : Lil.con -> Lil.sv32 list -> Lil.sv32 P.pexp) (no: Lil.con -> Lil.sv32 list -> Lil.sv32 P.pexp) = 
	  let
	    fun mkswitch srep drep args = 
	      let
		fun arm iw = 
		  let
		    val ksum = R_i (LD.C.nat iw) srep
		    val x = Name.fresh_named_var "x"
		    val x_i = LD.E.project ksum (Var_32 x)
		    val e = fn _ => if BFloatidx = iw then yes srep args else no srep args
		    val exp = P.bind x_i 
		      (fn x_i =>LD.E.vcase iw srep e x_i)
		    val exp = P.Lili.to_exp exp
		    val exp = LR.renameEVarsExp exp
		  in (iw,x,exp)
		  end
		
		val switch = 
		  Sumcase {arg = drep,
			   arms = [arm Tupleidx, arm BFloatidx, arm Ptridx, arm Otheridx],
			   default = NONE,
			   rtype = mkrtype srep}
	      in P.ret (Switch switch )
	      end

	    fun mkfn () = 
	      let
		val srepv = Name.fresh_named_var "array_case_srep"
		val srep = mk_con (Var_c srepv)
		val repv = Name.fresh_named_var "array_case_rep"
		val rep = Lil.Var_32 repv
		val rept = R srep
		val argvs = map (fn _ => Name.fresh_named_var "array_case_arg") args
		val argvts = LO.map2 (fn (x,f) => (x,f srep)) (argvs,argtypes)
		val args = map Var_32 argvs
		val body = mkswitch srep rep args 

		val f = 
		  Lil.Function {tFormals = [(srepv,Tmil())],
				eFormals = [(repv,rept)]@argvts,
				fFormals = [],
				rtype    = mkrtype srep,
				body = P.Lili.op_to_exp body}
		val fv = Name.fresh_named_var "array_case" 
	      in P.Bind.fixcode' (P.ret [(fv,f)]) (P.ret (Var_32 fv))
	      end
	    
	  in
	    (case LD.E.letpath srep
	       of SOME srep => P.bind srep (fn srep => mkswitch srep drep args)
		| NONE => (betaexp();
			   P.bind (mkfn ()) (fn f => LD.E.allapp' f [srep] ([drep]@args) [])))
	  end

	fun create_dynamic staticrep crep (len,init) = 
	  let
	    val _ = debugdo (fn () => print "Entering create_dynamic\n")
	    fun ifbfloat staticrep [len,init] = 
	      let
		val sv64 = LD.E.unbox init
		val sv32 = P.lift (LD.E.create_array64 len) sv64
	      in sv32
	      end

	    fun notbfloat staticrep [len,init] = LD.E.create_array32 (interp staticrep) len init
	    val argtypes = [fn srep => LD.T.intt B4,fn srep => interp srep]
	    val op32 = ifboxfloat Arrayptr staticrep crep [len,init] argtypes ifbfloat notbfloat
	  in  op32
	  end

	fun create_int is (len,init) = LD.E.create_array32' (LD.T.intt (LU.i2size is)) len init
	fun create_float (len,init)  = LD.E.create_array64' len init
	fun create_other t (len,init) = LD.E.create_array32' t len init

	fun create_empty_dynamic staticrep crep () = 
	  let
	    fun ifbfloat staticrep [] = LD.E.create_empty_array64 ()
	    fun notbfloat staticrep [] = LD.E.create_empty_array32 (interp staticrep) 
	    val op32 = ifboxfloat Arrayptr staticrep crep [] [] ifbfloat notbfloat
	  in  op32
	  end
	fun create_empty_int is () = LD.E.create_empty_array32' (LD.T.intt (LU.i2size is)) 
	fun create_empty_float () = LD.E.create_empty_array64' ()
	fun create_empty_other t () = LD.E.create_empty_array32' t 


	fun sub_dynamic staticrep crep (arr,i) = 
	  let
	    val _ = debugdo (fn () => print "Entering sub_dynamic \n")
	    fun ifbfloat staticrep [arr,i] =
	      let
		val sv64 = LD.E.sub_array64 arr i
		val sv32 = P.lift LD.E.box sv64
	      in sv32
	      end
	    fun notbfloat staticrep [arr,i] = LD.E.sub_array32 (interp staticrep) arr i
	    val args = [arr,i] 
	    val argtypes = [Arrayptr,fn srep => LD.T.intt B4]
	      
	    val op32 = ifboxfloat interp staticrep crep args argtypes ifbfloat notbfloat
	  in op32
	  end
	
	fun sub_int is  (arr,i) = LD.E.sub_array32' (LD.T.intt (LU.i2size is)) arr i
	fun sub_float   (arr,i) = LD.E.sub_array64' arr i
	fun sub_other t (arr,i) = LD.E.sub_array32' t arr i

	fun update_dynamic staticrep crep (arr,i,v) = 
	  let
	    val _ = debugdo (fn () => print "Entering update \n")
	    fun ifbfloat staticrep [arr,i,v] = 
	      let
		val sv64 = LD.E.unbox v
		val pexp = P.lift (LD.E.update_array64 arr i) sv64
	      in pexp
	      end

	    fun notbfloat staticrep [arr,i,v] = LD.E.update_array32 (interp staticrep) arr i v
	    val args = [arr,i,v]
	    val argtypes = [Arrayptr,fn _ => LD.T.intt B4,interp]
	    val op32 = ifboxfloat (fn _ => LD.T.unit()) staticrep crep args argtypes ifbfloat notbfloat
	  in op32
	  end
	fun update_int is  (arr,i,v) = LD.E.update_array32' (LD.T.intt (LU.i2size is)) arr i v
	fun update_float   (arr,i,v) = LD.E.update_array64' arr i v
	fun update_other t (arr,i,v) = LD.E.update_array32' t arr i v

	fun length_dynamic staticrep crep arr = 
	  let
	    val _ = debugdo (fn () => print "Entering length\n")
	    fun ifbfloat  staticrep [arr] = LD.E.length_array64 arr
	    fun notbfloat staticrep [arr] = LD.E.length_array32 (interp staticrep) arr 
	    val args = [arr]
	    val argtypes = [Arrayptr]
	    val op32 = ifboxfloat (fn _ => LD.T.intt B4) staticrep crep args argtypes ifbfloat notbfloat
	  in op32
	  end
	fun length_int is  arr = LD.E.length_array32' (LD.T.intt (LU.i2size is)) arr 
	fun length_float   arr = LD.E.length_array64' arr 
	fun length_other t arr = LD.E.length_array32' t arr 

	fun equal_dynamic staticrep crep (arr1,arr2) = 
	  let
	    val _ = debugdo (fn () => print "Entering equality \n")
	    fun ifbfloat  staticrep [arr1,arr2] = LD.E.equal_array64 arr1 arr2
	    fun notbfloat staticrep [arr1,arr2] = LD.E.equal_array32 (interp staticrep) arr1 arr2
	    val args = [arr1,arr2]
	    val argtypes = [Arrayptr,Arrayptr]
	    val op32 = ifboxfloat (fn _ => LD.T.bool()) staticrep crep args argtypes ifbfloat notbfloat
	  in op32
	  end
	fun equal_int is  (arr1,arr2) = LD.E.equal_array32' (LD.T.intt (LU.i2size is)) arr1 arr2 
	fun equal_float   (arr1,arr2) = LD.E.equal_array64' arr1 arr2 
	fun equal_other t (arr1,arr2) = LD.E.equal_array32' t arr1 arr2 

      end

  end