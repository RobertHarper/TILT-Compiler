(*$import NILTOLIL TranslationDefs Listops *)

structure NiltoLil :> NILTOLIL =
  struct
    structure TD = TranslationDefs
    structure LD = LilDefs
    structure LU = LilUtil
    structure LR = LilRename
    structure NC = NilContext
    structure NU = NilUtil
    structure NN = Normalize
    structure ND = NilDefs
    structure NS = NilSubst
    structure Typeof = Synthesis.Typeof

    structure LD = 
      struct 
	open LD
	structure COps = 
	  struct
	    open COps
	    val sum2ksum' = fn which => fn c => ((sum2ksum' which c) handle any => (PpLil.pp_con c;raise any))
	    val sum2ksum = fn which => fn c => ((sum2ksum which c) handle any => (PpLil.pp_con c;raise any))
	  end
      end
    structure SElim = SingletonElim
    structure Vmap = Name.VarMap
    structure LO = Listops
    structure Subst = LilSubst
    val chatlev = Stats.int("NilToLilChatlev",0)
    val debuglev = Stats.int("NilToLilDebuglev",0)

    (* From Util *)
    val error = fn s => Util.error "niltolil.sml" s

    val debug = Stats.ff("NilToLilDebug")
    val inline_varargs = Stats.ff("NilToLilInlineVarargs")
    val inline_oneargs = Stats.ff("NilToLilInlineOneargs")

    val array_is_taglike = CompilerControl.ArrayIsTaglike

    fun debugdo (i,t) = if (!debug) andalso (i <= !debuglev) then (t(); ()) else ()

    fun chatp i = (!chatlev) >= i 
      
    fun chat i s = if chatp i then print s else ()

    (* From Listops *)
    val map_unzip = LO.map_unzip

    val w2i = TilWord32.toInt
    val i2w = TilWord32.fromInt
    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k


    datatype ('a,'b) choice = A of 'a | B of 'b
    fun chooseA ch = case ch of A a => SOME a | _ => NONE
    fun chooseB ch = case ch of B b => SOME b | _ => NONE

    (* Memoized thunks*)
    datatype 'a thunk = FROZEN of (unit -> 'a) | THAWED of 'a
    type 'a delay = 'a thunk ref
      
    fun delay thunk = ref(FROZEN thunk)
    fun thaw (ref (THAWED v)) = v
      | thaw (r as ref(FROZEN t)) = let val v = t()
					val _ = r := (THAWED v) 
				    in  v
				    end

    (* The NIL doesn't distinguish between lettype and letcon, so
     * we use delays to allow the context of occurence to distinguish 
     * between them.
     *)
    datatype env = Env of {D          : NC.context, 
			   repVars    : Lil.var Vmap.map, 
			   staticreps : Lil.con delay Vmap.map,
			   typedefs   : Lil.con delay Vmap.map,
			   renames    : Lil.var Vmap.map,
			   globals    : string -> Lil.sv32}
      
    fun new_env () = 
      Env {D = NilContext.empty(),
	   repVars = Vmap.empty,
	   staticreps = Vmap.empty,
	   typedefs   = Vmap.empty,
	   renames    = Vmap.empty,
	   globals = fn s => error ("global "^s^" not found")}


    fun get_ctx (Env {D,...}) = D
    fun set_ctx (Env {D,repVars,staticreps,typedefs,renames,globals}) D' = Env {D = D',repVars = repVars,staticreps = staticreps, typedefs = typedefs,renames = renames, globals = globals}

    fun rename_var (Env {renames,...}) v = (case Vmap.find (renames,v)
					      of SOME v => v
					       | NONE => v)
    fun get_rename (Env {D,repVars,staticreps, typedefs,renames,globals}) v = 
      let
	val v' = Name.derived_var v
	val env = 
	  Env {D          = D,
	       repVars    = repVars,
	       staticreps = staticreps,
	       typedefs   = typedefs,
	       renames    = Vmap.insert (renames,v,v'),
	       globals    = globals}
      in (env,v')
      end
    fun get_staticrep (Env {staticreps,...}) v = Vmap.find (staticreps,v)
    fun add_staticrep (Env {D,repVars,staticreps, typedefs, renames,globals}) (v,c) = 
      let
(*	val _ = debugdo (3,fn () => (print "Adding staticrep:\t";Ppnil.pp_var  v;
				     print " = ";PpLil.pp_con (c());
				     print "\n"))*)
      in
	Env {D          = D,
	     repVars    = repVars,
	     staticreps = Vmap.insert (staticreps,v,delay c), 
	     typedefs   = typedefs, 
	     renames    = renames,
	     globals    = globals}
      end
    fun get_typedef (Env {typedefs,...}) v = Vmap.find (typedefs,v)
    fun add_typedef (Env {D,repVars,staticreps, typedefs, renames,globals}) (v,c) = 
      let
(*	val _ = debugdo (3,fn () => (print "Adding typedef:\t";Ppnil.pp_var  v;
				     print " = ";PpLil.pp_con (c());
				     print "\n"))*)
      in
	Env {D          = D,
	     repVars    = repVars,
	     typedefs   = Vmap.insert (typedefs,v,delay c), 
	     staticreps = staticreps,
	     renames    = renames,
	     globals    = globals}
      end

    fun set_var (Env {D,repVars,staticreps,typedefs, renames, globals}) (a,xa) = 
      Env {D = D,
	   staticreps = staticreps, 
	   typedefs   = typedefs,
	   renames    = renames,
	   repVars = Vmap.insert(repVars,a,xa), 
	   globals = globals}

    local 
      val globals : unit P.pexp ref = ref (P.ret ())
      fun add_global_bnds p = globals := P.bind (!globals) (fn () => p)
    in
      fun reset_globals () = globals := (P.ret ())
      fun get_global (Env {globals,...}) s = globals s
      fun add_global (Env {D,repVars,staticreps,typedefs, renames, globals}) (s,mker) = 
	let
	  val thunk = fn () =>
	    let
	      val svp = mker()
	      val sv = P.out svp
	      val bnds = P.Unit.ignore svp
	      val () = add_global_bnds bnds
	    in sv
	    end
	  val f = Util.memoize thunk
	in
	  Env {D = D,
	       repVars = repVars,
	       staticreps = staticreps, 
	       typedefs = typedefs, 
	       renames = renames,
	       globals = fn s' => if s = s' then f() else globals s'
	       }
	end
      fun wrap_with_globals p = P.bind (!globals) (fn () => p)
    end
    local 
      val data : Lil.data list ref = ref []
      fun add_data d = data := d :: !data
    in
      fun add_dtuple (l,c,q,svs) = add_data (Lil.Dtuple (l,c,q,svs))
      fun add_dboxed (l,sv)    = add_data (Lil.Dboxed (l,sv))
      fun add_darray (l,sz,t,sv) = add_data (Lil.Darray (l,sz,t,sv))
      fun add_dcode (l,f)      = add_data (Lil.Dcode (l,f))

      fun add_string (l,s)     = 
	let
	  fun char2val c = Prim.uint(Prim.W8,TilWord64.fromInt (ord c))
	  val cs = String.explode s
	  val vs = map char2val cs
	in add_darray (l,Lil.B1,LD.T.intt Lil.B1,vs)
	end

      fun get_data () = rev (!data)
      fun reset_data () = data := []
    end

    fun new_rep_var env v = 
      let 
	val new_var = Name.fresh_named_var ((Name.var2string v)^"_rep")
	val env = set_var env (v,new_var)
      in (env,new_var)
      end

    fun new_rep_var_list env vs = 
      let fun folder (v,env) = let val (env,v) = new_rep_var env v in (v,env) end
	val (vs,env) = LO.foldl_acc folder env vs
      in (env,vs)
      end

    fun rep_var (Env {repVars,...}) v = 
      (case Vmap.find (repVars,v)
	 of SOME x_a => x_a
	  | _ => error "rep_var not found")

    fun NilWHNF env c = 
      (case NN.reduce_hnf (get_ctx env,c)
	 of (true, c) => c
	  | (false,c) => c)
	 
    fun NilProjectRecordType (Env {D,...}) lbl con = 
      NN.projectRecordType (D,con,lbl)

    fun nil_strip_arrow (Env {D,...}) con = 
      NN.strip_arrow_norm D con

    fun type_of (Env {D,...}) e = 
      let
	val _ = debugdo (5,fn () => (print "calling type_of with exp:\n\t";
				     Ppnil.pp_exp e;
				     print "\n"))
	val t = NN.type_of (D,e)
	val _ = debugdo (5,fn () => (print "type_of returned type:\n\t";
				     Ppnil.pp_con t;
				     print "\n"))
      in t
      end

    fun kind_of (Env {D,...}) c = 
      let
	val _ = debugdo (5,fn () => (print "calling kind_of with con:\n\t";
				     Ppnil.pp_con c;
				     print "\n"))
	val k = NC.kind_of (D,c)
	val _ = debugdo (5,fn () => (print "kind_of returned kind:\n\t";
				     Ppnil.pp_kind k;
				     print "\n"))
      in k
      end

    fun nil_bool_con () = NilPrimUtilParam.con_bool (NC.empty())    

    fun find_type (Env {D,...}) a = ((NC.find_con (D,a)) handle NC.Unbound => error ("Unbound NIL con variable: "^(Name.var2string a)))
    fun add_type env (x,t) = set_ctx env (NC.insert_con (get_ctx env,x,t))
    fun add_type_list env xts = set_ctx env (NC.insert_con_list (get_ctx env,xts))


    fun add_kind env (a,k) = set_ctx env (NC.insert_kind (get_ctx env,a,k))
    fun add_cbnd env cb    = set_ctx env (NC.insert_cbnd (get_ctx env,cb))
    fun add_kind_list env aks = set_ctx env (NC.insert_kind_list (get_ctx env,aks))

    fun erased_kind_of env c = SElim.erasek (get_ctx env) (kind_of env c)

    fun index_from_tuple env c l = 
      let
	val lvks = 
	  (case kind_of env c
	     of Nil.Record_k lvks => lvks
	      | _ => error "Not a con tuple")
	val i = LO.index_of (fn ((l',_),_) => Name.eq_label (l,l')) lvks
      in (i,List.length lvks)
      end

    fun index_from_tuple' env c l = let val (i,len) = index_from_tuple env c l in (i2w i,len) end

    fun index_from_tuple_type env c l = 
      let
	val ls = 
	  (case NU.strip_record (NilWHNF env c)
	     of SOME (ls,cs) => ls
	      | _ => error "Not a tuple type")
	val i = LO.index_of (fn l' => Name.eq_label (l,l')) ls
      in i
      end

    fun index_from_tuple_type' env c l = i2w(index_from_tuple_type env c l)

    fun prim_get_type (Env {D,...}) p cons = 
      let
	val (_,args,ret) = NilPrimUtil.get_type D p cons
      in (args,ret)
      end

    fun debugwrap name pp_item f env item = 
      let
	val () = debugdo(6,fn () => (print (name^" called with:\n");
				     pp_item item;print "\n"))
	val res = f env item
	val () = debugdo(5,fn () => (print (name ^ " returning:\n")))
      in res
      end

    fun ktrans (k : Nil.kind) : Lil.kind = 
      (case k
	 of Nil.Type_k => TD.Tmil ()
	  | Nil.Record_k [(lv,k)] => ktrans k  (* Special case - get rid of singleton tuples *)
	  | Nil.Record_k lvks => 
	   let
	     val ks = LO.map (fn (_,k) => ktrans k) lvks 
	   in LD.K.ntuple ks
	   end
	  | Nil.Arrow_k (_ ,args,ret) => 
	   let 
	     val args = LO.map (fn (_,k) => ktrans k) args
	     val ret  = ktrans ret
	   in LD.K.narrow args ret
	   end
	  | _ => error "NiltoLil shouldn't see singletons")
	 
	 
     val i_sizetrans = LU.i2size
     val f_sizetrans = LU.f2size
  
    fun primcontrans env (p : Nil.primcon) (args : Nil.con list) : Lil.con = 
      let
	val ctrans = ctrans env

	fun mk0 constr pcon = constr (Lil.mk_pcon pcon)
	fun mk1 constr pcon arg = constr (LD.C.pcon_app pcon [arg])

	val Ptr0 = mk0 TD.PtrRep
	val Ptr1 = mk1 TD.PtrRep
	val Other0  = mk0 TD.OtherRep
	val Other1  = mk1 TD.OtherRep


      in
	case p
	  of Nil.Record_c _ => 
	    let
	      val types = map (ctrans_interp env) args
	    in TD.TupleRep types
	    end
	   | _ => 
	case args
	  of [] =>
	    (case p
	       of Nil.Int_c sz       => 
		 let
		   val sz = i_sizetrans sz
		   val t = (case sz
			      of Lil.B4 => LD.T.intt Lil.B4
			       | _ => LD.T.embed sz (LD.T.intt sz))
		 in TD.OtherRep t
		 end
		| Nil.BoxFloat_c fsz => TD.BFloatRep ()
		| Nil.Exn_c          => TD.OtherRep (LD.T.exn())
		| Nil.IntArray_c sz  => 
		 let
		   val sz = i_sizetrans sz
		 in 
		   if !array_is_taglike then 
		     TD.OtherRep (LD.T.arrayptr sz (LD.T.intt sz))
		   else 
		     TD.PtrRep (LD.T.array sz (LD.T.intt sz))
		 end
		| Nil.IntVector_c sz  => 
		 let
		   val sz = i_sizetrans sz
		 in 
		   if !array_is_taglike then 
		     TD.OtherRep (LD.T.arrayptr sz (LD.T.intt sz))
		   else 
		     TD.PtrRep (LD.T.array sz (LD.T.intt sz))
		 end
		| Nil.FloatArray_c sz  => 
		 let
		   val sz = f_sizetrans sz
		 in 
		   if !array_is_taglike then 
		     TD.OtherRep (LD.T.arrayptr sz (LD.T.float()))
		   else 
		     TD.PtrRep (LD.T.array sz (LD.T.float()))
		 end
		| Nil.FloatVector_c sz  => 
		 let
		   val sz = f_sizetrans sz
		 in 
		   if !array_is_taglike then 
		     TD.OtherRep (LD.T.arrayptr sz (LD.T.float()))
		   else 
		     TD.PtrRep (LD.T.array sz (LD.T.float()))
		 end
		| Nil.Loc_c          => error "Locatives not supported"
		| Nil.Float_c fsz    => error "Float is not a constructor"
		| _                  => error "Bad primcon: not enough args")
	   | [c] =>
	    (case p 
	       of Nil.Array_c        => 
		 if !array_is_taglike then 
		   TD.OtherRep (TD.Arrayptr (ctrans c))
		 else 
		   TD.PtrRep (TD.Array (ctrans c))
		| Nil.Vector_c       => 
		 if !array_is_taglike then 
		   TD.OtherRep (TD.Arrayptr (ctrans c))
		 else 
		   TD.PtrRep (TD.Array (ctrans c))
		| Nil.Ref_c          => TD.PtrRep (LD.T.refc (TD.interp (ctrans c)))
		| Nil.Exntag_c       => Other1 Lil.Dyntag_c (TD.interp (ctrans c))
		| Nil.GCTag_c        => TD.PtrRep (LD.T.tuple'[])  (*We don't use GCtags right now*)

	        | Nil.Sum_c {tagcount,totalcount,known} =>
		 let
		   val nontagcount = w2i(TilWord32.uminus(totalcount,tagcount))

		   val carrier = c

		   val single_carrier = nontagcount = 1

		   val args = 
		     if single_carrier
		       then
			 let 
			   val argrep = ctrans carrier
			   val t = TD.interp argrep
			 in [TD.taglike_interp argrep]
			 end
		     else
		       let 
			 fun ith_arm i = 
			   let
			     val nilc = Nil.Proj_c(carrier,NilUtil.generate_tuple_label (i+1))
			     val tag = LD.T.tag ((LU.i2w i) + tagcount)
			     val t = LD.T.tupleptr' [tag,ctrans_interp env nilc]
			   in t
			   end
			 val cons  = LO.map0count ith_arm nontagcount
		       in
			 cons
		       end
		   val t = 
		     case known 
		       of SOME k => LD.T.ksum' k tagcount args
			| NONE => LD.T.sum' tagcount args
		 in TD.OtherRep t
		 end
		| _ => error "Wrong number of args (1) to con primitive")
	   | [argc,resc] => 
	    (case p 
	       of Nil.Vararg_c _ => 
		 let 
		   val resc = ctrans resc
		   val argc = ctrans argc
		   val srep = TD.Vararg argc resc
		 in
		   TD.OtherRep srep
		 end
		| _ => error "Wrong number of args (2) to con primitive")
	   | many => error "Wrong number of args (>2) to non Recordcon primitive"
      end
		      
    and ctrans_interp env (c : Nil.con) : Lil.con = TD.interp (ctrans env c)
    and ctrans env (c : Nil.con) : Lil.con = 
      let

      in
	case c
	  of Nil.Prim_c (p,args) => primcontrans env p args
	  | Nil.AllArrow_c {openness,
			    effect,
			    tFormals = [],
			    eFormals,
			    fFormals,
			    body_type} =>
	    let
	      val args32 = map (ctrans_interp env) eFormals
	      val Float = Lil.mk_pcon Lil.Float_c
	      val args64 = LO.map0count (fn _ => Float) (TilWord32.toInt fFormals)
	      val ret = ctrans_interp env body_type
	    in TD.OtherRep(LD.T.arrow' args32 args64 ret)
	    end
	| Nil.AllArrow_c {openness,
			  effect,
			  tFormals,
			  eFormals,
			  fFormals,
			  body_type} => error "Forall is not a constructor"

	(*Special case for for single recursion - just to make things more readable,
	 * and to agree with the general policy for translating singleton con records
	 *)
        | Nil.Mu_c (isrec,[(a,c)]) =>
	    let
	      val env = if isrec 
			  then add_kind env (a,Nil.Type_k)
			else env

	      val unit_c = LD.C.star()
	      val unit_k = LD.K.unit()
		
	      val outer_k = Lil.mk_kind (Lil.Arrow_k (unit_k,LD.K.T32()))

	      val f = LD.C.lambda' outer_k 
		(fn r => LD.C.lambda' unit_k 
		 (fn w => 
		  let
		    val env = add_typedef env (a,fn () => LD.C.app r unit_c)
		    val env = add_staticrep env (a,fn () => TD.OtherRep (LD.C.app r unit_c))
		    val c = ctrans_interp env c
		  in c
		  end))
		
	      val res = TD.OtherRep (LD.C.appn (LD.C.k_app (Lil.mk_pcon Lil.Rec_c) unit_k) [f,unit_c])
	    in res
	    end
        | Nil.Mu_c (isrec,arms) =>
	    let
	      val (vars,cons) = LO.unzip arms
	      val len = List.length cons
	      val env = if isrec 
			  then let fun folder(v,env) = add_kind env (v,Nil.Type_k)
			       in  List.foldl folder env vars end
			else env

	      val unit_c = LD.C.star()
	      val unit_k = LD.K.unit()

	      val k = LD.K.sum (LO.map0count (fn _ => unit_k) len)

	      val outer_k = Lil.mk_kind (Lil.Arrow_k (k,LD.K.T32()))

	      val f = LD.C.lambda' outer_k 
		(fn r => LD.C.lambda' k 
		 (fn w => 
		  let
		    fun self_i iw = LD.C.app r (LD.C.inj iw k unit_c)
		    fun folder (i,a,env) = 
		      let 
			val iw = LU.i2w i
			val env = add_typedef env (a,fn () => self_i iw)
			val env = add_staticrep env (a,fn () => TD.OtherRep (self_i iw))
		      in env
		      end
		    val env = LO.foldl2 folder env (LO.count len,vars)
		    val cons = map (ctrans_interp env) cons

		    val arms = LO.mapcount (fn (i,c) => (LU.i2w i,fn _ => c))cons
		  in LD.C.sumcase w arms NONE
		  end))
		
		
	      val injs = 
		LO.map0count (fn i => LD.C.inj (LU.i2w i) k unit_c) len
		
	      val fields  = 
		List.map
		(fn p => TD.OtherRep (LD.C.appn (LD.C.k_app (Lil.mk_pcon Lil.Rec_c) k) [f,p]))
		injs
	      val res = LD.C.ntuple fields
	    in res
	    end
	  | Nil.Let_c (_,cbnds,body) => 
	    let
	      val env = cbnds_trans env cbnds
	      val c = ctrans env body
	    in c
	    end
	  | Nil.Var_c v => 
	    (case get_staticrep env v 
	       of SOME thunk => thaw thunk
		| NONE => Lil.mk_con (Lil.Var_c v))
	  | Nil.Crecord_c [(l,c)] => ctrans env c
	  | Nil.Crecord_c fields  => LD.C.ntuple (map (fn (l,c) => ctrans env c) fields)
	  | Nil.Proj_c (c,l) => 
	     let 
	       val (i,len) = index_from_tuple env c l 
	       val crec = ctrans env c
	     in
	       if len = 1 then crec
	       else LD.C.nproj crec i
	     end
	  | Nil.App_c (c1,args) => LD.C.appn (ctrans env c1) (map (ctrans env) args)
	  | Nil.Coercion_c _ => error "Coercion_c is not a constructor"
	  | Nil.ExternArrow_c (args,ret) => error "ExternArrow is not a constructor"
	  | Nil.Closure_c _ => error "Unexpected closure"
      end

    (* Does not add the kind to the context, to avoid double entry in
     * the translation of Con_cb
     *)
    and cbnd_trans env bnd = 
      let
	val res = 
	  (case bnd
	     of Nil.Con_cb(a,c) => 
	       let
		 val thunk = fn () => 
		   let
		     val _  = debugdo (4, fn () => (print "\t rewriting cbnd:\n";
						    Ppnil.pp_conbnd bnd;
						    print "\n"))

		     val c = ctrans env c
		     val _ = debugdo (4, fn () => (print "\t rewrote Con_cb to :\n";
						   PpLil.pp_con c;
						   print "\n"))
		   in c
		   end
		 val env = add_staticrep env (a,thunk)
	       in env
	       end
	      | Nil.Open_cb (a,vks,body) => 
	       let
		 val thunk = fn() =>
		   let
		     val _  = debugdo (4, fn () => (print "\t rewriting cbnd:\n";
						    Ppnil.pp_conbnd bnd;
						    print "\n"))
		       
		     val inner_env = add_kind_list env vks
		     val vks = LO.map_second ktrans vks
		     val body = ctrans inner_env body
		     val lam = LD.C.nlambda vks body
		     val _ = debugdo (4, fn () => (print "\t rewrote Open_cb to :\n";
						   PpLil.pp_con lam;
						   print "\n"))
		   in lam
		   end
		 val final_env = add_staticrep env (a,thunk)
	       in final_env
	       end
	      | _ => error "Code_cb shouldn't happen")
      in res
      end

    and cbnds_trans env cbnds = 
      (case cbnds 
	 of [] => env 
	  | (cbnd::cbnds) => cbnds_trans (add_cbnd (cbnd_trans env cbnd) cbnd) cbnds)


    val ctrans = debugwrap "ctrans" Ppnil.pp_con ctrans

    fun dyn_rep_type env (c : Nil.con, k : Nil.kind) : Lil.con =
      let

      in
	case k
	  of Nil.Type_k => TD.R (ctrans env c)
	   | Nil.Record_k [] => LD.T.unit()
	   | Nil.Record_k [((l,v),k)] => dyn_rep_type env (Nil.Proj_c (c,l),k)
	   | Nil.Record_k lvks => 
	    let
	      val _ = debugdo (4,fn () => print "Representing record kind\n")

	      val (lvs,ks) = LO.unzip  lvks 
	      val cons = map (fn (l,_) => Nil.Proj_c (c,l)) lvs
	      val types = LO.map2 (dyn_rep_type env) (cons,ks)

	      val res = LD.T.ptr (LD.T.tuple' types)

	      val _ = debugdo (4,fn () => (print "Tuple type is: \n";
					   PpLil.pp_con res;
					   print "\n"))
	    in res
	    end
	   | Nil.Arrow_k (_ ,args,ret) => 
	    let 
	      (* Rename the variables here to avoid variable conflicts.
	       * Note that singelim guarantees that the variables
	       * do not appear free anywhere 
	       *)
	      val args = LO.map_first Name.derived_var args
	      val vars = List.map #1 args
	      val cFormals = LO.map_second ktrans args
	      val env = add_kind_list env args
	      val eFormals = List.map (var_dyn_rep_type env) args
	      val rettype  = dyn_rep_type env (Nil.App_c (c, map Nil.Var_c vars),ret)
	      val arrow = LD.T.arrow' eFormals [] rettype
	    in LD.T.nary_forall cFormals arrow
	    end
	   | _ => error "NiltoLil shouldn't see singletons" 
      end
    
    and var_dyn_rep_type env (v,k) = dyn_rep_type env (Nil.Var_c v,k)

    val dyn_rep_type = debugwrap "dyn_rep_type" (fn (c,k) => (Ppnil.pp_con c;print "\n";Ppnil.pp_kind k)) dyn_rep_type


    fun dyn_rep_list dyn_rep_fn env clist = P.List.map_from_list (dyn_rep_fn env) clist

    (*
     * dyn_rep env c --> (bnds, sv) s.t.
     * let (rev bnds) in sv   is the dynamic representation of c
     *)

    fun dyn_rep env (c : Nil.con) : Lil.sv32 P.pexp = P.SV32.from_op (dyn_rep' env c)
    and dyn_rep' env (c : Nil.con) : Lil.op32 P.pexp =
      let
	fun dyn_rep_tuple env c i  = TD.tuple' i (TD.R (ctrans env c))

	fun dyn_rep_other env c   = TD.otherwise' (TD.R (ctrans env c))
	fun dyn_rep_other' env c   = TD.otherwise (TD.R (ctrans env c))

	fun dyn_rep_bfloat env c    = TD.bfloat' (TD.R (ctrans env c))
	fun dyn_rep_ptr env c = TD.ptr' (TD.R (ctrans env c))

	fun dyn_rep_array env c = 
	  if !array_is_taglike then 
	    dyn_rep_other env c
	  else 
	    dyn_rep_ptr env c

      in
	case c
	  of Nil.Var_c v => P.ret (Lil.Val (Lil.Var_32 (rep_var env v)))
	   | Nil.Let_c (_,cbnds,body) => 
	    let
	      val p_env = dyn_rep_cbnds env cbnds
	      val body        = P.bind p_env (fn env => dyn_rep' env body)
	    in body
	    end

	   | Nil.App_c (c1,args) => 
	    let
	      val f = dyn_rep env c1
	      val targs = map (ctrans env) args
	      val tapp = P.map (LD.E.nary_tapp' targs) f

	      val argvals = P.List.map_from_list (dyn_rep env) args

	      val appop = P.bind tapp (fn tapp => P.bind argvals (fn argvals => P.ret(Lil.App (tapp,argvals,[]))))
	    in appop
	    end
	   | Nil.Proj_c (c,l) => 
	    let 
	      val (i,len) = index_from_tuple' env c l
	      val pexp = if len = 1 then dyn_rep' env c
			 else P.lift (LD.E.select' i) (dyn_rep env c)
	    in pexp 
	    end
	   | Nil.Crecord_c [] => P.SV32.to_op (LD.E.unit ())
	   | Nil.Crecord_c [(l,c)] => dyn_rep' env c
	   | Nil.Crecord_c fields => 
	    let
	      val vals = P.List.map_from_list (fn (l,c) => dyn_rep env c) fields
	      val tup = P.lift LD.E.tuple' vals
	    in tup
	    end
	  
	   | Nil.Prim_c (p,args) => 
	    (case p
	       of Nil.Int_c sz => dyn_rep_other env c
		| Nil.Exntag_c => dyn_rep_other env c
		| Nil.Sum_c _  => dyn_rep_other env c
		| Nil.Record_c lbls => dyn_rep_tuple env c (List.length lbls)
		| Nil.BoxFloat_c sz => dyn_rep_bfloat env c
		| Nil.Float_c sz => error "Float is not a constructor"
		| Nil.Exn_c => dyn_rep_other env c
		| Nil.Array_c => dyn_rep_array env c
		| Nil.Vector_c => dyn_rep_array env c                              
		| Nil.IntArray_c _ => dyn_rep_array env c
		| Nil.IntVector_c _ => dyn_rep_array env c                         
		| Nil.FloatArray_c _ => dyn_rep_array env c
		| Nil.FloatVector_c _ => dyn_rep_array env c 
		| Nil.Ref_c => dyn_rep_ptr env c
		| Nil.Loc_c => error "Locatives not supported"         
		| Nil.Vararg_c _ => dyn_rep_other env c
		| Nil.GCTag_c  => dyn_rep_ptr env c)
	    
	   | Nil.Mu_c (isrec,arms) => 
	     let 
	       val len = List.length arms
	       val projs = LO.map0count 
		 (fn i => Nil.Proj_c (c,NU.generate_tuple_label(i+1))) len
	       val fields = dyn_rep_list dyn_rep_other' env projs
	       val mu = P.lift (fn fields => case fields of [sv] => P.ret (Lil.Val sv) | _ => LD.E.tuple' fields) fields
	     in mu
	     end

	   | Nil.AllArrow_c {openness,
			     effect,
			     tFormals = [],
			     eFormals,
			     fFormals,
			     body_type} => dyn_rep_other env c
	   | Nil.AllArrow_c {openness,
			     effect,
			     tFormals,
			     eFormals,
			     fFormals,
			     body_type} => error "Forall is not a constructor"
	   | Nil.Coercion_c _ => error "Coercion_c is not a constructor"
	   | Nil.ExternArrow_c (args,ret) => error "ExternArrow is not a constructor"
	   | Nil.Closure_c _ => error "Unexpected closure"
      end
    and dyn_rep_cbnd env cbnd : (env P.pexp) = 
      let
	val env = cbnd_trans env cbnd
      in
	case cbnd
	  of Nil.Con_cb(a,c) => 
	    let
	      val c_rep = dyn_rep' env c
	      val (env,x_a) = new_rep_var env a
	      val env = add_kind env (a,Nil.Single_k c)

	      val bnd_rep = P.Bind.op32' x_a c_rep (P.ret env)
		
	    in bnd_rep
	    end
	   | Nil.Open_cb (a,vks,body) => 
	    let
	      val (env,x_a) = new_rep_var env a

	      val outer_env = add_kind env (a,Nil.Arrow_k (Nil.Open,vks,Nil.Single_k body))

	      val (vks,body) = 
		let
		  fun rename ((a,k),subst) =
		    let
		      val a' = Name.derived_var a
		    in ((a',k),NS.C.sim_add subst (a,Nil.Var_c a'))
		    end
		  val (vks,subst) = LO.foldl_acc rename (NS.C.empty()) vks
		  val body = NS.substConInCon subst body
		in (vks,body)
		end

	      fun folder ((a_i,k_i),env) = 
		let
		  val (env,x_ai) = new_rep_var env a_i
		  val env = add_kind env (a_i,k_i)
		  val t_i = var_dyn_rep_type env (a_i,k_i)
		  val k_i = ktrans k_i
		in (((a_i,k_i),(x_ai,t_i)),env)
		end

	      val (vks_xts,inner_env) = LO.foldl_acc folder env vks
		
		
	      val (vks,xts) = LO.unzip vks_xts

	      val rettype = dyn_rep_type inner_env (body,erased_kind_of inner_env body)

	      val body = P.Lili.to_exp (dyn_rep inner_env body)
		
	      val newf = Lil.Function {tFormals  = vks,
				       eFormals  = xts,
				       fFormals  = [],
				       rtype     = rettype,
				       body      = body}
		
	    in P.Bind.fixcode' (P.ret [(x_a,newf)]) (P.ret outer_env)
	    end
	   | _ => error "Code_cb shouldn't happen"
      end	 
    and dyn_rep_cbnds env cbnds : (env P.pexp) = 
      let
	val dyn_rep_cbnd = fn (cbnd,env) => dyn_rep_cbnd env cbnd
	val bnds_env = P.List.foldl_from_list dyn_rep_cbnd env cbnds
      in bnds_env
      end

    val dyn_rep = debugwrap "dyn_rep" Ppnil.pp_con dyn_rep

    fun ttrans env t = 
      let
      in
	case t 
	  of Nil.Var_c a => 
	    (case get_typedef env a
	       of SOME t => thaw t
		| NONE => TD.interp (ctrans env t))
	   | Nil.Coercion_c {vars,from,to} => 
	    let
	      val vks = map (fn a => (a,Nil.Type_k)) vars
	      val env = add_kind_list env vks
	      val from = ttrans env from
	      val to   = ttrans env to
	      val lvks = map (fn a => (a,TD.Tmil())) vars
	    in LD.T.nary_forall lvks (LD.C.pcon_app Lil.Coercion_c [from,to])
	    end
	   | Nil.ExternArrow_c (args,ret) => 
	    let
	      val args = map (txxtrans env) args
	      val ret = txxtrans env ret
	      fun mapper p = 
		(case p 
		   of A a => LD.C.inl (LD.K.T32or64()) a
		    | B b => LD.C.inr (LD.K.T32or64()) b)
	      val args = List.map mapper args
	      val (size,ret) = 
		(case ret
		   of A t32 => (Lil.B4,t32)
		    | B t64 => (Lil.B8,t64))
	    in
	      LD.T.externarrow' size args ret
	    end
	   | Nil.AllArrow_c {tFormals,eFormals,fFormals,body_type,...} => 
	    let
	      val vks = LO.map_second ktrans tFormals
	      val env = add_kind_list env tFormals
	      val repFormals = LO.map (var_dyn_rep_type env) tFormals
	      val eFormals = map (ttrans env) eFormals
	      val k = TilWord32.toInt fFormals
	      val float = LD.T.float64
	      val fFormals = LO.map0count (fn _ => LD.T.float64()) k
	      val ret  = ttrans env body_type
	    in LD.T.allarrow' vks (repFormals@eFormals) fFormals ret
	    end
	   | Nil.Let_c (_,cbnds,body) => 
	    let
	      val env = tbnds_trans env cbnds
	    in ttrans env body
	    end

	   | Nil.Prim_c (Nil.Record_c lbls,fields) =>
	    let
	      val types = map (ttrans env) fields
	    in LD.T.ptr (LD.T.tuple' types)
	    end
	   | T_c => TD.interp (ctrans env T_c)
      end
    and txxtrans env c : (Lil.con, Lil.con) choice = 
      (case NU.strip_float (NilWHNF env c)
	 of SOME Prim.F64 => B (LD.T.float ())
	  | _ => A (ttrans env c))

    (* For compiletime type bindings, we don't know (a-priori) whether they
     * are going to be used as types, or as type constructors, so
     * we don't know whether to create the static representation (::Tmil)
     * or to create the type translation (::T32).  So we check the kind:
     * if it is Type, then we translate as a type and generate a typedef.
     * Otherwise, we compute the static representation.
     *)
    and tbnd_trans env cbnd = 
      (case cbnd
	 of Nil.Con_cb(a,c) => 
	   (case erased_kind_of env c
	      of Nil.Type_k => 
		let
		  val env = add_typedef env (a,fn () => ttrans env c)
		  val env = add_staticrep env (a,fn () => ctrans env c)
		in env
		end
	       | _ => cbnd_trans env cbnd)
	  | _ => cbnd_trans env cbnd)
    and tbnds_trans env cbnds = 
      (case cbnds 
	 of [] => env 
	  | (cbnd::cbnds) => tbnds_trans (add_cbnd (tbnd_trans env cbnd) cbnd) cbnds)

    val ttrans = debugwrap "ttrans" Ppnil.pp_con ttrans



    fun sv32_trans (env : env) (e : Nil.exp) : Lil.sv32 * Nil.con = 
      (case e 
	 of Nil.Var_e v => (Lil.Var_32 (rename_var env v),find_type env v)
	  | Nil.Const_e v =>
	   (case v
	      of (Prim.int (intsize, w)) =>
		(Lil.Const_32 (Prim.int (intsize, w)),Nil.Prim_c (Nil.Int_c intsize,[]))
	       | (Prim.uint (intsize, w)) =>
		(Lil.Const_32 (Prim.uint (intsize, w)),	Nil.Prim_c (Nil.Int_c intsize,[]))
	       | (Prim.intvector (Prim.W8, v)) =>
		let
		  val svs = Array.foldr (op ::) nil v
		  val svs = map (sv32_trans' env) svs
		  val lsz = i_sizetrans (Prim.W8)
		  val l = Name.fresh_internal_label "vec_const"
		  fun getval sv = 
		    (case sv of Lil.Const_32 v => v | _ => error "Vector constant element not a const!")
		  val vs = map getval svs
		  val () = add_darray (l,lsz,LD.T.intt lsz,vs)
		in (Lil.Label l,Nil.Prim_c (Nil.IntVector_c (Prim.W8),[]))
		end
	       | _ => (print "ERROR: ";
		       Ppnil.pp_exp e;
		       error "sv32_trans got a large constant"))
	  | _ => (print "ERROR: ";
		  Ppnil.pp_exp e;
		  error "sv32_trans got a large value"))

    and sv32_trans' (env : env) (e : Nil.exp) : Lil.sv32 = #1 (sv32_trans env e)

    fun sv64_trans (env : env) (e : Nil.exp) : Lil.sv64 * Nil.con = 
      (case e 
	 of Nil.Var_e v => (Lil.Var_64 (rename_var env v),find_type env v)
	  | _ => (print "ERROR: ";
		  Ppnil.pp_exp e;
		  error "sv64_trans got a non-variable"))

    fun sv64_trans' (env : env) (e : Nil.exp) : Lil.sv64 = #1 (sv64_trans env e)

    fun svxx_trans (env : env) (e : Nil.exp,t : Nil.con) : (Lil.sv32,Lil.sv64) choice = 
      (if NU.is_float_c (NilWHNF env t)
	 then B (sv64_trans' env e)
       else A (sv32_trans' env e))

    fun primarg_trans (env : env) (e : Nil.exp,t : Nil.con) : Lil.primarg = 
      (case (NilWHNF env t)
	 of Nil.Prim_c(Nil.Float_c Prim.F64,[]) => Lil.arg64 (sv64_trans' env e)
	  | Nil.Prim_c(Nil.Int_c Prim.W8,[])    => Lil.slice (Lil.B1,sv32_trans' env e)
	  | _    => Lil.arg32 (sv32_trans' env e))

    fun exp2exp32_trans (env : env) (e : Nil.exp) : (Lil.exp * Nil.con) = 
      let 
	val (oper,t) = exp32_trans env e
      in (P.Lili.op_to_exp oper,t)
      end
    and exp64_trans (env : env) (e : Nil.exp) : Lil.op64 = 
      let
      in
	case e of 
	    Nil.Var_e _ =>
	    let val sv = sv64_trans' env e
	    in Lil.Val_64 sv
	    end
	  | Nil.Prim_e (Nil.NilPrimOp (Nil.unbox_float Prim.F64),[],[],[sv]) =>  Lil.Unbox (sv32_trans' env sv)
	  | Nil.Prim_e (Nil.PrimOp prim,_,[],svs) =>
	      let
		val (arg_types,return_type) = prim_get_type env prim []
		val _ = if NU.is_float_c return_type then () 
			else error "64 bit prim returns non-float!"
		val args = LO.map2 (primarg_trans env) (svs,arg_types)
	      in Lil.Prim64 (prim,args)
	      end
	  | Nil.ExternApp_e (f,args) =>
	    let 
	      val (f_sv,c) = sv32_trans env f
	      val (argtypes,ret) =
		(case NU.strip_externarrow (NilWHNF env c)
		   of SOME res => res
		    | NONE => error "Externapp of non externarrow")

	      val args = LO.map2 (primarg_trans env) (args,argtypes)
	      val oper = Lil.ExternAppf (f_sv,args)
	    in oper
	    end

          | Nil.Const_e v => 
	    let
	      val sv = Lil.Const_64 (value64_trans env v)
	      val l = Name.fresh_internal_label "float"
	      val () = add_dboxed (l,sv)
	      val op64 = Lil.Unbox (Lil.Label l)
	    in op64
	    end
	  | _ => error "Not a valid 64 bit expression"
      end
    and value64_trans (env : env) (v : (Nil.con,Nil.exp) Prim.value) : (Lil.con,Lil.primarg) Prim.value = 
      (case v
	 of Prim.float (floatsize, f) => Prim.float (floatsize, f)
	  | _ => error "Expected 64 bit value")

    and nilprim32_trans env (p,cons,exps) : (Lil.op32 P.pexp * Nil.con) = 
      let
	fun do_sum_project which argcon argexp static = 
	  let
	    val sumcon_hnf = NilWHNF env argcon
	      
	    val (tagcount,totalcount,carrier) = 
	      (case NU.strip_sum sumcon_hnf of
		 SOME (tc,total,NONE,c) => (tc,total,c)
	       | SOME (tc,total,SOME _,c) => error "Argument to project is a known sum"
	       | NONE => error "Argument to project not a sum")
		 
	    val sum = ttrans env sumcon_hnf
	    val ksum = LD.COps.sum2ksum' which sum
		 
	    val nontagcount = w2i(TilWord32.uminus(totalcount,tagcount))
	      
	    val single_carrier = nontagcount = 1
	    val argsv = sv32_trans' env argexp
	    (* Coerce from known sum type *)
	    val sumval = LD.E.project' ksum argsv
	  in
	    if single_carrier then  (*Typecase *)
	      let 
		(* Typecase *)
		val rep = 
		  if static then
		    if NU.is_taglike (NilWHNF env carrier) then
		      LD.E.select' 0w0 sumval
		    else
		      P.ret (Lil.Val sumval)
		  else 
		    let
		      val drep = dyn_rep env carrier
		      val srep = ctrans env carrier
		      val project_dyn = get_global env "project_dyn"
		      val op32 = P.bind drep 
			(fn drep => TD.mk_project_dynamic_app project_dyn srep drep sumval)
		    in
		      op32
		    end
		  
	      in (rep,carrier)
	      end
	    else (* Value is tagged and boxed*)
	      let 
		val which = TilWord32.uminus(which,tagcount)
		  
		val rtype = Nil.Proj_c(carrier,NilUtil.generate_tuple_label ((w2i which) + 1))
		(*Project out the value *)
		val rep  = LD.E.select' 0w1 sumval
	      in
		(rep,rtype)
	      end
	  end
	fun do_inject_sum_tag env sumtype sumcon = 
	  let
	    val rtype = NU.convert_sum_to_special(NilWHNF env sumcon, sumtype)
	    val t = ttrans env rtype
	    val tag = LD.E.tag sumtype
	    val tag = P.SV32.to_op (P.map (LD.E.injunion' t) tag)
	  in (tag,rtype)
	  end

	fun do_inject_sum sumtype sumcon exps static = 
	  let
	    val sumcon_hnf = NilWHNF env sumcon
	    val (tagcount,totalcount,carrier) = 
	      (case NU.strip_sum sumcon_hnf of
		 SOME (tc,total,NONE,c) => (tc,total,c)
	       | SOME _ => error "inject type argument has special sum type"
	       | NONE => error "inject given invalid type argument")

	    val rtype = NU.convert_sum_to_special(sumcon_hnf, sumtype)

	    val nontagcount = w2i(TilWord32.uminus(totalcount,tagcount))

	    val single_carrier = nontagcount = 1

	    val argsv = 
	      case exps
		of [v]    => sv32_trans' env v
		 | [t,v]  => sv32_trans' env v  (* Ignore GC tag if present *)
		 | _ => error "too many inject args"
	    val value = 
	      if single_carrier then  (*Typecase *)
		let 
		  (* Typecase *)
		  val v = 
		    if static then
		      if NU.is_taglike (NilWHNF env carrier) then
			LD.E.tuple [argsv]
		      else
			P.ret (argsv)
		    else 
		      let
			val drep = dyn_rep env carrier
			val srep = ctrans env carrier
			val inject_dyn = get_global env "inject_dyn"
			val op32 = P.bind drep 
			  (fn drep => TD.mk_inject_dynamic_app inject_dyn srep drep argsv)
		      in P.SV32.from_op op32
		      end
		in v
		end
	      else (* Value is tagged and boxed*)
		let 
		  val pexp  = LD.E.tag_and_box sumtype argsv
		in
		  pexp
		end

	    val t = ttrans env rtype
	    val res = P.SV32.to_op (P.map (LD.E.injunion' t) value)

	  in
	    (res,rtype)
	  end


	val lcons = map (ttrans env) cons

	fun do_vararg env argc resc repsv f = 
	  if !inline_varargs then
	    TD.mk_vararg_case argc resc repsv f
	  else
	    let
	      val vararg = get_global env "vararg"
	    in TD.mk_vararg_app vararg argc resc repsv f
	    end

	fun do_onearg env argc resc repsv f = 
	  if !inline_oneargs then 
	    TD.mk_onearg_case argc resc repsv f
	  else
	    let
	      val onearg = get_global env "onearg"
	    in TD.mk_onearg_app onearg argc resc repsv f
	    end

	val (op32,return_type) = 
	  (case (p,cons,exps) 
	     of (*(Nil.record [],[],[]) => (P.SV32.to_op(LD.E.unit ()),ND.unit_con)*)
	       (Nil.record [],[],[]) => (LD.E.tuple' [],ND.unit_con)
	     | (Nil.record labels,[],_::exps) =>  (*Remove tag*)
		 let 
		   val (svs,cons)  = map_unzip (sv32_trans env) exps
		   val atype = Nil.Prim_c (Nil.Record_c labels,cons)
		   val pexp = LD.E.tuple' svs
		 in (pexp,atype)
		 end	
	     | (Nil.partialRecord _,_,_) => error "partialRecord not supported by this backend"
	     | (Nil.select lbl,[],[exp]) => 
		 let
		   val (sv,con) = sv32_trans env exp
		   val i = index_from_tuple_type' env con lbl
		   val rtype = NilProjectRecordType env lbl con
		   val pexp = LD.E.select' i sv
		 in (pexp,rtype)
		 end

	     | (Nil.project k,[argcon],[argexp]) => do_sum_project k argcon argexp false
	     | (Nil.project_known k,[argcon],[argexp]) => do_sum_project k argcon argexp true

	     | (Nil.inject sumtype,[sumcon],[]) => do_inject_sum_tag env sumtype sumcon
	     | (Nil.inject_known sumtype,[sumcon],[]) => do_inject_sum_tag env sumtype sumcon

	     | (Nil.inject sumtype,[sumcon],exps as [v])  => do_inject_sum sumtype sumcon exps false

	     | (Nil.inject_known sumtype,[sumcon],exps) => do_inject_sum sumtype sumcon exps true

	     | (Nil.box_float floatsize,[],[exp]) => 
		 let
		   val sv64 = sv64_trans' env exp
		   val pexp = LD.E.box' sv64
		   val rtype = ND.boxfloat_con
		 in (pexp,rtype)
		 end
	    | (Nil.unbox_float floatsize,[],[exp]) => error "unbox not 32 bit"

	    | (Nil.make_exntag,[argcon],[]) => 
		let 
		  val t = ttrans env argcon
		  val pexp = LD.E.dyntag' t
		  val rtype = Nil.Prim_c (Nil.Exntag_c,[argcon])
		in (pexp,rtype)
		end

	    | (Nil.inj_exn name,[],[exp1,exp2]) => 
		let 
		  val dtag = sv32_trans' env exp1
		  val (dval,dcon) = sv32_trans env exp2
		  val dtype = ttrans env dcon

		  val l = Name.fresh_internal_label "exn_nm"
		  val () = add_string (l,name)
		  val dnm = Lil.Label l

		  val pexp = LD.E.inj_exn dtype dtag dval dnm
		  val pexp = P.SV32.to_op pexp
		  val rtype = Nil.Prim_c (Nil.Exn_c,[])
		in (pexp,rtype)
		end
	    | (Nil.make_vararg (openness,effect),[argc,resc],[e]) =>
	       let
		 val f = sv32_trans' env e
		 val largc = ctrans env argc
		 val lresc = ctrans env resc
		 val rep = dyn_rep env argc 
		 val tcase = P.bind rep
		   (fn repsv => do_vararg env largc lresc repsv f) 

		 val rtype = Nil.Prim_c(Nil.Vararg_c(openness,effect),[argc,resc])
	       in (tcase,rtype)
	       end
	    | (Nil.make_onearg (openness,effect),[argc,resc],[e]) =>  
	       let
		 val f = sv32_trans' env e
		 val largc = ctrans env argc
		 val lresc = ctrans env resc
		 val rep = dyn_rep env argc 
		 val tcase = P.bind rep
		   (fn repsv => do_onearg env largc lresc repsv f)

		 val rtype = 
		   Nil.AllArrow_c{openness=openness,effect=effect,
			      tFormals=[],eFormals=[argc],fFormals=0w0,body_type=resc}
	       in (tcase,rtype)
	       end
	    | (Nil.mk_record_gctag, [c],[])   => (LD.E.tuple' [],ND.unit_con)
	    | (Nil.mk_sum_known_gctag,[c],[]) => (LD.E.tuple' [],ND.unit_con)
	    | _ => error "Bad prim")
      in (op32,return_type)
      end
    and allprim32_trans (env : env) (prim,cons,exps) : (Lil.op32 P.pexp * Nil.con) = 
      let


	fun float_float2bool prim f1 f2 =
	  let
	    val sv64_1 = sv64_trans' env f1
	    val sv64_2 = sv64_trans' env f2
	    val op32 = Lil.Prim32 (prim,[],[Lil.arg64 sv64_1,Lil.arg64 sv64_2])
	    val op32 = P.ret op32
	  in (op32,nil_bool_con ())
	  end

	fun do_dispatch env (t,cs,trans,ftrans,xint,xfloat,xother,xdynamic,args) = 
	  let
	    fun dynamic c = 
	      let
		val crep = dyn_rep env c
		val staticrep = ctrans env c
		val args = trans env args
	      in P.bind crep (fn crep => xdynamic staticrep crep args)
	      end
	    fun other c = 
	      let
		val t = ttrans env c
		val args = trans env args
	      in xother t args
	      end
	  in case (t,cs)
	       of (Prim.IntArray is,_)          => (xint is (trans env args),Nil.Prim_c(Nil.Int_c is,[]))
		| (Prim.IntVector is,_)         => (xint is (trans env args),Nil.Prim_c(Nil.Int_c is,[]))
		| (Prim.FloatArray fs,_)        => (xfloat (ftrans env args),Nil.Prim_c(Nil.BoxFloat_c fs,[]))
		| (Prim.FloatVector fs,_)       => (xfloat (ftrans env args),Nil.Prim_c(Nil.BoxFloat_c fs,[]))
		| (Prim.OtherArray true, [c])   => (other c,c)
		| (Prim.OtherVector true, [c])  => (other c,c)
		| (Prim.OtherArray false, [c])  => (dynamic c,c)
		| (Prim.OtherVector false, [c]) => (dynamic c,c)
		| _ => error "table primitive did not have right type args"
	  end

	val (argtypes,rtype) = prim_get_type env prim cons

	val (op32,rtype) = 
	  (case (prim,cons,exps) 
	     (* Typecasing array primitives *)
	     of (Prim.create_table t,cs,[len,init]) =>
	       let
		 val _ = debugdo(6,fn () => (print "Doing create table.\n";
					     print "Cons are: ";
					     Ppnil.pp_list Ppnil.pp_con' cs ("{",",","}",false);
					     print "\nlen is: ";Ppnil.pp_exp len;
					     print "\ninit is: ";Ppnil.pp_exp init;print "\n"))
		 fun trans env (len,init) = 
		   let
		     val len = sv32_trans' env len
		     val init = sv32_trans' env init
		   in (len,init)
		   end
		 fun ftrans env (len,init) = 
		   let
		     val len = sv32_trans' env len
		     val init = sv64_trans' env init
		   in (len,init)
		   end
		 val (op32,eltt) = do_dispatch env (t,cs,trans,ftrans,
						    TD.Array.create_int,
						    TD.Array.create_float,
						    TD.Array.create_other,
						    TD.Array.create_dynamic,(len,init))

	       in (op32,rtype)
	       end
	      | (Prim.create_empty_table t,cs,[]) =>
	       (* Does this need dispatch?*)
	       let
		 fun trans env () = ()
		 val (op32,eltt) = do_dispatch env (t,cs,trans,trans,
						    TD.Array.create_empty_int,
						    TD.Array.create_empty_float,
						    TD.Array.create_empty_other,
						    TD.Array.create_empty_dynamic,())

	       in (op32,rtype)
	       end
	      | (Prim.length_table t,cs,[arr]) =>
	       (* Does this need dispatch? *)
	       let
		 val (op32,eltt) = do_dispatch env (t,cs,sv32_trans',sv32_trans',
						    TD.Array.length_int,
						    TD.Array.length_float,
						    TD.Array.length_other,
						    TD.Array.length_dynamic,arr)
		   
	       in (op32,ND.int_con)
	       end

	      | (Prim.sub t,cs,[arr,i]) =>
	       let
		 fun trans env (arr,i) = 
		   let
		     val arr = sv32_trans' env arr
		     val i = sv32_trans' env i
		   in (arr,i)
		   end
		 val (op32,eltt) = do_dispatch env (t,cs,trans,trans,
						    TD.Array.sub_int,
						    fn (arr,i) => error "Float sub is not a prim32",
						    TD.Array.sub_other,
						    TD.Array.sub_dynamic,(arr,i))

	       in (op32,eltt)
	       end
	      | (Prim.update t,cs,[arr,i,v]) =>
	       let
		 fun trans env (arr,i,v) = 
		   let
		     val arr = sv32_trans' env arr
		     val i = sv32_trans' env i
		     val v = sv32_trans' env v
		   in (arr,i,v)
		   end

		 fun ftrans env (arr,i,v) = 
		   let
		     val arr = sv32_trans' env arr
		     val i = sv32_trans' env i
		     val v = sv64_trans' env v
		   in (arr,i,v)
		   end

		 val (op32,eltt) = do_dispatch env (t,cs,trans,ftrans,
						    TD.Array.update_int,
						    TD.Array.update_float,
						    TD.Array.update_other,
						    TD.Array.update_dynamic,(arr,i,v))
		   
	       in (op32,ND.unit_con)
	       end
	      | (Prim.array2vector aggregate,cons,[arr]) => (P.ret (Lil.Val (sv32_trans' env arr)),rtype)
	      | (Prim.vector2array aggregate,cons,[arr]) => (P.ret (Lil.Val (sv32_trans' env arr)),rtype)
	      | (Prim.equal_table t,cs,[arr1,arr2]) => 
	       let
		 fun trans env (arr1,arr2) = 
		   let
		     val arr1 = sv32_trans' env arr1
		     val arr2 = sv32_trans' env arr2
		   in (arr1,arr2)
		   end
		 val (op32,eltt) = do_dispatch env (t,cs,trans,trans,
						    TD.Array.equal_int,
						    TD.Array.equal_float,
						    TD.Array.equal_other,
						    TD.Array.equal_dynamic,(arr1,arr2))
	       in (op32,nil_bool_con ())
	       end

              (* Floating point arguments *)
  
	      | (Prim.float2int, _,[fl]) => 
	       let
		 val sv64 = sv64_trans' env fl
		 val op32 = Lil.Prim32 (prim,[],[Lil.arg64 sv64])
	       in (P.ret op32,ND.int_con)
	       end
	      | (Prim.greater_float _,[],[f1,f2])  => float_float2bool prim f1 f2
	      | (Prim.less_float _,[],[f1,f2])     => float_float2bool prim f1 f2
	      | (Prim.lesseq_float _,[],[f1,f2])   => float_float2bool prim f1 f2
	      | (Prim.greatereq_float _,[],[f1,f2])=> float_float2bool prim f1 f2
	      | (Prim.eq_float _,[],[f1,f2])       => float_float2bool prim f1 f2
	      | (Prim.neq_float _,[],[f1,f2])      => float_float2bool prim f1 f2

	      | (Prim.eq_ref,[t],[sv1,sv2]) => 
	       let
		 val sv1 = sv32_trans' env sv1
		 val sv2 = sv32_trans' env sv2
	       in (LD.E.ptreq' sv1 sv2,nil_bool_con ())
	       end
	      (* Other ref prims *)
	      | (prim,[t],args) => 
	       let
		 val t = ttrans env t
		 val args = map (fn e => Lil.arg32 (sv32_trans' env e)) exps
	       in
		  (P.ret (Lil.Prim32 (prim,[t],args)),rtype)
	       end

	      (* All other 32 bit prims take only 32 bit args,
	       * but may still need to be wrapped up into the external bool
	       * type and/or embedded into the 32 bit type
	       *)
	      | (_,[],exps) => 
	       let
		 val cons = map (ttrans env) cons
		 val args = LO.map2 (primarg_trans env) (exps,argtypes)
		 val res32 = (P.ret (Lil.Prim32 (prim,cons,args)),rtype)
		 fun doint sz = 
		   (case sz
		      of Prim.W32 => res32
		       | _ => (P.ret (Lil.PrimEmbed (i_sizetrans sz,prim,args)),rtype))
	       in
		 case prim
		    of Prim.int2int (_,sz) => doint sz
		     | Prim.uint2uint (_,sz) => doint sz
		     | Prim.int2uint (_,sz) => doint sz
		     | Prim.uint2int (_,sz) => doint sz
		     | Prim.plus_int sz => doint sz
		     | Prim.minus_int sz => doint sz
		     | Prim.mul_int sz => doint sz
		     | Prim.div_int sz => doint sz
		     | Prim.mod_int sz => doint sz
		     | Prim.quot_int sz => doint sz
		     | Prim.rem_int sz => doint sz
		     | Prim.plus_uint sz => doint sz
		     | Prim.minus_uint sz => doint sz
		     | Prim.mul_uint sz => doint sz
		     | Prim.div_uint sz => doint sz
		     | Prim.mod_uint sz => doint sz
		     | Prim.neg_int sz => doint sz
		     | Prim.abs_int sz => doint sz
		     | Prim.not_int sz => doint sz
		     | Prim.and_int sz => doint sz
		     | Prim.or_int sz => doint sz
		     | Prim.xor_int sz => doint sz
		     | Prim.lshift_int sz => doint sz
		     | Prim.rshift_int sz => doint sz
		     | Prim.rshift_uint sz => doint sz
		     | _ => res32
	       end
	      | _ => (print "Bad prim:\n";
		      Ppnil.pp_exp (Nil.Prim_e (Nil.PrimOp prim,[],cons,exps));
		      error "wrong args to prim"))
      in (op32,rtype)
      end

    and exp32_trans (env : env) (e : Nil.exp) : (Lil.op32 P.pexp * Nil.con) = 
      let
      in 
	case e 
	  of Nil.Let_e (_,bnds,exp) => 
	    let 
	      val (oper,c) = 
		P.bind_first (bnds_trans env bnds)
		(fn (env,subst) => 
		 let
		   val (p,c) = exp32_trans env exp
		 in (p,NS.substConInCon subst c)
		 end)
	    in (oper,c)
	    end
	  
	   | Nil.Prim_e (Nil.NilPrimOp p,_,cons,exps) => nilprim32_trans env (p,cons,exps)
	   | Nil.Prim_e (Nil.PrimOp p,_,cons,exps)    => allprim32_trans env (p,cons,exps)
	    
	   | Nil.Switch_e switch => switch_trans env switch
	   | Nil.App_e (_,exp,cons,args,fargs) => 
	    let
	      val (sv1,c1) = sv32_trans env exp
	      val cargs = map (ctrans env) cons
	      val creps = dyn_rep_list dyn_rep env cons
	      val (tVars,arg_types,body_type) = 
		(case NU.strip_arrow (NilWHNF env c1)
		   of SOME {eFormals,tFormals,body_type,...} => (map #1 tFormals,eFormals,body_type)
		    | _ => error "Application of non-function")

	      val (args,types) = map_unzip (sv32_trans env) args
	      val fargs = map (sv64_trans' env) fargs
	      val oper = P.map (fn creps => LD.E.allapp'' sv1 cargs (creps@args) fargs) creps 
	      val res_type = Nil.Let_c (Nil.Sequential,ListPair.map Nil.Con_cb (tVars,cons),body_type)
	    in (oper, res_type)
	    end
	   | Nil.ExternApp_e (f,args) =>
	    let 
	      val (f_sv,c) = sv32_trans env f
	      val (argtypes,ret) =
		(case NU.strip_externarrow (NilWHNF env c)
		   of SOME res => res
		    | NONE => error "Externapp of non externarrow")

	      val args = LO.map2 (primarg_trans env) (args,argtypes)
	      val oper = Lil.ExternApp (f_sv,args)
	    in (P.ret oper,ret)
	    end
	   | Nil.Raise_e (e,c) => 
	    let 
	      val sv = sv32_trans' env e
	      val newc = ttrans env c
	      val oper = Lil.Raise (newc,sv)
	    in (P.ret oper,c)
	    end
	   | Nil.Handle_e {body,bound,handler,result_type} => 
	    let
	      val t = ttrans env result_type
	      val (body,_) = exp2exp32_trans env body
	      val env  = add_type env (bound,ND.exn_con)
	      val (handler,_) = exp2exp32_trans env handler
	      val oper = Lil.Handle {t = t, e = body, h = {b = bound,he = handler}}
	    in (P.ret oper,result_type)
	    end
	   | Nil.ForgetKnown_e (sumcon,which) =>
	    let
	      val to  = ttrans env sumcon
	      val ksum = LD.COps.sum2ksum' which to
	      val coercion = LD.Q.forgetknown ksum
	      val sumcon = NilWHNF env sumcon
	      val ksumcon = NU.convert_sum_to_special(sumcon,which)

	    in (P.ret (Lil.Val coercion),Nil.Coercion_c {vars = [],from = ksumcon,to = sumcon})
	    end
	   | Nil.Fold_e (vars,from,to) =>
	    let
	      val env = add_kind_list env (map (fn v => (v,Nil.Type_k)) vars)
	      val lto = ttrans env to
	      val coercion = Lil.Coercion (Lil.Roll, [lto])
	      val vks = map (fn a => (a,TD.Tmil())) vars
	      val sv = LD.E.nary_tabs vks coercion
	    in (P.SV32.to_op sv,Nil.Coercion_c {vars = vars,from = from,to = to})
	    end
	   | Nil.Unfold_e (vars,from,to) =>
	    let
	      val env = add_kind_list env (map (fn v => (v,Nil.Type_k)) vars)
	      val lfrom = ttrans env from
	      val coercion = Lil.Coercion (Lil.Unroll, [lfrom])
	      val vks = map (fn a => (a,TD.Tmil())) vars
	      val sv = LD.E.nary_tabs vks coercion
	    in (P.SV32.to_op sv,Nil.Coercion_c {vars = vars,from = from,to = to})
	    end
	   | Nil.Coerce_e (q,cons,e) => 
	    let
	      val (q,qtype) = sv32_trans env q
	      val e = sv32_trans' env e
	      val types = map (ctrans env) cons
	      val sv = LD.Q.coerce (LD.E.nary_tapp' types q) e
	      val ret_type = 
		(case NU.strip_coercion (NilWHNF env qtype)
		   of SOME {vars,from,to} => NU.makeLetC (map Nil.Con_cb (LO.zip vars cons)) to
		    | NONE => error "Coerce has non-coercion argument")
	    in (P.ret (Lil.Val sv),ret_type)
	    end
	   | (e as Nil.Var_e _) => 
	    let val (sv,svtype) = sv32_trans env e
	    in (P.ret (Lil.Val sv),svtype)
	    end
	   | (e as Nil.Const_e _) => 
	    let val (sv,svtype) = sv32_trans env e
	    in (P.ret (Lil.Val sv),svtype)
	    end
      end
    and tFormals_trans (env : env) (vks : (Nil.var * Nil.kind) list) : (env * (Lil.var * Lil.kind) list * (Lil.var * Lil.con) list) = 
      let
	val env = add_kind_list env vks
	val (tvars,kinds) = LO.unzip vks
	val kinds = map ktrans kinds
	val (env,repvars)  = new_rep_var_list env tvars
	val reptypes = LO.map (var_dyn_rep_type env) vks
	val repFormals = LO.zip repvars reptypes
	val tFormals = LO.zip tvars kinds
      in (env,tFormals,repFormals)
      end

    and bnds_trans (env : env) (bnds : Nil.bnd list) : ((env * NS.con_subst) P.pexp) = 
      let
	fun do_function env (c,Nil.Function{effect,recursive,tFormals,eFormals,fFormals,body}) = 
	  let
	    val arr = NilWHNF env c
	    
	    val arrow = nil_strip_arrow env arr
  
	    val {tFormals,eFormals=etypes,...} = NU.rename_arrow (arrow, tFormals)


	    val (env,tFormals,repFormals) = tFormals_trans env tFormals

	    val env = LO.foldl2 (fn ((v,_),c,env) => add_type env (v, c)) env (eFormals,etypes)
	    val env = List.foldl (fn (v,env) => add_type env (v,ND.ftype64)) env fFormals
	    val (body,rtype) = exp2exp32_trans env body

	    val rtype = ttrans env rtype
	    val etypes = LO.map (ttrans env) etypes
	    val eFormals = LO.zip (map #1 eFormals) etypes
	    val fFormals = LO.map (fn xf => (xf,LD.T.float())) fFormals



	  in Lil.Function {tFormals = tFormals,
			   eFormals = repFormals @ eFormals,
			   fFormals = fFormals,
			   rtype    = rtype,
			   body = body}
	  end

	fun do_function_nest env subst vcflist = 
	  let
	    val outerenv = foldl (fn (((v,c),_),env) => add_type env (v,c)) env vcflist
	    val vfs = map (fn ((v,c),f) => (v,do_function outerenv (c,f))) vcflist
	    val vfss = LU.break_fix vfs
	    val res = foldr (fn (vfs,pexp) => P.Bind.fixcode' (P.ret vfs) pexp) (P.ret (outerenv,subst)) vfss
	  in res
	  end

	fun do_bnd (bnd,(env,subst)) = 
	  (case bnd 
	     of Nil.Con_b (Nil.Runtime,cbnd) =>
	       let
		 val (bnds_env) = dyn_rep_cbnd env cbnd
		 val (a,c) = NU.extractCbnd cbnd
		 val subst = NS.C.addr (subst,a,c)
		 val (bnds_env_subst) = P.map (fn env => (env,subst)) bnds_env
	       in (bnds_env_subst)
	       end
	      | Nil.Con_b (Nil.Compiletime,cbnd) =>
	       let
		 val env = tbnd_trans env cbnd
		 val env = add_cbnd env cbnd
		 val (a,c) = NU.extractCbnd cbnd
		 val subst = NS.C.addr (subst,a,c)
	       in P.ret (env,subst)
	       end
	      | Nil.Exp_b (x,Nil.TraceKnown TraceInfo.Notrace_Real,e) => 
	       let
		 val oper = P.ret (exp64_trans env e)
		 val env = add_type env (x,ND.ftype64)
		 val res = P.Bind.op64' x oper (P.ret (env,subst))
	       in res
	       end
	      | Nil.Exp_b (x,_,e) => 
	       let
		 val (oper,t) = exp32_trans env e
		 val env = add_type env (x,t)
	       in P.Bind.op32' x oper (P.ret (env,subst))
	       end
	      | Nil.Fixopen_b vcflist => do_function_nest env subst vcflist
	      | _ => error "Unexpected binding form")

      in P.List.foldl_from_list do_bnd (env,NS.C.empty()) bnds
      end

    and switch_trans env switch = 
      let
	
      in
	case switch
	  of Nil.Intsw_e {arg,size,result_type,arms,default} =>
	    let
	      val size = i_sizetrans size
	      val arg = sv32_trans' env arg
	      val rtype = ttrans env result_type
	      val default = case default of SOME def => def | NONE => error "No default for int switch"
	      val default = #1 (exp2exp32_trans env default)
	      val arms = map (fn (w,e) => (w, #1(exp2exp32_trans env e))) arms
	    in
	      (P.ret (Lil.Switch (Lil.Intcase {arg = arg,arms = arms, size = size, default = default,rtype = rtype})),result_type)
	    end
	| Nil.Sumsw_e {arg, sumtype, result_type,  bound, arms, default} =>
	    let
	      val arg = sv32_trans' env arg
	      val rtype = ttrans env result_type
	      val default = Util.mapopt (#1 o (exp2exp32_trans env)) default
	      val sumtype = NilWHNF env sumtype
	      fun stype w = NilUtil.convert_sum_to_special (sumtype,w)
	      val ltype = ttrans env sumtype
	      fun mapper (w,_,e) = 
		let
		  val (env,bound') = get_rename env bound
		in
		  (w,bound', #1 (exp2exp32_trans (add_type env (bound,stype w)) e))
		end
	      val arms = map mapper arms
	    in
	      (P.ret(Lil.Switch (Lil.Sumcase {arg = arg, arms = arms, default = default, rtype = rtype})),result_type)
	    end
	| Nil.Exncase_e {arg, result_type, bound,arms,default}     =>
	    let
	      val arg = sv32_trans' env arg
	      val uarg = LD.E.unpack arg

	      val rtype = ttrans env result_type
	      val default = case default of SOME def => def | NONE => error "No default for exn switch"
	      val default = #1 (exp2exp32_trans env default)
	      fun mapper (sv,_,e) = 
		let
		  val (sv,tagtype) = sv32_trans env sv
		  val con = case NilUtil.strip_exntag (NilWHNF env tagtype)
			      of SOME con => con
			       | NONE => error "Not an exntag"
		  val carriedt = ttrans env con
		  val ltype = LD.T.tupleptr' [LD.T.dyntag carriedt,carriedt,LD.T.stringt()]
		  val (env,bound') = get_rename env bound
		  val env = add_type env (bound,con)
		  val e = #1 (exp2exp32_trans env e)
		  val x = Name.derived_var bound
		    
		  val e = LD.E.mk_let [Lil.Exp32_b (bound',LD.E.select'' 0w1 (Lil.Var_32 x))] e
		in (sv,(x,ltype),e)
		end
	      val arms = map mapper arms
	      val res = 
		P.map (fn arg => Lil.Switch (Lil.Dyncase {arg = arg, arms = arms, default = default,rtype = rtype})) uarg
	    in (res,result_type)
	    end
	| _ => error "Typecase and ifthenelse not handled"
      end


    local
      fun need_unit l = 
	let val l = if Name.is_flat l then hd (Name.split_label l) else l
	in if Name.is_unit l then SOME l
	   else NONE (* extern *)
	end
      
      fun get_parms imports = 
	let
	  fun mapper (import : Nil.import_entry) : Name.label option =
	    (case import
	       of Nil.ImportValue (l,_,_,_) => need_unit l
		| Nil.ImportType (l,_,_) => need_unit l
		| Nil.ImportBnd _ => NONE)
	  val unitlist = List.mapPartial mapper imports
	  val parms = Name.LabelSet.addList (Name.LabelSet.empty, unitlist)
	in parms
	end

      fun get_timport imp = case imp of Nil.ImportType lvk         => SOME lvk | _ => NONE
      fun get_vimport imp = case imp 
			      of Nil.ImportValue (l,v,tr,c) => if Name.is_unit l then SOME ((l,v),c) else NONE
			       | _ => NONE
      fun get_extern imp = case imp 
			     of Nil.ImportValue (l,v,tr,c) => if Name.is_unit l then NONE else SOME ((l,v),c)
			      | _ => NONE
      fun get_ibnd imp    = case imp of Nil.ImportBnd arg          => SOME arg       | _ => NONE
      fun get_cbnd bnd    = case bnd of Nil.Con_b (Nil.Runtime,cb) => SOME cb        | _ => NONE
      fun get_texport e   = case e   of Nil.ExportType (l,v)       => SOME (l,Nil.Var_c v) | _ => NONE
      fun get_vexport e   = case e   of Nil.ExportValue (l,v)      => SOME (l,Nil.Var_e v) | _ => NONE
      fun l_cmp l1 l2 = String.compare (Name.label2name l1,Name.label2name l2)
      fun cmp_imp (((l1,_),_),((l2,_),_)) = l_cmp l1 l2
      val isort = fn l => LO.insertion_sort cmp_imp l
      fun cmp_timp ((l1,_,_),(l2,_,_)) = l_cmp l1 l2
      val tisort = fn l => LO.insertion_sort cmp_timp l
      fun cmp_exp ((l1,_),(l2,_)) = l_cmp l1 l2
      val esort = fn l => LO.insertion_sort cmp_exp l
	
      fun dropl l = map (fn ((l,v),ck) => (v,ck)) l
      fun drops (p,cb) = (case p of Nil.Runtime => SOME cb | _ => NONE)

      fun get_timports imports = tisort (List.mapPartial get_timport imports)
      fun get_vimports imports = isort (List.mapPartial get_vimport imports)
      fun get_externs imports = List.mapPartial get_extern imports
      fun get_itbnds imports   = List.mapPartial get_ibnd imports
      fun get_icbnds itbnds   = List.mapPartial drops itbnds 
      fun get_texports exports = esort (List.mapPartial get_texport exports)
      fun get_vexports exports = esort (List.mapPartial get_vexport exports)
    in
      fun get_cbnds bnds    = List.mapPartial get_cbnd bnds
      fun separate_imports imports = 
	let
	  val parms = get_parms imports
	  val timports = get_timports imports
	  val vimports = get_vimports imports
	  val externs  = get_externs imports
	  val itbnds   = get_itbnds imports
	  val icbnds   = get_icbnds itbnds 
	in (parms,timports,vimports,externs,itbnds,icbnds)
	end
      fun separate_exports exports = 
	let
	  val texports = get_texports exports
	  val vexports = get_vexports exports
	in (texports,vexports)
	end

    end  

      
    fun mk_type_fun (unitname,ibnds,cbnds,texports,texports_int) = 
      let
(*	val f    = Name.fresh_named_var "mk_export_c"*)
	val (l,a,k,res)  = 
	  (case (texports,texports_int)
	     of ([(l,c)],[(_,a,k)]) => (l,a,k,c)
	      | _ => error "Multiple exports not handled")
	val cbnds = ibnds @ cbnds
	val body = NU.makeLetC cbnds res
      in (l,a,k,body)
      end
    
    (* We don't have enough type information at this point
     * to create the right syntax
     *)
    fun mk_term_fun (unitname,vimports,externs,ibnds,bnds,texports,vexports,vexports_int,vexports_tbnds) = 
      let
	val f     = Name.fresh_named_var "mk_export_v"
	val (elabels,eexps) = LO.unzip vexports
	val ibnds = map Nil.Con_b ibnds
	val gctag = Nil.Var_e (Name.fresh_named_var "fake gctag")
	val (l_r,vres)  = 
	  (case (eexps,elabels)
	     of ([e],[l]) => (l,e)
	      | _ => (Name.internal_label (unitname ^ "." ^ "code_r"),
		      Nil.Prim_e(Nil.NilPrimOp (Nil.record elabels),[],[],(gctag::eexps))))
	val (l_c,cres)  = 
	  (case texports
	     of [(l,c)] => (l,c)
	      | _ => (Name.internal_label (unitname ^ "." ^ "code_c"),Nil.Crecord_c texports))
	val l_r_t = 
	  (case vexports_int
	     of [(_,t)] => NU.makeLetC vexports_tbnds t
	      | _ => error "Multiple exports not handled")
      in (f, ibnds, vimports, externs,bnds,l_c,cres,l_r,l_r_t,vres)
      end
  
    fun modfuns (unitname,Nil.MODULE {bnds : Nil.bnd list,
				      imports : Nil.import_entry list,
				      exports : Nil.export_entry list,
				      exports_int : Nil.import_entry list option}) = 
      let
	val (parms,timports,vimports,externs,itbnds,icbnds) = separate_imports imports

	val cbnds    = get_cbnds bnds

	val (texports,vexports) = separate_exports exports

	val (_,texports_int,vexports_int,_,vexports_tbnds,_) = 
	  (case exports_int
	     of SOME ei => separate_imports ei
	      | NONE => error "No export interface")

	val tlam = mk_type_fun (unitname,icbnds,cbnds,texports,texports_int)
	val vlam = mk_term_fun (unitname,vimports,externs,itbnds,bnds,texports,vexports,vexports_int,map #2 vexports_tbnds)
      in (parms,timports,tlam,vlam)
      end


    fun add_globals env = 
      let
	val env = add_global env ("vararg",TD.mk_vararg_fn)
	val env = add_global env ("onearg", TD.mk_onearg_fn)
	val env = add_global env ("project_dyn",TD.mk_project_dynamic_fn)
	val env = add_global env ("inject_dyn",TD.mk_inject_dynamic_fn)
      in env
      end


    fun niltolil unitname module =  
      let

	val () = reset_globals ()
	val () = reset_data()
	val env = new_env ()
	val (parms,timports,(mainLabel_c,mainVar_c,mainK,tlam),vlam) = modfuns (unitname,module)

	val _ = chat 2 "  Translating type imports\n"

	(* THese labels are wrong in the flattened case (not currently used) *)
	val (env,timports,rargs) = 
	  let
	    val (ls,vks) = map_unzip (fn (l,v,k) => (l,(v,k))) timports
	    val (env,targs,rargs) = tFormals_trans env vks
	    val timports = ListPair.map (fn (l,(v,k)) => (l,v,k)) (ls,targs) 
	  in (env,timports,rargs)
	  end

	val _ = chat 2 "  Translating constructor fun\n"

	val _ = debugdo (2,fn () => 
			 (print "NIL Constructor fun is:\n";
			  Ppnil.pp_con tlam;
			  print "\n"))

	val tfun = ctrans env tlam

	val _ = debugdo (2,fn () => 
			 (print "Constructor fun is:\n";
			  PpLil.pp_con tfun;
			  print "\n"))

	val _ = chat 2 "  Translating mainLabel_c kind\n"

	val LilMainK = ktrans mainK

	val _ = chat 1 "  Translating expressions\n"
	  
	val (f, ibnds, vimports, externs, bnds, _, cres, mainLabel_r, mainLabel_t,vres) = vlam
	  
	val _ = debugdo (2,fn () => 
			 (print "Term nil function is:\n";
			  print "IBNDS:\n";
			  Ppnil.pp_bnds ibnds;
			  print "BNDS:\n";
			  Ppnil.pp_bnds bnds;
			  print "CRES:\n";
			  Ppnil.pp_con cres;
			  print "VRES:\n";
			  Ppnil.pp_exp vres;
			  print "\n"))
	  
	val _ = chat 2 "  Translating ibnds\n"

	val ibnds = bnds_trans env ibnds

	(* At the NIL level, the vargs may refer to type bnds in ibnds, so we must translate
,	 * the ibnds first.  Once translated however, there will be no references, so
	 * we can bind them at the same level as the rargs.  Therefore, we hoist them
	 * out of the monad along with the return type.
	 * *)
	val (body,(vargs,rtype,LilMainLabel_t,externs)) = 
	  P.bind_first ibnds
	  (fn (env,_) =>   (* As long as the NIL type doesn't escape, we don't need the subst*)
	   let

	     val _ = chat 2 "  Translating vimports\n"
	     val vimports = LO.map_first #2 vimports
	     val vargs = LO.map_second (ttrans env) vimports
	     val env = add_type_list env vimports

	     val _ = chat 2 "  Translating externs\n"
	     val l_externs = map (fn ((l,v),c) => (l,ttrans env c)) externs
	     val l_bnds = map (fn ((l,v),c) => Lil.Exp32_b(v,Lil.Val (Lil.Label l))) externs
	     val l_bnds = P.Lili.from_bnds (l_bnds,())
	     val env = foldl (fn (((l,v),c),env) => add_type env (v,c)) env externs

	     val _ = chat 2 "  Building global definitions\n"
	     val env = add_globals env

	     val _ = chat 2 "  Translating bnds\n"
	     val bnds = bnds_trans env bnds
	     val (body,(rtype,mainLabel_t)) = P.bind_first bnds
	       (fn (env,_) =>
		let
		  val _ = chat 2 "  Translating export interface type\n"
		  val exportEnv = add_kind env (mainVar_c,mainK)
		  val mainLabel_t = ttrans exportEnv mainLabel_t
		  val mainRep_t = dyn_rep_type exportEnv (Nil.Var_c mainVar_c,mainK)
		  val mainLabel_t = LD.T.tupleptr' [mainRep_t,mainLabel_t]
		  val mainLabel_t = LD.T.code' (map #2 (rargs@vargs)) [] mainLabel_t

		  val crtype = dyn_rep_type env (cres,erased_kind_of env cres)
		  val cres = dyn_rep' env cres
		  val (vres,vrtype) = exp32_trans env vres
		  val vrtype = ttrans env vrtype
		  val rtype = LD.T.tupleptr' [crtype,vrtype]
		  val cresv = Name.fresh_named_var "TypeExportRepRecord"
		  val vresv = Name.fresh_named_var "TermExportRecord"
		  val resv = Name.fresh_named_var "ModuleExports"
		  val res = LD.E.tuple' [Lil.Var_32 cresv,Lil.Var_32 vresv]
		in (P.Bind.op32' cresv cres (P.Bind.op32' vresv vres res),(rtype,mainLabel_t))
		end)

	     val body = wrap_with_globals body
	     val body = P.bind l_bnds (fn () => body)

	   in (body,(vargs,rtype,mainLabel_t,l_externs))
	   end)

	val body = P.Lili.op_to_exp body

	val lam = 
	  Lil.Function {tFormals = [],
			eFormals = rargs@vargs,
			fFormals = [],
			rtype    = rtype,
			body = body}


      	val exp = Lil.mk_exp (Lil.Let_e ([Lil.Fixcode_b [(f,lam)]],
					 Lil.mk_exp (Lil.Val32_e(Lil.Var_32 f))))

	val _ = debugdo (2,fn () => 
			 (print "Term function is:\n";
			  PpLil.pp_exp exp;
			  print "\n"))

	val _ = chat 1 "  Finished translation to LIL\n"

	val data = get_data ()
	val () = reset_globals ()
	val () = reset_data ()
      in Lil.MODULE {unitname = unitname,
		     parms = parms,
		     entry_c = (mainLabel_c,mainVar_c,LilMainK),
		     entry_r = (mainLabel_r,LilMainLabel_t),
		     timports = timports,
		     vimports = externs,
		     data   = (Lil.Dcode (mainLabel_r,lam))::data,
		     confun = tfun}
      end




    fun mk_confun_kind (texports) = 
      let
	val res = (case texports
		     of [one] => one
		      | _ => error "Multiple texports not currently supported")
      in res
      end
    
    (* We don't have enough type information at this point
     * to create the right syntax
     *)
    fun mk_term_fun_type (vimports : ((Nil.label * Nil.var) * Nil.con) list,cbnds : (Nil.phase * Nil.conbnd) list,vexports : ((Nil.label * Nil.var) * Nil.con) list ) = 
      let
	val (l_r,vres)  = 
	  (case vexports
	     of [((l,v),c)] => (l,c)
	      | many => error "Multiple term exports not currently supported")
      in (map #2 cbnds, vimports, l_r,vres)
      end
  
    fun intfuns (Nil.INTERFACE {imports : Nil.import_entry list,
				exports : Nil.import_entry list}) = 
      let
	val (parms,timports,vimports,_,itbnds,_) = separate_imports imports
	val (_,texports,vexports,_,itbnds',_) = separate_imports exports

	val cbnds = itbnds @ itbnds'

	val tk = mk_confun_kind (texports)

	val vc = mk_term_fun_type (vimports,cbnds,vexports)
      in (parms,timports,tk,vc)
      end


    fun niltolil_int unitname (interface : Nil.interface) : Lil.interface =  
      let
	val (parms,timports,tlam,vlam) = intfuns interface

	val env = new_env ()

	val _ = chat 2 "  Translating type imports\n"

	(* These labels are wrong in the nary-case *)
	fun do_import ((l,a,k),env) = 
	  let
	    val env = add_kind env (a,k)
	    val k = ktrans k
	  in ((l,a,k),env)
	  end

	val (env,timports,rargs) = 
	  let
	    val (ls,vks) = map_unzip (fn (l,v,k) => (l,(v,k))) timports
	    val (env,targs,rargs) = tFormals_trans env vks
	    val rargs = map #2 rargs
	    val timports = ListPair.map (fn (l,(v,k)) => (l,v,k)) (ls,targs) 
	  in (env,timports,rargs)
	  end

	val _ = chat 2 "  Translating constructor fun kind\n"

	val (entry_c,entry_c_rep_type,env) = 
	  let
	    val (l,v,lk) = tlam
	    val env = add_kind env (v,lk)
	    val k = ktrans lk
	    val Rvk = var_dyn_rep_type env (v,lk)
	  in ((l,v,k),Rvk,env)
	  end
	val _ = chat 1 "  Translating expression type\n"

	val entry_r = 
	  let
	    val (cbnds, vimports, l_r, vres) = vlam
	  
	    val _ = chat 2 "  Translating cbnds\n"

	    val env = tbnds_trans env cbnds

	    val _ = chat 2 "  Translating vimports\n"
	    val vimports = LO.map_first #2 vimports
	    val vargs = LO.map (fn (v,c) => ttrans env c) vimports
	    val env = add_type_list env vimports

	    val c = ttrans env vres
	    val rtype = LD.T.tupleptr' [entry_c_rep_type,c]
	    val ftype = LD.T.code' (rargs@vargs) [] rtype
	  in (l_r,ftype)
	  end
	  
	val _ = chat 1 "  Finished translation to LIL\n"

      in Lil.INTERFACE {unitname = unitname,
			timports = timports,
			entry_c = entry_c,
			entry_r = entry_r}
      end

  end
