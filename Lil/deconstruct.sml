

signature STRIP =
  sig
    
    val var : Lil.con -> Lil.var
    val var' : Lil.con -> Lil.var option
    val APP : Lil.con -> Lil.con * Lil.kind
    val APP' : Lil.con -> (Lil.con * Lil.kind) option
    val app  : Lil.con -> Lil.con * Lil.con
    val app' : Lil.con -> (Lil.con * Lil.con) option
    val fold  : Lil.con -> Lil.kind * Lil.con
    val fold' : Lil.con -> (Lil.kind * Lil.con) option
    val kapp  : Lil.con -> Lil.con * Lil.kind
    val kapp' : Lil.con -> (Lil.con * Lil.kind) option
    val ptr  : Lil.con -> Lil.con
    val ptr' : Lil.con -> Lil.con option
    val pi1  : Lil.con -> Lil.con
    val pi1' : Lil.con -> Lil.con option
    val pi2  : Lil.con -> Lil.con
    val pi2' : Lil.con -> Lil.con option
    val nat : Lil.con -> Lil.w32
    val nat' : Lil.con -> Lil.w32 option
    val lam  : Lil.con -> (Lil.var * Lil.kind) * Lil.con 
    val lam' : Lil.con -> ((Lil.var * Lil.kind) * Lil.con) option
    val LAM  : Lil.con -> Lil.var * Lil.con 
    val LAM' : Lil.con -> (Lil.var * Lil.con) option
    val pair : Lil.con -> Lil.con * Lil.con
    val pair' : Lil.con -> (Lil.con * Lil.con) option
    val star  : Lil.con -> unit
    val star' : Lil.con -> unit option
    val inj  : Lil.con -> (Lil.w32 * Lil.kind * Lil.con)
    val inj' : Lil.con -> (Lil.w32 * Lil.kind * Lil.con) option
    val sumcase  : Lil.con -> (Lil.con * (Lil.w32 * (Lil.var * Lil.con)) list * Lil.con option) 
    val sumcase' : Lil.con -> (Lil.con * (Lil.w32 * (Lil.var * Lil.con)) list * Lil.con option) option
    val pr   : Lil.con -> (Lil.var * (Lil.var * Lil.kind) * Lil.kind * Lil.var * Lil.con) 
    val pr'  : Lil.con -> (Lil.var * (Lil.var * Lil.kind) * Lil.kind * Lil.var * Lil.con) option 
  end    (* STRIP *)

local
  (*  NO functors in structures for TILT!! *)
    open Lil
    val error = fn s => Util.error "deconstruct.sml" s
    val debug = Stats.ff "DeconstructDebug"
    val w2i = TilWord32.toInt
    val i2w = TilWord32.fromInt
    structure LS = LilSubst      
    structure LU = LilUtil
    structure LO = Listops
    structure R = Reduce

    fun obind opt f = 
      (case opt 
	 of SOME a => f a
	  | NONE => NONE)

    fun errbind s opt f = 
      (case opt 
	 of SOME a => f a
	  | NONE => error s)

    fun opt_out s f = fn arg => (case f arg 
				   of SOME res => res
				    | NONE => error s)

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

	functor CF (structure Strip : STRIP) = 
	  struct
	    open Strip

	    fun prim' (primapp : con) = 
	      let
		fun loop (c,args) = 
		  (case app' c
		     of SOME (c,arg) => loop (c, arg::args)
		      | NONE => 
		       (case cout c 
			  of Prim_c prim => SOME (prim,args)
			   | _ => NONE))
	      in loop (primapp,[])
	      end
	    val prim = opt_out "Expected Prim_c" prim'
	      
	    fun prim0' c = 
	      (case prim' c
		 of SOME (p,[]) => SOME p
		  | _ => NONE)
	    val prim0 = opt_out "Expected Prim_c[]" prim0'
	      
	    fun prim1' c = 
	      (case prim' c
		 of SOME (p,[c]) => SOME (p,c)
		  | _ => NONE)
	    val prim1 = opt_out "Expected Prim_c[c]" prim1'
	      
	    fun prim2' c = 
	      (case prim' c
		 of SOME (p,[c1,c2]) => SOME (p,c1,c2)
		  | _ => NONE)
	    val prim2 = opt_out "Expected Prim_c[c1,c2]" prim2'
	      
	    fun prim3' c = 
	      (case prim' c
		 of SOME (p,[c1,c2,c3]) => SOME (p,c1,c2,c3)
		  | _ => NONE)
	    val prim3 = opt_out "Expected Prim_c[c1,c2,c3]" prim3'
	      
	      
	    fun polyprim' (primapp : con) = 
	      let
		fun loop (c,cargs) = 
		  (case app' c
		     of SOME (c,arg) => loop (c, arg::cargs)
		      | NONE => 
		       let
			 fun loop (c,kargs) = 
			   (case APP' c
			      of SOME (c,karg) => loop(c,karg::kargs)
			       | NONE => (case cout c
					    of Prim_c prim => SOME (prim,kargs,cargs)
					     | _ => NONE))
		       in loop(c,[])
		       end)
	      in loop (primapp,[])
	      end

	    val polyprim = opt_out "Expected polymorphic Prim_c" polyprim'
	      
	    fun polyprim11' c = 
	      (case polyprim' c
		 of SOME (p,[k],[c]) => SOME (p,k,c)
		  | _ => NONE)
	    val polyprim11 = opt_out "Expected Prim_c[k][c]" polyprim11'

	    fun inji' (i,c : con) = 
	      obind (inj' c)
	      (fn (i',k,c) => if i = i' then SOME (k,c) else NONE)

	    val inji = opt_out "Expected Inj_c i" inji'
		 
	    fun inl' (c : con) = inji' (0w0,c)
	    val inl = opt_out "Expected Inj_c 0w0" inl'
	    fun inr' (c : con) = inji' (0w1,c)
	    val inr = opt_out "Expected Inj_c 0w1" inl'

	    fun nill' (c : con) = 
	      obind (fold' c)
	      (fn (k,c) => obind (inl' c)
	       (fn (_,c) => 
		(obind (star' c)
		 (fn () => SOME k))))

	    val nill = opt_out "Not an empty list" nill'

	    fun cons' (c : con) =
	      obind (fold' c)
	      (fn (_,c) => obind (inr' c)
	       (fn (k,c) => SOME c))
	      
	    val cons = opt_out "Not a cons cell" cons'
	    fun cons_ml' c = Option.mapPartial pair' (cons' c)
	    val cons_ml  = opt_out "Not a pair/cons cell" cons_ml'

	    fun list' (c : con) =
	      let
		fun loop (c,elts) = 
                  (case cons' c
		     of SOME c => obind (pair' c) (fn (hd,tl) => loop (tl,hd::elts))
		      | NONE => 
		  (case nill' c
		     of SOME k => SOME (k,rev elts)
		      | NONE => NONE))
	      in loop(c,[]) (*Start with junk kind*)
	      end
	    val list = opt_out "Not a list" list'

	    fun sum' (sumtype : con) = 
	      (case prim2' sumtype
		 of SOME (Sum_c,tagc,sum_args) => SOME (tagc,sum_args)
		  | _ => NONE)

	    val sum = opt_out "Not a sum type" sum'

	    fun sum_ml' (sumtype : con) = obind (sum' sumtype)
	      (fn (n,arg_list) => obind (nat' n)
	       (fn n => obind (list' arg_list)
		(fn (_,arg_list) => SOME (n,arg_list))))

	    val sum_ml = opt_out "Not a deconstructible sum type" sum_ml'

	    fun ksum' (sumtype : con) = 
	      (case prim3' sumtype
		 of SOME (KSum_c,which,tagc,sum_args) => SOME (which,tagc,sum_args)
		  | _ => NONE)
	    val ksum = opt_out "Not a known sum" ksum'

	    fun ksum_ml' sumtype = obind (ksum' sumtype)
	      (fn (which,tagc,sum_args) => obind (nat' tagc)
	       (fn k => obind (nat' which)
		(fn j => obind (list' sum_args)
		 (fn (_,l) => SOME (j,k,l)))))

	    val ksum_ml = opt_out "Not a deconstructible known sum" ksum_ml'


	    fun rek' c = 
	      (case polyprim' c
		 of SOME (Rec_c,[k],[c,cp]) => SOME (k,c,cp)
		  | _ => NONE)
		 
	    val rek = opt_out "Not a recursive type" rek'

	    fun exists' (c : Lil.con) : (Lil.kind * Lil.con) option = 
	      (case polyprim11' c
		 of SOME (Exists_c,k,c) => SOME (k,c)
		  | _ => NONE)
	    val exists = opt_out "Not a exists" exists'

	    fun exists_ml' (c : Lil.con) : ((Lil.var * Lil.kind) * Lil.con) option = 
	      obind (exists' c) (fn (_,l) => lam' l)

	    val exists_ml = opt_out "Not a exists" exists_ml'

	    fun forall' (c : Lil.con) : (Lil.kind * Lil.con) option = 
	      (case polyprim11' c
		 of SOME (Forall_c,k,c) => SOME (k,c)
		  | _ => NONE)
	    val forall = opt_out "Not a forall" forall'

	    fun forall_ml' (c : Lil.con) : ((Lil.var * Lil.kind) * Lil.con) option = 
	      obind (forall' c) (fn (_,l) => lam' l)

	    val forall_ml = opt_out "Not a forall" forall_ml'

	    fun nary_forall (c : Lil.con) : ((Lil.var * Lil.kind) list * Lil.con) = 
	      let
		fun loop (acc,c) = 
		  (case forall_ml' c
		     of SOME (vk,c) => loop (vk::acc,c)
		      | NONE => (rev acc,c))
	      in loop ([],c)
	      end

	    fun arrow' c = 
	      (case prim3' c
		 of SOME (Arrow_c, args,fargs,rt) =>SOME (args,fargs,rt)
		  | _ => NONE)
	    val arrow = opt_out "Not an arrow" arrow'
	      
	    fun arrow_ml' c = obind (arrow' c)
	      (fn (args,fargs,rt) => obind (list' args)
	       (fn (_,args) => obind (list' fargs)
		(fn (_,fargs) => SOME(args,fargs,rt))))

	    val arrow_ml = opt_out "Not a deconstructible arrow" arrow_ml'

	    fun allarrow' c = 
	      let 
		(* Every type is an nary-forall *)
		val (vks,c) = nary_forall c
	      in case arrow' c
		   of SOME (args,fargs,rt) => SOME (vks,args,fargs,rt)
		    | _ => NONE
	      end

	    val allarrow = opt_out "Not an allarrow" allarrow'

	    fun allarrow_ml' c = obind (allarrow' c)
	      (fn (vks,args,fargs,rt) => obind (list' args)
	       (fn (_,args) => obind (list' fargs)
		(fn (_,fargs) => SOME (vks,args,fargs,rt))))

	    val allarrow_ml = opt_out "Not a deconstructible allarrow" allarrow_ml'

	    fun code' c = 
	      (case prim3' c
		 of SOME (Code_c, args,fargs,rt) =>SOME (args,fargs,rt)
		  | _ => NONE)
	    val code = opt_out "Not a code" code'
	      
	    fun code_ml' c = obind (code' c)
	      (fn (args,fargs,rt) => obind (list' args)
	       (fn (_,args) => obind (list' fargs)
		(fn (_,fargs) => SOME(args,fargs,rt))))

	    val code_ml = opt_out "Not a deconstructible code" code_ml'

	    fun allcode' c = 
	      let 
		(* Every type is an nary-forall *)
		val (vks,c) = nary_forall c
	      in case code' c
		   of SOME (args,fargs,rt) => SOME (vks,args,fargs,rt)
		    | _ => NONE
	      end

	    val allcode = opt_out "Not an allcode" allcode'

	    fun allcode_ml' c = obind (allcode' c)
	      (fn (vks,args,fargs,rt) => obind (list' args)
	       (fn (_,args) => obind (list' fargs)
		(fn (_,fargs) => SOME (vks,args,fargs,rt))))

	    val allcode_ml = opt_out "Not a deconstructible allcode" allcode_ml'

	    fun ntuple' c = 
	      let
		fun loop (c,acc) =
		  (case cout c
		     of Star_c  => SOME (rev acc)
		      | Pair_c (c1,c2) => loop(c2,c1::acc)
		      | _ => NONE)
	      in loop (c,[])
	      end
	    val ntuple = opt_out "Not a deconstructible con tuple" ntuple'

	    (* Every nproj has the form of a pi1 followed by n pi2s.
	     *)
	    fun nproj' c =
	      (case pi1' c
		 of SOME c => 
		   let
		     fun loop (i,c) = 
		       (case pi2' c
			  of SOME c => loop (i+1,c)
			   | NONE => (i,c))
		   in SOME (loop(0,c))
		   end
		  | NONE => NONE)
	    val nproj = opt_out "Not an nary projection" nproj'

	    fun tuple' c = 
	      (case prim1' c
		 of SOME (Tuple_c,args) => SOME args
		  | _ => NONE)

	    val tuple = opt_out "Not a tuple type" tuple'

	    fun tuple_ml' c = obind (tuple' c) (fn l => obind (list' l) (fn (_,l) => SOME l))
	      
	    val tuple_ml = opt_out "Not a deconstructible tuple type" tuple_ml'

	    fun float' c = obind (prim0' c) (fn Float_c => SOME () | _ => NONE)
	    val float = opt_out "Not float type" float'

	    fun void' c = obind (prim0' c) (fn Void_c => SOME () | _ => NONE)
	    val void = opt_out "Not void type" void'

	    fun tl (c : con) = 
	      (case cons_ml' c
		 of SOME (hd,tl) => tl
		  | NONE => error "Tail of empty list")

	    fun hd (c : con) = 
	      (case cons_ml' c
		 of SOME (hd,tl) => hd
		  | NONE => error "Head of empty list")

	    fun nth' i (c: con) = 
	      (case cons_ml' c
		 of SOME (hd,tl) => if i = 0 then SOME hd else nth' (i-1) tl
		  | NONE => NONE)
		
	    fun nth i (c : con) = 
	      if i = 0 then
		hd c
	      else nth (i-1) (tl c)

	    fun unfold' (c : con) : kind option = 
	      obind (pr' c)
	      (fn (j,(a,k_in),k,r,c) => 
	       obind (var' c)
	       (fn a' => if Name.eq_var (a,a') then SOME (mk_kind (Mu_k (j,k_in)))
			 else NONE))

	    val unfold = opt_out "Not an unfold" unfold'

	    fun boxed' c = 
	      obind (prim1' c)
	      (fn (Boxed_c s, c) => SOME (s,c) | _ => NONE)
	    val boxed = opt_out "Not a boxed type" boxed'

	    fun externarrow' c = 
	      obind (prim3' c)
	      (fn (ExternArrow_c s, args,fargs,res) => SOME (s,args,fargs,res) | _ => NONE)
	    val externarrow = opt_out "Not an externarrow type" externarrow'

	    fun externarrow_ml' c = obind (externarrow' c) 
	      (fn (s,l,fl,r) => obind (list' l) 
	       (fn (k,l) => obind (list' fl) 
		(fn (k,fl) => (SOME (s,l,fl,r)))))
	    val externarrow_ml = opt_out "Not a deconstructible externarrow" externarrow_ml'

	    fun coercion' c = 
	      obind (prim2' c)
	      (fn (Coercion_c,c1,c2) => SOME (c1,c2) | _ => NONE)

	    val coercion = opt_out "Not a coercion type" coercion'
	      
	    fun exn_packet' (c : con) = 
	      obind (ptr' c)
	      (fn tup => obind (tuple_ml' tup)
	       (fn [d,c] => SOME(d,c)
	          | _ =>  NONE))

	    val exn_packet = opt_out "Not an exn packet" exn_packet'
	  end (* CF *)
in

structure Deconstruct :> DECONSTRUCT =
  struct
      
    structure Dec = 
      struct
	structure K = 
	  struct
	    fun T32' (k : Lil.kind) = 
	      (case kout k
		 of T s => if s = B4 then SOME () else NONE
		  | _ => NONE)
	    val T32 = opt_out "Not T32" T32'
	      
	    fun TM' (k : Lil.kind) = 
	      (case kout k
		 of Tmem => SOME ()
		  | _ => NONE)
	    val TM = opt_out "Not Tmem" TM'

	    fun var' (k : Lil.kind) = 
	      (case kout k
		 of Var_k j => SOME j
		  | _ => NONE)
	    val var = opt_out "Not a var kind" var'

	    fun arrow' (k : Lil.kind) = 
	      (case kout k
		 of Arrow_k args => SOME args
		  | _ => NONE)
	    val arrow = opt_out "Not an arrow kind" arrow'
	      
	    fun forall' (k :Lil.kind) =
	      (case kout k
		 of All_k args => SOME args
		  | _ => NONE)
	    val forall = opt_out "Not a forall kind" forall'
	      
	    fun sum' (sumk : kind) =
	      (case kout sumk 
		 of Sum_k ks => SOME ks
		  | _ => NONE)
	    val sum = opt_out "Not a sum kind" sum'

	    fun binsum' (sumk : kind) =
	      obind (sum' sumk) (fn ks => (case ks of [k1,k2] => SOME (k1,k2) | _ => NONE))
	    val binsum = opt_out "Not a binary sum kind" binsum'
	      
	    fun pair' (pairk : kind) =
	      (case kout pairk 
		 of Prod_k ks => SOME ks
		  | _ => NONE)
	    val pair = opt_out "Not a pair kind" pair'
	      
	    fun mu' (k :Lil.kind) =
	      (case kout k
		 of Mu_k args => SOME args
		  | _ => NONE)
	    val mu = opt_out "Not a mu kind" forall'

	    fun unit' (k : kind) = 
	      (case kout k
		 of Unit_k => SOME ()
		  | _ => NONE)
	    val unit = opt_out "Not unit kind" unit'

	    fun ntuple' (k : kind) = 
	      let
		fun loop (k,acc) = 
		  (case kout k
		     of Unit_k => SOME (rev acc)
		      | Prod_k(k1,k2) => loop (k2,k1::acc)
		      | _ => NONE)
	      in loop (k,[])
	      end
	    val ntuple = opt_out "Not an ntuple kind" ntuple'

	    fun list' (k : kind) = 
	      obind (mu' k)
	      (fn (j,k) => obind (binsum' k)
	       (fn (k1,k2) => obind (unit' k1)
		(fn () => obind (pair' k2) 
		 (fn (k1,k2) => obind (var' k2) 
		  (fn j' => if Name.eq_var (j,j') then SOME k1
			    else NONE)))))
	    val list = opt_out "Not a list kind" list' 
	  end (* K *)

	(* The strip functions deconstruct a constructor that is already in
	 * the appropriate shape.  They do no head normalization.
	 * The strip' functions return an option.  The strip versions
	 * raise an error if the corresponding strip' function returns NONE.
	 *
	 *)
       
	structure Strip' = 
	  struct
	    
	    fun kapp' (app : con) =
	      (case cout app
		 of APP_c args => SOME args
		  | _ => NONE)
	    val kapp = opt_out "Expected APP_c" kapp'

	    fun app' (app : con) =
	      (case cout app
		 of App_c args => SOME args
		  | _ => NONE)
	    val app = opt_out "Expected App_c" app'

	    fun APP' (APP : con) =
	      (case cout APP
		 of APP_c args => SOME args
		  | _ => NONE)
	    val APP = opt_out "Expected APP_c" APP'

	    fun nat' (n : con) = 
	      (case cout n
		 of Nat_c n => SOME n
		  | _ =>  NONE)
	    val nat = opt_out "Expected Nat_c" nat'
	      
	    fun pair' (pair : con) = 
	      (case cout pair
		 of Pair_c args => SOME args
		  | _ => NONE)
	    val pair = opt_out "Expected Pair_c" pair'

	    fun fold' (fold : con) = 
	      (case cout fold
		 of Fold_c args => SOME args
		  | _ => NONE)
	    val fold = opt_out "Expected Fold_c" fold'

	    fun lam' (c : Lil.con) = 
	      (case cout c
		 of Lam_c args => SOME args
		  | _ => NONE)
	    val lam = opt_out "Expected Lam_c" lam'

	    fun LAM' (c : Lil.con) = 
	      (case cout c
		 of LAM_c args => SOME args
		  | _ => NONE)
	    val LAM = opt_out "Expected LAM_c" LAM'

	    fun pr' (c: con) : (var * (var * kind) * kind * var * con) option = 
	      (case cout c
		 of (Pr_c args) => SOME args
		  | _ => NONE)
	    val pr = opt_out "Expected Pr_c" pr'

	    fun inj' (c : con) = 
	      (case cout c
		 of Inj_c (i,k,c) => SOME (i,k,c)
		  | _ => NONE)
	    val inj = opt_out "Expected Inj_c" inj'

	    fun star' (c : con) = 
	      (case cout c
		 of Star_c => SOME ()
		  | _ => NONE)
	    val star = opt_out "Expected Star_c" star'

	    fun var' (c : con) = 
	      (case cout c
		 of Var_c a => SOME a
		  | _ => NONE)
	    val var = opt_out "Expected Var_c" var'

	    fun ptr' (c : con) =
	      (case cout c
		 of Ptr_c c => SOME c
		  | _ => NONE)
	    val ptr = opt_out "Expected Ptr_c" ptr'

	    fun pi1' (c : con) =
	      (case cout c
		 of Pi1_c c => SOME c
		  | _ => NONE)
	    val pi1 = opt_out "Expected Pi1_c" pi1'

	    fun pi2' (c : con) =
	      (case cout c
		 of Pi2_c c => SOME c
		  | _ => NONE)
	    val pi2 = opt_out "Expected Pi2_c" pi2'

	    fun sumcase' (c : con) = 
	      (case cout c
		 of Case_c arg => SOME arg
		  | _ => NONE)
	    val sumcase = opt_out "Expected Case_c" sumcase'
	  end  (* Strip' *)


	structure Strip = 
	  struct

	    fun normf f = fn c => f (R.whnf c)
	    fun normp (f,f') = (normf f,normf f')

	    val kapp = normf Strip'.kapp
	    val kapp' = normf Strip'.kapp'
	    val app = normf Strip'.app
	    val app' = normf Strip'.app'
	    val APP = normf Strip'.APP
	    val APP' = normf Strip'.APP'
	    val nat = normf Strip'.nat
	    val nat' = normf Strip'.nat'
	    val pair = normf Strip'.pair
	    val pair' = normf Strip'.pair'	      
	    val fold = normf Strip'.fold
	    val fold' = normf Strip'.fold'
	    val lam = normf Strip'.lam
	    val lam' = normf Strip'.lam'
	    val pr = normf Strip'.pr
	    val pr' = normf Strip'.pr'
	    val inj = normf Strip'.inj
	    val inj' = normf Strip'.inj'
	    val star = normf Strip'.star
	    val star' = normf Strip'.star'
	    val (LAM,LAM') = normp (Strip'.LAM,Strip'.LAM')
	    val (var,var') = normp (Strip'.var,Strip'.var')
	    val (ptr,ptr') = normp (Strip'.ptr,Strip'.ptr')
	    val (pi1,pi1') = normp (Strip'.pi1,Strip'.pi1')
	    val (pi2,pi2') = normp (Strip'.pi2,Strip'.pi2')
	    val (sumcase,sumcase') = normp (Strip'.sumcase,Strip'.sumcase')
	  end (* Strip *)


	structure C' = CF(structure Strip = Strip')
	structure C = CF(structure Strip = Strip)
	  
	structure Q =
	  struct
	    fun coercion' sv =
	      (case sv
		 of Coercion q => SOME q
		  | _ => NONE)
	    val coercion = opt_out "Not a coercion" coercion'

	    fun pack' sv =
	      obind (coercion' sv)
	      (fn q => case q
			 of (Pack,[tas,thiding]) => SOME (tas,thiding)
			  | _ => NONE)
	    val pack = opt_out "Not a pack" pack'

	    fun forgetknown' sv =
	      obind (coercion' sv)
	      (fn q => case q
			 of (ForgetKnown,[ksum]) => SOME ksum
			  | _ => NONE)
	    val forgetknown = opt_out "Not a forgetknown" forgetknown'

	    fun projknown' sv =
	      obind (coercion' sv)
	      (fn q => case q
			 of (ProjKnown,[ksum]) => SOME ksum
			  | _ => NONE)
	    val projknown = opt_out "Not a projknown" projknown'

	    fun injunion' sv =
	      obind (coercion' sv)
	      (fn q => case q
			 of (InjUnion,[ksum]) => SOME ksum
			  | _ => NONE)
	    val injunion = opt_out "Not an injunion" injunion'

	    fun injforget' sv =
	      obind (coercion' sv)
	      (fn q => case q
			 of (InjForget,[ksum]) => SOME ksum
			  | _ => NONE)
	    val injforget = opt_out "Not an injforget" injforget'

	  end

	structure E = 
	  struct

	    fun nary_tapp (sv : sv32) : (sv32 * con list) = 
	      let
		fun loop (sv,cc) = 
		  (case sv
		     of TApp (sv,c) => loop (sv,c::cc)
		      | _ => (sv, cc))
	      in loop (sv,[])
	      end

	    fun coerce' sv =
	      (case sv 
		 of Coerce(q,sv) => SOME (q,sv)
		  | _ => NONE)

	    val coerce = opt_out "Not a coerce" coerce'

	    fun pack' sv = 
	      obind (coerce' sv)
	      (fn (q,sv) => obind (Q.pack' q)
	       (fn (tas,thiding) => SOME (tas,thiding,sv)))
	    val pack = opt_out "Not a pack" pack'

	    fun forgetknown' sv = 
	      obind (coerce' sv)
	      (fn (q,sv) => obind (Q.forgetknown' q)
	       (fn ksum => SOME (ksum,sv)))
	    val forgetknown = opt_out "Not a forgetknown" forgetknown'

	    fun projknown' sv = 
	      obind (coerce' sv)
	      (fn (q,sv) => obind (Q.projknown' q)
	       (fn ksum => SOME (ksum,sv)))
	    val projknown = opt_out "Not a projknown" projknown'

	    fun injunion' sv = 
	      obind (coerce' sv)
	      (fn (q,sv) => obind (Q.injunion' q)
	       (fn ksum => SOME (ksum,sv)))
	    val injunion = opt_out "Not a injunion" injunion'

	    fun injforget' sv = 
	      obind (coerce' sv)
	      (fn (q,sv) => obind (Q.injforget' q)
	       (fn ksum => SOME (ksum,sv)))
	    val injforget = opt_out "Not a injforget" injforget'
	      	    
	  end
	

    (* This must be totally in synch with the TranslationDefs module. *)
    structure TD = 
      struct

	fun all f l = if LO.all isSome (map f l) then SOME () else NONE
	fun all2 f (l1,l2) = if LO.all isSome (LO.map2 f (l1,l2)) then SOME () else NONE

	fun app4 (d1,d2,d3,d4) [a1,a2,a3,a4] =
	  (obind (d1 a1)
	   (fn _ => obind (d2 a2)
	    (fn _ => obind (d3 a3)
	     (fn _ => obind (d4 a4)
	      (fn _ => SOME ())))))
	  | app4 _ _ = NONE

	fun alln i decs l = 
	  if List.length l <> 4 then NONE
	  else all2 (fn (f,t) => f t) (decs,l)
	    
	fun countflat f l = 
	  if List.all isSome (LO.mapcount f l)
	    then SOME ()
	  else NONE
	fun countbig f t = f (!flattenThreshold + 1,t)
	fun countall f g l = 
	  if List.length l <> (!flattenThreshold + 2) then NONE
	  else
	    let
	      val (flats,big) = LO.split l
	    in 
	      obind (countflat f flats)
	      (fn () => countbig g big)
	    end

	val Tupleidx'  = fn i => LU.i2w (if i > !flattenThreshold then !flattenThreshold +1 else i)
	val Flatidx    = fn i => LU.i2w i
	val Bigidx     = fn () => LU.i2w (!flattenThreshold + 1)
	  
	val Tupleidx  : w32 = 0w0
	val BFloatidx : w32 = 0w1
	val Ptridx    : w32 = 0w2
	val Otheridx  : w32 = 0w3
	  
	fun Tmilr k = 
	  obind (K.sum' k)
	  (fn ks => 
	   let
	     fun tuple (i,k) = 
	       obind (K.ntuple' k)
	       (fn ks => if List.length ks <> i then NONE
			 else all K.T32' ks)
	     fun big (i,k) = 
	       obind (K.list' k)
	       (fn k => K.T32' k)
	   in countall tuple big ks
	   end)
	
	fun Tmil (k : kind) = 
	   obind (K.sum' k)
	   (fn ks => app4 (Tmilr,K.unit',K.TM',K.T32') ks)
	fun vareq a1 a2 = if Name.eq_var (a1,a2) then SOME () else NONE

	fun listed_tuple a l =
	  let 
	    fun isiproj (i,opt) = 
	      obind opt
	      (fn (i',c) => 
	       if i = i' then 
		 obind (C.var' c) (vareq a)
	       else NONE)
	    val l = map C.nproj' l
	    val l = LO.mapcount isiproj l
	  in if List.all isSome l then SOME ()
	     else NONE
	  end

	fun interpr (c : Lil.con) : Lil.con option = 
	  case (C.sumcase' c)
	    of SOME (arg,arms,NONE) => 
	      let
		fun flatarm (i,(i',(a,c))) = 
		  if (LU.i2w i) <> i' then NONE
		  else obind (C.ptr' c)
		    (fn c => obind (C.tuple_ml' c)
		     (fn l => obind (listed_tuple a l)
		      (fn () => SOME ())))
		fun bigarm (i,(i',(a,c))) =
		  if (LU.i2w i) <> i' then NONE
		  else obind (C.ptr' c)
		    (fn c => obind (C.tuple' c)
		     (fn l => obind (C.var' l)
		      (vareq a)))
		val isinterp = isSome (countall flatarm bigarm arms)
	      in if isinterp then SOME arg
		 else NONE
	      end
	     | _ => NONE
      
	fun interp (c : Lil.con) : Lil.con option = 
	  obind (C.sumcase' c)
	  (fn (arg,arms,NONE) => 
	   let
	     fun tuple (i,(a,c)) =
	       if i <> Tupleidx then NONE
	       else obind (interpr c)
		 (fn c => obind (C.var' c)
		  (vareq a))
	     fun bfloat (i,(a,c)) =
	       if i <> BFloatidx then NONE
	       else obind (C.ptr' c)
		 (fn c => obind (C.boxed' c)
		  (fn (s,c) => if s = B8 then C.float' c else NONE))

	     fun ptr (i,(a,c)) =
	       if i <> Ptridx then NONE
	       else obind (C.ptr' c)
		 (fn c => obind (C.var' c)
		  (vareq a))

	     fun other (i,(a,c)) =
	       if i <> Otheridx then NONE
	       else obind (C.var' c) (vareq a)

	   in if isSome (app4 (tuple,bfloat,ptr,other) arms) then SOME arg
	      else NONE
	   end
	| _ => NONE)
	  

(*
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

      (* Consider moving the case to the inside? 
       *)

      (* :: Tmilr -> T32 -> T32 *)
      fun VarargTuplecasedef arg rest = 
	let
	  val flatarm = fn i => (Flatidx i,fn l => LD.T.arrow (LD.COps.ntuple2tlist i l) (LD.C.nill (LD.K.T64())) rest)
	  val bigarm  = fn i => (Bigidx(),fn t => LD.T.arrow' [LD.T.tupleptr t] [] rest)
	  val arms= countall flatarm bigarm
	in LD.C.sumcase arg arms NONE
	end

      (* :: Tmil ->  T32  -> T32 *)
      fun Varargcasedef argc rest = 
	let
	  val BFloata = fn _ => LD.T.arrow' [LD.T.ptr (LD.T.boxed_float ())] [] rest
	  val Ptra = fn t => LD.T.arrow' [LD.T.ptr t] [] rest
	  val Otherwisea = fn t => LD.T.arrow' [t] [] rest
	  val Tuplea = fn t => VarargTuplecasedef t rest
	  val branch = LD.C.sumcase argc [(Tupleidx,Tuplea),(BFloatidx,BFloata),(Ptridx,Ptra),(Otheridx,Otherwisea)] NONE
	in branch
	end
      (* :: Tmil -> T32 -> T32 *)
      fun Varargdef () = 
	let
	  val argv = Name.fresh_named_var "arg_c"
	  val argc = LD.C.var argv
	  val restv = Name.fresh_named_var "res_t"
	  val rest = LD.C.var restv
	in 
	  LD.C.nlambda [(argv,Tmil()),(restv,LD.K.T32())]
	  (Varargcasedef argc rest)
	end

      (* If argc is known, then we can reduce the sum and make
       * this much smaller.  Otherwise, we are much happier 
       * leaving the beta redex around so that we only have one copy
       * of the argument types and their interpretations.
       *)
      fun Vararg argc resc = 
	(case (Dec.C.inj' argc,!simplify)
	   of (SOME _,true) => Varargcasedef argc (interp resc)
	    | _ => LD.C.appn (Varargdef()) [argc,interp resc])
	   
      fun IfTaglike arg ift ifnott = 
	let
	  val ift = fn _ => ift
	  val ifnott = fn _ => ifnott
	  val Tuplea = ifnott
	  val BFloata = ifnott
	  val Ptra = ifnott
	  val Otherwisea = ift
	in LD.C.sumcase arg [(Tupleidx,Tuplea),(BFloatidx,BFloata),(Ptridx,Ptra),(Otheridx,Otherwisea)] NONE
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
*)

    fun unit_arm c = obind (C.ptr' c) (fn c => obind (C.tuple_ml' c) (fn l => if null l then SOME () else NONE))
    fun void_arm c = C.void' c
	
    fun proof' w chkarm c = 
      (case C.sumcase' c
	 of SOME (arg,[(w',(a,c))],SOME def) => 
	   if w <> w' then NONE
	   else obind (void_arm def)
	     (fn _ => obind (chkarm c)
	      (fn _ => SOME arg))
	  | _ => NONE)

    fun proof w c = proof' w unit_arm c
	
    fun Rtuple c = 
      (case C.sum_ml' c
	 of SOME (0w0,arms) => countall (fn (i,c) => proof (LU.i2w i) c) (fn (i,c) => proof (LU.i2w i) c) arms
	  | _ => NONE)

    val tuple = proof' Tupleidx Rtuple
    val bfloat = proof BFloatidx
    val ptr = proof Ptridx
    val other = proof Otheridx
       
    (* WARNING: This does not check that all of the case args are the same.
     * This could make life unhappy.  I want my recursive modules!!!
     *)
    fun R c = 
      (case (C.sum_ml' c)
	 of SOME (0w0,[t,b,p,ot]) => 
	   obind (tuple t)
	   (fn _ => obind (bfloat b)
	    (fn _ => obind (ptr p)
	     (fn _ => obind (other ot)
	      (fn c => SOME c ))))
	  | _ => NONE)

      end

      end (* Dec *)


    structure Elim = 
      struct
	structure K = 
	  struct
	    (***************** Kind destructors *****************)

	    fun app' k = obind (Dec.K.arrow' k) (fn (_,rk) => SOME rk)
	    val app = opt_out "App of non-arrow kind" app'
	    fun APP' k1 k = 
	      obind (Dec.K.forall' k1)
	      (fn (j,kbody) => SOME (LS.varKindKindSubst j k kbody))

	    fun APP k1 k = 
	      let
		val (j,kbody) = Dec.K.forall k1
	      in LS.varKindKindSubst j k kbody
	      end

	    fun pi1' k = obind (Dec.K.pair' k) (fn (a,_) => SOME a)
	    fun pi2' k = obind (Dec.K.pair' k) (fn (_,b) => SOME b)
	    val pi1 = opt_out "Not a pair" pi1'
	    val pi2 = opt_out "Not a pair" pi2'

	    fun unfold' (k : Lil.kind) = 
	      obind (Dec.K.mu' k)
	      (fn (j,kbody) => SOME (LS.varKindKindSubst j k kbody))
	    val unfold = opt_out "Not a mu" unfold'

	  end (* K *)

	structure C = 
	  struct

	    (* Inlined from Lildefs.C.app.  Need recursive modules, Derek! 
	     *)
	    fun ldapp c1 c2 = Lil.mk_con (Lil.App_c (c1,c2))

	    fun unroll' c = 
	      obind (Dec.C.rek' c)
	      (fn (k,c,cp) => 
	       let
		 val prec = ldapp (mk_con (APP_c (mk_pcon Rec_c,k))) c
	       in SOME (ldapp (ldapp c prec) cp)
	       end)

	    val unroll = opt_out "Not an eliminable recursive type" unroll'

	    fun instantiate q w = 
	      let
		val lam = 
		  (case Dec.C.polyprim11 q
		     of (Exists_c,_,lam) => lam
		      | (Forall_c,_,lam) => lam
		      | _ => error "Illegal instantation")
	      in ldapp lam w
	      end
	      
	    fun unpack (a,t) = 
	      case Dec.C.polyprim11 t 
		of (Exists_c,k,lam) => (k,ldapp lam (mk_con (Var_c a)))
		 | _ => error "Unpack of non-existential"

	    fun unbox c = 
	      errbind "unbox: Not a ptr" (Dec.C.ptr' c)
	      (fn c => errbind "Not a boxed type" (Dec.C.boxed' c)
	       (fn (_,c) => c))

	    fun coerce c = 
	      case Dec.C.prim2 c
		of (Coercion_c,from,to) => to
		 | _ => error "Not a coercion type"

	    fun select iw c = 
	      errbind "select: Not a ptr" (Dec.C.ptr' c)
	      (fn c => errbind "Not a tuple" (Dec.C.tuple_ml' c)
	       (fn args => errbind "Insufficient fields" (LU.wnth' iw args) (fn c => c)))
	      
	    fun project iw c = 
	      case Dec.C.prim3 c
		of (KSum_c,which,tagcount,carriers) => Dec.C.nth ((w2i iw) - (w2i (Dec.C.nat tagcount))) carriers
		 | _ => error "Not a known sum"

	    fun externapp c = 
	      case Dec.C.prim3 c
		of (ExternArrow_c s, args,fargs,res) => res
		 | _ => error "Not an extern arrow"

	    fun app c = 
	      errbind "app: Not an arrow" (Dec.C.arrow' c)
	      (fn (args,fargs,rtype) => rtype)

	    fun call c = 
	      errbind "Not a code arrow" (Dec.C.code' c)
	      (fn (args,fargs,rtype) => rtype)

	  end (* C *)
      end (* Elim *)


  end (* Deconstruct *)

end (* local *)