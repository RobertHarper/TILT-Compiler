
(* The main LIL datatype
 * Leaf Petersen
 *)

structure Lil :> LIL =
  struct
    structure VarSet = Name.VarSet

    type uid = Word.word
    type var = Name.var
    type label = Name.label

    type w32 = Word32.word
    type prim = Prim.prim

    datatype size = B1 | B2 | B4 | B8


    val flattenThreshold = Nil.flattenThreshold


    datatype kind_ =
      T of size
    | Tmem
    | Unit_k
    | Nat_k
    | Arrow_k of kind * kind
    | Prod_k of kind * kind
    | Sum_k of kind list
    | Var_k of var
    | Mu_k of var * kind
    | All_k of var * kind

    withtype kind = { k : kind_ , id : uid, kvars : VarSet.set}
      

    datatype primcon =                             (* classifies term-level ... *)
      Int_c of size                           (* register integers *)
    | Float_c                          (* register floating-points *)
    | Boxed_c of size                         (* boxed values *)
    | Embed_c of size
    | Void_c
    | Tuple_c
    | Dyntag_c
    | Array_c of size                         (* arrays *)
    | Tag_c
    | Ref_c 
    | Sum_c                                   (* tagcount, carriers *)
    | KSum_c                                  (* which, tagcount, carriers *)
    | Exists_c
    | Forall_c
    | Rec_c
    | Arrow_c
    | Code_c 
    | ExternArrow_c of size
    | Coercion_c

    datatype con_ = 
      Var_c of var
    | Nat_c of w32
    | App_c of con * con
    | APP_c of con * kind
    | Pi1_c of con
    | Pi2_c of con
    | Prim_c of primcon
    | Pr_c of var * (var * kind) * kind * var * con
    | Case_c of con * (w32 * (var * con)) list * con option  
    | LAM_c of var * con
    | Lam_c of (var * kind) * con
    | Star_c
    | Pair_c of con * con
    | Inj_c of w32 * kind * con 
    | Fold_c of kind * con
    | Ptr_c of con

    withtype con = { c : con_,id : uid,whnf : con_ option ref, cvars : VarSet.set, kvars : VarSet.set } 

    datatype ctag = 
      Roll 
    | Unroll 
    | Pack 
    | ForgetKnown 
    | ProjKnown
    | InjUnion
    | InjForget   (*Composite inject and forget *)
    withtype coercion = ctag * con list (* Coercion and decorations*)

    datatype primarg = slice of size * sv32 | arg32 of sv32 | arg64 of sv64
    and sv64 = 
      Var_64 of var 
      | Const_64 of value
    and sv32 = 
      Var_32 of var 
      | Label of label
      | Coercion of coercion 
      | Coerce of sv32 * sv32 
      | Tabs of (var * kind) * sv32
      | TApp of sv32 * con
      | Const_32 of value
      | Tag of w32		(*  w32 *)
      | Unit
    withtype value = (con,primarg) Prim.value


    datatype op64 = 
      Val_64 of sv64 
      | Unbox of sv32
      | Prim64 of Prim.prim * primarg list
      | ExternAppf of sv32 * sv32 list * sv64 list

    datatype lilprimop32 = 
      Box 			(*  sv64 *)
      | Tuple 			(*  sv32 list *)
      | Select of w32		(*  w32 * sv32 *)
      | Dyntag 			(*  con *)
      | Ptreq                   (* sv32 * sv32 *)

    datatype op32 = 
      Val of sv32 
      | Prim32 of Prim.prim * con list * primarg list
      | PrimEmbed of size * Prim.prim * primarg list
      | LilPrimOp32 of lilprimop32 * con list * sv32 list * sv64 list
      | ExternApp of sv32 * sv32 list * sv64 list
      | App of sv32 * sv32 list * sv64 list
      | Call of sv32 * sv32 list * sv64 list
      | Switch of switch
      | Raise of con * sv32 
      | Handle of con * exp * (var * exp)
    and exp_ = 
      Val32_e of sv32
      | Let_e of bnd list * exp
    and bnd =
      Fixcode_b of  (var * function) list
      | Exp32_b of var * op32
      | Exp64_b of var * op64
      | Unpack_b of var * var  * sv32
      | Split_b of var * var * con
      | Unfold_b of var * con
      | Inj_b of w32 * var * con * sv32
      (* let inj_i a = (c,sv) in e == Vcase argc i a e sv 
       * argc = argument
       * i = index of inhabited arm
       * a = bound variable
       * e = inhabited arm
       * sv : void in all uninhabited arms
       *)

    and function = Function of {tFormals    : (var * kind) list,
				eFormals    : (var * con) list,
				fFormals    : (var * con) list,
				rtype       : con,
				body        : exp}
    and switch = 
      Sumcase of   {arg : sv32,arms :(w32  * var * exp) list,         default: exp option, rtype : con}
      | Dyncase of {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con}
      | Intcase of {arg : sv32,arms :(w32 * exp) list,size : size,    default: exp,        rtype : con}
      | Ifthenelse of {arg : conditionCode,  (* Ifnonzero arg.  If arg <> Tag 0 then thenArm else elseArm *)
		       thenArm : exp,
		       elseArm : exp,
		       rtype : con}
    and conditionCode =                          (* Used by Ifthenelse *)
      Exp_cc of exp                              
      | And_cc  of conditionCode * conditionCode (* Short-circuiting *)
      | Or_cc   of conditionCode * conditionCode (* Short-circuiting *)
      | Not_cc  of conditionCode
      
    withtype exp = { e: exp_ }

    (* Invariant: data is closed *)
    datatype data = 
      Dboxed of label * sv64 
      | Darray of label * size * con * value list
      | Dtuple of label * con * coercion list * sv32 list  (* l : c = q @ <svs> *)
      | Dcode of label * function
      
    type timport = label * var * kind

    (* A lil module is:
     * 1) a list of global imported types, which are bound throughout the module
     * 2) a list of data entries (closed values)
     * 3) a constructor, which taken together with the imported types
     *    comprises a function from the imported types to the exported types
     * 4) a function (implicitly polymorphic over the imported types) whose
     *    initial arguments are the representations of the type imports 
     *    and whose subsequent arguments are the term imports of the module.
     *    This can be implemented as two seperate functions, but it is hard
     *    to avoid duplicating lots of code in this case.
     *)
    datatype module = MODULE of {unitname : string,
				 parms : Name.LabelSet.set,
				 entry_c : label,
				 entry_r : label,
				 timports : timport list,
				 data   : data list,
				 confun : con}


    datatype interface = INTERFACE of {unitname : string,
				       timports : timport list,
				       entry_c : label * var * kind,
				       entry_r : label * con}


    val inc = Stats.counter_inc
    val con_lookups = Stats.counter "contable_lookups"
    val con_makes = Stats.counter "contable_misses"
    val kind_lookups = Stats.counter "kindtable_lookups"
    val kind_makes = Stats.counter "kindtable_misses"

    (* Hashing code borrowed from Karl Crary's TALT implementation
     *)
    fun hash_aux h l =
      (case l of
	 nil => h
       | x :: t => hash_aux (Word.<<(h, 0w5) + h + 0w720 + x) t)

    fun hash l = hash_aux 0w0 l

    (* Start stamps at 200 to avoid colliding with small numbers *)
    val next_stamp = ref 0w200
    fun new_stamp () =
      let val s = !next_stamp
      in
	next_stamp := s+0w1;
	s
      end

    (*
     * The following list of primes comes from http://planetmath.org/encyclopedia/GoodHashTablePrimes.html
     * with the following commentary. -leaf
     *)
    (*
     In the course of designing a good hashing configuration, it is helpful to have a list of prime numbers for the hash table size.
     
     The following is such a list. It has the properties that:
     
     1. each number in the list is prime (as you no doubt expected by now)
     2. each number is slightly less than twice the size of the previous
     3. each number is as far as possible from the nearest two powers of two
     
     Using primes for hash tables is a good idea because it minimizes clustering in the hashed table.
     
     Item (2) is nice because it is convenient for growing a hash table in the face of expanding data.
     
     Item (3) has, allegedly, been shown to yield especially good results in practice.
     *)

    val primes = [
		  53,
		  97,
		  193,
		  389,
		  769,
		  1543,
		  3079,
		  6151,
		  12289,
		  24593,
		  49157,
		  98317,
		  196613,
		  393241,
		  786433,
		  1572869,
		  3145739,
		  6291469,
		  12582917,
		  25165843,
		  50331653,
		  100663319,
		  201326611,
		  402653189,
		  805306457
(*		  ,1610612741   too large for 31 bit ints *)
		  ]

    (*******  KINDS  *******)

    fun eq_kind (k1 : kind,k2 : kind) = (#id k1) = (#id k2)
    fun cmp_kind (k1 : kind,k2 : kind) = Word.compare(#id k1,#id k2)
  

    (*****  hash table  *****)

    fun size2word (s : size) : word = 
      (case s
	 of B1 => 0w11
	  | B2 => 0w21
	  | B4 => 0w41
	  | B8 => 0w81)

    val v2w = Word.fromInt
    val w2w = Word.fromLargeWord


    structure KindKey : HASH_KEY =
      struct
	type hash_key = kind_
	  
	fun hashVal k_ =
	  (case k_
	     of T s => hash [0w0,size2word s]
	      | Tmem => hash [0w1]
	      | Unit_k => hash [0w2]
	      | Nat_k => hash [0w3]
	      | Arrow_k (k1,k2) => hash [0w4,#id k1,#id k2]
	      | Prod_k (k1,k2)  => hash [0w5,#id k1,#id k2]
	      | Sum_k ks => 
	       let
		 fun loop ([] : kind list,acc) = 0w6::acc
		   | loop (k::ks,acc) = loop(ks,(#id k)::acc)
	       in hash (loop(ks,[]))
	       end
	      | Var_k j => hash [0w7, v2w j]
	      | Mu_k (j,k) => hash [0w8,v2w j,#id k]
	      | All_k (j,k) => hash [0w9,v2w j,#id k])
	     
	fun sameKey (k1_, k2_) =
	  (case (k1_,k2_)
	     of (T s1,T s2) => s1 = s2
	      | (Tmem,Tmem) => true
	      | (Unit_k,Unit_k) => true
	      | (Nat_k,Nat_k)   => true
	      | (Arrow_k (k1,k2),Arrow_k (k1',k2')) => eq_kind (k1,k1') andalso eq_kind(k2,k2')
	      | (Prod_k (k1,k2),Prod_k (k1',k2'))   => eq_kind (k1,k1') andalso eq_kind(k2,k2')
	      | (Sum_k args1,Sum_k args2) =>
	       let
		 fun loop ([],[]) = true
		   | loop (k1::ks1,k2::ks2) = eq_kind (k1,k2) andalso loop (ks1,ks2)
		   | loop _ = false
	       in loop (args1,args2)
	       end
	      | (Var_k j1,Var_k j2) => Name.eq_var(j1,j2)
	      | (Mu_k (j1,k1),Mu_k (j2,k2))   => Name.eq_var(j1,j2) andalso (eq_kind(k1,k2))
	      | (All_k (j1,k1),All_k (j2,k2)) => Name.eq_var(j1,j2) andalso (eq_kind(k1,k2))
	      | _ => false)
      end

    structure KT = HashTableFn (KindKey)

    (*****  injection  *****)
    val kind_table : kind KT.hash_table ref =
      ref (KT.mkTable (101, Fail "call find, not lookup"))
      
    local

      val T1 = {k=T B1, id = new_stamp(), kvars = VarSet.empty}
      val T2 = {k=T B2, id = new_stamp(), kvars = VarSet.empty}
      val T4 = {k=T B4, id = new_stamp(), kvars = VarSet.empty}
      val T8 = {k=T B4, id = new_stamp(), kvars = VarSet.empty}
      val TM = {k=Tmem, id = new_stamp(), kvars = VarSet.empty}
      val Unitk = {k=Unit_k, id = new_stamp(), kvars = VarSet.empty}
      val Nat = {k=Nat_k, id = new_stamp(), kvars = VarSet.empty}


      fun free_kvars k_ = 
	(case k_ 
	   of Arrow_k (k1,k2) => VarSet.union (#kvars k1,#kvars k2)
	    | Prod_k (k1,k2) => VarSet.union (#kvars k1,#kvars k2)
	    | Sum_k ks => List.foldl (fn (k,f) => VarSet.union(#kvars k,f)) VarSet.empty ks
	    | Var_k j => VarSet.singleton j
	    | Mu_k (j,k) => VarSet.difference(#kvars k,VarSet.singleton j)
	    | All_k (j,k) => VarSet.difference(#kvars k,VarSet.singleton j)
	    | _ => VarSet.empty)

    in
      fun reset_kind_table () = kind_table := (KT.mkTable (101, Fail "call find, not lookup"))

      fun mk_kind (k_ : kind_) = 
	(case k_
	   of T s => 
	     (case s
		of B1 => T1
		 | B2 => T2
		 | B4 => T4
		 | B8 => T8)
	    | Tmem => TM
	    | Unit_k => Unitk
	    | Nat_k => Nat
	    | _ => 
		let
		  val () = inc kind_lookups
		in
		  (case KT.find (!kind_table) k_ of
		     NONE =>
		       let
			 val () = inc kind_makes
			 val stamp = new_stamp ()
			 val kvars = free_kvars k_
			 val k = {k=k_, id = stamp, kvars = kvars}
		       in
			 KT.insert (!kind_table) (k_, k);
			 k
		       end
		   | SOME k => k)
		end)
    end

    (*******  CONSTRUCTORS  *******)

    fun eq_con (c1 : con,c2 : con) = (#id c1) = (#id c2)
    fun cmp_con (c1 : con,c2 : con) = Word.compare (#id c1,#id c2)

    (*****  hash tables  *****)

    structure ConKey : HASH_KEY =
      struct
	type hash_key = con_
	  
	fun hashprimcon h p =
	  (case p
	     of Int_c s    => hash[h,0w2000,size2word s]
	      | Float_c    => hash[h,0w2001]
	      | Void_c     => hash[h,0w2002]
	      | Boxed_c s  => hash[h,0w2003,size2word s]
	      | Tuple_c    => hash[h,0w2004]
	      | Array_c s  => hash[h,0w2005,size2word s]
	      | Arrow_c    => hash[h,0w2006]
	      | Code_c     => hash[h,0w2007]
	      | Dyntag_c   => hash[h,0w2008]
	      | Tag_c      => hash[h,0w2009]
	      | Sum_c      => hash[h,0w2010]
	      | KSum_c     => hash[h,0w2011]
	      | Exists_c   => hash[h,0w2012]
	      | Forall_c   => hash[h,0w2013]
	      | Rec_c      => hash[h,0w2014]
	      | ExternArrow_c s => hash[h,0w2015,size2word s]
	      | Coercion_c => hash[h,0w2016]
	      | Embed_c s  => hash[h,0w2017,size2word s]
	      | Ref_c      => hash[h,0w2018])

	fun hashVal c_ =
	  (case c_
	     of Var_c a => hash[0w0,v2w a]
	      | Nat_c w => hash[0w1,w2w w]
	      | App_c (c1,c2) => hash[0w2,#id c1,#id c2]
	      | APP_c (c,k)   => hash[0w3,#id c, #id k]
	      | Pi1_c c       => hash[0w4,#id c]
	      | Pi2_c c       => hash[0w5,#id c]
	      | Prim_c p      => hashprimcon 0w6 p
	      | Pr_c (j,(a,k),k',r,body)  => 
	       hash[0w7,v2w j,v2w a,#id k,#id k',v2w r, #id body]
	      | Case_c (c,arms,def) => 
	       let
		 fun loop ([] : (w32 * (var * con)) list ,acc) = 
		   (case def
		      of SOME cdef => 0w0::(#id cdef)::acc
		       | NONE      => 0w1::acc)
		   | loop ((w,(a,c))::arms,acc) = loop(arms,(w2w w)::(v2w a)::(#id c)::acc)
	       in hash (loop(arms,[0w8,#id c]))
	       end
	      | LAM_c (j,c) => hash[0w9,v2w j,#id c]
	      | Lam_c ((a,k),c) => hash[0w10, v2w a, #id k, #id c]
	      | Pair_c (c1,c2) => hash[0w11,#id c1,#id c2]
	      | Star_c => hash[0w12]
	      | Inj_c (w,k,c) => hash[0w13,#id k,#id c,w2w w]
	      | Fold_c (k,c) => hash[0w14,#id k,#id c]
	      | Ptr_c c => hash[0w15,#id c])

	fun sameKey (c1_, c2_) =
	  (case (c1_,c2_) 
	     of (Var_c a1,Var_c a2) => Name.eq_var(a1,a2)
	      | (Nat_c w1,Nat_c w2) => w1 = w2
	      | (App_c (c1,c2),App_c (c1',c2')) => eq_con(c1,c1') andalso eq_con(c2,c2')
	      | (APP_c (c1,k1), APP_c (c2,k2))  => eq_con(c1,c2) andalso eq_kind(k1,k2)
	      | (Pi1_c c1, Pi1_c c2) => eq_con (c1,c2)
	      | (Pi2_c c1, Pi2_c c2) => eq_con(c2,c2)
	      | (Prim_c pcon1,Prim_c pcon2) => pcon1 = pcon2
	      | (Pr_c (j1,(a1,k_in1),k1,r1,c1),Pr_c (j2,(a2,k_in2),k2,r2,c2)) =>
	       Name.eq_var(j1,j2) andalso
	       Name.eq_var(a1,a2) andalso
	       eq_kind (k_in1,k_in2) andalso
	       eq_kind (k1,k2) andalso
	       Name.eq_var(r1,r2) andalso
	       eq_con (c1,c2)
	      | (Case_c (c1,arms1,def1),Case_c (c2,arms2,def2)) =>
	       let
		 fun loop ([],[]) = true
		   | loop ((w1,(a1,c1))::arms1,(w2,(a2,c2))::arms2) = 
		   w1 = w2 andalso
		   Name.eq_var(a1,a2) andalso 
		   eq_con (c1,c2) andalso
		   loop (arms1,arms2)
		   | loop _ = false
	       in 
		 eq_con (c1,c2) andalso
		 (case(def1,def2) 
		    of (SOME c1,SOME c2) => eq_con (c1,c2) 
		     | (NONE,NONE) => true
		     | _ => false) andalso 
		 loop(arms1,arms2)
	       end
	      | (LAM_c (j1,c1), LAM_c (j2,c2)) => Name.eq_var(j1,j2) andalso eq_con(c1,c2)
	      | (Lam_c ((a1,k1),c1), Lam_c ((a2,k2),c2)) =>
	       Name.eq_var(a1,a2) andalso
	       eq_kind (k1,k2) andalso
	       eq_con(c1,c2)
	      | (Pair_c (c1,c2),Pair_c (c1',c2')) => eq_con(c1,c1') andalso eq_con(c2,c2')
	      | (Star_c,Star_c) => true
	      | (Inj_c (w1,k1,c1),Inj_c (w2,k2,c2)) => 
	       TilWord32.equal (w1,w2) andalso 
	       eq_kind (k1,k2) andalso
	       eq_con(c1,c2)
	      | (Fold_c (k1,c1),Fold_c (k2,c2)) => eq_kind(k1,k2) andalso eq_con(c1,c2)
	      | (Ptr_c c1,Ptr_c c2) => eq_con(c1,c2)
	      | _ => false)

      end
    structure CT = HashTableFn (ConKey)
    val con_table : con CT.hash_table ref =
      ref (CT.mkTable (3001, Fail "call find, not lookup"))
      
    structure ColKey : ORD_KEY = struct
				   type ord_key = word
				   val compare = Word.compare
				 end
    structure ColMap   = SplayMapFn(ColKey)
    val collisions : con_ list ColMap.map ref = ref ColMap.empty

    local
      fun primc p = {c=Prim_c p, id=new_stamp(), whnf=ref (SOME (Prim_c p)), cvars = VarSet.empty, kvars = VarSet.empty}
      val int1 = primc(Int_c B1)
      val int2 = primc(Int_c B2)
      val int4 = primc(Int_c B4)
      val int8 = primc(Int_c B8)
      val float = primc(Float_c)
      val void   = primc(Void_c)
      val boxed1 = primc(Boxed_c B1)
      val boxed2 = primc(Boxed_c B2)
      val boxed4 = primc(Boxed_c B4)
      val boxed8 = primc(Boxed_c B8)
      val tuple = primc(Tuple_c)
      val array1 = primc(Array_c B1)
      val array2 = primc(Array_c B2)
      val array4 = primc(Array_c B4)
      val array8 = primc(Array_c B8)
      val arrow = primc(Arrow_c)
      val code = primc(Code_c)
      val dyntag = primc(Dyntag_c)
      val tag = primc(Tag_c)
      val sum = primc(Sum_c)
      val ksum = primc(KSum_c)
      val exists = primc(Exists_c)
      val forall = primc(Forall_c)
      val rek = primc(Rec_c)
      val externarrow1 = primc(ExternArrow_c B1)
      val externarrow2 = primc(ExternArrow_c B2)
      val externarrow4 = primc(ExternArrow_c B4)
      val externarrow8 = primc(ExternArrow_c B8)
      val coercion = primc(Coercion_c)
      val refc = primc(Ref_c)
      val embed1 = primc(Embed_c B1)
      val embed2 = primc(Embed_c B2)
      val embed4 = primc(Embed_c B4)
      val embed8 = primc(Embed_c B8)

      fun normc c_ = {c = c_, id=new_stamp(), whnf=ref (SOME c_), cvars = VarSet.empty, kvars = VarSet.empty}
      val star = normc Star_c

      val nat_cached = 32
      fun mknat i = 
	let
	  val iw = TilWord32.fromInt i
	in
	  normc (Nat_c iw)
	end
      val nats = Array.tabulate(nat_cached,mknat)

      fun con_details (con : con_) = 
	let 
	  fun cvars (c:con) = #cvars c
	  fun kvars (c:con) = #kvars c
	in
	  case con
	    of Var_c a => (SOME con,VarSet.singleton a,VarSet.empty)
	     | App_c (c1,c2) => (NONE,VarSet.union(cvars c1,cvars c2),VarSet.union (kvars c1, kvars c2))
	     | APP_c (c,k) => (NONE,cvars c,VarSet.union(kvars c,#kvars k))
	     | Pi1_c c => (NONE,cvars c,kvars c)
	     | Pi2_c c => (NONE,cvars c,kvars c)
	     | Pr_c (j,(a,k1),k2,r,c) => 
	      (SOME con,
	       VarSet.difference (cvars c,VarSet.addList(VarSet.empty,[a,r])),
	       VarSet.difference (VarSet.union(VarSet.union(#kvars k1,#kvars k2),kvars c),VarSet.singleton j))
	     | Case_c (arg,arms,default) => 
	      let
		val cs1 = cvars arg
		val ks1 = kvars arg
		fun arm ((w,(a,c)),(cs,ks)) =
		  (VarSet.union(cs,VarSet.difference(cvars c,VarSet.singleton a)),
		   VarSet.union (ks,#kvars c))
		val (cs,ks) = foldl arm (cs1,ks1) arms
		val (cs,ks) = 
		  case default
		    of NONE => (cs,ks) 
		     | SOME c => (VarSet.union(cs,cvars c),VarSet.union(ks,kvars c))
	      in (NONE,cs,ks)
	      end
	     | LAM_c (a,c) => (SOME con,cvars c,VarSet.difference(kvars c,VarSet.singleton a))
	     | Lam_c ((a,_),c) => (SOME con,VarSet.difference(cvars c,VarSet.singleton a),kvars c)
	     | Pair_c (c1,c2) => (SOME con,VarSet.union (cvars c1, cvars c2),VarSet.union (kvars c1,kvars c2))
	     | Inj_c (w,k,c) => (SOME con,cvars c,VarSet.union(#kvars k,kvars c))
	     | Fold_c (k,c) => (SOME con,cvars c,VarSet.union(#kvars k,kvars c))
	     | Ptr_c c => (SOME con,cvars c,kvars c)
	     | Star_c => (SOME con,VarSet.empty,VarSet.empty)
	     | Nat_c _ => (SOME con,VarSet.empty,VarSet.empty)
	     | Prim_c _=> (SOME con,VarSet.empty,VarSet.empty)
	end

      fun hash_con (c_ : con_) = 
	let
	  val () = inc con_lookups
	in
	  case CT.find (!con_table) c_ of
	    NONE =>
	      let 
		val () = inc con_makes
		val stamp = new_stamp ()
		val (whnf,cfrees,kfrees) = con_details c_
		val whnf = ref whnf
		val c = {c=c_, id=stamp, whnf=whnf, cvars = cfrees, kvars = kfrees}
	      in
		CT.insert (!con_table) (c_, c);
		c
	      end
	  | SOME c => c
	end
	
    in
      fun reset_con_table () = 
	(con_table := (CT.mkTable (3001, Fail "call find, not lookup"));
	 collisions := ColMap.empty)

      fun mk_pcon p =
	(case p
	   of Int_c s    => 
	     (case s
		of B4 => int4
		 | B1 => int1
		 | B2 => int2
		 | B8 => int8)
	    | Float_c    => float
	    | Void_c     => void
	    | Boxed_c s  => 
              (case s
		 of B8 => boxed8
		  | B4 => boxed4
		  | B1 => boxed1
		  | B2 => boxed2)
	    | Tuple_c    => tuple
	    | Array_c s  => 
              (case s
		 of B4 => array4
		  | B8 => array8
		  | B1 => array1
		  | B2 => array2)
	    | Arrow_c    => arrow
	    | Code_c     => code
	    | Dyntag_c   => dyntag
	    | Tag_c      => tag
	    | Sum_c      => sum
	    | KSum_c     => ksum
	    | Exists_c   => exists
	    | Forall_c   => forall
	    | Rec_c      => rek
	    | ExternArrow_c s => 
              (case s
		 of B4 => externarrow4
		  | B1 => externarrow1
		  | B2 => externarrow2
		  | B8 => externarrow8)
	    | Coercion_c => coercion
	    | Ref_c => refc
	    | Embed_c s => 
		 (case s 
		    of B1 => embed1
		     | B2 => embed2
		     | B4 => embed4
		     | B8 => embed8))
	         
      fun mk_con (con : con_) : con = 
	let 
	in
	  case con
	    of Nat_c w => 
	      let
		val i = TilWord32.toInt w
	      in
		if i < nat_cached then
		  Array.sub(nats,i)
		else
		  hash_con con
	      end
	     | Prim_c p => mk_pcon p
	     | Star_c => star
	     | _ => hash_con con
	end
    end

    fun free_cvars_con (c : con) = #cvars c
    fun free_kvars_con (c : con) = #kvars c
    fun free_kvars_kind (k : kind) = #kvars k

    fun set_whnf (c1 : con,c2 : con) : unit = (#whnf c1) := SOME (#c c2)
    fun name_con (c : con) = "con_"^(Word.toString (#id c))
    fun name_kind (k : kind) = "kind_"^(Word.toString (#id k))

    fun mk_exp (e_ : exp_) = {e = e_ } 

    structure ConKey : ORD_KEY = struct
				   type ord_key = con
				   val compare = cmp_con
				 end
    structure ConMap   = SplayMapFn(ConKey)
    structure ConSet   = SplaySetFn(ConKey)
    structure KindKey : ORD_KEY = struct
				   type ord_key = kind
				   val compare = cmp_kind
				 end
    structure KindMap   = SplayMapFn(KindKey)
    structure KindSet   = SplaySetFn(KindKey)

    fun reset_tables () = (reset_kind_table();reset_con_table())

    fun report () = {csize = CT.numItems (!con_table),
		     ksize = KT.numItems (!kind_table),
		     cbuckets = CT.bucketSizes (!con_table),
		     kbuckets = KT.bucketSizes (!kind_table),
		     collisions = List.filter (fn (_,l) => List.length l > 1) (ColMap.listItemsi (!collisions))}

  end

