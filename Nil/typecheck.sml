functor TypeCheck(structure Nil : NIL) : TYPECHECK = 
struct

    structure Nil = Nil
    open Nil Util
    open HashTable

    exception NotFound
    exception IllFormed

    fun is_word_kind (Type_k | Word_k) = true
      | is_word_kind _ = false

    fun eprim_check (PrimOp p) = 
      | eprim_check (NilPrimOp np) = 
	(case np of
	     record => 
	   | select field =>
	   | inject which =>
	   | inject_record which =>
	   | project_sum =>
	   | project_sum_record field =>
	   | roll =>
	   | unroll =>   
	   | make_exntag =>
	   | inj_exn =>           
	   | make_vararg =>
	   | make_onearg =>
	   | get_tag =>
	   | peq =>
	   | ptreq =>
	   | closure => )



    fun is_kind tcont k = 
	(case k of 
	     (Type_k | Word_k) => true
	   | (List_k k) => is_kind tcont k
	   | (Singleton_k c) => (is_word_kind(kind_check tcont c)
				 handle IllFormed => false)
	   | (Record_k lvk) => andfold' (fn ((_,v,k),tcont) => 
					 let val tcont' = insert tcont (v,k)
					 in (is_kind tcont k, tcont')
					 end) tcont lvk
	   | (Arrow_k (a,vk,k)) => is_kind (foldl insert tcont vk) k)

    and kind_check tcont c = raise UNIMP

    fun norm_con tcont c = raise UNIMP
    fun type_check tcont econt c = raise UNIMP

    fun eq_con table (c1,c2) = 
	let 
	    val self = eq_con table
	    fun eq_primcon pc1 pc2 = pc2 = pc2
	    fun add_var table v1 v2 = insert (insert table (v1,v2)) (v2,v2)
	in (case (c1,c2) of
		(Var_c v1, Var_c v2) => eq_var(lookup table v1, lookup table v2)
	      | (Prim_c (c1,cs1), Prim_c (c2,cs2)) => (eq_primcon(c1,c2) andalso
						       eq_list(self, c1, c2))
	      | (Crecord_c lvc1, Crecord_c lvc2) =>
		    let val entries = map2 (fn ((_,v1,_),(_,v2,_)) => (v1,v2))
			val table' = foldl add_var entries table
		    in andfold (fn ((l1,_,c1),(l2,_,c2)) => (eq_label(l1,l2) andalso 
							     eq_con table'(c1,c2)))
			(zip2 lvc1 lvc2)
		    end
	      | (Proj_c (l1,c1), Proj_c (l2,c2)) => eq_label(l1,l2) andalso self(c1,c2)
	      | (Let_c (v1,c1,c1'), Let_c (v2,c2,c2')) => (self (c1,c2) andalso
							   eq_con (add_var table v1 v2) (c1',c2'))
	      | (Fun_c (a1,cf1), Fun_C(a2,cf2)) => (a1 = a2) andalso eq_confun table (cf1,cf2)
	      | (Closure_c(c1,c1'), Closure_c(c2,c2')) => (self (c1,c2) andalso
							   self (c1',c2'))
	      | (App_c(a1,c1,cs1), App_c(a2,c2,cs2)) => ((a1 = a2) andalso self(c1,c2) andalso 
							 eq_list(self,cs1,cs2))
	      | (Mu_c(vc1,c1),Mu_c(vc2,c2)) => 
		    let val entries = map2 (fn ((v1,_),(v2,_)) => (v1,v2)) (vc1,vc2)
			val table' = foldl add_var entries table
		    in eq_con table' (c1,c2) andalso
			andfold (fn ((_,c1),(_,c2)) => eq_con table'(c1,c2))
			(zip2 vc1 vc2)
		    end
	      | (Listcase_c {arg=c1,arms=lcfs1,default=co1}, 
		    Listcase_c {arg=c2,arms=lcfs2,default=co2}) =>
		    (self(c1,c2) andalso eq_opt(self,co1,co2) andalso
		     eq_list (fn ((lc1,cf1),(lc2,cf2)) => ((lc1 = lc2 andalso 
							    eq_confun table (cf1,cf2)), lcfs1, lcfs2)))
	      | (All_c cf1, All_c cf2) => eq_confun table (cf1,cf2) 
	      | (Code_c (e1,cf1), Code_c (e2,cf2)) => e1 = e2 andalso (eq_confun table (cf1,cf2))
	      | (Annotate_c(a1,c1), Annotate_c(a2,c2)) => self (c1,c2))
	end
    and eq_confun table ((vks1,c1), (vks2,c2)) = 
	let val entries = map2 (fn ((v1,_),(v2,_)) => (v1,v2)) (vks1,vks2)
	    val table' = foldl add_var entries table
	in eq_con table' (c1,c2) (* andalso eq_kind on the kinds? XXX *)
	end
    val eq_con = fn arg => eq_con (Name.mk_var_hash_table (100,NotFound)) arg

    val empty_econtext : (Nil.var, Nil.con) HashTable.hash_table 
	= Name.mk_var_hash_table (100,NotFound)
    val empty_tcontext : (Nil.var, Nil.kind) HashTable.hash_table 
	= Name.mk_var_hash_table (100,NotFound)

end