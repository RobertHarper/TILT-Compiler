(* Lil pretty-printer. *)

structure PpLil :> PPLIL =
  struct

    open Lil Formatter
    open Util Name Prim Ppprim
    structure Dec = Deconstruct.Dec

    val SyntaxWarn = Stats.tt "PpLilSyntaxWarn"

    (* TDsugar implies sugar *)
    val sugar = Stats.tt "PpLilSugar"
    val TDsugar = Stats.tt "PpLilTDSugar"
    val show_frees = Stats.ff "PpLilShowFrees"
    val show_sharing = Stats.tt "PpLilShowSharing"
    val error = fn s => error "Lil:pp.sml" s

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    fun warn s = if !SyntaxWarn then print s else ()


    local
      val cons  : ConSet.set ref = ref (ConSet.empty)
      val kinds : KindSet.set ref = ref (KindSet.empty)
    in
      
      fun reset () = (cons := (ConSet.empty);kinds := KindSet.empty)
      fun kseen k = KindSet.member (!kinds,k)
      fun ksee k = kinds := KindSet.add (!kinds,k)

      fun cseen c = ConSet.member (!cons,c)
      fun csee c = cons := ConSet.add (!cons,c)
    end

    fun smallcon c = 
      (case cout c
	 of Var_c _ => true
	  | Nat_c _ => true
	  | Pi1_c c => smallcon c
	  | Pi2_c c => smallcon c
	  | Prim_c _ => true
	  | Star_c => true
	  | Inj_c (w,k,c) => smallcon c
	  | Fold_c (k,c) => smallcon c
	  | Ptr_c c => smallcon c
	  | _ => false)

    fun pp_region s1 s2 fmt = HOVbox((String s1) :: (fmt @ [String s2]))
    fun separate [] sep = []
      | separate [a] sep = [a]
      | separate (a::rest) sep = a :: sep :: (separate rest sep)


    fun pp_list_flat doer objs (left,sep,right,break) = 
      let 
	fun loop [] = [String right]
	  | loop [a] = [doer a, String right]
	  | loop (a::rest) = (doer a) :: (String sep) :: Break :: (loop rest)
	val fmts = (String left) :: (loop objs)
      in fmts
      end
    fun pp_list doer objs (pref as (left,_,_,break)) = 
	let val fmts = pp_list_flat doer objs pref
	in (if break then Vbox0 else HOVbox0 1) (size left) 1 fmts
	end
    val pp_listid = pp_list (fn x => x)

    fun pp_listb' doer objs = pp_list doer objs ("[", ",","]", false)
    fun pp_list' doer objs = pp_list doer objs ("(", ",",")", false)
    fun pp_list'' doer objs = pp_list doer objs ("", ".","", false)

    fun pp_var v = String(var2string v)
    fun pp_label l = String(label2name l)
    fun pp_tag n = String(tag2string n)
    val pp_word = String o TilWord32.toDecimalString
      
    fun primarg2value (arg32(Const_32 v)) = SOME v
      | primarg2value (arg64(Const_64 v)) = SOME v
      | primarg2value _ = NONE

    fun wrapper pp out obj = 
      let 
	val fmtstream = open_fmt out
	val fmt = pp obj
      in (output_fmt (fmtstream,fmt); 
	  close_fmt fmtstream;
	  fmt)
      end



    fun pp_path (v,[]) = pp_var v
      | pp_path (v,labs) = Hbox[pp_var v, String ".", pp_list'' pp_label labs]

    fun pp_labvar (l,v) = [pp_label l, String " > ", pp_var v]

    fun pp_size s = 
      String (case s
		of B1 =>  "8"
		 | B2 =>  "16"
		 | B4 =>  "32"
		 | B8 =>  "64")



    fun pp_bin' doer (obj1,obj2) (left,sep,right) = 
      HOVbox [String left,doer obj1, Break , String sep,Break,doer obj2,String  right]

    fun pp_bin pp_obj connector (obj1,obj2) = pp_bin' pp_obj (obj1,obj2) ("(",connector,")")

    and pp_kind (k : kind) = 
      if !show_sharing then
	if kseen k then String ("(|"^(name_kind k)^"|)")
	else 
	  let val res = HOVbox[String "(",String (name_kind k),String "==",Break,pp_kind' k,String ")"]
	  in ksee k;res
	  end
      else pp_kind' k


    and pp_kind' (k as {k=k_,...} : kind) = 
      let
	fun pp_kind_sugar (k as {k=k_,...} : kind) = 
	  (case Dec.K.ntuple' k
	     of SOME [] => String "1"
	      | SOME ks => pp_list pp_kind ks ("TUP_K{", ",","}", true)
	      | NONE    => 
          (case Dec.K.list' k
	     of SOME k => HOVbox[String "List[",pp_kind k,String "]"]
	      | NONE => pp_kind_ k_)
	     )

	fun pp_kind_tdsugar (k as {k=k_,...} : kind) = 
	  (case Dec.TD.Tmil k
	     of SOME () => String "Tmil"
	      | NONE => 
	  (case Dec.TD.Tmilr k
	     of SOME () => String "Tmilr"
	      | NONE => pp_kind_sugar k))
      in
	if !TDsugar then pp_kind_tdsugar k
	else if !sugar then pp_kind_sugar k
	     else pp_kind_ k_
      end
    and pp_kind_ (kind_ : kind_) = 
      (case kind_ 
	 of T s         => HOVbox[String "T",pp_size s]
	  | Tmem          => String "Tmem"
	  | Unit_k      => String "1"
	  | Nat_k       => String "NAT"
	  | Arrow_k ks  => pp_bin pp_kind "=>" ks
	  | Prod_k ks   => pp_bin pp_kind "x" ks
	  | Sum_k  ks   => pp_list pp_kind ks ("(", "+",")", true)
	  | Var_k j     => pp_var j
	  | Mu_k (j,k)  => HOVbox[String "MU(",
				  pp_var j,
				  String ").",
				  Break0 0 3,
				  pp_kind k]
	  | All_k (j,k) => HOVbox[String "ALL(",
				  pp_var j,
				  String ").",
				  Break0 0 3,
				  pp_kind k])
	 
    and pp_wkdec s k = HOVbox[String s,String "[",pp_kind k, String"]"]
    and pp_vk(v,k) = Hbox[pp_var v, String "::", Break, pp_kind k]
    and pp_vklist (vklist : (var * kind) list) : format = pp_list pp_vk vklist ("",",","",false)
    
    and pp_cbnd (v,k,c) : format = Hbox[pp_var v, String "::", pp_kind k,
					String " = ", Break0 0 5, pp_con c]
      

    and pp_primcon pcon = 
      (case pcon 
	 of Int_c size => Hbox[String "Int", pp_size size]
	  | Float_c => String "Float"
	  | Boxed_c size => Hbox[String "Boxed", pp_size size]
	  | Void_c  => String "Void"
	  | Tuple_c => String "Tuple"
	  | Dyntag_c => String "Dyntag"	 
	  | Array_c size => Hbox[String "Array", pp_size size]
	  | Tag_c => String "Tag"
	  | Sum_c => String "Sum"
	  | KSum_c => String "KSum"
	  | Exists_c => String "Exists" 
	  | Forall_c => String "Forall"
	  | Rec_c => String "Rec"
	  | Arrow_c => String "Arrow"
	  | Code_c => String "Code"
	  | ExternArrow_c s => Hbox[String "ExternArrow",pp_size s]
	  | Coercion_c => String "Coercion")

    and pp_confun code (tFormals,eFormals,fFormals,body_type) =
	HOVbox[if code then String "ALLCODE(" else String "ALLARROW(",
	       (pp_list' (fn (v,k) => Hbox[pp_var v,String " :: ", pp_kind k]) tFormals),
	       String "; ", Break0 0 3,
	       (pp_list' pp_con eFormals),
	       String "; ", 
	       (pp_list' pp_con fFormals),
	       String "; ", Break0 0 3,
	       pp_con body_type,
	       String ")"]
    and pp_tupletype cs = pp_list pp_con cs ("Tup_c(",",",")",true)

    and pp_con c = 
      if !show_sharing andalso not (smallcon c) then
	if cseen c then String ("(|"^(name_con c)^"|)")
	else 
	  let val res = HOVbox[String "(",String (name_con c),String "==",Break,pp_con' c,String ")"]
	  in csee c;res
	  end
      else if !show_frees then
	let 
	  val frees = VarSet.listItems (free_cvars_con c)
	  val freeformat = pp_list pp_var frees ("{",",","}",false)
	  val res = HOVbox[String "(",pp_con' c,Break,String "frees ==",freeformat,String ")"]
	in res
	end
      	   else pp_con' c
    and pp_con' (con as {c,...}) = 
      let
	fun pp_sugar () = 
	  (case Dec.C'.allarrow_ml' con
	     of SOME a => pp_confun false a
	      | NONE => 
	  (case Dec.C'.allcode_ml' con
	     of SOME a => pp_confun true a
	      | NONE => 
	  (case Dec.C'.tuple_ml' con
	     of SOME a => pp_tupletype a
	      | NONE => 
          (case Dec.C'.ntuple' con
	     of SOME [] => String "*"
	      | SOME cs  => pp_list pp_con cs ("CTUP{", ",","}", true)
	      | NONE => 
	  (case Dec.C'.polyprim' con
	     of SOME (p,[],[]) => pp_primcon p
	      | SOME (p,[],cargs) => Hbox[pp_primcon p, Break0 0 2,
					  pp_list' pp_con cargs]
	      | SOME (p,kargs,cargs) => Hbox[pp_primcon p, Break0 0 2,
					     pp_listb' pp_kind kargs, Break,
					     pp_list' pp_con cargs]
	       
	      | NONE => 
	  (case Dec.C'.unfold' con
	     of SOME k => pp_wkdec "Unfold" k
	      | NONE => 
	  (case Dec.C'.list' con
	     of SOME (k,cs) => 
	       let
		 val header = 
		   (case Dec.K.list' k
		      of SOME k => [String "List of ",pp_kind k, String " is "]
		       | NONE => [String "List:",pp_kind k])
	       in
		 HOVbox (header @ [Break0 0 2,String "[",
				   pp_list pp_con cs ("",",","",false), String "]"])
	       end
	      | NONE => pp_con_ c)))))))

	fun pp_tdsugar () = 
	(case Dec.TD.interp con
	   of SOME c => HOVbox[String "interp(",pp_con c,String ")"]
	    | NONE => 
	(case Dec.TD.interpr con
	   of SOME c => HOVbox[String "interpr(",pp_con c,String ")"]
	    | NONE => 
	(case Dec.TD.R con
	   of SOME c => HOVbox[String "R(",pp_con c,String ")"]
	    | NONE => 
	(case Dec.TD.Rtuple con
	   of SOME c => HOVbox[String "Rtuple(",pp_con c,String ")"]
	    | NONE => pp_sugar()))))
      in
	if !TDsugar then pp_tdsugar()
	else if !sugar then pp_sugar()
        else pp_con_ c
      end
    and pp_con_ con_ = 
      (case con_ of
	 Var_c v => pp_var v
(*       | Rcon c => HOVbox[String "R", String "(", pp_con c, String ")"]*)
       | Nat_c w => pp_word w
       | App_c (c1,c2) => HOVbox[String"APP(",pp_con c1, String ",", pp_con c2, String ")"]
       | APP_c (c,k) => HOVbox[pp_con c, String "[", pp_kind k, String "]"]
       | Pi1_c c => HOVbox[String "pi1(",pp_con c,String ")"]
       | Pi2_c c => HOVbox[String "pi2(",pp_con c,String ")"]
       | Prim_c primcon => pp_primcon primcon
       | Pr_c (j,(a,k),k',r,body)  => 
	   HOVbox[String "pr(",pp_var j,String ",",pp_vk (a,k),String ",",
		  pp_var r,String "::(",pp_var j,String "->", pp_kind k',String ")",Break0 0 2,
		  String ".",pp_con body, String ")"]
       | Case_c (c,arms,default) => 
	   let
	     fun pp_arm (w,(v,c)) = HOVbox[String "(",pp_word w,String ",",pp_var v,String ") =>",Break0 0 2,pp_con c]

	     fun pp_default NONE = String ""
	       | pp_default (SOME c) = Hbox[String "( _ ) => ", Break0 0 2, pp_con c]
	   in
	     HOVbox[String "case(",pp_con c,String ")",
		    Break0 0 2,
		    (pp_list pp_arm arms ("","","", true)),
		    Break0 0 2,
		    pp_default default]
	   end
       | LAM_c (j,c) => 
	   HOVbox[String "LAM(",
		  Space,pp_var j,
		  String ")",
		  Break0 0 2,
		  String " = ",
		  pp_con c]
       | Lam_c (vk,c) => 
	   HOVbox[String "Lam(",
		  pp_vk vk,
		  String ")",
		  Break0 0 2,
		  String " = ",
		  pp_con c]
       | Pair_c (c1,c2) => pp_bin' pp_con (c1,c2) ("<",",",">")
       | Star_c  => String "Unit"
       | Inj_c (i,k,c) => HOVbox[String "Inj_",pp_word i,String"[",pp_kind k, String"]",Break, String "(",pp_con c,String ")"]
       | Fold_c (k,c) => HOVbox[String "Fold[",pp_kind k, String"]",Break, String "(",pp_con c,String ")"]
       | Ptr_c c => Hbox [String "Ptr(",pp_con c,String ")"])

    and pp_ctag q = 
      (case q
	 of Roll => String "Roll"
	  | Unroll => String "Unroll"
	  | Pack => String "Pack"
	  | ForgetKnown => String "Forgetknown"
	  | ProjKnown => String "ProjKnown"
	  | InjUnion => String "InjUnion"
	  | InjForget => String "InjForget")

    and pp_coercion (q,args) = 
      (case (q,args)
	 of (Pack,[cas,chiding]) => 
	   HOVbox[String "Pack[",Break,
		  String "as",Break,pp_con cas,Break,
		  String "hiding",Break,pp_con chiding,String"]"]
	  | (q,[c]) =>
	   HOVbox[pp_ctag q,String"[",pp_con c,String"]"]
	  | _ =>
	   HOVbox[pp_ctag q,pp_list pp_con args ("[", ",","]", false)])

    and pp_primarg arg = 
      (case arg
	 of arg32 sv32 => Hbox[String "32(",pp_sv32 sv32,String ")"]
	  | arg64 sv64 => Hbox[String "64(",pp_sv64 sv64,String ")"])
    and pp_sv64 sv = 
      (case sv 
	 of Var_64 v => pp_var v
	  | Const_64 v => Ppprim.pp_value' primarg2value pp_primarg pp_con v)
    and pp_sv32 sv = 
      case sv
	of Var_32 v => pp_var v
	 | Label l => pp_label l
	 | Coercion q => pp_coercion q
	 | Coerce (q,sv) => Hbox[pp_sv32 q,String "@",pp_sv32 sv]
	 | Tabs (vk,sv) =>  Hbox[String "/\\", pp_vk vk, String ".", pp_sv32 sv]
	 | TApp (sv,c) =>Hbox[pp_sv32 sv, String "[", pp_con c, String "]"]
	 | Const_32 v => Ppprim.pp_value' primarg2value pp_primarg pp_con v
	 | Tag w => Hbox[String "tag(",pp_word w,String ")"]
	 | Unit => String "uval"
    and pp_op64 oper = 
      (case oper
	 of Val_64 sv64 => pp_sv64 sv64
	  | Unbox sv32 => Hbox[String "unbox",Break,pp_sv32 sv32]
	  | ExternAppf (sv32,args,fargs) => 
	   (pp_region ("ExternAppf_(") ")"
	    [pp_sv32 sv32, String ", ", Break,
	     pp_list pp_sv32 args ("",",","",false), Break,
	     pp_list pp_sv64 fargs ("",",","",false)])
	  | Prim64 (p,args) =>
	   let
	     val p = pp_prim' p
	     val args = pp_list pp_primarg args ("(",",",")",false)
	   in HOVbox[p,args]
	   end)

    and pp_lilprimop32 pop = 
      (case pop
	 of Box => String "box"
	  | Tuple => String "tuple"
	  | Select w => Hbox[String "select_",pp_word w]
	  | Dyntag => String "dyntag")
    and pp_op32 oper = 
      (case oper
	 of Val sv32 => pp_sv32 sv32
	  | Prim32 (p,cons,args) =>
	   let
	     val p = pp_prim' p
	     val cons = pp_list pp_con cons ("[",",","]",false)
	     val args = pp_list pp_primarg args ("(",",",")",false)
	   in HOVbox[p,cons,args]
	   end
	  | LilPrimOp32 (lp,cons,sv32s,sv64s) =>
	   (case (lp,cons,sv32s,sv64s)
	      of (Box,[],[],[sv64]) => Hbox[String "box(",pp_sv64 sv64,String ")"]
	       | (Tuple,[],sv32s,[]) => pp_list pp_sv32 sv32s ("<",",",">",false)
	       | (Select w,[],[sv32],[]) => Hbox[String "select_",pp_word w,Break,pp_sv32 sv32]
	       | (Dyntag,[c],[],[]) => HOVbox[String "dyntag_",pp_con c]
	       | _ => 
		let
		  val _ = warn "lilprim got unexpected args"
		  val p = pp_lilprimop32 lp
		  val cons = pp_list pp_con cons ("[",",","]",false)
		  val sv32s = pp_list pp_sv32 sv32s ("(",",",")",false)
		  val sv64s = pp_list pp_sv64 sv64s ("(",",",")",false)
		in HOVbox[p,cons,sv32s,sv64s]
		end)
	  | ExternApp (sv32,args,fargs) => 
	      (pp_region ("ExternApp_(") ")"
	       [pp_sv32 sv32, String ", ", Break,
		pp_list pp_sv32 args ("",",","",false), Break,
		pp_list pp_sv64 fargs ("",",","",false)])
	  | App (f,sv32s,sv64s) =>
	      HOVbox[String "App(",pp_sv32 f, String ";", Break,
		     pp_list pp_sv32 sv32s ("",",","",false), 
		     String "; ",
		     pp_list pp_sv64 sv64s (" ",",","",false), 
		     String ")"]
	  | Call (f,sv32s,sv64s) =>
	      HOVbox[String "Call(",pp_sv32 f, String ";", Break,
		     pp_list pp_sv32 sv32s ("",",","",false), 
		     String "; ",
		     pp_list pp_sv64 sv64s (" ",",","",false), 
		     String ")"]
	  | Switch sw => pp_switch sw
	  | Raise (c,sv32) => pp_region "Raise(" ")" [pp_sv32 sv32, String ",", pp_con c]
	  | Handle (t,e1,(v,e2))=>
	      Vbox[HOVbox[String "Handle[", pp_con t,
			     String "] ", Break0 0 3,pp_exp e1],
		   Break0 0 0,
		   HOVbox[String "With ", pp_var v,
			  String ": EXN = ", Break0 0 3,
			  pp_exp e2]])
    and pp_exp_ e = 
      (case e 
	 of Val32_e sv32 => Hbox[String "Val(",pp_sv32 sv32,String ")"]
	  | Let_e (bnds,e) => 
	   Vbox0 0 1 [String "Let  ",Break0 0 2,
		      Vbox (separate (map pp_bnd bnds) (Break0 0 0)),
		      Break,
		      String "In   ",
		      pp_exp e,
		      Break,
		      String "End"])
    and pp_exp {e} = pp_exp_ e
    and pp_bnd b =
      (case b
	 of Fixcode_b vfs => Vbox(separate (map pp_fix vfs) Break)
	  | Exp32_b (v,oper) => HOVbox[pp_var v, String " = ", Break0 0 2, pp_op32 oper]
	  | Exp64_b (v,oper) => HOVbox[pp_var v, String " =f= ", Break0 0 2, pp_op64 oper]
	  | Unpack_b (a,x,sv32) => 
	   HOVbox[String "[",pp_var a,String ",",pp_var x,String"] = ", Break0 0 2, pp_sv32 sv32]
	  | Split_b (a1,a2,c) =>
	   HOVbox[String "<",pp_var a1,String ",",pp_var a2,String"> = ",Break0 0 2, pp_con c]
	  | Unfold_b (a,c) => HOVbox[String "fold ",pp_var a, String " = ", Break0 0 2, pp_con c]
	  | Inj_b (w,a,arg,sv) =>
	      HOVbox[String "Inj_", pp_word w, Break, pp_var a, Break,String "=", 
		     Break0 0 2,
		     HOVbox [String "(",pp_con arg,String ",",Break,pp_sv32 sv,String ")"]])
    and pp_fix (v,f) : format = 
      let 
      in
	HOVbox([String "/\\ ",
		pp_var v, Break0 0 2,
		pp_function f])
      end
    and pp_function (Function{tFormals, eFormals, fFormals,body,rtype}) : format =
      let 
	val vkformats = (pp_list_flat pp_vk
			 tFormals ("",",","",false))
	val vcformats = (pp_list_flat (fn (v,c) =>
				       HOVbox[pp_var v,
					      String " : ", Break0 0 2, pp_con c])
			 eFormals ("",",","",false))
	val vfformats = (pp_list_flat (fn (v,c) => 
				       HOVbox[pp_var v, 
					      String " : ", Break0 0 2, pp_con c])
			 fFormals ("",",","",false))
      in
	Vbox [pp_region "(" ")"
	      [HOVbox(vkformats @
		      (if (null vkformats)
			 then [String " ;; "] else
			   [String " ;; ", Break0 0 0]) @
			 vcformats @
			 (if (null vfformats)
			    then [String " ;; "]
			  else [String " ;; ", Break0 0 0] @ vfformats))],
	      String " : ", pp_con rtype,
	      Break0 0 0,
	      String " =", Break,
	      pp_exp body]
      end

    and pp_switch sw =
      let fun pp_default NONE = String "NODEFAULT"
	    | pp_default (SOME e) = Hbox[String "DEFAULT = ", Break0 0 2, pp_exp e]
      in
	case sw of
	  Intcase {arg,arms,default,rtype} => 
	    HOVbox[String "INT_SWITCH(", 
		   pp_sv32 arg, String ", ",
		   Break0 0 3,
		   (pp_list (fn (w,e) => HOVbox[pp_word w, String "=> ", Break0 0 2, pp_exp e])
		    arms ("","","", true)),
		   Break0 1 3,
		   pp_exp default,
		   String ")",String" : ",pp_con rtype]
	| Sumcase {arg,arms,default,rtype} => 
	    HOVbox[String "SUM_SWITCH(", 
		   pp_sv32 arg, String ", ",
		   Break0 0 3,
		   (pp_list (fn (w,v,e) => 
			     HOVbox[String "(",pp_word w, String ",", pp_var v,String ")=> ",
				    Break0 0 2, pp_exp e])
		    arms ("","","", true)),
		   Break0 0 3,
		   pp_default default,
		   String ")",String" : ",pp_con rtype]
	| Dyncase {arg,arms,default,rtype} => 
	    HOVbox[String "EXN_SWITCH(", 
		   pp_sv32 arg, String ": EXN, ",
		   Break0 0 3,
		   (pp_list (fn (t,(v,c),e) => 
			     HOVbox[pp_sv32 t, 
				    String "(",pp_var v,String ":",  pp_con c, String ")",
				    String "=> ", Break0 0 2, pp_exp e])
		    arms ("","","", true)),
		   Break0 0 3,
		   pp_exp default,
		   String ")",String" : ",pp_con rtype]
	| Ifthenelse {arg, rtype, thenArm, elseArm} =>
	    HOVbox[String "IF (", 
		   pp_conditionCode arg,
		   String ") ",
		   Break0 0 3,
		   String "THEN ", pp_exp thenArm, 
		   String " ",
		   Break0 0 3,
		   String "ELSE ", pp_exp elseArm, 
		   String " ",
		   Break0 0 3,
		   String" : ", pp_con rtype]
      end

    and pp_conditionCode cc = 
      (case cc of
	 Exp_cc exp => pp_exp exp
       | And_cc (cc1,cc2) => HOVbox[String "AND(", pp_conditionCode cc1, String ", ", pp_conditionCode cc2, String ")"]
       | Or_cc  (cc1,cc2) => HOVbox[String "OR(", pp_conditionCode cc1, String ", ", pp_conditionCode cc2, String ")"]
       | Not_cc  cc => HOVbox[String "NOT(", pp_conditionCode cc, String ")"])
	 

    fun pp_bnds bnds = pp_list pp_bnd bnds ("[",",","]",true)
    fun pp_data d = 
      let
	fun pp_char sv = 
	  (case sv
	     of Const_32 (Prim.uint(W8,c)) => 
	       let val c = chr(TilWord64.toInt c)
	       in  if (c = #"\n") then String ("\\n") else String (Char.toString c)
	       end
	      | _ => error "bad vector value: corrupt string")
		
      in 
      (case d
	 of Dboxed (l,sv) => HOVbox[pp_label l,String " = ",Break0 0 2,pp_sv64 sv]
	  | Dtuple (l,c,qopt,svs) => 
	   HOVbox[pp_label l, String ":",Break0 0 2,pp_con c,String " = ",Break0 0 2,
		  (case qopt of SOME sv => pp_sv32 sv | NONE => String ""),Break,
		     String "@",Break,
		     pp_list pp_sv32 svs ("<",",",">",false)]

	  | Darray (l,sz,c,svs) => 
	   HOVbox[pp_label l, String ":",Break0 0 2,pp_con c,String " = ",Break0 0 2,
		     pp_list pp_char svs ("\"","","\"",false)]

	  | Dcode (l,f) => HOVbox[pp_label l,String " = ",Break0 0 2,pp_function f])
      end
    fun pp_datalist ds = pp_list pp_data ds ("",",","",true)

    fun pp_module (MODULE{timports,data,confun,expfun}) = 
      Vbox0 0 1 [
		 String "TIMPORTS:", Break0 0 2,
		 pp_list pp_vk timports ("",",","",true), Break0 0 2,
		 String "DATA:", Break0 0 2,
		 pp_datalist data, Break,
		 String "CONFUN:", Break0 0 2,
		 pp_con confun, Break,
		 String "EXPFUN:", Break0 0 2,
		 pp_exp expfun,Break
		 ]

    fun help pp obj = 
      (let
	 val () = reset ()
	 val res = pp obj
	 val () = reset()
       in res
       end handle any => (reset();raise any))

    fun help' pp obj = (reset();wrapper pp TextIO.stdOut obj; reset();()) 
      handle any => (reset();raise any)
      
    val pp_list' = pp_list
    val pp_var' = help pp_var
    val pp_label'  = help pp_label
    val pp_con' = help pp_con
    val pp_kind' = help pp_kind
    val pp_bnd' = help pp_bnd
    val pp_bnds' = help pp_bnds
    val pp_exp' = help pp_exp
    val pp_sv32' = help pp_sv32
    val pp_sv64' = help pp_sv64
    val pp_op32' = help pp_op32
    val pp_op64' = help pp_op64
    val pp_module' = help pp_module

    val pp_var = help' pp_var
    val pp_label  = help' pp_label
    val pp_con = help' pp_con
    val pp_kind = help' pp_kind
    fun pp_list doer data = help' (pp_list' doer data)
    val pp_bnd = help' pp_bnd
    val pp_bnds = help' pp_bnds
    val pp_exp = help' pp_exp
    val pp_sv32 = help' pp_sv32
    val pp_sv64 = help' pp_sv64
    val pp_op32 = help' pp_op32
    val pp_op64 = help' pp_op64
    val pp_module = help' pp_module
    val pp_pass = 
      fn {module, name:string, pass:string, header:string} =>
      (print "PASS: "; print pass; print "\n";
       print header; print "\n\n";
       pp_module module)
      
  end
