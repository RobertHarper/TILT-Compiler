(* The main LIL datatype
 * Leaf Petersen
 *)

signature LIL =
  sig

    type uid    
    type var = Name.var
    type label = Name.label
      
    type w32 = Word32.word
    type prim = Prim.prim

    datatype size = B1 | B2 | B4 | B8


    val flattenThreshold : int ref


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

    withtype kind = { k : kind_ , id : uid}
      
    datatype primcon =                             (* classifies term-level ... *)
      Int_c of size                           (* register integers *)
    | Float_c                          (* register floating-points *)
    | Boxed_c of size                         (* boxed values *)
    | Embed_c of size
    | Void_c
    | Tuple_c
    | Dyntag_c
    | Array_c of size                         (* arrays *)
    | Ref_c
    | Tag_c
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

    withtype con = {c : con_,id : uid,whnf : con_ option ref, cvars : Name.VarSet.set} 

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
      | Intcase of {arg : sv32,arms :(w32 * exp) list, size : size,   default: exp,        rtype : con}
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
      | Dtuple of label * con * sv32 option * sv32 list  (* l : c = sv @ <svs> *)
      | Dcode of label * function
      

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
    datatype module = MODULE of {timports : (var * kind) list,
				 data   : data list,
				 confun : con,
				 expfun : exp}

    val mk_kind : kind_ -> kind
    val eq_kind : kind * kind -> bool
    val cmp_kind : kind * kind -> order

    val mk_con  : con_  -> con
    val mk_exp  : exp_  -> exp
    val mk_pcon     : primcon -> con

    val free_cvars_con : con -> Name.VarSet.set
    val eq_con : con * con -> bool
    val cmp_con : con * con -> order
    val set_whnf : con * con -> unit
    val name_con : con -> string
    val name_kind : kind -> string

    structure ConMap : ORD_MAP where type Key.ord_key = con
    structure ConSet : ORD_SET where type Key.ord_key = con
    structure KindMap : ORD_MAP where type Key.ord_key = kind
    structure KindSet : ORD_SET where type Key.ord_key = kind

    val reset_tables : unit -> unit

    val report : unit -> {csize : int,
			  ksize : int,
			  cbuckets : int list,
			  kbuckets : int list,
			  collisions : (Word.word * con_ list) list}
end

