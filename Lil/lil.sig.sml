(* The main LIL datatype
 * Leaf Petersen
 *)

signature LIL =
  sig

    type var = Name.var
    type label = Name.label

    type w32 = Word32.word
    type prim = Prim.prim

    datatype size = B1 | B2 | B4 | B8


    val flattenThreshold : int ref



    (*  A leaf procedure makes no function calls.
     *  A non-recursive function may not directly call itself or functions
     *    bound in the same cluster.  This means that the functions bound
     *    are not (mutually recursive) and can thus be inlined.
     *  An arbitrary function may call itself or other functions.
     *)

    datatype recursive = Leaf | NonRecursive | Arbitrary

    datatype kind_ =
      T of size
    | TD
    | TU
    | Unit_k
    | Nat_k
    | Arrow_k of kind * kind
    | Prod_k of kind * kind
    | Sum_k of kind * kind
    | Var_k of var
    | Mu_k of var * kind
    | All_k of var * kind

    withtype kind = { k : kind_ }
      

    datatype primcon =                             (* classifies term-level ... *)
      Int_c of size                           (* register integers *)
    | Float_c                          (* register floating-points *)
    | Boxed_c of size                         (* boxed values *)
    | Void_c
    | Star_c
    | Tuple_c
    | Dyn_c                                   (* exceptions *)
    | Dyntag_c
    | Array_c of size                         (* arrays *)
    | Vector_c of size                        (* vectors *)
    | Tag_c
    | Sum_c 
    | KSum_c
    | Exists_c
    | Forall_c
    | Rec_c
    | Arrow_c
    | ExternArrow_c 
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
    | Case_c of kind * con * ((var * kind) * con) * ((var * kind) * con)
    | LAM_c of var * con
    | Lam_c of (var * kind) * con
    | Pair_c of con * con
    | Inr_c of kind * con 
    | Inl_c of kind * con 
    | Fold_c of kind * con

    withtype con = {c : con_}

    datatype coercion = 
      Roll 
    | Unroll 
    | Pack 
    | ForgetKnown 
    | InjUnion

    and primarg = arg32 of sv32 | arg64 of sv64
    and sv64 = 
      Var_64 of var 
      | Const_64 of (con,sv64) Prim.value
    and sv32 = 
      Var_32 of var 
      | Coercion of coercion * con list  (* Coercion and decorations*)
      | Coerce of sv32 * sv32 
      | Tabs of (var * kind) * sv32
      | TApp of sv32 * con
      | Const_32 of (con,sv32) Prim.value
    and op64 = 
      Val_64 of sv64 
      | Unbox of sv32
      | Prim64 of Prim.prim * con list * primarg list
    and lilprimop32 = 
      Box 			(*  sv64 *)
      | Tuple 			(*  sv32 list *)
      | Inj_dyn 		(*  con * sv32 * sv32 *)
      | Tag of w32		(*  w32 *)
      | Select of w32		(*  w32 * sv32 *)
      | Proj of w32		(*  w32 * sv32 *)
      | Dyntag 			(*  con *)
    and op32 = 
      Val of sv32 
      | Prim32 of Prim.prim * con list * primarg list
      | LilPrimOp32 of lilprimop32 * con list * sv32 list * sv64 list
      | ExternApp of sv32 * sv32 list
      | App of sv32 * sv32 list * sv64 list
      | Switch of switch
      | Raise of con * sv32 
      | Handle of exp * (var * exp)
      | Vcasel of con * (con * ((var * kind) * exp)  * ((var * kind) * sv32))
      | Vcaser of con * (con * ((var * kind) * sv32) * ((var * kind) * exp))
    and exp_ = 
      Val32_e of sv32
      | Let_e of bnd list * exp
    and bnd =
      Fixcode_b of (var * con) list  * function list
      | Exp32_b of (var * con) * op32
      | Exp64_b of (var * con) * op64
      | Unpack_b of (var * kind) * (var * con) * sv32
      | Split_b of (var * kind) * (var * kind) * con
      | Unfold_b of (var * kind) * con
    and function = Function of {tFormals    : (var * kind) list,
				eFormals    : (var * con) list,
				fFormals    : (var * con) list,
				rtype       : con,
				body        : exp}
    and switch = 
      Sumcase of   {arg : sv32,arms :(w32  * (var * con) * exp) list, default: exp option, tipe : con}
      | Dyncase of {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        tipe : con}
      | Intcase of {arg : sv32,arms :(w32 * exp) list,        default: exp,        tipe : con}
      
    withtype exp = { e: exp_ }

    datatype import_entry =
      ImportValue of label * var * con
      | ImportType  of label * var * kind

      
    datatype export_entry = 
      ExportValue of label * var
      | ExportType  of label * var

    datatype module = MODULE of {bnds : bnd list,
			         imports : import_entry list,
				 exports : export_entry list}


    val mk_kind : kind_ -> kind
    val mk_con  : con_  -> con
    val mk_exp  : exp_  -> exp

    val mk_pcon     : primcon -> con

end

