(*$import Prelude *)

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

    type size = int 

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
      
    datatype primcon =                        (* classifies term-level ... *)
      Int_c of size                           (* register integers *)
    | Float_c of size                         (* register floating-points *)
    | Boxed_c of size                         (* boxed values *)
    | Void_c
    | Star_c
    | Tuple_c
    | Dyn_c                                   (* exceptions *)
    | Dyntag_c
    | Array_c of size                         (* arrays *)
    | Vector_c of size                        (* vectors *)
    | Tag_c
    | GCTag_c
    | Sum_c 
    | KSum_c
    | Exists_c
    | Forall_c
    | Rec_c
    | Arrow_c

    datatype scon_ = 
      Var_c of var
    | Nat_c of int
    | App_c of scon * acon
    | APP_c of scon * kind
    | Pi1_c of scon
    | Pi2_c of scon
    | Prim_c of primcon
    | Pr_c of var * (var * kind) * kind * var * scon
    | Case_c of scon * (var * acon) * (var * acon)
    | AtoS_c of acon * kind

    and acon_      = 
      LAM_c of var * acon
      | Lam_c of var * acon
      | Pair_c of acon * acon
      | Inr_c of con 
      | Inl_c of con 
      | Fold_c of con
      | StoA_c of scon

    withtype cbnd = (var * kind * acon)
    and scon = {c : scon_ }
    and acon = {c : acon_ }

    datatype sv64 = Var_64 of var | Const_64 of (con,exp) Prim.value
    and sv32 = 
      Var of var 
      | Const of (con,exp) Prim.value
    and v32 = 
      Box of sv64
      | Tuple of sv32 list
      | Inj_tag of int * ( int * con) * sv32
      | Inj of int * (int * con) * sv32
      | Inj_dyn of con * sv32 * sv32
      | Roll of con * sv32
      | Pack of sv32 * con * con
      | TApp of sv32 * con
      | ForgetKnown of sv32
      | Tag of int
    and op64 = Val_64 of sv64 | Sub_64 of con * sv32 * sv32
    and op32 = 
      Val of sv32 
      | Unroll of con * sv32
      | Select of int * sv32
      | Case of sv32 * (var * exp) list
      | Proj of int * sv32
      | Dyncase of sv32 * (sv32 * exp) list * exp
      | Dyntag of con
      | Raise of con * sv32
      | Handle of con * sv32 * (var * sv32)
      | Unbox of sv32
      | App of sv32 * sv32 list * sv64 list
      | Vcase of con * (con * (var * sv32) * (var * exp))
      | Array_32 of sv32 * sv32 
      | Array_64 of sv64 * sv64
      | Sub of sv32 * sv32
      | Upd_32 of sv32 * sv32 * sv32
      | Upd_64 of sv32 * sv32 * sv64
    and exp_ = 
      Val_e of sv32
      | Let_e of bnd list * exp
    and bnd = 
      Fixcode_b of (var * con) list  * function list
      | Exp32_b of var * op32
      | Exp64_b of var * op64
      | Unpack_b of var * var * sv32
      | Split_b of var * var * con
      | Unfold_b of var * con
    and function = Function of {effect      : effect,
				recursive   : recursive,
				tFormals    : var list,
				eFormals    : var list,
				fFormals    : var list,
				body        : exp}
      
    withtype exp = { e: exp_ }

    datatype import_entry = 
      ImportValue of label * var * con
      | ImportType  of label * var * kind
      | ImportBnd of var * con
      
    datatype export_entry = 
      ExportValue of label * var
      | ExportType  of label * var
      
    datatype module = MODULE of {bnds : bnd list,
			         imports : import_entry list,
				 exports : export_entry list}

end