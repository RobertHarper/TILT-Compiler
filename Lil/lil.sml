(*$import LIL *)

(* The main LIL datatype
 * Leaf Petersen 
 *)

structure Lil :> LIL = 
  struct

    type var = Name.var
    type label = Name.label
      
    type w32 = Word32.word
    type prim = Prim.prim

    datatype size = B1 | B2 | B4 | B8

    val flattenThreshold : int ref = ref 6


    (*  A leaf procedure makes no function calls.
     *  A non-recursive function may not directly call itself or functions
     *    bound in the same cluster.  This means that the functions bound
     *    are not (mutually recursive) and can thus be inlined.
     *  An arbitrary function may call itself or other functions.
     *)

    datatype recursive = Leaf | NonRecursive | Arbitrary

    datatype kind_ = 
      T of size
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

    datatype con_ = 
      Star_c
    | Var_c of var
    | Nat_c of int
    | Lam_c of (var * kind) * con
    | App_c of con * con
    | LAM_c of var * con
    | APP_c of con * kind
    | Pair_c of con * con
    | Proj_c of con * int
    | Inj_c of kind * con * int
    | Case_c of con * kind * (var * con) * (var * con)
    | Fold_c of kind * con
    | Pr_c of var * (var * kind) * kind * var * con
    | Prim_c of primcon * con list

    withtype con = {c : con_ }

    datatype v64 = Var_64 of var | Const_64 of (con,exp) Prim.value
    and v32 = 
      Var of var 
      | Const of (con,exp) Prim.value
      | Box of v64
      | Tuple of v32 list
      | Inj_tag of int * ( int * con) * v32
      | Inj of int * (int * con) * v32
      | Inj_dyn of con * v32 * v32
      | Roll of con * v32
      | Pack of v32 * con * con
      | TApp of v32 * con
      | ForgetKnown of v32
      | Tag of int
    and op64 = Val_64 of v64 | Sub_64 of con * v32 * v32
    and op32 = 
      Val of v32 
      | Unroll of con * v32
      | Select of int * v32
      | Case of v32 * (var * exp) list
      | Proj of int * v32
      | Dyncase of v32 * (v32 * exp) list * exp
      | Dyntag of con
      | Raise of con * v32
      | Handle of con * v32 * (var * v32)
      | Unbox of v32
      | App of v32 * v32 list * v64 list
      | Vcase of con * (con * (var * v32) * (var * exp))
      | Array_32 of v32 * v32 
      | Array_64 of v64 * v64
      | Sub of v32 * v32
      | Upd_32 of v32 * v32 * v32
      | Upd_64 of v32 * v32 * v64
    and exp_ = 
      Val_e of v32
      | Let_e of bnd list * exp
    and bnd = 
      Code_b of var * (var * kind) list * (var * con) list * (var * con) list * exp
      | Exp32_b of var * op32
      | Exp64_b of var * op64
      | Unpack_b of var * var * v32
      | Split_b of var * var * con
      | Unfold_b of var * con
    withtype exp = { e: exp_ }


    datatype import_entry = ImportValue of label * var * con
                          | ImportType  of label * var * kind

    datatype export_entry = ExportValue of label * var
                          | ExportType  of label * var

    datatype module = MODULE of {bnds : bnd list,
			         imports : import_entry list,
				 exports : export_entry list}

end
