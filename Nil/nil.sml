structure Nil :> NIL =
struct

  open Util Name Listops

  val error = fn s => error "nil.sml" s

  val flattenThreshold = ref 6

  type var = Name.var
  type label = Name.label

  type w32 = Word32.word
  type prim = Prim.prim

  type ('a,'b) sequence = ('a * 'b) list



  (* In general, we want to distinguish between functions/arrow types that
   * are open (possibly having free variables) or those that are closed.
   *)
  datatype openness = Open | Closure | Code

  (* In addition, we would like to know if application of an arrow object
   * is total(i.e. effect-free) or partial (i.e. not necessarily effect-free)
   * In the total case, we are then guaranteed that the computation always
   * terminates and that, when given the same input, will yield the same output.
   *)
  datatype effect = Total | Partial

  datatype recursive = Leaf | NonRecursive | Arbitrary

  (* Trace annotations are inserted by the reifier and used by tortl.
   *)
  datatype niltrace = TraceUnknown
                    | TraceKnown of TraceInfo.traceinfo
                    | TraceCompute of var

  datatype letsort = Sequential | Parallel
  datatype phase = Runtime | Compiletime
  datatype kind =
      Type_k                        (* constructors that are types *)
    | SingleType_k of con           (* singleton-kind at kind type *)
    | Single_k of con               (* singleton-kind at any kind *)
                                    (* dependent record kind *)
    | Record_k of ((label*var),kind) sequence
                                    (* dependent arrow kinds for open
				       constr funs, closed funs, or closures *)
    | Arrow_k of openness * (var * kind) list * kind

  and primcon =                          (* classifies term-level ... *)
      Int_c of Prim.intsize                   (* register integers *)
      (* Params: None *)
    | Float_c of Prim.floatsize               (* register floating-points *)
      (* Params: None *)
    | BoxFloat_c of Prim.floatsize            (* boxed floating-points *)
      (* Params: None *)
    | Exn_c                                   (* exceptions *)
      (* Params: None *)
    | Array_c                                 (* arrays *)
      (* Params: Element con *)
    | Vector_c                                (* vectors *)
      (* Params: Element con *)
    | IntArray_c of Prim.intsize
      (* Params: None *)
    | IntVector_c of Prim.intsize
      (* Params: None *)
    | FloatArray_c of Prim.floatsize
      (* Params: None *)
    | FloatVector_c of Prim.floatsize
      (* Params: None *)
    | Ref_c 
      (* Params: Element con *)
    | Loc_c                                   (* locatives *)
      (* Params: None *)
    | Exntag_c                                (* exception tags *)
      (* Params: Carried con *)
    | Record_c of label list                  (* records *)
      (* Params: Cons to inject *)
    | Sum_c of {tagcount : w32,
		totalcount : w32,
                known : w32 option}           (* sum types *)
      (* Params: Con to inject *)
    | Vararg_c of openness * effect           (* classifies make_vararg and make_onearg *)
      (* Params: Arg con, result con *)
    | GCTag_c                                 (* Type of header word for heap values *)
      (* Params: Con to tag *)

  and con =
      Prim_c of primcon * con list                (* Classify term-level values
                                                       of primitive types *)
    | Mu_c of bool * (var,con) sequence           (* Constructors that classify values of
						       a recursive type *)
    | AllArrow_c of {openness : openness, (* open functions, code functions, and closures *)
		     effect : effect,
		     tFormals : (var * kind) list,
		     eFormals : con list,
		     fFormals : w32,
		     body_type : con}
    | ExternArrow_c of con list * con
    | Var_c of var
    | Let_c of letsort * conbnd list * con        (* Constructor-level bindings *)
    | Crecord_c of (label * con) list             (* Constructor-level records *)
    | Proj_c of con * label                       (* Constructor-level record projection *)
    | Closure_c of con * con                      (* Constructor-level closure:
					               code and environment *)
    | App_c of con * con list                     (* Constructor-level application
						       of open or closed constructor function *)
    | Coercion_c of {vars : var list,         (* Coercions: *)
		     from : con,              (* forall[vars].from=>to *)
		     to : con}

  and conbnd = Con_cb  of (var * con)
             | Open_cb of (var * (var * kind) list * con)
             | Code_cb of (var * (var * kind) list * con)

  and nilprim =
      record of label list       (* record intro *)
      (* Params: Terms to inject *)
    | partialRecord of label list * int (* record with a missing zero-indexed field *)
      (* Params: Terms to inject, minus missing term *)
    | select of label            (* record field selection; takes the record type *)
      (* Params: Record from which to select *)
    | inject of TilWord32.word   (* slow; must be given one type that is
				    reducible to a sum type *)
      (* Params: Sum con and term to inject *)
    | inject_known of TilWord32.word   (* fast; the injected field is a non-carrier or else
                                                its type is reducible to HNF *)
      (* Params: Term to inject and sum con *)

    | project of TilWord32.word  (* corresponds to inject *)
      (* Params: Known sum con and known sum term *)
    | project_known of TilWord32.word  (* corresponds to inject_known *)
      (* Params: Known sum term *)


    | box_float of Prim.floatsize   (* boxing floating-points *)
      (* Params: Float term to box *)
    | unbox_float of Prim.floatsize (* unboxing floating-points *)
      (* Params: BoxFloat term to unbox *)

    | make_exntag                (* generate new exn tag *)
      (* Params: Carried con *)
    | inj_exn of string          (* takes tag + value and give exn *)
      (* Params: Tag and term to inject *)
    | make_vararg of openness * effect  (* given a function in onearg calling conv, convert to vararg *)
      (* Params: Normal term function *)
    | make_onearg of openness * effect  (* given a function in vararg calling conv, convert to onearg *)
      (* Params: Vararg term function *)
    | mk_record_gctag       (* Create the gc tag for a record *)
      (* Params: Record con *)
    | mk_sum_known_gctag    (* Create the gc tag for a sum with a statically known carrier type *)
      (* Params: Sum con *)

  and allprim = NilPrimOp of nilprim
              | PrimOp of prim

  (* Intswitch should be apparent.
   * The con list in the Sumswitch tells us the sum type and the w32
   *    is used to index the different cases for the sum type.
   * Exncase's arms are indexed by expressions that must have type Exntag_c(c) and
   *    the arms must have type tau for some fixed result type tau, assuming bound has type c
   *)
  and switch =                                 (* Switching on / Elim Form *)
      Intsw_e of {arg  : exp,
		  size : Prim.intsize,
		  result_type : con,
		  arms : (w32 * exp) list,
		  default : exp option}             (* integers *)
    | Sumsw_e of {arg : exp,
		  sumtype : con,
		  result_type : con,
		  bound : var,
		  arms : (w32 * niltrace * exp) list,  (*Always Trace, see above*)
		  default : exp option}             (* sum types *)
    | Exncase_e of {arg : exp,
		    result_type : con,
		    bound : var,
		    arms : (exp * niltrace * exp) list,
		    default : exp option}           (* exceptions *)
    | Typecase_e of {arg : con,
		     arms : (primcon * (var * kind) list * exp) list,
		     default : exp,
		     result_type: con }                 (* typecase *)
    | Ifthenelse_e of {arg : conditionCode,
		       thenArm : exp,
		       elseArm : exp,
		       result_type : con}


  and exp =                                          (* Term-level constructs *)
      Var_e of var                                        (* Term-level variables *)
    | Const_e of (con,exp) Prim.value                     (* Term-level constants *)
    | Let_e of letsort * bnd list * exp                   (* Binding construct *)
    | Prim_e of allprim * (niltrace list) * (con list) * (exp list)
                                                          (* primops must be fully applied *)
    | Switch_e of switch                                  (* Switch statements *)
    | App_e of openness * exp * (con list) *
                       exp list * exp list     (* Application of open functions and closures *)
    | ExternApp_e of exp * exp list
    | Raise_e of exp * con
    | Handle_e of {body :exp,
	           bound : var,
                   handler : exp,
                   result_type : con}
    | ForgetKnown_e of con * w32                       (* Coercion mapping known sum to unknown sum *)
    | Fold_e of var list * con * con           (* Fold_e and Unfold_e are coercions*)
    | Unfold_e of var list * con * con
    | Coerce_e of exp * (con list) * exp       (* Coerce_e applies a coercion *)

  and conditionCode =                          (* Used by Ifthenelse_e *)
      Exp_cc of exp
    | And_cc  of conditionCode * conditionCode (* Short-circuiting *)
    | Or_cc   of conditionCode * conditionCode (* Short-circuiting *)
    | Not_cc  of conditionCode

  (* result types are needed for recursive definitions in order to make
   * type-checking syntax directed.  Also note that I've forced all functions
   * to be named -- that is, they do not appear as exp forms.  We need
   * recursive expressions involving records and closures in order to
   * closure-convert recursive functions.
   *)

  and bnd =                                (* Term-level Bindings with optional classifiers *)
      Con_b of phase * conbnd                (* Binds constructors *)
    | Exp_b of var * niltrace * exp          (* Binds expressions *)
    | Fixopen_b of (var*con,function) sequence   (* Binds mutually recursive open functions *)
    | Fixcode_b of (var*con,function) sequence   (* Binds mutually recursive code functions *)
                                             (* Allows the creation of term and for-all closures *)
    | Fixclosure_b of bool * (var*con , {code:var, cenv:con, venv:exp}) sequence

  (* A function is either open or closed.  It is a "code pointer" if it is closed.
   * It may or may not be effect-free and may or may not be recursive.
   * The type and value parameters with their kinds and types are given next.
   * Finally, the body and the result type is given.
   * Note that the type of the function can be easily given from these ingredients.
   *)

  and function = Function of {effect      : effect,
			      recursive   : recursive,
			      tFormals    : var list,
			      eFormals    : (var * niltrace) list,
			      fFormals    : (var list),
			      body        : exp}

  datatype import_entry = ImportValue of label * var * niltrace * con
                        | ImportType  of label * var * kind
                        | ImportBnd   of phase * conbnd

  datatype export_entry = ExportValue of label * var
                        | ExportType  of label * var

  datatype module = MODULE of {bnds : bnd list,
			       imports : import_entry list,
			       exports : export_entry list,
			       exports_int : import_entry list option}

  datatype interface = INTERFACE of {imports : import_entry list,
				     exports : import_entry list}

end

