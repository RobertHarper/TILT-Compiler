(*$import Prim Annotation Sequence TraceInfo *)

signature NIL =
sig

  type var = Name.var
  type label = Name.label

  type w32 = Word32.word
  type prim = Prim.prim
  type ('a,'b) sequence = ('a,'b) Sequence.sequence
                          (* An ('a,'b) sequence is conceptually equivalent to an ('a*'b) list.
			     Additionally, it has fast access using 'a as a key *)

  val flattenThreshold : int ref

  datatype annotation = datatype Annotation.annotation

  (* In general, we want to distinguish between different function-like terms.
   *   Open - open lambda terms (with free variables)
   *   Code - closed lambda terms (with no free variables) taking environments
   *   Closure - closed lambda terms packaged with their environment
   *)
  datatype openness = Open | Code | Closure

  (* In addition, we would like to know if application of an arrow object
   * is total(i.e. effect-free) or partial (i.e. not necessarily effect-free)
   * In the total case, we are then guaranteed that the computation always
   * terminates and that, when given the same input, will yield the same output.
   *)
  datatype effect = Total | Partial

  (*  A leaf procedure makes no function calls.
   *  A non-recursive function may not directly call itself or functions
   *    bound in the same cluster.  This means that the functions bound
   *    are not (mutually recursive) and can thus be inlined.
   *  An arbitrary function may call itself or other functions.
   *)
  datatype recursive = Leaf | NonRecursive | Arbitrary

  (* Trace annotations are inserted by the reifier and used by tortl.
   *)
  datatype niltrace = TraceUnknown
                    | TraceKnown of TraceInfo.traceinfo
                    | TraceCompute of var

  (* A sequential let only permits the bindings to be evaluated sequentially. 
   * A parallel let permits the bindings to be concurrently executed.
   *)
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
    | Float_c of Prim.floatsize               (* register floating-points *)
    | BoxFloat_c of Prim.floatsize            (* boxed floating-points *)
    | Exn_c                                   (* exceptions *)
    | Array_c                                 (* arrays *)
    | Vector_c                                (* vectors *)
    | Loc_c                                   (* locatives *)
    | Exntag_c                                (* exception tags *)
    | Record_c of label list * var list option  (* records *)
    | Sum_c of {tagcount : w32,
		totalcount : w32,
                known : w32 option}           (* sum types *)
    | Vararg_c of openness * effect           (* classifies make_vararg and make_onearg *)

  and con = 
      Prim_c of primcon * con list                (* Classify term-level values 
                                                       of primitive types *)
    | Mu_c of bool * (var,con) sequence           (* Constructors that classify values of
						    a recursive type; bool indicates if it
						    is really recursive *)
    | AllArrow_c of {openness : openness, (* open functions, code functions, and closures *)
		     effect : effect,
		     isDependent : bool,
		     tFormals : (var * kind) list,
		     eFormals : (var option * con) list,
		     fFormals : w32,
		     body_type : con}
    | ExternArrow_c of con list * con
    | Var_c of var
    | Let_c of letsort * conbnd list * con        (* Constructor-level bindings *)
    | Typeof_c of exp                             (* is equivalent to type of given expression *)
    | Crecord_c of (label * con) list             (* Constructor-level records *)
    | Proj_c of con * label                       (* Constructor-level record projection *)
    | Closure_c of con * con                      (* Constructor-level closure: 
                                                       code and environment *)
    | App_c of con * con list                     (* Constructor-level application 
						       of open or closed constructor function *)
    | Typecase_c of {arg : con,
                     arms : (primcon * (var * kind) list * con) list,
                     default : con,
		     kind : kind}        (* Constructor-level typecase *)
    | Annotate_c of kind annotation * con  (* General-purpose place to hang information *)

  and conbnd = Con_cb  of (var * con)
             | Open_cb of (var * (var * kind) list * con)
             | Code_cb of (var * (var * kind) list * con)

  and nilprim = 
      record of label list       (* record intro *)
    | partialRecord of label list * int (* record with a missing zero-indexed field *)
    | select of label            (* record field selection; takes the record type *)
    | inject of TilWord32.word   (* slow; must be given one type that is
				    reducible to a sum type *)
    | inject_known of TilWord32.word   (* fast; the injected field is a non-carrier or else
                                                its type is reducible to HNF *)
    | inject_known_record of TilWord32.word 
                                 (* fast; the type of the injected field must be reducible to a record type
				    further, the term arguments consituting the record are passed separately *)

    | project of TilWord32.word  (* corresponds to inject *)
    | project_known of TilWord32.word  (* corresponds to inject_known *)
    | project_known_record of TilWord32.word * label (* corresponds to inkject_known_record *)


    | box_float of Prim.floatsize   (* boxing floating-points *)
    | unbox_float of Prim.floatsize (* unboxing floating-points *)
    | roll | unroll              (* coerce to/from recursive type *) 
    | make_exntag                (* generate new exn tag *)
    | inj_exn of string          (* takes tag + value and give exn *)
    | make_vararg of openness * effect  (* given a function in onearg calling conv, convert to vararg *)
    | make_onearg of openness * effect  (* given a function in vararg calling conv, convert to onearg *)
    | peq                        (* polymorphic equality: unused since HIL compiles away equality *)


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
		  arms : (w32 * niltrace * exp) list,
		  default : exp option}             (* sum types *)
    | Exncase_e of {arg : exp,
		    result_type : con,
		    bound : var,
		    arms : (exp * niltrace * exp) list,
		    default : exp option}           (* exceptions *)
    | Typecase_e of {arg : con,
		     result_type : con,
		     arms : (primcon * (var * kind) list * exp) list,
		     default : exp}                 (* typecase *)
    | Ifthenelse_e of {arg : conditionCode,             
		       thenArm : exp,
		       elseArm : exp,
		       result_type : con}

  and exp =                                          (* Term-level constructs *)
      Var_e of var                                   (* Term-level variables *)
    | Const_e of (con,exp) Prim.value                (* Term-level constants *)
    | Let_e of letsort * bnd list * exp              (* Binding construct *)
    | Prim_e of allprim * (con list) * (exp list)    (* primops must be fully applied *)
    | Switch_e of switch                             (* Switch statements *)
    | App_e of openness * exp * con list *           (* application of open funs, code, or closures *)
                       exp list * exp list           (* in the case of code, the first exp must be a var *)
    | ExternApp_e of exp * exp list
    | Raise_e of exp * con                                
    | Handle_e of {body :exp,
	           bound : var,
                   handler : exp,
                   result_type : con}

  and conditionCode =                          (* Usd by Ifthenelse_e *)
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

  and bnd =                                 (* Term-level Bindings with optional classifiers *)
      Con_b of phase * conbnd               (* Binds constructors *)
    | Exp_b of var * niltrace * exp         (* Binds expressions *)
    | Fixopen_b of (var,function) sequence  (* Binds mutually recursive open functions *)
    | Fixcode_b of (var,function) sequence  (* Binds mutually recursive code functions *)
                                            (* Allows the creation of term and for-all closures;
                                                bool indicates if it is really recursive *)
    | Fixclosure_b of bool * (var , {code:var, cenv:con, venv:exp, tipe:con}) sequence
                                             
  and function = Function of {effect      : effect,
			      recursive   : recursive,
			      isDependent : bool,
			      tFormals    : (var * kind) list,
			      eFormals    : (var * niltrace * con) list,
			      fFormals    : (var list),
			      body        : exp,
			      body_type   : con}

  datatype import_entry = ImportValue of label * var * niltrace * con
                        | ImportType  of label * var * kind

  datatype export_entry = ExportValue of label * var
                        | ExportType  of label * var

  datatype module = MODULE of {bnds : bnd list,
			       imports : import_entry list,
			       exports : export_entry list}

end

