(*$import Prim PRIM Annotation Sequence TraceInfo *)

signature NIL =
sig

  type var = Name.var
  type label = Name.label

  type w32 = Word32.word
  type prim = Prim.prim
  type ('a,'b) sequence = ('a,'b) Sequence.sequence
                          (* conceptually this is a ('a*'b) list that allows
			     fast access using 'a as a key *)
  datatype annotation = datatype Annotation.annotation

  (* In general, we want to distinguish between functions/arrow types that 
   * are open (possibly having free variables) or those that are closed.
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
       bound in the same cluster.  This means that the functions bound
       are not (mutually recursive) and can thus be inlined.
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
    | Ref_c                                   (* references *)
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
    | AllArrow_c of openness * effect *        (* open functions, code functions, and closures *)
                    (var * kind) list * 
		    var list option * con list *
		    w32 * con
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

  and conbnd = Con_cb of (var * con)
             | Open_cb of (var * (var * kind) list * con * kind)
             | Code_cb of (var * (var * kind) list * con * kind)

  and nilprim = 
      record of label list       (* record intro *)
    | select of label            (* record field selection; takes the record type *)
    | inject of TilWord32.word   (* slow; must be given one type that is
				    reducible to a sum type *)
    | inject_nonrecord of TilWord32.word   (* fast; the type of the injected field must reduce
					            to a non-record type *)
    | inject_record of TilWord32.word 
                                 (* fast; must be given one type that is reducible
				    to a sum type where the indicated component must 
				    be reducible to a record; the term components
				    consituting the record are passed separately 
				    to this primitive *)
    | project_sum of TilWord32.word (* slow; same requirement as inject *)
    | project_sum_record of TilWord32.word * label (* fast; same requirement as inject_record;
				                      the record type must contain the label given here *)
    | project_sum_nonrecord of TilWord32.word (* fast; same as project_sum except
					          we know that field is not a record type *)


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
		  arms : (w32 * exp) list,
		  default : exp option}             (* integers *)
    | Sumsw_e of {arg : exp,
		  sumtype : con,
		  bound : var,
		  arms : (w32 * exp) list,
		  default : exp option}             (* sum types *)
    | Exncase_e of {arg : exp,
		    bound : var,
		    arms : (exp * exp) list,
		    default : exp option}           (* exceptions *)
    | Typecase_e of {arg : con,
		     arms : ((var * kind) list * exp) list,
		     default : exp option}          (* typecase *)

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
    | Handle_e of exp * var * exp


  (* result types are needed for recursive definitions in order to make
   * type-checking syntax directed.  Also note that I've forced all functions
   * to be named -- that is, they do not appear as exp forms.  We need
   * recursive expressions involving records and closures in order to
   * closure-convert recursive functions.
   *)

  and bnd =                                (* Term-level Bindings with optional classifiers *)
      Con_b of phase * conbnd                (* Binds constructors *)
    | Exp_b of var * niltrace * exp          (* Binds expressions *)
    | Fixopen_b of (var,function) sequence  (* Binds mutually recursive open functions *)
    | Fixcode_b of (var,function) sequence  (* Binds mutually recursive code functions *)
                                             (* Allows the creation of term and for-all closures;
                                                bool indicates if it is really recursive *)
    | Fixclosure_b of bool * (var , {code:var, cenv:con, venv:exp, tipe:con}) sequence
                                             

  (* A function is either open or closed.  It is a "code pointer" if it is closed.
   * It may or may not be effect-free and may or may not be recursive.
   * The type and value parameters with their kinds and types are given next.
   * Finally, the body and the result type is given.
   * Note that the type of the function can be easily given from these ingredients.
   *)

  and function = Function of effect * recursive * (var * kind) list * 
                             bool * (var * con) list * (var list) * exp * con  


  datatype import_entry = ImportValue of label * var * con
                        | ImportType of label * var * kind
  datatype export_entry = ExportValue of label * var
                        | ExportType of label * var
  datatype module = MODULE of {bnds : bnd list,
			       imports : import_entry list,
			       exports : export_entry list}

end

