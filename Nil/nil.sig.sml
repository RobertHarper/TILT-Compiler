(*$import PRIM ANNOTATION *)

signature NIL =
sig
  structure Prim : PRIM
  structure Annotation : ANNOTATION

  type var = Name.var
  type label = Name.label
  datatype annotation = datatype Annotation.annotation

  type w32 = Word32.word
  type prim = Prim.prim
  type ('a,'b) sequence = ('a,'b) Util.sequence
                          (* conceptually this is a ('a*'b) list that allows
			     fast access using 'a as a key *)
  type ('a,'b) set = ('a,'b) Util.sequence
                          (* sets are used instead of lists in places 
			   where a natural ordering does not exist *)

  (* In general, we want to distinguish between functions/arrow types that 
   * are open (possibly having free variables) or those that are closed.
   *)
  datatype openness = Open | Code | Closure | ExternCode

  (* In addition, we would like to know if application of an arrow object
   * is total(i.e. effect-free) or partial (i.e. not necessarily effect-free)
   * In the total case, we are then guaranteed that the computation always
   * terminates and that, when given the same input, will yield the same output.
   *)
  datatype effect = Total | Partial

  (* A leaf procedure makes no function calls.
   *  A non-leaf procedure may call any functions.
   *)
  datatype recursive = Leaf | Nonleaf

  (* A sequential let only permits the bindings to be evaluated sequentially. 
   * A parallel let permits the bindings to be concurrently executed.
   *)
  datatype letsort = Sequential | Parallel
  datatype phase = Runtime | Compiletime
  datatype kind = 
      Type_k of phase               (* classifies constructors that are types *)
    | Word_k of phase               (* classifies types that fit in a word *)
    | Singleton_k of phase * kind * con     (* singleton-kind at kind type that leaks 
				               through the constructor *)
                                    (* dependent record kind classify records of 
				           constructors *)
    | Record_k of ((label*var),kind) sequence
                                    (* dependent arrow kinds classify open 
				       constructor funs, closed funs, or closures *)
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
    | Sum_c of {tagcount : w32,
		totalcount : w32,
                known : w32 option}           (* sum types *)
    | Record_c of label list                  (* records *)
    | Vararg_c of openness * effect           (* helps classify make_vararg and make_onearg *)

  and con = 
      Prim_c of primcon * con list                (* Classify term-level values 
                                                       of primitive types *)
    | Mu_c of bool * (var,con) sequence           (* Constructors that classify values of
						    a recursive type; bool indicates if it
						    is really recursive *)
    | AllArrow_c of openness * effect *           (* open functions, code functions, and closures *)
                    (var * kind) list * con list * w32 * con
    | Var_c of var
    | Let_c of letsort * conbnd list * con        (* Constructor-level bindings *)
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
    | Annotate_c of annot * con                   (* General-purpose place to hang information *)

  and conbnd = Con_cb of (var * kind * con)
             | Open_cb of (var * (var * kind) list * con * kind)
             | Code_cb of (var * (var * kind) list * con * kind)

  withtype annot = kind annotation

  datatype nilprim = 
      record of label list       (* record intro *)
    | select of label            (* record field selection; takes the record type *)
    | inject                     (* slow; must be given one type that is
				    reducible to a known sum type *)
    | inject_record              (* fast; must be given one type that is reducible
				    to a known sum type; the known component must 
				    be reducible to a record; the term components
				    consituting the record are passed separately 
				    to this primitive *)
    | project_sum                (* slow; same requirement as inject *)
    | project_sum_record of label (* fast; same requirement as inject_record;
				     the record type must contain the label given here *)

    | box_float of Prim.floatsize   (* boxing floating-points *)
    | unbox_float of Prim.floatsize (* unboxing floating-points *)
    | roll | unroll              (* coerce to/from recursive type *) 
    | make_exntag                (* generate new exn tag *)
    | inj_exn of string          (* takes tag + value and give exn *)
    | make_vararg of openness * effect  (* given a function in onearg calling conv, convert to vararg *)
    | make_onearg of openness * effect  (* given a function in vararg calling conv, convert to onearg *)
    | peq                        (* polymorphic equality: unused since HIL compiles away equality *)


  datatype allprim = NilPrimOp of nilprim
                   | PrimOp of prim

  (* Intswitch should be apparent.
   * The con list in the Sumswitch tells us the sum type and the w32
   *    is used to index the different cases for the sum type.
   * Exncase's arms are indexed by expressions that must have type Exntag_c(c) and
   *    the arms must be functions from [c]->tau for some fixed result type tau.  
   *)
  datatype switch =                                 (* Switching on / Elim Form *)
      Intsw_e of (Prim.intsize,exp,w32) sw                (* integers *)
    | Sumsw_e of (con,exp,w32) sw                         (* sum types *)
    | Exncase_e of (unit,exp,exp) sw                      (* exceptions *)
    | Typecase_e of (unit,con,primcon) sw                 (* typecase *)

  and exp =                                          (* Term-level constructs *)
      Var_e of var                                        (* Term-level variables *)
    | Const_e of (con,exp) Prim.value                           (* Term-level constants *)
    | Let_e of letsort * bnd list * exp                   (* Binding construct *)
    | Prim_e of allprim * (con list) * (exp list)         (* primops must be fully applied *)
    | Switch_e of switch                                  (* Switch statements *)
    | App_e of openness * exp * con list *                (* application of open funs, code, or closures *)
                       exp list * exp list                (* in the case of code, the first exp must be a var *)
    | Raise_e of exp * con                                
    | Handle_e of exp * function


  (* result types are needed for recursive definitions in order to make
   * type-checking syntax directed.  Also note that I've forced all functions
   * to be named -- that is, they do not appear as exp forms.  We need
   * recursive expressions involving records and closures in order to
   * closure-convert recursive functions.
   *)

  and bnd =                                (* Term-level Bindings with optional classifiers *)
      Con_b of var * kind * con              (* Binds constructors *)
    | Exp_b of var * con * exp               (* Binds expressions *)
    | Fixopen_b of (var,function) set        (* Binds mutually recursive open functions *)
    | Fixcode_b of (var,function) set        (* Binds mutually recursive code functions *)
                                             (* Allows the creation of term and for-all closures;
                                                bool indicates if it is really recursive *)
    | Fixclosure_b of bool * (var , {code:var, cenv:con, venv:exp, tipe:con}) set
                                             

  (* A function is either open or closed.  It is a "code pointer" if it is closed.
   * It may or may not be effect-free and may or may not be recursive.
   * The type and value parameters with their kinds and types are given next.
   * Finally, the body and the result type is given.
   * Note that the type of the function can be easily given from these ingredients.
   *)

  and function = Function of effect * recursive * (var * kind) list * 
                             (var * con) list * (var list) * exp * con  

  (* a generic term-level switch construct. *)
  withtype ('info,'arg,'t) sw = 
    {info : 'info, arg: 'arg, arms : ('t * function) list, default : exp option}

  datatype import_entry = ImportValue of label * var * con
                        | ImportType of label * var * kind
  datatype export_entry = ExportValue of label * exp * con
                        | ExportType of label * con * kind
  datatype module = MODULE of {bnds : bnd list,
			       imports : import_entry list,
			       exports : export_entry list}

end

