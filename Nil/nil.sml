functor Nil(structure Annotation : ANNOTATION
	    structure Name : NAME
	    structure Prim : PRIM)
	   : NIL =
struct	

  structure Annotation = Annotation
  structure Name = Name
  structure Prim = Prim
  open Util
  val error = fn s => error "nil.sml" s

  type var = Name.var
  type label = Name.label
  type annot = Annotation.annotation
  type w32 = Word32.word
  type prim = Prim.prim


  (* In general, we want to distinguish between functions/arrow types that 
   * are open (possibly having free variables) or those that are closed.
   *)
  datatype openness = Open | Closed

  (* In addition, we would like to know if application of an arrow object
   * is total(i.e. effect-free) or partial (i.e. not necessarily effect-free)
   * In the total case, we are then guaranteed that the computation always
   * terminates and that, when given the same input, will yield the same output.
   *)
  datatype effect = Total | Partial

  (* A leaf procedure makes no function calls.  A self-recursive procedure may 
   * only call itself.  A non-leaf procedure may call any functions.
   *)
  datatype recursive = Leaf | Self | Nonleaf

  (* A sequential let only permits the bindings to be evaluated sequentially. 
   * A parallel let permits the bindings to be concurrently executed.
   *)
  datatype letsort = Sequential | Parallel




  datatype kind = 
      Type_k                        (* classifies constructors that are types *)
    | Word_k                        (* classifies types that fit in a word *)
    | Singleton_k of kind * con     (* singleton-kind at kind type that leaks 
				           through the constructor *)
                                    (* dependent record kind classify records of 
				           constructors *)
    | Record_k of (label * var * kind) list
                                    (* dependent arrow kinds classify open 
				       constructor funs or closures *)
    | Arrow_k of openness * (var * kind) list * kind 
                                    (* classifies closed constructor functions *)
    | Code_k of (var * kind) list * kind 


  and primcon =                          (* classifies term-level ... *)
      Int_c of Prim.intsize                   (* integers *)
    | Float_c of Prim.floatsize               (* register floating-points *)
    | BoxFloat_c of Prim.floatsize            (* boxed floating-points *)
    | Exn_c                                   (* exceptions *)
    | Array_c                                 (* arrays *)
    | Vector_c                                (* vectors *)
    | Ref_c                                   (* references *)
    | Exntag_c                                (* exception tags *)
    | Sum_c                                   (* sum types *)
    | Record_c                                (* records *)
    | Vararg_c | Onearg_c                     (* make_vararg and make_onearg *)

  and con = 
      Var_c of var
    | Let_c of letsort * (var * con) list * con   (* Constructor-level bindings *)
    | Fun_c of openness * (var * kind) list * con (* Constructor-level lambdas *)
    | Crecord_c of (label * var * con) list       (* Constructor-level records *)
    | Proj_c of con * label                       (* Constructor-level record projection *)
    | Closure_c of con * con                      (* Constructor-level closure: 
					               code and environment *)
    | App_c of openness * con * (con list)        (* Constructor-level application 
						       of open or closed constructor function *)
    | Prim_c of primcon * (con list)              (* Classify term-level values 
                                                       of primitive types *)
    | Mu_c of (var * con) list * con              (* Fix-point of constructors classify 
						       recursive datatypes *)
    | Arrow_c of openness * confun                (* open functions and closures *)
    | Code_c of confun                            (* for describing code at the term level: 
					               note that the classifiers of open functions 
                                                       and closures are given by Arrow_c *)
    | Annotate_c of annot * con                   (* General-purpose place to hang information *)

  withtype confun = effect * (var * kind) list * con list * con

  datatype nilprim = 
      record of label list       (* record intro *)
    | select of label            (* record field selection *)
    | inject of w32              (* slow; sum intro *)
    | inject_record of w32       (* fast; sum intro where argument is a record
					  whose components are individually passed in *)
    | project_sum                (* slow; given a special sum type, return carried value *)
    | project_sum_record of w32  (* fast; given a special sum type of record type, 
				          return the specified field *)
    | box_float of Prim.floatsize   (* boxing floating-points *)
    | unbox_float of Prim.floatsize (* unboxing floating-points *)
    | roll | unroll              (* coerce to/from recursive type *) 
    | make_exntag                (* generate new exn tag *)
    | inj_exn                    (* takes tag + value and give exn *)
    | make_vararg                (* given a function in onearg calling convention, convert to vararg *)
    | make_onearg                (* given a function in vararg calling convention, convert to onearg *)
    | peq                        (* polymorphic equality: unused since HIL compiles away equality *)


  datatype allprim = NilPrimOp of nilprim
                   | PrimOp of prim


  (* Intswitch should be apparent.
   * The con in the Sumswitch tells us the sum type and the w32
   *    is used to index the different cases for the sum type.
   * Exncase's arms are indexed by variables that must have type Exntag_c(c) and
   *    the arms must be functions from [c]->tau for some fixed result type tau.  
   *)
  datatype switch =                                 (* Switching on / Elim Form *)
      Intsw_e of (Prim.intsize,exp,w32) sw                (* integers *)
    | Sumsw_e of (con,exp,w32) sw                         (* sum types *)
    | Exncase_e of (unit,exp,var) sw                      (* exceptions *)


  and exp =                                          (* Term-level constructs *)
      Var_e of var                                        (* Term-level variables *)
    | Const_e of exp Prim.value                           (* Term-level constants *)
    | Let_e of letsort * bnd list * exp                   (* Binding construct *)
    | Prim_e of allprim * (con list) * (exp list) option  (* allow primops to be partially applied *)
    | Switch_e of switch                                  (* Switch statements *)
    | App_e of openness * exp * (con list) * (exp list)   (* Application of open functions and closures *)
    | Call_e of openness * var * (con list) * (exp list)  (* Application of code pointers *)
    | Raise_e of exp * con                                
    | Handle_e of exp * function


  (* result types are needed for recursive definitions in order to make
   * type-checking syntax directed.  Also note that I've forced all functions
   * to be named -- that is, they do not appear as exp forms.  We need
   * recursive expressions involving records and closures in order to
   * closure-convert recursive functions.
   *)

  and bnd =                                (* Term-level Bindings *)
      Con_b of var * con                            (* Binds constructors *)
    | Exp_b of var * exp                            (* Binds expressions *)
    | Fixfun_b of (var * function) list             (* Binds mutually recursive functions *)
                                                    (* Allows the creation of closures *)
    | Fixclosure_b of (var * con * {code:var, cenv:con, venv:exp}) list 

  (* A function is either open or closed.  It is a "code pointer" if it is closed.
   * It may or may not be effect-free and may or may not be recursive.
   * The type and value parameters with their kinds and types are given next.
   * Finally, the body and the result type is given.
   * Note that the type of the function can be easily given from these ingredients.
   *)

  and function = Function of openness * effect * recursive *
                             ((var * kind) list) * ((var * con) list) * 
			     exp * con  

  (* a generic term-level switch construct. *)
  withtype ('info,'arg,'t) sw = 
    {info : 'info, arg: 'arg, arms : ('t * function) list, default : exp option}


end

