signature TRANSLATIONDEFS =
  sig

    val Tmil : unit -> Lil.kind

    (* :: Tmil -> T32 *)
    val interp  : Lil.con -> Lil.con

    (* :: Tmil -> T32 *)
    val R : Lil.con -> Lil.con

    (* :: T32 list -> Tmil *)
    val TupleRep  : Lil.con list -> Lil.con
    (* :: unit -> Tmil *)
    val BFloatRep : unit -> Lil.con
    (* :: Tmem -> Tmil *)
    val PtrRep    : Lil.con -> Lil.con
    (* :: T32 -> Tmil *)
    val OtherRep  : Lil.con -> Lil.con
 
    (* Tmil -> Tmem *)
    val Array : Lil.con -> Lil.con
    (* Tmil -> T32 *)
    val Arrayptr : Lil.con -> Lil.con

    (* Tmil -> Tmil -> T32 *)
    val Vararg : Lil.con -> Lil.con -> Lil.con

    (* :: Tmil -> T32 -> T32 -> T32 *)
    val IfTaglike : Lil.con -> Lil.con -> Lil.con -> Lil.con

    (* : int -> a::Tmil -> R(a) *)
    val tuple'   : int -> Lil.con -> Lil.op32 P.pexp
    val tuple    : int -> Lil.con -> Lil.sv32 P.pexp
    (* : a::Tmil -> R(a) *)
    val bfloat'   : Lil.con -> Lil.op32 P.pexp
    val bfloat    : Lil.con -> Lil.sv32 P.pexp
    (* : a::Tmil -> R(a) *)
    val ptr'      : Lil.con -> Lil.op32 P.pexp
    val ptr       : Lil.con -> Lil.sv32 P.pexp
    (* : a::Tmil -> R(a) *)
    val otherwise': Lil.con -> Lil.op32 P.pexp
    val otherwise : Lil.con -> Lil.sv32 P.pexp


    val mk_project_dynamic_fn : unit -> Lil.sv32 P.pexp 
    val mk_inject_dynamic_fn  : unit -> Lil.sv32 P.pexp 
    val mk_project_dynamic_app : Lil.sv32 -> Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp 
    val mk_inject_dynamic_app  : Lil.sv32 -> Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp 

    structure Array :
      sig
	
	val create_dynamic : Lil.con -> Lil.sv32 -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val create_int     : Prim.intsize -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val create_float   : (Lil.sv32 * Lil.sv64) -> Lil.op32 P.pexp 
	val create_other   : Lil.con -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 

	val create_empty_dynamic : Lil.con -> Lil.sv32 -> unit -> Lil.op32 P.pexp 
	val create_empty_int     : Prim.intsize -> unit -> Lil.op32 P.pexp 
	val create_empty_float   : unit -> Lil.op32 P.pexp 
	val create_empty_other   : Lil.con -> unit -> Lil.op32 P.pexp 

	val sub_dynamic : Lil.con -> Lil.sv32 -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val sub_int     : Prim.intsize -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val sub_float   : (Lil.sv32 * Lil.sv32) -> Lil.op64 P.pexp
	val sub_other   : Lil.con -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 

	val update_dynamic : Lil.con -> Lil.sv32 -> (Lil.sv32 * Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val update_int     : Prim.intsize -> (Lil.sv32 * Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val update_float   : (Lil.sv32 * Lil.sv32 * Lil.sv64) -> Lil.op32 P.pexp 
	val update_other   : Lil.con -> (Lil.sv32 * Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 

	val length_dynamic : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp 
	val length_int     : Prim.intsize -> Lil.sv32 -> Lil.op32 P.pexp 
	val length_float   : Lil.sv32 -> Lil.op32 P.pexp 
	val length_other   : Lil.con -> Lil.sv32 -> Lil.op32 P.pexp 

	val equal_dynamic : Lil.con -> Lil.sv32 -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val equal_int     : Prim.intsize -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val equal_float   : (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
	val equal_other   : Lil.con -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp 
      end

    (* Ready? *)

    (* argc::Tmil -> resc::Tmil -> R(argc) -> (interp(argc) -> interp(resc)) -> (Vararg argc rtype) *)
    (* argc::Tmil -> resc::Tmil -> R(argc) -> (Vararg argc resc) -> (interp(argc) -> interp(resc)) *)
    val mk_vararg_case : Lil.con -> Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp
    val mk_onearg_case : Lil.con -> Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp

    (* Yowser. *)      

    (* : unit -> All(argc::Tmil).All(rtype::T32).R(argc) -> (interp(argc) -> rtype) -> (Vararg argc (uninterp rtype)) *)
    (* : unit -> All(argc::Tmil).All(rtype::T32).R(argc) -> (Varargdef argc (uninterp rtype)) -> (interp(argc) -> rtype) *)
    val mk_vararg_fn : unit -> Lil.sv32 P.pexp 
    val mk_onearg_fn : unit -> Lil.sv32 P.pexp 
      
    (* Deep breath - almost there. *)

    (* typeof (mk_vararg_fn()) -> argc::Tmil -> resc::Tmil -> R(argc) -> (interp(argc) -> interp(resc)) -> (Vararg argc rtype) *)
    (* typeof (mk_onearg_fn()) -> argc::Tmil -> resc::Tmil -> R(argc) -> (Vararg argc resc) -> (interp(argc) -> interp(resc)) *)
    val mk_vararg_app : Lil.sv32 -> Lil.con -> Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp 
    val mk_onearg_app : Lil.sv32 -> Lil.con -> Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp 

  end  (* Freeeeeyow.  Not a moment to soon. *)