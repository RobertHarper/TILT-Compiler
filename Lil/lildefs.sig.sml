signature LILDEFS = 
  sig

    structure K : 
      sig
	val ntuple : Lil.kind list -> Lil.kind
	val narrow : Lil.kind list -> Lil.kind -> Lil.kind

	val forall : Lil.var * Lil.kind -> Lil.kind
	val forall' : (Lil.var -> Lil.kind) -> Lil.kind
	val arrow : Lil.kind -> Lil.kind -> Lil.kind
	val pair : Lil.kind * Lil.kind -> Lil.kind
	val binsum : Lil.kind * Lil.kind -> Lil.kind
	val sum : Lil.kind list -> Lil.kind
	val nat : unit -> Lil.kind
	val unit : unit -> Lil.kind
	val var : Lil.var -> Lil.kind

	val mu : Lil.var * Lil.kind -> Lil.kind
	val mu' : (Lil.var -> Lil.kind) -> Lil.kind

	val Type : Lil.size -> Lil.kind
	val T32 : unit -> Lil.kind
	val T64 : unit -> Lil.kind
	val TM  : unit -> Lil.kind

	val list : Lil.kind-> Lil.kind
	val tlist : unit -> Lil.kind
      end

    structure KOps :
      sig
	val sumw : Lil.w32 -> Lil.kind -> Lil.kind option
      end

    structure C :
      sig 
	val var : Lil.var -> Lil.con

	val app : Lil.con -> Lil.con -> Lil.con
	val appn : Lil.con -> Lil.con list -> Lil.con 

	val k_app : Lil.con -> Lil.kind -> Lil.con

	val nill : Lil.kind -> Lil.con

	val cons' : Lil.kind -> (Lil.con*Lil.con) -> Lil.con
	val cons : Lil.kind -> Lil.con -> Lil.con

	val list  : Lil.kind -> Lil.con list -> Lil.con
	val tlist : Lil.con list -> Lil.con

	val nlambda : (Lil.var * Lil.kind) list -> Lil.con -> Lil.con

	(* 0 indexed n-ary tuples *)
	val nproj : Lil.con -> int -> Lil.con
	val ntuple    : Lil.con list -> Lil.con

	val pair : Lil.con -> Lil.con -> Lil.con
	val pi1  : Lil.con -> Lil.con
	val pi2  : Lil.con -> Lil.con

	val star : unit -> Lil.con 
	val nat : Lil.w32 -> Lil.con
	val nat' : int -> Lil.con

	val inj : Lil.w32 -> Lil.kind -> Lil.con -> Lil.con
	val inl : Lil.kind -> Lil.con -> Lil.con
	val inr : Lil.kind -> Lil.con -> Lil.con

	val fold : Lil.kind -> Lil.con -> Lil.con

	(* The defined unfold constructor for the given kind*)
	val unfold : Lil.kind -> Lil.con
	(* Apply the defined unfold constructor for the given kind
	 * to the given con 
	 *)
	val do_unfold : Lil.kind -> Lil.con -> Lil.con

	val lambda  : Lil.var * Lil.kind -> Lil.con -> Lil.con
	val lambda' : Lil.kind -> (Lil.con -> Lil.con) -> Lil.con

	val sumcase    : Lil.con ->  (Lil.w32 * (Lil.con -> Lil.con)) list -> Lil.con option -> Lil.con
	val binsumcase : Lil.con -> (Lil.con -> Lil.con) -> (Lil.con -> Lil.con) -> Lil.con

	(* listcase eltk arg ifnil ifcons 
	 * arg :: list_k[eltk]
	 * listcase arg
	 * | [] => ifnil
	 * | a::aa => ifcons(<a,aa>)
	 *)
	val listcase  : Lil.kind -> Lil.con -> Lil.con -> (Lil.con -> Lil.con) -> Lil.con


	(* listcase eltk arg ifnil ifcons 
	 * arg :: list_k[eltk]
	 * listcase arg
	 * | [] => ifnil
	 * | a::aa => ifcons(a,aa)
	 *)
	val listcase' : Lil.kind -> Lil.con -> Lil.con -> ((Lil.con * Lil.con) -> Lil.con) -> Lil.con

	(* nary_listcase (eltk : kind) (c : con) (arms : (Lil.con list -> Lil.con) list) : Lil.con 
	 * c :: list_k[eltk],
	 * listcase c
	 * | [] => arms_1([])
	 * | [a] => arms_2([a])
	 * | [a,b] => arms_3([a,b])
	 * | (a::b::rest) => arms_4([a,b,rest])
	 *)
	val nary_listcase : Lil.kind -> Lil.con -> (Lil.con list -> Lil.con) list -> Lil.con


	val pcon_app : Lil.primcon -> Lil.con list -> Lil.con

	(* closure :: T32 list -> T64 list -> T32 -> T32 *)
	val closure : unit -> Lil.con
      end

    structure COps :
      sig
	val ksum2arm : Lil.con -> Lil.con
	val ksum2sum : Lil.con -> Lil.con
	val sum2ksum  : Lil.con -> Lil.con -> Lil.con
	val sum2ksum' : Lil.w32 -> Lil.con -> Lil.con

	val sum_totalcount : Lil.con -> Lil.w32
	(* Project the first i fields from the given con *)
	val project_i_fields : int -> Lil.con -> Lil.con list

	val ntuple2tlist : int -> Lil.con -> Lil.con

	val mkT32 : Lil.size -> Lil.con -> Lil.con
      end

    structure T :
      sig
	(* Where applicable, the primed versions of functions generally
	 * present a "meta-level" interface, whereas the unprimed 
	 * versions work on object level terms.  So for example, we might
	 * have 
	 *  f  : Lil.con -> Lil.con
	 *  f' : Lil.con list -> Lil.con
	 * where f expects an argument of list kind, 
	 * and f' l == f (tlist l)
	 **)

	val allarrow  : (Lil.var * Lil.kind) list
	                -> Lil.con -> Lil.con -> Lil.con -> Lil.con
	val allarrow' : (Lil.var * Lil.kind) list
	                -> Lil.con list -> Lil.con list -> Lil.con -> Lil.con
	val arrow  : Lil.con -> Lil.con -> Lil.con -> Lil.con
	val arrow' : Lil.con list -> Lil.con list -> Lil.con -> Lil.con

	val allcode  : (Lil.var * Lil.kind) list
	                -> Lil.con -> Lil.con -> Lil.con -> Lil.con
	val allcode' : (Lil.var * Lil.kind) list
	                -> Lil.con list -> Lil.con list -> Lil.con -> Lil.con
	val code  : Lil.con -> Lil.con -> Lil.con -> Lil.con
	val code' : Lil.con list -> Lil.con list -> Lil.con -> Lil.con

	val externarrow : Lil.size -> Lil.con -> Lil.con -> Lil.con -> Lil.con
	val externarrow' : Lil.size -> Lil.con list -> Lil.con list -> Lil.con -> Lil.con

	val float : unit -> Lil.con
	val boxed : Lil.size -> Lil.con -> Lil.con
	val boxed_float : unit -> Lil.con

	val embed : Lil.size -> Lil.con -> Lil.con
	val closure : (Lil.var * Lil.kind) list -> Lil.con -> Lil.con -> Lil.con -> Lil.con

	val coercion : Lil.con -> Lil.con -> Lil.con
	  
	(* exists (v,k) c === exists' k (Lam(v,k).c) 
	 *)
	val exists  : Lil.var * Lil.kind -> Lil.con -> Lil.con
	val exists' : Lil.kind -> Lil.con -> Lil.con
	(* forall (v,k) c === forall' k (Lam(v,k).c) 
	 *)
	val forall : Lil.var * Lil.kind -> Lil.con -> Lil.con
	val forall' : Lil.kind -> Lil.con -> Lil.con

	val float64 : unit -> Lil.con
	  
	val ksum : Lil.con -> Lil.con -> Lil.con -> Lil.con
	val ksum' : Lil.w32 -> Lil.w32 -> Lil.con list -> Lil.con
	val nary_forall : (Lil.var * Lil.kind) list -> Lil.con -> Lil.con
	val sum : Lil.con -> Lil.con -> Lil.con
	val sum' : Lil.w32 -> Lil.con list -> Lil.con
	val tuple : Lil.con -> Lil.con
	val tuple' : Lil.con list -> Lil.con

	val ptr : Lil.con -> Lil.con

	val tupleptr : Lil.con -> Lil.con
	val tupleptr' : Lil.con list -> Lil.con

	val bool : unit -> Lil.con

	val unit : unit -> Lil.con

	val array : Lil.size -> Lil.con -> Lil.con
	val refc : Lil.con -> Lil.con

	val tag : Lil.w32 -> Lil.con
	val dyntag : Lil.con -> Lil.con
	val void : unit -> Lil.con
	val exn : unit -> Lil.con
	val intt : Lil.size -> Lil.con
      end

    structure Q :
      sig
	val coerce : Lil.sv32 -> Lil.sv32 -> Lil.sv32
	val coerce_many : Lil.sv32 list -> Lil.sv32 -> Lil.sv32
	val forgetknown : Lil.con -> Lil.sv32
	val projknown : Lil.con -> Lil.sv32
	val injunion : Lil.con -> Lil.sv32
	val injforget : Lil.con -> Lil.sv32
	val pack : Lil.con -> Lil.con -> Lil.sv32
      end

    structure EOps :
      sig

	val project_all_tuple_fields'' : Lil.sv32 -> int -> Lil.op32 list 
	val project_all_tuple_fields'  : Lil.sv32 -> int -> Lil.op32 list P.pexp
	val project_all_tuple_fields   : Lil.sv32 -> int -> Lil.sv32 list P.pexp
	  
      end

    structure E :
      sig 
	val op2exp : Lil.op32 -> Lil.exp

	val tuple'' : Lil.sv32 list -> Lil.op32
	val tuple'  : Lil.sv32 list -> Lil.op32 P.pexp
	val tuple   : Lil.sv32 list -> Lil.sv32 P.pexp

	val unit'  : unit -> Lil.sv32
	val unit   : unit -> Lil.sv32 P.pexp

	val select'' : Lil.w32 -> Lil.sv32 -> Lil.op32
	val select'  : Lil.w32 -> Lil.sv32 -> Lil.op32 P.pexp
	val select   : Lil.w32 -> Lil.sv32 -> Lil.sv32 P.pexp

	(* inj_exn c tag v  : T.exn_type()
	 *   v : c
	 *   tag : T.dyntag c
	 *)
	val inj_exn : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val dyntag'' : Lil.con -> Lil.op32 
	val dyntag'  : Lil.con -> Lil.op32 P.pexp
	val dyntag   : Lil.con -> Lil.sv32 P.pexp

	val tag' : Lil.w32 -> Lil.sv32
	val tag  : Lil.w32 -> Lil.sv32 P.pexp

	val tag_and_box'' : Lil.w32 -> Lil.sv32 -> Lil.op32 
	val tag_and_box'  : Lil.w32 -> Lil.sv32 -> Lil.op32 P.pexp
	val tag_and_box   : Lil.w32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val unbox'' : Lil.sv32 -> Lil.op64 
	val unbox' : Lil.sv32 -> Lil.op64 P.pexp
	val unbox : Lil.sv32 -> Lil.sv64 P.pexp

	val box'' : Lil.sv64 -> Lil.op32
	val box'  : Lil.sv64 -> Lil.op32 P.pexp
	val box   : Lil.sv64 -> Lil.sv32 P.pexp

	val ptreq'' : Lil.sv32 -> Lil.sv32 -> Lil.op32
	val ptreq'  : Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp
	val ptreq   : Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val inteq'' : Lil.size -> (Lil.sv32 * Lil.sv32) -> Lil.op32 
	val inteq'  : Lil.size -> (Lil.sv32 * Lil.sv32) -> Lil.op32 P.pexp
	val inteq   : Lil.size -> (Lil.sv32 * Lil.sv32) -> Lil.sv32 P.pexp

	val intconst' : Lil.size -> Lil.w32 -> Lil.sv32
	val intconst  : Lil.size -> Lil.w32 -> Lil.sv32 P.pexp

	val update_array64'' : Lil.sv32 -> Lil.sv32 -> Lil.sv64 -> Lil.op32
	val update_array64'  : Lil.sv32 -> Lil.sv32 -> Lil.sv64 -> Lil.op32 P.pexp
	val update_array64   : Lil.sv32 -> Lil.sv32 -> Lil.sv64 -> Lil.sv32 P.pexp

	val update_array32'' : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 -> Lil.op32
	val update_array32'  : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp
	val update_array32   : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val update_intarray'' : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 -> Lil.op32
	val update_intarray'  : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp
	val update_intarray   : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val sub_array64'' : Lil.sv32 -> Lil.sv32 -> Lil.op64
	val sub_array64'  : Lil.sv32 -> Lil.sv32 -> Lil.op64 P.pexp
	val sub_array64   : Lil.sv32 -> Lil.sv32 -> Lil.sv64 P.pexp

	val sub_array32'' : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32
	val sub_array32'  : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp
	val sub_array32   : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val sub_intarray'' : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.op32
	val sub_intarray'  : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp
	val sub_intarray   : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val create_array64'' : Lil.sv32 -> Lil.sv64 -> Lil.op32
	val create_array64'  : Lil.sv32 -> Lil.sv64 -> Lil.op32 P.pexp
	val create_array64   : Lil.sv32 -> Lil.sv64 -> Lil.sv32 P.pexp

	val create_array32'' : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32
	val create_array32'  : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp 
	val create_array32   : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val create_intarray'' : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.op32
	val create_intarray'  : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp 
	val create_intarray   : Lil.size -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val create_empty_array64'' : unit -> Lil.op32
	val create_empty_array64'  : unit -> Lil.op32 P.pexp
	val create_empty_array64   : unit -> Lil.sv32 P.pexp

	val create_empty_array32'' : Lil.con ->  Lil.op32
	val create_empty_array32'  : Lil.con ->  Lil.op32 P.pexp 
	val create_empty_array32   : Lil.con ->  Lil.sv32 P.pexp

	val create_empty_intarray'' : Lil.size ->  Lil.op32
	val create_empty_intarray'  : Lil.size ->  Lil.op32 P.pexp 
	val create_empty_intarray   : Lil.size ->  Lil.sv32 P.pexp

	val length_array64'' : Lil.sv32 -> Lil.op32
	val length_array64'  : Lil.sv32 -> Lil.op32 P.pexp
	val length_array64   : Lil.sv32 -> Lil.sv32 P.pexp

	val length_array32'' : Lil.con -> Lil.sv32 -> Lil.op32
	val length_array32'  : Lil.con -> Lil.sv32 -> Lil.op32 P.pexp 
	val length_array32   : Lil.con -> Lil.sv32 -> Lil.sv32 P.pexp

	val length_intarray'' : Lil.size -> Lil.sv32 -> Lil.op32
	val length_intarray'  : Lil.size -> Lil.sv32 -> Lil.op32 P.pexp 
	val length_intarray   : Lil.size -> Lil.sv32 -> Lil.sv32 P.pexp

	(* Superceded by Ptreq
	val equal_array64'' : Lil.sv32 -> Lil.sv32 -> Lil.op32
	val equal_array64'  : Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp
	val equal_array64   : Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp

	val equal_array32'' : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32
	val equal_array32'  : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.op32 P.pexp 
	val equal_array32   : Lil.con -> Lil.sv32 -> Lil.sv32 -> Lil.sv32 P.pexp
	  *)

	(*inject into sum and forget *)
	val inj_nontag_from_sumtype' : Lil.w32 -> Lil.con -> Lil.sv32 -> Lil.sv32
	val inj_nontag_from_sumtype  : Lil.w32 -> Lil.con -> Lil.sv32 -> Lil.sv32 P.pexp

	val project'  : Lil.con -> Lil.sv32 -> Lil.sv32
	val project   : Lil.con -> Lil.sv32 -> Lil.sv32 P.pexp

	val injunion' : Lil.con -> Lil.sv32 -> Lil.sv32
	val injunion  : Lil.con -> Lil.sv32 -> Lil.sv32 P.pexp

	val bool' : bool -> Lil.sv32
	val bool  : bool -> Lil.sv32 P.pexp

	val tapp': Lil.con -> Lil.sv32 -> Lil.sv32
	val tapp : Lil.con -> Lil.sv32 -> Lil.sv32 P.pexp
	val nary_tapp' : Lil.con list -> Lil.sv32 -> Lil.sv32
	val nary_tapp  : Lil.con list -> Lil.sv32 -> Lil.sv32 P.pexp


	val allapp'' : Lil.sv32 -> Lil.con list -> Lil.sv32 list -> Lil.sv64 list -> Lil.op32 
	val allapp'  : Lil.sv32 -> Lil.con list -> Lil.sv32 list -> Lil.sv64 list -> Lil.op32 P.pexp
	val allapp   : Lil.sv32 -> Lil.con list -> Lil.sv32 list -> Lil.sv64 list -> Lil.sv32 P.pexp

	val nary_tabs' : (Lil.var * Lil.kind) list -> Lil.sv32 -> Lil.sv32
	val nary_tabs  : (Lil.var * Lil.kind) list -> Lil.sv32 -> Lil.sv32 P.pexp

	val lambda : (Lil.var * Lil.con) list -> Lil.con -> Lil.exp -> Lil.sv32 P.pexp

	(* closure (code_var,venv,closure_type,venv_type) *)
	val closure : (Lil.sv32 * Lil.sv32 * Lil.con * Lil.con) -> Lil.sv32 P.pexp
	val closure_app' : (Lil.sv32 * Lil.con list * Lil.sv32 list * Lil.sv64 list) -> Lil.op32 P.pexp
	val closure_app  : (Lil.sv32 * Lil.con list * Lil.sv32 list * Lil.sv64 list) -> Lil.sv32 P.pexp

	val injforget' : Lil.con -> Lil.sv32 -> Lil.sv32
	val injforget  : Lil.con -> Lil.sv32 -> Lil.sv32 P.pexp

	val forget' : Lil.con -> Lil.sv32 -> Lil.sv32
	val forget  : Lil.con -> Lil.sv32 -> Lil.sv32 P.pexp

	val pack' : Lil.con -> Lil.con -> Lil.sv32 -> Lil.sv32 
	val pack  : Lil.con -> Lil.con -> Lil.sv32 -> Lil.sv32 P.pexp

	val unpack : Lil.sv32 -> Lil.sv32 P.pexp 

	val mk_let : Lil.bnd list -> Lil.exp -> Lil.exp

	val letinj : Lil.w32 -> Lil.con * Lil.sv32 -> Lil.con P.pexp 
	val letfold : Lil.con -> Lil.con P.pexp 
	val letsplit : Lil.con -> (Lil.con * Lil.con) P.pexp 
	val letpath : Lil.con -> Lil.con P.pexp option
	val vcase : Lil.w32 -> Lil.con -> (Lil.con -> Lil.sv32 P.pexp) -> Lil.sv32 -> Lil.sv32 P.pexp

	val list_refine_to_nil  : Lil.con -> Lil.sv32 -> Lil.con P.pexp
	val list_refine_to_cons  : Lil.con -> Lil.sv32 -> Lil.con P.pexp
	val list_refine_to_cons'  : Lil.con -> Lil.sv32 -> (Lil.con * Lil.con) P.pexp

(*	(* list_vcase_cons rtype srep eltk dead f
	 * let fold a = srep
         * in vcaser[rtype] a ((inl * ) => dead 
         *                    | (inr b) => let <b1,b2> = b in f(b1,b2))
	 *)
	val list_vcase_cons': Lil.con -> Lil.con ->  Lil.kind -> Lil.sv32 -> (Lil.var * Lil.var -> Lil.exp) -> Lil.op32 P.pexp
	val list_vcase_cons : Lil.con -> Lil.con ->  Lil.kind -> Lil.sv32 -> (Lil.var * Lil.var -> Lil.exp) -> Lil.sv32 P.pexp

	val list_vcase_nth' : Lil.con -> Lil.con -> Lil.kind -> Lil.sv32 -> (Lil.var list -> Lil.exp) -> int -> Lil.op32 P.pexp
	val list_vcase_nth  : Lil.con -> Lil.con -> Lil.kind -> Lil.sv32 -> (Lil.var list -> Lil.exp) -> int -> Lil.sv32 P.pexp
*)
      end

  end