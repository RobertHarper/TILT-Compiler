(*$import Nil *)

(*
 * The NilDefs structure is a central repository for NIL syntactic
 * definitions, and functions for constructing NIL constructs.
 * 
 *)

signature NILDEFS = 
  sig

    (* Is the constructor "small", according to the definition
     * of A-normal form
     *)
    val small_con : Nil.con -> bool

    (* Is the expression "small", according to the definition
     * of A-normal form
     *)
    val small_exp : Nil.exp -> bool

    (* Is the expression syntactically a closed value
     *)
    val is_closed_value : Nil.exp -> bool

    (* could the expression have an effect?
     * assumes expression is A-normal *)
    val effect : Nil.exp -> bool 

    (* Is the primcon covariant with respect to subtyping
     *)
    val covariant_prim: Nil.primcon -> bool

    (* Does a primitive use it's constructor arguments
     * as data, or just a classifiers.
     *)
    val allprim_uses_carg : Nil.allprim -> bool

    (*Translate between paths represented as lists of labels
     * and constructor paths
     *)
    val path2con : Nil.var * Nil.label list -> Nil.con
    val con2path : Nil.con -> (Nil.var * Nil.label list) option

    val con_tuple : Nil.con list -> Nil.con
    val tuple_con : Nil.con list -> Nil.con

    val tuple_kind : Nil.kind list -> Nil.kind

   (*
         Creates the kind for a "tuple" of types of length n. 
         1-tuples yield kind Type, rather than a record kind.
    *)
    val kind_type_tuple : int -> Nil.kind

    val unit_con : Nil.con
    val unit_exp : Nil.exp

    val match_exn : Nil.exp
    val bool_con : Nil.con
    val string_con : Nil.con
    val exn_con : Nil.con
    val true_exp : Nil.exp
    val false_exp : Nil.exp
    val int_con : Nil.con   (* 32-bit ints *)
    val char_con : Nil.con  (* 8-bit ints *)

    (* If optional variable argument is present, then the record will be bound
     * to that variable, and the expression returned will simply be that var
     *)
    val mk_record_with_gctag : 
      (Nil.label list) * (Nil.niltrace list option) * 
      (Nil.con list) * (Nil.exp list) * Nil.var option->  (Nil.bnd list * Nil.exp) 

  end

