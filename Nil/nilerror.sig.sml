(*$import Nil *)

(*
 Miscellaneous functions for use in error checking/reporting
*)

signature NILERROR = 
  sig
    exception FailedAssert of string

    type location = string
    (*Create a location from a filename and a function name*)
    val locate : string -> string -> location

    (*Given a location and a list of pairs of flags and error
     * reporting functions, call the first error function in the
     * list for which the flag is true, and raise FailedAssert with the
     * location information.
     *)
    val assert : location -> (bool * (unit -> unit)) list -> unit

    (*c_all pred fc list
     *       => true if forall(x in list), pred x => true
     *       => fc v if v is the first element in list s.t. pred v => false
     *)
    val c_all : ('elt -> bool) -> ('elt -> bool) -> 'elt list -> bool
    
    (*c_all1 pred fc list
     *       => true if forall(x in list), pred x => true
     *       => fc (SOME v) if v is the first element in list s.t. pred v => false
     *)
    val c_all1 : ('elt -> bool) -> ('elt option -> bool) -> 'elt list -> bool

    
    (*c_all2 pred fc (l1,l2)
     *       => fc NONE if length l1 != length l2
     *       => true if forall((x,y) in lists), pred (x,y) => true
     *       => fc (SOME (x,y)) if (x,y) is the first pair in l1,l2 s.t. pred (x,y) => false
     *)
    val c_all2 : (('elt * 'elt) -> bool) -> (('elt * 'elt) option -> bool) 
        -> ('elt list * 'elt list) -> bool
    
    (*c_all3 pred fc (l1,l2,l3)
     *       => fc NONE if not(length l1 = length l2 = length l2)
     *       => true if forall((x,y,z) in lists), pred (x,y,z) => true
     *       => fc (SOME (x,y,z)) if (x,y,z) is the first pair in lists 
     *          s.t. pred (x,y,z) => false
     *)
    val c_all3 : (('elt * 'elt * 'elt) -> bool) -> (('elt * 'elt * 'elt) option -> bool) 
        -> ('elt list * 'elt list * 'elt list) -> bool

    (* Printing functions suitable for use in error reporting *)
    val perr_e : Nil.exp -> unit
    val perr_c : Nil.con -> unit
    val perr_k : Nil.kind -> unit
    val perr_e_c : Nil.exp * Nil.con -> unit 
    val perr_c_c : Nil.con * Nil.con -> unit
    val perr_c_k : Nil.con * Nil.kind -> unit
    val perr_k_k : Nil.kind * Nil.kind -> unit
    val perr_c_k_k : Nil.con * Nil.kind * Nil.kind -> unit
    val perr_e_c_c : Nil.exp * Nil.con * Nil.con -> unit

    (* Prints its parameter with perr_k and returns false *)
    val b_perr_k : Nil.kind -> bool

    (* Converts a printing function like one of the above into one that takes an optional version of its usual parameter
       and returns false *)
    val o_perr : ('val -> unit) -> string -> 'val option -> bool
    
    (* o_perr'd versions of the corresponding functions above *)
    val o_perr_e : string -> Nil.exp option -> bool
    val o_perr_c : string -> Nil.con option -> bool
    val o_perr_k : string -> Nil.kind option -> bool
    val o_perr_e_c : string -> (Nil.exp * Nil.con) option -> bool
    val o_perr_c_c : string -> (Nil.con * Nil.con) option -> bool
    val o_perr_k_k : string -> (Nil.kind * Nil.kind) option -> bool
    val o_perr_c_k_k : string -> (Nil.con * Nil.kind * Nil.kind) option -> bool
    val o_perr_e_c_c : string -> (Nil.exp * Nil.con * Nil.con) option -> bool
  end
