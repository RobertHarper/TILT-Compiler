(*$import *)

(* All of these now work.  Bug2 failed with our first fix.
   A few of these probably worked all along. *)

datatype bool = false | true
    
type 'a ref = 'a TiltPrim.ref

type int = TiltPrim.int32
    
signature BUG =
sig
    type 'a bug
    val eq : 'a bug * 'a bug -> bool
end

structure Bug1 =
struct
    datatype 'a bug = BUG of 'a TiltPrim.ref
    fun eq (BUG a, BUG b) = (op=) (a, b)
end

structure OpaqueBug1 :> BUG = Bug1

structure Bug7 =
struct
    datatype 'a bug = BUG of 'a ref
    val eq = op =
end

structure OpaqueBug7 :> BUG = Bug7

structure Bug2 =
struct
    datatype 'a bug = BUG of 'a ref
    fun eq (BUG a, BUG b) = (op=) (a, b) (* XXX *)
end

structure OpaqueBug2 :> BUG = Bug2

structure Bug4 =
struct
    type 'a bug = int ref
    fun eq (a : 'a bug, b : 'a bug) = (op=)(a,b)
end

structure OpaqueBug4 :> BUG = Bug4

structure Bug5 =
struct
    type 'a bug = 'a ref
    fun eq (a : 'a bug, b : 'a bug) = (op=)(a,b)
end

structure OpaqueBug5 :> BUG = Bug5

structure Bug6 =
struct
    datatype 'a bug = BUG of int ref
    fun eq (BUG a, BUG b) = (op=) (a,b)
end

structure OpaqueBug6 :> BUG = Bug6

structure EqType =
struct
    type 'a eqty = TiltPrim.unit
end

structure OpaqueEqType :> sig eqtype 'a eqty end = EqType

structure Bug8 =
struct
    datatype 'a bug = BUG of int EqType.eqty
    fun eq (BUG a, BUG b) = (op=) (a, b)
end

structure OpaqueBug8 :> BUG = Bug8

structure Bug3 =
struct
    datatype 'a bug = BUG of 'a EqType.eqty
    fun eq (BUG a, BUG b) = (op=) (a, b) (* XXX *)
end

structure OpaqueBug3 :> 
    sig
	type 'a bug
	val eq : ''a bug * ''a bug -> bool
    end = Bug3
