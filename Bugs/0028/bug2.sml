(*$import *)

datatype bool = false | true
    
type 'a ref = 'a TiltPrim.ref
    
structure Bug =
struct
    datatype 'a bug = BUG of 'a ref
    val eq = fn (BUG a) => (op=) (a,a)
end

signature BUG =
sig
    type 'a bug
    val eq : 'a bug -> bool
end

structure Bug2 : BUG = Bug
