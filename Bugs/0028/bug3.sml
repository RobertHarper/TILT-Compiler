(*$import *)

datatype bool = false | true
    
type 'a ref = 'a TiltPrim.ref

datatype 'a broken = BROKEN of 'a ref
    
val eq : 'a broken * 'a broken -> bool = (op =)

fun eq' (BROKEN a, BROKEN b) = (op=) (a, b)
