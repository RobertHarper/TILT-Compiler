(*$import *)

type 'a ref = 'a TiltPrim.ref
    
signature NODE =
sig
    type 'a hash_table
end

functor GraphFn(A : NODE) =
struct
    type graph = TiltPrim.int32 A.hash_table
end

structure HashTable =
    struct
	type 'a hash_table = TiltPrim.unit
    end

structure Node = 
    struct
	open HashTable
    end

structure Graph = GraphFn(Node)

fun refresh (reverse : Graph.graph ref) = !reverse
