(*$import *)

(* bug0 for unmodified tilt *)
(* Yes: the bug existed before the equality changes *) 

type 'a myref = 'a ref
    
signature NODE =
sig
    type 'a hash_table
end

functor GraphFn(A : NODE) =
struct
    type graph = int A.hash_table
end

structure HashTable =
    struct
	type 'a hash_table = unit
    end

structure Node = 
    struct
	open HashTable
    end

structure Graph = GraphFn(Node)

fun refresh (reverse : Graph.graph myref) = !reverse
