(*
	Finite equivalence relations.
*)
functor Equiv (Map : ORD_MAP) :> EQUIV where type elem = Map.Key.ord_key =
struct
    type elem = Map.Key.ord_key
    (*
	URef is the SML/NJ library implementation of the disjoint set
	data structure described by Tarjan in Data Structures and
	Network Algorithms, SIAM, 1983.
    *)
    type class = unit URef.uref
    type equiv = class Map.map
    val empty : equiv = Map.empty

    fun class (e : equiv, elem : elem) : equiv * class =
	(case Map.find (e,elem)
	   of NONE =>
		let val c = URef.uRef ()
		in  (Map.insert (e,elem,c), c)
		end
	    | SOME c => (e,c))

    fun insert (e : equiv, a : elem, b : elem) : equiv =
	let val (e,A) = class (e,a)
	    val (e,B) = class (e,b)
	    val _ = URef.union (A,B)
	in  e
	end

    fun equiv (e : equiv) (a : elem, b : elem) : bool =
	(case (Map.find(e,a), Map.find(e,b))
	   of (SOME A, SOME B) => URef.equal (A,B)
	    | _ => Map.Key.compare (a,b) = EQUAL)
end
