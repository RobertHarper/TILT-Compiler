(* For procedure-level callgraph *)

structure Labelgraph :> DIRECTEDGRAPH where type node = Rtl.label =
struct
    structure Key =
    struct
	type hash_key = Rtl.label
	val hashVal = Rtl.hash_label
	val sameKey = Rtl.eq_label
    end
    structure Node = Node(structure Key = Key
			  val toString = Pprtl.label2s)
    structure Graph = Graph(Node)
    open Graph
end
