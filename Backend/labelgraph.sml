(*$import MACHINE GRAPH HashTableFn Graph *)
(* For procedure-level callgraph *)

functor Labelgraph (structure Machine : MACHINE) : DIRECTEDGRAPH =
struct
    open Machine.Rtl
     structure HashKey =
       struct
         type hash_key = Machine.loclabel
         fun hashVal (LOCAL_CODE v) = Word.fromInt (Name.var2int v)
	   | hashVal (LOCAL_DATA v) = Word.fromInt ((Name.var2int v) + 1000000)
         fun sameKey (LOCAL_CODE v1, LOCAL_CODE v2) = Name.eq_var(v1,v2)
         fun sameKey (LOCAL_DATA v1, LOCAL_DATA v2) = Name.eq_var(v1,v2)
	   | sameKey _ = false
       end
     structure HashTable : MONO_HASH_TABLE = HashTableFn(HashKey)
     exception LG_NotFound
     structure Node : NODE =
	 struct
	     type node = Machine.loclabel
	     open HashTable
	     fun make i = HashTable.mkTable(i,LG_NotFound)
	 end
     structure Graph = Graph(Node)
     open Graph
end
