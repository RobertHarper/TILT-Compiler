(*$import MACHINE GRAPH HashTableFn Graph MACHINE HashString *)
(* For procedure-level callgraph *)

structure Labelgraph :> DIRECTEDGRAPH where type node = Rtl.label =
struct
     structure HashKey =
       struct
	   open Rtl
         type hash_key = Rtl.label
         fun hashVal (ML_EXTERN_LABEL s) = HashString.hashString s
	   | hashVal (LOCAL_CODE s) = HashString.hashString s
	   | hashVal (LOCAL_DATA s) = HashString.hashString s
         val sameKey = Rtl.eq_label
       end

     structure HashTable = HashTableFn(HashKey)

     exception CG_NotFound

     structure Node : NODE =
	 struct
	     type node = Rtl.label
	     open HashTable
	     fun make i = HashTable.mkTable(i,CG_NotFound)
	 end

     structure Graph = Graph(Node)
     open Graph
end

