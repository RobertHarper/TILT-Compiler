(*$import Name Graph Word32 HashTableFn *)
(* Directed graphs whose vertices are variables.   Useful for
   representing dependency information such as call graphs.*)

functor Vargraph () :> DIRECTEDGRAPH where type node = Name.var =
struct
     structure HashKey =
       struct
         type hash_key = Name.var
         fun hashVal v = Word.fromInt(Name.var2int v)
         fun sameKey (v1,v2) = Name.eq_var(v1,v2)
       end

     structure HashTable = HashTableFn(HashKey)

     exception CG_NotFound

     structure Node : NODE =
	 struct
	     type node = Name.var
	     open HashTable
	     fun make i = HashTable.mkTable(i,CG_NotFound)
	 end

     structure Graph = Graph(Node)
     open Graph
end
