signature TERNARYTREE =
  sig
    (* This exception used in exactSearchExn prefixSearchExn. *)
    exception NotFound

    (* Result Type: a part of a key. *)
    type elem
    (* A key is a list of elements. *)
    type key = elem list
    (* Result Type: a ternary tree which returns data of type 'a when given a 
     * key. *)
    type 'a ternaryTree
      
    (* An empty ternary tree. *)
    val empty : 'a ternaryTree
      
    (* Given a ternary tree a key and data, return a ternary tree which maps 
     * key to data. If the key already exists replace the data for that key.*)
    val insert : 'a ternaryTree -> key -> 'a ternaryTree

  end (* TERNARYTREE *)









