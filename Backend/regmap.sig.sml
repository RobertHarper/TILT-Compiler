(*$REGMAP: DICT ORD_KEY *)

signature REGMAP = DICT
(*
  sig
    structure Key : ORD_KEY

    type 'a dict

    exception NotFound

    val mkDict : unit -> '1a dict
    (* Create a new dict
     *)

    val insert : '1a dict * Key.ord_key * '1a -> '1a dict
    (* Insert an item.  
     *)

    val find : 'a dict * Key.ord_key -> 'a
    (* Find an item, raising NotFound if not found
     *)

    val peek : 'a dict * Key.ord_key -> 'a option
    (* Look for an item, return NONE if the item doesn't exist *)

    val remove : '1a dict * Key.ord_key -> '1a dict * '1a
    (* Remove an item, returning new dictionary and value removed.
     * Raise NotFound if not found
     *)

    val numItems : 'a dict ->  int
    (* Return the number of items in the table *)

    val listItems : 'a dict -> (Key.ord_key * 'a) list
    (* Return a list of the items (and their keys) in the dictionary
     *)

    val app : ((Key.ord_key * 'a) -> 'b) -> 'a dict -> unit
    (* Apply a function to the entries of the dictionary
     * in dictionary order.
     *)

    val occupant : 
      ('a -> 'a -> bool) -> 'a dict -> 'a -> Key.ord_key option
    val simpleOccupant : 
      ('a -> 'a -> bool) -> ('a dict * 'a) -> Key.ord_key option

  end
*)
