signature NAME = 
  sig

    type var     (* IS  generative *)
    type label   (* NOT generative *)
    type tag
    type loc

    (* predicates *)
    val is_label_open     : label -> bool
(*
    val is_label_barred   : label -> bool
    val is_label_internal : label -> bool
*)
    val eq_label : (label * label) -> bool
    val eq_var   : (var * var) -> bool
    val eq_tag   : (tag * tag) -> bool
    val compare_tag : (tag * tag) -> order
    val compare_var : (var * var) -> order
    val compare_label : (label * label) -> order

    (* generative *)
    val fresh_named_var : string -> var
    val fresh_named_tag : string -> tag
    val fresh_var : unit -> var
    val fresh_tag : unit -> tag
    val gen_var_from_symbol : Symbol.symbol -> var  (* conveniently extracts string for you *)
    val fresh_internal_label  : string -> label          
    val fresh_open_internal_label : string -> label      

    (* injective *)
    val tag2int : tag -> int
    val internal_label  : string -> label
    val symbol_label    : Symbol.symbol -> label   
    val open_internal_label : string -> label      
    val open_symbol_label : Symbol.symbol -> label 
    val openlabel       : label -> label

    (* useful printing routine *)
    val var2int      : var   -> int
    val var2string   : var   -> string
    val label2string : label -> string
    val tag2string   : tag  -> string

    (* Hash tables *)
    val mk_var_hash_table : (int * exn) -> (var, 'val) HashTable.hash_table

    structure VarSet : ORD_SET
    sharing type VarSet.Key.ord_key = var

    structure VarMap : ORD_MAP
    sharing type VarMap.Key.ord_key = var

    structure LabelMap : ORD_MAP
    sharing type LabelMap.Key.ord_key = label

    structure TagMap : ORD_MAP
    sharing type TagMap.Key.ord_key = tag

    type vpath = var * label list
    structure PathMap : ORD_MAP
    sharing type PathMap.Key.ord_key = vpath

  end
