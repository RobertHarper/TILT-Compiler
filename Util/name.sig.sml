signature NAME = 
  sig

    type var     (* IS  generative *)
    type label   (* NOT generative *)
    type tag
    type loc

    (* predicates *)
    val is_label_open     : label -> bool
    val is_label_barred   : label -> bool
    val is_label_internal : label -> bool
    val eq_label : (label * label) -> bool
    val eq_var   : (var * var) -> bool
    val eq_tag   : (tag * tag) -> bool

    (* generative *)
    val fresh_named_var : string -> var
    val fresh_named_tag : string -> tag
    val fresh_var : unit -> var
    val fresh_tag : unit -> tag
    val gen_var_from_symbol : Symbol.symbol -> var  (* conveniently extracts string for you *)
    val fresh_internal_label  : string -> label          
    val fresh_open_internal_label : string -> label      

    (* injective *)
    val internal_label  : string -> label
    val symbol_label    : Symbol.symbol -> label   
    val open_internal_label : string -> label      
    val open_symbol_label : Symbol.symbol -> label 
    val openlabel       : label -> label

    (* useful printing routine *)
    val var2string   : var   -> string
    val label2string : label -> string
    val tag2string   : tag  -> string

    (* Hash tables *)
    val mk_var_hash_table : (int * exn) -> (var, 'val) HashTable.hash_table

  end
