signature NAME = 
  sig

    type tag
    type var 
    type label
    type loc

    (* predicates *)
    val is_label_open     : label -> bool
    val is_label_barred   : label -> bool
    val is_label_internal : label -> bool
    val eq_label : (label * label) -> bool
    val eq_var   : (var * var) -> bool
    val eq_tag   : (tag * tag) -> bool

    (* generators *)
    val fresh_named_var : string -> var
    val fresh_named_int_label: string -> label
    val fresh_named_open_label: string -> label
    val fresh_named_tag : string -> tag
    val fresh_var : unit -> var
    val fresh_int_label : unit -> label
    val fresh_open_label : unit -> label
    val fresh_tag : unit -> tag
    val gen_var_from_symbol : Symbol.symbol -> var

    (* conveters *)
    val openlabel    : label -> label
    val symbol2label : Symbol.symbol -> label (* goes to a barred symbol *)

    (* useful printing routine *)
    val var2string   : var   -> string
    val label2string : label -> string
    val tag2string   : tag  -> string

  end