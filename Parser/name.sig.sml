(*$import Prelude Symbol HashTable ORD_SET ORD_MAP *)

signature NAME = 
  sig

    type var = int                   (* IS  generative *)
    type label = int * string        (* NOT generative *)
    type tag
    type loc

    (* predicates *)
    val is_label_internal : label -> bool

    val eq_label : (label * label) -> bool
    val eq_var   : (var * var) -> bool
    val eq_var2 : var -> var -> bool
    val eq_label2 : label -> label -> bool
    val eq_tag   : (tag * tag) -> bool
    val compare_tag : (tag * tag) -> order
    val compare_var : (var * var) -> order
    val compare_label : label * label -> order

    val labels_name_sorted_distinct : label list -> bool

    (* generative *)
    val fresh_named_var : string -> var
    val fresh_named_tag : string -> tag
    val fresh_var : unit -> var
    val fresh_tag : unit -> tag
    val gen_var_from_symbol : Symbol.symbol -> var  (* conveniently extracts string for you *)
    val fresh_internal_label  : string -> label          
    val derived_var : var -> var
    val rename_var : var * string -> unit

    (* injective *)
    val tag2int : tag -> int
    val internal_label  : string -> label
    val symbol_label    : Symbol.symbol -> label   

    (* useful printing routine *)
    val var2int      : var   -> int     (* v_23 -> 23 *)
    val var2name     : var   -> string  (* v_23 -> "v" *)
    val var2string   : var   -> string  (* v_23 -> "v_23" *)
    val label2name   : label -> string
    val label2string : label -> string
    val tag2string   : tag  -> string

    (* These should be used by NameBlast only *)
    val reset_varmap : unit -> unit  (* clear out the string part of variables *)
    val deconstruct_label : label -> int * string 
    val construct_label : int * string -> label
    val deconstruct_loc : loc -> int
    val construct_loc : int -> loc
    val deconstruct_tag : tag -> int * string
    val construct_tag : int * string -> tag

    (* Hash tables *)
    val mk_var_hash_table : (int * exn) -> (var, 'val) HashTable.hash_table

    structure VarSet : ORD_SET where type Key.ord_key = var

    structure VarMap : ORD_MAP
    where type Key.ord_key = var

    structure LabelMap : ORD_MAP
    where type Key.ord_key = label

    structure TagMap : ORD_MAP
    where type Key.ord_key = tag

    type vpath = var * label list

    structure PathMap : ORD_MAP
    where type Key.ord_key = vpath

    structure PathSet : ORD_SET
    where type Key.ord_key = vpath


   
  end
