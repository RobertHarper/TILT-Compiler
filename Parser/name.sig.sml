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
*)
    val is_label_internal : label -> bool

    val eq_label : (label * label) -> bool
    val eq_var   : (var * var) -> bool
    val eq_var2 : var -> var -> bool
    val eq_label2 : label -> label -> bool
    val eq_tag   : (tag * tag) -> bool
    val compare_tag : (tag * tag) -> order
    val compare_var : (var * var) -> order
    val compare_label : (label * label) -> order

    val labels_sorted_distinct : label list -> bool

    (* generative *)
    val fresh_named_var : string -> var
    val fresh_named_tag : string -> tag
    val fresh_var : unit -> var
    val fresh_tag : unit -> tag
    val gen_var_from_symbol : Symbol.symbol -> var  (* conveniently extracts string for you *)
    val fresh_internal_label  : string -> label          
    val fresh_open_internal_label : string -> label      
    val derived_var : var -> var

    (* non-generative; used for genarating entry labels in tortl.. - Martin *)
    val non_generative_named_var : string -> var

    (* injective *)
    val tag2int : tag -> int
    val internal_label  : string -> label
    val symbol_label    : Symbol.symbol -> label   
    val open_internal_label : string -> label      
    val open_symbol_label : Symbol.symbol -> label 
    val openlabel       : label -> label


    (* useful printing routine *)
    val var2int      : var   -> int     (* v_23 -> 23 *)
    val var2name     : var   -> string  (* v_23 -> "v" *)
    val var2string   : var   -> string  (* v_23 -> "v_23" *)
    val label2string : label -> string
    val tag2string   : tag  -> string

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


    (* blasting routines *)
    val blastInVar    : TextIO.instream -> var
    val blastOutVar   : TextIO.outstream -> var -> unit
    val blastInLabel  : TextIO.instream -> label
    val blastOutLabel : TextIO.outstream -> label -> unit
    val blastInTag    : TextIO.instream -> tag
    val blastOutTag   : TextIO.outstream -> tag -> unit

    val blastOutVarmap   : TextIO.outstream -> 'a Blaster.blastout -> 'a VarMap.map -> unit
    val blastInVarmap    : TextIO.instream -> 'a Blaster.blastin -> 'a VarMap.map
    val blastOutLabelmap   : TextIO.outstream -> 'a Blaster.blastout -> 'a LabelMap.map -> unit
    val blastInLabelmap    : TextIO.instream -> 'a Blaster.blastin -> 'a LabelMap.map
    val blastOutTagmap   : TextIO.outstream -> 'a Blaster.blastout -> 'a TagMap.map -> unit
    val blastInTagmap    : TextIO.instream -> 'a Blaster.blastin -> 'a TagMap.map


  end
