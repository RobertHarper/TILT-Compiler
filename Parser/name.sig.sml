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
    val reset_label_counter : unit -> unit
    val fresh_internal_label  : string -> label
    val derived_var : var -> var
    val rename_var : var * string -> unit
    val var2label : var -> label
    val label2var : label -> var

    (* injective *)
    val tag2int : tag -> int
    val internal_label  : string -> label
    val unit_label	: string -> label	(* compilation unit *)
    val interface_label	: string -> label	(* compilation unit interface *)
    val env_label	: string -> label	(* environment variable *)
    val symbol_label    : Symbol.symbol -> label

    (* useful printing routine *)
    val var2int      : var   -> int     (* v_23 -> 23 *)
    val var2name     : var   -> string  (* v_23 -> "v" *)
    val var2string   : var   -> string  (* v_23 -> "v_23" *)
    val label2name   : label -> string
    val label2longname : label -> string	(* unit U, etc *)

    val join_labels  : label list -> label (* l1, l2 -> l1__l2 *)
    val split_label  : label -> label list (* l1__l2 -> l1, l2 *)

    val label2string : label -> string
    val tag2string   : tag  -> string
    val namespace : label -> Symbol.namespace option

    (* These should be used by NameBlast only *)
    val reset_varmap : unit -> unit  (* clear out the string part of variables *)
    val deconstruct_label : label -> int * string
    val construct_label : int * string -> label
    val deconstruct_loc : loc -> int
    val construct_loc : int -> loc
    val deconstruct_tag : tag -> int * string
    val construct_tag : int * string -> tag

    (*
       There are HIL- and NIl-level conventions for internal labels.
       We collect the label-mangling functions here to simplify the
       interface and definition of keep_import.
    *)
    val to_open      : label -> label	(* module that is open for lookup *)
    val to_dt        : label -> label	(* module that implements a datatype *)
    val to_cluster   : label -> label	(* cluster of mutually recursive functions *)
    val to_eq        : label -> label	(* equality function *)
    val to_coercion  : label -> label	(* coercion function *)
    val to_sum       : label -> label   (* sum type in datatype modules *)

    val is_unit	     : label -> bool
    val is_interface : label -> bool
    val is_env       : label -> bool
    val is_open      : label -> bool
    val is_dt        : label -> bool
    val is_cluster   : label -> bool
    val is_eq        : label -> bool
    val is_coercion  : label -> bool
    val is_flat      : label -> bool 
    val is_sum       : label -> bool

    val prependToInternalLabel : string * label -> label   (* Keeps characteristics *)
    val label2name' : label -> string	(* Discards characteristics *)

    val make_cr_labels   : label -> label * label (* for use after phase splitting *)
    (* This version also makes a label corresponding to the sum component... *)
    val make_csr_labels  : label -> label * label * label (* for use after phase splitting *)

    val internal_match_tag : tag

    (*
       The compiler should never discard data associated with
       top-level labels satisfying keep_import.

       A HIL context can be (and is) garbage collected to eliminate
       unused bindings prior to phase splitting.  The phase splitter
       (optionally) filters out unused imports.  These operations, for
       example, must respect keep_import.

       The problem is that certain imports, while not necessarily
       mentioned in a module's interface or code, may be used by the
       module.  For example, the module may use a primitive which the
       optimizer evaluates to a boolean.

       Our solution is to use keep_import to identify such imports
       (via their labels) and to prevent marked imports from being
       GC'ed, filtered, etc.
     *)
    val keep_import : label -> bool

    (* Hash tables *)
    val mk_var_hash_table : (int * exn) -> (var, 'val) HashTable.hash_table

    structure VarSet : ORD_SET where type Key.ord_key = var
    structure VarMap : ORD_MAP where type Key.ord_key = var

    structure LabelMap : ORD_MAP where type Key.ord_key = label
    structure LabelSet : ORD_SET where type Key.ord_key = label

    structure TagMap : ORD_MAP where type Key.ord_key = tag

    type vpath = var * label list

    structure PathMap : ORD_MAP where type Key.ord_key = vpath
    structure PathSet : ORD_SET  where type Key.ord_key = vpath

  end
