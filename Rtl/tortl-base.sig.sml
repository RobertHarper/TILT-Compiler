(*$import Prelude Name Rtl Nil TilWord32 *)
signature TORTL_BASE = 
sig

    val do_gcmerge : bool ref

    val do_constant_records : bool ref
    val do_forced_constant_records : bool ref
    val do_single_crecord : bool ref

    type exp = Nil.exp
    type con = Nil.con
    type kind = Nil.kind
    type regi = Rtl.regi
    type regf = Rtl.regf
    type rep = Rtl.rep
    type instr = Rtl.instr
    type data = Rtl.data
    type label = Rtl.label
    type var = Name.var
    type sv = Rtl.sv
    type reg = Rtl.reg

    (* A location contains a value.  Local values are in registers while those
       bound at top level reside at global labels *)
    datatype location =
	REGISTER of bool * reg   (* flag indicates whether value is constant *)
      | GLOBAL   of label * rep  (* value resides at this label: includes unboxed real *)

    (* Sometimes the value is known at compile-time.
       It is important to maintain a type distinction.  (e.g. between ints and tags) *)
    datatype value =
	VOID of rep             (* an undefined values *)
      | INT of TilWord32.word   (* an integer *)
      | TAG of TilWord32.word   (* a traceable small pointer value *)
      | REAL of label           (* an unboxed real stored at given label *)
      | RECORD of label * value list (* a record whose components are at the given label *)
      | LABEL of label          (* the value of this label: e.g. boxed real, array, vector, ... *)
      | CODE of label           (* code that residing at this label *)
	
   datatype term = LOCATION of location
                 | VALUE of value

   type var_rep = location option * value option
   type convar_rep = location option * value option

    val add_dynamic : Rtl.regi * {bitpos : int,
				 path : Rtl.rep_path} list -> unit

   (* (global) RTL translation state *)
   val set_global_state : string * (var * label) list * Name.VarSet.set -> unit
   val add_global : var -> unit
   val is_global : var -> bool
   val unset_global_state : unit -> unit
   val reset_state : bool * (var * label) -> unit
   val get_unitname : unit -> string
   val get_proc : unit -> Rtl.proc
   val set_args : reg list * regi -> unit
   val add_proc : Rtl.proc -> unit
   val exports : Rtl.label list Name.VarMap.map ref 
   val add_static_record : label * int * int -> unit   (* Number of non-heap pointer fields and (possible) heap pointer fields *)
   val get_static_records : unit -> {partialRecords : int,
				     totalRecords : int,
				     partialFields : int,
				     totalFields : int,
				     partialRecordLabels : label list}
   val pl : Rtl.proc list ref
   val dl : Rtl.data list ref
   
   (* (local) RTL translation state *)
   type state
   val make_state : unit -> state
   val show_state : state -> unit
   val new_gcstate : state -> state
   val join_states : state list -> state  (* take the join of the GC states; retain info of first state *)
   val promote_maps : state -> state

   (* These functions will allocate a global if the variable is in the global set *)
   val add_term        : (state * var * con * term * exp option) -> state  (* equation optional *)
   val add_code        : (state * var * con * label) -> state
   val add_reg         : (state * var * con * reg) -> state
   val add_conterm     : (state * var * kind * term option) -> state

   val getrep : state -> var -> var_rep
   val getconvarrep : state -> var -> convar_rep
   val getconvarrep' : state -> var -> convar_rep option
   val getCurrentFun : unit -> var * label
   val getLocals : unit -> reg list
   val getArgs : unit -> reg list
   val getResult : (unit -> reg) -> reg
   val getTop : unit -> label
   val istoplevel : unit -> bool

   (* Type reduction and Representation functions *)
   val simplify_type : state -> con -> bool * con
   val reduce_to_sum : string -> state -> con -> TilWord32.word * TilWord32.word option * con list
   val niltrace2rep : state -> Nil.niltrace -> rep
   val term2rep : term -> rep
   val type_of : state -> exp -> con
   val std_kind_of : state -> con -> kind

   (* Routines for loading RTL values *)
   val load_ireg_loc : location * regi option -> regi
   val load_ireg_val : value * regi option -> regi
   val load_ireg_sv : term -> sv
   val load_ireg_term : term * regi option -> regi
   val load_freg_term : term * regf option -> regf
   val load_reg_loc : location * reg option -> reg
   val load_reg_val : value * reg option -> reg
   val load_reg_term : term * reg option -> reg

   val repPathIsPointer : Rtl.rep_path -> regi
   val repIsNonheap : Rtl.rep -> bool

   (* Routines for allocating registers and labels *)
   val alloc_regi : rep -> regi
   val alloc_named_regi : var -> rep -> regi
   val alloc_regf : unit -> regf
   val alloc_named_regf : var -> regf
   val alloc_reg_trace : state -> Nil.niltrace -> rep * reg

   (* Routines for adding code and data *)
   val add_instr : instr -> unit
   val add_data : data -> unit
   val mk_float_data : string -> label
   val mk_named_float_data : string * label -> unit
   val needalloc : state * sv -> state
   val needmutate : state * int -> state
   val align_even_word  : unit -> unit
   val align_odd_word  : unit -> unit

   (* Special Registers and Values *)
   val heapptr : regi
   val stackptr : regi
   val exnptr : regi
   val exnarg : regi

   (* Helper routines *)
   val in_ea_range : int -> term -> int option
   val in_imm_range_vl : term -> int option
   val add : regi * int * regi -> unit  (* source, int not necessarily in immediate range, dest *)
   val shuffle_iregs : regi list * regi list -> unit
   val shuffle_fregs : regf list * regf list -> unit
   val shuffle_regs  : reg  list * reg  list -> unit
   val mv : reg * reg -> instr (* src -> dest *)
   val boxFloat : state * regf -> regi * state
   val boxFloat_vl : state * term -> term * state
   val unboxFloat : regi -> regf
   val fparray : state * term list -> regi * state

   (* Tag-related Operations - length always measured in bytes *)
   val mk_word_arraytag : regi * regi -> unit
   val mk_quad_arraytag : regi * regi -> unit
   val mk_ptr_arraytag : regi * regi -> unit
   val mk_mirror_ptr_arraytag : regi * regi -> unit
   val store_tag_zero : TilWord32.word -> unit
   val store_tag_disp : int * TilWord32.word -> unit

   (* Recording stats *)
   val incSelect : unit -> unit
   val incRecord : unit -> unit
   val incApp : unit -> unit
   val incFun : unit -> unit
   val incClosure : unit -> unit
   val incCase : unit -> unit
   val incSumInject : unit -> unit
   val incSumProject : unit -> unit
   val incSumDynInject : unit -> unit
   val incSumDynProject : unit -> unit
   val incVararg : unit -> unit
   val incOnearg : unit -> unit
   val incPrim : unit -> unit
   val incMutate : unit -> unit

end
