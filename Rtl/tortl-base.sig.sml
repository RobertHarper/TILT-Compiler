(*$import Rtl Nil *)
signature TORTL_BASE = 
sig

    val do_constant_records : bool ref
    val do_forced_constant_records : bool ref
    val do_gcmerge : bool ref
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

    (* local notion of register *)
    datatype reg = I of regi | F of regf

    (* RTL values can be located somewhere or the value is known *)
    datatype var_loc = 
	VREGISTER of bool * reg   (* flag indicates constant-ness *)
      | VGLOBAL of label * rep  (* I am located at this label: closure, data, ... *)

    (* The distinction between VINT and VTAG is important since they 
         have different representations.  This is important at join points. *)
    and var_val = 
	VINT of TilWord32.word   (* an integer *)
      | VTAG of TilWord32.word   (* a traceable small pointer value *)
      | VREAL of label           (* a real located at this label *)
      | VRECORD of label * var_val list (* I have the value of this label *)
      | VVOID of rep
      | VLABEL of label         (* I have the value of this label *)
      | VCODE of label          (* I have the value of this code label *)
	
   datatype loc_or_val = VAR_LOC of var_loc
                       | VAR_VAL of var_val


   type var_rep = var_loc option * var_val option * con
   type convar_rep = var_loc option * var_val option * kind
(*
   type varmap = var_rep VarMap.map
   type convarmap = convar_rep VarMap.map
*)

   (* (global) RTL translation state *)
   val reset_global_state : (var * label) list * Name.VarSet.set -> unit
   val reset_state : bool * (var * label) -> unit
   val get_code: unit -> instr list
   val set_args_result : (regi list * regf list) * reg * regi -> unit
   val add_proc : Rtl.proc -> unit
   val exports : Rtl.label list Name.VarMap.map ref 
   val get_mutable_variables : unit -> (label * rep) list
   val get_mutable_objects : unit -> label list
   val pl : Rtl.proc list ref
   val dl : Rtl.data list ref
   
   (* (local) RTL translation state *)
   type state
   val make_state : unit -> state
   val show_state : state -> unit
   val new_gcstate : state -> state
   val join_states : state list -> state
   val promote_maps : state -> state

   val add_code        : (state * var * con * label) -> state
   val add_reg         : (state * var * con * reg) -> state
   val add_var         : (state * var * con * var_loc option * var_val option) -> state
   val add_global      : (state * var * con * loc_or_val) -> state

   val add_concode     : string -> (state * var * kind * kind option * con option * label) -> state
   val add_convar      : string -> (state * var * kind * kind option * con option * var_loc option * var_val option) -> state
   val add_conglobal   : string -> (state * var * kind * kind option * con option * loc_or_val) -> state

   val getrep : state -> var -> var_rep
   val getconvarrep : state -> var -> convar_rep
   val getconvarrep' : state -> var -> convar_rep option
   val getCurrentFun : unit -> var * label
   val getLocals : unit -> (regi list * regf list)
   val getArgI : unit -> regi list
   val getArgF : unit -> regf list
   val getResult : unit -> reg
   val getTop : unit -> label
   val istoplevel : unit -> bool

   (* Type reduction and Representation functions *)
   val get_shape : state -> con -> kind
   val make_shape : state -> kind -> kind
   val simplify_type : state -> con -> bool * con
   val reduce_to_sum : string -> state -> con -> TilWord32.word * TilWord32.word option * con list
   val con2rep : state -> con -> rep
   val valloc2rep : loc_or_val -> rep
   val type_of : state -> exp -> con

   (* Routines for loading RTL values *)
   val load_ireg_loc : var_loc * regi option -> regi
   val load_ireg_val : var_val * regi option -> regi
   val load_ireg_sv : loc_or_val -> sv
   val load_ireg_locval : loc_or_val * regi option -> regi
   val load_freg_locval : loc_or_val * regf option -> regf
   val load_reg_loc : var_loc * reg option -> reg
   val load_reg_val : var_val * reg option -> reg

   (* Routines for allocating registers and labels *)
   val alloc_regi : rep -> regi
   val alloc_named_regi : var -> rep -> regi
   val alloc_regf : unit -> regf
   val alloc_named_regf : var -> regf
   val alloc_reg : state -> con -> reg
   val alloc_named_reg : state -> con * var -> reg


   (* Routines for adding code and data *)
   val add_instr : instr -> unit
   val add_data : data -> unit
   val mk_float_data : string -> label
   val mk_named_float_data : string * label -> unit
   val needgc : state * sv -> state
   val do_code_align : unit -> unit
   val align_even_word  : unit -> unit
   val align_odd_word  : unit -> unit

   (* Special Registers and Values *)
   val heapptr : regi
   val stackptr : regi
   val exnptr : regi
   val exnarg : regi
   val unitval : var_val
   val unit_vvc : loc_or_val * con

   (* Helper routines *)
   val in_ea_range : int -> loc_or_val -> int option
   val in_imm_range_vl : loc_or_val -> int option
   val coercei : string -> reg -> regi
   val coercef : reg -> regf
   val shuffle_iregs : regi list * regi list -> unit
   val shuffle_fregs : regf list * regf list -> unit
   val boxFloat : state * regf -> regi * state
   val boxFloat_vl : state * loc_or_val -> loc_or_val * state
   val unboxFloat : regi -> regf
   val fparray : state * loc_or_val list -> regi * state

   (* make_record statically allocates if all the arguments are var_vals
     make_record_const will always statically allocate
     make_record_mutable will never statically allocate *)
   val make_record : state * regi option * rep list * loc_or_val list -> loc_or_val * state
   val make_record_const : state * regi option * rep list * loc_or_val list -> loc_or_val * state
   val make_record_mutable : state * regi option * rep list * loc_or_val list -> loc_or_val * state


   (* Tag-related Operations *)
   val mk_realarraytag : regi * regi -> unit
   val mk_intarraytag : regi * regi -> unit
   val mk_ptrarraytag : regi * regi -> unit
   val store_tag_zero : TilWord32.word -> unit
   val store_tag_disp : int * TilWord32.word -> unit
   val HeapProfile : bool ref
   val SetHeapProfileCounter : int -> unit
   val MakeProfileTag : unit -> TilWord32.word


end