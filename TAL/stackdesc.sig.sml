signature STACKDESC =
  sig
    type rstat = StateTypes.rstat

    type slot
    type stackdesc

    exception FrameTooSmall

    (* empty_stackdesc use_fp *)
    val empty_stackdesc : unit -> stackdesc

    (* empty_stackdec (finarg_size,inarg_size,temp_size,temp64_size,outarg_size)  
     * NOTE: In bytes
     *)
    val empty_stackdesc' : Lil.w32 * Lil.w32 * Lil.w32 * Lil.w32 * Lil.w32 -> stackdesc

    val reset_stackdesc : stackdesc -> stackdesc

    (* Get slot by *count*.  E.g., inarg64_slot_i is the ith inarg64 slot (not the ith word of the inarg64 region)
     *)
    val inarg_slot_i : stackdesc -> int -> slot
    val inarg64_slot_i : stackdesc -> int -> slot
    val outarg_slot_i : stackdesc -> int -> slot
    val outarg64_slot_i : stackdesc -> int -> slot
    val temp_slot_i : stackdesc -> int -> slot
    val temp64_slot_i : stackdesc -> int -> slot

    (*Give offset from stack pointer of slot.  Will always succeed,
     * but only meaningful if not using virtual frame pointer. *)
    val slot2offset : stackdesc -> slot -> Tal.int32

    val alloc_temp_slot   : stackdesc -> stackdesc * slot
    val alloc_temp64_slot   : stackdesc -> stackdesc * slot
    val alloc_outarg_slot   : stackdesc -> stackdesc * slot
    val alloc_outarg64_slot   : stackdesc -> stackdesc * slot
    (* num32 num64 *)
    val alloc_inarg_space : stackdesc -> int -> int -> stackdesc 
    val adjust_inargs : stackdesc -> int -> int -> stackdesc 

    (* Ccalls require mixed 64 and 32 bit args. *)
    val alloc_outarg_space : stackdesc -> bool list -> stackdesc

    val grow_inarg32s  : stackdesc -> Tal.int32 -> stackdesc 
    val grow_inarg64s  : stackdesc -> Tal.int32 -> stackdesc 
    val grow_temps  : stackdesc -> Tal.int32 -> stackdesc 
    val grow_ftemps  : stackdesc -> Tal.int32 -> stackdesc 
    val grow_outargs  : stackdesc -> Tal.int32 -> stackdesc 

    val eq_slot : slot -> slot -> bool
    val is_temp : slot -> bool

    val get_frame_size : stackdesc -> Tal.int32

    (* In bytes *)
    val get_inarg_size  : stackdesc -> Tal.int32
    val get_inarg32_size  : stackdesc -> Tal.int32
    val get_inarg64_size  : stackdesc -> Tal.int32
    val get_outarg_size : stackdesc -> Tal.int32
    val get_temp_size   : stackdesc -> Tal.int32
    val get_temp64_size : stackdesc -> Tal.int32

    val get_frame_elt_count : stackdesc -> int
    val get_temp64_count : stackdesc -> int
    val get_outarg_count : stackdesc -> int
    val get_inarg_count : stackdesc -> int
    val get_inarg32_count : stackdesc -> int
    val get_inarg64_count : stackdesc -> int

    val get_sstat : stackdesc -> slot -> rstat
    val set_sstat : stackdesc -> slot -> rstat -> stackdesc

    val slot2genop : stackdesc -> slot -> Tal.genop
    val genop2slot32 : stackdesc -> Tal.genop -> slot
    val genop2slot64 : stackdesc -> Tal.genop -> slot

    (* bool is true if 32 bit, false otherwise *)
    val get_outargs : stackdesc -> (rstat * bool) list

    val pop_outargs : stackdesc -> stackdesc

    val print_stack : stackdesc -> unit
    val print_slot : slot -> unit

    val stack2strings : stackdesc -> string list



    val typeof_stack : stackdesc -> LilToTalEnv.env -> Tal.con
  end