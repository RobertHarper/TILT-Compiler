signature LILTOTALSTATE = 
  sig

    val debuglev : int ref
    val chatlev : int ref

    datatype con = LilCon of Lil.con | TalCon of Tal.con 

    type state
    exception FrameTooSmall

    val new_state : unit -> state
    val new_state' : (Tal.int32 * Tal.int32 * Tal.int32 * Tal.int32 * Tal.int32) -> state
    val copy_state : state -> state
    val reset_state : state -> state
    val emit : state -> Tal.instruction -> state
    val emitn : state -> Tal.instruction list -> state
    val emit_block : state -> Tal.label -> Tal.con option -> state

    val commentlev : int ref
    val comment : state -> int -> string -> state
    val comment_stack : state -> int -> state
    val print_stack : state -> unit
    val print_state : state -> unit

    (* FREE:
     * Use this protocol for destination registers.  Ensures that 
     * the register is not allocated to anything (spilling if necessary).
     *)
    val free_reg : state -> Tal.reg -> state

    (* Get a free temp.  true => stack ok, false => register only.
     *)
    val free_temp : bool -> state -> (state * Tal.genop)

    val free_temp_reg : state -> state * Tal.reg
    val free_temp_any : state -> state * Tal.genop


    (* RESERVE/RELEASE:
     * Use this protocol for source registers. 
     * RESERVE: spills the register if necessary, and locks it down
     * to prevent the allocator from double allocating or spilling it.
     * RELEASE: unlocks the register, so that it is available for
     * spilling and allocation.  Note that init_xxx functions release.
     *)


    val release_reg : state -> Tal.reg -> state
    val reserve_reg : state -> Tal.reg -> state
    val reserve_one_of_regs : state -> Tal.reg list -> (state * Tal.reg) 

    (* Reserve a temp.  true => stack ok, false => register only.
     * Note, to ensure that requests can be satisfied, reservations 
     * should be made in the following order: 
     * 1) specific registers (reserve_reg)
     * 2) register temps (reserve_temp false)
     * 3) general temps (reserve_temp true)
     *)
    val reserve_temp_slot : state -> (state * Tal.genop)
    val release_temp : state -> Tal.genop -> state  
    val release_temp64 : state -> Tal.genop -> state  
    val reserve_temp : bool -> state -> (state * Tal.genop)
    val reserve_temp_reg : state -> state * Tal.reg
    val reserve_temp_any : state -> state * Tal.genop

    val reserve_for_var : bool -> state -> Lil.var -> con -> state * Tal.genop
    val reserve_for_var64 : state -> Lil.var -> con -> state * Tal.genop

    val reserve_next_arg32 : state -> (state * Tal.genop)
    val reserve_next_arg64 : state -> (state * Tal.genop)

    val reserve_last_inarg32 : state -> (state * Tal.genop)

    val reserve_inarg32 : state -> int -> (state * Tal.genop)
    val reserve_inarg64 : state -> int -> (state * Tal.genop)

    val bind_formal32 : state  -> Lil.var -> con -> int -> state
    val bind_formal64 : state  -> Lil.var -> con -> int -> state

    (* Initialize and RELEASE *)
    val init_reg : state -> Tal.reg -> Tal.genop Tal.coerce -> state
    val init_temp : state -> Tal.genop -> Tal.genop Tal.coerce -> state
    val init_var32 : state -> Tal.genop -> Lil.var -> con -> state
    val init_var64 : state -> Tal.genop -> Lil.var -> con -> state

    (* shift_temp state g1 g2 
     * PRE: g1 is reserved, and g2 is available
     * POST: g2 is reserved, and g1 is available
     * ACTION: emits code to spill g2 if necessary, and to initialize g1 from g2 if necessary.
     * NOTE: handles g1 == g2
     *)
    val shift_temp : state -> Tal.genop -> Tal.genop -> state

    (* Load specific register and RESERVE for var *)
    val load_var : state -> Tal.reg -> Lil.var -> con -> state

    val kill_var32 : state -> Lil.var -> state
    val kill_var64 : state -> Lil.var -> state

    val define_var32 : bool -> state -> Lil.var -> (state * Tal.genop)
    val define_var64 : state -> Lil.var -> (state * Tal.genop)
    val define_var_from_reg : state -> Lil.var -> Tal.reg -> state

    val var32_used : state -> Lil.var -> bool
    val var64_used : state -> Lil.var -> bool

    val typeof_reg : state -> LilToTalEnv.env -> Tal.reg -> Tal.con option
    val typeof_reg' : state -> LilToTalEnv.env -> Tal.reg -> Tal.con 
    val typeof_stack : state -> LilToTalEnv.env -> Tal.con
    val typeof_state : state -> LilToTalEnv.env -> Tal.con
    val typeof_state' : state -> LilToTalEnv.env -> Tal.machine_state

    (* Get size of frame: that is, outargs and temps.  Does not include
     * inargs and return address.
     *)
    val get_frame_size : state -> Tal.int32
    (* Get size of inarg area including return address *)
    val get_inarg_size : state -> Tal.int32
    val get_inarg32_size : state -> Tal.int32
    val get_inarg64_size : state -> Tal.int32
    val get_outarg_size : state -> Tal.int32
    val get_temp_size : state -> Tal.int32
    val get_temp64_size : state -> Tal.int32



    (* add_blocks_from_state dest source:  appends blocks from source
     * to blocks from dest.  
     *)
    val add_blocks_from_state : state -> state -> state

    (* Ensure that the second state's frame is as big as the first *)
    val grow_state : state -> state -> state

    (* add_blocks_from_state dest source:  prepends blocks from source
     * to blocks from dest.  
     *)
    val add_blocks_before_from_state : state -> state -> state

    val get_blocks : state -> Tal.code_block list
    val flush_blocks : state -> state

    val pop_outargs : state -> state
    val alloc_outargs : state -> int -> int -> state
    val alloc_outargs' : state -> bool list -> state
    val alloc_inargs : state -> int -> int -> state
    val adjust_inargs : state -> int -> int -> state

    val revert_state : state -> state -> state
    val match_state : state -> state -> state

    val sanity_check : state -> unit
    val get_live_vars32 : state -> Lil.var list
    val get_live_vars64 : state -> Lil.var list
    val get_frame_elt_count : state -> int
    val get_outarg_count : state -> int

    (* We maintain the invariant that the temp64 slots
     * are always viewed as having 64 bit types.  To do this,
     * we must initially coerce them to have type pcjunk8, instead
     * of pcjunk4 :: pcjunk4.
     *)
    val setup_temp64s : state -> state
  end