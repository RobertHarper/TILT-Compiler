signature LILTOTALSTATE = 
  sig

    type state

    val new_state : unit -> state
    val new_state' : (Tal.int32 * Tal.int32 * Tal.int32) -> state

    val emit : state -> Tal.instruction -> state
    val emit_block : state -> Tal.label -> Tal.con option -> state

    val release_reg : state -> Tal.reg -> state

    (* Reserve a specific register *)
    val reserve_reg : state -> Tal.reg -> state

    (* Reserve a temp.  true => stack ok, false => register only.
     * Note, to ensure that requests can be satisfied, reservations 
     * should be made in the following order: 
     * 1) specific registers (reserve_reg)
     * 2) register temps (reserve_temp false)
     * 3) general temps (reserve_temp true)
     *)
    val reserve_temp : bool -> state -> (state * Tal.genop)

    val reserve_temp_reg : state -> state * Tal.reg
    val reserve_temp_any : state -> state * Tal.genop

    val reserve_for_var : bool -> state -> Lil.var -> Tal.con -> state * Tal.genop

    val reserve_next_arg32 : state -> (state * Tal.genop)
    val reserve_next_arg64 : state -> (state * Tal.genop)

    val reserve_next_inarg32 : state -> (state * Tal.genop)
    val reserve_next_inarg64 : state -> (state * Tal.genop)

    val bind_next_formal32 : state -> Lil.var -> Tal.con -> state
    val bind_next_formal64 : state -> Lil.var -> Tal.con -> state

    val init_reg : state -> Tal.reg -> Tal.genop Tal.coerce -> state
    val init_temp : state -> Tal.genop -> Tal.genop Tal.coerce -> state
    val init_var32 : state -> Tal.genop -> Lil.var -> Tal.con -> state
    val init_var64 : state -> Tal.genop -> Lil.var -> Tal.con -> state

    (* Lookup the assigned location for a variable, if one exists. *)
    val define_var : state -> Lil.var -> (state * Tal.genop option)

    val typeof_reg : state -> Tal.reg -> Tal.con option
    val typeof_stack : state -> Tal.con

    (* Get size of frame: that is, outargs and temps.  Does not include
     * inargs and return address.
     *)
    val get_frame_size : state -> Tal.int32
    (* Get size of inarg area including return address *)
    val get_inarg_size : state -> Tal.int32
    val get_outarg_size : state -> Tal.int32
    val get_temp_size : state -> Tal.int32

    val load_var : state -> Tal.reg -> Lil.var -> Tal.con -> state
    val spill_reg : state -> Tal.reg -> state

    (* add_blocks_from_state dest source:  appends blocks from source
     * to blocks from dest.  
     *)
    val add_blocks_from_state : state -> state -> state
    val get_blocks : state -> Tal.code_block list

    val pop_outargs : state -> state
    val alloc_outargs : state -> int -> int -> state
    val alloc_inargs : state -> int -> int -> state

    val coerce_state : state -> state -> state

  end