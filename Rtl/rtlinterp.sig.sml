signature RTLINTERP =
  sig
    type module
    type quad_val
    val Overflow_exnval : int;
    val DivideZero_exnval : int;
    val RTL_interp : (string * module) list * (quad_val list * quad_val list) * bool
                          -> (quad_val list * quad_val list) list;

    val print_dump : bool ref
    val print_dump_callret : bool ref
    val outstream : (TextIO.outstream option) ref
	
  end
