
(* =========================================================================
 * AlphaStandardFrame.sml
 * ========================================================================= *)

structure AlphaStandardFrame
	    :> STACK_FRAME
		 where type offset = AlphaMLRISCConstant.const
	    = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLRISCConstant = AlphaMLRISCConstant

  (* -- types -------------------------------------------------------------- *)

  type offset = MLRISCConstant.const

  (*
   * A stack frame is represented as the the maximum argument offset plus the
   * next unused offsets for the return, integer, and float regions.
   *
   * Alpha stack frames are arranged physically as follows:
   *
   * +------------------------------
   * | incoming argument n-1
   * |	 ...			     n*8 bytes
   * | incoming argument 0
   * +------------------------------ <-- incoming SP
   * | octaword pad (optional)	     0 or 8 bytes
   * +------------------------------
   * | local float n-1
   * |	 ...			     n*8 bytes
   * | local float 0
   * +------------------------------
   * | local integer pad (optional)  0 or 4 bytes
   * +------------------------------
   * | local integer n-1
   * |	 ...			     n*4 bytes
   * | local integer 0
   * +------------------------------
   * | return address (optional)     0 or 8 bytes
   * +------------------------------
   * | outgoing argument n-1
   * |	 ...			     n*8 bytes
   * | outgoing argument 0
   * +------------------------------ <-- outgoing SP
   *
   * We do not use the convention-prescribed save regions, since MLRISC
   * spills callee-saved registers and local temporaries uniformly.
   *)
  type frame = int ref * int ref * int ref * int ref

  (* -- values ------------------------------------------------------------- *)

  val empty = (ref 0, ref 0, ref 0, ref 0)

  (* -- functions ---------------------------------------------------------- *)

  fun topArgument(ref argument, _, _, _) =
	argument: int
  fun topReturn(frame as (_, ref return, _, _)) =
	return+topArgument frame
  fun topInteger(frame as (_, _, ref integer, _)) =
	integer+integer mod 8+topReturn frame
  fun topFloat(frame as (_, _, _, ref float)) =
	let
	  val total = float+topInteger frame
	in
	  total+total mod 16
	end

  fun frame() = (ref 0, ref 0, ref 0, ref 0)

  fun allocateArgument (argument, _, _, _) size =
	(argument := Int.max(!argument, size);
	 MLRISCConstant.Int)

  fun allocateReturn(frame as (_, return, _, _)) =
	(return := Int.max(!return, 8);
	 MLRISCConstant.DeferInt(fn() => topArgument frame))

  fun allocateInteger(frame as (_, _, integer, _)) =
	let
	  val offset = !integer
	in
	  integer := offset+4;
	  MLRISCConstant.DeferInt(fn() => topReturn frame+offset)
	end

  fun allocateFloat(frame as (_, _, _, float)) =
	let
	  val offset = !float
	in
	  float := offset+8;
	  MLRISCConstant.DeferInt(fn() => topInteger frame+offset)
	end

  fun offsetTop frame offset =
	MLRISCConstant.DeferInt(fn() => topFloat frame+offset)

  fun size frame = offsetTop frame 0

end

