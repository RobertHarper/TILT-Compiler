
(* =========================================================================
 * STACK_FRAME.sig.sml
 * ========================================================================= *)

signature STACK_FRAME = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Designates a location on the stack.
   *)
  type offset

  (*
   * A stack frame is a block of memory that holds temporary values.
   * Each stack frame is divided into regions as follows:
   *
   * +------------------------------
   * | local float n-1
   * |	 ...
   * | local float 0
   * +------------------------------
   * | local integer n-1
   * |	 ...
   * | local integer 0
   * +------------------------------
   * | return address (optional)
   * +------------------------------
   * | argument byte n-1
   * |	 ...
   * | argument byte 0
   * +------------------------------
   * 
   * Note that the physical arrangement of these regions depends on the
   * calling convention of the target architecture.
   *)
  type frame

  (* -- values ------------------------------------------------------------- *)

  (*
   * An empty stack frame.
   *)
  val empty: frame

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a new, empty stack frame.
   * <- the new stack frame
   *)
  val frame: unit -> frame

  (*
   * Allocate an area from the argument region of a given stack frame and
   * return an offset constant function for the allocated area.
   * frame <-> the stack frame to allocate the area from
   * size   -> the size of the area to allocate
   * <- the offset constant function for the area
   *)
  val allocateArgument: frame -> int -> int -> offset

  (*
   * Allocate an element from a region of a given stack frame and return an
   * offset constant for the allocated element.
   * frame <-> the stack frame to allocate the element from
   * <- the offset constant for the element
   *)
  val allocateReturn:  frame -> offset
  val allocateInteger: frame -> offset
  val allocateFloat:   frame -> offset

  (*
   * Return an offset constant from the top of a given stack frame.
   * frame  -> the stack frame to return the offset constant for
   * offset -> the offset from the top of frame
   * <- the offset constant
   *)
  val offsetTop: frame -> int -> offset

  (*
   * Return an offset constant for the size of a given stack frame.
   * frame -> the stack frame to return the size of
   * <- an offset constant representing the size of frame
   *)
  val size: frame -> offset

end

