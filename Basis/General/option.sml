(*$import Prelude OPTION *)
(* option.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *)

structure Option : OPTION =
  struct

    datatype 'a option = datatype option
    exception Option

    fun getOpt (SOME x, y) = x
      | getOpt (NONE, y) = y
    fun isSome (SOME _) = true
      | isSome NONE = false
    fun valOf (SOME x) = x
      | valOf _ = raise Option

    fun filter pred x = if (pred x) then SOME x else NONE
    fun join (SOME opt) = opt
      | join NONE = NONE
    fun map f (SOME x) = SOME(f x)
      | map f NONE = NONE
    fun mapPartial f (SOME x) = f x
      | mapPartial f NONE = NONE
    fun compose (f, g) x = map f (g x)
    fun composePartial (f, g) x = mapPartial f (g x)

  end;

