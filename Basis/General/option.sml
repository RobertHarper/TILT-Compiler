(* option.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *)

structure Option :> OPTION where type 'a option = 'a option =
  struct

    datatype option = datatype option

    exception Option = Option

    val getOpt = getOpt
    val isSome = isSome
    val valOf = valOf

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

