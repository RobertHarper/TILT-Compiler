(* constant.sml --- constants used to specialize MLRISC and the code generators.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature CONSTANT = sig
  type const

  val toString : const -> string
  val valueOf : const -> int
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:22  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:21  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
