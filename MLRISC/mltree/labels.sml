(* labels.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature LABEL =
  sig
    datatype label = Label of {id : int, addr : int ref, name:string option}

    val newLabel : string -> label
    val nameOf   : label -> string
    val id       : label -> int
    val addrOf   : label -> int
    val reset	 : unit -> unit
    val setAddr  : label * int -> unit
  end


structure Label : LABEL =
  struct
    datatype label = Label of {id:int, addr:int ref, name:string option}

    val cnt 		           = ref 0

    fun new name                   = Label{id= !cnt, addr=ref 0, name=name}
					before (cnt := !cnt + 1)
    fun newLabel ""		   = new NONE
      | newLabel name		   = new(SOME name)
    fun nameOf(Label{id,name=NONE,...})   = "LL" ^ Int.toString id
      | nameOf(Label{name=SOME lab, ...}) = lab
    fun id(Label{id,...})          = id
    fun addrOf(Label{addr,...})    = !addr
    fun setAddr(Label{addr,...},n) = addr := n
    fun reset() 		   = cnt := 0
  end


(*
 * $Log$
# Revision 1.1  99/02/17  21:17:24  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:25  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
