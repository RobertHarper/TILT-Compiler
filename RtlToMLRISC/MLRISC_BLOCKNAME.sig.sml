(* MLRISC_BLOCKNAME
 * 
 * The type name, is used to label basic blocks.
 *)
signature MLRISC_BLOCKNAME = sig
  type name
  val default : name
  val toString : name -> string
end
