(*$import Nil *)

signature TRAIL = 
  sig
    type trail
    val empty  : trail
    val equate : trail * (Nil.con * Nil.con) -> trail
    val equal  : trail * (Nil.con * Nil.con) -> bool
  end