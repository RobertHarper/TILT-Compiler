(*$import Prelude List TopLevel *)

signature FIFO =
  sig
    type 'a fifo

    val empty : 'a fifo
  end (* FIFO *)

structure Fifo : FIFO =
  struct
    datatype 'a fifo = Q of {front: 'a list, rear: 'a list}

    val empty = Q{front=[],rear=[]}

  end
