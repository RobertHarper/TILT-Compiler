(*$import Il *)
(* Contains the initial basis context to translate in. *)
signature BASIS = 
  sig

    val empty_context : Il.context
    val initial_context : unit -> Il.context

  end
