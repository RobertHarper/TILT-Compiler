(* Contains the initial basis context to translate in. *)
signature BASIS = 
  sig
    structure Il : IL

    val empty_context : Il.context
    val initial_context : unit -> Il.context * Il.sbnd list

  end;
