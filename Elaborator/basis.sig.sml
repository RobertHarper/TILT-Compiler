(*$import Il *)
(* Contains the initial basis context to translate in. *)
signature BASIS = 
  sig
    val empty_context : Il.context
    val tiltprim : Il.context -> Il.partial_context
  end
