(*$import SIGINT *)
structure SigINT :> SIGINT =
struct

  fun interruptLoop (loop:unit -> unit) = loop()

end;  (* structure SigINT *)
