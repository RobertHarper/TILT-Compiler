(*$import *)
datatype Y = A
structure S = struct datatype D = datatype Y end
signature S = sig datatype D = A end
structure T : S = S

(*
datatype Y = A
structure S :> sig datatype D = A end =
struct
    datatype D = datatype Y
end
*)
