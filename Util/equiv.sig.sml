(*
	Finite equivalence relations.
*)
signature EQUIV =
sig
    type elem
    type equiv

    val empty : equiv
    val insert : equiv * elem * elem -> equiv
    val equiv : equiv -> elem * elem -> bool
end
