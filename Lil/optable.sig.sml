(* Maps from expressions and constructors *)

signature OPTABLE =
sig

    structure Op32map : ORD_MAP where type Key.ord_key = Lil.op32
    structure Op64map : ORD_MAP where type Key.ord_key = Lil.op64

end

