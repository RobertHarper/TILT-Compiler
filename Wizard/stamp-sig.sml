signature STAMP =
    sig

	type stamp = word

	val compare : stamp * stamp -> order
	val eq : stamp * stamp -> bool

	val new_stamp : unit -> stamp

    end
