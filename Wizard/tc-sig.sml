signature TC =
    sig

	(* typing context *)
	type context = IL.tipe Symbol.SymbolMap.map

	(* IL type equivalence check *)
	val equiv : context * IL.tipe * IL.tipe -> bool

	(* IL type synthesis and analysis *)
	exception ILTypeError
	val synth : context * IL.term -> IL.tipe
	val anal : context * IL.term * IL.tipe -> unit

	(* elaboration from EL to IL *)
	exception ElabError
	val elab_tipe : context * EL.tipe -> IL.tipe
	val elab_term : context * EL.term -> IL.term * IL.tipe
	val check_term : context * EL.term * EL.tipe -> IL.term * IL.tipe

    end
