val _ =
	let
		val g = #foo
		val _ = g {foo = 13, goo = 1.0}
		val _ = g {foo = "yes", goo = false}
	in
		()
	end
