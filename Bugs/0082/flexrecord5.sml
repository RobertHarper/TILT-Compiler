val _ =
	let
		val f = #foo
		val g = fn h => fn y => h (f y)
		val h = fn x => f x
		val _ = f {foo=0, bar=1}
	in
		()
	end
