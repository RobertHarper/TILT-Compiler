structure S =
struct
	val f = #foo
end

val _ = S.f {foo=1, goo=2}
