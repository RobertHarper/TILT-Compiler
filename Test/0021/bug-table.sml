structure ConKey =
struct
	type ord_key = int
end
structure Conmap = BinaryMapFn(ConKey)
