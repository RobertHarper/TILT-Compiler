signature HASH_VAL =
    sig

	type hashval = word

	val nullary : word -> hashval
	val unary : word * Stamp.stamp -> hashval
	val binary : word * Stamp.stamp * Stamp.stamp -> hashval
	val ternary : word * Stamp.stamp * Stamp.stamp * Stamp.stamp -> hashval
	val quaternary : word * Stamp.stamp * Stamp.stamp * Stamp.stamp * Stamp.stamp -> hashval

	val n_ary : word * Stamp.stamp list -> hashval

    end
