(* mono-array-fn.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * This simple functor allows easy construction of new monomorphic array
 * structures.
 *)

functor MonoArrayFn (type elem) :> MONO_ARRAY where type elem = elem =
struct
    open Array
    type array = elem Array.array
    type elem = elem
    structure Vector =
    struct
	open Vector
	type vector = elem Vector.vector
	type elem = elem
    end
end
