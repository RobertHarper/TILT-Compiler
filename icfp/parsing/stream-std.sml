structure Stream :> STREAM =
    struct

        exception Empty

        type 'a susp = unit -> 'a

        datatype 'a front = Nil | Cons of 'a * 'a T
        withtype 'a T = 'a front susp

        fun force s = s()
        fun forceTo f s = f(s())

        fun delay s =
            let
                exception Impossible
                val memo = ref (fn () => raise Impossible)
                fun s'() = let val r = s()() in memo := (fn () => r); r end
            in
                memo := s';
                fn () => (!memo)()
            end

        val empty = fn () => Nil
        fun cons (h, t) = fn () => Cons (h,t)

	fun lcons (x, f) = cons (x, delay f)

	fun is_empty s = forceTo
            (fn Nil => true
              | _ => false) s

	fun uncons s = forceTo
            (fn Nil => raise Empty
              | Cons p => p) s

	fun hd s = #1 (uncons s)
	fun tl s = #2 (uncons s)
	fun ltl s = delay (fn () => tl s)

        fun map' f s () = forceTo
            (fn Nil => empty
              | Cons (h, t) => lcons (f h, map' f t)) s
        fun map f s = delay (map' f s)

        fun app f s = forceTo
            (fn Nil => ()
              | Cons (h, t) => (f h; app f t)) s

        fun foldr a b s = forceTo
            (fn Nil => b
              | Cons (h, t) => a (h, foldr a b t)) s

        fun foldl a b s = forceTo
            (fn Nil => b
              | Cons (h, t) => foldl a (a (h, b)) t) s

        (* don't bother memoizing *)
        fun fromList nil = (fn () => Nil)
          | fromList (h::t) = (fn () => Cons(h, fromList t))

        fun toList s = foldr op:: [] s

    end
