structure TiltCleanUp :
sig
	type tag

	val atExit : (unit -> unit) -> tag
	val remove : tag -> unit
	val replace : tag * (unit -> unit) -> unit

	val exitting : unit -> unit
end =
struct

	(* Not defined yet. *)
	fun app (f:'a -> 'b) (xs : 'a list) : unit =
		(case xs of
			nil => ()
		|	x :: xs => (f x; app f xs))

	(* Not defined yet. *)
	fun op @ (xs : 'a list, ys : 'a list) : 'a list =
		(case xs of
			nil => ys
		|	x :: xs => x :: (xs @ ys))

	type tag = int

	local
		val next : int ref = ref 0
	in
		fun tag () : tag =
			let	val r = !next
				val _ = next := r + 1
			in	r
			end
	end

	type hook = tag * (unit -> unit)
	type hooks = hook list

	val hooks : hooks ref =
		ref nil

	fun atExit (f : unit -> unit ) : tag =
		let	val tag = tag()
			val hook = (tag,f)
			val _ = hooks := hook :: (!hooks)
		in	tag
		end

	fun find (tag:tag, left:hooks, right:hooks) : (hooks * hook * hooks) option =
		(case right of
			nil => NONE
		|	(hook as (tag',_)) :: right =>
				if tag = tag' then
					SOME(rev left, hook, right)
				else
					find (tag, hook::left, right))

	fun update (tag : tag, f : hooks * hook * hooks -> hooks) : unit =
		(case (find(tag,nil,!hooks)) of
			NONE => ()
		|	SOME arg => hooks := f arg)

	fun remove (tag : tag) : unit =
		update(tag, fn (left,_,right) => left @ right)

	fun replace (hook as (tag : tag, f : unit -> unit)) : unit =
		update(tag, fn (left,_,right) => left @ (hook :: right))

	fun exitting () : unit =
		let	val _ = app (fn (_,f) => f() handle _ => ()) (!hooks)
			val _ = hooks := nil
		in	()
		end

end
