(* General -- incomplete 1996-04-19, 1996-09-30, 1997-03-12 *)
(* Modified: David Swasey *)

exception NoExceptionRaised

fun get (f:unit -> 'a) : exn =
	(f(); NoExceptionRaised) handle e => e

fun test (name:string, e:exn, ok:exn -> bool) : unit =
	let	val result =
			(case e of
				NoExceptionRaised => "WRONG no exn"
			|	_ =>
					if ok e then "OK"
					else "WRONG " ^ exnName e)
	in	print (concat[name,"\t",result,"\n"])
	end

exception E1
exception E2 = E1

val _ = app test [
	("E1", E1, fn E2 => true | _ => false),
	("E2", E2, fn E1 => true | _ => false),
	("Bind", get(fn _ => let val true = false in () end), fn Bind => true | _ => false),
	("Match", get(fn _ => (fn true => ()) false), fn Match => true | _ => false),
	("Subscript", get(fn _ => Vector.sub(vector[], ~1)), fn Subscript => true | _ => false),
	("Size", get(fn _ => Array.array(Array.maxLen+1, ())), fn Size => true | _ => false),
	("Div", get(fn _ => 1 div 0), fn Div => true | _ => false),
	("Chr", get(fn _ => chr 9999999), fn Chr => true | _ => false),
	("Fail", Fail "demo", fn Fail _ => true | _ => false),
	("Option", get(fn _ => valOf NONE), fn Option => true | _ => false),
	("Empty", get(fn _ => hd []), fn Empty => true | _ => false),
	("Io", get(fn _ => TextIO.openOut "."), fn IO.Io _ => true | _ => false)
]
