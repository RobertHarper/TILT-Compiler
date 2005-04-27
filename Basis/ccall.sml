(*
	C calls are limited.  The sparc GC does not know about C
	roots.  Sparc C calls may perform at most one allocation and
	must consider any other ML pointers on hand to be invalidated
	by allocation.  Talx86 C calls can not raise exceptions

	Here we provide wrappers for C calls that have to throw
	exceptions.  ML allocates a cerr value, C records information
	about any error in that value, and ML examines that value to
	throw any exception.
*)
type uct = word

(*
	Must agree with ../Runtime/port/ccall.c
*)
type cerr = int ref * uct ref	(* tag, carried *)
	val Tok = 0
	val Terrno = 1
	val Terrmsg = 2

fun new () : cerr =
	let	val tag = ref Tok
		val carried = ref 0w0
	in	(tag,carried)
	end

fun check (cerr:cerr) : unit =
	let	val (ref tag,carried) = cerr
	in
		if tag = Tok then ()
		else if tag = Terrno then
			let	val e = TiltPrim.uint32toint32(!carried)
				val msg = Ccall(syserror_msg,e)
				val exn = TiltExn.SysErr(msg, SOME e)
			in	raise exn
			end
		else if tag = Terrmsg then
			let	val p = !carried
				val msg = Ccall(ccall_errmsg,p)
				val exn = TiltExn.SysErr(msg,NONE)
			in	raise exn
			end
		else raise TiltExn.LibFail "ccall bad tag"
	end

fun ccall0 (f : (cerr,'r)-->) : unit -> 'r =
	fn () =>
	let	val cerr = new()
		val r = Ccall(f,cerr)
		val () = check cerr
	in	r
	end
	
fun ccall1 (f : (cerr,'a,'r)-->) : 'a -> 'r =
	fn a =>
	let	val cerr = new()
		val r = Ccall(f,cerr,a)
		val () = check cerr
	in	r
	end
	
fun ccall2 (f : (cerr,'a,'b,'r)-->) : 'a * 'b -> 'r =
	fn (a,b) =>
	let	val cerr = new()
		val r = Ccall(f,cerr,a,b)
		val () = check cerr
	in	r
	end
	
fun ccall3 (f : (cerr,'a,'b,'c,'r)-->) : 'a * 'b * 'c -> 'r =
	fn (a,b,c) =>
	let	val cerr = new()
		val r = Ccall(f,cerr,a,b,c)
		val () = check cerr
	in	r
	end
	
fun ccall4 (f : (cerr,'a,'b,'c,'d,'r)-->) : 'a * 'b * 'c * 'd -> 'r =
	fn (a,b,c,d) =>
	let	val cerr = new()
		val r = Ccall(f,cerr,a,b,c,d)
		val () = check cerr
	in	r
	end
