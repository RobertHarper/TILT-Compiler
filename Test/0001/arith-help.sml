(*$import IEEEReal *)

structure ArithHelp :>
    sig
	val CHR : ('a -> 'b) -> 'a -> string -> unit (* Expects Chr *)
	val OVF : ('a -> 'b) -> 'a -> string -> unit (* Expects Overflow *)
	val DIV : ('a -> 'b) -> 'a -> string -> unit (* Expects Div *)
	val DOM : ('a -> 'b) -> 'a -> string -> unit (* Expects Domain *)
	val UNO : ('a -> 'b) -> 'a -> string -> unit (* Expects IEEEReal.Unordered *)
    end =
struct
    fun trace (what, name) = print ("testing " ^ name ^ " " ^ what ^ "...")
    fun trace' true = print "ok\n"
      | trace' false = print "failed\n"

    fun CHR f a s =
	let val _ = trace("chr", s)
	    val ok = ((ignore(f a); false)
		      handle Chr => true)
	in  trace' ok
	end
    
    fun OVF f a s =
	let val _ = trace("overflow", s)
	    val ok = ((ignore(f a); false)
		      handle Overflow => true)
	in  trace' ok
	end

    fun DIV f a s =
	let val _ = trace("div", s)
	    val ok = ((ignore(f a); false)
		      handle Div => true)
	in  trace' ok
	end

    fun DOM f a s =
	let val _ = trace("domain", s)
	    val ok = ((ignore(f a); false)
		      handle Domain => true)
	in  trace' ok
	end
    
    fun UNO f a s =
	let val _ = trace("unordered", s)
	    val ok = ((ignore(f a); false)
		      handle IEEEReal.Unordered => true)
	in  trace' ok
	end
end

