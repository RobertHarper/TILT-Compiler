(* Chris Okasaki / Robert Harper
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

structure BasicParsing : BASIC_PARSING =
  (* LL-style parsing combinators. *)
struct

  type pos = Pos.T
  type 't stream = ('t * pos) Stream.T

  type ('a,'t) T = pos * 't stream -> 'a * pos * pos * 't stream

  infix  2 -- ##

  exception Fail of pos

  (* Primitive Parsers *)

  fun succeed x (pos,ts) = (x,pos,pos,ts)
  fun fail (pos,ts) = raise Fail pos

  fun done x (pos,ts) =
        if Stream.is_empty ts then (x,pos,pos,ts)
	else raise Fail (Pos.rightedge pos)

  fun any (pos,ts) =
    let
        val r = if Stream.is_empty ts then raise Fail (Pos.rightedge pos)
	else let val ((x,pos),ts) = Stream.uncons ts
	     in (x,pos,pos,ts) end
    in r end
                   
  fun (p -- q) (pos,ts) =
        let val (x,posx,pos,ts) = p (pos,ts)
	    val (y,posy,pos,ts) = q x (pos,ts)
	in
	    (y,Pos.union (posx,posy),pos,ts)
	end

  fun (p ## q) (pos,ts) =
        p (pos,ts)
	handle Fail err1 =>
	    q err1 (pos,ts)
	    handle Fail err2 =>
		raise Fail (Pos.max (err1,err2))

  fun lookahead p q (pos,ts) =
        let val (x,_,_,_) = p (pos,ts)
	in q x (pos,ts) end

  fun !! p (pos,ts) =
    let
      val (x,posx,pos,ts) = p (pos, ts)
    in
      ((x,posx),posx,pos,ts)
    end

  fun $ p (pos,ts) = p () (pos,ts)

  fun fix f (pos,ts) = f (fix f) (pos,ts)

  fun parsewith s f p ts =
        let val (x,_,_,_) = p (Pos.initpos,ts)
	in s x end
        handle Fail err => f err

  fun parse p = parsewith SOME (fn _ => NONE) p

  fun transform p ts =
    let
	fun trans (pos, ts) () =
            if Stream.is_empty ts then
		Stream.empty
	    else
		let
		    val (x,_,pos',ts') = p (pos,ts)
		in
		    Stream.lcons(x,trans(pos',ts'))
		end
	    handle Fail err => Stream.empty
    in
	Stream.delay(trans(Pos.initpos,ts))
    end

end
