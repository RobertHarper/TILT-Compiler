(* Chris Okasaki / Robert Harper
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

(* Fixed markstream (not lazy enough) --Kevin Watkins *)

structure Pos :> POS =
  (* positions within a file (for reporting errors) *)
struct

  type Coord = {char : int,line : int}

  datatype T = At of Coord
             | Between of Coord * Coord

  val initpos = At {char = 1,line = 1}

  fun nextchar (At {char,line}) = At {char = char + 1,line = line}
    | nextchar (Between (_,{char,line})) = At {char = char + 1,line = line}

  fun nextline (At {char,line}) = At {char = 1,line = line + 1}
    | nextline (Between (_,{char,line})) = At {char = 1,line = line + 1}

  val tabsize = 8
  fun tab {char=c,line=l} = {char = c + (tabsize - (c-1) mod tabsize),
                             line = l}

  fun nl {char=c,line=l} = {char = 1, line = l+1}

  fun graph {char=c,line=l} = {char = c+1, line = l}

  fun advance (coord,#"\n") = nl coord
    | advance (coord,#"\t") = tab coord
    | advance (coord,_) = graph coord

  fun markstream s =
      let
	  fun mark (s, coord) () =
	      let
		  val (c, s) = Stream.uncons s
                  val coord' = advance(coord,c)
	      in
		  Stream.lcons((c,Between(coord,coord')),mark(s,coord'))
	      end
	      handle Stream.Empty => Stream.empty
      in
	  Stream.delay(mark(s,{char=1,line=1}))
      end

  fun right (At coord) = coord
    | right (Between (_,coord)) = coord

  fun left (At coord) = coord
    | left (Between (coord,_)) = coord

  fun rightedge pos = At (right pos)

  fun leftmost (a as {char=c1,line=l1},b as {char=c2,line=l2}) : Coord =
        if      l1 < l2 then a
        else if l2 < l1 then b
        else if c1 < c2 then a
        else (* c2 <= c1 *)  b

  fun rightmost (a as {char=c1,line=l1},b as {char=c2,line=l2}) : Coord =
        if      l1 > l2 then a
        else if l2 > l1 then b
        else if c1 > c2 then a
        else (* c2 >= c1 *)  b

  fun union (p1,p2) =
        Between (leftmost  (left p1,left p2),rightmost (right p1,right p2))
  fun max (p1,p2) =
        Between (rightmost (left p1,left p2),rightmost (right p1,right p2))
  fun min (p1,p2) =
        Between (leftmost  (left p1,left p2),leftmost  (right p1,right p2))

  fun coord2string {char,line} =
        Int.toString line ^ "." ^ Int.toString char

  fun toString (At coord) = coord2string coord
    | toString (Between (coord1,coord2 as {char,line})) =
        if coord1 = coord2 then coord2string coord1
        else coord2string coord1 ^ "-" ^ coord2string {char=char-1,line=line}

end
