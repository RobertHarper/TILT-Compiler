(*$import LIST *)
(* list.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Available (unqualified) at top level:
 *   type list
 *   val nil, ::, hd, tl, null, length, @, app, map, foldr, foldl, rev
 *
 * Consequently the following are not visible at top level:
 *   val last, nth, take, drop, concat, revAppend, mapPartial, find, filter,
 *       partition, exists, all, tabulate
 *   exception Empty
 *
 * The following infix declarations will hold at top level:
 *   infixr 5 :: @
 *
 *)

structure List : LIST =
  struct

(*
    val op +  = InlineT.DfltInt.+
    val op -  = InlineT.DfltInt.-
    val op <  = InlineT.DfltInt.<
    val op <= = InlineT.DfltInt.<=
    val op >  = InlineT.DfltInt.>
    val op >= = InlineT.DfltInt.>=
    val op =  = InlineT.=
*)
    datatype list = datatype list

    exception Empty

    fun null [] = true | null _ = false

    fun hd (h::_) = h | hd _ = raise Empty
    fun tl (_::t) = t | tl _ = raise Empty
    fun last [] = raise Empty
      | last [x] = x
      | last (_::r) = last r

    fun getItem [] = NONE
      | getItem (x::r) = SOME(x, r)

    fun nth (l,n) = let
          fun loop ((e::_),0) = e
            | loop ([],_) = raise Subscript
            | loop ((_::t),n) = loop(t,n-1)
          in
            if n >= 0 then loop (l,n) else raise Subscript
          end

    fun take (l, n) = let
          fun loop (l, 0) = []
            | loop ([], _) = raise Subscript
            | loop ((x::t), n) = x :: loop (t, n-1)
          in
            if n >= 0 then loop (l, n) else raise Subscript
          end

    fun drop (l, n) = let
          fun loop (l,0) = l
            | loop ([],_) = raise Subscript
            | loop ((_::t),n) = loop(t,n-1)
          in
            if n >= 0 then loop (l,n) else raise Subscript
          end

    fun length l = let 
          fun loop(acc,[]) = acc
            | loop(acc, _::x) = loop(acc+1,x)
          in loop(0,l) end

    fun rev l = let 
          fun loop ([], acc) = acc
            | loop (a::r, acc) = loop(r, a::acc)
          in
	    loop (l, [])
	  end

    fun op @(x,[]) = x
      | op @(x,l) = let 
          fun f([],l) = l
            | f([a],l) = a::l
            | f([a,b],l) = a::b::l
            | f([a,b,c],l) = a::b::c::l
            | f(a::b::c::d::r,l) = a::b::c::d::f(r,l)
          in f(x,l) end

    fun concat [] = []
      | concat (l::r) = l @ concat r

    fun revAppend ([],l) = l
      | revAppend (h::t,l) = revAppend(t,h::l)

    fun app f = let 
          fun a2 (e::r) = (f e; a2 r) | a2 [] = () 
          in a2 end

    fun map f = let 
          fun m [] = []
            | m [a] = [f a]
            | m [a,b] = [f a, f b]
            | m [a,b,c] = [f a, f b, f c]
            | m (a::b::c::d::r) = f a :: f b :: f c :: f d :: m r
          in m end

    fun mapPartial pred l = let
          fun mapp ([], l) = rev l
            | mapp (x::r, l) = (case (pred x)
                 of SOME y => mapp(r, y::l)
                  | NONE => mapp(r, l)
                (* end case *))
          in
            mapp (l, [])
          end

    fun find pred [] = NONE
      | find pred (a::rest) = if pred a then SOME a else (find pred rest)

    fun filter pred [] = []
      | filter pred (a::rest) = if pred a then a::(filter pred rest) 
                                else (filter pred rest)

    fun partition pred l = let
          fun loop ([],trueList,falseList) = (rev trueList, rev falseList)
            | loop (h::t,trueList,falseList) = 
                if pred h then loop(t, h::trueList, falseList)
                else loop(t, trueList, h::falseList)
          in loop (l,[],[]) end

    fun foldr f b = let
          fun f2 [] = b
            | f2 (a::t) = f(a,f2 t)
          in f2 end

    fun foldl f b l = let
          fun f2 ([],b) = b
            | f2 (a::r,b) = f2(r,f(a,b))
          in f2 (l,b) end

    fun exists pred = let 
          fun f [] = false
            | f (h::t) = pred h orelse f t
          in f end
    fun all pred = let 
          fun f [] = true
            | f (h::t) = pred h andalso f t
          in f end

    fun tabulate (len, genfn) = 
          if len < 0 then raise Size
          else let
            fun loop n = if n = len then []
                         else (genfn n)::(loop(n+1))
            in loop 0 end

  end (* structure List *)

(*
 * $Log$
# Revision 1.1  98/03/09  19:50:16  pscheng
# added basis
# 
 * Revision 1.2  1997/06/02  19:15:02  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:15  george
 *   Version 109.24
 *
 *)
