(* Chris Okasaki / Robert Harper / Frank Pfenning
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

structure Parsing :> PARSING =
struct

  open BasicParsing

  fun flat3 (a,(b,c)) = (a,b,c)
  fun flat4 (a,(b,(c,d))) = (a,b,c,d)
  fun flat5 (a,(b,(c,(d,e)))) = (a,b,c,d,e)

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard
  infixr 1 ||

  fun p && q = p -- (fn x => q -- (fn y => succeed (x,y)))
  fun p || q = p ## (fn _ => q)

  fun p wth f      = p -- succeed o f
  fun p suchthat g = p -- (fn x => if g x then succeed x else fail)
  fun p return x   = p -- (fn _ => succeed x)

  fun p guard g    = p ## (fn errpos => (g errpos; fail))

(*
  val seq = Base.foldr (fn (ph,pt) => ph && pt wth op::) (succeed [])
  val alt = Base.foldr op|| fail
*)
  fun seq ps = foldr (fn (ph,pt) => ph && pt wth op::) (succeed []) ps
  fun alt ps = foldr op|| fail ps

  fun satisfy g = any suchthat g

  fun literal t = satisfy (fn t' => t = t')

  fun string ts = seq (List.map literal ts)
  fun oneof ts  = alt (List.map literal ts)

  fun opt p = p wth SOME || succeed NONE
  fun optional f x p = p wth f || succeed x

  fun repeat p = fix (fn rep => p && rep wth op:: || succeed [])

(*
  fun repeat p = let fun rep () =  p && $rep wth op:: || succeed []
		 in $rep end
*)

  fun repeat1 p = p && repeat p wth op::

  fun first p q    = p -- (fn x => q return x)
  fun second p q   = p -- (fn _ => q)
  fun middle p q r = p -- (fn _ => q -- (fn x => r return x))

  fun (p << q) = first p q
  fun (p >> q) = second p q

  fun separate  p q = p && repeat (second q p) wth op::
  fun separate0 p q = separate p q || succeed []
  fun separate' p q = first (separate p q) (opt q)

  fun use p = p -- (fn q => q)

  (***** pre/in/post-fix parsing *****)

  datatype Associativity = LeftAssoc | RightAssoc | NonAssoc

  datatype 'a Opr =
      Prefix of int * ('a -> 'a)
    | Infix of Associativity * int * ('a * 'a -> 'a)
    | Postfix of int * ('a -> 'a)

  datatype 'a FixityItem =
      Atm of 'a
    | Opr of 'a Opr

  fun assoc (Prefix _) = NonAssoc
    | assoc (Infix(asc,_,_)) = asc
    | assoc (Postfix _) = NonAssoc

  fun prec (Prefix(n,_)) = n
    | prec (Infix(_,n,_)) = n
    | prec (Postfix(n,_)) = n

  fun resolvefixity ys =
      let fun resolve (xs,c as Atm _,ys) =
	         next (c::xs,ys)
	    | resolve (xs,c as Opr(Prefix _),ys) =
	         next (c::xs,ys)
            | resolve (x::[],c as Opr(Infix _),ys) =
	         next (c::x::[],ys)
            | resolve (x::(c' as Opr(f'))::xs,c as Opr(f as Infix _),ys) =
	         if prec(f) > prec(f') then next (c::x::c'::xs,ys)
		 else if prec(f') > prec(f) then reduce (x::c'::xs,c::ys)
                 else (case (assoc(f'),assoc(f))
			 of (LeftAssoc,LeftAssoc) => reduce (x::c'::xs,c::ys)
                          | (RightAssoc,RightAssoc) => next (c::x::c'::xs,ys)
                          | _ => fail)   (* ambiguous *)
            | resolve(x::[],c as Opr(Postfix _),ys) =
	         reduce (c::x::[],ys)
	    | resolve (x::(c' as Opr(f'))::xs,c as Opr(f as Postfix _),ys) =
	         if prec(f) > prec(f') then reduce (c::x::c'::xs,ys)
		 else if prec(f') > prec(f) then reduce (x::c'::xs,c::ys)
		 else fail     (* ambiguous *)
            | resolve _ = fail (* atm/opr mismatch *)

          and reduce (Atm(a)::Opr(Prefix(_,cprefix))::xs,ys) =
	         next(Atm(cprefix(a))::xs,ys)
	    | reduce (Atm(a)::Opr(Infix(_,_,cinfix))::Atm(a')::xs,ys) =
	         next(Atm(cinfix(a',a))::xs,ys)
            | reduce (Opr(Postfix(_,cpostfix))::Atm(a)::xs,ys) =
	         next(Atm(cpostfix(a))::xs,ys)
            | reduce _ = fail  (* atm/opr mismatch *)

          and next (Atm(a)::[],[]) = succeed a
	    | next (xs,[]) = reduce(xs,[])
	    | next (xs,y::ys) = resolve (xs,y,ys)

       in next ([],ys) end

  fun resolvefixityadj cadj ys =
      let fun resolve (Atm(a)::xs,Atm(a'),ys) =
                 next (Atm(cadj(a,a'))::xs,ys)
            | resolve (xs,Atm(a),ys) =
                 next (Atm(a)::xs,ys)
            | resolve (xs,c as Opr(Prefix _),ys) =
                 next (c::xs,ys)
            | resolve (x::[],c,ys) =
                 next (c::x::[],ys)
            | resolve ((c' as Opr _)::xs,c,ys) =
                 reduce (c'::xs,c::ys)
            | resolve (x::(c' as Opr(f'))::xs,c as Opr(f),ys) =
                 if prec(f) > prec(f') then next (c::x::c'::xs,ys)
                 else if prec(f') > prec(f) then reduce (x::c'::xs,c::ys)
                 else (case (assoc(f'),assoc(f))
                         of (LeftAssoc,LeftAssoc) => reduce (x::c'::xs,c::ys)
                          | (RightAssoc,RightAssoc) => next (c::x::c'::xs,ys)
                          | _ => fail)  (* ambiguous *)
            | resolve _ = fail  (* mismatch *)

          and reduce (Atm(a)::Opr(Prefix(_,cprefix))::xs,ys) =
                 next (Atm(cprefix(a))::xs,ys)
            | reduce (Atm(a)::Opr(Infix(_,_,cinfix))::Atm(a')::xs,ys) =
                 next (Atm(cinfix(a',a))::xs,ys)
            | reduce (Opr(Postfix(_,cpostfix))::Atm(a)::xs,ys) =
                 next (Atm(cpostfix(a))::xs,ys)
            | reduce _ = fail  (* mismatch *)

          and next (Atm(a)::[],[]) = succeed a
            | next (xs,[]) = reduce(xs,[])
            | next (xs,y::ys) = resolve(xs,y,ys)

       in next ([],ys) end

  fun parsefixity p =
      (repeat1 p) -- (fn ys => resolvefixity ys)

  fun parsefixityadj p adj =
      (repeat1 p) -- (resolvefixityadj adj)

end
