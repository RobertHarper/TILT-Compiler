signature PROBABILITY =
sig
   eqtype prob

   val prob     : int * int -> prob
   val zero     : prob
   val one      : prob
   val <        : prob * prob -> bool
   val >        : prob * prob -> bool
   val >=       : prob * prob -> bool
   val <=       : prob * prob -> bool
   val !=       : prob * prob -> bool
   val ==       : prob * prob -> bool
   val compare  : prob * prob -> order
   val avg      : (prob * int) list -> int
   val avg_prob : (prob * prob) list -> prob
   val +        : prob * prob -> prob
   val -        : prob * prob -> prob
   val *        : prob * real -> prob
   val /        : prob * real -> prob
   val toString : prob -> string
   val toReal   : prob -> real
   val fromReal : real -> prob
end

structure Probability :> PROBABILITY =
struct

   structure W = Word

   type prob = W.word
  
   val word = W.fromInt

   val <<   = W.<<
   val >>   = W.>>
   val op + = W.+
   val op - = W.-

   infix << >>

   val shift   = 0w16
   val shift2  = 0w8
   val one     = 0w1 << shift
   val half    = one >> 0w1
   val half'   = (0w1 << shift2) >> 0w1
   val realOne = Real.fromInt(Word.toInt one)
   val zero    = 0w0
   val op <    = W.<
   val op >    = W.>
   val op >=   = W.>=
   val op <=   = W.<=
   val !=      = fn (i:word,j:word) => i <> j
   val ==      = fn (i:word,j:word) => i = j
   val compare = W.compare 

   fun prob (x,y)   = 
       let val h = word y 
       in W.div((word x << shift) + (h >> 0w1),h) end
   fun fromReal r   = Word.fromInt(Real.round(r * realOne))
   fun toReal p     = Real.fromInt(Word.toInt p) / realOne
   fun toString p   = Real.toString(Real.fromInt(
                          Real.round(toReal p * 100.0))/100.0)

   fun p * r = fromReal(Real.*(toReal p, r))
   fun p / r = fromReal(Real./(toReal p, r))

   fun avg [] = 0
     | avg l  =
   let fun f([],s,n)       = (s,n)
         | f((p,i)::l,s,n) = f(l,W.*(p,word i) + s,n+0w1)
       val (sum,n) = f(l,zero,0w0)
   in  Word.toInt(W.div(sum,n))
   end

   fun avg_prob [] = zero
     | avg_prob l =
   let fun f([],s,n)       = (s,n)
         | f((p,i)::l,s,n) = f(l,W.*((p+half')>>shift2,(i+half')>>shift2) + s,
                               n+0w1)
       val (sum,n) = f(l,zero,0w0)
   in  W.div(sum,n) 
   end


end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:10  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:58  pscheng
# *** empty log message ***
#
 *)
