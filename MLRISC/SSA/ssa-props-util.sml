structure SSAPropsUtil : SSA_PROPERTIES_UTIL =
struct

   structure E = LabelExp
   structure L = Label

   local

      fun hash(E.LABEL(L.Label{id,...})) = id + 10000
        | hash(E.CONST i) = i
        | hash(E.PLUS(x,y)) = hash x + hash y + 20000
        | hash(E.MINUS(x,y)) = hash x + hash y + 30000
        | hash(E.MULT(x,y)) = hash x + hash y + 40000
        | hash(E.DIV(x,y)) = hash x + hash y + 50000
        | hash(E.LSHIFT(x,i)) = hash x + Word.toIntX i + 60000
        | hash(E.RSHIFT(x,i)) = hash x + Word.toIntX i + 7000 
        | hash(E.AND(x,i)) = hash x + Word.toIntX i + 80000
        | hash(E.OR(x,i)) = hash x + Word.toIntX i + 90000
   
      fun eq(E.LABEL(L.Label{id=x,...}),E.LABEL(L.Label{id=y,...})) = x=y
        | eq(E.CONST i, E.CONST j) = i = j
        | eq(E.PLUS(a,b), E.PLUS(c,d)) = eq(a,c) andalso eq(b,d)
        | eq(E.MINUS(a,b), E.MINUS(c,d)) = eq(a,c) andalso eq(b,d)
        | eq(E.MULT(a,b), E.MULT(c,d)) = eq(a,c) andalso eq(b,d)
        | eq(E.DIV(a,b), E.DIV(c,d)) = eq(a,c) andalso eq(b,d)
        | eq(E.LSHIFT(a,b), E.LSHIFT(c,d)) = b = d andalso eq(a,c)
        | eq(E.RSHIFT(a,b), E.RSHIFT(c,d)) = b = d andalso eq(a,c)
        | eq(E.AND(a,b), E.AND(c,d)) = b = d andalso eq(a,c)
        | eq(E.OR(a,b), E.OR(c,d)) = b = d andalso eq(a,c)
        | eq _ = false 
   
   in
   
      fun hashLabel(L.Label{id,...}) = id
      fun eqLabel(L.Label{id=x,...},L.Label{id=y,...}) = x = y

      val hashLabelExp = hash
      val eqLabelExp = eq

   end
   
end
   
