functor SSAConstantFoldingFn(SSA : SSA) : SSA_CONSTANT_FOLDING =
struct

   structure SSA  = SSA
   structure SP   = SSA.SP
   structure E    = SSAExp
   structure G    = Graph

   type valnum = int

   val bot      = ~98765432
   val top      = ~10000000
   val volatile = ~12345678 (* this value is not equal to itself! *)
   val zero     = ~1
   val one      = ~2

   fun error msg = MLRiscErrorMsg.impossible("SSAConstantFolding."^msg)

   fun hashOp(opcode,rs) =
       let fun sum([],h) = h
             | sum(r::rs,h) = sum(rs,h+r)
       in  sum(rs,E.hash opcode) end

   (*
    * For everything except 
    *)
   fun equalOp((op1 : E.exp,rs1 : int list),(op2 : E.exp,rs2 : int list)) =
       op1 = op2 andalso
          let fun eqList(a::b,c::d) = 
                  a <> ~12345678 andalso a = c andalso eqList(b,d)
                | eqList([],[]) = true
                | eqList(_,_) = false
          in  eqList(rs1,rs2) end
 
   fun word i = word i
   fun orb(i,j)  = Word.toInt(Word.orb(word i,word j))
   fun andb(i,j) = Word.toInt(Word.andb(word i,word j))
   fun xorb(i,j) = Word.toInt(Word.xorb(word i,word j))
   fun notb i    = Word.toInt(Word.notb(word i))
   fun sll(i,j)  = Word.toInt(Word.<<(word i,word j))
   fun srl(i,j)  = Word.toInt(Word.>>(word i,word j))
   fun sra(i,j)  = Word.toInt(Word.~>>(word i,word j))

   fun cmp(E.EQ,i,j)  = if i = j then ~2 else ~1
     | cmp(E.NE,i,j)  = if i <> j then ~2 else ~1
     | cmp(E.LT,i,j)  = if i < j then ~2 else ~1
     | cmp(E.LE,i,j)  = if i <= j then ~2 else ~1
     | cmp(E.GT,i,j)  = if i > j then ~2 else ~1
     | cmp(E.GE,i,j)  = if i >= j then ~2 else ~1
     | cmp(E.LTU,i,j) = if word i < word j then ~2 else ~1
     | cmp(E.LEU,i,j) = if word i <= word j then ~2 else ~1
     | cmp(E.GTU,i,j) = if word i > word j then ~2 else ~1
     | cmp(E.GEU,i,j) = if word i >= word j then ~2 else ~1
     | cmp(E.SETCC,_,_) = error "cmp"

   fun hashTable (n,exn) =
       HashTable.create{== = equalOp,hash=hashOp,size=n,exn=exn}

   val LT  = E.BINOP(E.CMP E.LT,E.I32,E.ID 0,E.ID 1)
   val LE  = E.BINOP(E.CMP E.LE,E.I32,E.ID 0,E.ID 1)
   val LTU = E.BINOP(E.CMP E.LTU,E.I32,E.ID 0,E.ID 1)
   val LEU = E.BINOP(E.CMP E.LEU,E.I32,E.ID 0,E.ID 1)
   (*
    * Compute the value number 
    *)
   fun constantFolding (SSA as G.GRAPH ssa) lookup =
   let val const = SSA.const SSA 
       val immed = SSA.immed SSA

       fun process(e as E.UNARY(opcode,E.I32,E.ID 0),xs as [x],w) = 
             if x < 0 then unary(e,opcode,xs,x,w)
             else lookup(e,xs,w)
         | process(e as E.BINOP(opcode,E.I32,E.ID 0,E.ID 1),xs as [x,y],w) = 
             binop(e,opcode,xs,x,y,w)
         | process(E.LI,[a],_) = a
         | process(E.COPY,[a],_) = a
         | process(e as E.PHI _,xs,w) = 
             let fun uniq([],x) = x
                   | uniq(v::vs,~10000000) = uniq(vs,v)
                   | uniq(v::vs,x) = if x = v then uniq(vs,x)
                                     else lookup(e,xs,w)
             in  uniq(xs,~10000000)
             end
         | process(e,xs,w) = lookup(e,xs,w)

       (* constant folding for unary operators *)
       and unary(e,opcode,xs,x,w) =
           case (opcode,const x) of
             (E.NEG,SP.IMMED i) => immed(~i)
           | (E.NOT,SP.IMMED i) => immed(if i = ~1 then ~2 else ~1)
           | (E.NOTB,SP.IMMED i) => immed(notb i)
           | (E.ABS,SP.IMMED i) => immed(if i < 0 then ~i else i)
           | _ => lookup(e,xs,w)

       (* constant folding for binary operators *)
       and binop(e,opcode,xs,x,y,w) =
            if x < 0 andalso y < 0 then
               (case (const x,const y) of
                  (SP.IMMED i, SP.IMMED j) => 
                     ((case opcode of 
                        (E.ADD | E.ADDT) => immed(i + j)
                      | (E.SUB | E.SUBT) => immed(i - j)
                      | (E.MUL | E.MULT) => immed(i * j)
                      | (E.DIV | E.DIVT) => immed(i div j)
                      | (E.MOD | E.MODT) => immed(i mod j)
                      | E.ANDB => immed(andb(i,j))
                      | E.ORB  => immed(orb(i,j))
                      | E.XORB => immed(xorb(i,j))
                      | E.SRA  => immed(sra(i,j))
                      | E.SRL  => immed(srl(i,j))
                      | E.SLL  => immed(sll(i,j))
                      | E.CMP E.SETCC => simplify(e,opcode,xs,x,y,w)
                      | E.CMP cc => immed(cmp(cc,i,j))
                      | _ => simplify(e,opcode,xs,x,y,w)
                      ) handle (Div | Overflow) => simplify(e,opcode,xs,x,y,w))
                  | _ => simplify(e,opcode,xs,x,y,w)
               )
            else simplify(e,opcode,xs,x,y,w)

       (* algebraic simplification *)
       and simplify(e,binop,xs,x,y,w) = 
           (case (binop,x,y) of
              ((E.ADD|E.ADDT|E.ORB),~1,y) => y
            | ((E.ADD|E.ADDT|E.ORB|E.SUB|E.SUBT),x,~1) => x
            | ((E.SUB|E.SUBT),x,y) => if x = y then ~1 else lookup(e,xs,w)
            | ((E.MUL|E.MULT|E.ANDB|E.DIV|E.DIVT|E.MOD|E.MODT),~1,_) => ~1
            | ((E.MUL|E.MULT|E.ANDB),x,~1) => ~1
            | ((E.MUL|E.MULT),~2,y) => y
            | ((E.MUL|E.MULT|E.DIV|E.DIVT),x,~2) => y
            | (E.CMP E.EQ,x,y) => if x = y then ~2 else
                                     normalize(e,binop,xs,x,y,w)
            | (E.CMP E.NE,x,y) => if x = y then ~1 else
                                     normalize(e,binop,xs,x,y,w)
            | (E.CMP E.GTU,x,~1) => ~2 (* true *)
            | (E.CMP E.LTU,~1,y) => ~1 (* false *)
            | _ => normalize(e,binop,xs,x,y,w)
           )

           (*
            * normalize commutative operators
            *)
       and normalize(e,binop,xs,x,y,w) =
           let val (e',xs') =
                  case binop of
                    (E.ADD | E.ADDT | E.MUL | E.MULT | E.ANDB | E.ORB | E.XORB
                    | E.CMP E.EQ | E.CMP E.NE) => 
                       (e,if x < y then xs else [y,x])
                  |  E.CMP E.GT  => (LT,[y,x])
                  |  E.CMP E.GE  => (LE,[y,x])
                  |  E.CMP E.GTU => (LTU,[y,x])
                  |  E.CMP E.GEU => (LEU,[y,x])
                  |  _ => (e,xs)
           in  lookup(e',xs',w) 
           end 
   in  process
   end

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:45  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:51  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:19  pscheng
# *** empty log message ***
#
 *)
