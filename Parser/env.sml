(*$import ENV TopLevel StrgHash Array Vector Control ErrorMsg *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* env.sml *)



structure Env :> ENV =
struct

(* debugging *)
val say = Control.Print.say
val debugging = ref false
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()


structure Symbol = 
struct
  val varInt = 0 and sigInt = 1 and strInt = 2 and fsigInt = 3 and 
      fctInt = 4 and tycInt = 5 and labInt = 6 and tyvInt = 7 and
      fixInt = 8

  datatype symbol = SYMBOL of int * string
  datatype namespace =
     VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
     LABspace | TYVspace | FSIGspace 

  fun eq(SYMBOL(a1,b1),SYMBOL(a2,b2)) = a1=a2 andalso b1=b2
  fun symbolGt(SYMBOL(_,s1), SYMBOL(_,s2)) = s1 > s2
  fun symbolCMLt (SYMBOL (a1, s1), SYMBOL (a2, s2)) =
        a1 < a2 orelse a1 = a2 andalso s1 < s2

  fun varSymbol (name: string) =
        SYMBOL(StrgHash.hashString name + varInt,name)
  fun tycSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + tycInt, name)
  fun fixSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + fixInt, name)
  fun labSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + labInt, name)
  fun tyvSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + tyvInt, name)
  fun sigSymbol (name: string) =
        SYMBOL(StrgHash.hashString name + sigInt, name)
  fun strSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + strInt, name)
  fun fctSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + fctInt, name)
  fun fsigSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + fsigInt, name)

  fun var'n'fix name =
        let val h = StrgHash.hashString name
	 in (SYMBOL(h+varInt,name),SYMBOL(h+fixInt,name))
	end

  fun name (SYMBOL(_,name)) = name
  fun number (SYMBOL(number,_)) = number
  fun nameSpace (SYMBOL(number,name)) : namespace =
        case number - StrgHash.hashString name
	 of 0 => VALspace
          | 5 => TYCspace
          | 1 => SIGspace
          | 2 => STRspace
          | 4 => FCTspace
          | 8 => FIXspace
          | 6 => LABspace
          | 7 => TYVspace
	  | 3 => FSIGspace
	  | _ => ErrorMsg.impossible "Symbol.nameSpace"

  fun nameSpaceToString (n : namespace) : string =
        case n
         of VALspace => "variable or constructor"
          | TYCspace => "type constructor"
          | SIGspace => "signature"
          | STRspace => "structure"
          | FCTspace => "functor"
          | FIXspace => "fixity"
          | LABspace => "label"
	  | TYVspace => "type variable"
	  | FSIGspace => "functor signature"

  fun symbolToString(SYMBOL(number,name)) : string =
        case number - StrgHash.hashString name
         of 0 => "VAL$"^name
          | 1 => "SIG$"^name
          | 2 => "STR$"^name
          | 3 => "FSIG$"^name
          | 4 => "FCT$"^name
          | 5 => "TYC$"^name
          | 6 => "LAB$"^name
          | 7 => "TYV$"^name
          | 8 => "FIX$"^name
          | _ => ErrorMsg.impossible "Symbol.toString"

end (* structure Symbol *)

structure FastSymbol = 
struct
  local open Symbol
  in

  type symbol = symbol

  (* Another version of symbols but hash numbers have no increments
   * according to their nameSpace *)
  datatype raw_symbol = RAWSYM of int * string

  (* builds a raw symbol from a pair name, hash number *)
  fun rawSymbol hash_name = RAWSYM hash_name

  (* builds a symbol from a raw symbol belonging to the same space as
   * a reference symbol *)
  fun sameSpaceSymbol (SYMBOL(i,s)) (RAWSYM(i',s')) =
        SYMBOL(i' + (i - StrgHash.hashString s), s')

  (* build symbols in various name space from raw symbols *)
  fun varSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + varInt,name)
  fun tycSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + tycInt, name)
  fun fixSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + fixInt, name)
  fun labSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + labInt, name)
  fun tyvSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + tyvInt, name)
  fun sigSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + sigInt, name)
  fun strSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + strInt, name)
  fun fctSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + fctInt, name)
  fun fsigSymbol (RAWSYM (hash,name)) =
        SYMBOL(hash + fsigInt, name)
  fun var'n'fix (RAWSYM (h,name)) =
        (SYMBOL(h+varInt,name),SYMBOL(h+fixInt,name))

  end (* local FastSymbol *)
end (* structure FastSymbol *)

(* exception Unbound = System.Unsafe.Assembly.UnboundTable *)
exception Unbound


structure IntStrMapV :> INTSTRMAPV =
struct 

  structure V = Vector
  datatype 'a bucket = NIL | B of (int * string * 'a * 'a bucket)
  type 'a intstrmap = 'a bucket V.vector

  val elems = V.length
  fun bucketmap f =
    let fun loop NIL = NIL
          | loop(B(i,s,j,r)) = B(i,s,f(j),loop r)
     in loop
    end

  fun bucketapp f =
    let fun loop NIL = ()
          | loop(B(i,s,j,r)) = (f(i,s,j); loop r)
     in loop
    end

  fun transform f v = V.tabulate(V.length v, fn i => bucketmap f (V.sub(v,i)))

  fun map v (i,s) =
    let fun find NIL = raise Unbound
          | find (B(i',s',j,r)) = if i=i' andalso s=s' then j else find r
     in (find (V.sub(v,Int.rem(i,V.length v)))) handle Div => raise Unbound
    end

  fun app f v =
    let val n = V.length v
        val bapp = bucketapp f
        fun f i = if i=n then () else (bapp(V.sub(v,i)); f(i+1))
     in f 0
    end

  fun fold f zero v =
    let val n = V.length v
        fun bucketfold (NIL,x) = x
          | bucketfold (B(i,s,j,r), x) = bucketfold(r, f((i,s,j),x))

        fun g(i,x) = if i=n then x else g(i+1,bucketfold(V.sub(v,i),x))
     in g(0,zero)
    end

  fun new (bindings: (int*string*'_b) list) =
    let val n = List.length bindings
        val a0 = Array.array(n,NIL: '_b bucket)
        val dups = ref 0

        fun add a (i,s,b) =
          let val index = i mod (Array.length a)
              fun f NIL = B(i,s,b,NIL)
                | f (B(i',s',b',r)) =
                     if i'=i andalso s'=s
                     then (dups := !dups+1; B(i,s,b,r))
                     else B(i',s',b',f r)
           in Array.update(a,index,f(Array.sub(a,index)))
          end

        val _ = List.app (add a0) bindings
        val a1 = case !dups
                  of 0 => a0
                   | d => let val a = Array.array(n-d, NIL: '_b bucket)
                           in List.app (add a) bindings; a
                          end

     in Vector.tabulate(Array.length a1, fn i => Array.sub(a1,i))
    end
    handle Div => (ErrorMsg.impossible "IntStrMapV.new raises Div";
                 raise Div)

end (* structure IntStrMapV *)

(* representation of environments *)
(* 'b will always be instantiated to Basics.binding *)

datatype 'b env
  = EMPTY
  | BIND of int * string * 'b * 'b env
  | TABLE of 'b IntStrMapV.intstrmap * 'b env
  | SPECIAL of (Symbol.symbol -> 'b) * (unit -> Symbol.symbol list) * 'b env
         (* for, e.g., debugger *)

exception SpecialEnv 
  (* raised by app when it encounters a SPECIAL env *)

val empty = EMPTY

fun look (env,sym as Symbol.SYMBOL(is as (i,s))) = 
  let fun f EMPTY = (debugmsg("$Env.look "^s); raise Unbound)
        | f (BIND(i',s',b,n)) =
            if i = i' andalso s = s' then b else f n
        | f (TABLE(t,n)) = (IntStrMapV.map t is handle Unbound => f n)
        | f (SPECIAL(g,_,n)) = (g sym handle Unbound => f n)
   in f env
  end

(*
val look = fn x =>
  Stats.doPhase (Stats.makePhase "Compiler 032 2-lookEnv ") look x
*)

fun bind (Symbol.SYMBOL(i,s),binding,env) = BIND (i,s,binding,env)

exception NoSymbolList

fun special (look', getSyms) =
  let val memo_env = ref empty
      fun lookMem sym =
            look(!memo_env, sym) 
            handle Unbound => 
                  let val binding = look' sym
                   in memo_env := bind(sym,binding,!memo_env);
                      binding
                  end

      val memo_syms = ref(NONE: Symbol.symbol list option)
      fun getsymsMem() =
        case !memo_syms
         of NONE => let val syms = getSyms()
                     in memo_syms := SOME syms; syms
                    end
          | SOME syms => syms
   in SPECIAL(lookMem,getsymsMem,empty)
  end

infix atop

fun EMPTY atop e = e
  | (BIND(i,s,b,n)) atop e = BIND(i,s,b,n atop e)
  | (TABLE(t,n)) atop e = TABLE(t,n atop e)
  | (SPECIAL(g,syms,n)) atop e = SPECIAL(g, syms, n atop e)
 
fun app f =
  let fun g (BIND(i,s,b,n)) = (g n; f (Symbol.SYMBOL(i,s),b))
        | g (TABLE(t,n)) =
              (g n; IntStrMapV.app (fn (i,s,b) => f(Symbol.SYMBOL(i,s),b)) t)
        | g (SPECIAL(looker,syms,n)) = 
              (g n; List.app (fn sym=>f(sym,looker sym)) (syms()))
        | g (EMPTY) = ()
   in g
  end

fun symbols env =
  let fun f(syms,BIND(i,s,b,n)) = f(Symbol.SYMBOL(i,s)::syms,n)
        | f(syms,TABLE(t,n)) =
             let val r = ref syms
                 fun add(i,s,_) = r := Symbol.SYMBOL(i,s):: !r
              in IntStrMapV.app add t; f(!r,n)
             end
        | f(syms,SPECIAL(_,syms',n)) = f(syms'()@syms, n)
        | f(syms,EMPTY) = syms
   in f(nil,env)
  end

fun map func (TABLE(t,EMPTY)) =  (* optimized case *)
      TABLE(IntStrMapV.transform func t, EMPTY)
  | map func env =
      let fun f(syms,BIND(i,s,b,n)) = f((i,s,func b)::syms,n)
            | f(syms,TABLE(t,n)) =
                 let val r = ref syms
                     fun add(i,s,b) = r := (i,s,func b) :: !r
                  in IntStrMapV.app add t;
                     f(!r,n)
                 end
            | f(syms,SPECIAL(look',syms',n)) = 
                 f(List.map (fn (sym as Symbol.SYMBOL(i,s)) =>
                                    (i,s,func(look' sym))) (syms'())@syms, 
                   n)
            | f(syms,EMPTY) = syms
 
       in TABLE(IntStrMapV.new(f(nil,env)), EMPTY)
      end

fun fold f base e =
  let fun g (BIND(i,s,b,n),x) = 
              let val y = g(n,x)
               in f((Symbol.SYMBOL(i,s),b),y)
              end
        | g (e as TABLE(t,n),x) =
              let val y = g(n,x)
               in IntStrMapV.fold
                     (fn ((i,s,b),z) => f((Symbol.SYMBOL(i,s),b),z)) y t
              end
        | g (SPECIAL(looker,syms,n),x) = 
              let val y = g(n,x)
                  val symbols = (syms())
               in List.foldr (fn (sym,z) =>f((sym,looker sym),z)) y symbols
              end
        | g (EMPTY,x) = x
    in g(e,base)
   end

fun consolidate (env as TABLE(_,EMPTY)) = env
  | consolidate (env as EMPTY) = env
  | consolidate env = map (fn x => x) env handle NoSymbolList => env

end (* structure Env *)


structure Symbol = Env.Symbol
structure FastSymbol = Env.FastSymbol


(*
 * $Log$
# Revision 1.5  98/02/27  17:32:14  pscheng
# The fix in compilation of implementation files with interfaces
# allows Symbol and FastSymbol to be exported without Env.
# Thus, HIL no longer depends on Env (which had depended on many
# Basis modules) .
# 
# Revision 1.4  1998/02/15  22:43:24  pscheng
# bootstrapping changes
#
# Revision 1.3  1998/01/21  20:40:13  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.2  97/07/24  11:24:27  pscheng
# changed Unbound exception for 109.30
# 
# Revision 1.1  97/03/26  14:12:25  pscheng
# added copy of SMLNJ parser files
# 
 * Revision 1.1.1.1  1997/01/14  01:38:36  george
 *   Version 109.24
 *
 *)
