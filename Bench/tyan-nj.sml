(*$import List Array Int TextIO *)

functor makeRunTyan() = 
struct
local


fun fold f l acc = List.foldr f acc l
fun revfold f l acc = List.foldl f acc l
val update = Array.update
val sub = Array.sub
val max = Int.max
val max32 = Int32.max
val array = Array.array 

val int32touint32 = Word32.fromLargeInt
val uint32toint32 = Word32.toLargeInt
val && = Word32.andb
val || = Word32.orb 
val << = fn (x,y:Int32.int) => (Word32.<< (x,Word.fromLargeInt y))
val >> = fn (x,y:Int32.int) => (Word32.>> (x,Word.fromLargeInt y))

infix 7 && || << >>

val makestring_int = Int.toString
val makestring_int32 = Int32.toString

val andb = fn(x,y) => uint32toint32((int32touint32 x) && (int32touint32 y))
val orb = fn(x,y) => uint32toint32((int32touint32 x) || (int32touint32 y))
val rshift = fn(x,y : Int32.int) => uint32toint32((int32touint32 x) >> y)
val lshift = fn(x,y : Int32.int) => uint32toint32((int32touint32 x) << y)
val (op >>) = rshift
val (op <<) = lshift

nonfix smlnj_mod
nonfix smlnj_div
val smlnj_mod = op mod
val smlnj_div = op div
infix 7 smlnj_mod
infix 7 smlnj_div

nonfix smlnj_mod32
nonfix smlnj_div32
val smlnj_mod32 = Int32.mod
val smlnj_div32 = Int32.div
infix 7 smlnj_mod32
infix 7 smlnj_div32

val char_implode = implode
val char_explode = explode
fun string_implode [] = ""
  | string_implode (s1::srest) = s1 ^ (string_implode srest)
fun string_explode s = map (fn c => implode[c]) (explode s)

exception Tabulate
fun tabulate (i,f) = 
if i <= 0 then raise Tabulate else
let val a = array(i,f 0)
    fun tabify j = if j < i then (update(a,j,f j); tabify (j+1)) else a
in
    tabify 1
end

exception ArrayofList
fun arrayoflist (hd::tl) =
let val a = array((length tl) + 1,hd)
    fun al([],_) = a
      | al(hd::tl,i) = (update(a,i,hd); al(tl,i+1))
in
    al(tl,1)
end
  | arrayoflist ([]) = raise ArrayofList


structure Util = struct
    datatype relation = Less | Equal | Greater

    exception NotImplemented of string
    exception Impossible of string (* flag "impossible" condition  *)
    exception Illegal of string (* flag function use violating precondition *)

    fun error exn msg = raise (exn msg)
    fun notImplemented msg = error NotImplemented msg
    fun impossible msg = error Impossible msg
    fun illegal msg = error Illegal msg

    (* arr[i] := obj :: arr[i]; extend non-empty arr if necessary *)
    fun insert (obj,i,arr) = let
          val i = Int32.toInt i
	  val len = Array.length arr
          val res =  if i<len then (update(arr,i,obj::sub(arr,i)); arr)
	     else let val arr' = array(max(i+1,len+len),[])
		      fun copy ~1 = (update(arr',i,[obj]); arr')
			| copy j = (update(arr',j,sub(arr,j));
				    copy(j-1))
		      in copy(len-1) end 
          in res
          end

(*
    fun arrayoflists [] = arrayoflist []
      | arrayoflists ([]::ls) = arrayoflists ls
      | arrayoflists [l] = arrayoflist l
      | arrayoflists (ls as (obj0::_)::_) = let	 
	  val a = array(revfold (fn (l,n) => length l + n) ls 0,obj0)
	  fun ins (i,[]) = i | ins (i,x::l) = (update(a,i,x); ins(i+1,l))
	  fun insert (i,[]) = a | insert (i,l::ll) = insert(ins(i,l),ll)
          in insert(0,ls) end
*)

    (* given compare and array a, return list of contents of a sorted in
     * ascending order, with duplicates stripped out; which copy of a duplicate
     * remains is random.  NOTE that a is modified.
     *)
    fun stripSort compare = fn a => let
	  infix sub

(*	  val op sub = sub and update = update *)
	  fun swap (i,j) = let val ai = a sub i
			   in update(a,i,a sub j); update(a,j,ai) end
	  (* sort all a[k], 0<=i<=k<j<=length a *)
	  fun s (i,j,acc) = if i=j then acc else let
	        val pivot = a sub ((i+j) smlnj_div 2)
		fun partition (lo,k,hi) = if k=hi then (lo,hi) else
		      case compare (pivot,a sub k) of
			  Less => (swap (lo,k); partition (lo+1,k+1,hi))
			| Equal => partition (lo,k+1,hi)
			| Greater => (swap (k,hi-1); partition (lo,k,hi-1))
		val (lo,hi) = partition (i,i,j)
	        in s(i,lo,pivot::s(hi,j,acc)) end
	   val res = s(0,Array.length a,[]) 

          in 
	   res
	  end
end

structure F = struct
    val p = Int32.fromInt 17

    datatype field = F of Int32.int (* for (F n), always 0<=n<p *)
    (* exception Div = Integer.Div *)
    fun show (F x) = print (makestring_int32 x)

    val char = p

    val zero = F 0
    val one = F 1
    fun coerceInt n = F (n smlnj_mod32 p)

    fun add (F n,F m) = let val k = n+m in if k>=p then F(k-p) else F k end
    fun subtract (F n,F m) = if n>=m then F(n-m) else F(n-m+p)
    fun negate (F 0) = F 0 | negate (F n) = F(p-n)
    fun multiply (F n,F m) = F ((n*m) smlnj_mod32 p)
    fun reciprocal (F 0) = raise Div
      | reciprocal (F n) = let
          (* consider euclid gcd alg on (a,b) starting with a=p, b=n.
           * if maintain a = a1 n + a2 p, b = b1 n + b2 p, a>b,
	   * then when 1 = a = a1 n + a2 p, have a1 = inverse of n mod p
           * note that it is not necessary to keep a2, b2 around.
           *)
 	  fun gcd ((a,a1),(b,b1)) =
	      if b=1 then (* by continued fraction expansion, 0<|b1|<p *)
		 if b1<0 then F(p+b1) else F b1
	      else let val q = a smlnj_div32 b
	           in gcd((b,b1),(a-q*b,a1-q*b1)) end
          in gcd ((p,0),(n,1)) end
    fun divide (n,m) = multiply (n, reciprocal m)


    fun power(n,k) =
	  if k<=3 then case k of
	      0 => one
	    | 1 => n
	    | 2 => multiply(n,n)
	    | 3 => multiply(n,multiply(n,n))
	    | _ => reciprocal (power (n,~k)) (* know k<0 *)
	  else if andb(k,1)=0 then power(multiply(n,n),rshift(k,1))
	       else multiply(n,power(multiply(n,n),rshift(k,1)))

    fun isZero (F n) = n=0
    fun equal (F n,F m) = n=m

    fun display (F n) = if n<=p smlnj_div32 2 then makestring_int32 n
			else "-" ^ makestring_int32 (p-n)
end


structure M = struct (* MONO *)
    local
	infix sub << >> andb
    in

(* encode (var,pwr) as a long word: hi word is var, lo word is pwr
   masks 0xffff for pwr, mask ~0x10000 for var, rshift 16 for var
   note that encoded pairs u, v have same var if u>=v, u andb ~0x10000<v
*)

    datatype mono = M of Int32.int list
(*
    fun show (M x) = (print "<"; app (fn i => (print (makestring_int i); print ",")) x; print">")
*)
    exception DoesntDivide

    val numVars = 32

    val one = M []
    fun x_i v = M [(v<<16)+1]
    fun explode (M l) = map (fn v => (v>>16,v andb 65535)) l
    fun implode l = M (map (fn (v,p) => (v<<16)+p) l)
    val ord = Int32.fromInt o ord
    val chr = chr o Int32.toInt

    val deg = let fun d([],n) = n | d(u::ul,n) = d(ul,(u andb 65535) + n)
              in fn (M l) => d(l,0) end

    (* x^k > y^l if x>k or x=y and k>l *)
    val compare = let
	  fun cmp ([],[]) = Util.Equal
	    | cmp (_::_,[]) = Util.Greater
	    | cmp ([],_::_) = Util.Less
	    | cmp ((u::us), (v::vs)) = if u=v then cmp (us,vs)
		                  else if u<v then Util.Less
		                  else (* u>v *)   Util.Greater
          in fn (M m,M m') => cmp(m,m') end

    fun display (M l) = 
	let
	    val aa : Int32.int = (ord #"a")
	    val AA : Int32.int = (ord #"A")
	    fun dv (v : Int32.int) : string = 
		let val c : char = if v<26 then chr (v+aa) else chr (v-26+AA)
		in  char_implode[c]
		end
	    fun d (vv,acc) = 
		let val v = vv>>16 
		    val p = vv andb 65535
		in if p=1 then (dv v) ^ acc
		   else (dv v) ^ (makestring_int32 p) ^ acc
		end
	in foldl d "" l
	end

    val multiply = let
	  fun mul ([],m) = m
	    | mul (m,[]) = m
	    | mul (u::us, v::vs) = let
	        val uu = u andb ~65536
		in if uu = (v andb ~65536) then let
		      val w = u + (v andb 65535)
		      in if uu = (w andb ~65536) then w::mul(us,vs)
			 else Util.illegal 
			     (("Mono.multiply overflow: ") ^ 
			      (display (M(u::us))) ^ ", " ^ (display (M(v::vs))))
		      end
	           else if u>v then u :: mul(us,v::vs)
		   else (* u<v *) v :: mul(u::us,vs)
		end
          in fn (M m,M m') => M (mul (m,m')) end

    val lcm = let
	  fun lcm ([],m) = m
	    | lcm (m,[]) = m
	    | lcm (u::us, v::vs) =
	        if u>=v then if (u andb ~65536)<v then u::lcm(us,vs)
			     			    else u::lcm(us,v::vs)
			else if (v andb ~65536)<u then v::lcm(us,vs)
			     			    else v::lcm(u::us,vs)
          in fn (M m,M m') => M (lcm (m,m')) end
    val tryDivide = let
	  fun rev([],l) = l | rev(x::xs,l)=rev(xs,x::l)
	  fun d (m,[],q) = SOME(M(rev(q,m)))
	    | d ([],_::_,_) = NONE
	    | d (u::us,v::vs,q) =
	        if u<v then NONE
		else if (u andb ~65536) = (v andb ~65536) then
		    if u=v then d(us,vs,q) else d(us,vs,u-(v andb 65535)::q)
	        else d(us,v::vs,u::q)
          in fn (M m,M m') => d (m,m',[]) end
    fun divide (m,m') =
	  case tryDivide(m,m') of SOME q => q | NONE => raise DoesntDivide

end end (* local, structure M *)



structure MI = struct (* MONO_IDEAL *)

    (* trie:
     * index first by increasing order of vars
     * children listed in increasing degree order
     *)
    datatype 'a mono_trie = MT of 'a option * (Int32.int * 'a mono_trie) list
	                    (* tag, encoded (var,pwr) and children *)
    datatype 'a mono_ideal = MI of (Int32.int * 'a mono_trie) ref
	                    (* int maxDegree = least degree > all elements *)
    
    fun rev ([],l) = l | rev (x::xs,l) = rev(xs,x::l)
    fun tl (_::l) = l | tl [] = raise (Util.Impossible "MONO_IDEAL.tl")
    fun hd (x::_) = x | hd [] = raise (Util.Impossible "MONO_IDEAL.hd")

    val emptyTrie = MT(NONE,[])
    fun mkEmpty () = MI(ref (0,emptyTrie))

    fun maxDeg (MI(x)) = #1(!x)


    fun encode (var,pwr) = lshift(var,16)+pwr
    fun decode vp = (rshift(vp,16),andb(vp,65535))
    fun grabVar vp = andb(vp,~65536)
    fun grabPwr vp = andb(vp,65535)
    fun smallerVar (vp,vp') = vp < andb(vp',~65536)

    exception Found
    fun search (MI(x),M.M m') = let 
	  val (d,mt) = !x
	  val result = ref NONE
	  (* exception Found of M.mono * '_a *)
	  (* s works on remaining input mono, current output mono, tag, trie *)
	  fun s (_,m,MT(SOME a,_)) =
	        raise(result := SOME (M.M m,a); Found)
	    | s (m',m,MT(NONE,trie)) = s'(m',m,trie)
	  and s'([],_,_) = NONE
	    | s'(_,_,[]) = NONE
	    | s'(vp'::m',m,trie as (vp,child)::children) =
		if smallerVar(vp',vp) then s'(m',m,trie)
		else if grabPwr vp = 0 then (s(vp'::m',m,child);
					     s'(vp'::m',m,children))
		else if smallerVar(vp,vp') then NONE 
		else if vp<=vp' then (s(m',vp::m,child);
				      s'(vp'::m',m,children))
		else NONE
          in s(rev(m',[]),[],mt)
             handle Found (* (m,a) => SOME(m,a) *) => !result 
          end



   (* assume m is a new generator, i.e. not a multiple of an existing one *)
    fun insert (MI (mi),m,a) = let
	  val (d,mt) = !mi
	  fun i ([],MT (SOME _,_)) = Util.illegal "MONO_IDEAL.insert duplicate"
	    | i ([],MT (NONE,children)) = MT(SOME a,children)
	    | i (vp::m,MT(a',[])) = MT(a',[(vp,i(m,emptyTrie))])
	    | i (vp::m,mt as MT(a',trie as (vp',_)::_)) = let
		fun j [] = [(vp,i(m,emptyTrie))]
		  | j ((vp',child)::children) =
		      if vp<vp' then (vp,i(m,emptyTrie))::(vp',child)::children
		      else if vp=vp' then (vp',i(m,child))::children
		      else (vp',child) :: j children
		in 
		   if smallerVar(vp,vp') then
		       MT(a',[(grabVar vp,MT(NONE,trie)),(vp,i(m,emptyTrie))])
		   else if smallerVar(vp',vp) then i(grabVar vp'::vp::m,mt)
		   else MT(a',j trie)
	        end
	  in mi := (max32(d,M.deg m),i (rev(map encode(M.explode m),[]),mt)) end

    fun mkIdeal [] = mkEmpty() 
      | mkIdeal (orig_ms : (M.mono * '_a) list)= let
	  fun ins ((m,a),arr) = Util.insert((m,a),M.deg m,arr)
	  val msa = arrayoflist orig_ms
	  val ms : (M.mono * '_a) list = 
	      Util.stripSort (fn ((m,_),(m',_)) => M.compare (m,m')) msa
	  val buckets = revfold ins ms (array(0,[]))
	  val n = Array.length buckets
	  val mi = mkEmpty()
          fun sort i = if i>=n then mi else let
	        fun redundant (m,_) = case search(mi,m) of NONE => false
						         | SOME _ => true
		fun filter ([],l) = app (fn (m,a) => insert(mi,m,a)) l
		  | filter (x::xx,l) = if redundant x then filter(xx,l)
				       else filter(xx,x::l)
		in filter(sub(buckets,i),[]);
		   update(buckets,i,[]);
		   sort(i+1)
		end
          in sort 0 end

    fun fold g (MI(x)) init = let
	  val (_,mt) = !x
	  fun f(acc,m,MT(NONE,children)) = f'(acc,m,children)
	    | f(acc,m,MT(SOME a,children)) =
	        f'(g((M.M m,a),acc),m,children)
	  and f'(acc,m,[]) = acc
	    | f'(acc,m,(vp,child)::children) =
	        if grabPwr vp=0 then f'(f(acc,m,child),m,children)
		else f'(f(acc,vp::m,child),m,children)
	  in f(init,[],mt) end

    fun searchDeg (mi,d) =
	  if d>maxDeg mi then []
	  else fold (fn ((m,a),l) => if M.deg m=d then (m,a)::l else l) mi []

end (* structure MI *)



val maxLeft = ref 0
val maxRight = ref 0
val counts = tabulate(20,fn _ => array(20,0))
val indices = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
fun resetCounts() = app(fn i => app (fn j => update(sub(counts,i),j,0)) indices) indices
fun pair(l,r) = let
      val log = let fun log(n,l) = if n<=1 then l else log((n >> 1),1+l)
          in fn n => log(n,0) end
      val l = log l and r = log r
      val _ = maxLeft := max(!maxLeft,l) and _ = maxRight := max(!maxRight,r)
      val a = sub(counts,l)
      in update(a,r,sub(a,r)+1) end
fun getCounts () = 
  map (fn i => map (fn j => sub(sub(counts,i),j)) indices) indices


structure P = struct

    datatype poly = P of (F.field*M.mono) list (* descending mono order *)
(*	
    fun show (P x) = (print "[ "; 
		      app (fn (f, m) =>
			   (print "("; F.show f; print ","; M.show m; print ") ")) x;
		      print " ]")
*)
    val zero = P []
    val one = P [(F.one,M.one)]
    fun coerceInt n = P [(F.coerceInt n,M.one)]
    fun coerceField a = P [(a,M.one)]
    fun coerceMono m = P [(F.one,m)]
    fun coerce (a,m) = P [(a,m)]
    fun implode p = P p
    fun cons (am,P p) = P (am::p)

local
    fun neg p = (map (fn (a,m) => (F.negate a,m)) p)
    fun plus ([],p2) = p2
      | plus (p1,[]) = p1
      | plus ((a,m)::ms,(b,n)::ns) = case M.compare(m,n) of
	    Util.Less => (b,n) :: plus ((a,m)::ms,ns)
	  | Util.Greater => (a,m) :: plus (ms,(b,n)::ns)
	  | Util.Equal => let val c = F.add(a,b)
       		             in if F.isZero c then plus(ms,ns)
		                else (c,m)::plus(ms,ns)
		             end
    fun minus ([],p2) = neg p2
      | minus (p1,[]) = p1
      | minus ((a,m)::ms,(b,n)::ns) = case M.compare(m,n) of
	    Util.Less => (F.negate b,n) :: minus ((a,m)::ms,ns)
	  | Util.Greater => (a,m) :: minus (ms,(b,n)::ns)
	  | Util.Equal => let val c = F.subtract(a,b)
       		             in if F.isZero c then minus(ms,ns)
			        else (c,m)::minus(ms,ns)
		             end
    fun termMult (a,m,p) =
	  (map (fn (a',m') => (F.multiply(a,a'),M.multiply(m,m'))) p)
in
    val length = fn l => (Int32.fromInt o length) l
    fun negate (P p) = P (neg p)
    fun add (P p1,P p2) = (pair(length p1,length p2); P (plus(p1,p2)))
    fun subtract (P p1,P p2) = (pair(length p1,length p2); P (minus(p1,p2)))
    val multiply = let
	  fun times (p1,p2) = 
	        revfold (fn ((a,m),tot) => plus (termMult(a,m,p2),tot)) p1 []
          in fn (P p1,P p2) => if length p1 > length p2 then P(times (p2,p1))
			       else P(times (p1,p2))
          end
    fun singleReduce (P y,a,m,P x) = (pair(length y,length x); P(minus(y,termMult(a,m,x))))
    fun spair (a,m,P f,b,n,P g) = (pair(length f,length g); P(minus(termMult(a,m,f),termMult(b,n,g))))
    val termMult = fn (a,m,P f) => P(termMult(a,m,f))
end

    fun scalarMult (a,P p) = P (map (fn (b,m) => (F.multiply(a,b),m)) p)
    fun power(p,k) =
	  if k<=3 then case k of
	      0 => one
	    | 1 => p
	    | 2 => multiply(p,p)
	    | 3 => multiply(p,multiply(p,p))
	    | _ => Util.illegal "POLY.power with k<0"
	  else if andb(k,1)=0 then power(multiply(p,p),rshift(k,1))
	       else multiply(p,power(multiply(p,p),rshift(k,1)))

    fun isZero (P []) = true | isZero (P (_::_)) = false
    val equal = let
	  fun eq ([],[]) = true
	    | eq (_::_,[]) = false
	    | eq ([],_::_) = false
	    | eq ((a,m)::p,(b,n)::q) =
	        F.equal(a,b) andalso M.compare(m,n)=Util.Equal
		andalso eq (p,q)
          in fn (P p,P q) => eq (p,q) end

    (* these should only be called if there is a leading term, i.e. poly<>0 *)
    fun leadTerm (P(am::_)) = am
      | leadTerm (P []) = Util.illegal "POLY.leadTerm"
    fun leadMono (P((_,m)::_)) = m
      | leadMono (P []) = Util.illegal "POLY.leadMono"
    fun leadCoeff (P((a,_)::_)) = a
      | leadCoeff (P []) = Util.illegal "POLY.leadCoeff"
    fun rest (P (_::p)) = P p
      | rest (P []) = Util.illegal "POLY.rest"
    fun leadAndRest (P (lead::rest)) = (lead,P rest)
      | leadAndRest (P []) = Util.illegal "POLY.leadAndRest"

    fun deg (P []) = Util.illegal "POLY.deg on zero poly"
      | deg (P ((_,m)::_)) = M.deg m (* homogeneous poly *)
    fun numTerms (P p) = length p

    fun display (P []) = F.display F.zero
      | display (P p) = let
	  fun dsp (a,m) = let
	        val s = 
		      if M.deg m = 0 then F.display a
 		      else if F.equal(F.one,F.negate a) then "-" ^ M.display m
		      else if F.equal(F.one,a) then M.display m
		      else F.display a ^ M.display m
	        in if substring(s,0,1)="-" then s else "+" ^ s end
	  in string_implode(map dsp p) end
end

structure HP = struct
	datatype hpoly = HP of P.poly array
	val log = let
	      fun log(n,l) = if n<8 then l else log((n >> 2),1+l)
	      in fn n => log(n,0) end
	fun mkHPoly p = let
	      val l = log(P.numTerms p)
	      in HP(tabulate(l+1,fn i => if i=l then p else P.zero)) end
	fun add(p,HP ps) = let
	      val l = log(P.numTerms p)
	      in if l>=Array.length ps then let
		   val n = Array.length ps
		   in HP(tabulate(n+n,
			 fn i => if i<n then sub(ps,i)
			         else if i=l then p else P.zero))
		   end
		 else let
		   val p = P.add(p,sub(ps,l))
		   in if l=log(P.numTerms p) then (update(ps,l,p); HP ps)
		      else (update(ps,l,P.zero); add (p,HP ps))
		   end
	      end
	fun leadAndRest (HP ps) = let
	      val n = Array.length ps
	      fun lar (m,indices,i) = if i>=n then lar'(m,indices) else let
		    val p = sub(ps,i)
		    in if P.isZero p then lar(m,indices,i+1)
		       else if null indices then lar(P.leadMono p,[i],i+1)
			    else case M.compare(m,P.leadMono p) of
				Util.Less => lar(P.leadMono p,[i],i+1)
			      | Util.Equal => lar(m,i::indices,i+1)
			      | Util.Greater => lar(m,indices,i+1)
		    end
	      and lar' (_,[]) = NONE
		| lar' (m,i::is) = let
		    fun extract i = case P.leadAndRest(sub(ps,i)) of
			  ((a,_),rest) => (update(ps,i,rest); a)
		    val a = revfold (fn (j,b) => F.add(extract j,b))
				    is (extract i)
		    in if F.isZero a then lar(M.one,[],0) else SOME(a,m,HP ps)
		    end
	      in lar(M.one,[],0) end
end


structure G = struct
    val autoReduce = ref true
    val maxDeg = ref 10000
    val maybePairs = ref 0
    val primePairs = ref 0
    val usedPairs = ref 0
    val newGens = ref 0

    fun reset () = (maybePairs:=0; primePairs:=0; usedPairs:=0; newGens:=0)

    fun inc r = r := !r + 1

    fun reduce (f,mi) = if P.isZero f then f else let
          (* use accumulator and reverse at end? *)
	  fun r hp = case HP.leadAndRest hp of
	        NONE => []
	      | (SOME(a,m,hp)) => case MI.search(mi,m) of
		    NONE => (a,m)::(r hp)
		  | SOME (m',p) => r (HP.add(P.termMult(F.negate a,M.divide(m,m'),!p),hp))
	  in P.implode(r (HP.mkHPoly f)) end

    (* assume f<>0 *)
    fun mkMonic f = P.scalarMult(F.reciprocal(P.leadCoeff f),f)

    (* given monic h, a monomial ideal mi of m's tagged with g's representing
     * an ideal (g1,...,gn): a poly g is represented as (lead mono m,rest of g).
     * update pairs to include new s-pairs induced by h on g's:
     * 1) compute minimal gi1...gik so that <gij:h's> generate <gi:h's>, i.e.
     *    compute monomial ideal for gi:h's tagged with gi
     * 2) toss out gij's whose lead mono is rel. prime to h's lead mono (why?)
     * 3) put (h,gij) pairs into degree buckets: for h,gij with lead mono's m,m'
     *    deg(h,gij) = deg lcm(m,m') = deg (lcm/m) + deg m = deg (m':m) + deg m
     * 4) store list of pairs (h,g1),...,(h,gn) as vector (h,g1,...,gn)
     *)
    fun addPairs (h,mi,pairs) = 
	let
	    val m = P.leadMono h
	    val d = M.deg m
	    fun tag ((m' : M.mono,g' : P.poly ref),quots) = (inc maybePairs;
							     (M.divide(M.lcm(m,m'),m),(m',!g'))::quots)
	    fun insert ((mm,(m',g')),arr) = (* recall mm = m':m *)
	        if M.compare(m',mm)=Util.Equal then (* rel. prime *)
		    (inc primePairs; arr)
		else (inc usedPairs;
		      Util.insert(P.cons((F.one,m'),g'),M.deg mm+d,arr))
	    val buckets = MI.fold insert (MI.mkIdeal (MI.fold tag mi []))
		(array(0,[]))
	    fun ins (~1,pairs) = pairs
	      | ins (i,pairs) = case sub(buckets,i) of
		[] => ins(i-1,pairs)
	      | gs => ins(i-1,Util.insert(arrayoflist(h::gs),Int32.fromInt i,pairs))
	in ins(Array.length buckets - 1,pairs) 
	end

    fun grobner fs = let
	 fun pr l = print (string_implode (l@["\n"]))
	  val fs = revfold (fn (f,fs) => Util.insert(f,P.deg f,fs))
	      		   fs (array(0,[]))
	  (* pairs at least as long as fs, so done when done w/ all pairs *)
	  val pairs = ref(array(Array.length fs,[]))
	  val mi = MI.mkEmpty()
	  val newDegGens = ref []
          val addGen = (* add and maybe auto-reduce new monic generator h *)
	        if not(!autoReduce) then
		    fn h => MI.insert (mi,P.leadMono h,ref (P.rest h))
		else fn h => let
		    val ((_,m),rh) = P.leadAndRest h
		    fun autoReduce f =
			  if P.isZero f then f
			  else let val ((a,m'),rf) = P.leadAndRest f
			       in case M.compare(m,m') of
				   Util.Less => P.cons((a,m'),autoReduce rf)
				 | Util.Equal => P.subtract(rf,P.scalarMult(a,rh))
				 | Util.Greater => f
			       end
		    val rrh = ref rh
		    in
			MI.insert (mi,P.leadMono h,rrh);
			app (fn f => f:=autoReduce(!f)) (!newDegGens);
			newDegGens := rrh :: !newDegGens
		    end
	  val tasksleft = ref 0
	  fun feedback () = let
	        val n = !tasksleft
	        in 
		    if andb(n,15)=0 then print (makestring_int32 n) else (); 
			print "."; 
			TextIO.flushOut TextIO.stdOut;
			tasksleft := n-1
		end
	  fun try h = 
	      let
		  val _ = feedback ()
		  val h = reduce(h,mi)
	      in if P.isZero h 
		     then ()
		 else let val h = mkMonic h
			  val _ = (print "#"; TextIO.flushOut TextIO.stdOut)
		      in pairs := addPairs(h,mi,!pairs);
			  addGen h;
			  inc newGens
		      end
	      end
	  fun tryPairs fgs = 
	      let
		  val ((a,m),f) = P.leadAndRest (sub(fgs,0))
		  fun tryPair 0 = ()
		    | tryPair i =
		      let val ((b,n),g) = P.leadAndRest (sub(fgs,i))
			  val k = M.lcm(m,n)
		      in  try (P.spair(b,M.divide(k,m),f,a,M.divide(k,n),g));
			  tryPair (i-1)
		      end
	      in tryPair (Array.length fgs -1) 
	      end
	  fun numPairs ([],n) = Int32.fromInt n
	    | numPairs (p::ps,n) = numPairs(ps,n-1+Array.length p)
	  fun gb d = 
	      if d>=Array.length(!pairs) 
		  then mi 
	      else (* note: i nullify entries to reclaim space *)
	        (pr ["DEGREE ",makestring_int d," with ",
		     makestring_int32(numPairs(sub(!pairs,d),0))," pairs ",
		     if d>=Array.length fs then "0" else makestring_int(length(sub(fs,d))),
			 " generators to do"];
		 tasksleft := numPairs(sub(!pairs,d),0);
		 if d>=Array.length fs 
		     then () 
		 else tasksleft := !tasksleft + (Int32.fromInt (length (sub(fs,d))));
		 if d>(!maxDeg) 
		     then ()
		 else (reset();
		       newDegGens := [];
		       app tryPairs (sub(!pairs,d));
		       update(!pairs,d,[]);
		       if d>=Array.length fs then ()
		       else (app try (sub(fs,d)); update(fs,d,[]));
		       pr ["maybe ",makestring_int(!maybePairs)," prime ",makestring_int (!primePairs),
			   " using ",makestring_int (!usedPairs),"; found ",makestring_int (!newGens)]);
		 gb(d+1))
	in gb 0 
        end


local
    (* grammar:
     dig  ::= 0 | ... | 9
     var  ::= a | ... | z | A | ... | Z
     sign ::= + | -
     nat  ::= dig | nat dig
     mono ::=  | var mono | var num mono
     term ::= nat mono | mono
     poly ::= term | sign term | poly sign term
    *)
    datatype char = Dig of Int32.int | Var of Int32.int | Sign of Int32.int
    fun char ch =
	let 
	  val och = Int32.fromInt (ord ch)
	  val ord = Int32.fromInt o ord in
	  if ord #"0"<=och andalso och<=ord #"9" then Dig (och - ord #"0")
	  else if ord #"a"<=och andalso och<=ord #"z" then Var (och - ord #"a")
	  else if ord #"A"<=och andalso och<=ord #"Z" then Var (och - ord #"A" + 26)
	  else if och = ord #"+" then Sign 1
	  else if och = ord #"-" then Sign ~1
	       else Util.illegal ("bad ch in poly: " ^ (implode[ch]))
        end

    fun nat (n,Dig d::l) = nat(n*10+d,l) | nat (n,l) = (n,l)
    fun mono (m,Var v::Dig d::l) =
	  let val (n,l) = nat(d,l)
	  in mono(M.multiply(M.implode[(v,n)],m),l) end
      | mono (m,Var v::l) = mono(M.multiply(M.x_i v,m),l)
      | mono (m,l) = (m,l)

    fun term l = let
	  val (n,l) = case l of (Dig d::l) => nat(d,l) | _ => (1,l)
	  val (m,l) = mono(M.one,l)
    	  in ((F.coerceInt n,m),l) end
    fun poly (p,[]) = p
      | poly (p,l) = let
	  val (s,l) = case l of Sign s::l => (F.coerceInt s,l) | _ => (F.one,l)
	  val ((a,m),l) = term l
	  in poly(P.add(P.coerce(F.multiply(s,a),m),p),l) end

in
    fun parsePoly s = poly (P.zero,map char(char_explode s))

    fun readIdeal stream = let
	  fun readLine () = let
	        val s = TextIO.inputLine stream
		val n = size s
		val n = if n>0 andalso substring(s,n-1,1)="\n" then n-1 else n
		fun r i = if i>=n then []
			  else case substring(s,i,1) of
			      ";" => r(i+1)
			    | " " => r(i+1)
			    | _ => map char (char_explode(substring(s,i,n-i)))
		in r 0 end
	  fun r () = if TextIO.endOfStream stream then []
		     else poly(P.zero,readLine()) :: r()
	  fun num() = if TextIO.endOfStream stream then Util.illegal "missing #"
		      else case nat(0,readLine()) of
			  (_,_::_) => Util.illegal "junk after #"
			| (n,_) => n
	  val _ = 1=num() orelse Util.illegal "stream doesn't start w/ `1'"
	  val n = num()
	  val i = r()
	  val _ = (Int32.fromInt (length i)) = n orelse Util.illegal "wrong # poly's"
	  in i end

fun read filename = let
      val stream = TextIO.openIn filename
      val i = readIdeal stream
      val _ = TextIO.closeIn stream
      in i end
end (* local *) 

end (* structure G *)



val _ = G.maxDeg:=1000000

fun grab mi = MI.fold (fn ((m,g),l) => P.cons((F.one,m),!g)::l) mi []
fun r mi s = let
      val p = G.parsePoly s
      in print (P.display p); print "\n";
	 print (P.display(G.reduce(p,mi))); print "\n"
      end
fun p6 i= let val s= makestring_int (i:int)
	      val n= size s
          in print(substring("      ",0,6-n)); print s end
fun hex n = let
      fun h n = if n=0 then ""
		else h(n smlnj_div 16) ^ substring("0123456789ABCDEF",n smlnj_mod 16,1)
      in if n=0 then "0" else h n end
fun printCounts () = map (fn l => (map p6 l; print "\n")) (getCounts())
fun totalCount () = revfold (fn (l,c) => revfold op + l c) (getCounts()) 0
fun maxCount () = revfold (fn (l,m) => revfold max l m) (getCounts()) 0

fun terms (p,tt) = if P.isZero p then tt else terms(P.rest p,P.leadMono p::tt)
fun tails ([],tt) = tt
  | tails (t as _::t',tt) = tails (t',t::tt)

local
    val a = 16807.0  and  m = 2147483647.0
in
    val seed = ref 1.0
    fun random n = let val t = a*(!seed)
                   in seed := t - m * real(floor(t/m));
                      floor(real n * !seed/m)
                   end
end

fun sort [] = []
  | sort a = 
let
    val a = arrayoflist a
    val b = tabulate(Array.length a,fn i => i)
    val sub = sub and update = update
    infix sub
    fun swap (i,j) = let val ai = a sub i in update(a,i,a sub j); update(a,j,ai) end
    (* sort all k, 0<=i<=k<j<=length a *)
    fun s (i,j,acc) = if i=j then acc else let
        val pivot = a sub (b sub (i+random(j-i)))
	fun partition (dup,lo,k,hi) = if k=hi then (dup,lo,hi) else
	    (case M.compare (pivot, a sub (b sub k)) of
		 Util.Less => (swap (lo,k); partition (dup,lo+1,k+1,hi))
	       | Util.Equal => partition (dup+1,lo,k+1,hi)
	       | Util.Greater => (swap (k,hi-1); partition (dup,lo,k,hi-1)))
	val (dup,lo,hi) = partition (0,i,i,j)
	in s(i,lo,(dup,pivot)::s(hi,j,acc)) end
    in s(0,Array.length a,[]) end
;

fun sum f l = revfold op + (map f l) 0

fun analyze gb = let
      val aa = revfold terms gb []
      val bb = map M.explode aa
      val aa = sort aa
      fun len m = length (M.explode m)
      fun prt (s:string) (i:int) = (print s; print(makestring_int i); print "\n"; i)
      val m=  sum #1 aa
      val u=  length aa
      val cm =sum (fn (d,l) => d*len l) aa
      val cu =sum (len o #2) aa
      val for=length(sort(map M.implode (revfold tails bb [])))
      val bak=length(sort(map (M.implode o rev) (revfold tails (map rev bb) [])))
    in
     {m=prt "m  = " m, u=prt "u  = " u, cm =prt "cm = " cm, cu =prt "cu = " cu, for=prt "for= " for, bak=prt "bak= " bak}
    end

fun gb fs = let
      val g = G.grobner fs handle (Util.Illegal s) => (print s; raise Div)
      val fs = grab g
      fun info f = app print
	  [M.display(P.leadMono f),
	   " + ", makestring_int32(P.numTerms f - 1), " terms\n"]
      in app info fs end
;

fun report (e as Tabulate) = (print "exn: Tabulate\n"; raise e)
  | report (e as ArrayofList) = (print "exn: ArrayofList\n"; raise e)
  | report (e as (Util.NotImplemented s)) = (print ("exn: NotImplemented " ^ s ^ "\n"); raise e)
  | report (e as (Util.Impossible s)) = (print ("exn: Impossible " ^ s ^ "\n"); raise e)
  | report (e as (Util.Illegal s)) = (print ("exn: Illegal " ^ s ^ "\n"); raise e)
  | report (e as (M.DoesntDivide)) = (print ("exn: DoesntDivide\n"); raise e)
  | report (e as (MI.Found)) = (print ("exn: Found\n"); raise e)


val fs = map G.parsePoly ["-El-Dh-Cd+Bo+xn+tm","-Fo+Ep-Ek-Dg-Cc+Ao+wn+sm","-Fn-Ej+Dp-Df-Cb+zo+vn+rm",
"-Fm-Ei-De+Cp-Ca+yo+un+qm","Fl-Bp+Bk-Al-zh-yd+xj+ti","El-Bo-zg-yc+wj+si",
"Dl-Bn-Aj+zk-zf-yb+vj+ri","Cl-Bm-Ai-ze+yk-ya+uj+qi","Fh+Bg-xp+xf-wl-vh-ud+te",
"Eh+Ag-xo-wk+wf-vg-uc+se","Dh+zg-xn-wj-ub+re","Ch+yg-xm-wi-ve+uf-ua+qe",
"Fd+Bc+xb-tp+ta-sl-rh-qd","Ed+Ac+wb-to-sk+sa-rg-qc","Dd+zc+vb-tn-sj-rf+ra-qb",
"Cd+yc+ub-tm-si-re"]

val u7 = map G.parsePoly
["abcdefg-h7","a+b+c+d+e+f+g","ab+bc+cd+de+ef+fg+ga","abc+bcd+cde+def+efg+fga+gab","abcd+bcde+cdef+defg+efga+fgab+gabc","abcde+bcdef+cdefg+defga+efgab+fgabc+gabcd","abcdef+bcdefg+cdefga+defgab+efgabc+fgabcd+gabcde"]

val u6 = map G.parsePoly
["abcdef-g6","a+b+c+d+e+f","ab+bc+cd+de+ef+fa","abc+bcd+cde+def+efa+fab","abcd+bcde+cdef+defa+efab+fabc","abcde+bcdef+cdefa+defab+efabc+fabcd"]

val u5 = map G.parsePoly ["abcde-f5","a+b+c+d+e","ab+bc+cd+de+ea","abc+bcd+cde+dea+eab","abcd+bcde+cdea+deab+eabc"]

val u4 = map G.parsePoly ["abcd-e4","a+b+c+d","ab+bc+cd+da","abc+bcd+cda+dab"]


fun runit s = 
    let	val data = if (s = "fs") then fs else if (s = "u7") then u7 else if (s = "u6") then u6 else 
	    if (s = "u5") then u5 else if (s = "u4") then u4 else 
		(print "no such data\n"; raise (Util.Impossible "no such data"))
    in
	gb data handle e => report e
    end

fun readTyanInput() = 
    let val _ = (print "Enter fs, u7, u6, u5, or u4: "; TextIO.flushOut TextIO.stdOut);
	val s = TextIO.inputN(TextIO.stdIn,2)
    in s
    end
in
    fun runTyan() = runit "u6"  (* readTyanInput() *)
end
end

structure runTyan = makeRunTyan()
structure runTyan1 = makeRunTyan()
structure runTyan2 = makeRunTyan()
structure runTyan3 = makeRunTyan()

val runTyan = runTyan.runTyan
val runTyan1 = runTyan1.runTyan
val runTyan2 = runTyan2.runTyan
val runTyan3 = runTyan3.runTyan

