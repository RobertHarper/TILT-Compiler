functor OrderedSet (Set : ORD_SET)
    :> ORDERED_SET
	where type item = Set.item =
struct
    type item = Set.item
    type set = Set.set * item list
    val empty = (Set.empty, [])
    fun member (i : item,(set,_) : set) : bool = Set.member(set,i)
    fun cons (i : item,(set,list) : set) : set =
	if (Set.member(set,i))
	    then (set,list)
	else (Set.add(set,i), i::list)
    fun toList ((_,list) : set) : item list = list
    fun append (s1 : set, s2 : set) : set = foldr cons s2 (toList s1)
end

structure Util :> UTIL =
struct
    exception UNIMP

    (* avoid shadowing error since we export it at a different type! *)
    fun localerror s = UtilError.error "util.sml" s

    fun loop a b = if (a>b) then [] else a::(loop (a+1) b)
    fun count n = loop 0 (n-1)

    local
	val precomputeMax = 10
	fun rawSpaces 0 = ""
	  | rawSpaces 1 = " "
	  | rawSpaces n =
	    let val _ = if (n < 2) then localerror "rawSpaces given negative number" else ()
		val a = n div 2
		val b = n - a
		val aSpace = rawSpaces a
		val bSpace = if (a = b) then aSpace
			     else (aSpace ^ " ")
	    in  aSpace ^ bSpace
	    end
	val precompute = Array.tabulate(precomputeMax, rawSpaces)
    in  fun spaces n =
	if (n < 0)
	    then ""
	else if (n < precomputeMax)
		 then Array.sub(precompute,n)
	     else rawSpaces n
    end

    fun printl s = print (s^"\n")
    fun lprint s = print ("\n"^s)
    fun lprintl s = print ("\n"^s^"\n")

    fun printem strings = app print strings

    fun mapopt f NONE = NONE
      | mapopt f (SOME x) = SOME(f x)
    fun eq_opt (f, NONE, NONE) = true
      | eq_opt (f, SOME a, SOME b) = f(a,b)
      | eq_opt _ = false
    fun split_opt (NONE) = (NONE,NONE)
      | split_opt (SOME (a,b)) = (SOME a,SOME b)

    fun CharStr2char s = String.sub(s,0)

    type 'a oneshot = 'a option ref
    fun oneshot () = ref NONE
    fun oneshot_init a = ref (SOME a)
    fun oneshot_set (ref (SOME _), _) = localerror "oneshot_set called on something already set"
      | oneshot_set (r, a) = r := SOME a
    val oneshot_deref = !
    val eq_oneshot = op =

    fun substring (pattern,target) =
	let val pattern = explode pattern
	    val target = explode target
	    fun match [] _ = true
	      | match (a::arest) (b::brest) = (a = b) andalso (match arest brest)
	      | match _ _ = false
	    fun loop n cur = if (match pattern cur)
				 then SOME n
			     else (case cur of
				       [] => NONE
				     | (_::rest) => loop (n+1) rest)
	in  loop 0 target
	end

    fun curry2 f = fn a => fn b => f (a,b)
    fun curry3 f = fn a => fn b => fn c => f (a,b,c)

    fun all_pairs p =
      let
	fun loop [] = true
	  | loop [_]= true
	  | loop (fst::snd::rest) =
	  (p (fst,snd)) andalso (loop (snd::rest))
      in
	loop
      end

    fun memoize thunk =
	let val cell = ref thunk
	    val _ = cell := (fn () =>
			     let val r = thunk()
			     in  (cell := (fn () => r);
				  r)
			     end handle e =>
				 (cell := (fn () => raise e);
				  raise e))
	in  fn () => !cell()
	end

   fun system (command : string) : bool =
       OS.Process.system command <> OS.Process.failure

    val raise_error = UtilError.raise_error
    val error = UtilError.error

    local
	structure StringKey =
	struct
	    type ord_key = string
	    val compare = String.compare
	end
    in  structure StringMap = SplayMapFn(StringKey)
	structure StringSet = SplaySetFn(StringKey)
	structure StringOrderedSet = OrderedSet(StringSet)
    end
end
