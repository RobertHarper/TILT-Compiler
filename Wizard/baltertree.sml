functor BalancedTernaryTree(structure Compare:COMPARE) :> TERNARYTREE
  where type elem = Compare.t =
  struct
    exception NotFound
    type elem = Compare.t
    type key = Compare.t list
      
    datatype 'a ternaryTree =
      Leaf of 'a option
    | NextTree of {datum : 'a option, match : 'a matchTree}
    and 'a matchTree =
      MatchLeaf
    | MatchTree of
      {split : elem,
       eq : 'a ternaryTree,
       lo : 'a matchTree,
       hi : 'a matchTree,
       weight : real}
	     
    val empty = Leaf NONE
    
    val m = 2147483647.0
    val a = 16807.0
    val rs = ref 1.0
      
    fun rand () =
      let
	val t = a * !rs
	val r = t - m * real(floor(t/m))
      in
	rs := r;
	r
      end	 

    fun maintainLo (MatchTree {split, hi, lo, eq, weight},
		    newLo as MatchTree{split=split', hi=hi', lo=lo',
				       eq=eq', weight=weight'}) =
      if weight >= weight' then
	MatchTree {split=split, hi=hi, lo=newLo, eq=eq, weight=weight}
      else
	let
	  val newHi = MatchTree{split=split, hi=hi,
				lo=hi', eq=eq, weight=weight}
	in 
	  MatchTree {split=split', hi=newHi, lo=lo', eq=eq', weight=weight'}
	end
      
    fun maintainHi (MatchTree {split, hi, lo, eq, weight},
		    newHi as MatchTree{split=split', hi=hi', lo=lo',
				       eq=eq', weight=weight'}) =
      if weight >= weight' then
	MatchTree {split=split, hi=newHi, lo=lo, eq=eq, weight=weight}
      else
	let
	  val newLo = MatchTree{split=split, hi=lo',
				lo=lo, eq=eq, weight=weight}
	in 
	  MatchTree {split=split', hi=hi', lo=newLo, eq=eq', weight=weight'}
	end
    
    fun insert (Leaf _) [] datum' = Leaf (SOME datum')
      | insert (Leaf datum) key datum' =
	NextTree {match = insert' MatchLeaf key datum',
		  datum = datum}
      | insert (NextTree {match, ...}) [] datum' =
	NextTree {match = match, datum = SOME datum'}
      | insert (NextTree {match, datum}) key datum' =
	NextTree {match = insert' match key datum', datum = datum}
    and insert' MatchLeaf (k::ks) datum' =
      MatchTree{split = k, eq = insert empty ks datum',
		hi = MatchLeaf, lo = MatchLeaf, weight = rand()}
      | insert' (t as MatchTree {split, hi, lo, eq, weight})
                (key as k::ks) datum' =
	case Compare.compare(k, split) of
	  LESS => maintainLo(t, insert' lo key datum')
	| GREATER => maintainHi(t, insert' hi key datum')
	| EQUAL => MatchTree
	    {split = split, lo = lo, hi = hi,
	     eq = insert eq ks datum', weight = weight}      
  end (* TernaryTree *)
