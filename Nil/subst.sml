
(* Delayed/memoized computations *)
signature SUBST_DELAY =
sig
    type 'a delay
    val delay : (unit -> 'a) -> 'a delay
    val immediate : 'a -> 'a delay
    val thaw : 'a delay -> 'a
    val delayed : 'a delay -> bool
end

(* Lists with efficient insertion to both beginning and end *)
signature SUBST_DLIST =
sig
    type 'a dlist
    val DNIL : 'a dlist
    val LCONS : 'a * 'a dlist -> 'a dlist
    val RCONS : 'a dlist * 'a -> 'a dlist
    val dempty : 'a dlist -> bool
    val dListToList : 'a dlist -> 'a list
    val dListFromList : 'a list -> 'a dlist
    val dListApp : 'a dlist * 'a dlist -> 'a dlist
end

(* Functor parameter *)
signature SUBST_HELP =
sig
    structure Delay : SUBST_DELAY
    structure Dlist : SUBST_DLIST
    val make_lets : bool ref
	(* make_lets controls whether or not a particular optimization involving singleton kinds is used:
	 When substituting [c_i/a_i]S(c'), the optimization returns S(Let a_i = c_i In c' End) instead of performing the
	 substitution fully. *)
    val error : string -> string -> 'a
end

(* Here we define the abstract interface for substitutions.  This section
 * defines the bits that are generic to all levels in a functor parameterized
 * by the actual substitution functions, which are level specific.
 *)

functor SubstFn(structure Help : SUBST_HELP
		type item                                          (*What is it: e.g. con, exp *)
		type item_subst =				   (*The substitution type  *)
		    item Help.Delay.delay Name.VarMap.map * (Name.var * item Help.Delay.delay) Help.Dlist.dlist
		    (* The first part of this pair is a mapping from variables in the substitution
		     to delayed computations of their fully evaluated replacements (i.e., variables substituted
		     "to their lefts" in the substitution are replaced). The second pair is a straightforward list
		     of variables and replacements, stored in a "double-ended list." It is maintained as empty if
		     !make_lets is false. *)
		val substItemInItem : item_subst -> item -> item   (*For composition *)
		val renameItem : item -> item                      (*To avoid shadowing *)
		val printer : item -> unit)                        (*To print them out *)
  :> SUBST where type item = item
	     and type item_subst = item_subst =
struct

  (* Help *)
  val make_lets = Help.make_lets
  val error = Help.error
  open Help.Dlist Help.Delay

  (* Util *)
  val mapopt   = Util.mapopt

  (* Listops *)
  val map_second = Listops.map_second

  (* Name *)
  structure VarMap = Name.VarMap
  type var = Name.var


  type item = item
  type item_subst = item_subst

  fun rename (item :item) : item delay = delay (fn () => renameItem item)

  fun empty () : item_subst = (VarMap.empty,DNIL)

  (*Return the value (if any) that is associated with
   * the given variable in the given substitution
   *)
  fun substitute (subst,items) var = mapopt thaw (VarMap.find (subst,var))

  (*Return a list corresponding to the substitution
   * Note that toList (xxxfromList list) is not
   * required to be the identity.
   *)
  fun toList (subst : item_subst) = if !make_lets then map_second thaw (dListToList (#2 subst)) else (map_second thaw (VarMap.listItemsi (#1 subst)))
  (* The dList won't be kept if the make_lets optimization isn't used, but we want to use it if it's there for its possibly
   smaller/already computed replacements. *)

  fun item_insert (subst as (s,i),var,delay) =
	(VarMap.insert(s,var,delay),RCONS(i,(var,delay)))

  (*Add as if to a simultaneous substitution.
   * substitute (sim_add (s,x,v)) x => v
   * if s = {v_1/x_1,...,v_n/x_n} then
   * sim_add (s,x,v) = {v_1/x_1,...,v_n/x_n,v/x}
   * If x is in the domain of s, then v is the new value for x
   *)
  fun sim_add subst (var,value) : item_subst = item_insert(subst,var,rename value)

  (* Add as if to the left of a sequential substition.
   *
   * addl (x,v,s) = {v/x} o s (where o is the composition operator
   *)
  fun addl (var,item,(subst,items)) =
	let
	  val item_delay = rename item
	  val map_subst = item_insert(empty(),var,item_delay)
	  fun domap i = delay (fn () => substItemInItem map_subst (thaw i))
	in item_insert((VarMap.map domap subst,items),var,item_delay)
	end

  (* Add as if to the right of a sequential substition.
   *
   * addr (s,x,v) = s o {v/x} (where o is the composition operator
   *)
  fun addr (s as (subst,items),var,item) =
	let
	  val item_delay = rename item
	in (VarMap.insert (subst,var,delay (fn () => substItemInItem s (thaw item_delay))),
	    RCONS(items,(var,item_delay)))
	end

  (*Returns true if the subsitution is empty.  That is,
   * is_empty s => true iff forall x, substitute s x => NONE
   *)
  fun is_empty (subst,_) = (VarMap.numItems subst) = 0

  (*Create a substitution which when applies behaves as if the two
   * substitutions were applies sequentially.
   *
   * compose (s_1,s_2) = s_1 o s_2
   *)
  fun compose (s1 as (subst1,items1),(subst2,items2)) =
	let
	  fun domap item_delay = delay (fn () => substItemInItem s1 (thaw item_delay))
	  val subst2 = VarMap.map domap subst2
	  val subst = VarMap.unionWith (fn _ => error "compose" "Duplicate variables in substitution composition") (subst1,subst2)
	in (subst,dListApp(items1,items2))
	end

  (* Merge two substs, without looking at the ranges.
   * If the domains overlap at v, the image of v in the
   * new map is that of the second map
   *)
  fun merge ((subst1,items1),(subst2,items2)) =
	(VarMap.unionWith (fn _ => error "merge" "Duplicate variables in merged substitutions") (subst1,subst2),
	 dListApp(items1,items2))

  (* Treat the list as a simultaneous substitution.
   * Duplicate variables in the argument list may lead to
   * undefined behaviour.
   *)
  fun simFromList (list : (var * item) list) : item_subst = List.foldl (fn (v,s) => sim_add s v) (empty()) list

  (* Treat the list as a sequential substitution, with the
   * last element of the list substituted first, and the
   * first element substituted last.
   *)
  fun seqFromList (list : (var * item) list) : item_subst = List.foldl (fn ((v,i),s) => addr (s,v,i)) (empty()) list

  (* Print every mapping in the substitution using the given item printing function *)
  (* N.B. doesn't print items
   *)
  fun printf (printer : item -> unit) ((subst,items): item_subst) =
	let
	  fun print1 (v,a) =
	    (TextIO.print (Name.var2string v);
	     TextIO.print "->";
	     printer (thaw a);
	     TextIO.print "\n")
	in
	  (Util.lprintl "Substitution is";
	   VarMap.appi print1 subst;
	   Util.printl "")
	end

  (* Print every mapping in the substitution using the default item printing function *)
  val print = printf printer
end


