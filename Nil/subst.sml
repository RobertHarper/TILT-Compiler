
(* Delayed/memoized computations *)
signature SUBST_DELAY =
sig
    type 'a delay
    val delay : (unit -> 'a) -> 'a delay
    val immediate : 'a -> 'a delay
    val thaw : 'a delay -> 'a
    val delayed : 'a delay -> bool
end


(* Functor parameter *)
signature SUBST_HELP =
sig
    structure Delay : SUBST_DELAY
    val error : string -> string -> 'a
end

(* Here we define the abstract interface for substitutions.  This section
 * defines the bits that are generic to all levels in a functor parameterized
 * by the actual substitution functions, which are level specific.
 *)

functor SubstFn(structure Help : SUBST_HELP
		type item                                          (*What is it: e.g. con, exp *)
		type item_subst =				   (*The substitution type  *)
		    item Help.Delay.delay Name.VarMap.map 
		    (* A mapping from variables in the substitution
		     * to delayed computations of their fully evaluated replacements (i.e., variables substituted
		     * "to their lefts" in the substitution are replaced). 
		     *)
		val substItemInItem : item_subst -> item -> item   (*For composition *)
		val renameItem : item -> item                      (*To avoid shadowing *)
		val printer : item -> unit)                        (*To print them out *)
  :> SUBST where type item = item
	     and type item_subst = item_subst =
struct

  (* Help *)
  val error = Help.error
  open Help.Delay

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

  fun empty () : item_subst = VarMap.empty

  (*Return the value (if any) that is associated with
   * the given variable in the given substitution
   *)
  fun substitute subst var = mapopt thaw (VarMap.find (subst,var))

  (*Return a list corresponding to the substitution
   * Note that toList (xxxfromList list) is not
   * required to be the identity.
   *)
  fun toList (subst : item_subst) = map_second thaw (VarMap.listItemsi subst)

  fun item_insert (s,var,delay) = VarMap.insert(s,var,delay)

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
  fun addl (var,item,subst) =
	let
	  val item_delay = rename item
	  val map_subst = item_insert(empty(),var,item_delay)
	  fun domap i = delay (fn () => substItemInItem map_subst (thaw i))
	in item_insert(VarMap.map domap subst,var,item_delay)
	end

  (* Add as if to the right of a sequential substition.
   *
   * addr (s,x,v) = s o {v/x} (where o is the composition operator
   *)
  fun addr (subst,var,item) =
	let
	  val item_delay = rename item
	in VarMap.insert (subst,var,delay (fn () => substItemInItem subst (thaw item_delay)))
	end

  (*Returns true if the subsitution is empty.  That is,
   * is_empty s => true iff forall x, substitute s x => NONE
   *)
  fun is_empty subst = (VarMap.numItems subst) = 0

  (*Create a substitution which when applies behaves as if the two
   * substitutions were applies sequentially.
   *
   * compose (s_1,s_2) = s_1 o s_2
   *)
  fun compose (s1 as subst1,subst2) =
	let
	  fun domap item_delay = delay (fn () => substItemInItem s1 (thaw item_delay))
	  val subst2 = VarMap.map domap subst2
	  val subst = VarMap.unionWith (fn _ => error "compose" "Duplicate variables in substitution composition") (subst1,subst2)
	in subst
	end

  (* Merge two substs, without looking at the ranges.
   * If the domains overlap at v, the image of v in the
   * new map is that of the second map
   *)
  fun merge (subst1,subst2) =
    VarMap.unionWith (fn _ => error "merge" "Duplicate variables in merged substitutions") (subst1,subst2)

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
  fun printf (printer : item -> unit) (subst : item_subst) =
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


