(*Abstract substitutions used in NILSUBST*)
signature SUBST =
  sig
    type var = Name.var
    type item
    type item_subst

    val empty : unit -> item_subst

    (*Return the value (if any) that is associated with
     * the given variable in the given substitution
     *)
    val substitute : item_subst -> var -> item option

    (*Add as if to a simultaneous substitution.
     * substitute (sim_add (s,x,v)) x => v
     * if s = {v_1/x_1,...,v_n/x_n} then
     * sim_add (s,x,v) = {v_1/x_1,...,v_n/x_n,v/x}
     * If x is in the domain of s, then v is the new value for x
     *)
    val sim_add : item_subst -> (var * item) -> item_subst

    (* Add as if to the left of a sequential substition.
     *
     * addl (x,v,s) = {v/x} o s (where o is the composition operator
     *)
    val addl : (var * item * item_subst) -> item_subst

    (* Add as if to the right of a sequential substition.
     *
     * addr (s,x,v) = s o {v/x} (where o is the composition operator
     *)
    val addr : (item_subst * var * item) -> item_subst


    (*Returns true if the subsitution is empty.  That is,
     * is_empty s => true iff forall x, substitute s x => NONE
     *)
    val is_empty : item_subst -> bool

    (*Create a substitution which when applies behaves as if the two
     * substitutions were applies sequentially.
     *
     * compose (s_1,s_2) = s_1 o s_2
     *)
    val compose : (item_subst * item_subst) -> item_subst

    (* Merge two substs, without looking at the ranges.
     * If the domains overlap at v, the image of v in the
     * new map is that of the second map
     *)
    val merge : (item_subst * item_subst) -> item_subst

    (* Treat the list as a simultaneous substitution.
     * Duplicate variables in the argument list may lead to
     * undefined behaviour.
     *)
    val simFromList : (var * item) list -> item_subst

    (* Treat the list as a sequential substitution, with the
     * last element of the list substituted first, and the
     * first element substituted last.
     *)
    val seqFromList : (var * item) list -> item_subst

    (*Return a list corresponding to the substitution
     * Note that toList (xxxfromList list) is not
     * required to be the identity.
     *)
    val toList   : item_subst -> (var * item) list

    (* Print every mapping in the substitution using the given item printing function *)
    val printf : (item -> unit) -> item_subst -> unit
    (* Print every mapping in the substitution using the default item printing function *)
    val print : item_subst -> unit
  end

