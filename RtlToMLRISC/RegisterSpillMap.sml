
(* =========================================================================
 * RegisterSpillMap.sml
 * ========================================================================= *)

functor RegisterSpillMap(
	  structure MLRISCConstant: MLRISC_CONSTANT
	) :> REGISTER_SPILL_MAP
	       where type id	 = DenseRegisterMap.id
		 and type offset = MLRISCConstant.const
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure RegisterMap = DenseRegisterMap

  (* -- types -------------------------------------------------------------- *)

  type id = RegisterMap.id

  type offset = MLRISCConstant.const

  (*
   * A register spill map is a register map of spill offsets along with a
   * deferred register interval mapping function.
   *)
  type value = offset * offset
  type map   = value RegisterMap.map * (id -> value) ref

  (* -- exceptions --------------------------------------------------------- *)

  exception NotMapped of id

  (* -- functions ---------------------------------------------------------- *)

  fun notMapped id = (raise NotMapped id): value

  fun map() =
	let
	  val defaultRef = ref notMapped
	in
	  (RegisterMap.map(fn id => (!defaultRef) id), defaultRef)
	end

  fun insert(map, _) = RegisterMap.insert map

  fun defer (_, defaultRef) (predicate, deferral) =
	let
	  (*
	   * don't use linear search for this ???
	   *)
	  val default = !defaultRef

	  fun default' id = if predicate id then deferral id else default id
	in
	  defaultRef := default'
	end

  fun lookup(map, _)   = RegisterMap.lookup map: id -> value
  fun lookupSpill map  = #1 o lookup map
  fun lookupReload map = #2 o lookup map

  local
    fun compose f g =
	  let
	    fun f'(SOME value) = SOME(f value)
	      | f' NONE	       = NONE
	  in
	    f' o g
	  end
  in
    fun test(map, _)   = RegisterMap.test map: id -> value option
    fun testSpill map  = compose #1 (test map)
    fun testReload map = compose #2 (test map)
  end

  fun reset(map, defaultRef) = (RegisterMap.reset map;
				defaultRef := notMapped)

end

