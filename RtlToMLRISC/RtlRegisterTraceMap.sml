
(* =========================================================================
 * RtlRegisterTraceMap.sml
 * ========================================================================= *)

functor RtlRegisterTraceMap(
	  structure Cells:	 CELLS
	  structure IntSet:	 ORD_SET where type Key.ord_key = int
	  structure RegisterMap: REGISTER_MAP where type id = int
	  structure Rtl:	 RTL
	  structure TraceTable:	 TRACETABLE

	  sharing type Rtl.label = TraceTable.Machine.Rtl.label
	      and type Rtl.rep	 = TraceTable.Machine.Rtl.rep
	) :> REGISTER_TRACE_MAP
	       where type var		= Rtl.var
		 and type id		= RegisterMap.id
		 and type rep		= Rtl.rep
		 and type trace		= TraceTable.trace
		 and type stacklocation = TraceTable.Machine.stacklocation
	  = struct

  (* -- types -------------------------------------------------------------- *)

  type var = Rtl.var

  type id = RegisterMap.id

  type rep = Rtl.rep

  type trace = TraceTable.trace

  type register = var * rep

  type stacklocation = TraceTable.Machine.stacklocation

  (*
   * A trace value whose polymorphic spills might not be resolved to physical
   * stack locations (yet).
   *)
  datatype trace' =
    Trace of trace
  | TraceStack of id
  | TraceStackRec of id * int list

  (*
   * An Rtl register trace mapping is a a mapping from Rtl register ids to
   * MLRISC pseudo-register ids, and a mapping from MLRISC pseudo-register
   * ids to internal trace values.
   *)
  type map = id RegisterMap.map * trace' RegisterMap.map

  (* -- exceptions --------------------------------------------------------- *)

  exception InvalidSource of string

  exception NotMapped of id

  (* -- representation functions ------------------------------------------- *)

  (*
   * Translate an Rtl representation to a trace value.
   * lookup -> the lookup function to use for registers in polymorphic values
   *	    -> the Rtl representation to translate
   * <- the equivalent trace value
   *)
  local
    val traceYes   = Trace TraceTable.TRACE_YES
    val traceNo	   = Trace TraceTable.TRACE_YES
    val traceUnset = Trace TraceTable.TRACE_YES

    fun traceStack _ (Rtl.SREGI _) =
	  raise InvalidSource "polymorphic special register"
      | traceStack lookup (Rtl.REGI register) =
	  TraceStack(lookup register)

    fun traceStackRec _ (Rtl.SREGI _, _) =
	  raise InvalidSource "polymorphic special register"
      | traceStackRec lookup (Rtl.REGI register, indices) =
	  TraceStackRec(lookup register, indices)

    val traceGlobal	= Trace o TraceTable.TRACE_GLOBAL
    val traceGlobalRec	= Trace o TraceTable.TRACE_GLOBAL_REC
    val traceImpossible = Trace TraceTable.TRACE_IMPOSSIBLE

    fun tracePath lookup (Rtl.Var_p register) =
	  traceStack lookup register
      | tracePath lookup (Rtl.Projvar_p register) =
	  traceStackRec lookup register
      | tracePath _ (Rtl.Label_p label) =
	  traceGlobal label
      | tracePath _ (Rtl.Projlabel_p label) =
	  traceGlobalRec label
      | tracePath _ Rtl.Notneeded_p =
	  traceImpossible
  in
    fun traceRepresent _       Rtl.TRACE	 = traceYes
      | traceRepresent _       Rtl.LOCATIVE	 = traceImpossible
      | traceRepresent lookup (Rtl.COMPUTE path) = tracePath lookup path
      | traceRepresent _       Rtl.UNSET	 = traceUnset
      | traceRepresent _       Rtl.NOTRACE_INT	 = traceNo
      | traceRepresent _       Rtl.NOTRACE_REAL	 = traceNo
      | traceRepresent _       Rtl.NOTRACE_CODE	 = traceNo
      | traceRepresent _       Rtl.LABEL	 = traceNo
  end

  (*
   * Translate an internal trace value to a trace table trace value.
   * spill -> the spill function to use for registers in polymorphic values
   *	   -> the value to translate
   * <- the equivalent trace table trace value
   *)
  fun trace' _ (Trace trace) =
	trace
    | trace' spill (TraceStack id) =
	TraceTable.TRACE_STACK(spill id)
    | trace' spill (TraceStackRec(id, indices)) =
	TraceTable.TRACE_STACK_REC(spill id, indices)

  (* -- functions ---------------------------------------------------------- *)

  fun map() = (RegisterMap.map(fn _ => Cells.newReg()),
	       RegisterMap.map(fn id => raise NotMapped id))

  fun lookup lookup' (map, traceMap) (var, represent) =
	let
	  val id = RegisterMap.lookup map (Name.var2int var)
	in
	  (* don't store trace value each time ??? *)
	  RegisterMap.insert traceMap (id, traceRepresent lookup' represent);
	  id
	end

  fun assign (_, traceMap) (id, trace) =
	RegisterMap.insert traceMap (id, Trace trace)

  fun trace spill (_, traceMap) =
	trace' spill o RegisterMap.lookup traceMap

  local
    fun polySpillsTrace(Trace _, set)		   = set
      | polySpillsTrace(TraceStack id, set)	   = IntSet.add(set, id)
      | polySpillsTrace(TraceStackRec(id, _), set) = IntSet.add(set, id)

    fun polySpills1 traceMap (id, set) =
	  polySpillsTrace(RegisterMap.lookup traceMap id, set)

    fun member set item = IntSet.member(set, item)
  in
    fun polySpills(_, traceMap) =
	  member o foldr (polySpills1 traceMap) IntSet.empty
  end

  fun resetSource(map, _) =
	RegisterMap.reset map

  fun resetTarget(_, traceMap) =
	RegisterMap.reset traceMap

  local
    fun invalid _ = raise InvalidSource "stack-based polymorphic global"
  in
    val traceGlobalRepresent = trace' invalid o traceRepresent invalid
  end

end

