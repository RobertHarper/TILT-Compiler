signature TORTL_RECORD =
sig

    type regi = Rtl.regi
    type rep = Rtl.rep
    type reg = Rtl.reg
    type label = Rtl.label
    type term = TortlBase.term
    type state = TortlBase.state


    val record_tag_from_reps : rep list -> term

   (* If do_reject_nonValue is true, then static allocation occurs only if all fields are values.
      make_record              statically allocates if all fields are values or we are at top-level
      make_record_const        statically allocates because caller asserts non-value fields are invariant
      make_record_mutable      never statically allocate *)
    val do_reject_nonValue  : bool ref
    val empty_record        : term
    val make_record         : state * term list -> term * state
    val make_record_with_tag: state * term * term list -> term * state
    val make_record_const   : state * term list * label option -> term * state
    val make_record_mutable : state * term list -> term * state

   (* Add an extra field to the beginning of a record
      Remove the first field of a record
      These 2 functions are used for dynamic sum projections
    *)
   val record_insert : state * regi * regi * regi -> state * regi  (* state, record, record type, field : int
								      -> new state, new record *)
   val record_delete : regi -> regi         (* original record, new record *)



end
