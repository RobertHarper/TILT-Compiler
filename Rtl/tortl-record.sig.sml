(*$import Rtl Nil TortlBase *)

signature TORTL_RECORD = 
sig

    val do_constant_records : bool ref
    val do_forced_constant_records : bool ref

    type regi = Rtl.regi
    type rep = Rtl.rep
    type reg = Rtl.reg
    type label = Rtl.label
    type term = TortlBase.term
    type state = TortlBase.state

   (* make_record statically allocates if all the arguments are values or we are at top-level
      make_record_const will always statically allocate
      make_record_mutable will never statically allocate *)
   val empty_record        : term
   val make_record         : state * term list -> term * state
   val make_record_const   : state * term list * label option -> term * state
   val make_record_mutable : state * term list -> term * state
   val record_project      : regi * int * regi -> unit   (* register contaiing record, index of field, dest register *)

   (* Add an extra field to the beginning of a record
      Remove the first field of a record
      These 2 functions are used for dynamic sum projections 
    *)
   val record_insert : state * regi * regi * regi -> state * regi  (* state, record, record type, field : int 
								      -> new state, new record *)
   val record_delete : regi -> regi         (* original record, new record *)

   val HeapProfile : bool ref
   val SetHeapProfileCounter : int -> unit
   val MakeProfileTag : unit -> TilWord32.word


end