signature TRACETABLE =
  sig
    structure Machine: sig

      datatype register = R of int
                        | F of int
      type loclabel = Rtl.local_label
      datatype stacklocation = CALLER_FRAME_ARG of int
                             | THIS_FRAME_ARG of int
                             | SPILLED_INT of int
                             | SPILLED_FP of int
                             | ACTUAL4 of int
                             | ACTUAL8 of int
                             | RETADD_POS
    end

    datatype calllabel = CALLLABEL of Machine.loclabel
    datatype trace     = TRACE_YES 
                       | TRACE_NO
                       | TRACE_UNSET   (* unset variable; handle specially for gener GC *)
		       | TRACE_CALLEE     of Machine.register
			 (* should be resolved to ACTUAL stack locations in the end *)
		       | TRACE_STACK      of Machine.stacklocation
		        (* stack pos, rec pos *)
		       | TRACE_STACK_REC  of Machine.stacklocation * int list
		       | TRACE_GLOBAL     of Rtl.label
		       | TRACE_GLOBAL_REC of Rtl.label * int list

		       (* trace status should never be needed.  A bug 
			  if it is.*)
 
		       | TRACE_IMPOSSIBLE  

    datatype callinfo = CALLINFO of 
      {calllabel  : calllabel, 
       framesize  : int,
       retaddpos  : int,
       regtrace   : (Machine.register * trace) list,
       stacktrace : (int * trace) list                   
       }

    val ShowDebug        : bool ref
    val ShowDiag         : bool ref
    val TagEntry         : bool ref
    val MakeTableHeader  : string            -> Rtl.data list
    val MakeTable        : callinfo list     -> Rtl.data list
    val MakeTableTrailer : string            -> Rtl.data list

    (* MakeGlobalTable: given list of traceable global variables,
       construct table.   A ``global variable'' is a memory location containing
       a traceable value.*)

    val MakeGlobalTable  : string * (Rtl.label * trace) list -> Rtl.data list

    (* MakeGlobalTable: given a list of addresses of mutable statically-allocated
       objects, construct table.   These objects are statically-allocated arrays 
       and record which could be side-effected.*)

    val MakeMutableTable : string * Rtl.label list -> Rtl.data list

    val trace2string : trace -> string
  end;


