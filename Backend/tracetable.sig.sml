(*$import RTL MACHINE *)
signature TRACETABLE =
  sig
    structure Machine : MACHINE

    datatype calllabel = CALLLABEL of Machine.loclabel
    datatype trace     = TRACE_YES 
                       | TRACE_NO
                       | TRACE_UNSET   (* unset variable; handle specially for gener GC *)
		       | TRACE_CALLEE     of Machine.register
			 (* should be resolved to ACTUAL stack locations in the end *)
		       | TRACE_STACK      of Machine.stacklocation
		        (* stack pos, rec pos *)
		       | TRACE_STACK_REC  of Machine.stacklocation * int list
		       | TRACE_GLOBAL     of Machine.label
		       | TRACE_GLOBAL_REC of Machine.label * int list

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
    val MakeTableHeader  : string            -> Machine.data list
    val MakeTable        : callinfo list     -> Machine.data list
    val MakeTableTrailer : string            -> Machine.data list

    (* MakeGlobalTable: given list of traceable global variables,
       construct table.   A ``global variable'' is a memory location containing
       a traceable value.*)

    val MakeGlobalTable  : string * (Machine.label * trace) list -> Machine.data list

    (* MakeGlobalTable: given a list of addresses of mutable statically-allocated
       objects, construct table.   These objects are statically-allocated arrays 
       and record which could be side-effected.*)

    val MakeMutableTable : string * Machine.label list -> Machine.data list

    val trace2string : trace -> string
  end;


