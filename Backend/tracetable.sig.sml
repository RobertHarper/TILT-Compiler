(*$import RTL MACHINE *)
signature TRACETABLE =
  sig
    structure Machine : MACHINE

    datatype calllabel = CALLLABEL of Rtl.label
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
    val MakeTableHeader  : string            -> Rtl.data list
    val MakeTable        : callinfo list     -> Rtl.data list
    val MakeTableTrailer : string            -> Rtl.data list

    (* MakeMutableTable: given list of traceable global variables,
       construct table.   A ``global variable'' is a memory location containing
       a possibly traceable value.*)

    val MakeMutableTable  : string * (Rtl.label * trace) list -> Rtl.data list

    val trace2string : trace -> string
  end;


