(*$import Prelude Name TRACEINFO *)

structure TraceInfo :> TRACEINFO = 
struct

  datatype traceinfo = 
              Trace 
            | Unset
            | Notrace_Int
            | Notrace_Code
            | Notrace_Real
            | Label
            | Locative
            | Compute of Name.var * Name.label list

end
