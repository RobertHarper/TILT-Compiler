(*$import Name *)

(* Trace information for the garbage collector *)

signature TRACEINFO = 
sig

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
