(*$import Time TopLevel List *)

signature TRACE = sig
  type trace

  val mintime : Time.time ref
  val reset : unit -> unit

  val newtrace : string -> trace
  val enter : trace -> int
  val exit : trace -> Time.time
end

structure Trace :> TRACE = struct

  val tracecount = ref 0

  local
      fun spaces' 0 = []
	| spaces' n = " " :: spaces' (n-1)
  in
      fun spaces n =
        if (n > 60) then ("*" :: spaces (n - 60)) else (spaces' n)
  end
  

  fun reset () = (tracecount := 0)

  type trace = {name : string,
                calls : int ref,
                callstack : (Time.time * int) list ref}
		

  fun newtrace s : trace = 
      {name = s ^ " : ", calls = ref 0, callstack = ref nil}

  fun enter {name, calls as ref call_number, callstack} =
     let
	 val _ = List.app print (spaces (!tracecount))
	 val _ = print name
	 val _ = print (Int.toString call_number)
         val _ = print "\n"
	 val _ = (tracecount := (!tracecount) + 1)
	 val _ = (calls := (!calls) + 1)
         val _ = callstack := (Time.now(), call_number) :: (!callstack)
     in
	 call_number
     end

 val mintime = ref (Time.fromReal 0.25)

 fun exit {name, calls, callstack as ref ((t1,call_number)::rest)} =
     let
         val elapsed = Time.- (Time.now(), t1)
	 val _ = (tracecount := (!tracecount) - 1)
	 val _ = List.app print (spaces (!tracecount))
	 val _ = print name
	 val _ = print (Int.toString call_number)
	 val _ = if (Time.>(elapsed, !mintime)) then 
	            (print " time = "; print (Time.toString elapsed))
                 else ()
	 val _ = print "\n"
	 val _ = callstack := rest
     in 
	 elapsed
     end             


end