signature TIMEANDRUN = 
  sig
    val report : int * (((unit -> 'a) * string) list) -> unit
  end
