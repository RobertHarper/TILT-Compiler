(*$import Firstlude TiltPrim Prelude *)

extern ml_timeofday : (unit, (int * int)) -->

(* user sec, user usec, system sec, system usec *)
extern til_selfusage : (unit, int * int * int * int) -->

(* wall clock sec, wall clock msec *)
extern til_realtime : (unit, int * int) --> 

structure PreTime =
struct
    
    datatype time = TIME of {sec : int, usec : int}
	
end
