extern posix_time_gettimeofday : (unit, (int * int) cresult) -->

(* user sec, user usec, system sec, system usec *)
extern posix_time_getrusage_self : (unit, (int * int * int * int) cresult) -->

(* wall clock sec, wall clock msec *)
extern posix_time_ftime : (unit, int * int) -->
