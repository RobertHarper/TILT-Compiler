(*$import *)
extern Spawn : (unit -> unit, unit) -->
extern Yield : (unit, unit) -->
datatype 'a option = NONE | SOME of 'a
