(*$import Int *)

(*
string_option make_some (string s) {
  return (ptr_t) s;
}
string_option make_none (unit ignored) {
  return 0;
}

typedef ptr_t int_option;

int_option make_some_int (int i) {
  return alloc_manyint(1, i);
}

int_option make_none_int (unit ignored) {
  return 0;
}

typedef ptr_t exn;

exn make_some_exn (string s, int e) {
  return mkSysErrExn (s, 1, e);
}

exn make_none_exn (string s) {
  return mkSysErrExn (s, 0, 0);
}
*)
extern make_some : (string, string option) -->
extern make_none : (unit, string option) -->

fun o2s (SOME s) = "SOME " ^ s
  | o2s (NONE) = "NONE"

val _ = (print (o2s (Ccall(make_some,"test"))); print "\n";
	 print (o2s (Ccall(make_none,()))); print "\n")

extern make_some_int : (int, int option) -->
extern make_none_int : (unit, int option) -->

fun o2s (SOME i) = "SOME " ^ (Int.toString i)
  | o2s (NONE) = "NONE"

val _ = (print (o2s (Ccall(make_some_int,333))); print "\n";
	 print (o2s (Ccall(make_none_int,()))); print "\n")

extern make_some_exn : (string, int, exn) -->
extern make_none_exn : (string, exn) -->

fun e2s (TiltExn.SysErr (s, io)) = ("SysErr " ^ s ^ " " ^ o2s io)
  | e2s _ = "BAD"

val _ = (print (e2s (Ccall(make_some_exn,"test",333))); print "\n";
	 print (e2s (Ccall(make_none_exn,"test"))); print "\n")
