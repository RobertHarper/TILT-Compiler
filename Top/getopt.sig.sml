(*$import Prelude *)

signature GETOPT =
sig

    datatype 'a opt =
	Noarg of char * 'a
      | Arg of char * (string -> 'a)

    datatype 'a result =
	Error of string
      | Success of 'a list * string list

    val getopt : 'a opt list * string list -> 'a result

    (* getopt(optstring s, args) behaves more like getopt(3).
     * s contains a list of legal option characters.
     * If a character c is followed by a colon then every occurrence of
     * -c in args must be followed by an argument and will be converted
     * by getopt to (c, SOME arg).
     * Otherwise (no colon) every occurrence of -c in args will be
     * converted to (c, NONE).
     *)
    val optstring : string -> (char * string option) opt list
	
end
