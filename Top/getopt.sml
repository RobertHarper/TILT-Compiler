(*$import Prelude TopLevel GETOPT String Char List *)

structure Getopt :> GETOPT =
struct

    datatype 'a opt =
	Noarg of char * 'a
      | Arg of char * (string -> 'a)

    datatype 'a result =
	Error of string
      | Success of 'a list * string list

    fun fst s = String.sub (s, 0)
    fun snd s = String.sub (s, 1)
    fun tail s = String.extract (s, 1, NONE)
	
    (* optarg : string * string list -> (string * string list) option *)
    fun optarg ("", nil) = NONE
      | optarg ("", arg::args) = SOME (arg, args)
      | optarg (arg, args) = SOME (arg, args)

    exception Fail of string

    type 'a state = {look : char -> 'a opt option,
		     args : string list,
		     options : 'a list}

    (* matchList : 'a state -> 'a state *)
    fun matchList (s as {args=nil,...}) : 'a state = s
      | matchList (s as {look,args=arg::args',options}) =
	let val len = String.size arg
	    val s' = {look=look, args=args', options=options}
	in
	    if len = 0 orelse fst arg <> #"-" then s
	    else if len = 1 then s	(* bare - *)
		 else if snd arg = #"-"
			  then if len = 2 then s' (* bare -- *)
			       else s (* --foo *)
		      else matchOpts (tail arg, s')
	end

    (* matchOpts : string * 'a state -> 'a state *)
    and matchOpts ("", s) = matchList s
      | matchOpts (optstr, s) = matchOpt (fst optstr, tail optstr, s)

    (* matchOpt : char * string * 'a state -> 'a state *)
    and matchOpt (c, remain, {look, args, options}) =
	(case look c
	   of NONE => raise Fail ("illegal option -- " ^ Char.toString c)
	    | SOME (Arg (_, f)) =>
	       (case optarg (remain, args)
		  of NONE => raise Fail ("option requires an argument -- " ^
					 Char.toString c)
		   | SOME (optarg', args') =>
		      matchList {look=look, args=args',
				 options=(f optarg')::options})
	    | SOME (Noarg (_, opt)) =>
	       matchOpts (remain, {look=look, args=args,
				   options=opt::options}))

    (* stateToResult : 'a state -> 'a result *)
    fun stateToResult ({args, options,...} : 'a state) =
	Success (rev options, args)
	
    (* charof : 'a opt -> char *)
    fun charof (Noarg (c, _)) = c
      | charof (Arg (c, _)) = c
	
    (* matches : char -> 'a opt -> bool *)
    fun matches c opt = (c = charof opt)
	
    (* lookup : 'a opt list -> char -> 'a opt option *) 
    fun lookup opts c = List.find (matches c) opts

    (* getopt : 'a opt list * string list -> 'a result *)
    fun getopt (opts, args) =
	let
	    val startState = {look=lookup opts,
			      args=args,
			      options=nil}
	in
	    stateToResult (matchList startState)
	    handle Fail msg => Error msg
	end

    (* optstring : string -> (char * string option) opt list *)
    fun optstring s =
	let
	    fun convert nil acc = rev acc
	      | convert (c :: #":" :: cs) acc =
		    convert cs (Arg (c, fn arg => (c, SOME arg)) :: acc)
	      | convert (c :: cs) acc =
		    convert cs (Noarg (c, (c, NONE)) :: acc)
	in
	    convert (String.explode s) nil
	end
    
end
