structure Arg :> ARG =
struct

    type 'a argument =
	{argc : char,
	 acc : 'a,
	 argf : unit -> string option,
	 eargf : (unit -> string) -> string}

    fun options (F : 'a argument -> 'a)
		(opts : substring, args : string list,
		 acc : 'a) : string list * 'a =
	if Substring.isEmpty opts then arguments F (args,acc)
	else
	    let val argc = Substring.sub (opts,0)
		val opts = Substring.triml 1 opts
		val newopts = ref opts
		val newargs = ref args
		fun argf () : string option =
		    if Substring.isEmpty opts then
			(case args
			   of nil => NONE
			    | s :: args' => (newargs := args'; SOME s))
		    else
			(newopts := Substring.all "";
			 SOME (Substring.string opts))
		fun eargf (default : unit -> string) : string =
		    (case argf()
		       of SOME s => s
			| NONE => default())
		val arg : 'a argument =
		    {argc=argc, acc=acc, argf=argf, eargf=eargf}
		val acc = F arg
	    in  options F (!newopts, !newargs, acc)
	    end

    and arguments (F : 'a argument -> 'a)
		  (args : string list, acc : 'a) : string list * 'a =
	(case args
	   of nil => (args,acc)
	    | (arg :: args') =>
		if (String.size arg < 2 orelse String.sub(arg,0) <> #"-")
		then (args,acc)
		else if (String.size arg = 2 andalso String.sub(arg,1) = #"-")
		     then (args',acc)
		     else options F (Substring.extract (arg,1,NONE), args',acc))

    type arg =
	{argc : char,
	 argf : unit -> string option,
	 eargf : (unit -> string) -> string}

    fun lift (F : arg -> unit) (arg : 'a argument) : unit =
	let val {argc,argf,eargf,...} = arg
	    val arg : arg = {argc=argc, argf=argf, eargf=eargf}
	in  F arg
	end

    fun args (F : arg -> unit) (args : string list) : string list =
	#1(arguments (lift F) (args,()))

end
