structure Dump :>
sig
    val dump : string -> unit
    val crc : string -> unit
    include MAIN
end =
struct

    val error = fn s => Util.error "dump.sml" s

    structure B = Blaster
    structure F = Formatter

    val bad_magic : F.format = F.String "bad magic number"

    fun default_reader (is : B.instream) : F.format =
	(case B.checkMagic is
	   of SOME s =>
		if List.all Char.isPrint (String.explode s)
		then F.String ("uknown magic number: " ^ s)
		else bad_magic
	    | NONE => bad_magic)

    type reader = B.instream -> F.format

    val readers : reader list =
	[Ppil.pp_pinterface' o IlBlast.blastInPinterface,
	 Comm.pp_messages o Comm.blastInMessages,
	 Info.pp_info o Info.blastInInfo,
	 UnitEnvironment.pp_ue o UnitEnvironment.blastInUe,
	 default_reader]

    fun try_reader (path : string, reader : reader) : bool =
	let val is = Blaster.openIn path
	    val contents = (SOME (reader is) handle BadMagicNumber => NONE)
	    val _ = Blaster.closeIn is
	in  (case contents
	       of SOME contents =>
		    let val fmt =
			 F.HOVbox [F.String path, F.String ":", F.Break,
				   contents, F.Newline()]
		    in  F.print_fmt fmt;
			true
		    end
		| NONE => false)
	end

    fun dump (path : string) : unit =
	let fun loop nil = error ("default reader did not handle " ^ path)
	      | loop (r :: rs) =
		    if try_reader (path,r) then ()
		    else loop rs
	in  loop readers
	end

    fun crc (path : string) : unit =
	let val crc = Crc.crc_of_file path
	    val fmt =
		F.Hbox [F.String path, F.String ": ", Crc.pp_crc crc,
			F.Newline()]
	in  F.print_fmt fmt
	end

    fun main (_ : string, args : string list) : OS.Process.status =
	let val usage = "dump [-c] file ..."
	    val _ = ExnHandler.Interactive := false
	    val options = [Getopt.Noarg (#"c",())]
	    val _ = 
		(case Getopt.getopt (options,args)
		   of Getopt.Error msg =>
			raise Compiler.Reject (msg ^ "\n" ^ usage)
		    | Getopt.Success (opts,args) =>
			let val f = if null opts then dump else crc
			in  app f args
			end)
	in  OS.Process.success
	end handle e => ExnHandler.printAndExit e

end
