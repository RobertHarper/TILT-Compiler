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
	 IntSyn.S.pp_summary o IntSyn.S.blastInSummary,
	 IntSyn.pp_units o IntSyn.blastInUnits,
	 Comm.pp_messages o Comm.blastInMessages,
	 default_reader]

    fun try_reader (path : string, reader : reader) : bool =
	let val is = B.openIn path
	    val contents = (SOME (reader is) handle B.BadMagicNumber _ => NONE)
	    val _ = B.closeIn is
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

    fun usage () : 'a =
	(TextIO.output (TextIO.stdErr, "usage: dump [-c] file ...\n");
	 OS.Process.exit (OS.Process.failure))

    fun main (_ : string, args : string list) : OS.Process.status =
	let val _ = UtilError.Interactive := false
	    type acc = string -> unit
	    fun option ({argc,...} : acc Arg.argument) : acc =
		(case argc
		   of #"c" => crc
		    | _ => usage ())
	    val (files, f) = Arg.arguments option (args,dump)
	in  app f files;
	    OS.Process.success
	end handle e => UtilError.print_and_exit e

end
