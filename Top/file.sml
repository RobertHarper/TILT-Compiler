(*
	Writes to a temporary file and renames to avoid
	corrupt files when the compiler is interrupted.
	Closes streams when exceptions are raised.
	Creates directories as we try to write to them.
*)
structure File :> FILE =
struct

    val error = fn s => Util.error "file.sml" s

    local
	structure StringSet = Util.StringSet

	val known : StringSet.set ref =
	    ref StringSet.empty

	fun insert (dir : string) : unit =
	    known := StringSet.add(!known,dir)

	fun exists (dir : string) : bool =
	    StringSet.member (!known, dir) orelse
	    ((OS.FileSys.isDir dir handle _ => false) andalso
	     (insert dir; true))

	fun mkdir (dir : string) : unit =
	    if exists dir then ()
	    else (OS.FileSys.mkDir dir; insert dir)
    in
	fun flush_dir_cache() = known := StringSet.empty

	fun mkdirs (dir : string) : unit =
	    if exists dir then ()
	    else
		let val {isAbs, vol, arcs} = OS.Path.fromString dir
		    fun path (n : int) : string =
			OS.Path.toString {isAbs=isAbs,vol=vol,
					  arcs=List.take (arcs,n + 1)}
		    val paths = List.tabulate (length arcs, path)
		in  app mkdir paths
		end
    end

    type file = string		(* filename *)

    fun ignore_exn (f : 'a -> 'b) (arg : 'a) : unit =
	((f arg; ()) handle _ => ())

    fun cleanup (f : unit -> 'a, c : unit -> unit) : 'a =
	((f() handle e => (ignore_exn c (); raise e)) before c())

    fun copy (old : file, new : file) : unit =
	let val is = BinIO.openIn old
	in  cleanup (fn () =>
		     let val _ = mkdirs (OS.Path.dir new)
			 val os = BinIO.openOut new
			 fun copy () =
			     let val data = BinIO.input is
			     in  if Word8Vector.length data = 0 then ()
				 else (BinIO.output (os, data); copy())
			     end
		     in  cleanup (copy, fn () => BinIO.closeOut os)
		     end,
		     fn () => BinIO.closeIn is)
	end

    fun write' (writer : file -> unit) (file : file) : unit =
	let val _ =  mkdirs (OS.Path.dir file)
	    val tmp = Paths.tmpFile file
	    val _ = writer tmp
	    val _ = ignore_exn OS.FileSys.remove file
	    val _ = OS.FileSys.rename {old=tmp, new=file}
	in  ()
	end

    fun blastOut (f : Blaster.outstream -> 'a -> unit, x : 'a)
		 (file : file) : unit =
	let val os = Blaster.openOut file
	    fun writer () = f os x
	    fun cleaner () = Blaster.closeOut os
	in  cleanup (writer, cleaner)
	end

    fun write (f : Blaster.outstream -> 'a -> unit) file (x : 'a) : unit =
	write' (blastOut (f,x)) file

    fun read (f : Blaster.instream -> 'a) file : 'a =
	let val is = Blaster.openIn file
	    fun reader () = f is
	    fun cleaner () = Blaster.closeIn is
	in  cleanup (reader, cleaner)
	end

end
