(*
	Writes to a temporary file and renames to avoid corrupt files
	when the compiler is interrupted.  Closes streams when
	exceptions are raised.  Creates directories as we try to write
	to them.  Caches some file contents.

	The file cache raises Reject when magic numbers do not match
	up.  This happens, for example, when the user tries to use
	libraries generated by a different version of the compiler.
*)

structure Fs :> FS =
struct

    val error = fn s => Util.error "fs.sml" s
    val reject = Util.reject

    type file = string		(* filename *)
    type set = Name.LabelSet.set
    type pinterface = LinkIl.pinterface

    fun ignore_exn (f : 'a -> 'b) (arg : 'a) : unit =
	((f arg; ()) handle _ => ())

    fun cleanup (f : unit -> 'a, c : unit -> unit) : 'a =
	((f() handle e => (ignore_exn c (); raise e)) before c())

    fun bad (file : string) : 'a =
	reject ("bad magic number in " ^ file)

    fun identity () : file =
	let val host = Platform.hostname()
	    val host =
		(case Util.substring(".cs.cmu.edu",host)
		   of NONE => host
		    | SOME pos => String.substring(host,0,pos))
	    val pid = Int.toString(Word32.toInt(Platform.pid()))
	in  concat[host, ".", pid]
	end
	val identity = Util.memoize identity

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
	    else (ignore_exn OS.FileSys.mkDir dir; insert dir)
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

    fun tmpFile (file : string) : string =
	OS.Path.joinDirFile {dir=OS.Path.dir file, file=("tmp"^identity())}

    fun write' (writer : file -> unit) (file : file) : unit =
	let val _ =  mkdirs (OS.Path.dir file)
	    val tmp = tmpFile file
	    val _ = ignore_exn OS.FileSys.remove tmp
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
	    fun reader () = f is handle Blaster.BadMagicNumber => bad file
	    fun cleaner () = Blaster.closeIn is
	in  cleanup (reader, cleaner)
	end

    datatype name =
	NAME_PINTERFACE of string
    
    fun filename (n : name) : string =
	(case n
	   of NAME_PINTERFACE s => s)

    datatype internal =
	PINTERFACE of pinterface

    fun reader (name:name) : internal =
	(case name
	   of NAME_PINTERFACE file =>
		PINTERFACE (read LinkIl.blastInPinterface file))

    fun writer (file:string, internal:internal) : unit =
	(case internal
	   of PINTERFACE pi => write LinkIl.blastOutPinterface file pi)

    structure Arg =
    struct
	type name = name
	type internal = internal
	val filename = filename
	val reader = reader
	val writer = writer
	(*
		These numbers may need to be tuned.  In one working
		directory,
	
			du -a |
			awk '/TM.*iface/{sum+=$1}; END{print sum}'
	
		showed that the pinterface files for the libraries and
		TILT totalled around 5MB on disk.  There were 1429
		files and 22 over 25KB on disk.
	*)
	val max_file_size = 25 * 1024	(* bytes of disk space *)
	val data_high = 1000 * 1024
	val data_low = 900 * 1024
	val entries_low = 500		(* number of files in store *)
	val entries_high = 700
    end

    structure C = FileCache(Arg)

    fun flush () : unit =
	(C.flush_all();
	 flush_dir_cache())

    val exists = C.exists
    val crc = C.crc
    val remove = C.remove

    fun mismatch (file : string) : 'a =
	error ("file type mismatch with " ^ file)

    fun wrap (f : string -> 'a) : (string -> 'a option) =
	(fn file => SOME (f file) handle _ => NONE)

    fun read_pinterface (file:string) : pinterface =
	(case C.read (NAME_PINTERFACE file)
	   of PINTERFACE p => p
	    (*| _ => mismatch file*))

(*XXX*)
    fun read_pinterface_parm (file:string) : Name.label list =
	let val set = read LinkIl.blastInPinterfaceParm file
	in  Name.LabelSet.listItems set
	end

    val read_pinterface' = wrap read_pinterface

    fun write_pinterface (file:string, pi:pinterface) : unit =
	C.write (file, PINTERFACE pi)

end
