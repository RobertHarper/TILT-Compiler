(*
    Writes to a temporary file and renames to avoid corrupt files
    when the compiler is interrupted.  Closes streams when
    exceptions are raised.  Creates directories as we try to write
    to them.  Caches some file contents.
*)

structure Fs :> FS =
struct

    val error = fn s => Util.error "fs.sml" s
    val reject = Util.reject

    type file = string	(* filename *)
    type set = Name.LabelSet.set
    type pinterface = LinkIl.pinterface
    type units = Name.label list

    fun ignore_exn (f : 'a -> 'b) (arg : 'a) : unit =
	((f arg; ()) handle _ => ())

    fun cleanup (f : unit -> 'a, c : unit -> unit) : 'a =
	let val r = Util.apply(f,())
	    val _ = ignore_exn c ()
	in  r()
	end

    fun identity () : file =
	let val host = Platform.hostname()
	    val host =
		(case (Util.substring(".cs.cmu.edu",host)) of
		    NONE => host
		|   SOME pos => String.substring(host,0,pos))
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
			let val arcs = List.take(arcs, n+1)
			    val p = {isAbs=isAbs,vol=vol,arcs=arcs}
			in  OS.Path.toString p
			end
		    val paths = List.tabulate (length arcs, path)
		in  app mkdir paths
		end
    end

    fun copy (old : file, new : file) : unit =
	let val is = BinIO.openIn old
	in  cleanup (fn () =>
		     let    val _ = mkdirs (OS.Path.dir new)
			 val os = BinIO.openOut new
			 fun copy () =
			     let    val data = BinIO.input is
			     in	 if Word8Vector.length data = 0 then ()
				 else (BinIO.output (os, data); copy())
			     end
		     in	 cleanup (copy, fn () => BinIO.closeOut os)
		     end,
		     fn () => BinIO.closeIn is)
	end

    fun tmpFile (file : string) : string =
	OS.Path.joinDirFile {dir=OS.Path.dir file, file=("tmp"^identity())}

    fun write' (writer : file -> 'a) (file : file) : 'a =
	let val _ = mkdirs (OS.Path.dir file)
	    val tmp = tmpFile file
	    val _ = ignore_exn OS.FileSys.remove tmp
	    val r = writer tmp
	    val _ = ignore_exn OS.FileSys.remove file
	    val _ = OS.FileSys.rename {old=tmp, new=file}
	in  r
	end

    fun blastOut (f : Blaster.outstream -> 'a -> unit, x : 'a) (file : file) : unit =
	let val os = Blaster.openOut file
	    fun writer () = f os x
	    fun cleaner () = Blaster.closeOut os
	in  cleanup (writer, cleaner)
	end

    fun write (f : Blaster.outstream -> 'a -> unit) file (x : 'a) : unit =
	write' (blastOut (f,x)) file

    fun blastOutSized (f : Blaster.outstream -> 'a -> unit, x : 'a) (file : file) : int =
	let val os = Blaster.openOut file
	    fun writer () = (f os x; Blaster.getPosOut os)
	    fun cleaner () = Blaster.closeOut os
	in  cleanup (writer, cleaner)
	end

    fun sized_write (f : Blaster.outstream -> 'a -> unit) file (x : 'a) : int =
	write' (blastOutSized (f,x)) file

    fun read (f : Blaster.instream -> 'a) file : 'a =
	let val is = Blaster.openIn file
	    fun reader () = f is
	    fun cleaner () = Blaster.closeIn is
	in  cleanup (reader, cleaner)
	end

    fun sized_read (f : Blaster.instream -> 'a) : file -> ('a * int) =
	read (fn is => (f is, Blaster.getPosIn is))

    datatype attr =
	ATTR_PI
    |	ATTR_PI_UNITS
    |	ATTR_CRC
    |	ATTR_EXISTS

    datatype datum =
	PI of pinterface
    |	UNITS of units
    |	CRC of Crc.crc * int	(* file size *)
    |	EXISTS of bool

    fun mismatch (file : string) : 'a =
	error ("attribute/value mismatch with " ^ file)

    structure Arg =
    struct
	structure LS = Name.LabelSet

	structure Name =
	struct
	    type t = file
	    val hash : t -> word = HashString.hashString
	    val eq : t * t -> bool = op=
	    val tostring : t -> string = fn x => x
	end

	structure Attr =
	struct
	    type t = attr
	    fun eq (a:attr,b:attr) : bool =
		(case (a,b) of
		    (ATTR_PI,ATTR_PI) => true
		|   (ATTR_PI_UNITS,ATTR_PI_UNITS) => true
		|   (ATTR_CRC,ATTR_CRC) => true
		|   (ATTR_EXISTS,ATTR_EXISTS) => true
		|   _ => false)
	end

	type size = int
	type datum = datum

	(* Zero size keeps entry in cache longer. *)
	val retain : size = 0
	val dont_retain : size = 1

	val CachePiMisses = Stats.counter "CachePiMisses"
	val CachePiParmMisses = Stats.counter "CachePiParmMisses"
	val CacheCrcMisses = Stats.counter "CacheCrcMisses"
	val CacheExistsMisses = Stats.counter "CacheExistsMisses"
	val CachePiBytesDisk = Stats.counter "CachePiBytesDisk"
	val CachePiParmBytesDisk = Stats.counter "CachePiParmBytesDisk"
	val CacheCrcBytesDisk = Stats.counter "CacheCrcBytesDisk"

	fun get (file:file, attr:attr) : (datum*size) =
	    (case attr of
		ATTR_PI =>
		    let val (pi,sz) = sized_read LinkIl.blastInPinterface file
			val _ = Stats.counter_inc CachePiMisses
			val _ = Stats.counter_add(CachePiBytesDisk,sz)
		    in	(PI pi, sz)
		    end
	    |	ATTR_PI_UNITS =>
		    let val (set,sz) = sized_read LinkIl.blastInPinterfaceParm file
			val units = LS.listItems set
			val _ = Stats.counter_inc CachePiParmMisses
			val _ = Stats.counter_add(CachePiParmBytesDisk,sz)
		    in	(UNITS units,sz)
		    end
	    |	ATTR_CRC =>
		    let val crc = Crc.crc_of_file file
			val sz = OS.FileSys.fileSize file
			val _ = Stats.counter_inc CacheCrcMisses
			val _ = Stats.counter_add(CacheCrcBytesDisk,sz)
		    in	(CRC (crc,sz),retain)
		    end
	    |	ATTR_EXISTS =>
		    let val b =
			    (OS.FileSys.access(file,nil)
			     handle _ => false)
			val _ = Stats.counter_inc CacheExistsMisses
		    in	(EXISTS b,retain)
		    end)

	fun put (file:file, attr:attr, datum:datum) : size =
	    (case (attr,datum) of
		(ATTR_PI,PI pi) =>
		    sized_write LinkIl.blastOutPinterface file pi
	    |	(ATTR_PI_UNITS,UNITS units) => dont_retain
	    |	(ATTR_CRC,CRC _) => retain
	    |	(ATTR_EXISTS,EXISTS _) => retain
	    |	_ => mismatch file)

	(*
	    These numbers may need to be tuned.
	    Compiling with -fPrintStats and looking at
	    the Cache* counters should help.
	*)
	(* bytes on disk *)
	val datum_high = Stats.int("datum_high",5 * 1024)
	val data_high = Stats.int("data_high",100 * 1024)
	val data_low = Stats.int("data_low",90 * 1024)
	(* attributes in cache *)
	val entries_high = Stats.int("entries_high",2000)
	val entries_low = Stats.int("entries_low",1500)
    end

    structure C = Cache(Arg)

    fun flush () : unit =
	(C.flush_all();
	 flush_dir_cache())

    fun flush_some (files:file list) : unit =
	app C.flush files

    val CacheExistsInferred = Stats.counter "CacheExistsInferred"
    fun exists (file:file) : bool =
	(case (C.peek(file,ATTR_EXISTS)) of
	    SOME (EXISTS r) => r
	|   SOME _ => mismatch file
	|   NONE =>
		if C.cached file then
		    let val d = EXISTS true
			val _ = C.put(file,ATTR_EXISTS,d)
			val _ = Stats.counter_inc CacheExistsInferred
		    in	true
		    end
		else
		    (case (C.get(file,ATTR_EXISTS)) of
			EXISTS r => r
		    |	_ => mismatch file))
    val exists = Stats.count("CacheExistsCount",exists)

    val CacheCrcBytes = Stats.counter "CacheCrcBytes"
    fun crc (file:string) =
	(case (C.get(file,ATTR_CRC)) of
	    CRC (crc,sz) =>
		(Stats.counter_add(CacheCrcBytes,sz);
		 crc)
	|   _ => mismatch file)
    val crc = Stats.count("CacheCrcCount",crc)

    fun remove (file:string) : unit =
	(C.flush file;
	 C.put (file, ATTR_EXISTS, EXISTS false);
	 ignore_exn OS.FileSys.remove file)

    val CachePiBytes = Stats.counter "CachePiBytes"
    fun read_pinterface (file:file) : pinterface =
	(case (C.get'(file,ATTR_PI)) of
	    (PI pi,sz) =>
		let val _ = Stats.counter_add(CachePiBytes,sz)
		in  pi
		end
	|   _ => mismatch file)
    val read_pinterface = Stats.count("CachePiCount",read_pinterface)

    val CachePiParmInferred = Stats.counter "CachePiParmInferred"
    fun read_pinterface_parm (file:file) : units =
	(case (C.peek(file,ATTR_PI_UNITS)) of
	    SOME (UNITS units) => units
	|   SOME _ => mismatch file
	|   NONE =>
		(case (C.peek(file,ATTR_PI)) of
		    SOME (PI pi) =>
			let val set = LinkIl.parameters pi
			    val units = Name.LabelSet.listItems set
			    val d = UNITS units
			    val _ = C.put(file,ATTR_PI_UNITS,d)
			    val _ = Stats.counter_inc CachePiParmInferred
			in  units
			end
		|   SOME _ => mismatch file
		|   NONE =>
			(case (C.get(file,ATTR_PI_UNITS)) of
			    UNITS units => units
			|   _ => mismatch file)))
    val read_pinterface_parm =
	Stats.count("CachePiUsingCount",read_pinterface_parm)

    fun read_pinterface' (file:file) : pinterface option =
	if exists file then
	    (SOME (read_pinterface file)
	     handle Blaster.BadMagicNumber _ => NONE)
	else NONE

    fun write_pinterface (file:file, pi:pinterface) : unit =
	(C.flush file;
	 C.put (file,ATTR_PI,PI pi))

end
