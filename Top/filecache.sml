functor FileCache(type name
		  type internal
		  val filename : name -> string
		  val reader : name -> internal
		  val writer : string * internal -> unit) :>
    FILECACHE where type internal = internal and type name = name =
struct

  type name = name
  type internal = internal
  val cache = 2  (* Number of ticks before an unused entry is discarded *)
  val error = fn s => Util.error "filecache.sml" s
  datatype stat = ABSENT
                | PRESENT of int * Time.time                            (* file size + mod time *)
                | CRC     of int * Time.time * Crc.crc                  (* + CRC *)
                | CACHED  of int * Time.time * Crc.crc * int * internal (* + ticks + cached result *)

  val stats = ref (Util.StringMap.empty : stat Util.StringMap.map)

  fun set (file : string, stat : stat) : unit =
      stats := (Util.StringMap.insert(!stats,file,stat))
  fun get (file : string) : stat =
      (case Util.StringMap.find(!stats,file) of
	   NONE => let val stat = ABSENT
		       val _ = set (file, stat)
		   in  stat
		   end
	 | SOME stat => stat)

  (* On AFS it is a lot faster to do access() on a non-existent
   * file than to manipulate it and handle errors.
   *)
  fun access (file : string, how : OS.FileSys.access_mode) : bool =
      (OS.FileSys.access(file,[]) andalso
       OS.FileSys.access(file,[how])) handle _ => false

  (* This is the underlying uncached function *)
  fun modTimeSize_raw (file : string) : (int * Time.time) option =
      if access (file, OS.FileSys.A_READ) then
	   let val size = OS.FileSys.fileSize file
	       val time = OS.FileSys.modTime file
	   in   SOME (size,time)
	   end handle _ => NONE
      else NONE

  fun flushAll () : unit = (stats := Util.StringMap.empty)
  fun flushSome (files : string list) : unit =
      let fun remove file = (stats := #1 (Util.StringMap.remove(!stats, file))
			     handle _ => ())
      in  app remove files
      end

  fun remove (file : string) : unit =
      let val _ = flushSome [file]
	  val _ = (if OS.FileSys.access(file,[])
		   then OS.FileSys.remove file
		   else ()) handle _ => ()
      in ()
      end

  fun modTimeSize_cached (file : string) : (int * Time.time) option =
      (case get file of
	   ABSENT => (case modTimeSize_raw file of
			  NONE => NONE
			| SOME st => (set(file, PRESENT st); SOME st))
	 | PRESENT (s,t) => SOME (s,t)
	 | CRC (s,t, _) => SOME (s,t)
	 | CACHED (s, t, _, _, _) => SOME (s,t))

  val exists : string -> bool = isSome o modTimeSize_cached

  fun modTime file = (case modTimeSize_cached file of
			 NONE => error ("modTime on non-existent file " ^ file)
		       | SOME (s,t) => t)
  fun size file = (case modTimeSize_cached file of
		       NONE => error ("size on non-existent file " ^ file)
		     | SOME (s,t) => s)

  (* ----- Compute the latest mod time of a list of existing files --------- *)
    fun lastModTime [] = (NONE, Time.zeroTime)
      | lastModTime (f::fs) =
	let
	    val recur_result as (_,fstime) = lastModTime fs
	    val ftime = modTime f
	in  if (Time.>=(ftime, fstime)) then (SOME f, ftime) else recur_result
	end

  fun tick() : unit =
      let fun mapper (CACHED (s, t, crc, tick, r)) = if (tick <= 1) then PRESENT (s,t)
						     else CACHED(s, t, crc, tick-1, r)
	    | mapper entry = entry
      in  stats := (Util.StringMap.map mapper (!stats))
      end

  fun read (name : name) : internal =
      let val file = filename name
      in  (case (get file)
	     of CACHED (s, t, crc, tick, internal) =>
		 let val stat = CACHED(s, t, crc, cache, internal)
		     val _ = set (file, stat)
		 in  internal
		 end
	      | _ =>
		 (case modTimeSize_cached file
		    of NONE => error ("reading non-existent file " ^ file)
		     | SOME (s,t) =>
			let val crc = Crc.crc_of_file file
			    val internal = reader name
			    val stat = (if cache>0 then
					  CACHED(s, t, crc, cache, internal)
					else CRC (s, t, crc))
		    	    val _ = set(file, stat)
			in  internal
			end))
      end

  fun crc (file : string) : Crc.crc =
      (ignore (modTimeSize_cached file);
       case (get file) of
	   ABSENT => error ("crc of absent file " ^ file)
	 | CACHED (_, _, crc, _, _) => crc
	 | CRC (_, _, crc) => crc
	 | PRESENT (s, t)  => let val crc = Crc.crc_of_file file
				  val stat = CRC (s, t, crc)
				  val _ = set(file, stat)
			      in  crc
			      end)

  fun write (file : string, i : internal) : unit =
      let val _ = writer(file,i)
	  val crc = Crc.crc_of_file file
	  val (s,t) = (case modTimeSize_raw file
			 of SOME r => r
			  | NONE => error ("can't stat written file " ^ file))
	  val stat = if (cache>0)
			 then CACHED(s, t, crc, cache, i)
		     else CRC (s, t, crc)
	  val _ = set(file, stat)
      in  ()
      end

end
