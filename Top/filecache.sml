(*$import FILECACHE OS List SplayMapFn SplaySetFn Platform Dirs Delay Stats *)

structure StringKey = 
    struct
	type ord_key = string
	val compare = String.compare
    end
structure StringMap = SplayMapFn(StringKey)
structure StringSet = SplaySetFn(StringKey)
(* A set with an ordering maintained by a list *)
structure StringOrderedSet = 
struct
    type set = StringSet.set * string list
    val empty = (StringSet.empty, [])
    fun member (str,(set,_) : set) = StringSet.member(set,str)
    fun cons (str,(set,list) : set) : set = if (StringSet.member(set,str))
				    then (set,list)
				else (StringSet.add(set,str), str::list)
    fun toList ((set,list) : set) = list
end


functor FileCache(type internal
		  val equaler : internal * internal -> bool
		  val reader : string -> internal
		  val writer : string * internal -> unit) :>
    FILECACHE where type internal = internal =
struct

  val backupCtx = Stats.ff "CacheBackupCtx"
  type internal = internal
  val cache = ref 5  (* Number of ticks before an unused entry is discarded *)
  val error = fn s => Util.error "manager.sml" s
  datatype stat = ABSENT 
                | PRESENT of int * Time.time                            (* file size + mod time *)
                | CRC     of int * Time.time * Crc.crc                  (* + CRC *)
                | CACHED  of int * Time.time * Crc.crc * int * internal (* + ticks + cached result *)

  val stats = ref (StringMap.empty : stat StringMap.map)
	    
  fun set (file,stat) = stats := (StringMap.insert(!stats,file,stat))
  fun get file = 
      (case StringMap.find(!stats,file) of
	   NONE => let val stat = ABSENT
		       val _ = set (file, stat)
		   in  stat
		   end
	 | SOME stat => stat)

  (* This is the underlying uncached function *)
  fun modTimeSize_raw file =
      let val exists = 
	  ((OS.FileSys.access(file, [])) andalso
           (OS.FileSys.access(file, [OS.FileSys.A_READ])
	   handle _ => (print ("Warning: OS.FileSys.access on " ^ 
			       file ^ " failed \n"); false)))
      in  (if exists
	      then SOME(OS.FileSys.fileSize file, OS.FileSys.modTime file)
	  else NONE)
	      handle e => (print ("File exists for read access but fileSize of morTime failed on " ^ file ^ "\n");
			   raise e)
      end
	
  (* This two functions totally or partially flushes the cache *)
  fun flushAll() = (stats := StringMap.empty)
  fun flushSome files = 
      let fun remove file = (stats := #1 (StringMap.remove(!stats, file))
			     handle _ => ())
      in  app remove files
      end
	    
  fun modTimeSize_cached file =
      (case get file of
	   ABSENT => (case modTimeSize_raw file of
			  NONE => (set(file,ABSENT); NONE)
			| SOME st => (set(file, PRESENT st); SOME st))
	 | PRESENT (s,t) => SOME (s,t)
	 | CRC (s,t, _) => SOME (s,t)
	 | CACHED (s, t, _, _, _) => SOME (s,t))
	   
  fun exists file = (case modTimeSize_cached file of
			 NONE => false
		       | SOME _ => true)
  fun modTime file = (case modTimeSize_cached file of
			 NONE => error ("modTime on non-existent file " ^ file)
		       | SOME (s,t) => t)
  fun size file = (case modTimeSize_cached file of
		       NONE => error ("size on non-existent file " ^ file)
		     | SOME (s,t) => s)

  (* ----- Compute the latest mod time of a list of exiting files --------- *)
    fun lastModTime [] = (NONE, Time.zeroTime)
      | lastModTime (f::fs) = 
	let
	    val recur_result as (_,fstime) = lastModTime fs 
	    val ftime = modTime f
	in  if (Time.>=(ftime, fstime)) then (SOME f, ftime) else recur_result
	end

  fun tick() =
      let fun mapper (CACHED (s, t, crc, tick, r)) = if (tick <= 1) then PRESENT (s,t)
						     else CACHED(s, t, crc, tick-1, r)
	    | mapper entry = entry
      in  stats := (StringMap.map mapper (!stats))
      end

  fun updateCache (file, newValue) : bool = 
      (case (get file) of
	   CACHED (s, t, crc, tick, result) => 
	       let val stat = CACHED(s, t, crc, tick, newValue)
		   val _ = set(file, stat)
	       in  true
	       end
	 | _ => false)

  fun read file = 
      (case (get file) of
	   CACHED (s, t, crc, tick, result) => 
	       let val stat = CACHED(s, t, crc, Int.min(tick+2,!cache), result)
		   val _ = set (file, stat)
	       in  (true, result)
	       end
	 | _ => let val (s,t) = (case modTimeSize_raw file of
				     SOME st => st
				   | NONE => error ("Reading non-existent file " ^ file))
		    val crc = Crc.crc_of_file file
		    val result = reader file
		    val stat = if (!cache>0) 
				   then CACHED(s, t, crc, 2, result)
			       else CRC (s, t, crc)
		    val _ = set(file, stat)
		in  (false, result)
		end)

  fun crc file = 
      (size file;
       case (get file) of
	   ABSENT => error ("reading absent file " ^ file)
	 | CACHED (_, _, crc, _, _) => crc
	 | CRC (_, _, crc) => crc
	 | PRESENT (s, t)  => let val crc = Crc.crc_of_file file
				  val stat = CRC (s, t, crc)
				  val _ = set(file, stat)
			      in  crc
			      end)

  fun backup file =
      let val backup = file ^ ".BACKUP"
      in
	  (OS.FileSys.remove backup handle _ => ());
	  OS.FileSys.rename {old=file, new=backup}
      end

  fun write (file, result) = 
      let val exists = exists file
	  val same = exists andalso
		      let val (_,oldResult) = read file
		      in  equaler(result, oldResult)
		      end
      in  if same
	      then false
	  else
	      let val _ = if exists andalso (!backupCtx)
			      then backup file
			  else ()
		  val _ = writer(file,result)
		  val crc = Crc.crc_of_file file
		  val SOME (s,t) = modTimeSize_raw file
		  val stat = if (!cache>0) 
				 then CACHED(s, t, crc, 2, result)
			     else CRC (s, t, crc)
		  val _ = set(file, stat)
	      in  true
	      end
      end

end


