(*
	The implementation is divided into a structure Store that
	applies the limits and maintains LRU information and a functor
	FileCache that uses Store and handles IO.
*)
structure Store :>
sig

    type file = string
    type crc = Crc.crc
    type size = int

    datatype 'a entry =
	CRC of crc * size
      | DATUM of 'a * size
      | BOTH of crc * 'a * size

    val file_size : 'a entry -> int
    val datum_size : 'a entry -> int

    type 'a store

    val mkstore :
	{max_file_size : size,
	 data_high : size, data_low : size,
	 entries_high : int, entries_low : int}  -> 'a store

    val lookup : 'a store * file -> 'a entry option
    val remove : 'a store * file -> unit
    val insert : 'a store * file * 'a entry -> unit

end =
struct

    val error = fn s => Util.error "filecache.sml" s

    val Vacated = Stats.counter "Cache::data bytes vacated"
    val Demoted = Stats.counter "Cache::data bytes demoted"
    val DataEvicted = Stats.counter "Cache::data bytes evicted"
    val CRCEvicted = Stats.counter "Cache::CRC bytes evicted"

    val FileCacheDebug = Stats.ff"FileCacheDebug"
    fun debugdo t = if (!FileCacheDebug) then (t();()) else ()

    structure HashKey :> HASH_KEY where type hash_key = string =
    struct
	type hash_key = string
	val hashVal : hash_key -> word = HashString.hashString
	val sameKey : hash_key * hash_key -> bool = (op=)
    end

    structure H :> MONO_HASH_TABLE where Key = HashKey =
	HashTableFn(HashKey)

    type file = string
    type crc = Crc.crc
    type time = int
    type size = int

    datatype 'a entry =
	CRC of crc * size
      | DATUM of 'a * size
      | BOTH of crc * 'a * size

    type 'a entries = (file * (time * 'a entry)) list

    type 'a store =
	{table : (time * 'a entry) H.hash_table,
	 size : size ref,
	 timer : time ref,
	 max_file_size : size,
	 data_high : size, data_low : size,
	 entries_high : int, entries_low : int}

    fun time (e : (time * 'a entry)) : time = #1 e

    fun file_size (e : 'a entry) : int =
	(case e
	   of CRC (_,size) => size
	    | DATUM (_,size) => size
	    | BOTH (_,_,size) => size)

    fun datum_size (e : 'a entry) : int =
	(case e
	   of CRC _ => 0
	    | _ => file_size e)

    exception Hash

    fun mkstore {max_file_size : size,
		 data_high : size, data_low : size,
		 entries_high : int, entries_low : int} : 'a store =
	{table=H.mkTable (128,Hash),
	 size=ref 0,
	 timer=ref 0,
	 max_file_size=max_file_size, data_high=data_high,
	 data_low=data_low, entries_high=entries_high,
	 entries_low=entries_low}

    val cinc : Stats.counter * int -> unit = Stats.counter_add
    fun inc (r : int ref, i : int) = r := !r + i
    fun dec (r : int ref, i : int) = inc(r, ~i)

    fun note (what:string, file:file) : unit =
	debugdo (fn () => print (what ^ " " ^ file ^ "\n"))

    (*
	List store entries, oldest first.
    *)
    fun entries (store:'a store) : 'a entries =
	let val {table,...} = store
	    val entries : 'a entries = H.listItemsi table
	    fun newer ((_,a), (_,b)) = time a > time b
	in  ListMergeSort.sort newer entries
	end

    fun now (store:'a store) : time =
	let val {timer,...} = store
	    val t = !timer
	    val _ = inc(timer,1)
	in  t
	end

    (*
	Simple lookup.
    *)
    fun look (store:'a store, file:file) : 'a entry option =
	(case H.find (#table store) file
	   of NONE => NONE
	    | SOME (_,e) => SOME e)

    fun lookup (store:'a store, file:file) : 'a entry option =
	let val {table,...} = store
	in  (case H.find table file
	       of NONE => NONE
		| SOME (_,e) =>
		    let val _ = H.insert table (file, (now store,e))
		    in	SOME e
		    end)
	end

    (*
	Remove and maintain invariants.
    *)
    fun remove (store:'a store, file:file) : unit =
	(case look(store,file)
	   of NONE => ()
	    | SOME entry =>
		let val {table,size,...} = store
		    val _ = H.remove table file
		    val _ = dec(size,datum_size entry)
		in  ()
		end)

    (*
	Eviction is involuntary removal.
    *)
    fun evict (store:'a store, file:file, entry:'a entry) : unit =
	let val _ = note ("evicting",file)
	    val (datasize,crcsize) =
		(case entry
		   of CRC (_,size) => (0,size)
		    | DATUM (_,size) => (size,0)
		    | BOTH (_,_,size) => (size,size))
	    val _ = cinc(CRCEvicted,crcsize)
	    val _ = cinc(DataEvicted,datasize)
	    val _ = remove(store,file)
	in  ()
	end

    (*
	Vacate data from the store until the store size is at or below
	limit.
    *)
    fun vacate (store:'a store, limit:int) : unit =
	let val {size,table,...} = store
	    fun vacate_entry (file:file, (time:time,entry:'a entry)) : unit =
		(case entry
		   of CRC _ => ()
		    | DATUM _ => evict (store,file,entry)
		    | BOTH (c,_,s) =>
			let val _ = note ("vacating",file)
			    val entry = CRC(c,s)
			    val _ = H.insert table (file,(time,entry))
			    val _ = cinc(Vacated,s)
			    val _ = dec(size,s)
			in  ()
			end)
	    fun loop entries =
		if !size > limit then
		    (case entries
		       of nil => error "zero entries, nonzero size"
			| e :: es => (vacate_entry e; loop es))
		else ()
	    val entries = entries store
	in  loop entries
	end

    (*
	How many entries do we drop to accomodate file?
    *)
    fun avail (store:'a store, file:file) : int =
	(case look(store,file)
	   of NONE =>
		let val n = H.numItems (#table store) + 1
		    val h = #entries_high store
		    val l = #entries_low store
		in  if n > h then n - l else 0
		end
	    | SOME _ => 0)

    (*
	Insert and maintain invariants.
    *)
    fun insert (store:'a store, file:file, entry:'a entry) : unit =
	if datum_size entry > #max_file_size store then
	    let val _ = note ("demoting",file)
		val _ =
		    (case entry
		       of CRC _ => error "datum_size > 0 in CRC"
			| DATUM _ => remove(store,file)
			| BOTH (crc,_,s) => insert(store,file,CRC(crc,s)))
		val _ = cinc (Demoted,datum_size entry)
	    in  ()
	    end
	else
	    let val n = avail(store,file)
		val _ =
		    if n > 0 then
			let val entries = entries store
			    val entries = List.take (entries, n)
			in  app (fn (f,(_,e)) => evict(store,f,e)) entries
			end
		    else ()
		val delta =
		    (case look(store,file)
		       of NONE => datum_size entry
			| SOME old => datum_size entry - datum_size old)
		val {table,size,data_high,data_low,...} = store
		val _ =
		    if !size + delta > data_high then
			vacate (store, Int.max(0,data_low - delta))
		    else ()
		val _ = H.insert table (file,(now store,entry))
		val _ = inc(size,delta)
	    in  ()
	    end

end

functor FileCache (Arg : FILECACHE_ARG)
    :> FILECACHE
	where type name = Arg.name
	where type internal = Arg.internal =
struct

    val error = fn s => Util.error "filecache.sml" s

    val CRC = Stats.counter "Cache::bytes CRCed"
    val CacheCRC = Stats.counter "Cache::bytes CRCed from cache"
    val Read = Stats.counter "Cache::bytes read"
    val CacheRead = Stats.counter "Cache::bytes read from cache"
    val Write = Stats.counter "Cache::bytes written"

    type file = string
    type crc = Crc.crc
    type name = Arg.name
    type internal = Arg.internal

    type entry = internal Store.entry

    fun mkstore () : internal Store.store =
	Store.mkstore
	    {max_file_size = Arg.max_file_size,
	     data_high = Arg.data_high,
	     data_low = Arg.data_low,
	     entries_low = Arg.entries_low,
	     entries_high = Arg.entries_high}

    val store : internal Store.store ref = ref (mkstore())

    fun file_access (file:file) : bool =
	OS.FileSys.access(file,nil) handle _ => false

    fun file_size (file:file) : int =
	OS.FileSys.fileSize file

    val cinc : Stats.counter * int -> unit = Stats.counter_add

    fun flush_all () : unit = store := mkstore()

    fun flush_some (files : file list) : unit =
	app (fn file => Store.remove (!store, file)) files

    fun exists (file:file) : bool =
	(case Store.lookup (!store,file)
	   of NONE => file_access file
	    | SOME _ => true)

    fun uncached_crc (file:string, crc:crc, size:int, entry:entry) : crc =
	let val _ = cinc(CRC,size)
	    val _ = Store.insert (!store,file,entry)
	in  crc
	end

    fun cached_crc (crc:crc,size:int) : crc =
	let val _ = cinc(CRC,size)
	    val _ = cinc(CacheCRC,size)
	in  crc
	end

    fun crc (file:file) : crc =
	(case Store.lookup (!store,file)
	   of NONE =>
		let val size = file_size file
		    val crc = Crc.crc_of_file file
		    val e = Store.CRC(crc,size)
		in  uncached_crc(file,crc,size,e)
		end
	    | SOME e =>
		(case e
		   of Store.CRC (crc,size) => cached_crc (crc,size)
		    | Store.DATUM (datum,size) =>
			let val crc = Crc.crc_of_file file
			    val e = Store.BOTH (crc,datum,size)
			in  uncached_crc(file,crc,size,e)
			end
		    | Store.BOTH (crc,_,size) => cached_crc(crc,size)))

    fun remove (file:file) : unit =
	if exists file
	then (Store.remove (!store,file);
	      OS.FileSys.remove file handle _ => ())
	else ()

    fun uncached_read (file:file, i:internal, size:int, entry:entry) : internal =
	let val _ = cinc(Read,size)
	    val _ = Store.insert (!store,file,entry)
	in  i
	end

    fun cached_read (i:internal, size:int) : internal =
	let val _ = cinc(Read,size)
	    val _ = cinc(CacheRead,size)
	in  i
	end

    fun read (name:name) : internal =
	let val file = Arg.filename name
	in  (case Store.lookup (!store,file)
	       of NONE =>
		    let val size = file_size file
			val i = Arg.reader name
			val e = Store.DATUM(i,size)
		    in	uncached_read(file,i,size,e)
		    end
		| SOME e =>
		    (case e
		       of Store.CRC (crc,size) =>
			    let val i = Arg.reader name
				val e = Store.BOTH (crc,i,size)
			    in	uncached_read(file,i,size,e)
			    end
			| Store.DATUM (datum,size) => cached_read (datum,size)
			| Store.BOTH (_,datum,size) => cached_read (datum,size)))
	end

    fun write (file:file, i:internal) : unit =
	let val _ = Arg.writer (file,i)
	    val size = file_size file
	    val _ = cinc(Write,size)
	    val _ = Store.insert (!store, file, Store.DATUM (i,size))
	in  ()
	end

end
