functor Cache (Arg:CACHEARG) :> CACHE
    where type name = Arg.Name.t
    where type attr = Arg.Attr.t
    where type datum = Arg.datum =
struct

    val error = fn s => Util.error "cache.sml" s

    val CacheMaxFiles = Stats.counter' "CacheMaxFiles"
    val CacheMaxEntries = Stats.counter' "CacheMaxEntries"
    val CacheMaxEntrySize = Stats.counter' "CacheMaxEntrySize"
    val CacheMaxDataSize = Stats.counter' "CacheMaxDataSize"
    val CacheDemotions = Stats.counter "CacheDemotions"
    val CacheEvictions = Stats.counter "CacheEvictions"

    val CacheDebug = Stats.ff"CacheDebug"
    fun debugdo t = if (!CacheDebug) then (t();()) else ()

    structure HashKey =
    struct
	type hash_key = Arg.Name.t
	val hashVal = Arg.Name.hash
	val sameKey = Arg.Name.eq
    end
    structure H = HashTableFn(HashKey)

    type time = int
    type size = int
    type name = Arg.Name.t
    type attr = Arg.Attr.t
    type datum = Arg.datum
    type entry = {name:name, attr:attr, time:time ref, datum:datum, size:size}
    type entries = entry list

    exception Hash

    (*
	Invariant: If (name,entries) in table, then entries is not empty.
    *)
    val table : entries H.hash_table =
	H.mkTable (128,Hash)
    val numentries : int ref =
	ref 0
    val datasize : size ref =
	ref 0
    val timer : time ref =
	ref 0

    fun find' (entries:entries, attr:attr) : (entry * entries) option =
	let fun search (es:entries,acc:entries) : (entry * entries) option =
		(case es of
		    nil => NONE
		|   e::es =>
			if Arg.Attr.eq(#attr e,attr) then
			    SOME (e, List.revAppend(acc,es))
			else search(es,e::acc))
	in  search(entries,nil)
	end

    fun remove (name:name, attr:attr) : unit =
	(case (H.find table name) of
	    SOME entries =>
		(case (find'(entries,attr)) of
		    SOME (entry,others) =>
			let val {size,...} = entry
			    val _ = datasize := !datasize - size
			    val _ = numentries := !numentries - 1
			in  (case others of
				nil => ignore(H.remove table name)
			    |	_ => H.insert table (name,others))
			end
		|   NONE => ())
	|   NONE => ())

    fun evict ({name,attr,...}:entry) : unit =
	(debugdo (fn () => print ("evicting " ^ Arg.Name.tostring name ^ "\n"));
	 Stats.counter_inc CacheEvictions;
	 remove(name,attr))

    fun entries () : entries =
	let val entries = List.concat(H.listItems table)
	    fun time (entry:entry) : time =
		!(#time entry)
	    fun newer (e1:entry, e2:entry) : bool =
		time e1 > time e2
	    val entries = ListMergeSort.sort newer entries
	in  entries
	end

    fun limit_entries () : unit =
	let val size = !numentries
	    val target =
		if size = !Arg.entries_high then
		    Int.max(0,!Arg.entries_low-1)
		else
		    size
	    val drop = size - target
	in  if drop = 0 then ()
	    else
		let val entries = entries()
		    val entries = List.take(entries,drop)
		in  app evict entries
		end
	end

    fun limit_data (size:size) : unit =
	let val limit =
		if !datasize + size > !Arg.data_high then
		    Int.max(0, !Arg.data_low - size)
		else
		    !Arg.data_high
	    fun done () : bool = !datasize <= limit
	    fun loop (es:entry list) : unit =
		if done() then ()
		else
		    (case es of
			nil => error "zero entries, nonzero datasize"
		    |	{size=0,...} :: es' => loop es'
		    |	e :: es' => (evict e; loop es'))
	in  if done() then ()
	    else loop(entries())
	end

    (* assumes attribute not already present *)
    fun insert (e as {name,size,...} : entry) : unit =
	(Stats.counter_max(CacheMaxEntrySize,size);
	 if size < !Arg.datum_high then
	    let val _ = limit_entries()
		val _ = limit_data size
		val newsize = !datasize + size
		val newcount = !numentries + 1
		val _ = datasize := newsize
		val _ = numentries := newcount
		val entries =
		    (case (H.find table name) of
			SOME entries => entries
		    |	NONE => nil)
		val _ = H.insert table (name,e::entries)
		val _ = Stats.counter_max(CacheMaxFiles,H.numItems table)
		val _ = Stats.counter_max(CacheMaxDataSize,newsize)
		val _ = Stats.counter_max(CacheMaxEntries,newcount)
	    in	()
	    end
	  else
	    Stats.counter_inc CacheDemotions)

    fun now () : time =
	let val t = !timer
	    val _ = timer := t + 1
	in  t
	end

    fun flush_all () : unit =
	(H.filter(fn _ => false) table;
	 numentries := 0;
	 datasize := 0;
	 timer := 0)

    fun flush (name:name) : unit =
	(case (H.find table name) of
	    SOME entries =>
		let val size = foldl (fn (e,acc) => #size e + acc) 0 entries
		    val _ = datasize := !datasize - size
		    val _ = numentries := !numentries - (length entries)
		    val _ = ignore(H.remove table name)
		in  ()
		end
	|   NONE => ())

    fun cached (name:name) : bool =
	isSome (H.find table name)

    fun find (entries:entries, attr:attr) : entry option =
	List.find (fn e => Arg.Attr.eq(#attr e,attr)) entries

    fun peek' (name:name, attr:attr) : (datum * size) option =
	(case (H.find table name) of
	    SOME entries =>
		(case (find(entries,attr)) of
		    SOME {datum,size,time,...} =>
			let val _ = time := now()
			in  SOME(datum,size)
			end
		|   NONE => NONE)
	|   NONE => NONE)

    fun get' (name:name, attr:attr) : datum * size =
	(case (peek'(name,attr)) of
	    SOME r => r
	|   NONE =>
		let val r as (datum,size) = Arg.get(name,attr)
		    val time = ref(now())
		    val _ = insert {name=name, attr=attr, time=time,
			    datum=datum, size=size}
		in  r
		end)

    fun put' (name:name, attr:attr, datum:datum) : size =
	let val _ = remove(name,attr)
	    val size = Arg.put(name,attr,datum)
	    val time = ref(now())
	    val _ = insert {name=name, attr=attr, time=time,
		    datum=datum, size=size}
	in  size
	end

    fun peek (arg : name * attr) : datum option =
	Option.map (#1) (peek' arg)

    fun get (arg : name * attr) : datum =
	#1(get' arg)

    fun put (arg : name * attr * datum) : unit =
	ignore(put' arg)

end
