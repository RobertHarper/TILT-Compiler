(*
    The cache maintains a list of attribute/datum pairs for each
    abstract name.  An attribute value pair is called an entry.	 The
    size of an entry is the size of its datum.	The cache eviction
    policy is to discard the least recently used entries when the
    cache is full.

    If the size of a datum exceeds datum_high, then it is not kept
    in the cache.

    The number of entries in the cache can not exceed
    entries_high.  When this limit is exceeded, the oldest entries
    are discarded until the cache has entries_low entries.

    The total size of cached data can not exceed data_high.  When
    this limit is exceeded, the oldest entries with nonzero size
    are discarded until the data size is under data_low.

    The parameters in CACHEARG must satisfy:

	0 <= data_low, datum_high <= data_high
	0 <= entries_low <= entries_high

    The cache maintains the invariants:

	for every entry, 0 < size(entry) <= datum_high
	total data size <= data_high
	#entries <= entries_high

    The CACHEARG parameters are refs so that they can be set at the
    command line.  The cache assumes that their values are fixed
    before the cache is used.
*)

signature CACHEARG =
sig
    structure Name :
    sig
	type t
	val hash : t -> word
	val eq : t * t -> bool
	val tostring : t -> string  (* for debugging *)
    end

    structure Attr :
    sig
	type t
	val eq : t * t -> bool
    end

    type size = int
    type datum

    val get : Name.t * Attr.t -> datum * size
    val put : Name.t * Attr.t * datum -> size

    val datum_high : int ref
    val data_high : int ref
    val data_low : int ref
    val entries_high : int ref
    val entries_low : int ref
end

signature CACHE =
sig
    type size = int
    type name
    type attr
    type datum

    val flush_all : unit -> unit
    val flush : name -> unit
    val cached : name -> bool
    val peek' : name * attr -> (datum * size) option
    val get' : name * attr -> datum * size
    val put' : name * attr * datum -> size
    val peek : name * attr -> datum option
    val get : name * attr -> datum
    val put : name * attr * datum -> unit
end
