(*
	We cache a file's size and contents, CRC, or both.

	If the size of a file exceeds max_file_size, then its contents
	are not kept in the cache.  When this limit is enforced, we
	say that the file was demoted.  The cache may still hold a CRC
	for such a file.

	The number of entries in the cache can not exceed
	entries_high.  When this limit is exceeded, the oldest entries
	are evicted until the cache has entries_low entries.

	The total size of cached data can not exceed data_high.  When
	this limit is exceeded, the oldest entries are vacated (their
	data is discarded) until the data size is under data_low.

	The parameters in FILECACHE_ARG must satisfy:

		0 <= data_low, max_file_size <= data_high
		0 <= entries_low <= entries_high

	The cache maintains the invariants:

		for every entry, 0 < datum_size(entry) <= max_file_size
		total data size <= data_high
		#entries <= entries_high
*)

signature FILECACHE_ARG =
sig
    type name
    type internal

    val filename : name -> string
    val reader : name -> internal
    val writer : string * internal -> unit

    val max_file_size : int
    val data_high : int
    val data_low : int
    val entries_high : int
    val entries_low : int
end

signature FILECACHE =
sig
    type file = string
    type name
    type internal

    val flush_all : unit -> unit
    val flush_some : file list -> unit
    val exists : file -> bool
    val crc : file -> Crc.crc
    val remove : file -> unit
    val read : name -> internal
    val write : file * internal -> unit
end
