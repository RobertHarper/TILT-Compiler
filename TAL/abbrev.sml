functor AbbrevFn (type identifier
		  type index
		  type result
		  val abbrevs : (identifier * result) list ref
		  val closed : index -> bool
		  val newid : string -> identifier
		  val id2res : identifier -> result
		  structure ISet : ORD_SET where type Key.ord_key = identifier
		  structure Map : ORD_MAP where type Key.ord_key = index)
  :> ABBREV where type identifier = identifier
 	      and type index = index
              and type result = result 
= struct

    type identifier = identifier
    type index = index
    type result = result

    val idset : ISet.set ref = ref ISet.empty
    val seen    : result Map.map ref = ref Map.empty

    fun abbreviate id r = 
      (if ISet.member(!idset,id) then ()
       else (abbrevs := (id,r) :: (!abbrevs);
	     idset := ISet.add (!idset,id)))

    fun find i = Map.find (!seen,i)

    fun share i r = 
      let
	val r = 
	  if closed i then
	    let
	      val nm = newid "?ab"
	      val () = abbreviate nm r
	    in id2res nm
	    end
	  else r
	val () = seen := Map.insert(!seen,i,r)

      in r
      end

    fun reset () = (idset := ISet.empty;abbrevs := [];seen := Map.empty)
    val abbrevs = fn () => rev (!abbrevs)

  end
