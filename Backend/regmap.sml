functor Regmap (structure Machine : MACHINE) : ORD_MAP =
struct
  local 
      val error = fn s => Util.error "regmap.sml" s
      open Machine
      structure Regkey : ORD_KEY = 
	  struct
	      type ord_key = Machine.register
	      fun compare (Machine.R v, Machine.R v') = Int.compare(v,v')
		| compare (Machine.F v, Machine.F v') = Int.compare(v,v')
		| compare (Machine.R _, Machine.F _) = LESS
		| compare (Machine.F _, Machine.R _) = GREATER
	  end
      structure Regmap = BinaryMapFn(Regkey)
  in
    open Regmap

    fun find (m,x) = (case Regmap.find(m,x) of
			   SOME value => SOME value
			 | NONE =>
			       ((* print "variable ";
				 print (msReg x);
				 print " not found in regmap find\n"; *)
				NONE))
    fun remove (m,x) = (Regmap.remove(m,x)
			 handle _ =>
			     ((* print "variable ";
			       print(msReg x);
			       print " not found in regmap remove\n"; *)
			      error "regmap_notfound"))

    (* Inverses of a Regmap.dict lookup; given a position, what
       is currently stored in that position. *)

    exception Lookup
    fun lookupInv eqFun pairs key =
      let
	fun loop [] = raise Lookup
          | loop ((x,y) :: ls) =
	    if (eqFun key y) then x else loop ls
      in
	loop pairs
      end

    fun occupant eqFun regmap =
      let
	val assigns = listItems regmap
	fun occupant' pos = (SOME (lookupInv eqFun assigns pos))
	  handle Lookup => NONE
      in
	occupant'
      end

   exception Occupant of Machine.register
   fun simpleOccupant eqFun (regmap, pos) =
     let
       fun match (r,pos') = 
	 if (eqFun pos pos') then
	   raise Occupant r
	 else ()
     in
       Regmap.app match regmap;
       NONE
     end
       handle Occupant r => SOME r
   
  end
end
