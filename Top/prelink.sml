(*$import PRELINK Crc Util UnitEnvironment *)

structure Prelink
    :> PRELINK
        where type ue = UnitEnvironment.ue =
struct

    val error = fn s => Util.error "prelink.sml" s
	
    val doConsistent = Stats.tt "doConsistent"
	
    structure Ue = UnitEnvironment
    type ue = Ue.ue
    type package = {unit : string, imports : ue, exports : ue}

    (* confine : string * ue * ue -> ue *)
    fun confine (unitname, ue1, ue2) =
	(case Ue.confine (ue1, ue2)
	   of Ue.VALID ue3 =>
	       let val printUe = Ue.appi (fn (str,_) => (print str; print "  "))
		   val _ = if not (Ue.isEmpty ue3)
			       then (print "Could not find "; printUe ue3; print " in ";
				     printUe ue2; print "\n\n")
			   else ()
	       in  ue3
	       end
	    | Ue.WITNESS name =>
	       let val crc = valOf (Ue.find (ue1, name))
		   val crc2 = valOf (Ue.find (ue2, name))
		   val msg = "The unit " ^ unitname ^ " builds\n" ^
		       "on a version of " ^ name ^ " with CRC " ^ (Crc.toString crc) ^ "\n" ^
		       " which is inconsistent with the actual version with CRC " ^ (Crc.toString crc2) ^ "\n"
	       in if (!doConsistent)
		      then error ("Link Error: " ^ msg)
		  else (print ("Link Error overridden by doConsistent set to false: " ^ msg);
			Ue.confine' (ue1, ue2))
	       end)

    (* plus_overlap : string * ue * ue -> ue *)
    fun plus_overlap (unitname, ue1, ue2) =
	(case Ue.plus_overlap (ue1, ue2)
	   of Ue.VALID ue3 => ue3
	    | Ue.WITNESS name =>
	       error ("Link Error: The unit object " ^ unitname ^ " builds\n" ^
		      "on a version of " ^ name ^ " which is inconsistent\n" ^
		      "with versions of " ^ name ^ " imported elsewhere."))
	     
    (* plus_no_overlap : string * ue * ue -> ue *)
    fun plus_no_overlap (unitname, ue1, ue2) =
	(case Ue.plus_no_overlap (ue1, ue2)
	   of Ue.VALID ue3 => ue3
	    | Ue.WITNESS name =>
	       error ("Link Error: You are trying to link in the unit " ^ name ^ " more\n" ^
		      "than once. This is not allowed."))

    (* check : package list -> {imports:ue, exports:ue} *)
    fun check packages =
      let
	  fun li (iue0,eue0,[]) = (iue0,eue0)
	    | li (iue0,eue0,{unit,imports=iue,exports=eue}::rest) =
	      let val iue' = confine(unit,iue,eue0)
		  val iue0' = confine(unit,iue0,eue)
		  val iue_next = plus_overlap(unit,iue0',iue') 
		  val eue_next = plus_no_overlap(unit,eue0,eue)
(*
		  val _ = print "------------------------------------------------\n";
		  val _ = (print "iue0: "; Ue.appi (print o #1) iue0; print "\n\n")
		  val _ = (print "eue0: "; Ue.appi (print o #1) eue0; print "\n\n")
		  val _ = (print "iue: "; Ue.appi (print o #1) iue; print "\n\n")
		  val _ = (print "eue: "; Ue.appi (print o #1) eue; print "\n\n")
		  val _ = (print "iue': "; Ue.appi (print o #1) iue'; print "\n\n")
		  val _ = (print "iue0': "; Ue.appi (print o #1) iue0; print "\n\n")
		  val _ = (print "iue_next: "; Ue.appi (print o #1) iue_next; print "\n\n")
		  val _ = (print "eue_next: "; Ue.appi (print o #1) eue_next; print "\n\n")
*)
	      in li (iue_next,eue_next,rest)
	      end
	  val (imports, exports) = li (Ue.empty,Ue.empty,packages)
(*
	  val _ = (print "imports: "; Ue.appi (print o #1) imports; print "\n\n")
*)
      in  {imports = imports, exports = exports}
      end

    (* checkTarget : string * package list -> unit *)
    fun checkTarget (unit, packages) =
	let val {imports, exports} = check packages
		
	    fun pr_units [] = error "pr_units"
	      | pr_units [a] = a
	      | pr_units (a::rest) = (a ^ ", " ^ pr_units rest)
	in
	    if Ue.isEmpty imports then ()
	    else
		(print ("\nError! The units : [" ^ pr_units (map #1 (Ue.listItemsi imports)) ^ 
			"] have not been resolved.\nExports were : [" ^
			pr_units (map #1 (Ue.listItemsi exports)) ^
			"].\n I cannot generate an executable for you.\n");
		 error ("pre-link check failed for " ^ unit))
	end
end
