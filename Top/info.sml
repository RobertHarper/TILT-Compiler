(*$import INFO Util List String TextIO Bool TopHelp Int Date *)

(* XXX: timestamps should be written as dates in UTC.  No Date.fromString, though. *)

structure Info :> INFO =
struct

    (* We write a version number to help future compilers snarf old
     * files.  Its written first to give the file a magic number.
     * When a file is read, the order of the sections is discarded but
     * the order of the entries in a section is available.
     * (Currently, we ignore this ordering information.)  *)
    
    fun error s = Util.error "info.sml" s

    structure Ue = UnitEnvironment
    type info = {unit : string, lastWritten : Time.time, lastChecked : Time.time,
		 constrained : bool, imports : Ue.ue, exports : Ue.ue}

    val fieldChar = #":"
    val sectionChar = #"$"

    val fileSection = "InfoFile"
    val versionKey = "version"
    val versionValue = "1"
	
    val statusSection = "ContextFileStatus"
    val unitKey = "unit"
    val lastWrittenKey = "lastWritten"
    val lastCheckedKey = "lastChecked"
    val constrainedKey = "constrained"
	
    val importsSection = "Imports"
    val exportsSection = "Exports"

    (* We write timestamps as an elapsed time. *)
    local
	val baseDate = Date.date {year=2000, month=Date.Jan, day=1, hour=0,  minute=0, second=0,
				  offset=SOME Time.zeroTime} (* UTC *)
	val baseTime = Date.toTime baseDate (* local *)
    in
	fun timeToString t = Time.toString (Time.- (t, baseTime))
	fun timeFromString s = Option.map (fn t => Time.+ (t, baseTime)) (Time.fromString s)
    end

    (* validUnit : string -> bool
     * Check whether unit name would corrupt our file.  Really, we
     * should have an invariant that unit names are just like ascii
     * SML identifiers.
     *)
    fun validUnit unit = List.all (fn c => (c <> fieldChar andalso
					    c <> sectionChar andalso
					    c <> #"\n")) (String.explode unit)

    (* write : string * info -> unit *)
    fun write (infoFile, info : info) =
	let val stream = TextIO.openOut infoFile
	    fun output_record (s1, s2) = (TextIO.output (stream, s1);
					  TextIO.output1 (stream, fieldChar);
					  TextIO.output (stream, s2);
					  TextIO.output1 (stream, #"\n"))
	    fun output_section s = (TextIO.output1 (stream, sectionChar);
				    TextIO.output (stream, s);
				    TextIO.output1 (stream, #"\n"));
	    fun output_ue_binding (name, crc) =
		if validUnit name then
		    output_record (name, Crc.toString crc)
		else error ("invalid unit name \"" ^ String.toString name ^ "\"")
	    val _ = output_section fileSection
	    val _ = output_record (versionKey, versionValue)
	    val _ = output_section statusSection
	    val _ = app output_record [(unitKey, #unit info),
				       (lastWrittenKey, timeToString (#lastWritten info)),
				       (lastCheckedKey, timeToString (#lastChecked info)),
				       (constrainedKey, Bool.toString (#constrained info))]
	    val _ = output_section importsSection
	    val _ = Ue.appi output_ue_binding (#imports info)
	    val _ = output_section exportsSection
	    val _ = Ue.appi output_ue_binding (#exports info)
	in
	    TextIO.closeOut stream
	end

    datatype line = SECTION of string
                  | RECORD of string * string

    exception Error of int option * string
    
    (* read_lines : string -> line list *)
    fun read_lines file =
	let fun split' (nil, _) = NONE
	      | split' (c::rest, acc) = if c = fieldChar then SOME (rev acc, rest)
					else split' (rest, c::acc)
	    fun split (lineno, s) = case split' (String.explode s, nil)
				      of NONE => raise Error (SOME lineno, "malformed record")
				       | SOME (a,b) => (String.implode a, String.implode b)
	    val stream = TextIO.openIn file
	    fun loop (lineno, acc) =
		case TextIO.inputLine stream
		  of "" => rev acc
		   | s =>
		      let val line = if String.sub(s,0) = sectionChar
					 then SECTION (String.extract (s, 1, SOME (size s - 2)))
				     else RECORD (split (lineno, String.extract (s, 0, SOME (size s - 1))))
		      in  loop (lineno+1, line :: acc)
		      end
	    val lines = loop (1, nil)
	    val _ = TextIO.closeIn stream
	in  lines
	end

    structure StringMap = Help.StringMap

    (* The file is a mapping from section names to pairs of the form
     * (entMap, order) where entMap maps entry names to line numbers
     * and values and order preserves (in reverse) the file order of
     * the entries.
     *)
    type sections = ((int * string) StringMap.map * string list) StringMap.map
	
    (* discover_sections : line list -> sections *)
    fun discover_sections lines : sections =
	let val emptySection = (StringMap.empty, nil)
	    fun addLine (lineno, SECTION sec, (_, secMap)) =
	         if isSome (StringMap.find (secMap, sec))
		     then raise Error (SOME lineno, "duplicate section " ^ sec)
		 else (sec, StringMap.insert (secMap, sec, emptySection))
	      | addLine (lineno, RECORD (name, value), (sec, secMap)) =
	         let val (entMap, order) = case StringMap.find (secMap, sec)
					     of NONE => emptySection
					      | SOME x => x
		     val entMap' = if isSome (StringMap.find (entMap, name))
				       then raise Error (SOME lineno, "duplicate record " ^ name)
				   else StringMap.insert (entMap, name, (lineno,value))
		     val order' = name :: order
		     val secMap' = StringMap.insert (secMap, sec, (entMap', order'))
		 in  (sec, secMap')
		 end
	     fun addLines (lineno, nil, acc) = acc
	       | addLines (lineno, line::lines, acc) = addLines (lineno+1, lines, addLine (lineno, line, acc))
	     val (_, secMap) = addLines (1, lines, ("DummySection", StringMap.empty))
	in  secMap
	end
(*
    fun print_entries (entMap, order) =
	let
	    fun printEntry name =
		let val (lineno, value) = valOf (StringMap.find (entMap, name))
		in  print ("LINE " ^ Int.toString lineno ^ "  NAME <<" ^ name ^ ">>  VALUE <<" ^ value ^ ">>\n")
		end
	in
	    app printEntry (rev order)
	end
    
    fun print_sections secMap = app (fn (name, value) =>
				     (print ("SECTION <<" ^ name ^ ">>\n");
				      print_entries value)) (StringMap.listItemsi secMap)
*)
    (* read : string -> info option *)
    fun read file =
	let val lines = read_lines file
	    val secMap = discover_sections lines
(*	    val _ = print_sections secMap*)
	    fun getSection section = case StringMap.find (secMap, section)
				       of NONE => raise Error (NONE, "missing section " ^ section)
					| SOME x => x
	    val statusSection = #1 (getSection statusSection)
	    fun getStatus f entry = 
		case StringMap.find (statusSection, entry)
		  of NONE => raise Error (NONE, "missing entry " ^ entry)
		   | SOME s => f s
	    fun convert (what, conv) (lineno, s) =
		case conv s
		  of NONE => raise Error (SOME lineno, "malformed " ^ what)
		   | SOME x => x
	    val string = fn (_, s) => s
	    val time = convert ("time value", timeFromString)
	    val bool = convert ("boolean", Bool.fromString)
	    val crc = convert ("CRC", Crc.fromString)
	    fun getUnitEnvironment section =
		let fun folder (name, value, ue) = Ue.insert (ue, name, crc value)
		    val (entMap, _) = getSection section
		in  StringMap.foldli folder Ue.empty entMap
		end
	in
	    {unit = getStatus string unitKey,
	     lastWritten = getStatus time lastWrittenKey,
	     lastChecked = getStatus time lastCheckedKey,
	     constrained = getStatus bool constrainedKey,
	     imports = getUnitEnvironment importsSection,
	     exports = getUnitEnvironment exportsSection}
	end handle Error (SOME lineno, msg) => error (file ^ ":" ^ Int.toString lineno ^ " " ^ msg)
	         | Error (NONE, msg) => error (file ^ ": " ^ msg)

    fun eqTime ab = Time.compare ab = EQUAL
    fun eqBool (a:bool, b) = a = b
    fun eqUe ab = Ue.equal ab
	
    (* equal : info * info -> bool *)
    fun equal (a : info, b : info) =
	let fun check (project, eq) = eq (project a, project b)
	in
	    check (#lastWritten, eqTime) andalso
	    check (#lastChecked, eqTime) andalso
	    check (#constrained, eqBool) andalso
	    check (#imports, eqUe) andalso
	    check (#exports, eqUe)
	end
    
end
