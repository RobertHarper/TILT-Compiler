(*$import MAKEDEP Util Time TextIO Date OS Char *)

structure Makedep :> MAKEDEP = struct

  type unitName = string

  datatype file = SML of {name:unitName, path:string, imports:unitName list, time:Time.time} 
  | INT of {name:unitName, path:string, includes:unitName list, time:Time.time} 
  | UO of {name:unitName, path:string, time:Time.time} 
  | UI of {name:unitName, path:string, time:Time.time} 

  type dependency = file * file list

  type project = unitName * file list * dependency list

  exception FileNotFound

  val error = ref false

  fun  errMsg s = Util.error "Makedep" s

  fun isWhiteSpace istream = 
    case TextIO.lookahead istream of
      SOME c => Char.isSpace c
    | _ => false

  fun skipWhiteSpace istream =
    if isWhiteSpace istream then (skipWhiteSpace (TextIO.input1 istream; istream)) else istream

  exception BadFile

  fun match istream s = let
    val len = String.size s in
      case TextIO.canInput(istream, len) of 
	SOME n => if n = len then let
	  val next = TextIO.inputN(istream, len)
	in
	  if next = s then () else raise BadFile
	end else raise BadFile
    | NONE => raise BadFile
  end

  fun getString istream = 
    if (not (isWhiteSpace istream)) then 
      case TextIO.lookahead istream of
	SOME #"*" => []
      | SOME c =>  c :: (TextIO.input1 istream; getString istream)
      | NONE => []
    else
      []

  fun getStringList istream = let
    val istream = skipWhiteSpace istream
    val s = String.implode(getString istream)
  in
    if s = "" then [] else s :: (getStringList istream)
  end

    
  fun getImports name = let
    val file = TextIO.openIn name
    val _ = skipWhiteSpace file
    val s = (match file "(*$import";
	     getStringList file) handle BadFile => (
	     errMsg ("WARNING: Missing (*$import ...*) in file " ^ name ^ "\n"); [])
  in
    if s = [] then [] else let
      val _ = skipWhiteSpace file
      val s = (match file "*)"; s) handle BadFile => (error := true;
	       errMsg ("ERROR: Ill-formed (*$import ...*) in file " ^ name ^ "\n"); [])
      val _ = TextIO.closeIn file
    in 
      s
    end
  end

  fun getIncludes name = let
    val file = TextIO.openIn name
    val _ = skipWhiteSpace file
    val s = (match file "(*$include";
	     getStringList file) handle BadFile => (error := true;
             errMsg ("ERROR: Ill-formed (*$import ...*) in file " ^ name ^ "\n"); [])
    val _ = skipWhiteSpace file
    val s = (match file "*)"; s) handle BadFile => (error := true;
	     errMsg ("ERROR: Ill-formed (*$import ...*) in file " ^ name ^ "\n"); [])
    val _ = TextIO.closeIn file
  in 
    s
  end

  fun fileType name path = let
    val name = OS.Path.base name
    val suffix = case OS.Path.ext name of SOME s => s | NONE => ""
    val time = OS.FileSys.modTime path
  in case suffix of 
    "sml" => SOME(SML({name = name, path = path, imports = (getImports path), time = time}))
  | "int" => SOME(INT({name = name, path = path, includes = (getIncludes path), time = time}))
  | "ui" => SOME(UI({name = name, path = path, time = time}))
  | "uo" => SOME(UO({name = name, path = path, time = time}))
  | _ => NONE
  end

  fun getFileInfo name = let
    val path = OS.FileSys.fullPath name 
  in
    if (OS.FileSys.isDir path) then NONE 
    else if (OS.FileSys.isLink path) then getFileInfo (OS.FileSys.readLink path) 
	 else fileType name path
  end

  fun fileName (SML{name=n1, ...}) = n1
    | fileName (INT{name=n1, ...}) = n1
    | fileName (UI{name=n1, ...}) = n1
    | fileName (UO{name=n1, ...}) = n1

  fun eqFile (SML{name=n1, ...}) (SML{name=n2, ...}) = n1 = n2
    | eqFile (INT{name=n1, ...}) (INT{name=n2, ...}) = n1 = n2
    | eqFile (UI{name=n1, ...}) (UI{name=n2, ...}) = n1 = n2
    | eqFile (UO{name=n1, ...}) (UO{name=n2, ...}) = n1 = n2
    | eqFile _ _ = false

  fun inProject(file, files) = let
    fun member file [] = false
      | member file (f::files) = (eqFile file f) orelse member file files
  in
    member file files
  end

  fun getFile (file, files) = let
    fun getIt [] = raise BadFile
      | getIt (f::files) = if (eqFile file f) then f else getIt files
  in
    getIt files
  end

  fun mkDepList ([], f) = []
    | mkDepList ((name::rest), f) = let
	val ui = UI({name=name, path=("./"^name^".ui"), time = Time.zeroTime})
      in
	if inProject(ui, f) then (getFile(ui, f))::(mkDepList(rest, f)) else
	  ui::(mkDepList(rest, f))
      end

  fun getDependencies f = let
    fun getDependenciesH [] = []
      | getDependenciesH (SML({name=n1, ...})::rest) = let
	  val uo = UO({name=n1, path=("./"^n1^".uo"), time = Time.zeroTime})
	in
	  if inProject(uo, f) then
	    getDependenciesH rest 
	  else getDependenciesH (uo::rest)
	end
      | getDependenciesH (INT({name=n1, ...})::rest) = let
	  val ui = UI({name=n1, path=("./"^n1^".ui"), time = Time.zeroTime})
	in
	  if inProject(ui, f) then
	    getDependenciesH rest
	  else getDependenciesH (ui::rest)
	end
      | getDependenciesH ((uo as UO({name=n1, ...}))::rest) = let
	  val sml = SML({name=n1, path=("./"^n1^".sml"), time = Time.zeroTime, imports = []})
	  val int = INT({name=n1, path=("./"^n1^".sml"), time = Time.zeroTime, includes = []})
	in
	  if inProject(sml, f) then let
	    val SML({name, imports, ...}) = getFile (sml, f)
	  in 
	    (uo, mkDepList(imports, f) @ (if inProject(int, f) then [getFile(int, f)] else [])) ::
	    getDependenciesH rest
	  end 
	  else getDependenciesH rest
	end
      | getDependenciesH ((ui as UI({name=n1, ...}))::rest) = let
	  val int = INT({name=n1, path=("./"^n1^".sml"), time = Time.zeroTime, includes = []})
	in
	  if inProject(int, f) then let
	    val INT({name, includes, ...}) = getFile (int, f)
	  in
	    (ui, mkDepList(includes, f)) :: getDependenciesH rest
	  end 
	  else getDependenciesH rest
	end
  in
    getDependenciesH f
  end

  fun openProject dir = let
    val _ = error := false
    val oldDir = OS.FileSys.getDir()
    val pname = OS.FileSys.fullPath dir
    val _ = OS.FileSys.chDir pname
    val d = OS.FileSys.openDir pname
    fun findFiles d =
      let val name = OS.FileSys.readDir d in
	if name = "" then [] else
	  case getFileInfo(name) of
	    NONE => findFiles d
	  | SOME(info) => info :: (findFiles d)
      end
    val files = findFiles d
    val _ = OS.FileSys.closeDir d
    val _ = OS.FileSys.chDir oldDir
    val dep = getDependencies files
  in
    if (!error) then (errMsg("ERROR: Cannot open project " ^ dir); raise BadFile)
    else (pname, files, dep)
  end

  fun printFiles ((pname, files, _), out) = let 
    fun printFile (SML {name=name, imports=imports, time = time, ...}) =
      (TextIO.output(out, ("  " ^ name ^ " : SML\t" ^ (Date.toString(Date.fromTimeLocal(time))) ^
       "\n    Imports:\n"));
       List.app (fn s => TextIO.output (out, ("      "^s^"\n"))) imports)
      | printFile (INT {name=name, includes=includes, time=time, ...}) =
	(TextIO.output(out, ("  " ^ name ^ " : INT\t" ^ 
		  (Date.toString(Date.fromTimeLocal(time))) ^"\n    Includes:\n"));
	 List.app (fn s => TextIO.output (out, ("      "^s^"\n"))) includes)
      | printFile (UI {name=name, time=time, ...}) = TextIO.output (out, ("  " ^ name ^ " : UI\t" ^ 
		  (Date.toString(Date.fromTimeLocal(time))) ^"\n"))
      | printFile (UO {name=name, time=time, ...}) = TextIO.output (out, ("  " ^ name ^ " : UO\t" ^ 
		  (Date.toString(Date.fromTimeLocal(time))) ^"\n"))
  in
    (TextIO.output (out,  ("Project " ^ pname ^ " contains these files:\n"));
     List.app printFile files;
     TextIO.output (out,  "\n"))
  end

  fun outputDependencies ((n, _, dep), out) = let
    fun say s = TextIO.output(out, s)
    fun printName (SML{name=name, ...}) = say (name^".sml")
      | printName (INT{name=name, ...}) = say (name^".int")
      | printName (UO{name=name, ...}) = say (name^".uo")
      | printName (UI{name=name, ...}) = say (name^".ui")
    fun printOne (file, deps) = (printName file; say ":"; List.app (fn f => (say " "; printName f)) deps;
				 say "\n")
  in
    List.app printOne dep
  end

  fun appendDepsToFile(p, filename) = let
    fun findLine i = 
      if (TextIO.endOfStream i) then ["### DO NOT DELETE THIS LINE\n"]
      else let
	val str = TextIO.inputLine i
	val chars = String.explode str
	val chars = List.filter (fn c => not (Char.isSpace c)) chars
	val s = implode chars
      in if s = "###DONOTDELETETHISLINE" then [str]
	else str::(findLine i)
      end
    val instream = TextIO.openIn filename
    val lines = findLine instream
    val _ = TextIO.closeIn instream
    val out = TextIO.openOut filename
    val _ = List.app (fn s => TextIO.output(out, s)) lines
    val _ = outputDependencies(p, out)
    val _ = TextIO.closeOut out
  in
    ()
  end

  fun copy fromF toF = let
    val instream = TextIO.openIn fromF
    val outstream = TextIO.openOut toF
    val _ = TextIO.output(outstream, TextIO.inputAll(instream))
    val _ = TextIO.closeIn instream
    val _ = TextIO.closeOut outstream
  in
    ()
  end

  fun backup f = copy f (f^".bak")

  fun restore f = copy (f^".bak") f

  fun mkDep filename = let
    val _ = error := false
    val _ = (backup filename) handle _ => (errMsg "ERROR: Makefile not found\n"; error := true)
    val p = (if not (!error) then (openProject (OS.FileSys.getDir()))
	     else ("ERROR", [], [])) handle _ => (error := true; ("ERROR", [], []))
    val _ = (if not (!error) then appendDepsToFile(p, filename) else ()) handle _ => error := true
  in
    if !error then ((restore filename) handle _ => (); errMsg "ERROR: Invalid project, aborting mkDep\n")
    else ()
  end
end
