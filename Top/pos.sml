structure Pos :> POS =
struct

    val error = fn s => Util.error "pos.sml" s

    datatype pos =
	FILE of string
      | LINE of string * int
      | LIMBO

    fun pos (file:string, lineno:int) : pos = LINE (file,lineno)

    val pos' : string -> pos = FILE

    val nopos : pos = LIMBO

    fun valid (pos:pos) : bool =
	(case pos
	   of LIMBO => false
	    | _ => true)

    fun file (pos : pos) : string =
	(case pos
	   of FILE f => f
	    | LINE (f,_) => f
	    | LIMBO => error "position has no file name")

    fun file' (pos:pos) : string option =
	(case pos
	   of FILE f => SOME f
	    | LINE (f,_) => SOME f
	    | LIMBO => NONE)

    fun lineno (pos:pos) : int =
	(case pos
	   of LINE (_,l) => l
	    | _ => error "position has no line number")

    fun lineno' (pos:pos) : int option =
	(case pos
	   of LINE (_,l) => SOME l
	    | _ => NONE)

    fun tostring (pos:pos) : string =
	(case pos
	   of FILE f => f
	    | LINE (f,l) => concat[f,":",Int.toString l]
	    | LIMBO => "(no file)")

    structure B = Blaster

    fun blastOutPos (os:B.outstream) (pos:pos) : unit =
	(case pos
	   of FILE f => (B.blastOutInt os 0; B.blastOutString os f)
	    | LINE (f,l) => (B.blastOutInt os 1; B.blastOutString os f;
			     B.blastOutInt os l)
	    | LIMBO => B.blastOutInt os 2)

    fun blastInPos (is:B.instream) : pos =
	(case B.blastInInt is
	   of 0 => FILE (B.blastInString is)
	    | 1 => LINE (B.blastInString is, B.blastInInt is)
	    | 2 => LIMBO
	    | _ => error "bad pos")

    val (blastOutPos, blastInPos) =
	B.magic (blastOutPos, blastInPos, "pos $Revision$")

    fun pp_pos (pos:pos) : Formatter.format =
	Formatter.String (tostring pos)

end
