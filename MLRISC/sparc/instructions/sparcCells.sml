(* sparcCells.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
structure SparcCells : SPARCCELLS = struct
  structure SL = SortedList

  type register = int
  type regmap = register Intmap.intmap
  datatype cellclass = GP | FP | CC | Y | PSR | FSR | MEM | CTRL

  exception Cells

  val stackptrR		= 14
  val asmTmpR		= 10 (* %o2 *)
  val fasmTmp		= 30
  val y                 = 64
  val psr               = 65
  val fsr               = 66
  val linkReg           = 15

  val firstPseudo	= 256

  val counter = ref firstPseudo
  val regCnt = ref 0
  val fregCnt = ref 0
  fun bump (r as ref c) = (r := c+1; c)
  fun downto0 0 = [0]
    | downto0 n = n::downto0(n-1)
  val physicalRegs = downto0 31

  fun newReg () = (bump regCnt; bump counter)
  fun newFreg() = (bump fregCnt; bump counter)
  fun newCCreg () = (bump regCnt; bump counter)

  fun resetRegs() = let 
    val regmap = Intmap.new(64, Cells)
    val enter = Intmap.add regmap
  in
    counter:=firstPseudo; 
    regCnt :=0; 
    fregCnt:=0; 
    app (fn r => enter(r,r)) physicalRegs;
    regmap
  end

  fun newCell GP  = newReg
    | newCell FP  = newFreg
    | newCell CC  = newReg
    | newCell _   = (fn () => bump counter)

  fun maxCell () = !counter

  fun numCell GP = (fn () => !regCnt)
    | numCell FP = (fn () => !fregCnt)
    | numCell _  = raise Cells

  fun cellToString(r,GP) = 
       if r < 8 then "%g"^Int.toString r
       else if r = 14 then "%sp"
       else if r < 16 then "%o"^Int.toString(r-8)
       else if r < 24 then "%l"^Int.toString(r-16)
       else if r = 30 then "%fp"
       else if r < 32 then "%i"^Int.toString(r-24)
       else "%r"^Int.toString r
    | cellToString(r,class) = prefix class^Int.toString r
  and prefix GP   = "%r"
    | prefix FP   = "%f"
    | prefix CC   = "cc"
    | prefix Y    = "%y"
    | prefix PSR  = "%psr"
    | prefix FSR  = "%fsr"
    | prefix MEM  = "m"
    | prefix CTRL = "ctrl"

  fun zero GP = SOME 0
    | zero _  = NONE 

  type cellset  = (int list * int list)

  val empty = ([], [])
  fun cellset2string(regs, fregs) = let
    val gp = "gp=" :: map (fn r => (" %r" ^ Int.toString r)) regs
    val fp = " fp=" :: map (fn f => (" %f" ^ Int.toString f)) fregs
  in String.concat(gp @ fp)
  end
  fun addReg(r, (rc,fc)) = (SL.enter(r,rc), fc)
  fun addFreg(f, (rc,fc)) = (rc, SL.enter(f, fc))
  fun addCell GP = addReg
    | addCell FP = addFreg
    | addCell CC = addReg
    | addCell _  = raise Cells

  fun cellsetToRegs(regmap, (regs,fregs)) = let 
    val lookup = Intmap.map regmap 
    fun trans [] = []
      | trans(r::rs) = let
          val r = lookup r handle _ => r
        in  if r = 0 then trans rs else r::trans rs
        end
    fun ftrans r = let 
      val r = lookup r handle _ => r
    in  if r < 32 then r + 32 else r 
    end
  in (map ftrans fregs @ trans regs)
  end
end
