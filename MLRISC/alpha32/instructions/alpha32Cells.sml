(* alpha32Cells.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Alpha32Cells : ALPHA32CELLS = struct
  structure S = SortedList

  type register = int
  type regmap = int Intmap.intmap
  datatype cellclass = GP | FP | CC | CR | MEM | CTRL

  exception Cells

  val stackptrR		= 30
  val asmTmpR		= 28
  val fasmTmp		= 30
  val firstPseudo	= 256

  val counter = ref firstPseudo
  val regCnt = ref 0
  val fregCnt = ref 0
  fun bump (r as ref c) = (r := c+1; c)
  fun downto0 0 = [0]
    | downto0 n = n::downto0(n-1)
  val physicalRegs = downto0 31

  fun newReg ()   = (bump regCnt; bump counter)
  fun newFreg()   = (bump fregCnt; bump counter)
  fun newCCreg()  = (bump regCnt; bump counter)
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

  fun newCell GP = newReg
    | newCell FP = newFreg 
    | newCell CC = newReg 
    | newCell _  = fn () => bump counter

  fun maxCell () = !counter
  fun numCell GP = (fn () => !regCnt)
    | numCell FP = (fn () => !fregCnt)
    | numCell _  = raise Cells

  fun cellToString(r,class) = prefix class^Int.toString r
  and prefix GP   = "r"
    | prefix FP   = "f"
    | prefix CC   = "cc"
    | prefix MEM  = "m"
    | prefix CTRL = "ctrl"
  
  fun zero GP = SOME 31
    | zero FP = SOME 31
    | zero _ = NONE

  type cellset = int list * int list	(* (regs * fregs) *)
  fun cellset2string(regs, fregs) = let
    val gp = "gp=" :: map (fn r => (" $" ^ Int.toString r)) regs
    val fp = " fp=" :: map (fn f => (" $f" ^ Int.toString f)) fregs
  in String.concat(gp @ fp)
  end
  val empty = ([], [])
  fun addReg(r, (rc,fc)) = (S.enter(r,rc), fc)
  fun addFreg(f, (rc,fc)) = (rc, S.enter(f, fc))
  fun addCell GP = addReg
    | addCell FP = addFreg
    | addCell CC = addReg
    | addCell _  = raise Cells

  fun cellsetToRegs(regmap, (regs,fregs)) = let
    val lookup = Intmap.map regmap 
    fun trans r = lookup r handle _ => r
    fun ftrans r = let
      val r = lookup r handle _ => r
    in  if r < 32 then r + 32 else r 
    end
  in (map ftrans fregs @ map trans regs)
  end

end


(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:51  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:12  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:43  pscheng
# *** empty log message ***
#
 * Revision 1.4  1998/10/06 14:07:29  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.3  1998/05/25 15:10:46  george
 *   Fixed RCS keywords
 *
 *)
