(*$import RTL CORE Labelmap Regmap Regset String Rtl Util Char *)

structure Core :> CORE =
struct
      
    val error = fn s => Util.error "core.sml" s
    open Rtl

    structure Rtl = Rtl
    datatype register = R of int | F of int
    
    datatype operand = REGop of register
	               | IMMop of int

    datatype stacklocation =
	  CALLER_FRAME_ARG of int
	| THIS_FRAME_ARG of int
	| SPILLED_INT of int
	| SPILLED_FP of int
	| ACTUAL4 of int
	| ACTUAL8 of int
	| RETADD_POS
      
   (* Where a given pseudoregister is physically located *)
   datatype assign = IN_REG of register
                   | ON_STACK of stacklocation
                   | HINT of register
		   | UNKNOWN

    type label = Rtl.label
    type data = Rtl.data
    type align = Rtl.align

    structure Labelmap = Labelmap()
    structure Regmap = Regmap(structure Machine = struct datatype register = datatype register end)
    structure Regset = Regset(structure Machine = struct datatype register = datatype register end)

    fun isFloat (F _) = true
      | isFloat _ = false

    fun isInt (R _) = true
      | isInt _ = false

    structure W = TilWord32
    val i2w = W.fromInt
    val w2i = W.toInt
    fun ms n = if n<0 then ("-"^(Int.toString (~n))) else Int.toString n

  fun msReg (R n) = "$" ^ (ms n)
    | msReg (F n) = "$f" ^ (ms n)
      
  val makeAsmLabel =
    let 
      fun loop [] = ""
        | loop (#"'" :: rest) = "PRIME" ^ (loop rest)
        | loop (#"!" :: rest) = "BANG" ^ (loop rest)
        | loop (#"%" :: rest) = "PERCENT" ^ (loop rest)
        | loop (#"&" :: rest) = "AND" ^ (loop rest)
        | loop (#"$" :: rest) = "DOLLAR" ^ (loop rest)
        | loop (#"#" :: rest) = "HASH" ^ (loop rest)
        | loop (#"+" :: rest) = "PLUS" ^ (loop rest)
        | loop (#"-" :: rest) = "MINUS" ^ (loop rest)
        | loop (#"/" :: rest) = "SLASH" ^ (loop rest)
        | loop (#":" :: rest) = "COLON" ^ (loop rest)
        | loop (#"<" :: rest) = "LT" ^ (loop rest)
        | loop (#"=" :: rest) = "EQ" ^ (loop rest)
        | loop (#">" :: rest) = "GT" ^ (loop rest)
        | loop (#"?" :: rest) = "QUEST" ^ (loop rest)
        | loop (#"@" :: rest) = "AT" ^ (loop rest)
        | loop (#"\\" :: rest) = "BACKSLASH" ^ (loop rest)
        | loop (#"~" :: rest) = "TILDE" ^ (loop rest)
        | loop (#"`" :: rest) = "ANTIQUOTE" ^ (loop rest)
        | loop (#"^" :: rest) = "HAT" ^ (loop rest)
        | loop (#"|" :: rest) = "BAR" ^ (loop rest)
        | loop (#"*" :: rest) = "STAR" ^ (loop rest)
        | loop (s :: rest) = (String.str s) ^ (loop rest)
    in
      loop o explode
    end


  fun ms n = if n<0 then ("-"^(Int.toString (~n))) else Int.toString n
  fun msStackLocation (CALLER_FRAME_ARG i) = "CallerFrameArg["^(ms i)^"]"
    | msStackLocation (THIS_FRAME_ARG i) = "ThisFrameArg["^(ms i)^"]"
    | msStackLocation (SPILLED_INT i) = "Spill-I["^(ms i)^"]"
    | msStackLocation (SPILLED_FP i) = "Spill-F["^(ms i)^"]"
    | msStackLocation (ACTUAL4 i) = "4@" ^ (ms i)
    | msStackLocation (ACTUAL8 i) = "8@" ^ (ms i)
    | msStackLocation (RETADD_POS) = "RETADD_POS"

  fun sloc2int (ACTUAL4 x) = x
    | sloc2int (ACTUAL8 x) = x
    | sloc2int arg = error ("decalpha.sml: Attempt to concretize stacklocation: " ^ 
			    (msStackLocation arg))
    
   fun regNum (R n) = n
     | regNum (F n) = n

   fun eqRegs (R n) (R n') = n = n'
     | eqRegs (F n) (F n') = n = n'
     | eqRegs _ _ = false
   fun eqLabs l1 l2 = Rtl.eq_label(l1,l2)
   fun eqAssigns (IN_REG a) (IN_REG b) = eqRegs a b
     | eqAssigns (ON_STACK a) (ON_STACK b) = (a = b)
     | eqAssigns _ _ = false

end
