(*$import Rtl TilWord32 *)

signature CORE =
  sig

    type align = Rtl.align
    type label = Rtl.label
    type data = Rtl.data
	
    datatype register = R of int
                      | F of int
	
    datatype stacklocation = CALLER_FRAME_ARG of int
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
    structure Labelmap : ORD_MAP where type Key.ord_key = label
    structure Regmap   : ORD_MAP where type Key.ord_key = register
    structure Regset   : ORD_SET where type Key.ord_key = register
									      
    val eqRegs    : register -> register -> bool
    val eqLabs    : label -> label -> bool
    val eqAssigns : assign -> assign -> bool

    val sloc2int  : stacklocation -> int (* return offset if stacklocation concrete *)
    val isInt     : register -> bool
    val isFloat   : register -> bool
    val regNum    : register -> int

  end


