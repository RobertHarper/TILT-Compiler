(*$import RTL CORE String Rtl Util Char Int32 BinaryMapFn BinarySetFn *)

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


  local 
      open Rtl
      structure Labelkey : ORD_KEY = 
	  struct
	      type ord_key = Rtl.label
	      fun compare (LOCAL_CODE s1, LOCAL_CODE s2) = String.compare(s1,s2)
		| compare (LOCAL_DATA s1, LOCAL_DATA s2) = String.compare(s1,s2)
		| compare (C_EXTERN_LABEL s1, C_EXTERN_LABEL s2) = String.compare(s1,s2)
		| compare (ML_EXTERN_LABEL s1, ML_EXTERN_LABEL s2) = String.compare(s1,s2)
		| compare (LOCAL_CODE _, _) = LESS
		| compare (_, LOCAL_CODE _) = GREATER
		| compare (LOCAL_DATA _, _) = LESS
		| compare (_, LOCAL_DATA _) = GREATER
		| compare (C_EXTERN_LABEL _, _) = LESS
		| compare (_, C_EXTERN_LABEL _) = GREATER
	  end
  in  structure Labelmap = BinaryMapFn(Labelkey)
  end


  local 
      val error = fn s => Util.error "regmap.sml" s
      structure Regkey : ORD_KEY = 
	  struct
	      type ord_key = register
	      fun compare (R v, R v') = Int.compare(v,v')
		| compare (F v, F v') = Int.compare(v,v')
		| compare (R _, F _) = LESS
		| compare (F _, R _) = GREATER
	  end
      structure Regmap = BinaryMapFn(Regkey)
  in
      structure Regmap = 
	struct
	    
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
	  
	  exception Occupant of register
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

  local 
    structure Regkey : ORD_KEY = 
      struct
	type ord_key = register
	fun compare (R v, R v') = Int.compare(v,v')
	  | compare (F v, F v') = Int.compare(v,v')
          | compare (R _, F _) = LESS
          | compare (F _, R _) = GREATER
      end
  in
      structure Regset = BinarySetFn(Regkey)
  end

  datatype call_type = DIRECT of label * register option | INDIRECT of register

  datatype rtl_instruction =
    CALL of 
    {calltype : Rtl.calltype,          (* Is this an extern call to C *)
     func: call_type,                  (* label or temp containing addr. *)
     args : register list,             (* integer, floating temps *)
     results : register list,          (* integer, floating temps *)
     argregs : register list option,   (* actual registers *)
     resregs : register list option,   (*   "         "    *)
     destroys: register list option}   (*   "         "    *)

    | RETURN of {results: register list}       (* formals *)

    | JMP of register * label list
    | HANDLER_ENTRY
    | SAVE_CS of label

   datatype base_instruction = 
      MOVI     of register * register
    | MOVF     of register * register
    | PUSH     of register * stacklocation
    | POP      of register * stacklocation
    | PUSH_RET of stacklocation option
    | POP_RET  of stacklocation option
    | RTL      of rtl_instruction
    | TAILCALL of label
    | BR       of label
    | BSR      of label * register option * {regs_modified : register list, regs_destroyed : register list,
					     args : register list}
    | JSR      of bool * register * int * label list
    | RET      of bool * int
    | GC_CALLSITE of label
    | ILABEL of label
    | ICOMMENT of string
    | LADDR of register * label         (* dest, offset, label *)

   datatype procsig = 
     PROCSIG of {arg_ra_pos : (assign list) option,
		 res_ra_pos : assign list option,
		 allocated  : bool,
		 regs_destroyed  : register list ref,
		 regs_modified  : register list ref,
		 blocklabels: label list,
                 framesize  : int option,
		 ra_offset : int option,
		 callee_saved: register list,
		 args   : register list,  (* formals *)
		 res    : register list}  (* formals *)


end
