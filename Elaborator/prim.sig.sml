signature PRIM = 
  sig

    datatype scon = INT    of Word32.word
                  | UINT   of Word32.word
                  | FLOAT  of string
                  | STRING of string
                  | CHAR   of char
                  | BOOL   of bool

    datatype traptype = INT_TT | REAL_TT | BOTH_TT
    datatype 'Type prim0 = 
      SOFT_VTRAPprim of traptype
    | SOFT_ZTRAPprim of traptype
    | HARD_VTRAPprim of traptype
    | HARD_ZTRAPprim of traptype

    datatype 'Type prim1 =

      (* ref ops *)
        MK_REFprim of {instance : 'Type}
      | DEREFprim of {instance : 'Type}

      (* real operations *)	
      | NEG_FLOATprim
      | ABS_FLOATprim

      (* int operations *)
      | NOT_INTprim 
      | NEG_INTprim 
      | ABS_INTprim

      (* unsigned int operations *)
      | NOT_UINTprim 

      (* conversions amongst floats, ints, uints *)
      | FLOAT2INTprim (* floor *)
      | INT2FLOATprim (* real  *) 
      | INT2UINTprim
      | UINT2INTprim

      (* 1-d and 2-d arrays *)
      | LENGTH1prim of {instance : 'Type}

    datatype 'Type prim2 =

      (* ref operation *)
        EQ_REFprim of {instance : 'Type}
      | SETREFprim of {instance : 'Type}

      (* char operations *)	
      | EQ_CHARprim
      | NEQ_CHARprim

      (* real operations *)	
      | PLUS_FLOATprim
      | MINUS_FLOATprim
      | MUL_FLOATprim
      | DIV_FLOATprim                
      | LESS_FLOATprim
      | GREATER_FLOATprim
      | LESSEQ_FLOATprim
      | GREATEREQ_FLOATprim
      | EQ_FLOATprim
      | NEQ_FLOATprim

      (* int operations *)
      | PLUS_INTprim
      | MINUS_INTprim
      | MUL_INTprim
      | DIV_INTprim
      | MOD_INTprim
      | QUOT_INTprim
      | REM_INTprim
      | LESS_INTprim
      | GREATER_INTprim
      | LESSEQ_INTprim
      | GREATEREQ_INTprim
      | EQ_INTprim
      | NEQ_INTprim
      | LSHIFT_INTprim
      | RSHIFT_INTprim
      | AND_INTprim
      | OR_INTprim

      (* unsigned int operations *)
      | PLUS_UINTprim
      | MINUS_UINTprim
      | MUL_UINTprim 
      | DIV_UINTprim
      | MOD_UINTprim
      | LESS_UINTprim
      | GREATER_UINTprim
      | LESSEQ_UINTprim
      | GREATEREQ_UINTprim
      | EQ_UINTprim
      | NEQ_UINTprim
      | LSHIFT_UINTprim
      | RSHIFT_UINTprim
      | AND_UINTprim
      | OR_UINTprim

      (* array ops *)
      | SUB1prim of {instance : 'Type}
      | ARRAY1prim of {instance : 'Type}

    datatype 'Type prim3 =
	UPDATE1prim of {instance : 'Type}

    datatype 'Type prim =
        PRIM0 of 'Type prim0
      | PRIM1 of 'Type prim1
      | PRIM2 of 'Type prim2
      | PRIM3 of 'Type prim3

    val eq_prim : ('a * 'a -> bool) * 'a prim * 'a prim -> bool

end