functor Prim(structure Util : UTIL) 
  : PRIM = 
  struct

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


    fun eq_tt (INT_TT,INT_TT) = true
      | eq_tt (REAL_TT,REAL_TT) = true
      | eq_tt (BOTH_TT,BOTH_TT) = true
      | eq_tt (_,_) = false

    fun eq_prim0 (eq_type,p1,p2) =
      (case (p1,p2) of
         (SOFT_VTRAPprim tt1, SOFT_VTRAPprim tt2) => eq_tt(tt1,tt2)
       | (SOFT_VTRAPprim tt1, _) => false
       | (SOFT_ZTRAPprim tt1, SOFT_ZTRAPprim tt2) => eq_tt(tt1,tt2)
       | (SOFT_ZTRAPprim tt1, _) => false
       | (HARD_VTRAPprim tt1, HARD_VTRAPprim tt2) => eq_tt(tt1,tt2)
       | (HARD_VTRAPprim tt1, _) => false
       | (HARD_ZTRAPprim tt1, HARD_ZTRAPprim tt2) => eq_tt(tt1,tt2)
       | (HARD_ZTRAPprim tt1, _) => false)

    fun eq_prim1 (eq_type,p1,p2) =
      (case (p1,p2) of

         (MK_REFprim {instance=c1}, MK_REFprim {instance=c2}) => eq_type(c1,c2)
       | (MK_REFprim _, _) => false
       | (DEREFprim {instance=c1}, DEREFprim {instance=c2}) => eq_type(c1,c2)
       | (DEREFprim _, _) => false

       | (NEG_FLOATprim, NEG_FLOATprim) => true
       | (NEG_FLOATprim, _) => false
       | (ABS_FLOATprim, ABS_FLOATprim) => true
       | (ABS_FLOATprim, _) => false

       | (NOT_INTprim , NOT_INTprim ) => true
       | (NOT_INTprim , _) => false
       | (NEG_INTprim , NEG_INTprim ) => true
       | (NEG_INTprim , _) => false
       | (ABS_INTprim, ABS_INTprim) => true
       | (ABS_INTprim, _) => false

       | (NOT_UINTprim , NOT_UINTprim ) => true
       | (NOT_UINTprim , _) => false

       | (FLOAT2INTprim , FLOAT2INTprim ) => true
       | (FLOAT2INTprim , _) => false
       | (INT2FLOATprim , INT2FLOATprim ) => true
       | (INT2FLOATprim , _) => false
       | (INT2UINTprim, INT2UINTprim) => true
       | (INT2UINTprim, _) => false
       | (UINT2INTprim, UINT2INTprim) => true
       | (UINT2INTprim, _) => false

       | (LENGTH1prim {instance=i1}, LENGTH1prim {instance=i2}) => eq_type(i1,i2)
       | (LENGTH1prim _, _) => false)


    fun eq_prim3 (eq_type,p1,p2) =
      (case (p1,p2) of
         (UPDATE1prim {instance=i1}, UPDATE1prim {instance=i2}) => eq_type(i1,i2)
       | (UPDATE1prim _, _) => false)


    fun eq_prim2 (eq_type,p1,p2) =
      (case (p1,p2) of

         (EQ_REFprim {instance=c1}, EQ_REFprim {instance=c2}) => eq_type(c1,c2)
       | (EQ_REFprim _, _) => false
       | (SETREFprim {instance=c1}, SETREFprim {instance=c2}) => eq_type(c1,c2)
       | (SETREFprim _, _) => false
    
       | (EQ_CHARprim, EQ_CHARprim) => true
       | (EQ_CHARprim, _) => false
       | (NEQ_CHARprim, NEQ_CHARprim) => true
       | (NEQ_CHARprim, _) => false

       | (PLUS_FLOATprim, PLUS_FLOATprim) => true
       | (PLUS_FLOATprim, _) => false
       | (MINUS_FLOATprim, MINUS_FLOATprim) => true
       | (MINUS_FLOATprim, _) => false
       | (MUL_FLOATprim, MUL_FLOATprim) => true
       | (MUL_FLOATprim, _) => false
       | (DIV_FLOATprim, DIV_FLOATprim) => true
       | (DIV_FLOATprim, _) => false
       | (LESS_FLOATprim, LESS_FLOATprim) => true
       | (LESS_FLOATprim, _) => false
       | (GREATER_FLOATprim, GREATER_FLOATprim) => true
       | (GREATER_FLOATprim, _) => false
       | (LESSEQ_FLOATprim, LESSEQ_FLOATprim) => true
       | (LESSEQ_FLOATprim, _) => false
       | (GREATEREQ_FLOATprim, GREATEREQ_FLOATprim) => true
       | (GREATEREQ_FLOATprim, _) => false
       | (EQ_FLOATprim, EQ_FLOATprim) => true
       | (EQ_FLOATprim, _) => false
       | (NEQ_FLOATprim, NEQ_FLOATprim) => true
       | (NEQ_FLOATprim, _) => false


       | (PLUS_INTprim, PLUS_INTprim) => true
       | (PLUS_INTprim, _) => false
       | (MINUS_INTprim, MINUS_INTprim) => true
       | (MINUS_INTprim, _) => false
       | (MUL_INTprim, MUL_INTprim) => true
       | (MUL_INTprim, _) => false
       | (DIV_INTprim, DIV_INTprim) => true
       | (DIV_INTprim, _) => false
       | (MOD_INTprim, MOD_INTprim) => true
       | (MOD_INTprim, _) => false
       | (QUOT_INTprim, QUOT_INTprim) => true
       | (QUOT_INTprim, _) => false
       | (REM_INTprim, REM_INTprim) => true
       | (REM_INTprim, _) => false
       | (LESS_INTprim, LESS_INTprim) => true
       | (LESS_INTprim, _) => false
       | (GREATER_INTprim, GREATER_INTprim) => true
       | (GREATER_INTprim, _) => false
       | (LESSEQ_INTprim, LESSEQ_INTprim) => true
       | (LESSEQ_INTprim, _) => false
       | (GREATEREQ_INTprim, GREATEREQ_INTprim) => true
       | (GREATEREQ_INTprim, _) => false
       | (EQ_INTprim, EQ_INTprim) => true
       | (EQ_INTprim, _) => false
       | (NEQ_INTprim, NEQ_INTprim) => true
       | (NEQ_INTprim, _) => false
       | (LSHIFT_INTprim, LSHIFT_INTprim) => true
       | (LSHIFT_INTprim, _) => false
       | (RSHIFT_INTprim, RSHIFT_INTprim) => true
       | (RSHIFT_INTprim, _) => false
       | (AND_INTprim, AND_INTprim) => true
       | (AND_INTprim, _) => false
       | (OR_INTprim, OR_INTprim) => true
       | (OR_INTprim, _) => false

       | (PLUS_UINTprim, PLUS_UINTprim) => true
       | (PLUS_UINTprim, _) => false
       | (MINUS_UINTprim, MINUS_UINTprim) => true
       | (MINUS_UINTprim, _) => false
       | (MUL_UINTprim , MUL_UINTprim ) => true
       | (MUL_UINTprim , _) => false
       | (DIV_UINTprim, DIV_UINTprim) => true
       | (DIV_UINTprim, _) => false
       | (MOD_UINTprim, MOD_UINTprim) => true
       | (MOD_UINTprim, _) => false
       | (LESS_UINTprim, LESS_UINTprim) => true
       | (LESS_UINTprim, _) => false
       | (GREATER_UINTprim, GREATER_UINTprim) => true
       | (GREATER_UINTprim, _) => false
       | (LESSEQ_UINTprim, LESSEQ_UINTprim) => true
       | (LESSEQ_UINTprim, _) => false
       | (GREATEREQ_UINTprim, GREATEREQ_UINTprim) => true
       | (GREATEREQ_UINTprim, _) => false
       | (EQ_UINTprim, EQ_UINTprim) => true
       | (EQ_UINTprim, _) => false
       | (NEQ_UINTprim, NEQ_UINTprim) => true
       | (NEQ_UINTprim, _) => false
       | (LSHIFT_UINTprim, LSHIFT_UINTprim) => true
       | (LSHIFT_UINTprim, _) => false
       | (RSHIFT_UINTprim, RSHIFT_UINTprim) => true
       | (RSHIFT_UINTprim, _) => false
       | (AND_UINTprim, AND_UINTprim) => true
       | (AND_UINTprim, _) => false
       | (OR_UINTprim, OR_UINTprim) => true
       | (OR_UINTprim, _) => false

       | (ARRAY1prim  {instance=i1}, ARRAY1prim  {instance=i2}) => eq_type(i1,i2)
       | (ARRAY1prim  _, _) => false
       | (SUB1prim    {instance=i1}, SUB1prim    {instance=i2}) => eq_type(i1,i2)
       | (SUB1prim    _, _) => false)

    fun eq_prim (eq_type,p1,p2) = 
      (case (p1,p2) of
	 (PRIM0 p0, PRIM0 p0') => eq_prim0(eq_type,p0,p0')
       | (PRIM0 _, _) => false
       | (PRIM1 p1, PRIM1 p1') => eq_prim1(eq_type,p1,p1')
       | (PRIM1 _, _) => false
       | (PRIM2 p2, PRIM2 p2') => eq_prim2(eq_type,p2,p2')
       | (PRIM2 _, _) => false
       | (PRIM3 p3, PRIM3 p3') => eq_prim3(eq_type,p3,p3')
       | (PRIM3 _, _) => false)

  end
