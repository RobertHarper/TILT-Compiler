(*$import Prelude DECALPHA Word32 *)
signature DIVMULT =
  sig
    structure DA : DECALPHA

    val debug : bool ref
    val opt_on : bool ref;

    (* this emulates a MULQ or MULL assuming the argument in the register fits 
     in 32 bits; this is useful when overflowing 32-bits is either desired 
     as in its use by div or when overflow is guaranteed not to occur *)
    val quad_mult_convert    :  DA.register * Word32.word * DA.register -> DA.Machine.instruction list
    val unsigned_div_convert :  DA.register * Word32.word * DA.register -> DA.Machine.instruction list
    val signed_div_convert   :  DA.register * Word32.word * DA.register -> DA.Machine.instruction list
  end;
