(*$OPERATIONS: *)
signature OPERATIONS = 
  sig
    type cmp

    datatype hard_exn = OKAY | OVERFLOW | DIVIDEZERO
    val get_exn : unit -> hard_exn
    val reset_exn : unit -> unit
    val raise_overflow : unit -> unit
    val raise_dividezero : unit -> unit

    type w32 = Word32.word
    val getsign : w32 -> int
    val long_to_real : w32 * w32 -> real

    val plusop  : (w32 * w32) * (w32 * w32) * bool -> (w32 * w32)
    val minusop : (w32 * w32) * (w32 * w32) * bool -> (w32 * w32)
    val multop  : (w32 * w32) * (w32 * w32) * bool -> (w32 * w32)
    val divop   : (w32 * w32) * (w32 * w32) * bool -> (w32 * w32)


    val andop : (w32 * w32) * (w32 * w32) -> (w32 * w32)
    val orop  : (w32 * w32) * (w32 * w32) -> (w32 * w32)
    val xorop : (w32 * w32) * (w32 * w32) -> (w32 * w32)
    val notop : (w32 * w32)               -> (w32 * w32)

    val sllop : (w32 * w32) * (w32 * w32) -> (w32 * w32)
    val sraop : (w32 * w32) * (w32 * w32) -> (w32 * w32)
    val srlop : (w32 * w32) * (w32 * w32) -> (w32 * w32)

    val str_to_real : string -> real
    val sword_to_real : w32 -> real
    val word_to_real : w32 -> real

    val cmpiu_to_fun : cmp -> (w32 * w32) -> bool
    val cmpis_to_fun : cmp -> (w32 * w32) -> bool
    val cmpf_to_fun : cmp -> (real * real) -> bool

    val bool_to_ireg_val : bool -> (w32 * w32)
    val bool_to_freg_val : bool -> (w32 * w32)
  end;
