signature MLRISC_CONTROL =
sig

    val arch          : string ref             (* machine architecture *)
   
    val mlrisc        : bool ref               (* use the MLRISC optimizer? *)
    val mlrisc_phases : string list ref        (* the optimization phases *)
    val show          : (unit -> unit) ref     (* describe the phases *)
    val debug_stream  : TextIO.outstream ref   (* debugging output goes here *)
    val debug_msg     : bool ref               (* print debug messages? *)
    val verbose       : bool ref               (* verbose? *) 
    val view_IR       : bool ref
    val dump_IR       : bool ref
    val on            : (unit -> unit) ref     (* turns on MLRISC *)
    val off           : (unit -> unit) ref     (* turns off MLRISC *)
    val init          : (unit -> unit) ref     (* setup the system *)
    val int_spills    : int ref
    val int_reloads   : int ref
    val float_spills  : int ref
    val float_reloads : int ref
    val ssa_repair1   : int ref
    val ssa_repair2   : int ref
    val ssa_repair3   : int ref
    val ssa_repair4   : int ref
    val ssa_repair5   : int ref
    val ssa_repair6   : int ref

      (* profiling support *)
    val loop_multiplier : int ref    

      (* scheduling/optimizer flags *)
    val nontrapping_arithmetic    : bool ref
    val load_speculation          : bool ref
    val fill_branch_delay_slot    : bool ref
    val reorder_code_layout       : bool ref
    val scheduling_algorithm      : string ref
    val freq_variance             : int ref

    structure Visual    : MLRISC_VIEW_STYLE
    structure Timing    : MLRISC_TIMING
    structure Profiling : MLRISC_PROFILING
end

structure MLRISC_Control : MLRISC_CONTROL =
struct
   structure Visual    = MLRISC_ViewStyle
   structure Timing    = MLRISC_Timing
   structure Profiling = MLRISC_Profiling

   val arch          = ref "default"
   val mlrisc        = ref false
   val mlrisc_phases = ref [] : string list ref
   val show          = ref (fn () => ()) 
   val debug_stream  = ref TextIO.stdOut
   val debug_msg     = ref false
   val view_IR       = ref false
   val dump_IR       = ref false
   val verbose       = ref false
   val init          = ref (fn () => ())
   val on            = ref (fn () => (!init(); mlrisc := true))
   val off           = ref (fn () => mlrisc := false)
   val int_spills    = ref 0
   val int_reloads   = ref 0
   val float_spills  = ref 0
   val float_reloads = ref 0
   val ssa_repair1   = ref 0
   val ssa_repair2   = ref 0
   val ssa_repair3   = ref 0
   val ssa_repair4   = ref 0
   val ssa_repair5   = ref 0
   val ssa_repair6   = ref 0

   val loop_multiplier        = ref 10
   val nontrapping_arithmetic = ref false
   val load_speculation       = ref false
   val fill_branch_delay_slot = ref false
   val reorder_code_layout    = ref false
   val scheduling_algorithm   = ref "bernstein-rodeh"
   val freq_variance          = ref 100 
end

