signature MLRISC_TIMING =
sig

   type timing = {usr:Time.time,gc:Time.time,sys:Time.time}

   val copy_propagation : timing ref
   val cluster_to_IR    : timing ref
   val guess            : timing ref
   val IR_to_cluster    : timing ref
   val ssa              : timing ref
   val liveness         : timing ref
   val scheduling       : timing ref
   val ra               : timing ref
   val emit_code        : timing ref

end

structure MLRISC_Timing : MLRISC_TIMING =
struct

   structure T = Time
   type timing = {usr:T.time,gc:T.time,sys:T.time}

   val copy_propagation = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}
   val cluster_to_IR    = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}
   val guess            = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}
   val IR_to_cluster    = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}
   val ssa              = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}
   val liveness         = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}
   val scheduling       = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}
   val ra               = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}
   val emit_code        = ref {usr=T.zeroTime,gc=T.zeroTime,sys=T.zeroTime}

end

(*
 * $Log$
# Revision 1.1  99/02/17  22:33:26  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:01  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
