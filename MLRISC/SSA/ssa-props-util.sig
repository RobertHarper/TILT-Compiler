signature SSA_PROPERTIES_UTIL =
sig

   val hashLabelExp : LabelExp.labexp -> int
   val eqLabelExp   : LabelExp.labexp * LabelExp.labexp -> bool
   val hashLabel    : Label.label -> int
   val eqLabel      : Label.label * Label.label -> bool

end

(* 
 * $Log$
# Revision 1.1  99/02/17  21:15:06  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:29  pscheng
# *** empty log message ***
#
 *)
