signature ANNOTATION_PROPERTIES =
sig

   structure I : INSTRUCTIONS
   
   val annotate    : I.instruction * Annotations.annotation -> I.instruction
   val annotations : I.instruction -> Annotations.annotations

end

(* 
 * $Log$
# Revision 1.1  99/02/17  21:15:34  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:05  pscheng
# *** empty log message ***
#
 *)
