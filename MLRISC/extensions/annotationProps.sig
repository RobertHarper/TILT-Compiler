signature ANNOTATION_PROPERTIES =
sig

   structure I : INSTRUCTIONS
   
   val annotate    : I.instruction * Annotations.annotation -> I.instruction
   val annotations : I.instruction -> Annotations.annotations

end
