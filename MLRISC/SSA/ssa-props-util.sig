signature SSA_PROPERTIES_UTIL =
sig

   val hashLabelExp : LabelExp.labexp -> int
   val eqLabelExp   : LabelExp.labexp * LabelExp.labexp -> bool
   val hashLabel    : Label.label -> int
   val eqLabel      : Label.label * Label.label -> bool

end
