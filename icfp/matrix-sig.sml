signature MATRIX =
sig

  type m4 =  real * real * real * real * 
             real * real * real * real * 
             real * real * real * real
             real * real * real * real *
	type v4 =  real * real * real * real
  type v3 =  real * real * real

  val translate: real * real * real -> m4
  val scale : real * real * real -> m4
	val uscale: real -> m4
  val rotx : real -> m4
  val roty : real -> m4
  val rotz : real -> m4

  val apply : m4 * v4 -> v4
  val combine : m4 * m4 -> m4

  val translateM : real * real * real * v4 -> v4
  val scaleM : real * real * real -> v4
  val uScaleM : real * m4 -> m4
  val rotxM : real * m4 -> m4
  val rotyM: real * m4 -> m4
  val rotzM: real * m4 -> m4

	val m4toString: m4 -> string
  val v4toString: v4 -> string
  val v3toString: v3 -> string


end
