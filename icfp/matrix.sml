structure Matrix :> MATRIX =
struct
  exception notdone
  type m4 =  real * real * real * real * 
             real * real * real * real * 
             real * real * real * real * 
             real * real * real * real
	type v4 =  real * real * real * real
  type v3 =  real * real * real

	val sin = Math.sin
  val cos = Math.cos

	fun translate (rtx, rty, rtz) =
			(1.0, 0.0, 0.0, rtx,
			 0.0, 1.0, 0.0, rty,
			 0.0, 0.0, 1.0, rtz
			 0.0, 0.0, 0.0, 1.0  )

	fun scale (rsx, rsy, rsz) =
			(rsx, 0.0, 0.0, 0.0,
			 0.0, rsy, 0.0, 0.0,
			 0.0, 0.0, rsz, 0.0,
			 0.0, 0.0, 0.0,   1.0)

	fun uscale rs = 
			(rs, 0.0, 0.0, 0.0,
			 0.0, rs, 0.0, 0.0,
			 0.0, 0.0, rs, 0.0,
			 0.0, 0.0, 0.0,  1.0)

	fun rotx angle =
	let
			val t = angle * (180.0 / Math.pi)
      val s = sin(t)
			val c = cos(t)
	in
			(1.0, 0.0, 0.0,  0.0,
			 0.0, c, ~s, 0.0,
			 0.0, s,  c, 0.0,
			 0.0, 0.0,  0.0, 1.0)
	end

	fun roty angle =
	let
			val t = angle * (180.0 / Math.pi)
      val s = sin(t)
			val c = cos(t)
	in
			(c, 0.0, s, 0.0,
			 0.0, 1.0, 0.0, 0.0,
		  ~s, 0.0, c, 0.0,
			 0.0, 0.0, 0.0, 1.0)
	end

  fun rotz angle =
	let
			val t = angle * (180.0 / Math.pi)
      val s = sin(t)
			val c = cos(t)
	in
			(c, ~s, 0.0, 0.0,
			 s,  c, 0.0, 0.0,
			 0.0,  0.0, 1.0, 0.0
			 0.0,  0.0, 0.0, 1.0)
	end

	fun apply((a11, a12, a13, a14,
						 a21, a22, a23, a24,
						 a31, a32, a33, a34,
						 a41, a42, a43, a44), (wx, wy, wz, w)) =
			(a14*w + a11*wx + a12*wy + a13*wz,
			 a24*w + a21*wx + a22*wy + a23*wz,
			 a34*w + a31*wx + a32*wy + a33*wz,
			 a44*w + a41*wx + a42*wy + a43*wz)
			
  fun combine((a11, a12, a13, a14,
						   a21, a22, a23, a24,
						   a31, a32, a33, a34,
						   a41, a42, a43, a44),
							(b11, b12, b13, b14,
						   b21, b22, b23, b24,
						   b31, b32, b33, b34,
						   b41, b42, b43, b44)) =
			raise notdone

  val translateM : real * real * real * v4 -> v4 = raise notdone
  val scaleM : real * real * real -> v4 = raise notdone
  val uScaleM : real * m4 -> m4 = raise notdone
  val rotxM : real * m4 -> m4 = raise notdone
  val rotyM: real * m4 -> m4 = raise notdone
  val rotzM: real * m4 -> m4 = raise notdone

	val m4toString: m4 -> string = raise notdone
  val v4toString: v4 -> string = raise notdone
  val v3toString: v3 -> string = raise notdone

end
