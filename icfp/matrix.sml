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
			 0.0, 0.0, 1.0, rtz,
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
			 0.0,  0.0, 1.0, 0.0,
			 0.0,  0.0, 0.0, 1.0)
	end

	fun apply((a11, a12, a13, a14,
						 a21, a22, a23, a24,
						 a31, a32, a33, a34,
						 a41, a42, a43, a44):m4, (wx, wy, wz, w):v4) =
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
			(a11*b11+a12*b21+a13*b31+a14*b41,  a11*b12+a12*b22+a13*b32+a14*b42,
			 a11*b13+a12*b23+a13*b33+a14*b43,  a11*b14+a12*b24+a13*b34+a14*b44,

			 a21*b11+a22*b21+a23*b31+a24*b41,  a21*b12+a22*b22+a23*b32+a24*b42,
			 a21*b13+a22*b23+a23*b33+a24*b43,  a21*b14+a22*b24+a23*b34+a24*b44,

			 a31*b11+a32*b21+a33*b31+a34*b41,  a31*b12+a32*b22+a33*b32+a34*b42,
			 a31*b13+a32*b23+a33*b33+a34*b43,  a31*b14+a32*b24+a33*b34+a34*b44,

			 a41*b11+a42*b21+a43*b31+a44*b41,  a41*b12+a42*b22+a43*b32+a44*b42,
			 a41*b13+a42*b23+a43*b33+a44*b43,  a41*b14+a42*b24+a43*b34+a44*b44)



  fun translateM _ = raise notdone 
  fun scaleM _ = raise notdone
  fun uScaleM _ = raise notdone
  fun rotxM _ = raise notdone
  fun rotyM _ = raise notdone
  fun rotzM _ = raise notdone

	fun m4toString (a11, a12, a13, a14,
									a21, a22, a23, a24,
									a31, a32, a33, a34,
									a41, a42, a43, a44) = raise notdone
  fun v4toString _ = raise notdone
  fun v3toString _ = raise notdone

end
