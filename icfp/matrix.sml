structure Matrix :> MATRIX =
struct
  exception notdone
  type m4 = 
			(real * real * real * real * 
			 real * real * real * real * 
			 real * real * real * real * 
			 real * real * real * real ) *
			(real * real * real * real * 
			 real * real * real * real * 
			 real * real * real * real * 
			 real * real * real * real )
	type v4 =  real * real * real * real
  type v3 =  real * real * real

	val sin = Math.sin
  val cos = Math.cos
	fun d2r angle = angle * (Math.pi / 180.0)

	val ident =
			((1.0, 0.0, 0.0, 0.0,
				0.0, 1.0, 0.0, 0.0,
				0.0, 0.0, 1.0, 0.0,
				0.0, 0.0, 0.0, 1.0),
			 (1.0, 0.0, 0.0, 0.0,
				0.0, 1.0, 0.0, 0.0,
				0.0, 0.0, 1.0, 0.0,
				0.0, 0.0, 0.0, 1.0))
(*
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
			val t = d2r(angle)
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
			val t = d2r(angle)
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
			val t = d2r(angle)
      val s = sin(t)
			val c = cos(t)
	in
			(c, ~s, 0.0, 0.0,
			 s,  c, 0.0, 0.0,
			 0.0,  0.0, 1.0, 0.0,
			 0.0,  0.0, 0.0, 1.0)
	end
*)
	fun apply(((a11, a12, a13, a14,
							a21, a22, a23, a24,
							a31, a32, a33, a34,
							a41, a42, a43, a44), (_)):m4, (wx, wy, wz, w):v4) =
			(a14*w + a11*wx + a12*wy + a13*wz,
			 a24*w + a21*wx + a22*wy + a23*wz,
			 a34*w + a31*wx + a32*wy + a33*wz,
			 a44*w + a41*wx + a42*wy + a43*wz)


	fun applyV3(((a11, a12, a13, a14,
								a21, a22, a23, a24,
								a31, a32, a33, a34,
								a41, a42, a43, a44), _):m4, (x, y, z):v3):v3 =
			let
					val w = a44 + a41*x + a42*y + a43*z
			in
					((a14 + a11*x + a12*y + a13*z)/w,
					 (a24 + a21*x + a22*y + a23*z)/w,
					 (a34 + a31*x + a32*y + a33*z)/w)
			end

  fun combine(((a11, a12, a13, a14,
								a21, a22, a23, a24,
								a31, a32, a33, a34,
								a41, a42, a43, a44),
							 (i11, i12, i13, i14,
								i21, i22, i23, i24,
								i31, i32, i33, i34,
								i41, i42, i43, i44)):m4,
							((b11, b12, b13, b14,
								b21, b22, b23, b24,
								b31, b32, b33, b34,
								b41, b42, b43, b44),
							 (j11, j12, j13, j14,
								j21, j22, j23, j24,
								j31, j32, j33, j34,
								j41, j42, j43, j44)))	=
							 
			((a11*b11+a12*b21+a13*b31+a14*b41,  a11*b12+a12*b22+a13*b32+a14*b42,
				a11*b13+a12*b23+a13*b33+a14*b43,  a11*b14+a12*b24+a13*b34+a14*b44,
				
				a21*b11+a22*b21+a23*b31+a24*b41,  a21*b12+a22*b22+a23*b32+a24*b42,
				a21*b13+a22*b23+a23*b33+a24*b43,  a21*b14+a22*b24+a23*b34+a24*b44,
				
				a31*b11+a32*b21+a33*b31+a34*b41,  a31*b12+a32*b22+a33*b32+a34*b42,
				a31*b13+a32*b23+a33*b33+a34*b43,  a31*b14+a32*b24+a33*b34+a34*b44,

				a41*b11+a42*b21+a43*b31+a44*b41,  a41*b12+a42*b22+a43*b32+a44*b42,
				a41*b13+a42*b23+a43*b33+a44*b43,  a41*b14+a42*b24+a43*b34+a44*b44),

			 (j11*i11+j12*i21+j13*i31+j14*i41,  j11*i12+j12*i22+j13*i32+j14*i42,
				j11*i13+j12*i23+j13*i33+j14*i43,  j11*i14+j12*i24+j13*i34+j14*i44,
				
				j21*i11+j22*i21+j23*i31+j24*i41,  j21*i12+j22*i22+j23*i32+j24*i42,
				j21*i13+j22*i23+j23*i33+j24*i43,  j21*i14+j22*i24+j23*i34+j24*i44,
				
				j31*i11+j32*i21+j33*i31+j34*i41,  j31*i12+j32*i22+j33*i32+j34*i42,
				j31*i13+j32*i23+j33*i33+j34*i43,  j31*i14+j32*i24+j33*i34+j34*i44,

				j41*i11+j42*i21+j43*i31+j44*i41,  j41*i12+j42*i22+j43*i32+j44*i42,
				j41*i13+j42*i23+j43*i33+j44*i43,  j41*i14+j42*i24+j43*i34+j44*i44))

  fun translateM (rtx, rty, rtz,
									((a11, a12, a13, a14,
										a21, a22, a23, a24,
										a31, a32, a33, a34,
										a41, a42, a43, a44),
									 (i11, i12, i13, i14,
										i21, i22, i23, i24,
										i31, i32, i33, i34,
										i41, i42, i43, i44)):m4) =
			
			((a11+a41*rtx, a12+a42*rtx, a13+a43*rtx, a14+a44*rtx,
				a21+a41*rty, a22+a42*rty, a23+a43*rty, a24+a44*rty,
				a31+a41*rtz, a32+a42*rtz, a33+a43*rtz, a34+a44*rtz,
				a41,         a42,         a43,         a44        ),

			 (i11, i12, i13, i14-i11*rtx-i12*rty-i13*rtz, 
				i21, i22, i23, i24-i21*rtx-i22*rty-i23*rtz,
				i31, i32, i33, i34-i31*rtx-i32*rty-i33*rtz, 
				i41, i42, i43, i44-i41*rtx-i42*rty-i43*rtz))

  fun scaleM (rsx, rsy, rsz,
							((a11, a12, a13, a14,
								a21, a22, a23, a24,
								a31, a32, a33, a34,
								a41, a42, a43, a44),
							 (i11, i12, i13, i14,
								i21, i22, i23, i24,
								i31, i32, i33, i34,
								i41, i42, i43, i44)):m4) =
			((a11*rsx, a12*rsx, a13*rsx, a14*rsx,
				a21*rsy, a22*rsy, a23*rsy, a24*rsy, 
				a31*rsz, a32*rsz, a33*rsz, a34*rsz, 
				a41,     a42,     a43,     a44    ),
			 (i11/rsx, i12/rsy, i13/rsz, i14,
				i21/rsx, i22/rsy, i23/rsz, i24,
				i31/rsx, i32/rsy, i33/rsz, i34,
				i41/rsx, i42/rsy, i43/rsz, i44))


  fun uScaleM (rs, 
							((a11, a12, a13, a14,
								a21, a22, a23, a24,
								a31, a32, a33, a34,
								a41, a42, a43, a44),
							 (i11, i12, i13, i14,
								i21, i22, i23, i24,
								i31, i32, i33, i34,
								i41, i42, i43, i44)):m4) =
			((a11*rs,a12*rs,a13*rs,a14*rs,
				a21*rs,a22*rs,a23*rs,a24*rs,
				a31*rs,a32*rs,a33*rs,a34*rs,
				a41,   a42,   a43,   a44   ),
			 (i11/rs, i12/rs, i13/rs, i14,
				i21/rs, i22/rs, i23/rs, i24,
				i31/rs, i32/rs, i33/rs, i34,
				i41/rs, i42/rs, i43/rs, i44))



  fun rotxM (angle,
						 ((a11, a12, a13, a14,
							 a21, a22, a23, a24,
							 a31, a32, a33, a34,
							 a41, a42, a43, a44),
							(i11, i12, i13, i14,
							 i21, i22, i23, i24,
							 i31, i32, i33, i34,
							 i41, i42, i43, i44)):m4) =
			let
					val t = d2r(angle)
					val s = sin(t)
					val c = cos(t)
			in
					((a11,         a12,         a13,         a14,
						a21*c-a31*s, a22*c-a32*s, a23*c-a33*s, a24*c-a34*s,
						a31*c+a21*s, a32*c+a22*s, a33*c+a23*s, a34*c+a24*s,
						a41,         a42,         a43,         a44        ),
					 (i11, c*i12 - i13*s, c*i13 + i12*s, i14,
						i21, c*i22 - i23*s, c*i23 + i22*s, i24,
						i31, c*i32 - i33*s, c*i33 + i32*s, i34,
						i41, c*i42 - i43*s, c*i43 + i42*s, i44))

			end


  fun rotyM (angle,
						 ((a11, a12, a13, a14,
							 a21, a22, a23, a24,
							 a31, a32, a33, a34,
							 a41, a42, a43, a44),
							(i11, i12, i13, i14,
							 i21, i22, i23, i24,
							 i31, i32, i33, i34,
							 i41, i42, i43, i44)):m4) =
			let
					val t = d2r(angle)
					val s = sin(t)
					val c = cos(t)
			in
					((a11*c+a31*s, a12*c+a32*s, a13*c+a33*s, a14*c+a34*s,
						a21,         a22,         a23,         a24,
						a31*c-a11*s, a32*c-a12*s, a33*c-a13*s, a34*c-a14*s,
						a41,         a42,         a43,         a44        ),
					 (c*i11 + i13*s, i12, c*i13 - i11*s, i14,
						c*i21 + i23*s, i22, c*i23 - i21*s, i24,
						c*i31 + i33*s, i32, c*i33 - i31*s, i34,
						c*i41 + i43*s, i42, c*i43 - i41*s, i44))
			end

  fun rotzM (angle,
						 ((a11, a12, a13, a14,
							 a21, a22, a23, a24,
							 a31, a32, a33, a34,
							 a41, a42, a43, a44),
							(i11, i12, i13, i14,
							 i21, i22, i23, i24,
							 i31, i32, i33, i34,
							 i41, i42, i43, i44)):m4) =
			let
					val t = d2r(angle)
					val s = sin(t)
					val c = cos(t)
			in
					((a11*c-a21*s, a12*c-a22*s, a13*c-a23*s, a14*c-a24*s,
						a21*c+a11*s, a22*c+a12*s, a23*c+a13*s, a24*c+a14*s,
						a31,         a32,         a33,         a34,
						a41,         a42,         a43,         a44        ),
					 (c*i11 - i12*s, c*i12 + i11*s, i13, i14,
						c*i21 - i22*s, c*i22 + i21*s, i23, i24,
						c*i31 - i32*s, c*i32 + i31*s, i33, i34,
						c*i41 - i42*s, c*i42 + i41*s, i43, i44))
			end
					
	fun invert ((a,i):m4):m4 = (i,a)
	


	fun m4toString ((a11, a12, a13, a14,
									a21, a22, a23, a24,
									a31, a32, a33, a34,
									a41, a42, a43, a44),_) = 
			let
					val $ = Real.toString
					val s = ", "
					val r1 = "(" ^ $a11 ^ s ^ $a12 ^ s ^ $a13 ^ s^ $a14 ^ ")\n"
          val r2 = "(" ^ $a21 ^ s ^ $a22 ^ s ^ $a23 ^ s^ $a24 ^ ")\n"
					val r3 = "(" ^ $a31 ^ s ^ $a32 ^ s ^ $a33 ^ s^ $a34 ^ ")\n"
          val r4 = "(" ^ $a21 ^ s ^ $a22 ^ s ^ $a23 ^ s^ $a24 ^ ")\n"
			in
					"\n" ^ r1 ^ r2 ^ r3 ^ r4
			end
				
	 
  fun v4toString (wx,wy,wz,w) =
	let
			val $ = Real.toString
			val s = ", "
	in
			"<" ^ $wx ^ s ^ $wy ^ s ^ $wz ^ s ^ $w ^ ">"
	end


  fun v3toString (x,y,z) =
	let
			val $ = Real.toString
			val s = ", "
	in	
			"<" ^ $x ^ s ^ $y ^ s ^ $z ^ ">"
	end

end
