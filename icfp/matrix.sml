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
			(1, 0, 0, rtx,
			 0, 1, 0, rty,
			 0, 0, 1, rtz
			 0, 0, 0, 1  )

	fun scale (rsx, rsy, rsz) =
			(rsx, 0, 0, 0,
			 0, rsy, 0, 0,
			 0, 0, rsz, 0,
			 0, 0, 0,   1)

	fun uscale rs = 
			(rs, 0, 0, 0
			 0, rs, 0, 0
			 0, 0, rs, 0
			 0, 0, 0,  1)

	fun rotx angle =
	let
			val t = angle * (180.0 / Math.pi)
      val s = sin(t)
			val c = cos(t)
	in
			(1, 0, 0,  0,
			 0, c, ~s, 0,
			 0, s,  c, 0,
			 0, 0,  0, 1)
	end

	fun roty angle =
	let
			val t = angle * (180.0 / Math.pi)
      val s = sin(t)
			val c = cos(t)
	in
			(c, 0, s, 0,
			 0, 1, 0, 0,
		  ~s, 0, c, 0,
			 0, 0, 0, 1)
	end

  fun rotz angle =
	let
			val t = angle * (180.0 / Math.pi)
      val s = sin(t)
			val c = cos(t)
	in
			(c, ~s, 0, 0,
			 s,  c, 0, 0,
			 0,  0, 1, 0
			 0,  0, 0, 1)
	end

  val apply : m4 * v4 -> v4 = raise notdone
  val combine : m4 * m4 -> m4 = raise notdone

  val translateM : real * real * real * v4 -> v4 = raise notdone
  val scaleM : real * real * real -> v4 = raise notdone
  val uScaleM : real * m4 -> m4 = raise notdone
  val rotxM : real * m4 -> m4 = raise notdone
  val rotyM: real * m4 -> m4 = raise notdone
  val rotzM: real * m4 -> m4 = raise notdone

	val m4toString: m4 -> string = raise notdone
  val v4toString: v4 -> string = raise notdone
  val v3toString: v3 -> string = raise notdone

  (*
  datatype symbolic = one | zero | s of string
  fun sMult (m,n,r, A, B) =
	let
			fun access(l, i, j) = List.nth (List.nth (l, i-1), j-1)
			fun smult'(i,j) =
			let
					fun smult''(k) =
							(case (access(A,i,k), access(B,k,j))
								 of (zero, _) => zero::(if k=n then nil else smult'(k+1))
									| (_, zero) => zero::(if k=n then nil else smult'(k+1))
                  | (
			in
					(if (j=0) then "\n" else "") ^
		
*)			
end
