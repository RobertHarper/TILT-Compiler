structure Light = 
    struct

	open Vect
	val black = (0.0, 0.0, 0.0)

	fun toLight (src, light) = 
	    (case light of
		 Eval.Sunlight (dir, _) => dir
	       | Eval.Pointlight (pos, _) => makeDir(src, pos)
	       | Eval.Spotlight {pos, ...} => makeDir(src, pos))

	fun attenuate d = 100.0 / (99.0 + d * d * d)

	fun illuminate (light, src, dir) = 
	    (case light of
		 Eval.Sunlight (dir, color) => color
	       | Eval.Pointlight(pos, color) => let val d = distance(src, pos)
						in  scale(attenuate d, color)
						end
	       | Eval.Spotlight {pos, dir, color, cutoff, att} =>
						let val toSrc = makeDir(pos, src)
						    val targetAngle = angle(toSrc, dir)    (* Angle in degrees *)
						in  if (targetAngle > cutoff)
							then black
						    else let val factor = Math.pow(dp(dir, toSrc), att)
							     val d = distance(src, pos)
							 in  scale(factor * attenuate d, color)
							 end
						end)
		 
    end
