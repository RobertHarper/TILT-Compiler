structure Light = 
    struct

	open Base
	open Vect
	val black = (0.0, 0.0, 0.0)

	fun toLight (src, light) = 
	    (case light of
		 Sunlight (dir, _) => dir
	       | Pointlight (pos, _) => makeDir(src, pos)
	       | Spotlight {pos, ...} => makeDir(src, pos))

	fun attenuate d = 100.0 / (99.0 + d * d)

	fun illuminate (light, src, dir) = 
	    (case light of
		 Sunlight (dir, color) => color
	       | Pointlight(pos, color) => let val d = distance(src, pos)
						in  scale(attenuate d, color)
						end
	       | Spotlight {pos, dir, color, cutoff, att} =>
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
