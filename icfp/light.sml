structure Light = 
    struct

	fun toLight (src, light) = 
	    (case light of
		 Sunlight (dir, _) => dir
	       | Pointlight (pos, _) => makeDir(src, pos)
	       | Spotlight {pos, ...} => makeDir(src, pos))

	fun illuminate (light, src, dir) = 
	    (case light of
		 Sunlight (dir, color) => magnitude color
	       | Pointlight(pos, color) => magnitude color
	       | Spotlight {pos, dir, color, cutoff, att} =>
		     let val toSrc = makeDir(pos, src)
			 val targetAngle = angle(toSrc, dir)    (* Angle in degrees *)
		     in  if (targetAngle > cutoff)
			     then 0.0
			 else let val factor = exp(dp(dir, toSrc), att)
			      in  factor * (magnitude color)
			      end
		     end
		 
    end
