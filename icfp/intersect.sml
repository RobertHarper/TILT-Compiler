structure Intersect : INTERSECT = 
  struct
    type m4 = Matrix.m4
    type v4 = Matrix.v4
    type v3 = Matrix.v3

    open Vect

    type l1 = bool

    type l2 = {hit : v3,dist : real} list

    type l3info = {u:real,v:real,face:int, 
		   N : v3,         (*Normal vector*)
		   hit : v3,       (*Point of intersection in world coordinates *)
		   dist : real     (*Distance to viewer *)
		   } 
    type l3 = l3info list

    type result = l1 * (unit -> l2) * (unit -> l3)

    val invert    = Matrix.invert

    val dot       = Vect.dp 
    val distance  = Vect.distance
    val normalize = Vect.normalize

    fun no_l2 () : l2 = []
    fun no_l3 () : l3 = []
    val noIntersect = (false, no_l2, no_l3)
    fun memoize f = 
	let val result = ref NONE
	in  fn () => (case (!result) of
			  SOME r => r
			| NONE => let val r = f()
				      val _ = (result := (SOME r))
				  in r
				  end)
	end


    val eps = 1.0e~10
    fun iszero r = (Real.abs r) < eps

    fun quad (a,b,c) = 
      let
	val disc = b*b - 4.0*a* c
	val roots = 
	  if iszero disc then [~b / (2.0*a)]
	  else if disc < 0.0 then []
	  else 
	    let
	      val d = Math.sqrt disc
	      val t0 = (~b + d) / (2.0*a)
	      val t1 = (~b - d) / (2.0*a)
	    in [t0,t1]
	    end
      in (disc,roots)
      end



    (* Where does the plane y=0 transformed by m4 intersect the vector originating from src0 in the direction of dir0 *)
    fun plane1 (m4, src0, dir0) = (* Plane is Ax + By + Cz = D *)
	let val newOrigin = Matrix.applyPoint(m4, (0.0, 0.0, 0.0))
	    val oldNormal = (0.0, 1.0, 0.0)
	    val N = normalize(Matrix.applyVector (m4, oldNormal))
	    val D = dp(N, newOrigin)
	    val numerator = D - dp(N, src0)
	    val denominator = dp(N, dir0)


	    val _ = (print "\n--newOrigin = "; printV3 newOrigin;
		     print "\n  N = "; printV3 N;
		     print "\n  D = "; printR D;
		     print "\n  dir0 = "; printV3 dir0;
		     print "\n  src0 = "; printV3 src0;
		     print "  num = "; printR numerator;
		     print "  denom = "; printR denominator; print "\n")

	in  if (iszero(numerator) orelse   (* point in plane *)
		iszero(denominator))      (* direction parallel to plane *)
		then noIntersect
	    else let val t = numerator / denominator 
(*
		     val _ = (print "  t = "; printR t; print "\n")
*)
		     fun l2() : l2 = let val dist = t (* since dir0 is unit vector; dist might be negative *)
					 val hit = add(src0, scale(t, dir0))
				     in  [{dist = dist, hit = hit}]
				     end
		     val l2 = memoize l2
		     fun l3() : l3 = let val [{hit, dist}] = l2()
(*
					 val _ = (print "plane hit at "; printV3 hit; print "\n")
					 val _ = (print "forward transform is:\n"; Matrix.printM4 m4; print "\n")
*)
					 val (u,_,v) = Matrix.applyPoint(Matrix.invert m4, hit)
					 val tPos = t > 0.0
					 val dpNeg = dp(dir0, N) < 0.0
					 val N = if ((tPos andalso dpNeg) orelse
						     (not tPos andalso not dpNeg))
						     then N
						 else negate N
				     in  [{u = u, v = v, face = 0, N = N,
					   hit = hit, dist = dist}]
				     end
		     val l3 = memoize l3
		 in  (true, l2, l3)
		 end
	end

    

    fun plane2 (M: m4,orig:v3,dir:v3) : result = 
	let
	    
	    val M' = invert M
	    val orig_oc as (x,y,z) = Matrix.applyPoint (M',orig)
	    val dir_oc  as (xd,yd,zd) = Matrix.applyVector (M',dir)

	in  if (iszero y orelse iszero yd)
		then noIntersect
	    else let val dist = ~ y / yd
		     val hit_oc = add(orig_oc, scale(dist, dir_oc))
		     val (u, _, v) = hit_oc
		     val hit = Matrix.applyPoint(M, hit_oc)
		     val N = Matrix.applyVector(M, if (y > 0.0)
						       then (0.0, 1.0, 0.0)
						   else (0.0, ~1.0, 0.0))
		     fun l2() = [{hit=hit,dist=dist}]
		     fun l3() = [{u=u,v=v,face=0,N=N,hit=hit,dist=dist}]
		 in  (true, l2, l3)
		 end
	end

    val plane = plane2

    fun cylinder1 (M: m4,orig:v3,dir:v3) : result = 
      let

	val M' = invert M
	val orig_oc as (x,y,z) = Matrix.applyPoint (M',orig)
	val dir_oc  as (xd,yd,zd) = Matrix.applyVector (M',dir)

	val ttop = (1.0-y)/yd
	val tbot = ~y/yd

	fun intersect t = (x + t*xd, 
			   y + t*yd,
			   z + t*zd)

	fun compute (below,above,t,p as (x,y,z)) = 
	  let
	    val (t,hit' as (hitx',hity',hitz'),face,u,v) = 
	      if below then
		 (tbot,intersect tbot,2,x,z)
	      else if above then (ttop,intersect ttop,1,x,z)
	      else (t,p,0,(Math.atan2(x,z)) / (2.0 * Math.pi),y)
		     
	    val hit  = Matrix.applyPoint (M,hit')
	    val dist = (if t < 0.0 then ~1.0 else 1.0)  * (distance (orig,hit))
	    val N    = (hitx',hitz',~hity')
	    val N    = Matrix.applyVector(M, N)
	  in
	    {u = u,v= v,face = face,
	     N = N,
	     hit = hit,
	     dist = dist
	     }   
	  end

	
	val a = xd*xd + zd*zd  
	val b = 2.0*(x*xd + z*zd)
	val c = x*x + z*z - 1.0
	val (disc,roots) = quad(a,b,c)	  

	fun l2' (l3 : unit -> l3) = memoize (fn () => 
			      let fun f ({hit,dist,...} : l3info) = {hit = hit,dist = dist} 
			      in map f (l3()) 
			      end)

      in
	case roots
	  of []  => 
	    if iszero xd  andalso iszero zd then  (*Parallel to axis*)
	      if (Vect.magnitude (x,0.0,z)) < 1.0 then   (*Within the circle*)
		let
		  val l3 = memoize (fn () =>  [compute(false,true,ttop,(x,1.0,z)),
					       compute(true,false,tbot,(x,0.0,z))])
		in (true,l2' l3,l3)
		end
	      else noIntersect
	    else noIntersect
	   | [t] => 
	    let 
	      val p as (x0,y0,z0) = intersect t
	      val below = y0 < 0.0
	      val above = y0 > 1.0
	    in 
	      if below orelse above then noIntersect
	      else
		let val l3 = memoize (fn () => [compute(below,above,t,p)])
		in (true,l2' l3,l3)
		end
	    end
	   | [t0,t1] => 
	    let
	      val p0 as (_,y0,_) = intersect t0
	      val p1 as (_,y1,_) = intersect t1

	      val below0 = y0 < 0.0
	      val below1 = y1 < 0.0
	      val above0 = y0 > 1.0
	      val above1 = y1 > 1.0
	    in
	      if (below0 andalso below1) orelse (above0 andalso above1) then noIntersect
	      else
		let
		  fun l3 () = [compute (below0,above0,t0,p0), 
			       compute (below1,above1,t1,p1)]
		  val l3 = memoize l3
		in (true,l2' l3,l3)
		end
	    end
      end
	

   fun cylinder2 (M: m4,orig:v3,dir:v3) : result = 
      let
	  val top = Matrix.translateM(0.0,1.0,0.0,Matrix.ident)
	  val top = Matrix.combine(M, top)
	  val bot = Matrix.translateM(0.0,0.0,0.0,Matrix.ident)
	  val bot = Matrix.combine(M, bot)

	  fun l3() = 
	      let val topResult = 
		  let val (hit, _, l3) = plane(top, orig, dir)
		  in  if hit
			  then
			      let val [{u,v,face=_,N,hit,dist}] = l3()
			      in  if (u*u+v*v > 1.0)
				      then []
				  else let val u' = (u + 1.0) / 2.0
					   val v' = (v + 1.0) / 2.0
(*
					   val _ = (print "u  = "; printR u;  print "v  = "; printR v; print "\n")
					   val _ = (print "u' = "; printR u';  print "v' = "; printR v'; print "\n")
*)
				       in  [{u=u',v=v',face=1,N=N,hit=hit,dist=dist}]
				       end
			      end
		      else []
		  end
		  val botResult = 
		  let val (hit, _, l3) = plane(bot, orig, dir)
		  in  if hit
			  then
			      let val [{u,v,face=_,N,hit,dist}] = l3()
			      in  if (u*u+v*v > 1.0)
				      then []
				  else let val u' = (u + 1.0) / 2.0
					   val v' = (v + 1.0) / 2.0
(*
					   val _ = (print "u  = "; printR u;  print "v  = "; printR v; print "\n")
					   val _ = (print "u' = "; printR u';  print "v' = "; printR v'; print "\n")
*)
				       in  [{u=u',v=v',face=2,N=N,hit=hit,dist=dist}]
				       end
			      end
		      else []
		  end
		  val sideResult = 
		    let 
		      val M' = invert M
		      val orig_oc as (x,y,z) = Matrix.applyPoint (M',orig)
		      val dir_oc  as (xd,yd,zd) = Matrix.applyVector (M',dir)

		      val a = xd*xd + zd*zd  
		      val b = 2.0*(x*xd + z*zd)
		      val c = x*x + z*z - 1.0
		      val (disc,roots) = quad(a,b,c)	  

		      fun compute t =   
			let 
			  val hit' = add(orig_oc, scale(t, dir_oc))
			  val (hitx', hity', hitz') = hit'
			  val v = hity'
			in  if (v <= 0.0 orelse v >= 1.0)
			      then NONE
			    else 
			      let
				val u = (Math.atan2(hitx',hitz')) / (2.0 * Math.pi)
(*				val v = y *)
				val face = 0
				   
				val hit  = Matrix.applyPoint (M,hit')
				val dist = (if t > 0.0 then 1.0 else ~1.0) * (distance (orig,hit))
				val N    = (hitx',hitz',~hity')
				val N    = Matrix.applyVector(M, N)
			      in
				SOME {u = u,v= v,face = face,
				      N = N,
				      hit = hit,
				      dist = dist
				      }   
			      end
			end

		    in List.mapPartial compute roots
		    end
	      in  topResult @ botResult @ sideResult
	      end
	  val l3 = memoize l3
	  fun l2() = let fun get{hit,dist,u,v,face,N} = {hit=hit,dist=dist}
		     in  map get (l3())
		     end
	  val res = l3()
      in  if (null res)
	    then noIntersect
	  else (true, l2, l3)
      end

   val cylinder = cylinder2

   fun cone (M: m4,orig:v3,dir:v3) : result = 
      let
	  val top = Matrix.translateM(0.0,1.0,0.0,Matrix.ident)
	  val top = Matrix.combine(M, top)

	  fun l3() = 
	      let val baseResult = 
		  let val (hit, _, l3) = plane(top, orig, dir)
		  in  if hit
			  then
			      let val [{u,v,face=_,N,hit,dist}] = l3()
			      in  if (u*u+v*v > 1.0)
				      then []
				  else let val u' = (u + 1.0) / 2.0
					   val v' = (v + 1.0) / 2.0
(*
					   val _ = (print "u  = "; printR u;  print "v  = "; printR v; print "\n")
					   val _ = (print "u' = "; printR u';  print "v' = "; printR v'; print "\n")
*)
				       in  [{u=u',v=v',face=1,N=N,hit=hit,dist=dist}]
				       end
			      end
		      else []
		  end
		  val sideResult = 
		      let val M' = invert M
			  val orig' as (px,py,pz) = Matrix.applyPoint (M',orig)
			  val dir'  as (dx,dy,dz) = Matrix.applyVector (M',dir)
			  val A = dx*dx - dy*dy + dz*dz
			  val B = 2.0 * (px*dx - py*dy + pz*dz)
			  val C = px*px - py*py + pz*pz
			  val (disc,roots) = quad(A,B,C)
			  fun getRes t = 
			      let val hit' = add(orig', scale(t, dir'))
				  val (hitx', hity', hitz') = hit'
				  val v = hity'
(*				  val _ = (print "v = "; printR v; print "\n") *)
			      in  if (v <= 0.0 orelse v >= 1.0)
				      then NONE
				  else let val (vSinu, _, vCosu) = hit'
					   val narrowConv = 0.999999999999
					   val uRadPrin = if (Real.abs vSinu > 0.3)
							      then Math.asin (narrowConv * vSinu / v)
							  else Math.acos (narrowConv * vCosu / v)
					   val uDegPrin = rad2deg uRadPrin
					   val u = if (vSinu > 0.0) then uDegPrin else uDegPrin + 180.0
					   val hit = Matrix.applyPoint(M, hit')
					   val dist = (if t > 0.0 then 1.0 else ~1.0) * (distance(orig, hit))
					   val N' = (hitx', hitz', ~hity')  (* Differentiate x*x - y*y + z*z component-wise *)
					   val N = normalize(Matrix.applyVector(M, N'))
				       in  SOME {u=u,v=v,face=0,N=N,hit=hit,dist=dist}
				       end
			      end
		      in  List.mapPartial getRes roots
		      end
	      in  baseResult @ sideResult
	      end
	  val l3 = memoize l3
	  fun l2() = let fun get{hit,dist,u,v,face,N} = {hit=hit,dist=dist}
		     in  map get (l3())
		     end
	  val res = l3()
      in  if (null res)
	      then noIntersect
	  else (true, l2, l3)
      end

    fun sphere (M: m4,orig:v3,dir:v3) : result = 
      let
	
	fun l2info (p,d,_) = 
	  let 
	    val hit = Matrix.applyPoint (M,p)
	    val dist = distance (orig,hit)
	  in {hit = hit,dist = d * dist}
	  end
	
	fun l3info ({hit,dist},((x,y,z),_,n)) = 
	  let
	    val v = (y+1.0)/2.0
	    val u = if iszero v orelse iszero (v-1.0) then 0.0  (* What to do here?*)
		    else (Math.atan2(x,z)) / (2.0 * Math.pi)     (*180/pi * (atan (x/z))/360 *)
	    val N' = (n*x,n*y,n*z) 
	    val N = Matrix.applyVector(M, N')
	  in {u = u,v = v,face = 0,
	      hit = hit,
	      dist = dist,
	      N = N}
	  end
	    
	fun YES ps = 
	  let
	    val l2 = memoize (fn () => map l2info ps)
	    val l3 = memoize (fn () => ListPair.map l3info (l2 (),ps))
	  in (true,l2,l3)
	  end
	
	val M' = invert M
	val orig_oc as (x0,y0,z0) = Matrix.applyPoint (M',orig)
	val dir_oc  as (xd,yd,zd) = Matrix.applyVector (M',dir)

	fun intersect t = (x0 + t*xd, 
			   y0 + t*yd,
			   z0 + t*zd)

	val a = dot (dir_oc,dir_oc)                (*dx*dx + dy*dy + dz*dz*)              (*If unit length, this could be 1*)
	val b = 2.0 * (dot (orig_oc,dir_oc))       (*(x0*dx + y0*dy + z0*dz)*)
	val c = dot (orig_oc,orig_oc) - 1.0        (*x0*x0 + y0*y0 + z0*z0 - 1.0*)
	val (disc,roots) = quad(a,b,c)
      in
	case roots
	  of []      => (false,fn () => [],fn () => [])
	   | [t]     => 
	    let 
	      val p = intersect t
	    in YES [(p,1.0,if t < 0.0 then ~1.0 else 1.0)]
	    end
	   | [t0,t1] => 
	    let
	      val p0 = intersect t0
	      val p1 = intersect t1
	    in
	      if t0 < 0.0 then YES[(p1,~1.0,~1.0),(p0,~1.0,1.0)]          (* both points behind us *)
	      else if t1 < 0.0 then YES [(p1,~1.0,~1.0),(p0,1.0,~1.0)]    (* inside sphere *)
	      else YES [(p1,1.0,1.0),(p0,1.0,~1.0)]                       (* both points in front of us *)
	    end
      end






    fun cube (M: m4,orig:v3,dir:v3) : result = 
      let

	  val bottom = Matrix.ident
	  val top = Matrix.translateM(0.0,1.0,0.0,Matrix.ident)

	  val front = Matrix.rotxM(~90.0,bottom)
	  val back = Matrix.translateM(0.0,0.0,1.0,front)

	  val left = Matrix.rotyM(~90.0,front)
	  val right = Matrix.translateM(1.0,0.0,0.0,left)

	  val bottom = M
	  val top = Matrix.combine(M, top)
	  val left = Matrix.combine(M, left)
	  val right = Matrix.combine(M, right)
	  val front = Matrix.combine(M, front)
	  val back = Matrix.combine(M, back)

(*
	  val M' = invert M
	  val orig' as (px,py,pz) = Matrix.applyPoint (M',orig)
	  val dir'  as (dx,dy,dz) = Matrix.applyVector (M',dir)
	  val _ = (print "orig' = "; printV3 orig';
		   print "dir' = "; printV3 dir'; print "\n")
*)
	  fun doFace (p,face) = 
	      let val (hit, _, l3) = plane(p, orig, dir)
	      in  if (hit)
		      then
			  let val [{u,v,face=_,N,hit,dist}] = l3()
(*
			      val _ = (print "hit cube    u = "; printR u;
				       print  "      v = "; printR v;
				       print "       hit = ";
                                       printV3 hit;
				       print "       hit' = ";
                                       printV3 (Matrix.applyPoint (M', hit));
				       print "\n")
*)
			  in  if (u >= 0.0 andalso u < 1.0 andalso
				  v >= 0.0 andalso v < 1.0)
				  then [{u=u,v=v,face=face,N=N,hit=hit,dist=dist}]
			      else []
			  end
		  else []
	      end

	  fun l3() = 
	      let val res = (doFace(front, 0)) @  
		            (doFace(back, 1))  @ 
			    (doFace(left, 2)) @ 
		            (doFace(right, 3)) @ 
			    (doFace(top, 4)) @ 
		            (doFace(bottom, 5)) 

	      in  res
	      end
	  val l3 = memoize l3
	  fun l2() = let fun get{hit,dist,u,v,face,N} = {hit=hit,dist=dist}
		     in  map get (l3())
		     end

	  val res = l3()
      in  if (null res)
	      then noIntersect
	  else (true, l2, l3)
      end


  end
