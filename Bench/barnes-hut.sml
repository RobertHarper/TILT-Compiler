(*$import Prelude TopLevel Int List Help Math Real String Help *)

(* Thread safety due to no top-level refs *)

local

val realEq = Real.==

(* Two-dimensional position, a vector. *)
type pos = real * real

(* A particle's mass. *)
type mass = real

(* A particle's velocity. *)
type velocity = real * real

(* A force (vector) on a particle. *)
type force = real * real

(* A star. *)
type particle = pos * mass * velocity

fun show_real r = 
    let val str = Real.toString r
	val count = if (r >= 0.0)
			then (print " "; 7)
		    else 8
    in  print (if (size str > count)
		   then String.substring(str,0,count)
	       else str)
    end

fun show_pair left right (x,y) =
    (print left;
     show_real x; print ", ";
     show_real y; print right)

fun show_force (f : force) = show_pair "[" "]" f
fun show_velocity (v : velocity) = show_pair "{" "}" v
fun show_position (p : velocity) = show_pair "(" ")" p

fun show_particle ((p,m,v) : particle) = 
    (show_real m; print " @ "; show_position p; print " with "; show_velocity v)

fun show_list max str shower objlist =
    let fun loop (_,[]) = ()
	  | loop (index,p::rest) = (print (Int.toString index); 
				    print ": "; shower p;
				    print "\n"; 
				    if (index >= max)
					then print "...........\n"
				    else loop (index+1, rest))
    in  print str; print ":\n"; loop (0,objlist); print "\n"
    end

val show_particlelist = show_list 10 "Particles" show_particle
val show_forcelist = show_list 10 "Forces" show_force

(*
 * Given two vectors, determine if they are equal.
 *
 * Pre:  p', p vectors
 * Post:  result = p' = p
 *
 * Notes:  We use Real equality, which is not acceptable for real-life
 * numerical computation because of round-off errors.  But for this
 * assignment we do not worry about that.
 *)
fun vecEq ((x', y') : pos, (x, y) : pos) : bool =
    realEq (x', x) andalso realEq (y', y)


(*
 * Given two vectors, compute their sum.
 *
 * Pre:  p', p vectors
 * Post:  result = p' + p
 *)
fun vecSum ((x', y') : pos, (x, y) : pos) : pos =
    let
	val dx = x' + x
	val dy = y' + y
    in
	(dx, dy)
    end


(*
 * Given two vectors, compute the difference between the first and the
 * second.
 *
 * Pre:  p', p vectors
 * Post:  result = p' - p
 *)
fun vecDiff ((x', y') : pos, (x, y) : pos) : pos =
    let
	val dx = x' - x
	val dy = y' - y
    in
	(dx, dy)
    end


(*
 * Given a scalar and a vector, compute a scaled vector.
 *
 * Pre:  a scalar, p vectors
 * Post:  result = ap
 *)
fun vecScale (a : real, (x, y) : pos) : pos =
    let
	val ax = a * x
	val ay = a * y
    in
	(ax, ay)
    end


(*
 * Given two positions, compute the distance between them.
 *
 * Pre:  p', p vectors
 * Post:  result = |p' - p|
 *)
fun vecDist ((x', y') : pos, (x, y) : pos) : real =
    let
	val dx = x' - x
	val dy = y' - y
    in
	Math64.sqrt (dx*dx + dy*dy)
    end


(*
 * The zero vector 0.
 *
 * Invariant:  for all vectors v,  v + 0 = v = 0 + v
 *)
val vecZero = (0.0, 0.0)


(*
 * Auxiliary.
 * Used in grav.
 *
 * Determine if two particles are identical.  Here that means they
 * have the same position and the same mass.
 *
 * Notes:  the equality checking done here is dangerous because
 * of floating-point limitations.  It would have been possible
 * to avoid having to test for identity of particles by specifying
 * as a precondition for grav that the particles passed to it must
 * be known to be identical; in that case, the caller of grav
 * would have to have done its own filtering to guarantee it.
 *)
fun particleEq ((p, m, _) : particle, (p', m', _) : particle) : bool =
    vecEq (p, p') andalso realEq (m, m')


(*
 * Newton's law of gravitation.
 * The force of the first particle on the second particle.
 * Remember this is attractive toward the first.  If the particles are
 * the same, consider this case to result in no force.
 *
 * Pre:  part, part' particles
 *       mass of each particle >= 0
 *       distinct particles do not occupy the same point in space
 * Post:  force of part on part'
 *
 * Notes:  The case of 0 can arise either because the particle is really
 * massless (like photons), or because a dummy particle is
 * involved.
 *
 * That the two particles passed in are not distinct is not a
 * precondition.  That is why a test for identity is performed.
 *
 * Also, there is an assumption that distinct particles do not
 * get so close that they merge.  This happens in real life, of
 * course, as the gravitational attraction between bodies can
 * cause them to collide.  A real-life algorithm would have to
 * take this into account.
 *)
fun grav (part as (p, m, _) : particle, part' as (p', m', _) : particle) : force =
    if particleEq (part, part') then
	vecZero
    else
	let
	    val d = vecDist (p', p)
	    val r = vecDiff (p', p)
	in
	    vecScale (~ (m * m') / (d * d * d), r)
	end



(*
 * Exact calculation.
 *
 * Pre:  list of particles [p_1, ..., p_n]
 * Post:  list of forces [f_1, ..., f_n] such that
 *  f_i is the force exerted by p_1, ..., p_{i-1}, p{i+1}, ..., p_n
 *  on particle p_i.
 *
 * Notes:  The force on a particle is the sum of the forces from all
 * the other particles.
 *)
fun forces ([] : particle list) : force list = []
  | forces [_] = [vecZero]
  | forces ss =
    let
	(*
	 * Force on p' by all the particles in ss.
	 *
	 * Notes:  perhaps clearer, but less efficient, is
	 *  foldl (op vecSum) vecZero (map (fn p => grav (p, p')) ss)
	 *)
	fun force p' =
	    foldl (fn (p, acc) => vecSum (grav (p, p'), acc)) vecZero ss
    in
	map force ss
    end


(*** Below is for Barnes-Hut algorithm. ***)

(*
 * Region of space, determined by its lower left hand corner and its
 * width.
 *)
type box = pos * pos

(*
 * Specification.
 *
 * A tree represents either an empty region,
 * a region with a single particle,
 * or a region with four sub-regions, and a center of mass particle
 * representing all the particles in the region.
 *)
datatype tree =
    Empty
  | Leaf of particle
  | Node of box * particle * (tree * tree * tree * tree)

(*
 * Fraction of box size over distance before the box is considered to
 * be far away from a particle.
 *
 * Notes:  in a real-life algorithm, this would be a parameter,
 * not hard coded.
 *)
val theta : real = 0.5

(*
 * Auxiliary function.
 *
 * Pre:  a box
 * Post:  result = the longest width of the box
 *)
fun boxSize ((_, (dx, dy)) : box) : real =
    Real.max (dx, dy)

(*
 * Is the box b with center of mass particle "far enough" from the
 * given particle?
 *
 * Notes:  <= was used here, but < could also have been possible.
 * The assignment specified, however, "no greater", which is why
 * <= was used.
 *
 * A multiplication was used before comparison.  If a divide were
 * used, the numbers might be slightly different.
 *)
fun farEnough (b : box, (p, _, _) : particle, (p', _, _) : particle) : bool =
    (boxSize b) <= theta * vecDist (p, p')

(*
 * Return a box bounding a collection of particles.
 *
 * Pre:  list of particles
 * Post:  the box contains all the particles.
 *
 * Notes:  there is the question of how to satisfy the box specification,
 * which is that the box be closed on the lower boundary and open on the
 * higher.  Some slack was added here.
 *
 * The cases of zero or one particle are degenerate, and arbitrary
 * choices were made to satisfy the specification.  In reality,
 * it should be a made a precondition of boundingBox that it be
 * called only on lists of two or more particles.  Also good would
 * be if an extra parameter were passed in to indicate lower
 * bounds on the widths of the box to return.  This would avoid the
 * need for arbitrary slack, i.e., we could specify that a
 * precondition is that the lower bounds passed in be greater than
 * the maximum value of the coordinates of the particles in the list.
 * Then we could always choose an open boundary that lies halfway
 * between the maximum found and the lower bound passed in, in order
 * to satisfy the postcondition cleanly.
 *)
fun boundingBox ([] : particle list) : box =
    ((0.0, 0.0), (0.0, 0.0))		(* arbitrary empty *)
  | boundingBox [((x, y), _, _)] = ((x, y), (1.0, 1.0)) (* arbitrary width *)
  | boundingBox (((x'', y''), _, _)::ss) =
    let
	val (x, y, x', y') =
	    foldl (fn (((x, y), _, _), (minx, miny, maxx, maxy)) =>
		   (Real.min (x, minx),
		    Real.min (y, miny),
		    Real.max (x, maxx),
		    Real.max (y, maxy)))
	          (x'',
		   y'',
		   x'',
		   y'')
		  ss

	val (dx, dy) = (x'-x, y'-y)

	val slack = 1.00001

        (* Add slack so that the right boundary is open. *)
	val b = ((x, y), (dx*slack, dy*slack))
    in
	b
    end


(*
 * Pre:  each mass >= 0
 *
 * Return center of mass particle for two particles.
 * The velocity is not computed.
 *
 * If both particles have 0 mass, then return a dummy "zero" particle
 * that has no gravitational effect.
 *)
fun cm (((x, y), m, _) : particle, ((x', y'), m', _) : particle) : particle =
    if realEq (m, 0.0) andalso realEq (m', 0.0) then
	(vecZero, 0.0, vecZero)
    else
	let
	    val m'' = m + m'
	    val x'' = (x*m + x'*m')/m''
	    val y'' = (y*m + y'*m')/m''
	in
	    ((x'', y''), m'', vecZero)
	end

(*
 * Auxiliary.
 *
 * A dummy particle for the degenerate case of an empty region.
 *
 * Invariants:  the empty particle contributes 0 to any force
 * or center of mass calculation.
 *)
val emptyParticle : particle = (vecZero, 0.0, vecZero)

(*
 * Auxiliary.
 *
 * Return a particle gravitationally equivalent to the particles in
 * a given tree.
 *)
fun treeMass (Empty : tree) : particle = emptyParticle
  | treeMass (Leaf i) = i
  | treeMass (Node (_, i, (_, _, _, _))) = i


(*
 * Given a box and a collection of particles that are known to be
 * inside the box, return a tree representing the collection.
 *
 * Pre:  the collection of particles lies within the box.
 * Post:  a tree satisfying the data type's specification.
 *
 * Argument that the specification is satisfied:
 *   The induction is on the length of the list of
 *   particles and on the size of the box.
 *
 * - no particles => empty tree (ok)
 * - one particle => leaf (ok)
 * - many particles =>
 *   By construction of the four boxes, and the filtering of
 *   the original particle list into four lists, we know that
 *   each (box, list) satisfies the preconditions for createTree.
 *
 *   Note that the size of each box is half that of the original
 *   box, and the length of each list is <= the length of the
 *   original list.  Also note (geometry) that a box cannot be
 *   divided indefinitely while the number of particles remains
 *   the same.  So our induction is well-founded.
 *
 *   By induction, each of the resulting trees satisfies the
 *   createTree postcondition, and therefore each respects the
 *   data type's specification.
 *
 *   By construction, the four boxes partition the original box,
 *   so that the region represented by the original box is identical
 *   to the union of the regions represented by the four boxes
 *   (no particle is left out or extraneous).  In other words,
 *   "all the particles in box b" =
 *   union of "all the particles in box b_i".
 *
 *   By the property of center of mass particle combination, the
 *   center of mass particle that is computed, and put into the
 *   new tree, really does represent all the particles in box b.
 *
 *   Thus the returned tree satisfies the data type specification.
 *)
fun createTree (_ : box, [] : particle list) : tree = Empty
  | createTree (_, [i]) = Leaf i
  | createTree (b as ((x, y), (dx, dy)), ss) =
    let
	(* Halfway. *)
	val d as (dx', dy') = (dx/2.0, dy/2.0)

        (* Middle. *)
	val x' = x + dx'
	val y' = y + dy'

	(*
	 * New boxes.  These are disjoint.
	 *
	 * b1 = NE
	 * b2 = NW
	 * b3 = SE
	 * b4 = SW
	 *)
	val b1 = ((x', y'), d)
	val b2 = ((x, y'), d)
	val b3 = ((x', y), d)
	val b4 = ((x, y), d)

	(*
	 * Partition the particles in ss into the new boxes.
	 *
	 * Notes:  the partitioning could be done more efficiently
	 * by using a function that returns a pair of lists, one
	 * containing particles inside a region, the other containing
	 * particles outside the region; then each successive filtering
	 * would be done only on the list of particles remaining.
	 *
	 * The code was written for clarity instead.  Also, this
	 * simpler form is more suitable for parallelization,
	 * supposing we had runtime support that:  four calls to
	 * createTree could be made simultaneously.
	 *)
	val ss1 = List.filter (fn ((x'', y''), _, _) =>
			       x'' >= x' andalso y'' >= y') ss
	val ss2 = List.filter (fn ((x'', y''), _, _) =>
			       x'' < x' andalso y'' >= y') ss
	val ss3 = List.filter (fn ((x'', y''), _, _) =>
			       x'' >= x' andalso y'' < y') ss
	val ss4 = List.filter (fn ((x'', y''), _, _) =>
			       x'' < x' andalso y'' < y') ss

	(* New trees. *)
	val t1 = createTree (b1, ss1)
	val t2 = createTree (b2, ss2)
	val t3 = createTree (b3, ss3)
	val t4 = createTree (b4, ss4)

	(*
	 * We take advantage of the fact that the center of mass
	 * particle of two disjoin regions of particles = the center
	 * of mass particle of the center of mass particles of those
	 * regions.
	 *
	 * We can apply this fact because t1, t2, t3, t4 are known
	 * to be disjoint, from construction.
	 *)
	val p = cm (cm (cm (treeMass t1,
                            treeMass t2),
                        treeMass t3),
                    treeMass t4)
    in
	Node (b, p, (t1, t2, t3, t4))
    end

(* ------- Parallel version of createTree --------- *)
fun createTreeP (_ : box, [] : particle list) : tree = Empty
  | createTreeP (_, [i]) = Leaf i
  | createTreeP (b as ((x, y), (dx, dy)), ss) =
    let
	val d as (dx', dy') = (dx/2.0, dy/2.0)
	val x' = x + dx'
	val y' = y + dy'
	val b1 = ((x', y'), d)
	val b2 = ((x, y'), d)
	val b3 = ((x', y), d)
	val b4 = ((x, y), d)

	pval t1 = createTree(b1, List.filter (fn ((x'', y''), _, _) =>
					      x'' >= x' andalso y'' >= y') ss)
	and  t2 = createTree(b2, List.filter (fn ((x'', y''), _, _) =>
					      x'' < x' andalso y'' >= y') ss)
	and  t3 = createTree(b3, List.filter (fn ((x'', y''), _, _) =>
					      x'' >= x' andalso y'' < y') ss)
	and  t4 = createTree(b4, List.filter (fn ((x'', y''), _, _) =>
					      x'' < x' andalso y'' < y') ss)

	val p = cm (cm (cm (treeMass t1,
                            treeMass t2),
                        treeMass t3),
                    treeMass t4)
    in
	Node (b, p, (t1, t2, t3, t4))
    end


(*
 * The force of a collection of particles (represented by a tree)
 * on a given particle, allowing use of the ``far enough''
 * approximation where appropriate.
 *
 * Argument of correctness:
 *   Structural induction on the tree.
 *
 * - empty tree => represents no particles, so does not exert any force
 * - leaf => represents one particle, so the force exerted is
 *   the force of that single particle
 * - a node with subtrees =>
 *   - far enough => use single particle approximation (ok)
 *   - not far enough =>
 *     Since the total force on a particle = forces from all
 *     other particles, and the data type invariant is that
 *     all the particles are contained in the subtrees, a
 *     reasonable approximation to the total force is the sum
 *     of the total force of b1 particles, total force of b2 particles,
 *     total force of b3 particles, total force of b4 particles.
 *
 *     Finally, we assume that we can get a good approximation
 *     for a sum of elements by summing approximations of those
 *     elements.
 *
 *     Inductive step:
 *     Assume the recursive calls satisfy the specification, which
 *     is that they return approximate forces.  Thus the sum
 *     gives a good approximation.
 *)
fun treeForce (Empty : tree, _ : particle) : force = vecZero
  | treeForce (Leaf i, i') = grav (i, i')
  | treeForce (Node (b, i, (b1, b2, b3, b4)), i') =
    if farEnough (b, i, i') then
	grav (i, i')
    else
        vecSum (vecSum (vecSum (treeForce (b1, i'),
                                treeForce (b2, i')),
                        treeForce (b3, i')),
                treeForce (b4, i'))


(*
 * The Barnes-Hut force calculation.
 *
 * Notes:  for no particle, there is no force
 * For one particle, the force is zero
 *)
fun bhForces ([] : particle list) : force list = []
  | bhForces [_] = [vecZero]
  | bhForces ss =
    let
	val box = boundingBox ss
	val tree = createTree (box, ss)
    in
	map (fn s => treeForce (tree, s)) ss
    end

fun bhForcesP ([] : particle list) : force list = []
  | bhForcesP [_] = [vecZero]
  | bhForcesP ss =
    let 
	val box = boundingBox ss
	val tree = createTreeP (box, ss)
	val res = mapP (fn s => treeForce (tree, s)) ss
    in
	res
    end

(*
 * Given a list of particles, a corresponding list of forces on them, and an amount of time,
 *   update each particle's velocities by applying the force on it for the given amount of time,
 *   update the particle's position by moving it with the new velocity for the given amount of time. 
 *)   
fun applyForceAndMove (particles, forces, delta) = 
    let
	fun loop [] _ = []
	  | loop ((((px,py),m,(vx,vy)) : particle) :: prest) (((fx,fy):force) :: frest) = 
	    let val vx = vx + fx / m * delta
		val vy = vy + fy / m * delta
		val px = px + vx * delta
		val py = py + vy * delta
	    in  ((px,py),m,(vx,vy)) :: (loop prest frest)
	    end
	  | loop _ _ = (print "Error in map2\n"; [])
    in  loop particles forces
    end


fun simulate makeForce particles delta cur stop when = 
    (if (when cur)
	 then (print "Step "; print (Int.toString cur); print "\n";
	       show_particlelist particles)
     else ();
     if (cur < stop)
	 then let 
		  val forces = makeForce particles
		  val particles = applyForceAndMove(particles, forces, delta)
	      in  simulate makeForce particles delta (cur+1) stop when
	      end
     else particles)

(* ------------ Running the code -------------- *)
fun randomParticles n = 
    let  val last = ref 0.5
	(* Generates not so random numbers from 0.0 to 1.0 *)
	fun random() = 
	    let val next = !last
		val next = next * 23.345436 + 0.832723
		val next = next - real(floor next)
		val _ = last := next
	    in  next
	    end
	fun randomParticle() = ((random(), random()), 1.0, vecZero)
	fun loop acc 0 = acc
	  | loop acc n =
	    let val acc' = (randomParticle()) :: acc
	    in  loop acc' (n-1)
	    end
    in  loop [] n
    end

fun standard numParticles numIterations numCheckpoint = 
let 
    val particles = randomParticles numParticles
    val _ = print "\n\n--------- Standard Simulation -------------\n"
    val _ = simulate forces particles 0.00001 0 numIterations (fn n => n mod numCheckpoint = 0)
in  ()
end

fun bh numParticles numIterations numCheckpoint = 
let 
    val particles = randomParticles numParticles
    val _ = print "\n\n--------- Barnes-Hutt Simulation -------------\n"
    val _ = simulate bhForces particles 0.00001 0 numIterations (fn n => n mod numCheckpoint = 0)
in  ()
end;

fun bhP numParticles numIterations numCheckpoint = 
let 
    val particles = randomParticles numParticles
    val _ = print "\n\n--- Parallel Barnes-Hutt Simulation ---\n"
    val _ = simulate bhForcesP particles 0.00001 0 numIterations (fn n => n mod numCheckpoint = 0)
in  ()
end;

(* There are 3 version:
   (1) standard - Standard nbody simulation
   (2) bh - Sequential Barnes-Hutt
   (3) bhP - Parallel Barnes-Hutt
*)

(* val _ = standard 1000 2 1000 *)
(* val _ = bh 1000 10 1000  *)

in
    fun runBarnesHut() = bhP 1000 10 1000  
end
