(*$import STATISTICS Vector Real Math *)

structure VectorStats :> STATISTICS where type seq = real Vector.vector =
struct

    structure V = Vector
    type seq = real V.vector

    exception Unimplemented
    fun extract f v = if V.length v >= 1
                      then V.foldl f (V.sub (v, 0)) v
                      else raise Domain
    val min = extract Real.min
    fun median v = raise Unimplemented
    val max = extract Real.max
        
    fun square (x:real) = x*x
    fun sum (v, f) = V.foldl (fn (x,y) => y + f x) 0.0 v
    val len : seq -> real = Real.fromInt o V.length
        
    fun mean v = if V.length v >= 1 then sum(v, fn x => x) / len v
                 else raise Domain

    (* The err term and the division by n-1 are taken from Numerical Recipes *)
    fun variance v =
        if V.length v >= 2
        then let val mean = mean v
                 val n = len v
                 val var = sum (v, fn x => square (x - mean))
                 val err = (square (sum (v, fn x => x - mean))) / n
             in
                 (var - err) / (n - 1.0)
             end
        else raise Domain
                      
    val stddev = Math.sqrt o variance

    fun absdev v = if V.length v >= 1
                   then let val mean = mean v
                        in  sum(v, fn x => Real.abs (x - mean)) / len v
                        end
                   else raise Domain
                     
end

structure ListStats :> STATISTICS where type seq = real list =
struct
    type seq = real list
    structure VS = VectorStats
    val fromList = Vector.fromList
    val min = VS.min o fromList
    val median = VS.median o fromList
    val max = VS.max o fromList
    val mean = VS.mean o fromList
    val variance = VS.variance o fromList
    val stddev = VS.stddev o fromList
    val absdev = VS.absdev o fromList
end
