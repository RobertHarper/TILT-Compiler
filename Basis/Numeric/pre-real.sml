(*$import Prelude *)

extern real_logb : (real, int) -->
extern real_scalb : (real * int, real) -->
extern sqrt : (real, real) -->
extern exp : (real, real) -->
extern ln : (real, real) -->
extern log10 : (real, real) -->
extern sin : (real, real) -->
extern cos : (real, real) -->
extern tan : (real, real) -->
extern atan : (real, real) -->
extern asin : (real, real) -->
extern acos : (real, real) -->
extern tanh : (real, real) -->
extern sinh : (real, real) -->
extern cosh : (real, real) -->
extern setRoundingMode : (int, int) -->
extern getRoundingMode : (int, int) -->

structure PreLargeReal =
    struct
	type real = real
    end

structure PreReal =
    struct
	type real = real
    end
