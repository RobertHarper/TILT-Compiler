(*$import Prelude *)

type pair = int * int
    
local

    fun gt ((a, b) : pair, (c, d) : pair) : bool = a > c andalso b > d

in

    overload > : 'a as (* ... and *) gt

end
