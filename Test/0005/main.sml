(* This tests an easy-to-make bug regarding infix
   declarations in an SML parser.  - tom7
*)

(* this should parse as infix 0 x0 l.
   See the Definition, page 6.
   But many parsers treat it as a hexadecimal number
   and compile a completely different program...
   *)
local 
    infix 0x0 l
	
    val op x0 = 0
    fun f _ _ = 2
    fun f x0 0 = 1
      | f x0 x = f 0 (x - 1)
in
    val x = if f x0 1 = 1 then print "WRONG\n"
	                  else print "OK\n"
end

(* ditto with infixr. *)
local
    infixr 0x0 l

    val op x0 = 0
    fun f _ _ = 2
    fun f x0 0 = 1
      | f x0 x = f 0 (x - 1)
in
    val x = if f x0 1 = 1 then print "WRONG\n"
	                  else print "OK\n"
end

