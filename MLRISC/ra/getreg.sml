(* getreg.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** A simple round robin register allocator **)
signature GETREG = 
    sig 
        exception GetReg
	val getreg : {pref:int list, proh:int list} -> int 
	val reset : unit -> unit
    end


functor GetReg(val nRegs : int 
	       val available : int list) : GETREG =
struct
  exception GetReg
  val allRegs = Array.array(nRegs,false)

  fun restore regs = 
        app(fn r => Array.update(allRegs,r,true)) regs

  fun prohibit regs = 
        app(fn r => Array.update(allRegs,r,false)) regs

  fun find n = let
      fun search n = if Array.sub(allRegs,n) then n else search(n+1)
    in
	(if Array.sub(allRegs,n) then n else find (n+1))
	     handle _ => search 0
    end

  val lastReg = ref 0

  fun reset () = lastReg:=0

  val _ = restore available

  fun checkPreferred [] = NONE
    | checkPreferred(x::xs) = 
        if Array.sub(allRegs,x) then SOME x else checkPreferred xs

  fun getreg{pref,proh} = let
      val _ = prohibit proh
    in
	case checkPreferred pref
	of NONE => let 
	     val found = 
	       find(!lastReg) handle _ => (restore proh; raise GetReg)
	   in
	     found before (lastReg := (found+1)mod nRegs;
			   restore proh)
	   end
         | SOME found => found before restore proh
    end
end


    

