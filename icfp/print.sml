
structure ScurvyPrint :> SCURVYPRINT =
struct

    open Scurvy

    fun nspcs 0 = ""
      | nspcs n = " " ^ (nspcs (n - 1))

    fun tostring_e x =
	let 
	    fun tses spc x =
		let
		    val tse = tses spc
		in
	(case x of
	     Int i => ("'int " ^ Int.toString i ^ "'")
	   | String s => ("\"" ^ s ^ "\"")
	   | Char c => ("'char " ^ (implode [c]) ^ "'")
	   | Float f => ("'float " ^ Real.toString f ^ "'")
	   | Bool b => if b then "<true>" else "<false>"
	   | If (c,t,f) => ("(if " ^
			    (tse c) ^
			    " then " ^
			    (tse t) ^
			    " else " ^ 
			    (tse f) ^
			    ")")
	   | Fun (sm, arg, e) => ("(fun " ^ 
				  (case sm of
				       NONE => "<anon>"
				     | SOME s => "<" ^ s ^ ">") ^
				       " (" ^ arg ^ ") = " ^ (tse e) ^ ")")
	   | Apply (f, e) => ("(" ^
			      (tse f) ^ " " ^
			      (tse e) ^
			      ")")
	   | Var s => "'" ^ s ^ "'"
	   | Case (e, pel) => "case " ^ (tse e) ^ " of " ^
		 (foldr (fn ((p,e), b) => "\n" ^ 
			 (nspcs spc) ^ "   | " ^ (tostring_p p) ^ " => " ^ 
			     (tses (spc + 4) e) ^ b) "\n" pel)
	   | Tuple (h::t) => "(" ^
		 (foldl (fn (a, b) => b ^ ", " ^ (tse a))
		  (tse h) t) ^ ")"
           | Tuple _ => "<unit>"
	   | Raise e => "Raise (" ^ (tse e) ^ ")"
	   | Exception (se, t) => "Exception (" ^ (tse se) ^ 
		 " of " ^ (tostring_t t) ^ ")"
	   | Newstamp => "<newstamp>"
	   | Handle (e,ex, p, hlr) => "(" ^ (tse e) ^ ") handle (" ^ (tse ex)
		 ^ ") with " ^ (tostring_p p) ^ " => (" ^ (tse hlr) ^ ")"
	   | Internalfun _ => "(internalfun)")
	     
		end
	in
	    tses 0 x
	end

    and tostring_p x =
        (case x of 
	     PWild => "_"
	   | PVar s => "'" ^ s ^ "'"
	   | PTuple (h::t) => "(" ^ 
		 (foldl (fn (a, b) => b ^ ", " ^ (tostring_p a))
		  (tostring_p h) t) ^ ")"
           | PTuple _ => "!EMPTY-PATTERN-TUPLE!"
	   | PConstB true => "<true>"
	   | PConstB false => "<false>"
	   | PConstI i => Int.toString i
	   | PConstS s => "\"" ^ s ^ "\""
	   | PConstC c => "?" ^ (implode [c]))
      
    and tostring_t x = "(type)"

    and tostring_k x = "(kind)"

end