(*$import List Util Ppnil NILERROR *)

(*
 Miscellaneous functions for use in error checking/reporting
*)

structure NilError :> NILERROR =
  struct
    val printl = Util.printl
    val lprintl = Util.lprintl


    type location = string 

    exception FailedAssert of string 
  
    (*
       val assert : location -> (bool * (unit -> unit)) list -> unit
       Given a location and a list of pairs of flags and error
       reporting functions, call the first error function in the
       list for which the flag is true, and raise FailedAssert with the
       location information.
     *)
    fun assert location checkl = 
      let
	val loc_string = "\nAssertion violated in "^location^": "
	  
	fun check (b,f) = if b then () 
			  else (f() handle any => ();
				printl loc_string;
				raise FailedAssert location)
      in
	List.app check checkl
      end
    
    (*Create a location from a filename and a function name*)
    fun locate file function = file^"::"^function
      
    (*val c_all : ('elt -> bool) -> ('elt -> bool) -> 'elt list -> bool
     *c_all pred fc list
     *       => true if forall(x in list), pred x => true
     *       => fc v if v is the first element in list s.t. pred v => false
     *)
    fun c_all pred fc = 
      let
	fun all [] = true
	  | all (fst::rest) = 
	  if pred fst then
	    all rest
	  else
	    fc fst
      in
	all
      end

    (*val c_all1 : ('elt -> bool) -> ('elt option -> bool) -> 'elt list -> bool
     *c_all1 pred fc list
     *       => true if forall(x in list), pred x => true
     *       => fc (SOME v) if v is the first element in list s.t. pred v => false
     *)   
    fun c_all1 pred fc = 
      let
	fun all [] = true
	  | all (fst::rest) = 
	  if pred fst then
	    all rest
	  else
	    fc (SOME fst)
      in
	all
      end

    (*val c_all2 : (('elt * 'elt) -> bool) -> (('elt * 'elt) option -> bool) 
     *  -> ('elt list * 'elt list) -> bool
     *c_all2 pred fc (l1,l2)
     *       => fc NONE if length l1 != length l2
     *       => true if forall((x,y) in lists), pred (x,y) => true
     *       => fc (SOME (x,y)) if (x,y) is the first pair in l1,l2 s.t. pred (x,y) => false
     *)    
    fun c_all2 pred fc = 
      let
	fun all ([],[]) = true
	  | all (a::arest,b::brest) = 
	  if pred (a,b) then
	    all (arest,brest)
	  else
	  fc (SOME (a,b))
	  | all _ = fc NONE
      in
	all
      end

    (*val c_all3 : (('elt * 'elt * 'elt) -> bool) -> (('elt * 'elt * 'elt) option -> bool) 
     *  -> ('elt list * 'elt list * 'elt list) -> bool
     *c_all3 pred fc (l1,l2,l3)
     *       => fc NONE if not(length l1 = length l2 = length l2)
     *       => true if forall((x,y,z) in lists), pred (x,y,z) => true
     *       => fc (SOME (x,y,z)) if (x,y,z) is the first pair in lists 
     *          s.t. pred (x,y,z) => false
     *)    
    fun c_all3 pred fc = 
      let
	fun all ([],[],[]) = true
	  | all (a::arest,b::brest,c::crest) = 
	  if pred (a,b,c) then
	    all (arest,brest,crest)
	  else
	    fc (SOME (a,b,c))
	  | all _ = fc NONE
      in
	all
      end


  (* Printing functions suitable for use in error reporting *)

    fun perr_e exp = 
      (lprintl "Expression is";
       Ppnil.pp_exp exp;
       printl "")
      
    fun perr_c con =
      (lprintl "Constructor is";
       Ppnil.pp_con con;
       printl "")
      
    fun perr_k kind = 
      (lprintl "Kind is";
       Ppnil.pp_kind kind;
       printl "")
      
    fun b_perr_k kind = (perr_k kind;false)
      
    fun perr_e_c (exp,con) = 
      (lprintl "Expression is";
       Ppnil.pp_exp exp;
       lprintl "of type";
       Ppnil.pp_con con;
       printl "")
      
    fun perr_c_c (con1,con2) = 
      (lprintl "Expected constructor";
       Ppnil.pp_con con1;
       lprintl "Found constructor";
       Ppnil.pp_con con2;
       printl "")
      
    fun perr_c_k (con1,kind) = 
      (lprintl "Constructor";
       Ppnil.pp_con con1;
       lprintl "of Kind";
       Ppnil.pp_kind kind;
       printl "")
      
    fun perr_k_k (kind1,kind2) = 
      (lprintl "Expected kind";
       Ppnil.pp_kind kind1; 
       lprintl "Found kind";
       Ppnil.pp_kind kind2; print "\n")
      
    fun perr_c_k_k (con,kind1,kind2) = 
      (lprintl "Constructor is";
       Ppnil.pp_con con;
       lprintl "Expected kind";
       Ppnil.pp_kind kind1;
       lprintl "Found kind";
       Ppnil.pp_kind kind2;
       printl "")
      
    fun perr_e_c_c (exp,con1,con2) = 
      (lprintl "Expression is";
       Ppnil.pp_exp exp;
       lprintl "Expected type";
       Ppnil.pp_con con1;
       lprintl "Found type";
       Ppnil.pp_con con2;
       printl "")

    (* val o_perr : ('val -> unit) -> string -> 'val option -> bool
       Converts a printing function like one of the above into one that takes an optional version of its usual parameter
       and returns false *)      
    fun o_perr pr s opt =  
      let
	val _ = case opt 
		  of (SOME arg) => pr arg
		   | NONE => lprintl s
      in
	false
      end

    (* o_perr'd versions of the corresponding functions above *)
    
    val o_perr_e = o_perr perr_e
    val o_perr_c = o_perr perr_c
    val o_perr_k = o_perr perr_k
    val o_perr_e_c = o_perr perr_e_c
    val o_perr_c_c = o_perr perr_c_c
    val o_perr_k_k = o_perr perr_k_k
    val o_perr_c_k_k = o_perr perr_c_k_k
    val o_perr_e_c_c = o_perr perr_e_c_c
  end
