(*$import Prelude TopLevel List Util Ppnil NILERROR *)

structure NilError :> NILERROR =
  struct
    val printl = Util.printl
    val lprintl = Util.lprintl


    type location = string 

    exception FailedAssert of string 
    
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
    
    fun locate file function = file^"::"^function
      
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
      
    fun o_perr pr s opt =  
      let
	val _ = case opt 
		  of (SOME arg) => pr arg
		   | NONE => lprintl s
      in
	false
      end
    
    val o_perr_e = o_perr perr_e
    val o_perr_c = o_perr perr_c
    val o_perr_k = o_perr perr_k
    val o_perr_e_c = o_perr perr_e_c
    val o_perr_c_c = o_perr perr_c_c
    val o_perr_k_k = o_perr perr_k_k
    val o_perr_c_k_k = o_perr perr_c_k_k
    val o_perr_e_c_c = o_perr perr_e_c_c
  end
