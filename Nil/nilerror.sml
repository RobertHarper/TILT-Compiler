functor NilErrorFn(structure ArgNil : NIL
		   structure PpNil : PPNIL
		   sharing ArgNil = PpNil.Nil) : (*>*) NILERROR where structure Nil = ArgNil = 
  struct
    val printl = Util.printl
    val lprintl = Util.printl

    structure Nil = ArgNil

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
      (printl "Expression is";
       PpNil.pp_exp exp)
      
    fun perr_c con =
      (printl "Constructor is";
       PpNil.pp_con con)
      
    fun perr_k kind = 
      (printl "Kind is";
       PpNil.pp_kind kind)
      
    fun b_perr_k kind = (perr_k kind;false)
      
    fun perr_e_c (exp,con) = 
      (printl "Expression is";
       PpNil.pp_exp exp;
       lprintl "of type";
       PpNil.pp_con con)
      
    fun perr_c_c (con1,con2) = 
      (printl "Expected constructor";
       PpNil.pp_con con1;
       printl "Found constructor";
       PpNil.pp_con con2)
      
    fun perr_c_k (con1,kind) = 
      (printl "Constructor";
       PpNil.pp_con con1;
       printl "of Kind";
       PpNil.pp_kind kind)
      
    fun perr_k_k (kind1,kind2) = 
      (printl "Expected kind";
       PpNil.pp_kind kind1; print "\n";
       printl "Found kind";
       PpNil.pp_kind kind2; print "\n")
      
    fun perr_c_k_k (con,kind1,kind2) = 
      (printl "Constructor is";
       PpNil.pp_con con;
       lprintl "Expected kind";
       PpNil.pp_kind kind1;
       lprintl "Found kind";
       PpNil.pp_kind kind2)
      
    fun perr_e_c_c (exp,con1,con2) = 
      (printl "Expression is";
       PpNil.pp_exp exp;
       lprintl "Expected type";
       PpNil.pp_con con1;
       lprintl "Found type";
       PpNil.pp_con con2)
      
    fun o_perr pr s opt =  
      let
	val _ = case opt 
		  of (SOME arg) => pr arg
		   | NONE => printl s
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