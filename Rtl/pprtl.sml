functor Pprtl(structure Rtl : RTL
	      structure Rtltags : RTLTAGS
	      sharing Rtltags.Rtl = Rtl) : PPRTL = 
struct

  structure Rtl = Rtl
  structure Rtltags = Rtltags

  open Rtl Formatter
  structure Formatter = Formatter

  val elideSave = ref true
  val DEBUG = ref false

  val symbolic_name = ref true
  val predicted = ref false
  val i2s = Int.toString

  fun pp_list_flat doer objs (left,sep,right,break) = 
      let 
	  fun loop [] = [String right]
	    | loop [a] = [doer a, String right]
	    | loop (a::rest) = (doer a) :: (String sep) :: Break :: (loop rest)
	  val fmts = (String left) :: (loop objs)
      in fmts
      end

  fun pp_list doer objs (left,sep,right,break) = 
      let 
	  fun loop [] = [String right]
	    | loop [a] = [doer a, String right]
	    | loop (a::rest) = (doer a) :: (String sep) :: Break :: (loop rest)
	  val fmts = (String left) :: (loop objs)
      in (if break then Vbox0 else HOVbox0 1) (size left) 1 fmts
      end

  fun extend name = let val diff = 10 - size name
			fun loop n = if (n < 0) then ""
				     else " " ^ (loop (n-1))
		    in  name ^ loop diff
		    end
  fun plain [] = Hbox[]
    | plain (c::rest) = Hbox((String (extend c))::(map String rest))
  fun separate _ [] = []
    | separate _ [a] = [a]
    | separate sep (a::b) = a :: sep :: (separate sep b)


  (* ------------ various to-string functions --------------------------- *)

  val var2s = Name.var2string
  fun pp_var' v = String (var2s v)
  fun bool2s true = "true"
    | bool2s false = "false"
  fun local_label2s (LOCAL_CODE l) = "C" ^ var2s l
    | local_label2s (LOCAL_DATA l) = "D" ^ var2s l
  fun label2s (ML_EXTERN_LABEL s) = "ML "^s
    | label2s (C_EXTERN_LABEL s) = "C "^s
    | label2s (LOCAL_LABEL ll) = "L"^local_label2s ll
  fun rep2s TRACE  = "(TRACE)"
    | rep2s UNSET = "(UNSET)"
    | rep2s NOTRACE_INT = "(NOTRACE_INT)"
    | rep2s NOTRACE_CODE = "(NOTRACE_CODE)"
    | rep2s NOTRACE_REAL = "(NOTRACE_REAL)"
    | rep2s LABEL = "(LABEL)"
    | rep2s LOCATIVE = "(LOCATIVE)"
    | rep2s (COMPUTE p) = "(COMPUTE "^reppath2s p^")"

   and reppath2s p =
       let fun loop [] = ""
	   | loop (i::rest) = "." ^ (Int.toString i) ^ (loop rest)
       in  case p of
	   Var_p v => regi2s v
	 | Projvar_p (v,indices) => regi2s v^(loop indices)
	 | Label_p l => label2s l
	 | Projlabel_p (l,indices) => label2s l^(loop indices)
	 | Notneeded_p => "Notneed"
       end

  and regi2s (REGI(v,tf)) = (if !symbolic_name then var2s v else i2s(Name.var2int v)) ^ (rep2s tf)
    | regi2s (SREGI HEAPPTR)  = "HEAPPTR"
    | regi2s (SREGI HEAPLIMIT)  = "HEAPLIMIT"
    | regi2s (SREGI STACKPTR) = "STACKPTR"
    | regi2s (SREGI EXNPTR)   = "EXNPTR"
    | regi2s (SREGI EXNARG)   = "EXNARG"


  and regf2s (REGF(v,tf)) =
    (if !symbolic_name then var2s v else i2s(Name.var2int v))
       ^ (rep2s tf)

  fun ea2s (EA (r,d)) = i2s d^"("^regi2s r^")"

  fun sv2s (REG r) = regi2s r
    | sv2s (IMM i) = i2s i

  fun cmpf2s c =
      case c
      of EQ => "eq"
       | NE => "ne"
       | LE => "le"
       | LT => "lt" 
       | GE => "ge"
       | GT => "gt"
       | LBS => "lbs"
       | LBC => "lbc"

  fun cmpi2s c signflag = (if signflag then "" else "u")^(cmpf2s c)

  fun pred2s true = "taken"
    | pred2s false = "not taken"

  fun align2s LONG = "long"
    | align2s QUAD = "quad"
    | align2s ODDLONG = "oddlong"
    | align2s OCTA = "octa"
    | align2s ODDOCTA = "oddocta"

  fun tt2s INT_TT = "(INT_TT)"
    | tt2s REAL_TT = "(REAL_TT)"
    | tt2s BOTH_TT = "(BOTH_TT)"



  fun pp_LabelPair' (l,t) =
      HOVbox [String (label2s l),String (rep2s t)]

  fun pp_List' pr l = 
     let fun f (h::t) = String "," :: Break :: pr h :: f t
           | f nil = [String "]"]
     in case l
        of nil => String "[]"
         | h :: t => HOVbox (String "[" :: pr h :: f t)
     end

  fun pp_Array' pr a =
    let fun loop i = if i<Array.length a
		     then pr(Array.sub(a,i)) :: (Break0 0 0)  :: loop(i+1)
		     else nil
    in Vbox0 0 1 (loop 0)
    end

  val pp_RegiList' = pp_List' (String o regi2s)

  val pp_RegfList' = pp_List' (String o regf2s)

  fun pp_RegPair' (intregs,fregs) =
      HOVbox [String "(",pp_RegiList' intregs,
	      String "; ",pp_RegfList' fregs,String ")"]

  fun pp_Save'(SAVE p) = pp_RegPair' p



  fun op3i name (r,sv,dest) = plain [name,regi2s r,", ",sv2s sv,", ",regi2s dest]
  fun op2i name (r,dest) = plain [name,regi2s r,", ",regi2s dest]
  fun opif name (isrc,fdest) = plain [name,regi2s isrc,", ",regf2s fdest]
  fun opfi name (fsrc,idest) = plain [name,regf2s fsrc,", ",regi2s idest]
  fun opffi name (fsrc1,fsrc2,idest) = plain [name,regf2s fsrc1,", ",regf2s fsrc2,", ",regi2s idest]
  fun op3f name (r1,r2,dest) = plain [name,regf2s r1,", ",regf2s r2,", ",regf2s dest]
  fun op2f name (r,dest) = plain [name,regf2s r,", ",regf2s dest]
  fun op2si name (dest,ri) = plain [name,regi2s ri,", ",ea2s dest]
  fun op2li name (src,ri) = plain [name,regi2s ri,", ",ea2s src]



  val w2i = TilWord32.toInt
  val i2w = TilWord32.fromInt
  val word2str = TilWord32.toDecimalString

  fun wordpair2str (a,b) = "(" ^ (i2s (w2i a)) 
      ^ ", " ^ (i2s (w2i b)) ^ ")";


  fun pp_Instr' instr =
	     case instr of
		 LI (i,ri) => plain["li",word2str i,", ",regi2s ri]
	       | LADDR (label,i,r) => plain ["laddr",(i2s i),"(",
					     label2s label,"), ", regi2s r]
	      | LEA (ea,r) => plain["lea",(ea2s ea),", ",regi2s r]
	      | CMV (cmp,test,src,dest) =>
		    plain["cmv", cmpi2s cmp false, " ", 
			  regi2s test, ", ", sv2s src, ", ", regi2s dest]
	      | MV (src,dest) =>  plain["mv", regi2s src, ", ", regi2s dest]
	      | FMV (src,dest) => plain["fmv", regf2s src, ", ", regf2s dest]
	      | ADD a => op3i "addl" a
	      | SUB a => op3i "subl" a
	      | S4ADD a => op3i "s4add" a
              | S8ADD a => op3i "s8add" a
              | S4SUB a => op3i "s4sub" a
              | S8SUB a => op3i "s8sub" a
	      | MUL a => op3i "mull" a
	      | DIV a => op3i "divl" a
	      | MOD a => op3i "modl" a
              | ADDT a => op3i "addl/v" a
	      | SUBT a => op3i "subl/v" a
	      | MULT a => op3i "mull/v" a
	      | DIVT a => op3i "divl/v" a
	      | MODT a => op3i "modl/v" a
              | CMPUI (cmp,r,v,dest) => op3i ("cmp"^cmpi2s cmp false) (r,v,dest)
              | CMPSI (cmp,r,v,dest) => op3i ("cmp"^cmpi2s cmp true)  (r,v,dest)
	      | NOTB a => op2i "and" a
	      | ANDB a => op3i "and" a
	      | ORB a => op3i "or" a
              | XORB a => op3i "xor" a
              | SRA a => op3i "sra" a
              | SRL a => op3i "srl" a
              | SLL a => op3i "sll" a
	      | FADDD a => op3f "add" a
	      | FSUBD a => op3f "sub" a
	      | FMULD a => op3f "mul" a
              | FDIVD a => op3f "div" a
              | FABSD a => op2f "abs" a
              | FNEGD a => op2f "neg" a
	      | CVT_REAL2INT a => opfi "floor" a
	      | CVT_INT2REAL a => opif "int2real" a
	      | SQRT  a => op2f "sqrt" a
	      | SIN   a => op2f "sin" a
	      | COS   a => op2f "cos" a
	      | ARCTAN a => op2f "arctan" a
	      | EXP  a => op2f "exp" a
	      | LN   a => op2f "ln" a
	      | CMPF (cmp,r,v,dest) => opffi (cmpf2s cmp) (r,v,dest)
              | BR l => String("br "^local_label2s l)
              | BCNDI (cmp,regi,dest,pred) =>
		    plain("b"^(cmpi2s cmp true) ::
			  regi2s regi ::
			  local_label2s dest ::
			  (if !predicted then [pred2s pred]
			   else nil))
              | BCNDF (cmp,regf,dest,pred) =>
		    plain("br"^cmpf2s cmp ::
			  regf2s regf ::
			  local_label2s dest ::
			  (if !predicted then [pred2s pred]
			  else nil))
              | BCNDI2 (cmp,regi1,sv2,dest,pred) =>
		    plain("b"^(cmpi2s cmp true)^"2" ::
			  regi2s regi1 ::
			  sv2s sv2 ::
			  local_label2s dest ::
			  (if !predicted then [pred2s pred]
			   else nil))
              | BCNDF2 (cmp,regf1,regf2,dest,pred) =>
		    plain("br"^(cmpf2s cmp)^"2" ::
			  regf2s regf1 ::
			  regf2s regf2 ::
			  local_label2s dest ::
			  (if !predicted then [pred2s pred]
			  else nil))
              | JMP (r,labels) => Hbox [String ("jmp "^regi2s r),
					pp_List' (String o local_label2s) labels]
              | CALL {extern_call, func,return : regi option,args=(ia,fa),
			results=(ir,fr),tailcall,save} =>
		   HOVbox0 1 15 1
		   [String (extend (if extern_call then "ext_call" else  "call")),
		    String (case func
				of REG' f => (regi2s f)
			      | LABEL' l => (label2s l)),
		    Break,
		    String "arguments = (",
		    pp_RegPair'(ia,fa),
		    Break,

		    String "results = (",
		    pp_RegPair'(ir,fr),
		    Break,

		    String "{ret= ",
		    String (case return
				of (SOME ret) => (regi2s ret)
			      | _ => "none"),
		    if !elideSave then String ""
		    else (HOVbox [String " saved = ",pp_Save' save]),
			String " tailcall = ",
			String (bool2s tailcall),
			String "}"]
              | RETURN r => plain["return", regi2s r]
	      | SAVE_CS  l => String ("save_cs"^local_label2s l)
              | END_SAVE => String "end save"
	      | RESTORE_CS => String "restore_cs"
	      | LOAD32I a       => op2li "ldl" a
	      | STORE32I a      => op2si "stl" a
              | LOADQF (ea,r)   => plain ["ldt ",regf2s r,", ",ea2s ea]
              | STOREQF (ea,r)  => plain ["stt ",regf2s r,", ",ea2s ea]
              | NEEDMUTATE (r)  => plain ["needmutate ",regi2s r]
              | NEEDGC (sv)     => plain ["needgc ",sv2s sv]
              | FLOAT_ALLOC(r1,r2,r3,tag) => 
		   plain [regi2s r3, " <- float_alloc ",
			  regi2s r1,"  ",regf2s r2]
              | INT_ALLOC(r1,r2,r3,tag) => 
		   plain [regi2s r3, " <- int_alloc ",
			  regi2s r1,"  ",regi2s r2]
              | PTR_ALLOC(r1,r2,r3,tag) => 
		   plain [regi2s r3, " <- ptr_alloc ",
			  regi2s r1,"  ",regi2s r2]
	      | (SOFT_VBARRIER tt) => String ("soft_vbarrier" ^ (tt2s tt))
	      | (SOFT_ZBARRIER tt) => String ("soft_zbarrier" ^ (tt2s tt))
	      | (HARD_VBARRIER tt) => String ("hard_vbarrier" ^ (tt2s tt))
	      | (HARD_ZBARRIER tt) => String ("hard_zbarrier" ^ (tt2s tt))
	      | HANDLER_ENTRY    => String "handler_entry"
	      | IALIGN x        => String (".align "^align2s x)
	      | ILABEL l        => String (local_label2s l^":")
	      | HALT            => String ("halt")
	      | ICOMMENT s => String ("### " ^ s)

  fun labelortag2s (PTR l) = label2s l
    | labelortag2s (TAG i) = word2str i

  fun pp_Data' d =
      case d of
	  COMMENT (s) => String ("### " ^ s)
	| STRING (s) =>  String (".ascii \""^s^"\"")
	| INT32 (bi) =>  String (".long "^(word2str bi))
	| INT_FLOATSIZE (a) =>   String (".long_floatsize "^(word2str a))
	| FLOAT (s) =>  String (".double "^s)
	| DATA (l) => String(".data "^(label2s l))
	| ARRAYI (size,bi) =>  String (".long "^word2str bi^" : "^
				     (i2s size))
	| ARRAYF (size,s) =>  String (".double "^s^" : "^
				    (i2s size))
	| ARRAYP (size,l) => String (".data "^labelortag2s l^" : "^
				   (i2s size))
	| ALIGN (a) =>  String (".align "^(align2s a))
	| DLABEL (l) =>  String (label2s l^":")

  fun pp_DataList' da = 
      let fun pp_Data'' x =
	        let val s = pp_Data' x
		in case x of
		     DLABEL _ => s
		   | _ => Hbox[String "     ", s]
		end
      in  pp_List' pp_Data'' da
      end


  fun pp_code' code = 
      let fun pp_Instr'' x =
	        let val s = pp_Instr' x
		in case x
		   of ILABEL _ => s
		    | _ => Hbox[String "     ", s]
		end
      in  pp_Array' pp_Instr'' code
      end

  fun pp_Proc' (PROC{external_name,name,return,args,results,code,known,save,vars}) =
      (if !DEBUG then
	    (print "laying out procedure "; 
	     print (label2s (LOCAL_LABEL name)); print "\n")
      else ();
	   Vbox0 0 1 [String(label2s (LOCAL_LABEL name)),
		      Break,
		      String(case external_name of
				 NONE => ""
			       | SOME l => "(EXTERN = " ^ (label2s l) ^ ")"),
		      Break,
		      Hbox[String "     ", HOVbox[String "args = ",pp_RegPair' args]],
		      Break,
		      Hbox[String "     ret = ",String (regi2s return)],
		      Break,
		      Hbox[String "     known = ",String (bool2s known)],
		      Break,
		      Hbox[String "     ", HOVbox[String "results = ",pp_RegPair' results]],
		      Break,
		      if !elideSave 
			  then String ""
		      else (HOVbox [String " save = ",
				    pp_Save' save,
				    String ","]),
		      Break,
		      String "{", Break,
		      pp_code' code,
		      String "}", Break])
	   
  fun pp_Module' (MODULE{procs,data,main,mutable_objects,mutable_variables}) =
      Vbox0 0 1 ([Break,
		 String ("main = "^label2s (LOCAL_LABEL main)),
		 Break, Break]
		 @ 
		 (separate Break (map pp_Proc' procs))
		 @
		 [String "data objects = ",
		  Break,
		  pp_DataList'  data,
		  Break,
		  HOVbox[String "mutable objects = ",
			 pp_List' (String o label2s) mutable_objects],
		  Break,
		  HOVbox[String "mutable vars = ",
			 pp_List' pp_LabelPair' mutable_variables],
		  Break])

  fun pp_rep_path _ = String "rep_path_not_done"

  fun pp_tags' (tags : Rtltags.tags) = 
      let 
	  fun pp_dyn{bitpos,path} = 
	      Hbox[String (Int.toString bitpos), String " = ",
		   pp_rep_path path]
	  fun pp_tag {static,dynamic} = 
	      let val x = String ("0x" ^ (TilWord32.toHexString static))
		  val y = map pp_dyn dynamic
	      in  (case y of 
		       [] => x
		     | _ => HOVbox(x :: y))
	      end
      in  pp_list pp_tag tags ("(",",",")",false)
      end

    fun wrapper pp out obj = 
      let 
	val fmtstream = open_fmt out
	val fmt = pp obj
      in (Formatter.DoDepth := false;
	  output_fmt (fmtstream,fmt); 
	  close_fmt fmtstream;
	  Formatter.DoDepth := true;
	  fmt)
      end
    fun help pp obj = (wrapper pp TextIO.stdOut obj; ())
    val pp_var = help pp_var'
    val pp_Save = help pp_Save'
    val pp_Instr = help pp_Instr'
    val pp_Data = help pp_Data'
    val pp_Proc = help pp_Proc'
    val pp_Module = help pp_Module'
    val pp_tags = help pp_tags'



end (* Pprtl *)


      
