structure LilLinkUnit :> LILLINKUNIT = 
  struct
    structure LD = LilDefs
    structure TD = TranslationDefs
    structure LM = Name.LabelMap
    structure LO = Listops

    fun error s = Util.error "linkunit.sml" s

    fun unit_type_label unitname = Name.internal_label (unitname ^ ".type")
    fun unit_rtype_label unitname = Name.internal_label (unitname ^ ".rtype")
    fun unit_type_var name = Name.fresh_named_var (name ^ ".type") 
    fun unit_rtype_var name = Name.fresh_named_var (name ^ ".rtype") 

    fun main_label () = Name.internal_label "TiltMain"

    fun labels unitname = Name.make_cr_labels (Name.unit_label unitname)
    fun con_label unitname = #1 (labels unitname)
    fun code_label unitname = #2 (labels unitname)

    fun get_labelled_var (vars,l) = 
      (case LM.find(vars,l)
	 of SOME a => a
	  | NONE => error ("Label "^(Name.label2string l)^" not found in vars"))

    fun sort_unit_labels ls = 
      let
	val ls = map (fn l => (Name.label2name (#2 (Name.make_cr_labels l)),l)) ls
	val ls = LO.insertion_sort (fn ((s1,_),(s2,_)) => String.compare (s1,s2)) ls
	val ls = map #2 ls
      in ls
      end

    fun sort_strings ss = 
      let
	val ls = map (fn s => (Name.label2name (code_label s),s)) ss
	val ls = LO.insertion_sort (fn ((s1,_),(s2,_)) => String.compare (s1,s2)) ls
	val ss = map #2 ls
      in ss
      end

    fun make_code_type (imports : string list) (vars : Lil.var LM.map) (return : Lil.con) = 
      let
	val imports = sort_strings imports
	  
	fun rmapper s = 
	  let
	    val l = unit_rtype_label s
	    val a = get_labelled_var(vars,l)
	  in LD.C.var a
	  end
	val rargs = map rmapper imports
	fun mapper s = 
	  let
	    val l = unit_type_label s
	    val a = get_labelled_var(vars,l)
	  in LD.C.var a
	  end
	val args = map mapper imports
      in LD.T.code' (rargs@args) [] return
      end

    fun make_imports (units : {name : string, imports : string list} list) = 
      let
	val names = map #name units
	fun folder ({name, imports} ,vars) = 
	  let
	    val l = unit_type_label name
	    val a = unit_type_var name
	    val vars = LM.insert(vars,l,a)
	    val k = LD.K.T32 ()

	    val l_trep = unit_rtype_label name
	    val a_trep = unit_rtype_var name
	    val vars = LM.insert(vars,l_trep,a_trep)

	    val l_r = code_label name
	    val rtype = LD.T.tupleptr' [LD.C.var a_trep,LD.C.var a]
	    val code_type = make_code_type imports vars rtype
	  in (([(l,a,k),(l_trep,a_trep,k)],(l_r,code_type)),vars)
	  end
	val (acc,_) = LO.foldl_acc folder LM.empty units
	val (lvks,lcs) = LO.unzip acc
	val lvks = List.concat lvks
      in (lvks,lcs)
      end


    fun make_args (vars : Lil.var LM.map) (imports : Lil.label list) : Lil.sv32 list P.pexp = 
      let
	val imports = sort_unit_labels imports
	val svs = map (fn l => Lil.Var_32 (get_labelled_var (vars,l))) imports
	val rargs = List.map (LD.E.select 0w0) svs
	val vargs = List.map (LD.E.select 0w1) svs
	val args = rargs@vargs
	val args = P.List.concat args
      in args 
      end

    (* Given a unit l importing [l_0,...,l_i], 
     * create a call l_main(l_0_v,...,l_i_v)
     *)	
    fun make_call (vars : Lil.var LM.map) {name : Lil.label, imports : Lil.label list} : unit P.pexp = 
      let
	val sv32s = make_args vars imports
	val (entry_c,entry_r) = Name.make_cr_labels name
	val call = 
	  P.bind sv32s (fn sv32s => P.ret (Lil.Call (Lil.Label entry_r,sv32s,[])))
	val lv = get_labelled_var (vars,name)
      in P.Bind.op32' lv call (P.ret ())
      end

    fun make_tiltdata timports units entry_r = 
      let
	fun tolabels {name,imports} = {name = Name.unit_label name,imports = map Name.unit_label imports}
	val units = map tolabels units
	val vars = foldl (fn ({name,...},vars) => LM.insert(vars,name,Name.label2var name)) (LM.empty) units
	val calls = map (make_call vars) units
	val calls = P.Unit.concat calls
	val pexp = P.bind calls LD.E.unit
	val exp = P.Lili.to_exp pexp
	val codef = Lil.Function {tFormals = [],
				  eFormals = [],
				  fFormals = [],
				  rtype = LD.T.unit(),
				  body = exp}
	val datum = Lil.Dcode (entry_r,codef)
      in [datum]
      end
	
    fun linkunit units = 
      let
	val main_label = main_label()
	val (entry_c_l,entry_r_l) = Name.make_cr_labels main_label
	val (timports,vimports) = make_imports units
	val tiltdata = make_tiltdata timports units entry_r_l
	val entry_c = (entry_c_l,Name.fresh_named_var "",LD.K.unit())
	val entry_r = (entry_r_l,LD.T.code' [] [] (LD.T.unit()))
	val linkmod = 
	  Lil.MODULE {unitname = "TiltMain",
		      parms = Name.LabelSet.empty, (* Not really, but... *)
		      entry_c = entry_c,
		      entry_r = entry_r,
		      timports = timports,
		      vimports = vimports,
		      data   = tiltdata,
		      confun = LD.C.star()}
      in linkmod
      end

  end