(* The optimizer should eventually do the following:
 Eliminate redundant trap barriers,
 Eliminate unnecessary load immediates
 Jump paths shortening and possibly some little inlining
     like jumps to a return or a jump to a short sequence of
     instructions that end in a return or unconditional branch
*)     

(* XXX function calls in GC compress *)

functor MakeRtlopt(structure Rtl : RTL
		   structure Pprtl : PPRTL
		   sharing Pprtl.Rtl = Rtl)
  : RTLOPT    = 
  struct
    open Rtl
    open Pprtl

    val debug = ref false
    val killtraps = ref true
    val killimms  = ref true
    val killlocals  = ref true
    val error = fn s => Util.error "rtlopt.sml" s
    fun msg (s : string) = if (!debug) then print s else ()

	
    fun inreglist(r,[]) = false
      | inreglist(r,a::b) = eqregi(r,a) orelse (inreglist(r,b))
    fun islocal_reg (ireg,PROC{return,args=(arglist,_),results=(reslist,_),...}) =
	not(eqregi(return,ireg) orelse (inreglist(ireg,arglist)) orelse (inreglist(ireg,reslist)))

    local 
	structure IRegkey : ORD_KEY = 
	    struct
		type ord_key = regi
		fun sregtonum HEAPPTR = 0
		  | sregtonum HEAPLIMIT = 1
		  | sregtonum STACKPTR = 2
		  | sregtonum EXNPTR = 3
		  | sregtonum EXNARG = 4
		fun cmpsreg a b = Int.compare(sregtonum a, sregtonum b)
		fun compare (REGI (v1,_), REGI (v2,_)) = Name.compare_var(v1,v2)
		  | compare (SREGI a, SREGI b) = cmpsreg a b
		  | compare (SREGI _, REGI _) = LESS
		  | compare (REGI _, SREGI _) = GREATER
	    end
    in
	structure IRegmap = LocalSplayMapFn(IRegkey)
    end

    fun inproclist(l,[]) = false
      | inproclist(l,a::b) = eq_label(l,a) orelse (inproclist(l,b))



    val i2w = TilWord32.fromInt
    val w2i = TilWord32.toInt
    val wzero = i2w 0

    fun array2list a = 
	let val len = Array.length a
	    fun loop n = if (n >= len) then []
			 else (Array.sub(a,n))::(loop (n+1))
	in  loop 0
	end

    val heapptr  = SREGI HEAPPTR
    val stackptr = SREGI STACKPTR
    val exnptr   = SREGI EXNPTR
    val exnarg   = SREGI EXNARG
    fun is_special_reg r = 
      eqregi(r,heapptr) orelse eqregi(r,exnptr) orelse eqregi(r,exnarg)

    fun instrloop f acc code = 
      let 
	val sz = Array.length code
	fun loop pos acc = 
	  if (pos >= sz)
	    then acc
	  else
	    loop (pos+1) (f (Array.sub(code,pos)) acc)
      in
	loop 0 acc
      end

    fun get_label_proc (PROC {code,...}) acc = 
      let
	fun helper i acc =  
	      case i of
		(LADDR (l,_,_))             => l :: acc
	      | (BR (ll))                   => (LOCAL_LABEL ll) :: acc
	      | (BCNDI (_,_,ll,_))          => (LOCAL_LABEL ll) :: acc
	      | (BCNDF (_,_,ll,_))          => (LOCAL_LABEL ll) :: acc
	      | (BCNDI2 (_,_,_,ll,_))       => (LOCAL_LABEL ll) :: acc
	      | (BCNDF2 (_,_,_,ll,_))       => (LOCAL_LABEL ll) :: acc
	      | (JMP (_,ls))                => (map LOCAL_LABEL ls) @ acc
	      | (CALL{func=(LABEL' l),...}) => l :: acc
	      | SAVE_CS l                   => (LOCAL_LABEL l) :: acc
	      | END_SAVE                    => acc
	      | (ILABEL ll)                 => (LOCAL_LABEL ll) :: acc
	      | _                           => acc
      in instrloop helper acc code
      end

    fun get_label_data data acc = 
      let
	val sz = Array.length data
	fun loop pos acc = 
	  if (pos >= sz) 
	    then acc
	  else let 
		 val d = Array.sub(data,pos)
		 val next = loop (pos+1)
	       in 
		 case d of
		   (DATA (l)) => next (l :: acc)
		 | (DLABEL (l)) => next (l :: acc)
		 | (ARRAYP (_,PTR l)) => next (l :: acc)
		 | _ => next acc
	       end
      in loop 0 acc
      end
    
    (* conservatively return true if the instruction does not change memory
     or any of the special state registers or does not jump/branch *)
    fun state_readonly i = 
      let 
	fun default(r1,REG r, r2) = not ((is_special_reg r1) orelse
				     (is_special_reg r) orelse 
				     (is_special_reg r2))
	  | default(r1,IMM _, r2) = not ((is_special_reg r1) orelse
				       (is_special_reg r2))
      in
	case i of
	  LI  (_,r) => not (is_special_reg r)    
        | LADDR (_,_,r) => not (is_special_reg r)    
	| ADD arg => default arg
	| SUB arg => default arg
	| MUL arg => default arg
	| DIV arg => default arg
	| MOD arg => default arg
	| ADDT arg => default arg
	| SUBT arg => default arg
	| MULT arg => default arg
	| DIVT arg => default arg
	| MODT arg => default arg
	| _ => false
      end

    fun iaccess i = 
      let 
	fun default(r1,REG r,r2) = [r1,r,r2]
	  | default(r1,IMM _,r2) = [r1,r2]
      in 
	case i of
	  LI (_,r) => [r]
	| LADDR  (_,_,r) => [r]
	| LEA    (_,r) => [r]
	| MV     (r1,r2) => [r1,r2]
	| CMV    (_,a,b,c) => default(a,b,c)
	| FMV    (_) => []
	| ADD    arg => default arg
	| SUB    arg =>  default arg
	| MUL   arg =>  default arg
	| DIV   arg =>  default arg
	| MOD   arg =>  default arg
	| S4ADD arg =>  default arg
	| S8ADD arg =>  default arg
	| S4SUB arg =>  default arg
	| S8SUB arg =>  default arg
	| ADDT   arg =>  default arg
	| SUBT   arg =>  default arg
	| MULT   arg =>  default arg
	| DIVT   arg =>  default arg
	| MODT   arg =>  default arg
	| CMPSI  (_,a,b,c) => default (a,b,c)
	| CMPUI  (_,a,b,c) => default (a,b,c)
	
	| NOTB (r1,r2) => [r1,r2]
	| ANDB   arg =>  default arg
	| ORB    arg =>  default arg
	| XORB   arg =>  default arg
	| SRA   arg =>  default arg
	| SRL   arg =>  default arg
	| SLL   arg =>  default arg
	
	| CVT_REAL2INT (_,r) => [r]
	| CVT_INT2REAL (r,_) => [r]

	| FADDD  (_) => nil
	| FSUBD  (_) => nil
	| FMULD  (_) => nil
	| FDIVD  (_) => nil
	| FABSD  (_) => nil
	| FNEGD  (_) => nil

	| SQRT   (_) => nil
	| SIN    (_) => nil
	| COS    (_) => nil
	| ARCTAN (_) => nil
	| EXP    (_) => nil
	| LN     (_) => nil
	| CMPF   (_) => nil
	
	| BR     (_) => nil
	| BCNDI  (_,r,_,_) => [r]
	| BCNDF  (_,_,_,_) => nil
	| BCNDI2 (_,r,REG a,_,_) => [r,a]
	| BCNDI2 (_,r,IMM _,_,_) => [r]
	| BCNDF2 (_,_,_,_,_) => nil
	| JMP    (r,_) => [r]
	    
	| CALL  {extern_call, func: reg_or_label, return,args,results, tailcall,save} => 
	    ((case func of REG' r => [r] | LABEL' _ => nil) @
		(case return of SOME r => [r] | NONE => nil) @
		   (case results of (rs,_) => rs) @
		      (case save of SAVE(rs,_) => rs))

	| RETURN (r) => [r]
	    
	| SAVE_CS _ => []
	| END_SAVE => []
	| RESTORE_CS => []
	    
	| LOAD32I (_,r) => [r]
	| STORE32I (add,r) => [r]
	| LOADQF  (_,_) => nil
	| STOREQF (_,_) => nil
	| NEEDMUTATE (r) => [r]
	| NEEDGC (_) => nil
	| FLOAT_ALLOC (a,b,c,_) => [a,c]
	| INT_ALLOC (a,b,c,_) => [a,b,c]
	| PTR_ALLOC (a,b,c,_) => [a,b,c]
	| SOFT_VBARRIER _ => nil
	| SOFT_ZBARRIER _ => nil
	| HARD_VBARRIER _ => nil
	| HARD_ZBARRIER _ => nil
	| HANDLER_ENTRY => nil
	| ILABEL _ => nil
        | IALIGN _ => nil
	| HALT => nil
      end
    

    fun opt (module as (MODULE{procs=orig_procs, data, main, 
			       mutable_objects, mutable_variables})) = 
      let 
	fun procloop procs f v = 
	  let 
	    fun loop f [] v = v
	      | loop f (p::rest) v = loop f rest (f p v)
	  in loop f procs v
	  end
	val _ = print "-------- RTL optimizer: START ---------------\n";
	val usedlabel = (LOCAL_LABEL main)::
	  (get_label_data data (procloop orig_procs get_label_proc nil))
(*	val _ = map (fn l => (print (label2s l); print "\n")) usedlabel *)

	val used_procs = 
	  let 
	    fun keep (p as PROC{name,...}) acc =
	      if (inproclist (LOCAL_LABEL name, usedlabel))
		then p::acc
	      else (
(*		    print ("Dropping" ^ (label2s (LOCAL_LABEL name)) ^ "\n");  *)
		    acc)
	  in procloop orig_procs keep nil
	  end

	local
	    fun immregs procs = 
		let fun immuse (LI(n,r)) acc = if (TilWord32.ult(n,i2w 255)) 
						   then IRegmap.insert(acc,r,n) 
					       else acc
		      | immuse _         acc = acc
		in procloop procs (fn (PROC{code,...}) => fn acc => 
				   instrloop immuse acc code) 
		    (IRegmap.empty : Word32.word IRegmap.map)
		end
	    
	    fun inreglist(l,d) = (case IRegmap.find(d,l) of
				      SOME _ => true
				    | _ => false)
	    fun findreg_val(l,d) = (case IRegmap.find(d,l) of
				      SOME value => value
				    | NONE => error "findreg_val")
	    fun not_disjoint [] d = false
	      | not_disjoint (a::b) d = inreglist(a,d) orelse (not_disjoint b d)
	    fun make_immregs_safe procs = 
		let 
		    fun immuse i (regs : Word32.word IRegmap.map) = 
			let 
			    val relevant = iaccess i
			    fun intersect [] = false
			      | intersect (a::b) = inreglist(a,regs) orelse (intersect b)
			    fun remove_reg r d = #1(IRegmap.remove(d,r)) handle NotFound => d
			    fun add_regword (d,r,v) = IRegmap.insert(d,r,v)
			    fun remove_regs [] rest = rest
			      | remove_regs (a::b) rest = remove_regs b (remove_reg a rest)
			    fun commute_filter (r1,REG r, r2) regs =
				if (inreglist(r2,regs))
				    then (remove_regs [r1,r,r2] regs)
				else
				    let 
					val b1 = inreglist(r1,regs)
					val b = inreglist(r,regs)
				    in
					if ((not b1 andalso b) orelse
					    (not b  andalso b1))
					    then regs
					else 
					    if (b1 andalso b)
						then (remove_reg r1 regs)
					    else
						error "Rtlopt: LI -> ADD"
				    end
			      | commute_filter(r1, IMM _ ,r2) regs = 
				    (remove_regs [r1,r2] regs)
			    fun noncommute_filter (r1,REG r, r2) regs =
				if (inreglist(r2,regs))
				    then (remove_regs [r1,r,r2] regs)
				else
				    let 
					val b1 = inreglist(r1,regs)
					val b = inreglist(r,regs)
				    in
					if (not b1 andalso b) 
					    then regs
					else if (not b andalso b1)
						 then (remove_reg r1 regs)
					     else 
						 if (b1 andalso b)
						     then (remove_reg r1 regs)
						 else
						     error "Rtlopt: LI -> ADD"
				    end
			      | noncommute_filter(r1, IMM _ ,r2) regs = 
				    (remove_regs [r1,r2] regs)
			in
			    if (not (intersect relevant))
				then regs
			    else
				case i of
				    LI(n,r) => 
					((case IRegmap.find(regs,r) of
					      SOME n' => if (n'=n) then regs else remove_reg r regs
				            | NONE => regs))
				  | ADD arg => commute_filter arg regs
				  | MUL arg => commute_filter arg regs
				  | ADDT arg => commute_filter arg regs
				  | MULT arg => commute_filter arg regs
					
				  | ANDB arg => commute_filter arg regs
				  | ORB arg => commute_filter arg regs
				  | XORB arg => commute_filter arg regs
					
				  | SUB arg => noncommute_filter arg regs
				  | DIV arg => noncommute_filter arg regs
				  | MOD arg => noncommute_filter arg regs
				  | SUBT arg => noncommute_filter arg regs
				  | DIVT arg => noncommute_filter arg regs
				  | MODT arg => noncommute_filter arg regs
				  | S4ADD arg => noncommute_filter arg regs
				  | S8ADD arg => noncommute_filter arg regs
				  | S4SUB arg => noncommute_filter arg regs
				  | S8SUB arg => noncommute_filter arg regs
					
				  | SRA arg => noncommute_filter arg regs
				  | SRL arg => noncommute_filter arg regs
				  | SLL arg => noncommute_filter arg regs
					
				  | _ => (remove_regs relevant regs)
			end

		    fun keep_local p d = 
			let
			    fun foo p [] = []
			      | foo p ((a as (r,_))::rest) = if (islocal_reg (r,p))
								 then a::(foo p rest)
							     else (foo p rest)
			    val culled : (regi * Word32.word) list = foo p (IRegmap.listItemsi d)
			    fun join  ((r,v),d) =  IRegmap.insert(d,r,v) 
			    val res = foldr join 
				(IRegmap.empty : Word32.word IRegmap.map)
				culled
			in
			    res : Word32.word IRegmap.map
			end
		in
		    procloop procs (fn (p as PROC{code,...}) => fn acc => 
				    keep_local p (instrloop immuse acc code)) (immregs procs)
		end

	in

	    fun make_noimm_procs (immregs_safe : Word32.word IRegmap.map ,procs) = 
		let
		    val _ = msg "these are immregs_saves that are being dropped:\n"
		    val _ = if (!debug)
				then (IRegmap.mapi  
				      (fn (r,v) => (pp_Instr (LI (v,r)); print"\n")) immregs_safe; ())
			    else ()
		    fun immreg_process code = 
			let 
			    fun commute_transformer (r1,REG r,r2) =
				if (inreglist(r2,immregs_safe))
				    then error "commute_transformer: result const"
				else 
				    let 
					val b1 = inreglist(r1,immregs_safe)
					val b = inreglist(r,immregs_safe)
				    in
					if (b1 andalso (not b))
					    then (r,IMM (w2i(findreg_val(r1,immregs_safe))),r2)
					else 
					    if (b andalso (not b1))
						then (r1,IMM (w2i(findreg_val(r,immregs_safe))),r2)
					    else if (not b andalso not b1)
						     then (r1, REG r, r2)
						 else 
						     error "commute_transform: both true"
				    end
			      | commute_transformer (r1, IMM n, r2) = 
				    if (inreglist(r2,immregs_safe)
					orelse inreglist(r1,immregs_safe))
					then error "commute_ransfomer"
				    else (r1,IMM n, r2)
			    fun noncommute_transformer (r1,REG r,r2) =
				if (inreglist(r2,immregs_safe))
				    then error "commute_transformer: result not const"
				else 
				    let 
					val b1 = inreglist(r1,immregs_safe)
					val b = inreglist(r,immregs_safe)
				    in
					if (b andalso (not b1))
					    then (r1,IMM(w2i(findreg_val(r,immregs_safe))),r2)
					else 
					    if (not b andalso not b1)
						then (r1, REG r, r2)
					    else
						error "noncommute_transform: can't commute"
				    end
			      | noncommute_transformer (r1, IMM n, r2) = 
				    if (inreglist(r2,immregs_safe)
					orelse inreglist(r1,immregs_safe))
					then error "noncommute_ransfomer"
				    else (r1,IMM n, r2)
			    fun helper2 i =
				case i of
				    ADD arg => ADD(commute_transformer arg)
				  | MUL arg => MUL(commute_transformer arg)
				  | ADDT arg => ADDT(commute_transformer arg)
				  | MULT arg => MULT(commute_transformer arg)
					
				  | ANDB arg => ANDB(commute_transformer arg)
				  | ORB arg => ORB(commute_transformer arg)
				  | XORB arg => XORB(commute_transformer arg)
					
				  | SUB arg => SUB(noncommute_transformer arg)
				  | DIV arg => DIV(noncommute_transformer arg)
				  | MOD arg => MOD(noncommute_transformer arg)
				  | SUBT arg => SUBT(noncommute_transformer arg)
				  | DIVT arg => DIVT(noncommute_transformer arg)
				  | MODT arg => MODT(noncommute_transformer arg)
				  | S4ADD arg => S4ADD(noncommute_transformer arg)
				  | S8ADD arg => S8ADD(noncommute_transformer arg)
				  | S4SUB arg => S4SUB(noncommute_transformer arg)
				  | S8SUB arg => S8SUB(noncommute_transformer arg)
					
				  | SRA arg => SRA(noncommute_transformer arg)
				  | SRL arg => SRL(noncommute_transformer arg)
				  | SLL arg => SLL(noncommute_transformer arg)
				  | _ => if (not_disjoint (iaccess i) immregs_safe)
					     then  error "immreg: default case"
					 else i
			    fun helper i acc = 
				(case i of
				     LI (v',r') => if (inreglist(r',immregs_safe))
						       then acc else (i::acc)
				   | _ => (helper2 i)::acc)
			in instrloop helper nil code
			end
		    fun coder (PROC{external_name,code,name,return,args,results,known,save,vars}) acc = 
			(PROC{external_name=external_name,
			      code=Array.fromList(rev(immreg_process code)),
			      name=name,return=return,args=args,results=results,
			      known=known,save=save,vars=vars})::acc
		in
		    procloop procs coder nil
		end

	    fun immkiller procs = 
		(print "  RTL: immkilling\n";
		 make_noimm_procs (make_immregs_safe procs, procs))
	end
	
	fun localkiller procs = 
	    let 
		val _ = print "  RTL: localkilling\n"
		fun cand_loop p [] = []
		  | cand_loop p ((a as LOAD32I(ea,r1)) :: (b as MV(r2,r3)) :: rest) = 
		    if (eqregi(r1,r2) andalso (islocal_reg(r1,p)))
			then r1::(cand_loop p rest)
		    else (cand_loop p rest)
		  | cand_loop p (a::rest) = cand_loop p rest
			
		fun final_loop p seen c [] = true
		  | final_loop p seen c ((a as LOAD32I(EA(rea,_),r1)) :: (b as MV(r2,r3)) :: rest) = 
			if (eqregi(r1,c)) 
			    then
				if (eqregi(r1,r2) andalso (islocal_reg(r1,p)))
				    then if (seen) then false else final_loop p true c rest
				else false
			else 
			    if (eqregi(rea,c)) then false
			    else final_loop p false c rest
		  | final_loop p seen c (a::rest) = 
				if (inreglist (c,(iaccess a)))
				    then false
				else final_loop p seen c rest
		fun code_walk fs [] = []
		  | code_walk fs ((a as LOAD32I(ea,r1)) :: (b as MV(r2,r3)) :: rest) = 
		    if (eqregi(r1,r2) andalso (inreglist(r1,fs)))
			then 
			    (LOAD32I(ea,r3)::(code_walk fs rest))
		    else
			(a::(b::(code_walk fs rest)))
		  | code_walk fs (a::rest) = a::(code_walk fs rest)
			 

		fun helper (p as PROC{external_name,code,name,return,args,results,known,save,vars}) = 
			let 
			    val codelist = array2list code
			    val candidates = cand_loop p codelist
			    val finalists = 
				let fun x [] = []
				      | x (c::rest) = if (final_loop p false c codelist) 
							  then c::(x rest) else (x rest)
				in 
				    x candidates 
				end
			    val _ = if (!debug) then
				(print "\nThe candidates are: \n";
				 app (fn c => print ("  " ^ (regi2s c))) candidates;
				 print "The finalists are: \n";
				 app (fn c => print ("  " ^ (regi2s c))) finalists;
				 print "\n\n")
				else ()
			    val new_codelist = code_walk finalists codelist
			in
			    PROC{external_name=external_name,
				 code=Array.fromList new_codelist,
				 name=name,return=return,args=args,results=results,
				 known=known,save=save,vars=vars}
			end
	    in
		map helper procs
	    end
	
	fun trapkiller procs = 
	    let
		val _ = print "  RTL: trapkilling\n"
		fun remove_trap [] base = base
		  | remove_trap ((SOFT_VBARRIER INT_TT)::r) base = remove_trap r base
		  | remove_trap (i::r) base = i::(remove_trap r base)
		    
		fun consumer [] [] possible = rev possible
		  | consumer [] safe possible = 
		    (remove_trap safe [SOFT_VBARRIER INT_TT]) @ waiter(rev possible)
		  | consumer ((SOFT_VBARRIER INT_TT)::rest) safe possible = consumer rest (safe @ (rev possible) @ [SOFT_VBARRIER INT_TT]) nil
		  | consumer (i::rest) safe possible = 
		    if (state_readonly i)
			then consumer rest safe (i::possible)
		    else
			(remove_trap safe [SOFT_VBARRIER INT_TT]) @ 
			waiter((rev (remove_trap possible nil)) @ (i::rest))
		and waiter ((SOFT_VBARRIER INT_TT)::rest) = consumer rest nil [SOFT_VBARRIER INT_TT] 
		  | waiter (i::rest)     = i::(waiter rest)
		  | waiter []            = nil
		fun trap_process code = (
					 (*
				     print "trap_process--------------------\n";
				     app (fn i => (printInstr i; print "\n"))
 				       (array2list code);
*)
				     waiter (array2list code))
		fun trapkiller_help (PROC{external_name,code,
					  name,return,args,results,known,save,vars}) acc = 
		    (PROC{external_name=external_name,code=Array.fromList(trap_process code),
			  name=name,return=return,args=args,results=results,
			  known=known,save=save,vars=vars})::acc
	    in
		procloop procs trapkiller_help nil
	    end

	val cur_procs = used_procs
	val cur_procs = if (!killimms)   then immkiller cur_procs else cur_procs
	val cur_procs = if (!killtraps)  then trapkiller cur_procs else cur_procs
	val cur_procs = if (!killlocals) then localkiller cur_procs else cur_procs

	val _ = print "-------- RTL optimizer: END   ---------------\n";
	  
      in MODULE{procs=cur_procs, data=data, main=main,
		mutable_objects = mutable_objects,
		mutable_variables = mutable_variables}
      end


    exception MERGEPROC of string
    fun GCmerge (module as (MODULE{procs, data, main,
				   mutable_objects, mutable_variables})) = 
      let 
	datatype block = BLOCK of (instr list * int option * label list * bool ref)
	fun chunk pred acc [] = (rev acc,nil)
	  | chunk pred acc (a::b) = if (pred a) 
				      then (rev acc,a::b)
				    else chunk pred (a::acc) b
	fun partition pred [] = nil
	  | partition pred (a::b) = 
	      (case (chunk pred [a] b) of
		      (x,y) => x::(partition pred y))
	fun findblocks origcode = 
	  let 
	    fun islabel (ILABEL _) = true
	      | islabel _          = false
	    fun add_proclabel c = 
	      let
		fun loop [] = nil
		  | loop ((CALL arg)::b) = 
		    (ILABEL (named_code_label "CALLLABEL"))::(CALL arg)::(loop b)
		  | loop (a::b) = a::(loop b)
		val temp = loop c
	      in
		case (temp) of
		  (ILABEL _)::more => temp
		| _ => (ILABEL (named_code_label "STARTLABEL"))::temp
	      end
	    val code = add_proclabel origcode
	    val blocks = partition islabel code
	    val _ = if (!debug) then print "done partitioning\n" else ()
	    fun get_info code posslab = 
	      let 
		fun hloop [] (gcacc:int option) labs = BLOCK(code,gcacc,labs,ref false)
		  | hloop ((NEEDGC a)::more) (NONE:int option) labs = loop more NONE labs
		  | hloop ((NEEDGC (REG _))::more) (SOME a) labs = loop more NONE labs
		  | hloop ((NEEDGC (IMM b))::more) (SOME a) labs = loop more (SOME(a+b)) labs
		  | hloop ((BR    l)::more) gcacc labs = 
		    loop more gcacc (LOCAL_LABEL l::labs)
		  | hloop ((BCNDI (_,_,l,_))::more) gcacc labs = 
		    loop more gcacc (LOCAL_LABEL l::labs)
		  | hloop ((BCNDF (_,_,l,_))::more) gcacc labs = 
		    loop more gcacc (LOCAL_LABEL l::labs)
		  | hloop ((BCNDI2 (_,_,_,l,_))::more) gcacc labs = 
		    loop more gcacc (LOCAL_LABEL l::labs)
		  | hloop ((BCNDF2 (_,_,_,l,_))::more) gcacc labs = 
		    loop more gcacc (LOCAL_LABEL l::labs)
		  | hloop ((JMP (_,ls))::more) gcacc labs = 
		    loop more gcacc ((map LOCAL_LABEL ls) @ labs)
		  | hloop (i::more) gcacc labs = loop more gcacc labs
		and loop [BR l] (gcacc:int option) labs = hloop [BR l] gcacc labs
		  | loop [i] gcacc labs = (hloop [i] gcacc 
					   (case posslab of 
					      NONE => labs
					    | SOME l => l::labs))
		  | loop code gcacc labs = hloop code gcacc labs
		val _ = if (!debug) then print "getting info\n" else ()
		val res = loop code (SOME 0) nil
		val _ = if (!debug) then print "getting info\n" else ()
	      in
		res
	      end
	    fun get_info_blocks [] = nil
	      | get_info_blocks [a] = [get_info a NONE]
	      | get_info_blocks (a::(b as ((ILABEL l)::_))::c) = 
		(get_info a (SOME (LOCAL_LABEL l)))::(get_info_blocks(b::c))
	      | get_info_blocks _ = error "rtlopt: internal error"
	    val temp = (get_info_blocks blocks)
	    val _ = if (!debug) then print "done getting temp\n" else ()
	    val info_blocks = 
	      case (temp) of
		(BLOCK(x,y,z,_)::rest) => (BLOCK(x,y,z,ref true)::rest)
	      | _ => error "rtlopt: internal error"
	  in
	    info_blocks
	  end

	fun GCmerge_proc (PROC{external_name,code,name,return,args,results,known,save,vars}) =
	  let
	    val _ = if (!debug) then print "entered merge_proc\n" else ()
	    val blocks = findblocks (array2list code)
	    val _ = if (!debug) then print "found blocks merge_proc\n" else ()
	    fun flatten arg = foldr (op @) nil arg
	    fun get_label (BLOCK((ILABEL ll)::_,_,_,_)) = LOCAL_LABEL ll
	      | get_label _ = raise (MERGEPROC "get_label")
	    fun find_block l = 
	      let 
		fun loop [] = raise (MERGEPROC "find_block")
		  | loop (a::b) = 
		    if (Rtl.eq_label(l,get_label a)) then a else loop b
	      in 
		loop blocks
	      end
	    fun mark_block (BLOCK(_,_,_,r)) = r := true
	    fun get_children (BLOCK(_,_,labs,_)) = map find_block labs
	    fun do_backnodes bad seen nil = bad
	      | do_backnodes bad seen (a::b) = 
		if (List.exists (fn arg => eq_label(get_label arg,get_label a)) seen)
		  then do_backnodes (a::bad) seen b
		else
		  do_backnodes bad (a::seen) ((get_children a) @ b)
	    val backnodes = do_backnodes nil nil blocks
	    val infnodes = List.filter
	      (fn BLOCK(_,NONE,_,_) => true
	        | BLOCK(_,SOME _,_,_) => false) blocks;
	    val badnodes = backnodes @ infnodes
	    val badchildrennodes = flatten(map get_children badnodes)
	    val _ = app mark_block badnodes
	    val _ = app mark_block badchildrennodes

	    fun gcmodify v (b as BLOCK(c,NONE,_,_)) = b
	      | gcmodify v (b as BLOCK(c,x,y,z)) = 
		let 
		  fun loop [] = nil
		    | loop ((NEEDGC _)::more) = loop more
		    | loop (i::more) = i::(loop more)
		  val oldc = loop c
		  val newc = (case v of
				0 => oldc
			      | _ => (case oldc of
					(ILABEL l)::(CALL arg)::more => 
					  (ILABEL l)::(CALL arg)::(NEEDGC (IMM v))::more
				      | (ILABEL l)::more => 
					  (ILABEL l)::(NEEDGC (IMM v))::more
				      | (CALL arg)::more => 
					  (CALL arg)::(NEEDGC (IMM v))::more
				      | more => 
					  (NEEDGC (IMM v))::more))
		in
		  BLOCK(newc,x,y,z)
		end
	    local
	      fun searchhelp (BLOCK(_,NONE,_,_)) = 0
		| searchhelp (BLOCK(c,SOME s,labs,ref true)) = 0
		| searchhelp (b as BLOCK(c,SOME s,labs,ref false)) = 
		  s + (foldr Int.max 0 (map searchhelp (get_children b)))
	    in
	      fun search (BLOCK(_,NONE,_,_)) = 0
		| search (BLOCK(c,SOME s,labs,ref true)) = 
		  searchhelp(BLOCK(c,SOME s,labs,ref false))
		| search (BLOCK(c,SOME s,labs,ref false)) = 0
	    end
	    val newblocks = map (fn b => gcmodify (search b) b) blocks
	    val finalcode = flatten(map (fn BLOCK(c,_,_,_) => c) newblocks)
	  in
	    PROC{external_name=external_name,
		 code=Array.fromList(finalcode),name=name,return=return,args=args,
		 results=results,known=known,save=save,vars=vars}
	  end
	val newprocs = map GCmerge_proc procs
	  handle MERGEPROC s => (print "MERGE_PROC: "; print s; raise (MERGEPROC s))
      in
	MODULE{procs = newprocs, data=data, 
	       main = main, mutable_objects = mutable_objects,
	       mutable_variables = mutable_variables}
      end
  end;


