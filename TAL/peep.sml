(* simple peephole optimization 
 *)

structure TalPeep :> TALPEEP = 
  struct
    structure T = Tal

    fun eq_gops gop1 gop2 =
      (case (gop1,gop2)
	 of (Tal.Reg r,Tal.Reg r') => r = r'
	  | (Tal.Prjr ((Tal.Esp,[]),i,NONE), Tal.Prjr ((Tal.Esp,[]),i',NONE)) => i = i'
	  | _ => false)

    fun qopt qs = 
      (case qs
	 of [] => []
	  | [q]	=> [q]
	  | (T.Roll c)::(T.Tosum _)::qs => (Tal.RollTosum c):: (qopt qs)
	  | q::qs => q::qopt qs)

    (* uncomment s ==> s1,s2  
      * s1 is any initial comments, reversed
      * s2 is the remainder
      *)
    fun uncomment instrs = 
      let
	fun loop (coms,instrs) =
	  (case instrs 
	     of (i as T.Comment s)::instrs => loop(i :: coms,instrs)
	      | _ => (coms,instrs))
      in loop([],instrs)
      end
    
    fun peep2 (instr1,instr2) = 
      let
	val instrs_opt = 
	  (case (instr1,instr2)

	     (* ADD ESP, imm1       ========> ADD ESP, imm1+imm2
	       ADD ESP, imm2
	       *)
	     of (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i), T.ArithBin(T.Add,T.Reg T.Esp,T.Immed j)) =>
	       SOME [T.ArithBin(T.Add,T.Reg T.Esp,T.Immed(i + j))]


	      (* SUB ESP, imm1       ========> SUB ESP, imm1+imm2
		SUB ESP, imm2
		*)
	      | (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed i), T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed j)) =>
	       SOME [T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed(i+j))]

	      (* ADD ESP, imm1       ========> OP ESP, abs(imm1 - imm2)
		SUB ESP, imm2     where OP = ADD if imm1 > imm2, 
		OP = SUB if imm1 < imm2,
		instruction deleted otherwise
		*)
	      | (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i), T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed j)) =>
	       if i = j then SOME []
	       else 
		 let 
		   val (oper,arg) = if i > j then (T.Add,i-j) else (T.Sub,j-i)
		 in  SOME [T.ArithBin(oper,T.Reg T.Esp,T.Immed arg)]
		 end
	      | (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed j), T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i)) =>
	       if i = j then SOME []
	       else 
		 let 
		   val (oper,arg) = if i > j then (T.Add,i-j) else (T.Sub,j-i)
		 in  SOME [T.ArithBin(oper,T.Reg T.Esp,T.Immed arg)]
		 end

	      (* Fstp gop     =>  Fst gop
	       * Fld gop 
		*)
	      | (T.FPsomeargs(T.Fstp,T.FPgenop(T.B8,gop1)),T.FPsomeargs (T.Fld,T.FPgenop(T.B8,gop2))) => 
		 if eq_gops gop1 gop2 then
		   SOME [T.FPsomeargs(T.Fst,T.FPgenop(T.B8,gop1))]
		 else NONE
	      (* MOV    gop, (g,c1) ========> MOV gop, (g, c2@c1)
		COERCE gop, c2
		*)
	      | (T.Mov(gop1,(gop2,qs)),T.Coerce(gop1',qs')) =>
		   if eq_gops gop1 gop1' then
		     SOME[T.Mov(gop1,(gop2,qopt (qs'@qs)))]
		   else  NONE
	      (* COERCE (gop,qs) ========> COERCE (gop,qs'@qs)
		COERCE (gop, qs')
		*)
	      | (T.Coerce(gop,qs),T.Coerce(gop',qs')) =>
		 if eq_gops gop gop' then
		   SOME[T.Coerce(gop,qopt (qs'@qs))]
		 else  NONE
	      (* MOV gop1,gop2 ========> MOV gop1, gop2
	       * MOV gop2,gop1 
		*)
	      | (mv as T.Mov(gop1,(gop2,qs)),T.Mov(gop2',(gop1',qs'))) =>
		   if eq_gops gop1 gop1' andalso eq_gops gop2 gop2' then
		     let
		       val qs' = qs' @ qs
		     in 
		       (case qs'
			  of [] => SOME [mv]
			   | _ => SOME [mv,T.Coerce(gop2,qs')])
		     end
		   else  NONE

	      | _ => NONE)
      in instrs_opt
      end

    fun peep1 instr = 
      (case instr
	 of T.ArithBin(T.Add,gop,T.Immed i) =>
	   (case i
	      of 0w0 => SOME []
	       | 0w1 => SOME [T.ArithUn(T.Inc,gop)]
	       | _ => NONE)
	  | T.ArithBin(T.Sub,gop,T.Immed i) =>
	    (case i
	       of 0w0 => SOME []
		| 0w1 => SOME [T.ArithUn(T.Dec,gop)]
		| _ => NONE)
	  | _ => NONE)


    fun peep_incr (instr1,instrs) = 
      (case instr1 
	 of Tal.Comment _ => instr1::instrs
	  | _ => 
	   (case peep1 instr1
	      of SOME instrs' => Tal.Comment "Optimized":: peep_append instrs' instrs
	       | NONE => 
		let
		  val (comments,uncomments) = uncomment instrs
		in 
		  (case uncomments
		     of [] => instr1::instrs
		      | instr2::extra => 
		       (case peep2 (instr1,instr2)
			  of SOME instrs => List.revAppend (comments,Tal.Comment "Optimized"::peep_append instrs extra)
			   | NONE => instr1 :: instrs))
	   end))
    and peep_append instrs1 instrs2 = 
      (case instrs1 
	 of [] => instrs2
	  | instr1::instrs1 => peep_incr (instr1,peep_append instrs1 instrs2))
    and peep instrs = 
      (case instrs
	 of [] => instrs
	  | instr::instrs => peep_incr(instr,instrs))

  end

(*  (* MOV EAX, [ESP]    ======> POP EAX
    ADD ESP, 4
    *)
  | (T.Mov(T.Reg T.Eax,(T.Prjr((T.Esp,[]),i1,None),[])))::
      (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i2))::rest
      when i1=$i32_0 & i2=$i32_4 ->
	click((T.Pop(T.Reg T.Eax))::rest)

      (* PUSH gc  =========> MOV r,gc
         POP  r
       *)
    | (T.Push gc)::T.Pop(T.Reg r)::rest ->
	click((T.Mov(T.Reg r,gc))::rest)

      (* MOV  EAX, g1      ======> MOV EAX, g1
         PUSH EAX
         MOV  EAX, g2
         POP  EAX

         MOV  EAX, g1      ======> MOV r',  g1
         PUSH EAX                  MOV EAX, g2'
         MOV  EAX, g2     where r' != EAX and g2' is g2 with offset adjusted
         POP  r'          note: incorrect if g2 == r' DOES THIS HAPPEN?
       *)
    | (T.Mov(T.Reg T.Eax,cgop))::(T.Push(T.Reg T.Eax,[]))::
      (T.Mov(T.Reg T.Eax,(gop2,cs2)))::(T.Pop(T.Reg r'))::rest ->
      	(match r' with
	  T.Eax -> click((T.Mov(T.Reg T.Eax,cgop))::rest)
      	|	_ -> click((T.Mov(T.Reg r',cgop))::
			      (T.Mov(T.Reg T.Eax,((match gop2 with
		                T.Prjr((T.Esp,[]),i,None) ->
				  T.Prjr((T.Esp,[]),i-$i32_4,None)
	                      | _ -> gop2),cs2)))::rest))

(* Cyclone *)
(* Changes an absolute call to a relative call, which causes bugs for Cyclone
   so we disable it within templates *)
      (* MOV EAX, addr    ========> CALL addr
         CALL EAX
       *)
    | (T.Mov(T.Reg T.Eax,(T.Addr x,[])))::(T.Call(T.Reg T.Eax,c))::rest 
      when not(in_template) ->
      	click((T.Call(T.Addr x,c))::rest)
(* End Cyclone *)

      (* MOV  EAX, g ========> PUSH g
         PUSH EAX       note: incorrect if EAX is live but doesn't happen
       *)
    | (T.Mov(T.Reg T.Eax,cgop))::(T.Push(T.Reg T.Eax,[]))::rest ->
      	click((T.Push(cgop))::rest)

      (* MOV EAX, imm        ========> MOV [EBX+off], imm
         MOV [EBX+off], EAX   note: incorrect if EAX if live but doesn't happen
       *)
    | (T.Mov(T.Reg T.Eax,(((T.Immed _) as gop),cs)))::
      (T.Mov(T.Prjr ((T.Ebx,[]),i,None),(T.Reg T.Eax,[])))::rest ->
	click((T.Mov(T.Prjr((T.Ebx,[]),i,None),(gop,cs)))::rest)

      (* ADD ESP, imm1       ========> ADD ESP, imm1+imm2
         ADD ESP, imm2
       *)
    | (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i))::
      (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed j))::rest ->
      	click((T.ArithBin(T.Add,T.Reg T.Esp,T.Immed(i+$j)))::rest)

      (* SUB ESP, imm1       ========> SUB ESP, imm1+imm2
         SUB ESP, imm2
       *)
    | (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed i))::
      (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed j))::rest ->
      	click((T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed(i+$j)))::rest)

      (* ADD ESP, imm1       ========> OP ESP, abs(imm1 - imm2)
         SUB ESP, imm2     where OP = ADD if imm1 > imm2, 
                                 OP = SUB if imm1 < imm2,
	                         instruction deleted otherwise
       *)
    | (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i))::
      (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed j))::rest ->
	if i = j then click rest
	else 
	  let op,arg = if i > j then T.Add,i-$j else T.Sub,j-$i in
      	  click((T.ArithBin(op,T.Reg T.Esp,T.Immed arg))::rest)
      
       (* ADD ESP, 0       =========> delete instruction
          
          ADD ESP, 4      =========> MOV [ESP], g
          PUSH g        where g is not ESP, or a projection

          ADD  ESP, i      =========> ADD ESP, i-4 
          PUSH g                      MOV [ESP], g
	                where g is not ESP, or a projection and i > 4
	*)
    | ((T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i)) as i1)::
      (((T.Push(gop,cs))::rest) as tail) ->
	begin
	  let reduce = 
	    i >=$ i32_4 & 
	    (match gop with
	      (T.Immed _ | T.Addr _) -> true
	    | T.Reg T.Esp -> false
	    | T.Reg _ -> true
	    | _ -> false)
	  in
	  if reduce then
	    if i =$ i32_4 then
	      click((T.Mov(T.Prjr((T.Esp,[]),i32_0,None),(gop,cs)))::rest)
	    else 
	      click((T.ArithBin(T.Add,T.Reg T.Esp,T.Immed (i -$ i32_4)))::
		    (T.Mov(T.Prjr((T.Esp,[]),i32_0,None),(gop,cs)))::rest)
	  else if i =$ i32_0 then peephole tail
	  else i1::(peephole tail)
	end

      (* MOV    EAX, (g,c1) ========> MOV EAX, (g, c1@c2)
         COERCE EAX, c2
       *)
    | (T.Mov(T.Reg T.Eax,(gop,coercions)))::
      (T.Coerce(T.Reg T.Eax,coercions2))::rest ->
      	click((T.Mov(T.Reg T.Eax,(gop,coercions2 @ coercions)))::rest)

      (* MOV EBX, 1       ========> MOV EAX, g
         MOV EAX, g                 INC EAX
         ADD EAX, EBX     note: incorrect if EBX is live, but doesn't happen

         MOV EBX, 1       ========> MOV EAX, g
         MOV EAX, g                 DEC EAX
         SUB EAX, EBX     note: incorrect if EBX is live, but doesn't happen
         
         MOV EBX, imm     ========> MOV EAX, g
         MOV EAX, g                 op  EAX, imm
         op  EAX, EBX     note: incorrect if EBX is live, but doesn't happen
      *)
    | (T.Mov(T.Reg T.Ebx,(T.Immed i,[T.Subsume _])))::
      (T.Mov(T.Reg T.Eax,cgop))::
      (T.ArithBin(ab,T.Reg T.Eax,T.Reg T.Ebx))::rest ->
	(match ab with
	  T.Add when i=$i32_1 -> 
	    click((T.Mov(T.Reg T.Eax,cgop))::(T.ArithUn(T.Inc,T.Reg T.Eax))
		     ::rest)
	| T.Sub when i=$i32_1 ->
	    click((T.Mov(T.Reg T.Eax,cgop))::(T.ArithUn(T.Dec,T.Reg T.Eax))
		     ::rest)
	| _ -> click((T.Mov(T.Reg T.Eax,cgop))
			  ::(T.ArithBin(ab,T.Reg T.Eax,T.Immed i))::rest))

      (* MOV ECX, imm      ==========> MOV EAX, g
         MOV EAX, g                    CMP EAX, imm
         CMP EAX, ECX      note: incorrect if ECX is live, but doesn't happen
       *)
    | (T.Mov(T.Reg T.Ecx,(T.Immed i,cs)))::(T.Mov(T.Reg T.Eax,cgop))::
      (T.Cmp((T.Reg T.Eax,ccs),(T.Reg T.Ecx,[])))::rest ->
      	click((T.Mov(T.Reg T.Eax,cgop))::
	      (T.Cmp((T.Reg T.Eax,ccs),(T.Immed i,cs)))
	      ::rest)

       (* JMP g            =========> JMP g
          ...
        *)
    | (T.Jmp cgop)::_::_ -> click [(T.Jmp cgop)]

       (* MOV [ESP+off], EAX  ==========>     MOV [ESP+off], EAX
          MOV r, g                            MOV r,g
          MOV EAX, [ESP+off]   where r != EAX

          MOV [ESP+off1], EAX ==========>     MOV [ESP+off1], EAX
          MOV EAX, g                          MOV EAX, [ESP+off2]
          MOV EAX, [ESP+off2]
        *)
    | ((T.Mov(T.Prjr((T.Esp,[]),i,None),(T.Reg T.Eax,[]))) as i1)::
      ((T.Mov(T.Reg r,cgop)) as i2)::
      ((T.Mov(T.Reg T.Eax,(T.Prjr((T.Esp,[]),j,None),[]))) as i3)::rest ->
	(if i =$ j & r != T.Eax then click(i1::i2::rest)
	else if r = T.Eax then click(i1::i3::rest)
	else (i1::(peephole (List.tl insts))))

    | i::rest -> i::(peephole rest) in
  let rec optimize_loop insts = 
    (some_optimization := false;
     let insts = peephole insts in
     if !some_optimization then optimize_loop insts else insts)
  in optimize_loop insts
*)