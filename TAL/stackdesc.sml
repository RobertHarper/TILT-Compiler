structure StackDesc :> STACKDESC = 
  struct

    structure LU = LilUtil
    structure LTC = LilToTalCon
    structure TE = LilToTalEnv
    structure LC = LilContext

    open StateTypes

    val StackDescWarn = Stats.ff "StackDescWarn"
    fun warn s = if !StackDescWarn then (print "WARNING: ";print s;print "\n") else ()
    fun error s = Util.error "stackdesc.sml" s

    fun b2w i = LU.w2i (i div 0w4)
    fun w2b i = LU.i2w (i * 4)

    (* 
     * lists represent stacks, bottom to top,
     * with zero-indexed offsets indicating offsets from the bottom.
     *)

    (* Because of the adjustability of inargs, Inarg slots and FInarg slots
     * share space: that is, Inarg i means the ith word in Inarg + FInarg space
     * (not just in Inarg space).  *)
    datatype slot = 
      Temp of int     (* ith temp *)
      | Inarg of int  (* ith inarg/tailslot *)
      | Outarg of int (* ith outarg *)
      | FTemp of int
      | FInarg of int
      | FOutarg of int


    type sstat = rstat * bool

    type slist = sstat list 

    exception FrameTooSmall

    fun sstat_size ss = 
      (case ss
	 of (_,true) => 1
	  | (_,false) => 2)

    fun slist_size slist = List.foldl (fn (ss,acc) => sstat_size ss + acc) 0 slist

    fun print_slot slot = 
      let
	val s = 
	  case slot
	    of Temp i => ("Temp_"^(Int.toString i))
	     | Inarg i =>  ("Inarg_"^(Int.toString i))
	     | Outarg i => ("Outarg_"^(Int.toString i))
	     | FTemp i => ("FTemp_"^(Int.toString i))
	     | FInarg i => ("FInarg_"^(Int.toString i))
	     | FOutarg i => ("FOutarg_"^(Int.toString i))
      in print s
      end

    fun eq_slot s1 s2 = 
      (case (s1,s2) 
	 of (Temp i,Temp i') => i = i'
	  | (Inarg i,Inarg i') => i = i'
	  | (Outarg i,Outarg i') => i = i'
	  | (FTemp i,FTemp i') => i = i'
	  | (FInarg i,FInarg i') => i = i'
	  | (FOutarg i,FOutarg i') => i = i'
	  | _ => false)

    fun is_temp slot = 
      case slot of Temp i => true | FTemp i => true 
    | _ => false

    (* generate_functional_updates (NONE) (SOME "stackdesc") ["use_fp","finargs","inargs","outargs","temps","ftemps"]; *)
    type stackdesc = 
      {use_fp : bool,
       finargs : sstat list,
       inargs : sstat list,
       ftemps : sstat list,
       temps : sstat list,
       outargs : sstat list}
    fun set_use_fp ({ use_fp = _  , finargs , inargs , outargs , temps , ftemps } :stackdesc) use_fp =
      {
       use_fp = use_fp,
       finargs = finargs,
       inargs = inargs,
       outargs = outargs,
       temps = temps,
       ftemps = ftemps
       }
    fun set_finargs ({ use_fp , finargs = _  , inargs , outargs , temps , ftemps } :stackdesc) finargs =
      {
       use_fp = use_fp,
       finargs = finargs,
       inargs = inargs,
       outargs = outargs,
       temps = temps,
       ftemps = ftemps
       }
    fun set_inargs ({ use_fp , finargs , inargs = _  , outargs , temps , ftemps } :stackdesc) inargs =
      {
       use_fp = use_fp,
       finargs = finargs,
       inargs = inargs,
       outargs = outargs,
       temps = temps,
       ftemps = ftemps
       }
    fun set_outargs ({ use_fp , finargs , inargs , outargs = _  , temps , ftemps } :stackdesc) outargs =
      {
       use_fp = use_fp,
       finargs = finargs,
       inargs = inargs,
       outargs = outargs,
       temps = temps,
       ftemps = ftemps
       }
    fun set_temps ({ use_fp , finargs , inargs , outargs , temps = _  , ftemps } :stackdesc) temps =
      {
       use_fp = use_fp,
       finargs = finargs,
       inargs = inargs,
       outargs = outargs,
       temps = temps,
       ftemps = ftemps
       }
    fun set_ftemps ({ use_fp , finargs , inargs , outargs , temps , ftemps = _  } :stackdesc) ftemps =
      {
       use_fp = use_fp,
       finargs = finargs,
       inargs = inargs,
       outargs = outargs,
       temps = temps,
       ftemps = ftemps
       }

    fun empty_stackdesc () : stackdesc = 
      {use_fp = true,
       finargs = [],
       inargs = [],
       outargs = [],
       ftemps = [],
       temps = []}
      

    fun stack2strings ({use_fp,finargs,inargs, outargs, temps,ftemps}) =
      let
	fun slot2string (r,is32) = 
	  (rstat2string r) :: (if is32 then [] else [" float cont."])
	fun slist2strings s = List.concat (List.map slot2string s)
      in
	"****Stack****" ::
	"----FInargs---" ::
	slist2strings finargs @
	"----Inargs---" ::
	slist2strings inargs @
	"----Float Temps----" ::
	slist2strings ftemps @
	"----Temps----" ::
	slist2strings temps @
	"----Outargs--" ::
	slist2strings outargs @
	"*****End*****" ::
	[]
      end

    fun print_stack sd = app (fn s => (print s;print "\n")) (stack2strings sd)

    fun junk_sstat_b l = 
      let
	fun loop (l,acc) =
	  (case l
	     of [] => acc
	      | b::l => loop (l,(Avail Junk,b)::acc))
      in loop(l,[])
      end

    fun junk_sstat i =
      let
	fun loop (i,acc) =
	  if i = 0 then acc
	  else loop (i-1,(Avail Junk,true)::acc)
      in if i < 0 then error "Negative size to junk_ssat"
	else loop (i,[])
      end

    fun junk_sstat64 i =
      let
	fun loop (i,acc) =
	  if i = 0 then acc
	  else loop (i-2,(Avail Junk,false)::acc)
      in 
	if i < 0 then error "Negative size to junk_sstat64"
	else if i mod 2 = 1 then error "Odd size to junk_sstat64" 
	else loop (i,[])
      end

    fun inarg_offset (sd : stackdesc) i = i - (slist_size (#finargs sd))

    fun empty_stackdesc' (finarg_size,inarg_size,temp_size,ftemp_size,outarg_size) : stackdesc = 
      {
       use_fp  = false,
       finargs  = junk_sstat64 (b2w finarg_size),
       inargs  = junk_sstat (b2w inarg_size),
       outargs = junk_sstat (b2w outarg_size),
       ftemps  = junk_sstat64 (b2w ftemp_size),
       temps   = junk_sstat (b2w temp_size)
       }

    fun reset_stackdesc {use_fp,finargs,inargs,outargs,ftemps,temps} : stackdesc = 
      {
       use_fp  = use_fp,
       finargs  = junk_sstat64 (slist_size finargs),
       inargs  = junk_sstat (slist_size inargs),
       outargs = junk_sstat (slist_size outargs),
       ftemps  = junk_sstat64 (slist_size ftemps),
       temps   = junk_sstat (slist_size temps)
       }
      
    local
      (* Set the ith slot, adding slots as necessary.
       * I think that alloc_slot should guarantee that this
       * never needs to add slots.
       *)
      fun set_sstat_slot32 slist i s = 
	(case (slist,i) 
	   of ([],_) => error "set_sstat_slot32:Empty slist" (*set_sstat_slot (junk_sstat (i + 1)) i s*)
	    | ((ss,true)::slist,0) => (s,true)::slist
	    | ((ss,false)::slist,0) => (s,true)::(Avail Junk,true)::slist
	    | ((ss,false)::slist,1) => (Avail Junk,true)::(s,true)::slist
	    | ((ss,true)::slist,i) => (ss,true)::set_sstat_slot32 slist (i-1) s
	    | ((ss,false)::slist,i) => (ss,false)::set_sstat_slot32 slist (i-2) s)

      fun set_sstat_slot64 slist i s = 
	(case (slist,i) 
	   of ([],_) => error "set_sstat_slot64:Empty slist" (*set_sstat_slot (junk_sstat (i + 1)) i s*)
	    | ((ss1,true)::(ss2,true)::slist,0) => (s,false)::slist
	    | ((ss1,true)::(ss2,false)::slist,0) => (s,false)::(Avail Junk,true)::slist
	    | ((ss,false)::slist,0) => (s,false)::slist
	    | ((ss,false)::slist,1) => set_sstat_slot64 ((Avail Junk,true)::(Avail Junk,true)::slist) 1 s
	    | ((ss,true)::slist,i) => (ss,true)::set_sstat_slot64 slist (i-1) s
	    | ((ss,false)::slist,i) => (ss,false)::set_sstat_slot64 slist (i-2) s)


      fun set_xxx_slot set_slot (get_slist,set_slist) stackdesc i s = 
	let
	  val slist = get_slist stackdesc
	  val slist = ((set_slot slist i s) handle any => (print "Setting slot: ";print (Int.toString i);print "\n";
							   print_stack stackdesc;raise any))
	  val stackdesc = set_slist stackdesc slist
	in stackdesc
	end
      
      val set_temp_slot = set_xxx_slot set_sstat_slot32 (#temps,set_temps)
      val set_inarg_slot = set_xxx_slot set_sstat_slot32 (#inargs,set_inargs)
      val set_outarg_slot = set_xxx_slot set_sstat_slot32 (#outargs,set_outargs)

      val set_temp64_slot = set_xxx_slot set_sstat_slot64 (#ftemps,set_ftemps)
      val set_inarg64_slot = set_xxx_slot set_sstat_slot64 (#finargs,set_finargs)
      val set_outarg64_slot = set_xxx_slot set_sstat_slot64 (#outargs,set_outargs)
	
      fun get_sstat_slist slist i = 
	(case (slist,i)
	   of ([],_) => error "get_sstat_slist on empty list"
	    | ((s,true)::_,0) => s
	    | ((s,false)::_,0) => error "Not a 32 bit slot"
	    | ((s,false)::slist,1) => error "Index into middle of 64 bit slot"
	    | ((s,true)::slist,i) => get_sstat_slist slist (i-1)
	    | ((s,false)::slist,i) => get_sstat_slist slist (i-2))


	
      fun get_sstat64_slist slist i = 
	(case (slist,i)
	   of ([],_) => error "get_sstat64_slist on empty list"
	    | ((Avail Junk,true)::(Avail Junk,true)::_,0) =>  Avail Junk
	    | ((s,true)::_,0) =>  error "Not a 64 bit slot"
	    | ((s,false)::_,0) => s
	    | ((s,false)::slist,1) => error "Index into middle of 64 bit slot"
	    | ((s,true)::slist,i) => get_sstat64_slist slist (i-1)
	    | ((s,false)::slist,i) => get_sstat64_slist slist (i-2))

    in
	  
      (* Overwrite a slot with new info *)    
      fun set_sstat (stackdesc : stackdesc) (slot : slot) (s : rstat) : stackdesc = 
	(case slot
	   of Temp i => set_temp_slot stackdesc i s
	    | Inarg i => set_inarg_slot stackdesc (inarg_offset stackdesc i) s
	    | Outarg i => set_outarg_slot stackdesc i s
	    | FTemp i => set_temp64_slot stackdesc i s
	    | FInarg i => set_inarg64_slot stackdesc i s
	    | FOutarg i => set_outarg64_slot stackdesc i s)
	   
      (* Get slot info *)    
      fun get_sstat (stackdesc : stackdesc) (slot : slot) : rstat = 
	(case slot
	   of Temp i => get_sstat_slist (#temps stackdesc) i 
	    | Inarg i => get_sstat_slist (#inargs stackdesc) (inarg_offset stackdesc i)
	    | Outarg i => get_sstat_slist (#outargs stackdesc) i
	    | FTemp i => get_sstat64_slist (#ftemps stackdesc) i 
	    | FInarg i => get_sstat64_slist (#finargs stackdesc) i 
	    | FOutarg i => get_sstat64_slist (#outargs stackdesc) i)
	   handle any => (print "Problem with sstat for slot ";print_slot slot;print "\n";
			  print "Stack is:\n";
			  print_stack stackdesc;
			  raise any)
    end
  
    local
      
      fun alloc_slist_slot grow slist = 
	let
	  fun loop (rhd,sl,i) =
	    (case sl
	       of [] => if grow then (slist @ [(Avail Junk,true)],i) else raise FrameTooSmall
		| (Avail Junk,true)::sl => (slist,i)
		| (Avail Junk,false)::sl => 
		 let
		   val slist = List.revAppend (rhd,(Avail Junk,true)::(Avail Junk,true)::sl)
		 in (slist,i)
		 end
		| (a as (_,true))::sl => loop (a::rhd,sl,i+1)
		| (a as (_,false))::sl => loop (a::rhd,sl,i+2))
	in loop ([],slist,0)
	end

      fun alloc64_slist_slot grow slist = 
	let
	  fun loop (rhd,sl,i) =
	    (case sl
	       of [] => if grow then (slist @ [(Avail Junk,false)],i) else raise FrameTooSmall
		| (Avail Junk,false)::sl => (slist,i)
		| (Avail Junk,true)::(Avail Junk,true)::sl => 
		 let
		   val slist = List.revAppend (rhd,(Avail Junk,false)::sl)
		 in (slist,i)
		 end
		| (a as (_,true))::sl => loop (a::rhd,sl,i+1)
		| (a as (_,false))::sl => loop (a::rhd,sl,i+2))
	in loop ([],slist,0)
	end
      
      
      fun error (nm : string) (sd : stackdesc) = 
	(print "ERROR growing stack ";print nm; print ": stack is \n";
	 print_stack sd;
	 print "\n";
	 raise FrameTooSmall)
      fun alloc_slot (nm,get_slist,set_slist,alloc_slot',growfn,constr) (sd : stackdesc) : stackdesc * slot = 
	let
	  val slist = get_slist sd
	  val grow = growfn sd
	  val (slist,idx) = (alloc_slot' grow slist) handle FrameTooSmall => error nm sd
	  val sd = set_slist sd slist
	in (sd,constr idx)
	end

    in
      val alloc_temp_slot   = alloc_slot ("temp",#temps,  set_temps,  alloc_slist_slot,#use_fp,Temp)
(*      val alloc_inarg_slot  = alloc_slot (#inargs, set_inargs, alloc_slist_slot,#use_fp,Inarg) *)
      val alloc_outarg_slot = alloc_slot ("outargs",#outargs,set_outargs,alloc_slist_slot,fn _ => true,Outarg)
(*
      val alloc_inarg_slot'  = alloc_slot (#inargs, set_inargs, alloc_slist_slot',#use_fp,Inarg)
*)
      val alloc_temp64_slot   = alloc_slot ("ftemps",#ftemps,  set_ftemps,  alloc64_slist_slot,#use_fp,FTemp)
(*      val alloc_inarg64_slot  = alloc_slot (#inargs, set_inargs, alloc64_slist_slot,#use_fp,FInarg) *)
      val alloc_outarg64_slot = alloc_slot ("outargs",#outargs,set_outargs,alloc64_slist_slot,fn _ => true,FOutarg)
    end
  
    (* Translate offset from bottom of frame region
     * into offset from top of stack (stack ptr) 
     *)
    fun slot2sp_offset ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) (slot : slot) : Tal.int32 = 
      let
	val ol = slist_size outargs
	val tl = slist_size temps
	val fl = slist_size ftemps
	val il = slist_size inargs
	val ifl = slist_size finargs
	val (prefix,i,sz) =
	  (case slot
	     of FInarg i  => (ol + tl + fl + il + ifl,i,2)
	      | Inarg i   => (ol + tl + fl + il + ifl,i,1)
	      | FTemp i   => (ol + tl + fl,i,2)
	      | Temp i    => (ol + tl,i,1)
	      | Outarg i  => (ol,i,1)
	      | FOutarg i => (ol,i,2))
	val offset = w2b (prefix - (i + sz))
      in offset
      end


    val fp = Name.internal_label "fp"
    val tptr = Name.internal_label "temp_fp"
    val fptr = Name.internal_label "ftemp_fp"

    fun sp_plus i = Tal.Prjr ((Tal.Esp,[]),i,NONE)
    fun fp_minus fp i = Tal.Prjr ((Tal.Virt fp,[]),0w0 - (w2b i),NONE)

    fun slot2virt  (sd : stackdesc) (slot : slot) : Tal.genop = 
      (case slot
	 of Outarg _  => sp_plus (slot2sp_offset sd slot)
	  | FOutarg _ => sp_plus (slot2sp_offset sd slot)
	  | Temp i    => fp_minus tptr i
	  | FTemp i   => fp_minus fptr i
	  | Inarg i   => fp_minus fp i
	  | FInarg i  => fp_minus fp i)

    fun slot2genop (sd : stackdesc) (slot : slot) : Tal.genop = 
      if #use_fp sd then slot2virt sd slot
      else sp_plus (slot2sp_offset sd slot)

    (* Translate offset from top of stack (stack ptr)
     * into offset from bottom of a specific area.
     *)
    fun sp_offset2slot32 (sd as {use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) (w : Tal.int32) : slot = 
      let
	val i = b2w w
	val ol = slist_size outargs
	val tl = slist_size temps
	val fl = slist_size ftemps
	val il = slist_size inargs
	val ifl = slist_size finargs
      in 
	if i < ol then Outarg (ol - (i + 1))
	else if i < (ol + tl) then Temp ((ol + tl) - (i + 1))
	else if i < (ol + tl + fl) then error "Bad place for FTemp"
	else if i < (ol + tl + fl + il) then Inarg ((ol + tl + fl + il) - (i + 1) + ifl)
	     else (print "Offset ";print (Int.toString i);
		   print " too large\n";
		   print_stack sd;
		   error "Bad offset")
      end

    (* Translate offset from top of stack (stack ptr)
     * into offset from bottom of a specific area.
     *)
    fun sp_offset2slot64 (sd as {use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) (w : Tal.int32) : slot = 
      let
	val i = b2w w
	val ol = slist_size outargs
	val tl = slist_size temps
	val fl = slist_size ftemps
	val il = slist_size inargs
	val ifl = slist_size finargs
      in 
	if i < ol then FOutarg (ol - (i + 2))
	else if i < (ol + tl) then error "Bad place for Temp"
	else if i < (ol + tl + fl) then FTemp ((ol + tl + fl) - (i + 2))
	else if i < (ol + tl + fl + il + ifl) then FInarg ((ol + tl + fl + il + ifl) - (i + 2))
	     else (print "Offset ";print (Int.toString i);
		   print " too large\n";
		   print_stack sd;
		   error "Bad offset")
      end


    fun genop2slot32 (sd : stackdesc) gop = 
      (case gop
	 of Tal.Prjr ((Tal.Esp,[]),w,NONE) => sp_offset2slot32 sd w
	  | Tal.Prjr ((Tal.Virt id,[]),w,NONE) => 
	   let
	     val i = b2w (0w0 - w)
	   in 
	     if Name.eq_label (id,fp) then Inarg i
	     else if Name.eq_label (id,tptr) then Temp i
	     else if Name.eq_label (id,fptr) then error "Virt: Not a 32 bit slot"
             else error "Bad virtual register"
	   end
	  | _ => error "Not a slot")

    fun genop2slot64 (sd : stackdesc) gop = 
      (case gop
	 of Tal.Prjr ((Tal.Esp,[]),w,NONE) => sp_offset2slot64 sd w
	  | Tal.Prjr ((Tal.Virt id,[]),w,NONE) => 
	   let
	     val i = b2w (0w0 - w)
	   in 
	     if Name.eq_label (id,fp) then FInarg i
	     else if Name.eq_label (id,fptr) then FTemp i
	     else if Name.eq_label (id,tptr) then error "Virt: Not a 64 bit slot"
             else error "Bad virtual register"
	   end
	  | _ => error "Not a slot")


    fun sd_cons env (rs,sz) st =
      let
	(* Lil types may have been refined, so try to use the context for Lil vars.
	 * "Special" vars used by the translation won't be in the context, but
	 * also won't be refined. 
	 *)
	fun var32_type (c,x) = 
	  ((TE.find_talvar32 LTC.ctrans (env,x))
	   handle LC.Unbound _ => 
	     (case c
		of LilCon c => (warn ("Using lilcon in stack for var32 "^(Name.var2string x));LTC.ctrans env c)
		 | TalCon c => c))
	fun var64_type (c,x) = 
	  ((TE.find_talvar64 LTC.ctrans (env,x))
	   handle LC.Unbound _ => 
	     (case c
		of LilCon c => (warn ("Using lilcon in stack for var64 "^(Name.var2string x));LTC.ctrans env c)
		 | TalCon c => c))

      in
	case (rstat2rcont rs,sz)
	  of (Full a,true) => Tal.ccons (var32_type a) st
	   | (Full a,false) => Tal.ccons (var64_type a) st
	   | (Junk,true) => Tal.ccons (Tal.pcjunk4) st
	   | (Junk,false) => Tal.ccons (Tal.pcjunkbytes Tal.B8) st
      end
	 
    fun typeof_slist env slist =
      let
	fun loop (slist,acc) = 
	  (case slist
	     of [] => acc
	      | sd::slist => loop(slist,sd_cons env sd acc))
      in loop (slist,Tal.cempty)
      end
    

    fun typeof_stack ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) (env : TE.env) : Tal.con = 
      let
	val fbot = typeof_slist env finargs
	val bot = typeof_slist env inargs
	val fmid = typeof_slist env ftemps
	val mid = typeof_slist env temps
	val top = typeof_slist env outargs
      in Tal.cappend top (Tal.cappend mid (Tal.cappend fmid (Tal.cappend bot fbot)))
      end
    
    fun get_frame_size ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) : Tal.int32  = w2b ((slist_size outargs) + (slist_size temps) + (slist_size ftemps) + (slist_size inargs) + (slist_size finargs))
    fun get_inarg_size ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) : Tal.int32  = w2b (slist_size inargs + slist_size finargs)
    fun get_inarg32_size ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) : Tal.int32  = w2b (slist_size inargs)
    fun get_inarg64_size ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) : Tal.int32  = w2b (slist_size finargs)
    fun get_temp_size ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) : Tal.int32   = w2b (slist_size temps)
    fun get_temp64_size ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) : Tal.int32 = w2b (slist_size ftemps)
    fun get_outarg_size ({use_fp,finargs,inargs,outargs,temps,ftemps} : stackdesc) : Tal.int32 = w2b (slist_size outargs)
      
    local
	fun count_slist slist = List.length slist

    in
      fun get_frame_elt_count ({inargs,finargs,temps,ftemps,...} : stackdesc) : int =
	count_slist inargs + count_slist finargs + count_slist temps + count_slist ftemps

      fun get_outarg_count ({outargs,...} : stackdesc) : int = count_slist outargs
      fun get_inarg_count ({inargs,finargs,...} : stackdesc) : int = (count_slist inargs) + (count_slist finargs)
      fun get_inarg32_count ({inargs,...} : stackdesc) : int = count_slist inargs
      fun get_inarg64_count ({finargs,...} : stackdesc) : int = count_slist finargs
      fun get_temp64_count ({ftemps,...} : stackdesc) : int = count_slist ftemps

    end
    (*	fun pop_frame ({inargs,outargs,temps} : stackdesc) : stackdesc = {inargs = inargs,outargs = [],temps = []}*)
      
    fun get_outargs (sd : stackdesc) = #outargs sd
    fun pop_outargs (sd : stackdesc) = set_outargs sd []

    (* Description lists are stack-top to stack-bottom. *)
    fun alloc_outarg_space (sd : stackdesc) (l : bool list) = (set_outargs sd (junk_sstat_b l))

    fun alloc_inarg_space (sd : stackdesc) num32 num64 = 
      let
	val sd = set_inargs sd (junk_sstat num32)
	val sd = set_finargs sd (junk_sstat64 (2*num64))
      in sd
      end

    fun adjust_inargs (sd as {inargs,finargs,...} : stackdesc) num32 num64 = 
      let
	val cur32 = List.length inargs
	val cur64 = List.length finargs
	val () = 
	  if cur32 + 2*cur64 <> num32 + 2*num64 then
	    (print "num32 is: ";print (Int.toString num32);print "\n";
	     print "num64 is: ";print (Int.toString num64);print "\n";
	     print "cur32 is: ";print (Int.toString cur32);print "\n";
	     print "cur64 is: ";print (Int.toString cur64);print "\n";
	     error "adjust_inargs: mismatched sizes")
	  else ()
	val (finargs,inargs) = 
	  if cur64 > num64 then
	    let 
	      val finargs = List.take (finargs,num64)
	      val inargs = (junk_sstat (num32 - cur32))@inargs
	    in (finargs,inargs)
	    end
	  else if cur64 < num64 then 
	    let 
	      val inargs = List.drop (inargs,cur32 - num32)
	      val finargs = (junk_sstat64 (2*(num64 - cur64)))@finargs
	    in (finargs,inargs)
	    end
	       else (finargs,inargs)
	val sd = set_inargs sd inargs
	val sd = set_finargs sd finargs
      in sd
      end

    local 
      fun grow_slist mkjnk (slist : slist) len = 
	let
	  val slen = slist_size slist
	in  
	  if slen < len then slist @ (mkjnk (len - slen))
	  else slist
	end
    in 
      fun grow_inarg32s (sd : stackdesc) i = set_inargs sd (grow_slist junk_sstat (#inargs sd) (b2w i))
      fun grow_inarg64s (sd : stackdesc) i = set_finargs sd (grow_slist junk_sstat64 (#finargs sd) (b2w i))
      fun grow_temps (sd : stackdesc) i = set_temps sd (grow_slist junk_sstat (#temps sd) (b2w i))
      fun grow_ftemps (sd : stackdesc) i = set_ftemps sd (grow_slist junk_sstat64 (#ftemps sd) (b2w i))
      fun grow_outargs (sd : stackdesc) i = set_outargs sd (grow_slist junk_sstat (#outargs sd) (b2w i))
    end

    fun countidx2wordidx slist count = 
      let
	fun loop (slist,count,idx) = 
	  if count = 0 then idx else
	    (case slist
	       of [] => error "count out of range"
		| (_,true)::slist => loop(slist,count-1,idx + 1)
		| (_,false)::slist => loop(slist,count-1,idx + 2))
      in loop(slist,count,0)
      end
    fun outarg_slot_i   (sd : stackdesc) i = Outarg (countidx2wordidx (#outargs sd) i)
    fun outarg64_slot_i (sd : stackdesc) i = FOutarg(countidx2wordidx (#outargs sd) i)
    fun inarg_slot_i    (sd : stackdesc) i = Inarg  (i + (slist_size (#finargs sd)))
    fun inarg64_slot_i  (sd : stackdesc) i = FInarg (i*2)
    fun temp_slot_i   (sd : stackdesc) i = Temp i
    fun temp64_slot_i (sd : stackdesc) i = FTemp (i*2)

    val slot2offset = slot2sp_offset

  end (* structure StackDesc *)
