(* Reorder *)

structure Linklil :> LINKLIL  =
  struct

    structure Pplil = PpLil

    val SelfCheck = Stats.tt "LilSelfCheck"
    
    val LinkLilDiag = Stats.ff("LinkLilDiag")
    val show      = Stats.ff "ShowLil"     (* show after each pass *)
    val UpToLil   = Stats.ff "UpToLil"
    val typecheck = Stats.bool "Typecheck"   (* typecheck after each pass *)
    val showtypecheck = Stats.ff "showTypecheck"   (* typecheck after each pass *)
    val report = Stats.tt "HashConsReport"
    val show_collisions = Stats.ff "HashConsShowCollisions"
    val _ = LilSubst.install {pp_con = PpLil.pp_con}
    val _ = if !SelfCheck then SelfCheck.check () else ()


    fun hashreport () = 
      let

	fun max' f i l = 
	  let fun loop ([],i) = i
		| loop (j::r,i) = loop(r,f(i,j))
	  in loop(l,i)
	  end
	val max = max' Int.max 0
	val biggest = max' (fn (a as (_,l1),b as (_,l2)) => if (List.length l1) > (List.length l2) then a else b) (0w0,[])

	fun hist l = 
	  let
	    val max = max l
	    val arr = Array.array(max+1,0)
	    fun inc i = Array.update(arr,i,(Array.sub(arr,i) + 1))
	    val () = app inc l
	  in arr
	  end
	val () = 
	  if !report then
	    let
	      val {csize : int,
		   ksize : int,
		   cbuckets : int list,
		   kbuckets : int list,
		   collisions} = Lil.report()
	      fun print_collision (w,cons_) = 
		let
		  val cons = map Lil.mk_con cons_
		in
		  (print "Hash val = ";print (Word.toString w);print ". Collisions (";print (Int.toString (List.length cons));print") are:\n";
		   PpLil.pp_list PpLil.pp_con' cons ("",",","",true);
		   print "\n")
		end
	      fun print_hists a = 
		Array.appi (fn (i,count) => (print "\t";print (Int.toString i);print " : ";print (Int.toString count);print "\n")) (a,0,NONE)
	    in
	      print "\n";
	      print "Kind table size = ";print (Int.toString ksize);print "\n";
	      print "Con table size = ";print (Int.toString csize);print "\n";
	      print "Kind Bucket max = ";print (Int.toString (max kbuckets));print "\n";
	      print "Con Bucket max = ";print (Int.toString (max cbuckets));print "\n";
	      print "Kind distribution is:\n";print_hists (hist kbuckets);print "\n";
	      print "Con distribution is:\n";print_hists (hist cbuckets);print "\n";
	      if !show_collisions then print_collision (biggest collisions) else ()
	    end
	  else ()
      in()
      end

    fun gc () = 
      let
	val _ = Lil.reset_tables()
      in()
      end


    fun makeEntry (enable, str) = ((if enable then Stats.tt else Stats.ff) ("do" ^ str),
				   Stats.ff("show" ^ str),
				   Stats.ff("check" ^ str),
				   str)

    val niltolil       = makeEntry (true, "NiltoLil")
    val lilclose       = makeEntry (true, "LilClose")
    val optimize1      = makeEntry (true, "LilOptimize1")
    val optimize2      = makeEntry (true, "LilOptimize2")
    val optimize3      = makeEntry (true, "LilOptimize3")
      
    val error = fn s => Util.error "linklil.sml" s

    fun printBold str =
	if (!LinkLilDiag) then
	  (print "===== "; print str; print " =====\n")
	else ()
	  
    fun pass filename (ref false, _, _,_) (transformer,obj) =
      error "pass called with a false flag"
      | pass filename (ref true, showphase, checkphase, phasename) (transformer,obj) =
      let 
	val str = "Starting " ^ phasename ^ ": " ^ filename
	val _ = Timestamp.timestamp()
	val _ = printBold str
	val lilmod = Stats.timer(phasename,transformer) obj
	val () = hashreport()
	val _ = 
	  if !showphase orelse !show then
	    (PpLil.pp_pass
	     {module = lilmod,
	      header = phasename,
	      name = filename,
	      pass = phasename};
	     print "\n")
	  else ()

	val _ = 
	  if !checkphase orelse !typecheck then
	    Stats.timer(phasename^"_Typecheck",LilTypecheck.M.check) lilmod
	  else ()

	val () = hashreport()

	in  lilmod
	end

    fun transform filename (ref false,_,_,phasename) (_,lilmod) =
	let val str = "Skipping " ^ phasename ^ " : " ^ filename
	in  (* printBold str;  *)
	    lilmod
	end
      | transform filename (tr as (ref true,_,_,_)) arg =
	pass filename tr arg
	

    exception Stop of Lil.module

    fun compile' (filename,nilmodule : Nil.module) =
      let
	val pass = pass filename
	val transform = transform filename
	  
	val () = gc()

	val lilmod = pass niltolil (NiltoLil.niltolil, nilmodule)
	  
	val _ = if !UpToLil
		  then raise (Stop lilmod)
		else ()

	val lilmod = transform optimize1 (LilOptimize.optimize {doCse = true},lilmod)

	val () = gc()

	val lilmod = transform lilclose (LilClosure.close_mod, lilmod)
	 
	val lilmod = transform optimize2 (LilOptimize.optimize {doCse = true},lilmod) 

	val lilmod = transform optimize3 (LilOptimize.optimize {doCse = true},lilmod) 

	val () = gc()

      in  lilmod
      end
    handle Stop lilmod => lilmod
      
    val nil_to_lil = compile'

end
