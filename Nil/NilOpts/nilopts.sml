structure NilOpts = 
struct 
    val do_cse = ref true
    val do_flatten = ref true
    val do_reduce = ref true
    val do_inline = ref true
    val do_uncurry = ref true
    val do_fold_constants = ref false 

    val print_pre = ref false
    val print_anormalize = ref false
    val print_flatten = ref false
    val print_uncurry = ref false
    val print_inline = ref false
    val print_reduce = ref false 

    val max_name_size = ref 10
    type click = { name : string , last : int ref, total : int ref}
    val clicks = ref [] : (click list) ref

    fun fprint max_size str =
	   let fun loop i = if i < 0 then () else (print " "; loop (i-1))
	   in  print str;
	       loop (max_size - (size str))
	   end

    fun make_click str = 
	let val _ = max_name_size := Int.max(size str, !max_name_size)
	    val click = { name = str, last = ref 0, total = (ref 0) }
	in (  clicks := click :: !clicks ;
	    click )
	end

    fun inc_click { last=r, name=n, total= t } = 
		t := ! t + 1
    fun print_clicks unit = 
	let fun pritem {name = name, last = r, total = count } = 
	    (fprint (!max_name_size) name;
	     print " : ";
	     fprint 8 (Int.toString (!count));
	     print "\n")
	in  
	    print "Optimization results\n";
	    print "-------------------------------------------\n";
	    app pritem (rev(!clicks));
	    print "-------------------------------------------\n"
	end 
	
    fun init_clicks unit =
	app (fn {last = r, total = t, name=str } =>
	     ( t:= 0 ; r := 0) ) (!clicks)
 
    fun round_clicks clicks = 
	foldl  (fn ({last = r, total = t, ... }:click, acc:int) => 
	       ( let val temp = !t - (!r)
		 in ( r := !t ; temp+acc ) end )) 0 clicks 
    fun print_round_clicks clicks = 
	app (fn {name = n, last = r, total = t } =>
	     print (n ^ " :" ^ Int.toString ((!t)-(!r)) ^ "\n") ) clicks

end


