(*$import Nil *)
signature REDUCE =
    sig 
	val debug : bool ref 
	val doModule :  Nil.module -> Nil.module

	(* After the pass is complete, it will be possible to get the collected 
	 information only once. All transformations which wish to use this 
	 information should be bundled together in one pass occurring after reduce. 
	 
	 For each var the three refs are :    
	 1 - app - Counts the number of times a variable appears
	     in the function position 
	 2 - esc - Counts the number of times a variable appears
	     as an argument 
	 3 - recur - Counts the number of times a variable appears in
	     a recursive call or esc (only valid for function
	     names) *)

	type info
	val getInformationTable : unit -> 
	    ( (Nil.var, info) HashTable.hash_table ) option
	val count_app : info -> int ref
	val count_esc : info -> int ref
	val count_rec_app : info -> int ref
	val count_rec_esc : info -> int ref 
	val exp_isval : Nil.exp -> bool

    end
