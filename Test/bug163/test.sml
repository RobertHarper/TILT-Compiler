(*$import *)

    fun seal (i : {main : unit * unit}) : unit =
	let val {main=(v,sig_target),...} = i
	in  ()
	end
