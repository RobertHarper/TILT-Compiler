signature TALABBREV = 
  sig
    val register_global : Lil.var -> unit
    val reset : unit -> unit

    structure K : ABBREV where type identifier = Tal.identifier
			   and type index = Lil.kind
			   and type result = Tal.kind

    structure C : ABBREV where type identifier = Tal.identifier
			   and type index = Lil.con
			   and type result = Tal.con

    structure CS : ABBREV where type identifier = Tal.identifier
			   and type index = Lil.con
			   and type result = Tal.con
  end
