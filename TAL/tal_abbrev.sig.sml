signature TALABBREV = 
  sig
    structure K : ABBREV where type identifier = Tal.identifier
			   and type index = Lil.kind
			   and type result = Tal.kind

    structure C : ABBREV where type identifier = Tal.identifier
			   and type index = Lil.con
			   and type result = Tal.con
  end
