structure TalAbbrev :> TALABBREV = 
  struct
    fun varset_e s = Name.VarSet.numItems s = 0
    fun closed_kind k = varset_e (Lil.free_kvars_kind k)
    fun closed_con c = varset_e (Lil.free_kvars_con c) andalso varset_e (Lil.free_cvars_con c)

    structure K = AbbrevFn(type identifier = Tal.identifier
			   type index = Lil.kind
			   type result = Tal.kind
			   val closed = closed_kind
			   val newid = Name.fresh_named_var
			   val id2res = Tal.kvar
			   structure ISet = Name.VarSet
			   structure Map = Lil.KindMap)

    structure C = AbbrevFn(type identifier = Tal.identifier
			   type index = Lil.con
			   type result = Tal.con
			   val closed = closed_con
			   val newid = Name.fresh_named_var
			   val id2res = Tal.cvar
			   structure ISet = Name.VarSet
			   structure Map = Lil.ConMap)
  end
