structure TalAbbrev :> TALABBREV = 
  struct

    val globals : Name.VarSet.set ref = ref Name.VarSet.empty
    fun reset_globals () = globals := Name.VarSet.empty
    fun register_global a = globals := Name.VarSet.add(!globals,a)

    fun varset_e s = Name.VarSet.numItems s = 0
    fun closed_kind k = varset_e (Lil.free_kvars_kind k)

    fun closed_con c = 
      varset_e (Lil.free_kvars_con c) andalso 
      varset_e (Name.VarSet.difference(Lil.free_cvars_con c,!globals))

    val kabbrevs : (Tal.identifier * Tal.kind) list ref = ref []
    val cabbrevs : (Tal.identifier * Tal.con) list ref = ref []

    structure K = AbbrevFn(type identifier = Tal.identifier
			   type index = Lil.kind
			   type result = Tal.kind
			   val abbrevs = kabbrevs
			   val closed = closed_kind
			   val newid = Name.fresh_internal_label
			   val id2res = Tal.kvar
			   structure ISet = Name.LabelSet
			   structure Map = Lil.KindMap)

    structure C = AbbrevFn(type identifier = Tal.identifier
			   type index = Lil.con
			   type result = Tal.con
			   val abbrevs = cabbrevs
			   val closed = closed_con
			   val newid = Name.fresh_internal_label
			   val id2res = Tal.cvar
			   structure ISet = Name.LabelSet
			   structure Map = Lil.ConMap)

    structure CS = AbbrevFn(type identifier = Tal.identifier
			    type index = Lil.con
			    type result = Tal.con
			    val abbrevs = cabbrevs
			    val closed = closed_con
			    val newid = Name.fresh_internal_label
			    val id2res = Tal.cvar
			    structure ISet = Name.LabelSet
			    structure Map = Lil.ConMap)

    fun reset () = (K.reset();C.reset();CS.reset();reset_globals())
  end
