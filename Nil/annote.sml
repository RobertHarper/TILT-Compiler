structure Annotation : ANNOTATION = 
    struct
      structure VarSet = Name.VarSet
      datatype 'a annotation = 
	FREE_VARS of {con_vars:VarSet.set ,exp_vars:VarSet.set} 
      | TYPECHECKED of 'a
    end