(*$import Name *)
signature ANNOTATION = 
    sig
      datatype 'a annotation = 
	FREE_VARS of {con_vars:Name.VarSet.set ,exp_vars:Name.VarSet.set} 
      | TYPECHECKED of 'a
    end