(*$import Prelude TopLevel Name *)
signature ANNOTATION = 
    sig
      datatype 'a annotation = 
	FREE_VARS of {con_vars:Name.VarSet.set ref,exp_vars:Name.VarSet.set ref} 
      | TYPECHECKED of 'a
      | SUBST_RESULT
    end