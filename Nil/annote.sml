(*$import Prelude TopLevel Name ANNOTATION *)

structure Annotation : ANNOTATION = 
    struct
      structure VarSet = Name.VarSet
      datatype 'a annotation = 
	FREE_VARS of {con_vars:VarSet.set ref,exp_vars:VarSet.set ref} 
      | TYPECHECKED of 'a
      | SUBST_RESULT
    end