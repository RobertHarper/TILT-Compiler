(*$import *)

functor F (type var) =
struct
    type 'a bindparm = {fromVar  : var -> 'a}
    fun bind (parm : 'a bindparm) = ()
end
