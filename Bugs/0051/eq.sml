(*$import Eqsetup *)

type 'a bindparm = {fromVar  : Il.var -> 'a}
fun bind (parm : 'a bindparm) = ()