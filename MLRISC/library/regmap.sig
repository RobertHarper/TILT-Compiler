signature REGISTER_MAP =
sig

   type regmap 
   type value
   type reg = int

   val empty          : regmap
   val fromList       : (reg * value) list -> regmap
   val fromSortedList : (reg * value) list -> regmap
   val insert         : regmap * reg * value -> regmap 
   val remove         : regmap * reg -> regmap 
   val ==             : regmap * regmap -> bool
   val app            : (reg * value -> unit) -> regmap -> unit
   val contains       : regmap * reg -> bool
   val exists         : regmap * reg list -> bool
   val isEmpty        : regmap -> bool
   val toList         : regmap -> (reg * value) list
   val toString       : regmap -> string
   val union          : regmap list -> regmap
   val intersects     : regmap list -> regmap
   val +              : regmap * regmap -> regmap
   val *              : regmap * regmap -> regmap

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:21  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:10  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:59  pscheng
# *** empty log message ***
#
 *)
