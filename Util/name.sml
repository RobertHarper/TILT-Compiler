functor Name(structure Util : UTIL)
  : NAME = 
  struct

    structure Util = Util
    open Util

    val error = error "Name.sml"

    datatype var   = GVAR of int * string
    datatype label = GLABEL of int * string * bool
                   | GBAR   of Symbol.symbol * bool
    type   labels = label list
    datatype path = SIMPLE_PATH   of var 
                  | COMPOUND_PATH of var * labels
    datatype loc  = GLOC of int
    datatype tag  = GTAG of int * string

    fun is_label_open (GLABEL (_,_,flag)) = flag
      | is_label_open (GBAR (_,flag)) = flag
    fun is_label_barred (GLABEL _) = false
      | is_label_barred (GBAR _) = true
    fun is_label_internal (GLABEL _) = true
      | is_label_internal (GBAR _) = false

    (* equality and generation functions on Nameitive types *)
    fun eq_var   (GVAR v1, GVAR v2)     = v1 = v2
    fun eq_label (GLABEL l1, GLABEL l2) = l1 = l2
      | eq_label (GBAR l1, GBAR l2)     = l1 = l2
      | eq_label _ = false
    fun eq_tag   (GTAG n1, GTAG n2)     = n1 = n2
      
    fun make_counter() = 
      let
	val counter = ref 0
	fun inc() = (counter := (!counter) + 1; !counter)
      in inc
      end

    val var_counter = make_counter()
    val label_counter = make_counter()
    val tag_counter = make_counter()

    fun fresh_named_var s = GVAR(var_counter(),s)
    fun fresh_named_int_label s = GLABEL(label_counter(),s,false)
    fun fresh_named_open_label s = GLABEL(label_counter(),s,true)
    fun fresh_named_tag s = GTAG(tag_counter(),s)
      
    fun fresh_var   () = fresh_named_var "v"
    fun fresh_int_label () = fresh_named_int_label "i"
    fun fresh_open_label () = fresh_named_open_label "i"
    fun fresh_tag  () = fresh_named_tag "t"
      
    fun openlabel (GLABEL (i,s,_)) = GLABEL(i,s,true)
      | openlabel (GBAR (sym,_)) = GBAR(sym,true)
    fun symbol2label (s : Symbol.symbol) = GBAR(s, false)
    fun gen_var_from_symbol (s : Symbol.symbol) = fresh_named_var(Symbol.name s)

    (* these values copied from NJ source env/env.sml *)
    val varInt = 0 and sigInt = 1 and strInt = 2 and fsigInt = 3 and 
      fctInt = 4 and tycInt = 5 and labInt = 6 and tyvInt = 7 and
      fixInt = 8
    fun namespace2int Symbol.VALspace = 0
      | namespace2int Symbol.SIGspace = 1
      | namespace2int Symbol.STRspace = 2
      | namespace2int Symbol.FSIGspace = 3
      | namespace2int Symbol.FCTspace = 4
      | namespace2int Symbol.TYCspace = 5
      | namespace2int Symbol.LABspace = 6
      | namespace2int Symbol.TYVspace = 7
      | namespace2int Symbol.FIXspace = 8

    fun var2string (GVAR(i,s)) = (s ^ "_" ^ (makestring i))
    fun label2string l =
      let
	fun help false s = s
	  | help true s = "*" ^ s
      in (case l of
	    (GLABEL (i,s,f)) => help f (s ^ "__" ^ (makestring i))
	  | (GBAR (sym,f)) => help f (Symbol.name sym ^ "_" ^ (makestring 
							       (namespace2int (Symbol.nameSpace sym)))))
      end
    fun loc2string (GLOC i) = ("LOC_" ^ (makestring i))
    fun tag2string (GTAG (i,s)) = ("NAME_" ^ s ^ "_" ^ (makestring i))



  end
