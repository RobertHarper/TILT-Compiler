structure Name : NAME =
  struct

    structure Util = Util
    open Util

    val error = fn s => error "Name.sml" s

    datatype var   = GVAR of int * string
    datatype label = GLABEL of int * string * bool  (* <-- bool indicates openness *)
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
    fun compare_var (GVAR(a,_),GVAR(b,_)) = Int.compare(a,b)
    fun compare_tag (GTAG(a,_),GTAG(b,_)) = Int.compare(a,b)
    fun compare_label(GLABEL _, GBAR _) = LESS
      | compare_label(GBAR _, GLABEL _) = GREATER
      | compare_label(GLABEL (a,sa,_), GLABEL (b,sb,_)) = 
	(case Int.compare(a,b) of
	     EQUAL => String.compare(sa,sb)
	   | res => res)
      | compare_label(GBAR (s1,_), GBAR (s2,_)) = 
	if (Symbol.symbolCMLt(s1,s2))
	    then LESS
	else if (Symbol.eq(s1,s2))
		 then EQUAL
	     else GREATER

    fun make_counter() = 
      let
	val counter = ref 0
	fun inc() = (counter := (!counter) + 1; !counter)
      in inc
      end

    val var_counter = make_counter()
    val tag_counter = make_counter()
    val label_counter = make_counter()

    fun fresh_named_var s = GVAR(var_counter(),s)
    fun fresh_named_tag s = GTAG(tag_counter(),s)
    fun fresh_var   () = fresh_named_var "v"
    fun fresh_tag  () = fresh_named_tag "t"
    fun gen_var_from_symbol v = GVAR(var_counter(), Symbol.name v)

    fun fresh_named_int_label s = GLABEL(0,s,false)
    fun fresh_named_open_label s = GLABEL(0,s,true)
    fun fresh_int_label () = fresh_named_int_label "i"
    fun fresh_open_label () = fresh_named_open_label "i"

    fun symbol_label (s : Symbol.symbol) = GBAR(s, false)
    fun open_symbol_label (s : Symbol.symbol) = GBAR(s, true)
    fun internal_label (s : string) = GLABEL(0,s, false)
    fun fresh_internal_label (s : string) = GLABEL(label_counter(),s, false)
    fun open_internal_label (s : string) = GLABEL(0,s, true)
    fun fresh_open_internal_label (s : string) = GLABEL(label_counter(),s,true)
    fun openlabel (GLABEL (i,s,_)) = GLABEL(i,s,true)
      | openlabel (GBAR (sym,_)) = GBAR(sym,true)


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

    fun var2string (GVAR(i,s)) = (s ^ "_" ^ (Int.toString i))
    fun label2string l =
      let
	fun help false s = s
	  | help true s = "*" ^ s
      in (case l of
	    (GLABEL (0,s,f)) => help f (s ^ "_INT")
	  | (GLABEL (i,s,f)) => help f (s ^ "_INT(" ^ (Int.toString i) ^ ")")
	  | (GBAR (sym,f)) => help f (Symbol.name sym ^ 
				      (case (namespace2int (Symbol.nameSpace sym)) of
					   0 => ""
					 | x => "_" ^ (Int.toString x))))
      end
    fun loc2string (GLOC i) = ("LOC_" ^ (Int.toString i))
    fun tag2string (GTAG (i,s)) = ("NAME_" ^ s ^ "_" ^ (Int.toString i))


    fun mk_var_hash_table (size,notfound_exn) = 
	let
	    val b : word = 0wx3141592
	    fun hash (GVAR(i,_)) = 
		Word31.>>(Word31.*(Word31.fromInt i,b),0wx12)
	    fun eqKey (GVAR(i,_),GVAR(j,_)) = i=j
	in HashTable.mkTable (hash,eqKey) (size,notfound_exn)
	end

      structure VarKey : ORD_KEY = struct
				    type ord_key = var
				    val compare = compare_var
				end
      structure LabelKey : ORD_KEY = struct
					 type ord_key = label
					 val compare = compare_label
				     end
      structure TagKey : ORD_KEY = struct
					 type ord_key = tag
					 val compare = compare_tag
				     end
      structure VarMap = LocalSplayMapFn(VarKey) 
      structure LabelMap = LocalSplayMapFn(LabelKey) 
      structure TagMap = LocalSplayMapFn(TagKey) 

  end
