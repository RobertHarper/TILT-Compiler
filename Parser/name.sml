structure Name :> NAME =
  struct

    structure Util = Util
    open Util

    val error = fn s => error "Name.sml" s

    type var   = int * string

    type label = int * string * bool
    type   labels = label list
    datatype path = SIMPLE_PATH   of var 
                  | COMPOUND_PATH of var * labels
    datatype loc  = GLOC of int
    datatype tag  = GTAG of int * string

    fun is_label_open ((_,_,flag) : label) = flag

    (* equality and generation functions on Nameitive types *)
    fun eq_var   (v1 : var, v2)     = v1 = v2
    fun eq_label (l1 as (h1,_,_) : label, l2 as (h2,_,_)) = 
		let val res = (h1=h2) andalso (l1=l2)
(*			val _ = Stats.counter ("eq_label=" ^ 
				(Bool.toString res)) () *)
		in  res
		end
    fun eq_tag   (GTAG n1, GTAG n2)     = n1 = n2
    fun compare_var ((a,_) : var,(b,_) : var) = Int.compare(a,b)
    fun compare_tag (GTAG(a,_),GTAG(b,_)) = Int.compare(a,b)
    fun compare_label((a,sa,_) : label, (b,sb,_) : label) = 
	(case Int.compare(a,b) of
	     EQUAL => String.compare(sa,sb)
	   | res => res)

    fun make_counter() = 
      let
	val counter = ref 0
	fun inc() = (counter := (!counter) + 1; !counter)
      in inc
      end

    val var_counter = make_counter()
    val tag_counter = make_counter()
    val label_counter = make_counter()

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
    val maxnamespace = 8
    fun namespaceint (hash,str) = hash - (Symbol.number(Symbol.varSymbol str))

    fun fresh_named_var s : var = (var_counter(),s)
    fun fresh_named_tag s = GTAG(tag_counter(),s)
    fun fresh_var   () = fresh_named_var "v"
    fun fresh_tag  () = fresh_named_tag "t"
    fun gen_var_from_symbol v : var = (var_counter(), Symbol.name v)
    fun internal_hash s = Symbol.number(Symbol.varSymbol s) + maxnamespace + 1
    fun internal_label s : label = (internal_hash s,s,false)
    fun open_internal_label s : label = (internal_hash s,s,true)

    fun symbol_label s = (Symbol.number s, Symbol.name s, false)
    fun open_symbol_label s = (Symbol.number s, Symbol.name s, true)
    fun fresh_string s = Int.toString(label_counter()) ^ "_" ^ s
    fun fresh_internal_label s = internal_label(fresh_string s)
    fun fresh_open_internal_label s = open_internal_label(fresh_string s)
    fun openlabel ((i,s,_) : label) = (i,s,true)



    fun var2string ((i,s) : var) = (s ^ "_" ^ (Int.toString i))
    fun label2string (num,str,flag) =
      let
	fun help s = if flag then "*" ^ s else s
        val is_internal = internal_hash str = num
        val is_generative = Char.isDigit(String.sub(str,0))
        val space = namespaceint(num,str) 
      in (case (is_internal,is_generative) of
	    (true,false) => help (str ^ "_INT")
	  | (true,true) => help (str ^ "_INT_GEN")
	  | (false,_) => help (str ^ (case space of
					   0 => ""
					 | x => "_" ^ (Int.toString x))))
      end
    fun loc2string (GLOC i) = ("LOC_" ^ (Int.toString i))
    fun tag2string (GTAG (i,s)) = ("NAME_" ^ s ^ "_" ^ (Int.toString i))
    fun tag2int (GTAG (i,s)) = i

    fun mk_var_hash_table (size,notfound_exn) = 
	let
	    val b : word = 0wx3141592
	    fun hash ((i,_) : var) = 
		Word31.>>(Word31.*(Word31.fromInt i,b),0wx12)
	    fun eqKey ((i,_) : var, (j,_) : var) = i=j
	in HashTable.mkTable (hash,eqKey) (size,notfound_exn)
	end

      structure VarKey : ORD_KEY = struct
				    type ord_key = var
				    val compare = compare_var
				end
      type vpath = var * label list
      structure PathKey : ORD_KEY = 
	  struct
	      type ord_key = vpath
	      fun compare_labels([],[]) = EQUAL
		| compare_labels ([],_) = LESS
		| compare_labels(_,[]) = GREATER
		| compare_labels(a1::b1,a2::b2) = 
		  (case (compare_label(a1,a2)) of
		       LESS => LESS
		     | GREATER => GREATER
		     | EQUAL => compare_labels(b1,b2))
	      fun compare((v1,l1),(v2,l2)) = 
		  case (compare_var(v1,v2)) of
		      LESS => LESS
		    | GREATER => GREATER
		    | EQUAL => compare_labels(l1,l2)
	  end
      structure LabelKey : ORD_KEY = struct
					 type ord_key = label
					 val compare = compare_label
				     end
      structure TagKey : ORD_KEY = struct
					 type ord_key = tag
					 val compare = compare_tag
				     end
      structure VarSet = SplaySetFn(VarKey) 
      structure VarMap = LocalSplayMapFn(VarKey) 
      structure LabelMap = LocalSplayMapFn(LabelKey) 
      structure TagMap = LocalSplayMapFn(TagKey) 
      structure PathMap = LocalSplayMapFn(PathKey) 
  end
