(*$import NAME Util Symbol Int32 Word31 SplaySetFn SplayMapFn Char String *)

structure Name :> NAME =
  struct

    structure Util = Util
    open Util

    val error = fn s => error "Name.sml" s

    type var   = int
    type label = int * string
    type labels = label list
    type loc  = int
    type tag  = int * string


    (* equality and generation functions on Nameitive types *)
    fun eq_var   (v1 : var, v2)     = v1 = v2
    val eq_var2 = curry2 eq_var
    val compare_var = Int.compare
    structure VarKey : ORD_KEY = struct
				     type ord_key = var
				     val compare = compare_var
				 end
    structure VarSet = SplaySetFn(VarKey) 
    structure VarMap = SplayMapFn(VarKey) 
    val varmap = ref (VarMap.empty : string VarMap.map)
    fun reset_varmap() = varmap := VarMap.empty

    fun eq_label (l1 as (h1,_) : label, l2 as (h2,_)) = 
		let val res = (h1=h2) andalso (l1=l2)
		in  res
		end
    val eq_label2 = curry2 eq_label
    fun eq_tag   (n1, n2)     = n1 = n2
    fun compare_tag ((a,_),(b,_)) = Int.compare(a,b)

    fun compare_label((a,sa) : label, (b,sb) : label) = 
	(case Int.compare(a,b) of
	     EQUAL => String.compare(sa,sb) 
	   | res => res)


    val labels_name_sorted_distinct = all_pairs (fn (l1,l2) => compare_label(l1,l2) = LESS)
    
    (* XXX small variable numbers could be mapped to physical registers at the tortl level *)
    val var_counter = ref 256
    val tag_counter = ref 256
    val label_counter = ref 256

    fun inc_counter counter = 
      let val res = !counter
	  val _ = counter := res + 1
      in res
      end
    fun update_counter counter n = counter := (Int.max(!counter,n + 1))
    val update_var_counter = update_counter var_counter
    val update_label_counter = update_counter label_counter

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


    fun construct_var (i : int, s : string) : var = 
	let val s = if (size s > 0 andalso (Char.isDigit(String.sub(s,0))))
			then "v" ^ s else s
	    val _ = varmap := (VarMap.insert(!varmap,i,s))
	in  i
	end

    fun fresh_named_var str = construct_var(inc_counter var_counter, str)
    fun fresh_var() = inc_counter var_counter
    fun gen_var_from_symbol v : var = fresh_named_var(Symbol.name v)

    fun var2int    (v : var) = v
    fun var2name   (v : var) = (case VarMap.find(!varmap,v) of
				    NONE => ""
				  | SOME str => str)
    fun var2string (v : var) = (var2name v) ^ "_" ^ (Int.toString v)
    fun derived_var v = fresh_named_var(var2name v)
    fun deconstruct_var v = (v, var2name v)


    fun fresh_named_tag s = (inc_counter tag_counter,s)
    fun fresh_tag  () = fresh_named_tag "t"
    fun internal_hash s = Symbol.number(Symbol.varSymbol s) + maxnamespace + 1
    fun internal_label s : label = (internal_hash s,s)
    fun is_label_internal ((num,str) : label) = internal_hash str = num

    fun symbol_label sym : label = 
	let val str = Symbol.name sym
	    val numOpt = Int.fromString str
	    val hash = case numOpt of 
		SOME num => num 
	      | NONE => Symbol.number sym
	in  (hash, str)
	end

    fun fresh_string s = s ^ "_" ^ Int.toString(inc_counter label_counter)
    fun fresh_internal_label s = internal_label(fresh_string s)


    fun label2name ((_,str) : label) = str
    fun label2string ((num,str) : label) =
      let
        val is_internal = internal_hash str = num
        val is_generative = (size str > 0) andalso Char.isDigit(String.sub(str,0))
        val space = namespaceint(num,str) 
      in (case (is_internal,is_generative) of
	    (true,false) => (str ^ "_INT")
	  | (true,true) => (str ^ "_INT_GEN")
	  | (false,_) => (str ^ (case space of
					   0 => ""
					 | x => "_" ^ (Int.toString x))))
      end
    fun loc2string (i) = ("LOC_" ^ (Int.toString i))
    fun tag2string (i,s) = ("NAME_" ^ s ^ "_" ^ (Int.toString i))
    fun tag2int (i,s) = i

    fun mk_var_hash_table (size,notfound_exn) = 
	let
	    val b : word = 0wx3141592
	    fun hash (i : var) = Word31.>>(Word31.*(Word31.fromInt i,b),0wx12)
	    val eqKey = eq_var
	in HashTable.mkTable (hash,eqKey) (size,notfound_exn)
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
      structure LabelMap = SplayMapFn(LabelKey) 
      structure TagMap = SplayMapFn(TagKey) 
      structure PathMap = SplayMapFn(PathKey) 
      structure PathSet = SplaySetFn(PathKey) 


    fun construct_label x = x
    fun deconstruct_label x = x
    fun construct_tag x = x
    fun deconstruct_tag x = x
    fun construct_loc x = x
    fun deconstruct_loc x = x

end
