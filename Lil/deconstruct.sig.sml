(* This is the signature of the con deconstructor
 * functions defined below in Dec.C and Dec.C'.  Note that this
 * does *not* correspond to a toplevel structure
 *)

signature CDEC = 
  sig

    val var : Lil.con -> Lil.var
    val var' : Lil.con -> Lil.var option

    val APP : Lil.con -> Lil.con * Lil.kind
    val APP' : Lil.con -> (Lil.con * Lil.kind) option

    val arrow    : Lil.con -> Lil.con * Lil.con * Lil.con
    val arrow'   : Lil.con -> (Lil.con * Lil.con * Lil.con) option
    val arrow_ml : Lil.con -> Lil.con list * Lil.con list * Lil.con
    val arrow_ml': Lil.con -> (Lil.con list * Lil.con list * Lil.con) option
    val allarrow  : Lil.con
      -> (Lil.var * Lil.kind) list * Lil.con * Lil.con * Lil.con
    val allarrow' : Lil.con
      -> ((Lil.var * Lil.kind) list * Lil.con * Lil.con * Lil.con) option
    val allarrow_ml : Lil.con
      -> (Lil.var * Lil.kind) list * Lil.con list * Lil.con list * Lil.con
    val allarrow_ml': Lil.con
      -> ((Lil.var * Lil.kind) list * Lil.con list * Lil.con list * Lil.con) option
    val code    : Lil.con -> Lil.con * Lil.con * Lil.con
    val code'   : Lil.con -> (Lil.con * Lil.con * Lil.con) option
    val code_ml : Lil.con -> Lil.con list * Lil.con list * Lil.con
    val code_ml': Lil.con -> (Lil.con list * Lil.con list * Lil.con) option
    val allcode  : Lil.con
      -> (Lil.var * Lil.kind) list * Lil.con * Lil.con * Lil.con
    val allcode' : Lil.con
      -> ((Lil.var * Lil.kind) list * Lil.con * Lil.con * Lil.con) option
    val allcode_ml : Lil.con
      -> (Lil.var * Lil.kind) list * Lil.con list * Lil.con list * Lil.con
    val allcode_ml': Lil.con
      -> ((Lil.var * Lil.kind) list * Lil.con list * Lil.con list * Lil.con) option

    val app  : Lil.con -> Lil.con * Lil.con
    val app' : Lil.con -> (Lil.con * Lil.con) option
    val fold  : Lil.con -> Lil.kind * Lil.con
    val fold' : Lil.con -> (Lil.kind * Lil.con) option
    val kapp  : Lil.con -> Lil.con * Lil.kind
    val kapp' : Lil.con -> (Lil.con * Lil.kind) option
    val ksum    : Lil.con -> Lil.con * Lil.con * Lil.con
    val ksum'   : Lil.con -> (Lil.con * Lil.con * Lil.con) option
    val ksum_ml : Lil.con -> Lil.w32 * Lil.w32 * Lil.con list
    val ksum_ml': Lil.con -> (Lil.w32 * Lil.w32 * Lil.con list) option

    val float  : Lil.con -> unit
    val float' : Lil.con -> unit option

    val ptr  : Lil.con -> Lil.con
    val ptr' : Lil.con -> Lil.con option

    val pi1  : Lil.con -> Lil.con
    val pi1' : Lil.con -> Lil.con option
    val pi2  : Lil.con -> Lil.con
    val pi2' : Lil.con -> Lil.con option

    val exists  : Lil.con -> Lil.kind * Lil.con
    val exists' : Lil.con -> (Lil.kind * Lil.con) option
    val exists_ml  : Lil.con -> (Lil.var * Lil.kind) * Lil.con
    val exists_ml' : Lil.con -> ((Lil.var * Lil.kind) * Lil.con) option

    val forall  : Lil.con -> Lil.kind * Lil.con
    val forall' : Lil.con -> (Lil.kind * Lil.con) option
    val forall_ml  : Lil.con -> (Lil.var * Lil.kind) * Lil.con
    val forall_ml' : Lil.con -> ((Lil.var * Lil.kind) * Lil.con) option
    (* Every type is trivially an nary-forall *)
    val nary_forall  : Lil.con -> (Lil.var * Lil.kind) list * Lil.con

    val nat : Lil.con -> Lil.w32
    val nat' : Lil.con -> Lil.w32 option
    val rek  : Lil.con -> (Lil.kind * Lil.con * Lil.con)
    val rek' : Lil.con -> (Lil.kind * Lil.con * Lil.con) option
    val sum  : Lil.con -> Lil.con * Lil.con
    val sum' : Lil.con -> (Lil.con * Lil.con) option
    val sum_ml : Lil.con -> Lil.w32 * Lil.con list
    val sum_ml': Lil.con -> (Lil.w32 * Lil.con list) option

    val lam  : Lil.con -> (Lil.var * Lil.kind) * Lil.con 
    val lam' : Lil.con -> ((Lil.var * Lil.kind) * Lil.con) option
    val LAM  : Lil.con -> Lil.var * Lil.con 
    val LAM' : Lil.con -> (Lil.var * Lil.con) option

    val prim  : Lil.con -> Lil.primcon * Lil.con list
    val prim' : Lil.con -> (Lil.primcon * Lil.con list) option
    val prim0  : Lil.con -> Lil.primcon
    val prim0' : Lil.con -> Lil.primcon option
    val prim1  : Lil.con -> Lil.primcon * Lil.con
    val prim1' : Lil.con -> (Lil.primcon * Lil.con) option
    val prim2  : Lil.con -> Lil.primcon * Lil.con * Lil.con
    val prim2' : Lil.con -> (Lil.primcon * Lil.con * Lil.con) option
    val prim3  : Lil.con -> Lil.primcon * Lil.con * Lil.con * Lil.con
    val prim3' : Lil.con -> (Lil.primcon * Lil.con * Lil.con * Lil.con) option
    val polyprim  : Lil.con -> Lil.primcon * Lil.kind list * Lil.con list
    val polyprim' : Lil.con -> (Lil.primcon * Lil.kind list * Lil.con list) option
    val polyprim11  : Lil.con -> Lil.primcon * Lil.kind * Lil.con 
    val polyprim11' : Lil.con -> (Lil.primcon * Lil.kind * Lil.con) option

    (* tuples of cons *)
    val pair : Lil.con -> Lil.con * Lil.con
    val pair' : Lil.con -> (Lil.con * Lil.con) option
    val ntuple  : Lil.con -> Lil.con list
    val ntuple' : Lil.con -> Lil.con list option

    val nproj  : Lil.con -> (int * Lil.con) 
    val nproj' : Lil.con -> (int * Lil.con) option
    (* tuple type *)
    val tuple  : Lil.con -> Lil.con 
    val tuple' : Lil.con -> Lil.con option
    val tuple_ml  : Lil.con -> Lil.con list
    val tuple_ml' : Lil.con -> Lil.con list option

    val list     : Lil.con -> Lil.kind * (Lil.con list)
    val list'    : Lil.con -> (Lil.kind * (Lil.con list)) option
    val nill  : Lil.con -> Lil.kind 
    val nill' : Lil.con -> Lil.kind option
    val cons  : Lil.con -> Lil.con
    val cons' : Lil.con -> Lil.con option
    val cons_ml  : Lil.con -> (Lil.con * Lil.con)
    val cons_ml' : Lil.con -> (Lil.con * Lil.con) option

    val hd : Lil.con -> Lil.con
    val tl : Lil.con -> Lil.con
    val nth  : int -> Lil.con -> Lil.con
    val nth' : int -> Lil.con -> Lil.con option

    val star  : Lil.con -> unit
    val star' : Lil.con -> unit option

    val inj  : Lil.con -> (Lil.w32 * Lil.kind * Lil.con)
    val inj' : Lil.con -> (Lil.w32 * Lil.kind * Lil.con) option
    val inji : Lil.w32 * Lil.con -> (Lil.kind * Lil.con)
    val inji': Lil.w32 * Lil.con -> (Lil.kind * Lil.con) option
    val inl  : Lil.con -> (Lil.kind * Lil.con)
    val inl' : Lil.con -> (Lil.kind * Lil.con) option
    val inr  : Lil.con -> (Lil.kind * Lil.con)
    val inr' : Lil.con -> (Lil.kind * Lil.con) option

    val sumcase  : Lil.con -> (Lil.con * (Lil.w32 * (Lil.var * Lil.con)) list * Lil.con option) 
    val sumcase' : Lil.con -> (Lil.con * (Lil.w32 * (Lil.var * Lil.con)) list * Lil.con option) option

    val pr   : Lil.con -> (Lil.var * (Lil.var * Lil.kind) * Lil.kind * Lil.var * Lil.con) 
    val pr'  : Lil.con -> (Lil.var * (Lil.var * Lil.kind) * Lil.kind * Lil.var * Lil.con) option 
    val unfold  : Lil.con -> Lil.kind
    val unfold' : Lil.con -> Lil.kind option

    val boxed : Lil.con -> (Lil.size * Lil.con)
    val boxed' : Lil.con -> (Lil.size * Lil.con) option
      
    val externarrow  : Lil.con -> (Lil.size * Lil.con * Lil.con * Lil.con)
    val externarrow' : Lil.con -> (Lil.size * Lil.con * Lil.con * Lil.con) option
    val externarrow_ml  : Lil.con -> (Lil.size * Lil.con list * Lil.con list * Lil.con)
    val externarrow_ml' : Lil.con -> (Lil.size * Lil.con list * Lil.con list * Lil.con) option

    val coercion  : Lil.con -> (Lil.con * Lil.con)
    val coercion' : Lil.con -> (Lil.con * Lil.con) option

    val exn_packet  : Lil.con -> (Lil.con * Lil.con)
    val exn_packet' : Lil.con -> (Lil.con * Lil.con) option

  end    (* CDEC *)

signature DECONSTRUCT = 
  sig

    structure Dec : 
      sig
	structure K :
	  sig
	    (* Destruct kind level syntax
	     *)
	    val arrow : Lil.kind -> Lil.kind * Lil.kind
	    val arrow' : Lil.kind -> (Lil.kind * Lil.kind) option
	    val forall : Lil.kind -> Lil.var * Lil.kind
	    val forall' : Lil.kind -> (Lil.var * Lil.kind) option
	    val sum : Lil.kind -> Lil.kind list
	    val sum' : Lil.kind -> Lil.kind list option
	    val binsum  : Lil.kind -> (Lil.kind * Lil.kind)
	    val binsum' : Lil.kind -> (Lil.kind * Lil.kind) option
	    val pair : Lil.kind -> Lil.kind * Lil.kind
	    val pair' : Lil.kind -> (Lil.kind * Lil.kind) option
	    val mu : Lil.kind -> Lil.var * Lil.kind
	    val mu' : Lil.kind -> (Lil.var * Lil.kind) option

	    val ntuple  : Lil.kind -> Lil.kind list 
	    val ntuple' : Lil.kind -> Lil.kind list option
	    val list  : Lil.kind -> Lil.kind
	    val list' : Lil.kind -> Lil.kind option
	  end

	(* These are functions for deconstructing types.  The regular versions 
	 * deconstruct unconditionally, and throw an error if the constructor is
	 * not what was asked for.  The primed versions return an option.
	 * Where present, the _ml version deconstructs into ml level values.  
	 *
	 * The C versions weak head normalize at every level of pattern
	 * matching.  
	 * The C' versions are strictly syntactic.
	 *)
	structure C : CDEC
	structure C' : CDEC
	  
	structure Q : 
	  sig
	    val coercion  : Lil.sv32 -> (Lil.ctag * Lil.con list)
	    val coercion' : Lil.sv32 -> (Lil.ctag * Lil.con list) option
	    val pack : Lil.sv32 -> (Lil.con * Lil.con) 
	    val pack' : Lil.sv32 -> (Lil.con * Lil.con) option
	    val forgetknown : Lil.sv32 -> Lil.con 
	    val forgetknown' : Lil.sv32 -> Lil.con option
	    val projknown : Lil.sv32 -> Lil.con 
	    val projknown' : Lil.sv32 -> Lil.con option
	    val injunion : Lil.sv32 -> Lil.con 
	    val injunion' : Lil.sv32 -> Lil.con option
	    val injforget : Lil.sv32 -> Lil.con 
	    val injforget' : Lil.sv32 -> Lil.con option
	  end

	structure E :
	  sig
	    (* Every small value is trivially an nary-tapp
	     *)
	    val nary_tapp : Lil.sv32 -> Lil.sv32 * Lil.con list 
	    val coerce : Lil.sv32 -> (Lil.sv32 * Lil.sv32)
	    val coerce' : Lil.sv32 -> (Lil.sv32 * Lil.sv32) option
	    val pack : Lil.sv32 -> (Lil.con * Lil.con * Lil.sv32) 
	    val pack' : Lil.sv32 -> (Lil.con * Lil.con * Lil.sv32) option
	    val forgetknown : Lil.sv32 -> (Lil.con * Lil.sv32)
	    val forgetknown' : Lil.sv32 -> (Lil.con * Lil.sv32) option
	    val projknown : Lil.sv32 -> (Lil.con * Lil.sv32)
	    val projknown' : Lil.sv32 -> (Lil.con * Lil.sv32) option
	    val injunion : Lil.sv32 -> (Lil.con * Lil.sv32)
	    val injunion' : Lil.sv32 -> (Lil.con * Lil.sv32) option
	    val injforget : Lil.sv32 -> (Lil.con * Lil.sv32)
	    val injforget' : Lil.sv32 -> (Lil.con * Lil.sv32) option

	  end

	structure TD :
	  sig
	    val Tmilr : Lil.kind -> unit option
	    val Tmil  : Lil.kind -> unit option
	    val interpr : Lil.con -> Lil.con option
	    val interp  : Lil.con -> Lil.con option
	    val Rtuple : Lil.con -> Lil.con option
	    val R      : Lil.con -> Lil.con option
	  end
	
      end (* Dec *)

    structure Elim : 
      sig
	structure K :
	  sig
	    (* Compute the type of a constructor level elim form
	     *)
	    val app : Lil.kind -> Lil.kind
	    val APP : Lil.kind -> Lil.kind -> Lil.kind
	    val pi1 : Lil.kind -> Lil.kind 
	    val pi2 : Lil.kind -> Lil.kind	  
	    val unfold : Lil.kind -> Lil.kind

	    val app' : Lil.kind -> Lil.kind option
	    val APP' : Lil.kind -> Lil.kind -> Lil.kind option
	    val pi1' : Lil.kind -> Lil.kind option
	    val pi2' : Lil.kind -> Lil.kind option  
	    val unfold' : Lil.kind -> Lil.kind option

	  end

	structure C :
	  sig

	    (* These functions are type deconstructors for computing the types
	     * of the term level constructs.
	     *)
	    val instantiate : Lil.con -> Lil.con -> Lil.con
	    val unpack      : Lil.var * Lil.con -> Lil.kind * Lil.con
	    val unbox       : Lil.con -> Lil.con
	    val coerce      : Lil.con -> Lil.con
	    val select      : Lil.w32 -> Lil.con -> Lil.con
	    val project     : Lil.w32 -> Lil.con -> Lil.con
	    val externapp   : Lil.con -> Lil.con
	    val app         : Lil.con -> Lil.con
	    val call        : Lil.con -> Lil.con
	    val unroll      : Lil.con -> Lil.con 
	    val unroll'     : Lil.con -> Lil.con option
	  end    (* C *)
      end (* Dec *)

  end (*Deconstruct *)