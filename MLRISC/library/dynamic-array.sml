structure DynamicArray : 
  sig include ARRAY_SIG 
      val baseArray : 'a array -> 'a Array.array
      val clear     : 'a array * int -> unit
  end =
  struct
     structure A = Array
     type 'a vector = 'a A.vector 
     datatype 'a array = ARRAY of 'a A.array ref * 'a * int ref

     exception Subscript = General.Subscript
     exception Size      = General.Size
     exception Unimplemented

     infix 9 sub

     val maxLen = A.maxLen

     fun array (n,d) = ARRAY(ref(A.array (n,d)), d, ref 0) 
     fun clear (ARRAY(a,def,cnt),n) = (a := A.array(n,def); cnt := n)

     fun baseArray(ARRAY(ref a,_,_)) = a

     fun length (ARRAY (ref a,_,ref n)) = n

     fun (ARRAY(ref a, d, _)) sub i = A.sub(a,i) handle _ => d
    
     fun update (ARRAY(r as ref a, d, n), i, e) =
        (A.update(a,i,e); n := Int.max(!n,i+1)) handle Subscript =>
            let val new_size  = Int.max(i+1,!n*2)
                val new_size  = if new_size < 10 then 10 else new_size
                val new_array = A.array(new_size,d)
            in  A.copy {src = a, si = 0, len = NONE, dst = new_array, di = 0};
                r := new_array;
                n := i+1;
                A.update(new_array, i, e)
            end

     fun extract (ARRAY(r as ref a, _, ref n), i, j) = A.extract (a, i, j)

     fun copy { src = ARRAY(ref a,_,sz), si, len, dst, di } =
       let val n = case len of SOME l => si + l 
                             | NONE   => !sz
           fun cp(i,j) = 
                if i < n then (update(dst,j,A.sub(a,i)); cp(i+1,j+1)) else ()
       in  cp (si, di)
       end

     fun copyVec { src, si, len, dst = ARRAY(ref a,_,sz), di } = 
       A.copyVec { src = src, si = si, len = len, dst = a, di = di }

     fun tabulate (n, f) = 
         let val array   = A.tabulate(n, f)
             val default = A.sub(array,0)
         in
             ARRAY(ref array, default, ref n)
         end handle Subscript => raise Size

     fun fromList l =
         let val array   = A.fromList l
             val default = A.sub(array,0)
         in
             ARRAY(ref array, default, ref (List.length l))
         end handle Subscript => raise Size

     fun app f (ARRAY (ref a,_,ref n)) = 
         A.appi (fn (_,x) => f x) (a,0,SOME n)

     fun foldl f u (ARRAY (ref a,_,ref n)) = 
        A.foldli (fn (_,x,y) => f (x,y)) u (a, 0, SOME n)

     fun foldr f u (ARRAY (ref a,_,ref n)) = 
        A.foldri (fn (_,x,y) => f (x,y)) u (a, 0, SOME n)

     fun modify f (ARRAY (ref a,_,ref n)) =  
        A.modifyi (fn (_,x) => f x) (a, 0, SOME n)

     fun appi f (ARRAY(ref a,_,ref n), i, j) = A.appi f (a, i, j)

     fun foldli f u (ARRAY(ref a,_,ref n), i, j) = A.foldli f u (a, i, j)

     fun foldri f u (ARRAY(ref a,_,ref n), i, j) = A.foldri f u (a, i, j)

     fun modifyi f (ARRAY(ref a,_,ref n), i, j) = A.modifyi f (a, i, j)

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:18  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:55  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:42  pscheng
# *** empty log message ***
#
 *)
