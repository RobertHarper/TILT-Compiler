(* This benchmark runs a randomized search for solutions to the n-queens
   problem. (for n = 256, 1024, 8192)

    The program has very C-like behavior, allocating only a little
    memory and iterating over it many times. The efficiency of the
    code is probably bound tighly to the performance of standard
    bounds-checked, polymorphic arrays. This program also has several
    good candidates for inlining.
    
    RC4 is used as a deterministic arbitrary number generator.
*)

local

(* a simple 8-bit random number generator *)

signature RANDGEN =
sig

  exception KeyLength

  (* seed the generator with a string 1-256 characters long *)
  val seed : string -> unit

  (* get the next number in the sequence.
     return value will be in the range 0-255. *)
  val byte : unit -> int

end
(* 
    The RC4 algorithm by Rivest.
    Initialize the key with the seed function.

    SML by Tom 7, 2000
    Code in the public domain.
 *)

structure Randgen :> RANDGEN =
struct

  exception KeyLength

  val tub = Array.tabulate (256, fn x => x)
  fun S x = Array.sub (tub, x)
  val i = ref 0
  val j = ref 0

  fun seed key =
      let
          val l = size key
          val _ = (l = 0 orelse l > 256) andalso raise KeyLength
          (* fill 0 -> 255 *)
          val _ = Array.appi (fn (i,_) => Array.update (tub, i, i)) (tub, 0, NONE)
          fun K x = ord (CharVector.sub(key, x mod l))
          fun loop (256, u) = ()
            | loop (x, u) = 
              let val u = (u + S x + K x) mod 256
                  val old = S u
              in  Array.update(tub, u, S x);
                  Array.update(tub, x, old);
                  loop(x+1, u)
              end
      in
          loop (0,0);
          j := 0; 
          i := 0
      end

  fun byte () =
      let
          val _ = i := (!i + 1) mod 256
          val _ = j := (!j + S(!i)) mod 256
          val t = S(!i)
      in
          Array.update (tub, !i, S(!j));
          Array.update (tub, !j, t);
          S ( (S(!i) + S(!j)) mod 256 )
      end

end

signature RAND =
sig
    val rand : int * int -> int
end

structure Rand : RAND =
struct

    fun rand (low, hi) =
        let val u = 
            Randgen.byte () * 256 * 256 +
            Randgen.byte () * 256 +
            Randgen.byte ()
        in low + (u mod (hi - low))
        end

end

(* Random search for a solution to the n-queens problem,
   using patching and a hill-climbing heuristic.
   "Polynomial" time!

   "A Polynomial Time Algorithm for the N-Queens Problem",
        Rok Sosic and Jun Gu
        SIGART Bulletin, Vol. 1, 3, pp. 7-11, Oct 1990

   SML by Tom 7, 2001
   (distributed under the GPL)
*)

functor Queens (R : RAND) =
struct

    exception Inconsistent

    structure A = Array

    fun I x = x
        
    fun inc (a, n) = A.update(a, n, A.sub(a, n) + 1)
    fun dec (a, n) = A.update(a, n, A.sub(a, n) - 1)

    fun aall f a = A.foldl (fn (a, b) => b andalso (f a)) true a

    fun ar2s a =
        (A.foldl (fn (a, b) => b ^ ", " ^ Int.toString a) "" a)

    fun random_permutation n =
        let
            val arr = A.tabulate (n, I)
        in
            A.appi (fn (i, a) =>
                    let val j = R.rand (0, n)
                        val t = A.sub (arr, j)
                    in
                        A.update (arr, j, A.sub(arr, i));
                        A.update (arr, i, t)
                    end) (arr, 0, NONE);
            arr
        end
                
    fun queen_search n =
        let
            val board = random_permutation n
            val diagu = A.array (n * 2 - 1, 0)
            val diagd = A.array (n * 2 - 1, 0)

            val didswap = ref false

            fun du i = A.sub(board, i) + i
            fun dd i = A.sub(board, i) + ((n - 1) - i)

            (* intialize diagonals *)
            fun initd (u, d) i = 
                if i = n then ()
                else 
                    let in
                        inc (u, du i);
                        inc (d, dd i);
                        initd (u, d) (i + 1)
                    end

            fun swap ii jj =
                let
                    val t = A.sub(board, ii)
                in
                    dec (diagu, du ii);
                    dec (diagd, dd ii);
                    dec (diagu, du jj);
                    dec (diagd, dd jj);
                    A.update(board, ii, A.sub(board,jj));
                    A.update(board, jj, t);
                    inc (diagu, du ii);
                    inc (diagd, dd ii);
                    inc (diagu, du jj);
                    inc (diagd, dd jj)
                end
                
            fun tryswaps () =
                let

                    val _ = didswap := false

                    fun allpairs ii =
                        if ii >= n then ()
                        else
                            let
                                fun allj jj =
                                    if jj >= n then ()
                                    else
                                        let
                                        in
                                            if (A.sub (diagu, du ii) > 1 orelse
                                                A.sub (diagd, dd ii) > 1 orelse
                                                A.sub (diagu, du jj) > 1 orelse
                                                A.sub (diagd, dd jj) > 1) then
                                               (* one of ii and jj is attacked.
                                                  will swapping them help? *)
                                                let
                                                    val Then = (A.sub (diagu, du ii) +
                                                                A.sub (diagd, dd ii) +
                                                                A.sub (diagu, du jj) +
                                                                A.sub (diagd, dd jj))

                                                    val _ = swap ii jj
                                                        
                                                    val Now = (A.sub (diagu, du ii) +
                                                               A.sub (diagd, dd ii) +
                                                               A.sub (diagu, du jj) +
                                                               A.sub (diagd, dd jj))
                                                in
                                                    if Now >= Then then
                                                        (* made it worse! swap back. *)
                                                        let in
                                                            swap ii jj
                                                        end
                                                    else let in
                                                            didswap := true
                                                         end
                                                end
                                                
                                            else ();

                                            allj (jj + 1)
                                        end
                            in
                                allj (ii + 1);
                                allpairs (ii + 1)
                            end
                in
                    allpairs 0;
                    if (!didswap) then
                        tryswaps ()
                    else 
                            (* we are either done or stuck.
                             check it! *)
                        (if aall (fn u => u <= 1) diagu andalso
                            aall (fn u => u <= 1) diagd
                         then
                             board
                         else
                             queen_search n)

                end

        in
            initd (diagu, diagd) 0;
            tryswaps ()
        end
    

    fun printboard b =
        let
            val _ = print ("      \tb (" ^ ar2s b ^ ") u (pb)\n")
            val n = A.length b
            fun alln i = if i = n then ()
                         else let in
                             A.app (fn j => print (if i = j then "X " else "- ")) b;
                             print "\n";
                             alln (i + 1)
                             end
        in
            alln 0
        end
end


structure Q = Queens(Rand);


fun solve n = 
    let
        val b = Q.queen_search n
    in
        Q.printboard b
    end

in

    fun runPQueens () =
        let in
            Randgen.seed "SML";
            Q.queen_search 256;
            Q.queen_search 1024;
(*           Q.queen_search 8192;*)
            ()
        end
    
end

(*val _ = runPQueens ();*)
