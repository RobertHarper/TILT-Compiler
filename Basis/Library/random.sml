(* random.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This package implements a random number generator using a subtract-with-borrow
 * (SWB) generator as described in Marsaglia and Zaman, "A New Class of Random Number
 * Generators," Ann. Applied Prob. 1(3), 1991, pp. 462-480.
 *
 * The SWB generator is a 31-bit generator with lags 48 and 8. It has period
 * (2^1487 - 2^247)/105 or about 10^445. In general, these generators are
 * excellent. However, they act locally like a lagged Fibonacci generator
 * and thus have troubles with the birthday test. Thus, we combine this SWB
 * generator with the linear congruential generator (48271*a)mod(2^31-1).
 *
 * Although the interface is fairly abstract, the implementation uses
 * 31-bit ML words. At some point, it might be good to use 32-bit words.
 *
 * Since TILT does not have Word31, the code is modified to use Word32.
 * There is a possiblity that the linear congruential generator's magic
 * numbers have to be modified.
 *)

structure Random : RANDOM =
  struct
    structure A   = Array
    structure LW  = LargeWord
    structure W8A = Word8Array
    structure W8V = Word8Vector
    structure P   = Pack32Big

    val << = Word32.<<
    val >> = Word32.>>
    val & = Word32.andb
    val ++ = Word32.orb
    val xorb = Word32.xorb
    infix << >> & ++

    val nbits = 32                                      (* bits per word *)
    val maxWord : Word32.word = 0wxffffffff
    val bit31 : Word32.word   = 0wx80000000
    val lo31 : Word32.word    = 0wx7fffffff

    val N = 48
    val lag = 8
    val offset = N-lag

    fun error (f,msg) = LibBase.failure {module="Random",func=f, msg=msg}

    val two2neg31 = let val two2thirty = 0x40000000
		    in  0.5 / (real two2thirty)
		    end

    fun minus(x,y,false) = (x - y, y > x)
      | minus(x,y,true) = (x - y - 0w1, y >= x)

    datatype rand = RND of {
        vals   : Word32.word A.array,(* seed array *)
        borrow : bool ref,           (* last borrow *)
        congx  : Word32.word ref,    (* congruential seed *)
        index  : int ref             (* index of next available value in vals *)
      }

      (* We represent state as a string, starting with an initial
       * word acting as an magic cookie (with bit 0 determining the
       * value of borrow), followed by a word containing index and a word
       * containing congx, followed by the seed array.
       *)
    val numWords = 3 + N
    val magic : LW.word = 0wx72646e64
    fun toString (RND{vals, borrow, congx, index}) = let
          val arr = W8A.array (4*numWords, Word8.fromInt 0)
          val word0 = if !borrow then LW.orb (magic, 0w1) else magic
          fun fill (src,dst) =
                if src = N then ()
                else (
                  P.update (arr, dst, A.sub (vals, src));
                  fill (src+1,dst+1)
                )
          in
            P.update (arr, 0, word0);
            P.update (arr, 1, LW.fromInt (!index));
            P.update (arr, 2, (!congx));
            fill (0,3);
            Byte.bytesToString (W8A.extract (arr, 0, NONE))
          end

    fun fromString s = let
          val bytes = Byte.stringToBytes s
          val _ = if W8V.length bytes = 4 * numWords then ()
                  else error ("fromString","invalid state string")
          val word0 = P.subVec (bytes, 0)
          val _ = if LW.andb(word0, 0wxFFFFFFFE) = magic then ()
                  else error ("fromString","invalid state string")
          fun subVec i = P.subVec (bytes, i)
          val borrow = ref (LW.andb(word0,0w1) = 0w1)
          val index = ref (LW.toInt (subVec 1))
          val congx = ref (subVec 2)
          val arr = A.array (N, 0w0 : Word32.word)
          fun fill (src,dst) =
                if dst = N then ()
                else (
                  A.update (arr, dst, subVec src);
                  fill (src+1,dst+1)
                )
          in
            fill (3, 0);
            RND{vals = arr,
                index = index,
                congx = congx,
                borrow = borrow}
          end

      (* linear congruential generator:
       * multiplication by 48271 mod (2^32 - 1)
       *)
    val a : Word32.word = 0w48271
    val m : Word32.word = 0wxffffffff
    val q = m div a
    val r = m mod a
    fun lcg seed = let
          val left = a * (seed mod q)
          val right = r * (seed div q)
          in
            if left > right then left - right
            else (m - right) + left
          end

      (* Fill seed array using subtract-with-borrow generator:
       *  x[n] = x[n-lag] - x[n-N] - borrow
       * Sets index to 1 and returns 0th value.
       *)
    fun fill (RND{vals,index,congx,borrow}) = let
          fun update (ix,iy,b) = let
                val (z,b') = minus(A.sub(vals,ix), A.sub(vals,iy),b)
                in
                  A.update(vals,iy,z); b'
                end
          fun fillup (i,b) =
                if i = lag then b
                else fillup(i+1, update(i+offset,i,b))
          fun fillup' (i,b) =
                if i = N then b
                else fillup'(i+1, update(i-lag,i,b))
          in
            borrow := fillup' (lag, fillup (0,!borrow));
            index := 1;
            A.sub(vals,0)
          end

      (* Create initial seed array and state of generator.
       * Fills the seed array one bit at a time by taking the leading
       * bit of the xor of a shift register and a congruential sequence.
       * The congruential generator is (c*48271) mod (2^32 - 1).
       * The shift register generator is c(I + L18)(I + R13).
       * The same congruential generator continues to be used as a
       * mixing generator with the SWB generator.
       *)
    fun rand (congy, shrgx) = let
          fun mki (i,c,s) = let
                val c' = lcg c
                val s' = xorb(s, s << 0w18)
                val s'' = xorb(s', s' >> 0w13)
                val i' = (lo31 & (i >> 0w1)) ++ (bit31 & xorb(c',s''))
                in (i',c',s'') end
	  fun iterate (0, v) = v
	    | iterate (n, v) = iterate(n-1, mki v)
          fun mkseed (congx,shrgx) = iterate (nbits, (0w0,congx,shrgx))
          fun genseed (0,seeds,congx,_) = (seeds,congx)
            | genseed (n,seeds,congx,shrgx) = let
                val (seed,congx',shrgx') = mkseed (congx,shrgx)
                in genseed(n-1,seed::seeds,congx',shrgx') end
          val congx = ((Word32.fromInt congy & maxWord) << 0w1)+0w1
          val (seeds,congx) = genseed(N,[],congx, Word32.fromInt shrgx)
          in
            RND{vals = A.fromList seeds,
                index = ref 0,
                congx = ref congx,
                borrow = ref false}
          end

      (* Get next random number. The tweak function combines
       * the number from the SWB generator with a number from
       * the linear congruential generator.
       *)
    fun randWord (r as RND{vals, index,congx,...}) = let
         val idx = !index
         fun tweak i = let
               val c = lcg (!congx)
               in
                 congx := c;
                 xorb(i, c)
               end
         in
           if idx = N then tweak(fill r)
           else tweak(A.sub(vals,idx)) before index := idx+1
         end

    fun randInt state = Word32.toIntX(randWord state)
    fun randNat state = Word32.toIntX(randWord state & lo31)
    fun randReal state =
      (real(randNat state) + real(randNat state) * two2neg31) * two2neg31

    fun randRange (i,j) =
          if j < i
            then error ("randRange", "hi < lo")
            else let
              val R = two2neg31*real(j - i + 1)
              in
                fn s => i + trunc(R*real(randNat s))
              end handle _ => let
                val ri = real i
                val R = (real j)-ri+1.0
                in
                  fn s => trunc(ri + R*(randReal s))
                end

  end; (* Random *)


