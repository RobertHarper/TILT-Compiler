(*$import CRC Vector *)
(* CRC.sml -- written by Andrew Appel for SML/NJ, modified by PS 1994-10-24,
   modified by Martin Elsman 1997-05-30 
*)

  (* 128-bit CRC.
     The call `append crc c' corresponds to eight steps of a shift register
     circuit, shifting in one bit of character c from the left in each step.
     See Figure 2.16 in Bertsekas and Gallager: Data Networks (1987),
     or  Figure 3-32 in Siewiorek and Swarz: The Theory and Practice
                        of Reliable System Design (Digital Press, 1982).
  *)

structure Crc :> CRC =
  struct

    fun for f i j =
      if i > j then () else (f i : unit; for f (i+1) j)

    fun incr r = r := !r + 1

    type crc = int ref * int Array.array * int ref;

    (* The prime generator polynomial is 1 + x + x^2 + x^7 + x^128.
       Reversing the low coefficient bits we have 1110.0001 = 225 *)

    val poly = 225;

    local val ti = Word31.toInt
          val fi = Word31.fromInt
    in
      val andb_ : int -> int -> int = fn a => fn b => ti(Word31.andb(fi a,fi b))
      val xorb_ : int -> int -> int = fn a => fn b => ti(Word31.xorb(fi a,fi b))
      val rshiftsig_ : int -> int -> int = fn a => fn b => ti(Word31.~>>(fi a, fi b))
    end

    val table =
    Vector.fromList [
	0,   225,   450,   291,   900,   869,   582,   679,
     1800,  2025,  1738,  1579,  1164,  1133,  1358,  1455,
     3600,  3825,  4050,  3891,  3476,  3445,  3158,  3255,
     2328,  2553,  2266,  2107,  2716,  2685,  2910,  3007,
     7200,  7361,  7650,  7427,  8100,  8005,  7782,  7815,
     6952,  7113,  6890,  6667,  6316,  6221,  6510,  6543,
     4656,  4817,  5106,  4883,  4532,  4437,  4214,  4247,
     5432,  5593,  5370,  5147,  5820,  5725,  6014,  6047,
    14400, 14497, 14722, 14691, 15300, 15141, 14854, 15079,
    16200, 16297, 16010, 15979, 15564, 15405, 15630, 15855,
    13904, 14001, 14226, 14195, 13780, 13621, 13334, 13559,
    12632, 12729, 12442, 12411, 13020, 12861, 13086, 13311,
     9312,  9345,  9634,  9539, 10212,  9989,  9766,  9927,
     9064,  9097,  8874,  8779,  8428,  8205,  8494,  8655,
    10864, 10897, 11186, 11091, 10740, 10517, 10294, 10455,
    11640, 11673, 11450, 11355, 12028, 11805, 12094, 12255,
    28800, 28769, 28994, 29091, 29444, 29669, 29382, 29223,
    30600, 30569, 30282, 30379, 29708, 29933, 30158, 29999,
    32400, 32369, 32594, 32691, 32020, 32245, 31958, 31799,
    31128, 31097, 30810, 30907, 31260, 31485, 31710, 31551,
    27808, 27713, 28002, 28035, 28452, 28613, 28390, 28167,
    27560, 27465, 27242, 27275, 26668, 26829, 27118, 26895,
    25264, 25169, 25458, 25491, 24884, 25045, 24822, 24599,
    26040, 25945, 25722, 25755, 26172, 26333, 26622, 26399,
    18624, 18465, 18690, 18915, 19268, 19365, 19078, 19047,
    20424, 20265, 19978, 20203, 19532, 19629, 19854, 19823,
    18128, 17969, 18194, 18419, 17748, 17845, 17558, 17527,
    16856, 16697, 16410, 16635, 16988, 17085, 17310, 17279,
    21728, 21505, 21794, 21955, 22372, 22405, 22182, 22087,
    21480, 21257, 21034, 21195, 20588, 20621, 20910, 20815,
    23280, 23057, 23346, 23507, 22900, 22933, 22710, 22615,
    24056, 23833, 23610, 23771, 24188, 24221, 24510, 24415
    ];

    fun crc_new() = (ref 0, Array.array(16, 0), ref 0);

    fun crc_extract(ref len, a, ref i) =
      let open CharArray
	  val i' = i+1
	  val s = array(20, #"*")
      in
	update(s, 0, Char.chr (andb_ len 255));
	update(s, 1, Char.chr (andb_ (rshiftsig_ len 8) 255));
	update(s, 2, Char.chr (andb_ (rshiftsig_ len 16) 255));
	update(s, 3, Char.chr (andb_ (rshiftsig_ len 24) 255));
	for (fn k => update(s, 19-k, Char.chr(Array.sub(a, (i'+k) mod 16))))
	    0 15;
	extract(s, 0, SOME 20)
      end;

    fun crc_append (rlen , a, (ri as ref i)) c =
      let open Array
	  val rmbyte = sub(a, i)                (* rightmost byte *)
	  val hilo = Vector.sub(table, rmbyte)
	  val hi = rshiftsig_ hilo 8
	  val lo = andb_ hilo 255
	  val i' = (i+15) mod 16                (* leftmost byte  *)
      in
	ignore (Char.chr c);
	update(a, i, xorb_ c hi);
	update(a, i', xorb_ (sub(a, i')) lo);
	ri := (i+1) mod 16;
	incr rlen
      end;

    fun crc_of_string s =
      let val arr = crc_new ()
	  val len = size s
      in
	for (fn i => crc_append arr (Char.ord (CharVector.sub(s, i))))
	    0 (len-1);
	crc_extract arr
      end;

    (* crc_of_string "abcdefghijklmnopqrstuvwxyz"  -->
	  "\^Z\000\000\000kX\240\2109\151tV\178\^T\247\241onml" *)

    (* - crc "abcdefghijklmnopqrstuvwxyz";
       > val it = "\^Z\000\000\000kX\240\2109\151tV\178\^T\247\241onml" : string *)


    fun crc_of_file s =
      let val is = BinIO.openIn s
	  val arr = crc_new ()
	  val n = Word8Vector.maxLen
(* NT version of SML/NJ does not support canInput *)
(*	  fun loop () =
	    case BinIO.canInput(is,n)
	      of SOME n' => let val v = BinIO.inputN(is,n')
				val _ = Word8Vector.foldl (fn (e,_) => 
crc_append arr (Word8.toInt e)) () v
			    in if n=n' then loop()
			       else ()
			    end
	       | NONE => ()
*)
	  fun loop () =
		  let val v = BinIO.inputN(is,n)
		      val _ = Word8Vector.foldl (fn (e,_) => 
						 crc_append arr (Word8.toInt e)) () v
		  in if (Word8Vector.length v > 0) then loop()
		     else ()
		  end
      in loop(); BinIO.closeIn is; crc_extract arr
      end 
	    

    type crc = string
    val output_crc = BinIO_Util.output_string
	
    exception InputCrc
    fun input_crc is = 
      case BinIO_Util.input_string (is,20)
	of SOME s => s
	 | NONE => raise InputCrc
	    
    fun toString (crc : crc) =
	let fun w2s w = if w > 0w15 then Word8.toString w
			else "0" ^ Word8.toString w
	in  String.translate (fn c => (w2s (Byte.charToByte c))) crc
	end

    fun fromString s =
	if size s = 40 then
	    let fun char i = Option.map Byte.byteToChar (Word8.fromString (String.extract (s, i, SOME 2)))
		val result = CharArray.array (20, #"*")
		fun loop (20, _) = SOME (CharArray.extract (result, 0, NONE))
		  | loop (i, i2) = (case char i2
				      of NONE => NONE
				       | SOME c => (CharArray.update (result, i, c);
						    loop (i+1, i2+2)))
	    in  loop (0,0)
	    end
	else NONE
	    
(*
    structure Test =
     struct
       val file_crc = "/tmp/test.crc";

       fun out crc = let val os = BinIO.openOut file_crc
		     in output_crc (os,crc); BinIO.closeOut os
		     end

       fun input () = let val is = BinIO.openIn file_crc
			  val crc = input_crc is
		      in BinIO.closeIn is; crc
		      end

       exception TestCrc
       fun eq_test(crc1,crc1',b,s) =
	 if crc1=crc1' = b then ()
	 else (print ("Error during test of crc module: ( " ^ s ^ " )\n");
	       print (crc1 ^ "\n");
	       print (crc1' ^ "\n");
	       raise TestCrc)

       val crc1 = crc_of_string "aksjhglkjashf;ashdf;kjhsadfl;kjhsalfkdjhaslkjdfhlksajdhflkjhsdflkjahsdflkjhsadlfkjhsalkdjfh;lkjw;oirtuojv/lknqowreijhgfoijgohjagrl/;jha;lsrujtg;oiuejrgojaeg/loihjas/k,fgj";
       val crc2 = crc_of_file "/bin/ls"

       val _ = out crc1
       val crc1' = input()
       val _ = eq_test(crc1,crc1',true,"test1")
       val _ = eq_test(crc2,crc1',false, "test2")
       val _ = out crc2
       val crc2' = input()
       val _ = eq_test(crc2,crc2',true,"test3")
       val _ = eq_test(crc1',crc2',false,"test4")     
     end
*)
  end
