val sin = Real.Math.sin
val cos = Real.Math.cos
val log = Real.Math.ln
val exp = Real.Math.exp
val array1 = Array.array
type 'a array1 = 'a Array.array
val sub1 = Array.sub
val update1 = Array.update
val length1 = Array.length
val std_out = TextIO.stdOut
val std_in = TextIO.stdIn
fun output(des,s) = TextIO.output (des,s)
fun input_line des = TextIO.inputLine des
fun input arg = TextIO.inputN arg
fun open_in filename = TextIO.openIn filename
fun close_in des = TextIO.closeIn des
fun open_out filename = TextIO.openOut filename
fun close_out des = TextIO.closeOut des
fun flush_out des = TextIO.flushOut des
fun end_of_stream des = TextIO.endOfStream des
fun input1 arg = (case TextIO.input1 arg of
			 SOME c => c
		       | NONE => #" ")
fun lookahead arg = (case TextIO.lookahead arg of
			 SOME c => c
		       | NONE => #" ")


structure Integer = Int
fun int32touint32 (x : int) = x
fun uint32toint32 (x : int) = x
val uint8toint32 = ord
val int32touint8 = chr
fun max(x : int,y) = if (x>y) then x else y
infix && || >> <<
fun op >>(x,y) = Word32.toInt(Word32.>>(Word32.fromInt x,Word.fromInt y))
fun op <<(x,y) = Word32.toInt(Word32.<<(Word32.fromInt x,Word.fromInt y))
fun op &&(x,y) = Word32.toInt(Word32.andb(Word32.fromInt x,Word32.fromInt y))
fun op ||(x,y) = Word32.toInt(Word32.orb(Word32.fromInt x,Word32.fromInt y))
val op mod = (op mod) : int * int -> int
val op div = (op div) : int * int -> int
fun inc x = x := (!x + 1)
val vsub1 = String.sub
type instream = TextIO.instream
type outstream = TextIO.outstream

val notb = fn (x : int) => ~x - 1
val atan = Real.Math.atan
val sqrt = Real.Math.sqrt

structure Real = struct open Real fun eq(x,y) = Real.==(x,y) end