(* This program reverses the lines it receives from stdin,
   like the UNIX utility "tac". It assumes that no two
   consecutive lines have a total length >4096 bytes.

   It is tuned for speed for the Programming Languages Shootout Page;
   it is interesting to compare it with the natural solution using
   lists. (Also, using Unsafe.Word8Array.create and
   Unsafe.Word8Array.sub is a significant win in my tests.) 

   Code by Tom 7, 2001. Use, modify, and distribute freely.

*)

local
val bufsize = 4096
val rdbufsize = 4096

val stdout = Posix.FileSys.wordToFD 0w1
val stdin = Posix.FileSys.wordToFD 0w0

datatype block = END
               | MORE of int ref * Word8Array.array * block

val buff = Word8Array.array (rdbufsize, 0w0)

fun out END = ()
  | out (MORE (ir as ref i, a, next)) =
  let in
    Posix.IO.writeArr (stdout, {buf=a, i=i, sz=NONE});
    out next
  end

fun rd (start, len, count, b) =
  if (start + len) >= count then 
    (* done with this block. 
       Copy from start to the end of the array into
       buff, then return the starting index into buff. *)
    let in
      Word8Array.copy {di=0,
                       dst=buff,
                       src=buff,
                       len=SOME len,
                       si=start};
      (b, len)
    end
  else
    if Word8Array.sub(buff, start + len) = 0w10 then
      (* found newline *)
      case b of MORE(ir as ref i, a, _) =>
        if i > len then
            let in
              (* enough room *)
              Word8Array.copy {di=i-len - 1,
                               dst=a,
                               len=SOME(len + 1),
                               si=start,
                               src=buff};
              ir := i - (len + 1);
              
              rd(start + len + 1, 0, count, b)
            end
          else (* not enough room *)
            let
              (* going to need a new buffer *)
              val na = Word8Array.array (bufsize, 0w0)
              val l = (len + 1) - i
            in
              (* put the tail in whatever room is left *)
              Word8Array.copy {di=0,
                               dst=a,
                               len=SOME i,
                               si=(start + len + 1) - i,
                               src=buff};
              
              (* put the head in a new buffer *)
              Word8Array.copy {di=bufsize - l,
                               dst=na,
                               len=SOME l,
                               si=start,
                               src=buff};
              ir := 0;
              rd(start + len + 1, 0, count, MORE(ref (bufsize - l), na, b))
            end
        else rd (start, len + 1, count, b)
            
fun loop (b, s) =
  let 
    val count = Posix.IO.readArr (stdin, 
                                  {buf=buff, i=s, sz=SOME (rdbufsize-s)})
    val (bb, bs) = rd (0, s, count + s, b)
  in
    case count of
      0 => out bb
    | _ => loop (bb, bs)
  end

in
  fun runReverse () = loop (MORE(ref bufsize, Word8Array.array (bufsize,0w0), END), 0)
(*val _ = loop (MORE(ref bufsize, Word8Array.array (bufsize,0w0), END), 0)*)
end
