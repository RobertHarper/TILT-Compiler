(* The basis seems to be missing a string stream type *)
signature STRING_STREAM =
sig
   type streambuf 

   val mkStreamBuf   : unit -> streambuf
   val getString     : streambuf -> string
   val setString     : streambuf * string -> unit
   val openStringOut : streambuf -> TextIO.outstream

end

structure StringStream :> STRING_STREAM =
struct

   structure TextIO = TextIO
   structure TextPrimIO = TextPrimIO

   type streambuf = string list ref

   fun mkStreamBuf ()    = ref [] : streambuf
   fun getString (ref s) = String.concat(List.rev s)
   fun setString (r,s)   = r := [s]     

   fun openStringOut buffer =
   let 
       val writer =
           TextPrimIO.WR 
                { name       = "string stream",
                chunkSize  = 512,
                writeVec   = SOME (fn {buf, i, sz = SOME n} => 
                                      (buffer := buf :: !buffer; n)
                                   |  {buf, i, sz = NONE} =>
                                      (buffer := buf :: !buffer; size buf)),
                writeArr   = NONE,
                writeVecNB = NONE,
                writeArrNB = NONE,
                block      = NONE,
                canOutput  = NONE,
                getPos     = NONE,
                setPos     = NONE,
                endPos     = NONE,
                verifyPos  = NONE,
                close      = fn () => (),
                ioDesc     = NONE
              }
       val outstream = TextIO.mkOutstream 
              (TextIO.StreamIO.mkOutstream (writer,IO.NO_BUF))
   in  outstream
   end

end

