(*$import Array BinIO BinPrimIO Bool Byte Char CharArray CharVector CommandLine Date General IEEEReal Int IO LargeInt LargeReal LargeWord List ListPair Math Option OS Position Real SML90 String StringCvt Substring TextIO TextPrimIO Time Timer Vector Word Word8 Word8Array Word8Vector REQUIRED Top *)

(* Required basis modules. *)
(* See the introductory chapter of the basis library spec. *)

(* These where types express some sharing involving concrete types.  We
 * can't directly express the sharing between the concrete
 * Required.BinIO.StreamIO.vector and the abstract
 * Required.Word8Vector.vector.  We can make the latter concrete too
 * with the same definition as the former.  *)

structure Required :> REQUIRED where type Word8Vector.vector = Word8Vector.vector
				 and type Word8.word = Word8.word
				 and type CharVector.vector = CharVector.vector
				 and type Char.char = Char.char =
struct

    structure General = General
    structure Option = Option
    structure Bool = Bool
    structure SML90 = SML90

	
    structure Char = Char
    structure String = String
    structure Substring = Substring
    structure StringCvt = StringCvt
    structure Byte = Byte

    structure Int = Int
    structure LargeInt = LargeInt
    structure Position = Position
	
    structure Word = Word
    structure Word8 = Word8
    structure LargeWord = LargeWord
	
    structure Real = Real
    structure LargeReal = LargeReal
    structure Math = Math
    structure IEEEReal = IEEEReal
	
    structure List = List
    structure ListPair = ListPair
	
    structure Vector = Vector
    structure Word8Vector = Word8Vector
    structure CharVector = CharVector
    structure Array = Array
    structure Word8Array = Word8Array
    structure CharArray = CharArray
	
    structure IO = IO
    structure TextIO = TextIO
    structure BinIO = BinIO
	
    structure BinPrimIO = BinPrimIO
    structure TextPrimIO = TextPrimIO

    structure BinPrimIOCopy = BinPrimIO
    structure TextPrimIOCopy = TextPrimIO
	
    structure OS = OS
    structure OsFileSys = OS.FileSys
    structure OsIO = OS.IO
    structure OsPath = OS.Path
    structure OsProcess = OS.Process
    structure CommandLine = CommandLine
    structure Date = Date
    structure Time = Time
    structure Timer = Timer

    structure Top = Top
end
