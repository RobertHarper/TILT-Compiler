(* Required basis modules. *)
(* See the introductory chapter of the basis library spec. *)

signature MONO_ARRAY' =
sig
    include MONO_ARRAY
    sharing type elem = Vector.elem
end

signature REAL' =
sig
    include REAL
    sharing type real = Math.real
end

signature STRING' =
sig
    include STRING
    sharing type string = Char.string
end

signature SUBSTRING' =
sig
    include SUBSTRING
    sharing type String.string = String.Char.string
end

signature REQUIRED =
sig

    structure General : GENERAL
	
    structure Option : OPTION
    structure Bool : BOOL

    structure SML90 : SML90

    structure Char : CHAR
    structure String : STRING'
    structure Substring : SUBSTRING'
        sharing String.Char = Char
        sharing Substring.String = String

    structure StringCvt : STRING_CVT
    structure Byte : BYTE

    structure Int : INTEGER
    structure LargeInt : INTEGER
    structure Position : INTEGER
	
    structure Word : WORD
    structure Word8 : WORD
    structure LargeWord : WORD
	
    structure Real : REAL'
    structure LargeReal : REAL'
    structure Math : MATH
    structure IEEEReal : IEEE_REAL
	sharing Real.Math = Math
	
    structure List : LIST
    structure ListPair : LIST_PAIR
	
    structure Vector : VECTOR
    structure Word8Vector : MONO_VECTOR
    structure CharVector : MONO_VECTOR
	    
    structure Array : ARRAY
    structure Word8Array : MONO_ARRAY'
    structure CharArray : MONO_ARRAY'
	sharing Word8Array.Vector = Word8Vector
	sharing CharArray.Vector = CharVector
	
    structure IO : IO

    structure BinPrimIOCopy : PRIM_IO
    structure BinIO : BIN_IO		(* StreamIO.vector, StreamIO.elem concrete *)
	where type StreamIO.reader = BinPrimIOCopy.reader
	where type StreamIO.writer = BinPrimIOCopy.writer
    structure BinPrimIO : PRIM_IO	(* reader, writer concrete *)
	where type vector = BinIO.StreamIO.vector
	where type elem = BinIO.StreamIO.elem
    sharing type BinPrimIO.array = CharArray.array
    sharing type BinPrimIO.pos = BinIO.StreamIO.pos = Position.int

    structure TextPrimIOCopy : PRIM_IO
    structure TextIO : TEXT_IO		(* StreamIO.vector, StreamIO.elem concrete *)
	where type StreamIO.reader = TextPrimIOCopy.reader
	where type StreamIO.writer = TextPrimIOCopy.writer
    structure TextPrimIO : PRIM_IO	(* reader, writer concrete *)
	where type vector = TextIO.StreamIO.vector
	where type elem = TextIO.StreamIO.elem
    sharing type TextPrimIO.array = CharArray.array
    sharing type TextPrimIO.pos = TextIO.StreamIO.pos = Position.int

    structure OS : OS
    structure OsFileSys : OS_FILE_SYS
    structure OsIO : OS_IO
    structure OsPath : OS_PATH
    structure OsProcess : OS_PROCESS
    structure CommandLine : COMMAND_LINE
    structure Date : DATE
    structure Time : TIME
    structure Timer : TIMER

    structure Top : TOP

    sharing type Top.unit = General.unit
    sharing type Top.int = Int.int = LargeInt.int
    sharing type Top.word = Word.word = LargeWord.word
    sharing type Top.real = Real.real = LargeReal.real
    sharing type Top.char = Char.char = CharVector.elem (* = CharArray.elem *)
    sharing type            Word8.word = Word8Vector.elem (* = Word8Array.elem *)
    sharing type Top.string = Char.string = String.string = CharVector.vector
    sharing type Top.exn = General.exn
    sharing type Top.array = Array.array
    sharing type Top.vector = Vector.vector
    sharing type Top.bool = Bool.bool
    sharing type Top.option = Option.option
    sharing type Top.order = General.order
    sharing type Top.list = List.list
	
end
