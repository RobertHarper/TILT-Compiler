(*$import PACK_WORD POSIX POSIX_ERROR POSIX_FILE_SYS POSIX_FLAGS POSIX_IO POSIX_PROC_ENV POSIX_PROCESS POSIX_SIGNAL POSIX_SYS_DB POSIX_TTY UNIX REQUIRED INTEGER WORD MONO_ARRAY MONO_VECTOR *)

(* Optional basis modules. *)
(* See the introductory chapter of the basis library spec. *)

signature OPTIONAL =
sig

(*    structure Int8 : INTEGER *)
(*    structure Int16 : INTEGER *)
    structure Int32 : INTEGER
(*    structure FixedInt : INTEGER *)
(*    structure IntInf : INTEGER  --  sharing with LargeInt *)

(*    structure Word16 : WORD *)
    structure Word31 : WORD
    structure Word32 : WORD

(*    structure Real32 : REAL' *)
    structure Real64 : REAL'

(*    structure BoolVector : MONO_VECTOR *)
(*    structure IntVector : MONO_VECTOR *)
(*    structure Int8Vector : MONO_VECTOR *)
(*    structure Int16Vector : MONO_VECTOR *)
(*    structure Int32Vector : MONO_VECTOR *)
(*    structure WordVector : MONO_VECTOR *)
(*    structure Word16Vector : MONO_VECTOR *)
(*    structure Word31Vector : MONO_VECTOR *)
(*    structure Word32Vector : MONO_VECTOR *)
(*    structure RealVector : MONO_VECTOR *)
(*    structure Real32Vector : MONO_VECTOR *)
(*    structure Real64Vector : MONO_VECTOR *)
	
(*    structure BoolArray : MONO_ARRAY' *)
(*    structure IntArray : MONO_ARRAY' *)
(*    structure Int8Array : MONO_ARRAY' *)
(*    structure Int16Array : MONO_ARRAY' *)
(*    structure Int32Array : MONO_ARRAY' *)
(*    structure WordArray : MONO_ARRAY' *)
(*    structure Word16Array : MONO_ARRAY' *)
(*    structure Word31Array : MONO_ARRAY' *)
(*    structure Word32Array : MONO_ARRAY' *)
(*    structure RealArary : MONO_ARRAY' *)
(*    structure Real32Array : MONO_ARRAY' *)
(*    structure Real64Array : MONO_ARRAY' *)

(*    structure Array2 : ARRAY2 *)
(*    structure BoolArray2 : MONO_ARRAY2 *)
(*    structure CharArray2 : MONO_ARRAY2 *)
(*    structure IntArray2 : MONO_ARRAY2 *)
(*    structure Int8Array2 : MONO_ARRAY2 *)
(*    structure Int16Array2 : MONO_ARRAY2 *)
(*    structure Int32Array2 : MONO_ARRAY2 *)
(*    structure WordArray2 : MONO_ARRAY2 *)
(*    structure Word8Array2 : MONO_ARRAY2 *)
(*    structure Word16Array2 : MONO_ARRAY2 *)
(*    structure Word31Array2 : MONO_ARRAY2 *)
(*    structure Word32Array2 : MONO_ARRAY2 *)
(*    structure RealArray2 : MONO_ARRAY2 *)
(*    structure Real32Array2 : MONO_ARRAY2 *)
(*    structure Real64Array2 : MONO_ARRAY2 *)

(*    structure PackRealBig : PACK_REAL *)
(*    structure PackRealLittle : PACK_REAL *)
(*    structure PackReal32Big : PACK_REAL *)
(*    structure PackReal32Little : PACK_REAL *)
(*    structure PackReal64Big : PACK_REAL *)
(*    structure PackReal64Little : PACK_REAL *)

(*    structure Pack8Big : PACK_WORD *)
(*    structure Pack8Little : PACK_WORD *)
(*    structure Pack16Big : PACK_WORD *)
(*    structure Pack16Little : PACK_WORD *)
(*    structure Pack31Big : PACK_WORD *)
(*    structure Pack31Little : PACK_WORD *)
    structure Pack32Big : PACK_WORD
(*    structure Pack32Little : PACK_WORD *)

    structure Unix : UNIX
	
    structure SysWord : WORD
    structure Posix : POSIX
    structure PosixError : POSIX_ERROR
	sharing PosixError = Posix.Error
    structure PosixFileSys : POSIX_FILE_SYS
	sharing PosixFileSys = Posix.FileSys
    structure PosixIO : POSIX_IO
	sharing PosixIO = Posix.IO
    structure PosixProcEnv : POSIX_PROC_ENV
	sharing PosixProcEnv = Posix.ProcEnv
    structure PosixProcess : POSIX_PROCESS
	sharing PosixProcess = Posix.Process
    structure PosixSignal : POSIX_SIGNAL
	sharing PosixSignal = Posix.Signal
    structure PosixSysDB : POSIX_SYS_DB
	sharing Posix.SysDB = Posix.SysDB
    structure PosixTTY : POSIX_TTY
	sharing PosixTTY = Posix.TTY
	
    (* functor PrimIO *)
    (* functor StreamIO *)
    (* functor ImperativeIO *)

(* Provide together
    structure MultiByte : MULTIBYTE
    structure Locale : LOCALE
    structure WideChar : CHAR
    structure WideString : STRING'
    structure WideSubstring : SUBSTRING'
    structure WideCharVector : MONO_VECTOR
    structure WideCharArray : MONO_ARRAY'
*)
	
(*    structure WideCharArray2 : MONO_ARRAY2 *)
(*    structure WideTextPrimIO : PRIM_IO *)
(*    structure WideTextIO : TEXT_IO *)

    sharing type Posix.Process.pid = Posix.ProcEnv.pid = Posix.TTY.pid
    sharing type Posix.Process.signal = Posix.Signal.signal
    sharing type Posix.ProcEnv.file_desc = Posix.FileSys.file_desc = Posix.TTY.file_desc = Posix.IO.file_desc
    sharing type Posix.FileSys.open_mode = Posix.IO.open_mode
    sharing type Posix.ProcEnv.uid = Posix.FileSys.uid = Posix.SysDB.uid
    sharing type Posix.ProcEnv.gid = Posix.FileSys.gid = Posix.SysDB.gid
    sharing type Posix.FileSys.O.flags = Posix.IO.O.flags
end

