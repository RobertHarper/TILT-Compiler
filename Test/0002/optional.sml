(*$import Int32 Word31 Word32 Real64 Pack32Big Unix SysWord Posix OPTIONAL *)

(* Optional basis modules. *)
(* See the introductory chapter of the basis library spec. *)

structure Optional :> OPTIONAL =
struct
    structure Int32 = Int32
    structure Word31 = Word31
    structure Word32 = Word32
    structure Real64 = Real64
    structure Pack32Big = Pack32Big
    structure Unix = Unix
    structure SysWord = SysWord
    structure Posix = Posix
    structure PosixError = Posix.Error
    structure PosixFileSys = Posix.FileSys
    structure PosixIO = Posix.IO
    structure PosixProcEnv = Posix.ProcEnv
    structure PosixProcess = Posix.Process
    structure PosixSignal = Posix.Signal
    structure PosixSysDB = Posix.SysDB
    structure PosixTTY = Posix.TTY
end
