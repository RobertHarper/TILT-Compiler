(*$import Prelude POSIX_extern String SysWord *)

signature PRE_POSIX =
sig

    eqtype uid
    val uidToWord : uid -> SysWord.word
    val wordToUid : SysWord.word -> uid
	
    eqtype gid
    val gidToWord : gid -> SysWord.word
    val wordToGid : SysWord.word -> gid

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
    val omodeFromWord : SysWord.word -> open_mode
    val omodeToWord : open_mode -> SysWord.word

    val sysconf : string -> SysWord.word

end
	
structure PrePosix :> PRE_POSIX =
struct

    datatype uid = UID of word
    datatype gid = GID of word
    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

    fun uidToWord (UID i) = i
    fun wordToUid i = UID i

    fun gidToWord (GID i) = i
    fun wordToGid i = GID i

    fun w_osval (str : string) : word = Ccall(posix_filesys_num,str)

    val o_rdonly = w_osval "O_RDONLY"
    val o_wronly = w_osval "O_WRONLY"
    val o_rdwr = w_osval "O_RDWR"

    fun omodeFromWord omode =
          if omode = o_rdonly then O_RDONLY
          else if omode = o_wronly then O_WRONLY
          else if omode = o_rdwr then O_RDWR
          else raise TiltExn.LibFail (String.^ ("PrePosix.omodeFromWord: unknown mode ",
						(SysWord.toString omode)))

    fun omodeToWord O_RDONLY = o_rdonly
      | omodeToWord O_WRONLY = o_wronly
      | omodeToWord O_RDWR = o_rdwr

    fun sysconf (s : string) : SysWord.word = Ccall(posix_process_sysconf,s)

end    
