(*$import Prelude Word8Vector Int Word8Array *)
type statrep = (int * word * word * word * word * word * 
		word * int * int * int * int)
type flock_rep = int * int * Position.int * Position.int * int
type tm = (int * int * int * int * int * int * int * int * int)
type termio_rep = (word *       	(* iflags *)
		   word *       	(* oflags *)
		   word *       	(* cflags *)
		   word *       	(* lflags *)
		   Word8Vector.vector *	(* cc *)
		   word *		(* inspeed *)
		   word			(* outspeed *)
		   )

extern posix_ascTime :  (int * int * int * int * int * int * int * int * int, string) -->
extern posix_localTime :  (int * int, int * int * int * int * int * int * int * int * int) -->
extern posix_gmTime :  (int * int, int * int * int * int * int * int * int * int * int) -->
extern posix_mkTime : (int * int * int * int * int * int * int * int * int, int * int) -->
extern posix_strfTime : (string * (int * int * int * int * int * int * int * int * int), string) -->
extern posix_error_msg : (int, string) -->
extern posix_error_name : (int, string) -->
extern posix_error_num: (string, int) -->
extern posix_os_tmpname : (unit, string) -->
extern posix_os_poll : ((int * word) list, (int * int) option, (int * word) list) -->
extern posix_io_num : (string, int) -->
extern posix_io_pipe : (unit, (int * int)) -->
extern posix_io_dup : (int, int) -->
extern posix_io_dup2 : (int, int, unit) -->
extern posix_io_close : (int, unit) -->
extern posix_io_read : (int, int, Word8Vector.vector ) -->
extern posix_io_readbuf : (int, Word8Array.array, int, int, int) -->
extern posix_io_writebuf : (int, Word8Array.array, int, int, int) -->
extern posix_io_fcntl_d : (int, int, int) -->
extern posix_io_fcntl_gfd: (int, word) -->
extern posix_io_fcntl_sfd : (int, word, unit) -->
extern posix_io_fcntl_gfl : (int, word * word) -->
extern posix_io_fcntl_sfl : (int, word, unit) -->
(* due to a deficiency in the phase-splitter, externs cannot use types
   defined within the same unit *)
extern posix_io_fcntl_l : (int, int, 
			   int * int * Position.int * Position.int * int (* = flock_rep *), 
			   int * int * Position.int * Position.int * int (* = flock_rep *) ) -->
extern posix_io_lseek : (int, int, int, int) -->
extern posix_io_fsync : (int, unit) -->
extern posix_procenv_getpid : (unit, int) -->
extern posix_procenv_getppid : (unit, int) -->
extern posix_procenv_getuid : (unit, word) -->
extern posix_procenv_geteuid : (unit, word) -->
extern posix_procenv_getgid : (unit, word) -->
extern posix_procenv_getegid : (unit, word) -->
extern posix_procenv_setuid : (word, unit) -->
extern posix_procenv_setgid : (word, unit) -->
extern posix_procenv_getgroups : (unit, word list) -->
extern posix_procenv_getlogin : (unit, string) -->
extern posix_procenv_getpgrp : (unit, int) -->
extern posix_procenv_setsid : (unit, int) -->
extern posix_procenv_setpgid : (int, int, unit) -->
extern posix_procenv_uname : (unit, (string * string) list) -->

extern posix_tty_num : (string, int) -->
extern posix_tty_tcgetattr : (int, (word * word * word * word * Word8Vector.vector * word * word)
			           (* = termio_rep *) ) -->
extern posix_tty_tcsetattr : (int, int, (word * word * word * word * Word8Vector.vector * word * word)
			                (* = termio_rep *) , unit) -->
extern posix_tty_tcsendbreak : (int, int, unit) -->
extern posix_tty_tcdrain : (int, unit) -->
extern posix_tty_tcflush : (int, int, unit) -->
extern posix_tty_tcflow : (int, int, unit) -->
extern posix_tty_tcgetpgrp : (int, int) -->
extern posix_tty_tcsetpgrp : (int, int, unit) -->

extern posix_sysdb_getgrgid : (word, string * word * string list) -->
extern posix_sysdb_getgrnam : (string, string * word * string list) -->
extern posix_sysdb_getpwuid : (word, string * word * word * string * string) -->
extern posix_sysdb_getpwnam : (string, string * word * word * string * string) -->


extern posix_procenv_time : (unit, int) -->
extern posix_procenv_times : (unit, int * int * int * int * int) -->
extern posix_procenv_getenv : (string, string option) -->
extern posix_procenv_environ : (unit, string list) -->
extern posix_procenv_ctermid : (unit, string) -->
extern posix_procenv_ttyname : (int, string) -->
extern posix_procenv_isatty : (int, bool) -->


extern posix_process_num : (string, int) -->
extern posix_process_sysconf : (string, word) -->
extern posix_process_fork : (unit, int) -->
extern posix_process_exec : (string, string list, unit) -->
extern posix_process_exece : (string, string list, string list, unit) -->
extern posix_process_execp : (string, string list, unit) -->
extern posix_process_waitpid : (int, word, (int * int * int)) -->
extern posix_process_exit : (word8, unit) -->
extern posix_process_kill : (int, int, unit) -->
extern posix_process_alarm : (int, int) -->
extern posix_process_pause : (unit, unit) -->
extern posix_process_sleep : (int, int) -->
extern posix_signal_num : (string, int) -->
extern posix_filesys_num : (string, word) -->
extern posix_filesys_opendir : (string, int) -->
extern posix_filesys_readdir : (int, string) -->
extern posix_filesys_rewinddir : (int, unit) -->
extern posix_filesys_closedir : (int, unit) -->
extern posix_filesys_chdir : (string, unit) -->
extern posix_filesys_getcwd : (unit, string) -->
extern posix_filesys_openf : (string, word, word, int) -->
extern posix_filesys_umask : (word, int) -->
extern posix_filesys_link : (string, string, unit) -->
extern posix_filesys_rename : (string, string, unit) -->
extern posix_filesys_symlink : (string, string, unit) -->
extern posix_filesys_mkdir : (string, word, unit) -->
extern posix_filesys_mkfifo : (string, word, unit) -->
extern posix_filesys_unlink : (string, unit) -->
extern posix_filesys_rmdir : (string, unit) -->
extern posix_filesys_readlink : (string, string) -->
extern posix_filesys_ftruncate : (int, int, unit) -->
extern posix_filesys_stat : (string, (int * word * word * word * word * word * 
				      word * int * int * int * int) (* = statrep *) ) -->
extern posix_filesys_lstat : (string, (int * word * word * word * word * word * 
				       word * int * int * int * int) (* = statrep *) ) -->
extern posix_filesys_fstat : (int, (int * word * word * word * word * word * 
				    word * int * int * int * int) (* = statrep *) ) -->
extern posix_filesys_access : (string, word, bool) -->
extern posix_filesys_chmod : (string, word, unit) -->
extern posix_filesys_fchmod : (int, word, unit) -->
extern posix_filesys_chown : (string, word, word, unit) -->
extern posix_filesys_fchown : (int, word, word, unit) -->
extern posix_filesys_utime : (string, int, int, unit) -->
extern posix_filesys_umask : (word, word) -->
extern posix_filesys_link : (string, string, unit) -->
extern posix_filesys_rename : (string, string, unit) -->
extern posix_filesys_symlink : (string, string, unit) -->
extern posix_filesys_mkdir : (string, word, unit) -->
extern posix_filesys_mkfifo : (string, word, unit) -->
extern posix_filesys_unlink : (string, unit) -->
extern posix_filesys_rmdir : (string, unit) -->
extern posix_filesys_readlink : (string, string) -->
extern posix_filesys_ftruncate : (int, int, unit) -->
extern posix_filesys_pathconf : (string, string, (int * int)) -->
extern posix_filesys_fpathconf :  (int, string, (int * int)) -->
