extern posix_date_asctime :  (tmrep, string) -->
extern posix_date_localtime :  (int, tmrep cresult) -->
extern posix_date_gmtime :  (int, tmrep cresult) -->
extern posix_date_mktime : (tmrep, int cresult) -->
extern posix_date_strftime : (string, tmrep, string) -->

extern posix_error_msg : (int, string) -->
extern posix_error_name : (int, string cresult) -->
extern posix_error_num: (string, int cresult) -->

extern posix_os_tmpname : (unit, string cresult) -->
extern posix_os_poll : ((int * word) list, (int * int) option, (int * word) list cresult) -->

extern posix_io_num : (string, int cresult) -->
extern posix_io_pipe : (unit, (int * int) cresult) -->
extern posix_io_dup : (int, int cresult) -->
extern posix_io_dup2 : (int, int, unit cresult) -->
extern posix_io_close : (int, unit cresult) -->
extern posix_io_read : (int, int, TiltPrim.word8vector cresult) -->
extern posix_io_readbuf : (int, TiltPrim.word8array, int, int, int cresult) -->
extern posix_io_writebuf : (int, TiltPrim.word8array, int, int, int cresult) -->
extern posix_io_fcntl_d : (int, int, int cresult) -->
extern posix_io_fcntl_gfd: (int, word cresult) -->
extern posix_io_fcntl_sfd : (int, word, unit cresult) -->
extern posix_io_fcntl_gfl : (int, (word * word) cresult) -->
extern posix_io_fcntl_sfl : (int, word, unit cresult) -->
extern posix_io_fcntl_l : (int, int, flockrep, flockrep cresult) -->
extern posix_io_lseek : (int, int, int, int cresult) -->
extern posix_io_fsync : (int, unit cresult) -->

extern posix_procenv_getpid : (unit, int) -->
extern posix_procenv_getppid : (unit, int) -->
extern posix_procenv_getuid : (unit, word) -->
extern posix_procenv_geteuid : (unit, word) -->
extern posix_procenv_getgid : (unit, word) -->
extern posix_procenv_getegid : (unit, word) -->
extern posix_procenv_setuid : (word, unit cresult) -->
extern posix_procenv_setgid : (word, unit cresult) -->
extern posix_procenv_getgroups : (unit, word list cresult) -->
extern posix_procenv_getlogin : (unit, string cresult) -->
extern posix_procenv_getpgrp : (unit, int) -->
extern posix_procenv_setsid : (unit, int cresult) -->
extern posix_procenv_setpgid : (int, int, unit cresult) -->
extern posix_procenv_uname : (unit, (string * string) list cresult) -->

extern posix_tty_num : (string, int cresult) -->
extern posix_tty_tcgetattr : (int, termiorep cresult) -->
extern posix_tty_tcsetattr : (int, int, termiorep, unit cresult) -->
extern posix_tty_tcsendbreak : (int, int, unit cresult) -->
extern posix_tty_tcdrain : (int, unit cresult) -->
extern posix_tty_tcflush : (int, int, unit cresult) -->
extern posix_tty_tcflow : (int, int, unit cresult) -->
extern posix_tty_tcgetpgrp : (int, int cresult) -->
extern posix_tty_tcsetpgrp : (int, int, unit cresult) -->

extern posix_sysdb_getgrgid : (word, grouprep cresult) -->
extern posix_sysdb_getgrnam : (string, grouprep cresult) -->
extern posix_sysdb_getpwuid : (word, passwdrep cresult) -->
extern posix_sysdb_getpwnam : (string, passwdrep cresult) -->

extern posix_procenv_time : (unit, int cresult) -->
extern posix_procenv_times : (unit, timesrep cresult) -->
extern posix_procenv_getenv : (string, string option) -->
extern posix_procenv_environ : (unit, string list) -->
extern posix_procenv_ctermid : (unit, string cresult) -->
extern posix_procenv_ttyname : (int, string cresult) -->
extern posix_procenv_isatty : (int, bool) -->

extern posix_process_num : (string, int cresult) -->
extern posix_process_sysconf : (string, word cresult) -->
extern posix_process_fork : (unit, int cresult) -->
extern posix_process_exec : (string, string list, exn) -->
extern posix_process_exece : (string, string list, string list, exn) -->
extern posix_process_execp : (string, string list, exn) -->
extern posix_process_waitpid : (int, word, waitpidrep cresult) -->
extern posix_process_exit : (TiltPrim.uint8, exn) -->
extern posix_process_kill : (int, int, unit cresult) -->
extern posix_process_alarm : (int, int) -->
extern posix_process_pause : (unit, unit) -->
extern posix_process_sleep : (int, int) -->

extern posix_signal_num : (string, int cresult) -->

extern posix_filesys_num : (string, word cresult) -->
extern posix_filesys_opendir : (string, int (*DIR*) cresult) -->
extern posix_filesys_readdir : (int (*DIR*), string cresult) -->
extern posix_filesys_rewinddir : (int (*DIR*), unit) -->
extern posix_filesys_closedir : (int, unit cresult) -->
extern posix_filesys_chdir : (string, unit cresult) -->
extern posix_filesys_getcwd : (unit, string cresult) -->
extern posix_filesys_openf : (string, word, word, int cresult) -->
extern posix_filesys_umask : (word, word) -->
extern posix_filesys_link : (string, string, unit cresult) -->
extern posix_filesys_rename : (string, string, unit cresult) -->
extern posix_filesys_symlink : (string, string, unit cresult) -->
extern posix_filesys_mkdir : (string, word, unit cresult) -->
extern posix_filesys_mkfifo : (string, word, unit cresult) -->
extern posix_filesys_unlink : (string, unit cresult) -->
extern posix_filesys_rmdir : (string, unit cresult) -->
extern posix_filesys_readlink : (string, string cresult) -->
extern posix_filesys_ftruncate : (int, int, unit cresult) -->
extern posix_filesys_stat : (string, statrep cresult) -->
extern posix_filesys_lstat : (string, statrep cresult) -->
extern posix_filesys_fstat : (int, statrep cresult) -->
extern posix_filesys_access : (string, word, bool cresult) -->
extern posix_filesys_chmod : (string, word, unit cresult) -->
extern posix_filesys_fchmod : (int, word, unit cresult) -->
extern posix_filesys_chown : (string, word, word, unit cresult) -->
extern posix_filesys_fchown : (int, word, word, unit cresult) -->
extern posix_filesys_utime : (string, int, int, unit cresult) -->
extern posix_filesys_pathconf : (string, string, (int * int) cresult) -->
extern posix_filesys_fpathconf :  (int, string, (int * int) cresult) -->
