(*
	C library functions used by the basis.
*)

(* libc; used by Numeric/real64.sml *)
extern sqrt : (real, real) -->
extern exp : (real, real) -->
extern ln : (real, real) -->
extern log10 : (real, real) -->
extern sin : (real, real) -->
extern cos : (real, real) -->
extern tan : (real, real) -->
extern atan : (real, real) -->
extern asin : (real, real) -->
extern acos : (real, real) -->
extern tanh : (real, real) -->
extern sinh : (real, real) -->
extern cosh : (real, real) -->

(* ../Runtime/port/real.c *)
extern real_logb : (real, int) -->
extern real_scalb : (real, int, real) -->

(*
	../Runtime/sparc/fc.c
	../Runtime/talx86/fc.c
*)
extern setfc : (cerr, int, int, unit) -->
extern getfc : (cerr, int * int) -->

(*
	../Runtime/sparc/commandline.c
	../Runtime/talx86/commandline.c
*)
extern commandline_name : (unit, string) -->
extern commandline_arguments_size : (unit, int) -->
extern commandline_arguments_nth : (int, string) -->

(* ../Runtime/port/time.c *)
extern time_gettimeofday : (cerr, int * int) -->

(* ../Runtime/port/timer.c *)
extern timer_getrusage_self : (cerr, rusagerep) -->
extern timer_ftime : (unit, int * int) -->

(* ../Runtime/port/date.c *)
extern date_asctime : (tmrep, string) -->
extern date_localtime : (cerr, int, tmrep) -->
extern date_gmtime : (cerr, int, tmrep) -->
extern date_mktime : (cerr, tmrep, int) -->
extern date_strftime : (string, tmrep, string) -->

(* ../Runtime/port/os_filesys.c *)
extern os_filesys_tmpname : (cerr, string) -->

(* ../Runtime/port/os_io.c *)
extern os_io_poll : (cerr, int, (int * word) list, (int * int) option, uct) -->
extern os_io_poll_nth : (uct, int, int * word) -->
extern os_io_poll_free : (uct, unit) -->

(* ../Runtime/port/pre_posix.c *)
extern pre_posix_omode : (cerr, string, word) -->
extern pre_posix_sysconf : (cerr ,string, word) -->

(* ../Runtime/port/posix_error.c *)
extern posix_error_name : (cerr, int, string) -->
extern posix_error_num: (cerr, string, int) -->

(* ../Runtime/port/posix_io.c *)
extern posix_io_num : (cerr, string, int) -->
extern posix_io_pipe : (cerr, int * int) -->
extern posix_io_dup : (cerr, int, int) -->
extern posix_io_dup2 : (cerr, int, int, unit) -->
extern posix_io_close : (cerr, int, unit) -->
extern posix_io_read : (cerr, int, int, string) -->
extern posix_io_readbuf : (cerr, int, TiltPrim.word8array, int, int, int) -->
extern posix_io_writebuf : (cerr, int, TiltPrim.word8array, int, int, int) -->
extern posix_io_fcntl_d : (cerr, int, int, int) -->
extern posix_io_fcntl_gfd: (cerr, int, word) -->
extern posix_io_fcntl_sfd : (cerr, int, word, unit) -->
extern posix_io_fcntl_gfl : (cerr, int, word * word) -->
extern posix_io_fcntl_sfl : (cerr, int, word, unit) -->
extern posix_io_fcntl_l : (cerr, int, int, flockrep, flockrep) -->
extern posix_io_lseek : (cerr, int, int, int, int) -->
extern posix_io_fsync : (cerr, int, unit) -->

(* ../Runtime/port/posix_procenv.c *)
extern posix_procenv_getpid : (unit, int) -->
extern posix_procenv_getppid : (unit, int) -->
extern posix_procenv_getuid : (unit, word) -->
extern posix_procenv_geteuid : (unit, word) -->
extern posix_procenv_getgid : (unit, word) -->
extern posix_procenv_getegid : (unit, word) -->
extern posix_procenv_setuid : (cerr, word, unit) -->
extern posix_procenv_setgid : (cerr, word, unit) -->
extern posix_procenv_getgroups : (cerr, uct) -->
extern posix_procenv_getgroups_size : (uct, int) -->
extern posix_procenv_getgroups_nth : (uct, int, word) -->
extern posix_procenv_getgroups_free : (uct, unit) -->
extern posix_procenv_getlogin : (cerr, string) -->
extern posix_procenv_getpgrp : (unit, int) -->
extern posix_procenv_setsid : (cerr, int) -->
extern posix_procenv_setpgid : (cerr, int, int, unit) -->
extern posix_procenv_uname : (cerr, uct) -->
extern posix_procenv_uname_sysname : (uct, string) -->
extern posix_procenv_uname_nodename : (uct, string) -->
extern posix_procenv_uname_release : (uct, string) -->
extern posix_procenv_uname_version : (uct, string) -->
extern posix_procenv_uname_machine : (uct, string) -->
extern posix_procenv_uname_free : (uct, unit) -->
extern posix_procenv_time : (cerr, int) -->
extern posix_procenv_times : (cerr, timesrep) -->
extern posix_procenv_getenv : (string, string option) -->
extern posix_procenv_environ_size : (unit, int) -->
extern posix_procenv_environ_nth : (int, string) -->
extern posix_procenv_ctermid : (cerr, string) -->
extern posix_procenv_ttyname : (cerr, int, string) -->
extern posix_procenv_isatty : (int, bool) -->

(* ../Runtime/port/posix_tty.c *)
extern posix_tty_num : (cerr, string, int) -->
extern posix_tty_tcgetattr : (cerr, int, uct) -->
extern posix_tty_tcgetattr_termiorep : (uct, termiorep) -->
extern posix_tty_tcgetattr_cc : (uct, string) -->
extern posix_tty_tcgetattr_free : (uct, unit) -->
extern posix_tty_tcsetattr : (cerr, int, int, termiorep, string, unit) -->
extern posix_tty_tcsendbreak : (cerr, int, int, unit) -->
extern posix_tty_tcdrain : (cerr, int, unit) -->
extern posix_tty_tcflush : (cerr, int, int, unit) -->
extern posix_tty_tcflow : (cerr, int, int, unit) -->
extern posix_tty_tcgetpgrp : (cerr, int, int) -->
extern posix_tty_tcsetpgrp : (cerr, int, int, unit) -->

(* ../Runtime/port/posix_sysdb.c *)
extern posix_sysdb_getgrgid : (cerr, word, uct) -->
extern posix_sysdb_getgrnam : (cerr, string, uct) -->
extern posix_sysdb_gr_name : (uct, string) -->
extern posix_sysdb_gr_gid : (uct, word) -->
extern posix_sysdb_gr_mem_size : (uct, int) -->
extern posix_sysdb_gr_mem_nth : (uct, int, string) -->
extern posix_sysdb_gr_free : (uct, unit) -->
extern posix_sysdb_getpwuid : (cerr, word, uct) -->
extern posix_sysdb_getpwnam : (cerr, string, uct) -->
extern posix_sysdb_pw_name : (uct, string) -->
extern posix_sysdb_pw_uid_gid : (uct, word * word) -->
extern posix_sysdb_pw_dir : (uct, string) -->
extern posix_sysdb_pw_shell : (uct, string) -->
extern posix_sysdb_pw_free : (uct, unit) -->

(* ../Runtime/port/posix_process.c *)
extern posix_process_num : (cerr, string, int) -->
extern posix_process_fork : (cerr, int) -->
extern posix_process_exec : (string, string list, int) -->
extern posix_process_exece : (string, string list, string list, int) -->
extern posix_process_execp : (string, string list, int) -->
extern posix_process_waitpid : (cerr, int, word, waitpidrep) -->
extern posix_process_exit : (TiltPrim.uint8, int) -->
extern posix_process_kill : (cerr, int, int, unit) -->
extern posix_process_alarm : (int, int) -->
extern posix_process_pause : (unit, unit) -->
extern posix_process_sleep : (int, int) -->

(* ../Runtime/port/posix_signal.c *)
extern posix_signal_num : (cerr, string, int) -->

(* ../Runtime/port/posix_filesys.c *)
extern posix_filesys_num : (cerr, string, word) -->
extern posix_filesys_opendir : (cerr, string, uct) -->
extern posix_filesys_readdir : (cerr, uct, string) -->
extern posix_filesys_rewinddir : (uct, unit) -->
extern posix_filesys_closedir : (cerr, uct, unit) -->
extern posix_filesys_chdir : (cerr, string, unit) -->
extern posix_filesys_getcwd : (cerr, string) -->
extern posix_filesys_openf : (cerr, string, word, word, int) -->
extern posix_filesys_umask : (word, word) -->
extern posix_filesys_link : (cerr, string, string, unit) -->
extern posix_filesys_rename : (cerr, string, string, unit) -->
extern posix_filesys_symlink : (cerr, string, string, unit) -->
extern posix_filesys_mkdir : (cerr, string, word, unit) -->
extern posix_filesys_mkfifo : (cerr, string, word, unit) -->
extern posix_filesys_unlink : (cerr, string, unit) -->
extern posix_filesys_rmdir : (cerr, string, unit) -->
extern posix_filesys_readlink : (cerr, string, string) -->
extern posix_filesys_ftruncate : (cerr, int, int, unit) -->
extern posix_filesys_stat : (cerr, string, statrep) -->
extern posix_filesys_lstat : (cerr, string, statrep) -->
extern posix_filesys_fstat : (cerr, int, statrep) -->
extern posix_filesys_access : (cerr, string, word, bool) -->
extern posix_filesys_chmod : (cerr, string, word, unit) -->
extern posix_filesys_fchmod : (cerr, int, word, unit) -->
extern posix_filesys_chown : (cerr, string, word, word, unit) -->
extern posix_filesys_fchown : (cerr, int, word, word, unit) -->
extern posix_filesys_utime : (cerr, string, int, int, unit) -->
extern posix_filesys_pathconf_name : (cerr, string, int) -->
extern posix_filesys_pathconf : (cerr, string, int, word) -->
extern posix_filesys_fpathconf : (cerr, int, int, word) -->
