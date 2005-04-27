(*
	These types belong in extern.sml.  Due to a deficiency in the
	phase splitter, externs cannot use types defined within the
	same unit.

	These types must agree with the runtime.
*)

(* ../Runtime/port/timer.c *)
type rusagerep =
	int *	(* ru_utime.tv_sec *)
	int *	(* ru_utime.tv_usec *)
	int *	(* ru_stime.tv_sec *)
	int	(* ru_stime.tv_usec *)

(* ../Runtime/port/date.c *)
type tmrep =
	int *	(* tm_sec *)
	int *	(* tm_min *)
	int *	(* tm_hour *)
	int *	(* tm_mday *)
	int *	(* tm_mon *)
	int *	(* tm_year *)
	int *	(* tm_wday *)
	int *	(* tm_yday *)
	int	(* tm_isdst *)

(* ../Runtime/port/posix_io.c *)
type flockrep =
	int *	(* l_type *)
	int *	(* l_whence *)
	int *	(* l_start *)
	int *	(* l_len *)
	int	(* l_pid *)

(* ../Runtime/port/posix_tty.c *)
type termiorep =
	word *	(* c_iflag *)
	word *	(* c_oflag *)
	word *	(* c_cflag *)
	word *	(* c_lflag *)
	(* c_cc is allocated separately *)
	word *	(* ispeed *)
	word	(* ospeed *)

(* ../Runtime/port/posix_procenv.c *)
type timesrep =
	int *	(* elapsed real time *)
	int *	(* tms_utime *)
	int *	(* tms_stime *)
	int *	(* tms_cutime *)
	int	(* tms_cstime *)

(* ../Runtime/port/posix_process.c *)
type waitpidrep =
	int *	(* pid *)
	int * int	(* (0,sts) if exited; (1,sig) if signalled; (2,sig) if stopped *)

(* ../Runtime/port/posix_filesys.c *)
type statrep =
	int *	(* st_mode & S_IFMT *)
	word *	(* st_mode & (S_IRWXU | S_IRWXG | S_IRWXO | S_ISUID | S_ISGID) *)
	word *	(* st_ino *)
	word *	(* st_dev *)
	word *	(* st_nlink *)
	word *	(* st_uid *)
	word *	(* st_gid *)
	int *	(* st_size *)
	int *	(* st_atime *)
	int *	(* st_mtime *)
	int	(* st_ctime *)
