(*
	These types belong in extern.sml.  Due to a deficiency in the
	phase splitter, externs cannot use types defined within the
	same unit.

	These types must agree with the runtime.
*)

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

type flockrep =
	int *	(* l_type *)
	int *	(* l_whence *)
	int *	(* l_start *)
	int *	(* l_len *)
	int	(* l_pid *)

type termiorep =
	word *	(* iflags *)
	word *	(* oflags *)
	word *	(* cflags *)
	word *	(* lflags *)
	TiltPrim.word8vector *	(* cc *)
	word *	(* inspeed *)
	word	(* outspeed *)

type grouprep =
	string *	(* gr_name *)
	word *	(* gr_gid *)
	string list	(* gr_mem *)

type passwdrep =
	string *	(* pw_name *)
	word *	(* pw_uid *)
	word *	(* pw_gid *)
	string *	(* pw_dir *)
	string	(* pw_shell *)

type timesrep =
	int *	(* elapsed real time *)
	int *	(* tms_utime *)
	int *	(* tms_stime *)
	int *	(* tms_cutime *)
	int	(* tms_cstime *)

type waitpidrep =
	int *	(* pid *)
	int * int	(* (0,sts) if exited; (1,sig) if signalled; (2,sig) if stopped *)

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
