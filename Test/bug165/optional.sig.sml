signature POSIX_SYS_DB = sig end
signature POSIX = sig structure SysDB : POSIX_SYS_DB end

signature OPTIONAL =
sig
	structure Posix : POSIX
	structure PosixSysDB : POSIX_SYS_DB
		sharing Posix.SysDB = PosixSysDB
end
