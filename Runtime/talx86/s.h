#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

typedef unsigned char uint8;
typedef int int32;
typedef unsigned int uint32;
typedef double float64;

typedef unsigned long reg_t;

/*
	Some redhat linux linkers preach with

		../port/os_filesys.c:26: the use of `tmpnam' is dangerous, better use `mkstemp'

	but SunOS 5.5.1 does not have mkstemp so we use conditional
	compilation.  Ug.
*/
#define AVOID_TMPNAM
