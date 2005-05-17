#define	_REENTRANT	1	/* per-thread errno */

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

#define	sparc	1	/* Satisfy Perry's code */
