# On SunOS, some versions of GCC generate bad code with -O2.  The
# symptom has been that TILT-compiled binaries (which are linked against
# this runtime) do not terminate.  This problem has been seen under
# SunOS 5.9 with GCC 3.2.3 and GCC 3.4.2.  This problem has not been
# seen under SunOS 5.5.1 or SunOS 5.8 with GCC 2.95.3.  If you are
# adventurous, then you can change the "-O" to "-O2" below.
#CC=gcc -I . -I ../port -Werror -Winline -Wimplicit -g -Wa,-xarch=v8plus -DDEBUG -D__$(OS)__
CC=gcc -I . -I ../port -Winline -Wimplicit -g -Wa,-xarch=v8plus -O -finline-functions -D__$(OS)__
CFLAGS=
AS=/usr/ccs/bin/as -xarch=v8plus -P
ASFLAGS=

DEST=../../Lib/sparc/$(OS)

LIB=$(DEST)/runtime.a

# NB SunOS 5.5.1 make fails mysteriously if there is a backslash
# on the last entry in lists like HFILES and OFILES.

DIRS=\
	$(DEST)

EXTRA=\
	$(DEST)/firstdata.o

HFILES=\
	s.h\
	../port/r.h\
	sparc.h\
	sparcasm.h\
	asm.h\
	gcinline.h

OFILES=\
	barriers.o\
	bitmap.o\
	ccall.o\
	commandline.o\
	create.o\
	date.o\
	die.o\
	exn.o\
	fc.o\
	fork.o\
	forward.o\
	gc.o\
	gc_gen.o\
	gc_gen_conc.o\
	gc_gen_para.o\
	gc_large.o\
	gc_para.o\
	gc_semi.o\
	gc_semi_conc.o\
	gc_semi_para.o\
	gcstat.o\
	global.o\
	hash.o\
	main.o\
	malloc.o\
	math.o\
	memobj.o\
	os_filesys.o\
	os_io.o\
	platform.o\
	portcreate.o\
	portexn.o\
	portmalloc.o\
	posix_error.o\
	posix_filesys.o\
	posix_io.o\
	posix_procenv.o\
	posix_process.o\
	posix_signal.o\
	posix_sysdb.o\
	posix_tty.o\
	pre_posix.o\
	queue.o\
	real.o\
	rooms.o\
	show.o\
	sigaction.o\
	signal.o\
	stack.o\
	stats.o\
	support.o\
	syserror.o\
	sysvals.o\
	thread.o\
	time.o\
	timer.o

$(DEST)/%: %
	@echo
	cp $< $(DEST)

%.o:	%.c
	@echo
	$(CC) $(CFLAGS) -c -o $@ $*.c

%.o:	../port/%.c
	@echo
	$(CC) $(CFLAGS) -c -o $@ ../port/$*.c

%.o:	%.s
	@echo
	$(AS) $(ASFLAGS) -o $@ $<

all nuke:
	$(MAKE) OS=`../../Bin/ostype` dirs $@_continue

dirs:
	-mkdir $(DIRS)

all_continue:	$(LIB) $(EXTRA)

clean:
	-rm -f *.o

nuke_continue:
	-rm -f *.o $(LIB) $(EXTRA)

$(LIB): $(OFILES)
	@echo
	ar rvu $(LIB) $(OFILES)

$(OFILES) $(EXTRA): Makefile

$(OFILES) $(EXTRA): $(HFILES)
