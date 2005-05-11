#CC=gcc -g -Werror -Wall -I . -I ../port -I ../../../talc/runtime/loader/include
CC=gcc -O2 -g -Wall -I . -I ../port -I ../../../talc/runtime/loader/include
CFLAGS=
AS=../../Bin/talx86as
ASFLAGS=

TALCDIR=../../../talc
DEST=../../Lib/talx86
AUX=../../Bin/talx86/aux

LIB=$(DEST)/runtime.a

# The tali files are needed during assembly.

PREREQ=\
	$(AUX)/genCinit.exe\
	$(AUX)/talc.exe\
	$(DEST)/libgc.a\
	$(DEST)/tal_prog.tali\
	$(DEST)/tal_start.o\
	$(DEST)/tal_util.o\

EXTRA=\
	$(DEST)/runtime.o\
	$(DEST)/runtime.to\
	$(DEST)/stdlib.o\
	$(DEST)/support.o\
	$(DEST)/support.to\
	$(DEST)/stdlib.tali\
	$(DEST)/support_e.tali\
	$(DEST)/tilt_main.tali\

HFILES=\
	s.h\
	../port/r.h\
	talx86.h\

OFILES=\
	ccall.o\
	commandline.o\
	create.o\
	date.o\
	die.o\
	exn.o\
	fc.o\
	fork.o\
	malloc.o\
	math.o\
	os_filesys.o\
	os_io.o\
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
	real.o\
	stdlib.o\
	syserror.o\
	sysvals.o\
	time.o\
	timer.o\

$(DEST)/%: %
	@echo
	cp $< $(DEST)

$(DEST)/%: $(TALCDIR)/runtime/%
	@echo
	cp $< $(DEST)

$(AUX)/%: $(TALCDIR)/build/%
	@echo
	cp $< $(AUX)

%.o: %.c
	@echo
	$(CC) $(CFLAGS) -c -o $@ $*.c

%.o: ../port/%.c
	@echo
	$(CC) $(CFLAGS) -c -o $@ ../port/$*.c

%.o %.to: %.tal
	@echo
	$(AS) $(ASFLAGS) $*.o $*.to $?


all:	$(PREREQ) $(LIB) $(EXTRA)

$(LIB): $(OFILES)
	@echo
	ar rvu $(LIB) $(OFILES)

clean:
	-rm -f *.o *.to

nuke:
	-rm -f *.o *.to $(PREREQ) $(LIB) $(EXTRA)

$(OFILES) $(EXTRA): Makefile

$(OFILES) $(EXTRA): $(HFILES)

runtime.tal: runtime_i.tali

support.tal: support_e.tali support_i.tali
