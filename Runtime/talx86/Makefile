#CC=gcc -g -Werror -Wall -I . -I ../port -I ../../../talc/runtime/loader/include
CC=gcc -O2 -g -Wall -I . -I ../port -I ../../../talc/runtime/loader/include
CFLAGS=
AS=../../Bin/talx86as
ASFLAGS=

DEST=../../Lib/talx86

LIB=$(DEST)/runtime.a

EXTRA=\
	$(DEST)/runtime.o\
	$(DEST)/runtime.to\
	$(DEST)/stdlib.o\
	$(DEST)/support.o\
	$(DEST)/support.to\

HFILES=\
	s.h\
	../port/r.h\
	talx86.h\

OFILES=\
	commandline.o\
	create.o\
	date.o\
	die.o\
	fc.o\
	fork.o\
	malloc.o\
	math.o\
	os_filesys.o\
	os_io.o\
	portcreate.o\
	portexn.o\
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
	sysvals.o\
	time.o\
	timer.o\

$(DEST)/%: %
	@echo
	cp $< $(DEST)

%.o: %.c
	@echo
	$(CC) $(CFLAGS) -c -o $@ $*.c

%.o: ../port/%.c
	@echo
	$(CC) $(CFLAGS) -c -o $@ ../port/$*.c

%.o %.to: %.tal
	@echo
	$(AS) $(ASFLAGS) $*.o $*.to $?

all:	$(LIB) $(EXTRA)

$(LIB): $(OFILES)
	@echo
	ar rvu $(LIB) $(OFILES)

clean:
	-rm -f *.o *.to

nuke:
	-rm -f *.o *.to $(LIB) $(EXTRA)

$(OFILES) $(EXTRA): Makefile

$(OFILES) $(EXTRA): $(HFILES)

runtime.tal: runtime_i.tali

support.tal: support_e.tali support_i.tali
