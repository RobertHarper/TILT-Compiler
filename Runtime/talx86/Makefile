OS=`../../Bin/ostype`
#CC=gcc -g -Werror -Wall -I . -I ../port -D__$(OS)__
CC=gcc -O2 -g -Wall -I . -I ../port -D__$(OS)__
CFLAGS=
AS=../../Bin/talx86as
ASFLAGS=

TALCDIR=../../../talc
DEST=../../Lib/talx86/$(OS)
BIN=../../Bin/talx86/$(OS)

LIB=$(DEST)/runtime.a

DIRS=\
	$(DEST)\
	$(BIN)\

PREREQ=\
	$(BIN)/genCinit.exe\
	$(BIN)/talc.exe\
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
	printer.o\
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

$(BIN)/%: $(TALCDIR)/build/%
	@echo
	cp $< $(BIN)

%.o: %.c
	@echo
	$(CC) $(CFLAGS) -c -o $@ $*.c

%.o: ../port/%.c
	@echo
	$(CC) $(CFLAGS) -c -o $@ ../port/$*.c

%.o %.to: %.tal
	@echo
	$(AS) $(ASFLAGS) $*.o $*.to $?

all nuke:
	$(MAKE) OS=$(OS) dirs $@_continue

dirs:
	-mkdir $(DIRS)

all_continue:	$(PREREQ) $(LIB) $(EXTRA)

clean:
	-rm -f *.o *.to

nuke_continue:
	-rm -f *.o *.to $(PREREQ) $(LIB) $(EXTRA)

$(LIB): $(OFILES)
	@echo
	ar rvu $(LIB) $(OFILES)

$(OFILES) $(EXTRA): Makefile

$(OFILES) $(EXTRA): $(HFILES)

runtime.tal: runtime_i.tali

support.tal: support_e.tali support_i.tali
