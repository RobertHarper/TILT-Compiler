# Ensure that this Makefile and the scripts ./Bin/tilt-nj and
# ./Bin/dump-nj have the correct path to the SML/NJ compiler.  The TILT
# sources compile under SML/NJ versions 110.0.3 and 110.0.7 and under
# TILT.  If you are compiling with some other SML compiler, then you
# will probably need to modify our CM files, the script ./Bin/mkheap
# used to generate SML/NJ heaps, and perhaps some of the TILT code to
# account for basis library changes.
#
# Ensure that ./Bin/cputype prints sparc, alpha, or unsupported.  (After
# building an SML/NJ heap for TILT with "make heap", you can use
# ./Bin/tilt-nj to run TILT with the SML/NJ runtime on unsupported
# systems.)  If you are compiling on a sparc or alpha, then look at
# Runtime/Makefile too.
#
# To compile TILT, use "make" or "make with-slaves".  The latter assumes
# that the script ./Bin/tilt-slave works on your system.
#
# To install into PREFIX, use "make install".  This copies binaries to
# PREFIX/lib/tilt, manual pages to PREFIX/man, and wrapper scripts
# to PREFIX/bin.

PREFIX=/usr/local
smlnj=SML_VERSION=110 /usr/local/bin/sml
mkheap=./Bin/mkheap
cputype=`./Bin/cputype`
tiltnj=./Bin/tilt-nj -vv
master=$(tiltnj)
slaves=:
purge=$(tiltnj) -pp

all:\
	tilt-heap\
	dump-heap\
	runtime\
	slaves\
	basis\
	smlnj-lib\
	ml-yacc-lib\
	arg\
	tilt\
	dump\
	runtest

with-slaves: FORCE
	$(MAKE) 'master=$(tiltnj) -m' 'slaves=./Bin/tilt-slaves' all

FORCE:

# SML/NJ heaps

heap tilt-heap: FORCE
	$(mkheap) Heaps/tilt Main.main | $(smlnj)
dump-heap: FORCE
	$(mkheap) Heaps/dump Dump.main | $(smlnj)

# TILT-compiled libraries

basis: FORCE
	$(master) -fBootstrap -lLib/basis Basis/mapfile
smlnj-lib: FORCE
	$(master) -lLib/smlnj-lib Basis/Library/group
ml-yacc-lib: FORCE
	$(master) -lLib/ml-yacc-lib Parser/Library/group
arg: FORCE
	$(master) -lLib/arg Util/arg.group

# TILT runtime

runtime: FORCE
	(cd Runtime && $(MAKE))

# TILT-compiled binaries

tilt: FORCE
	$(master) -oBin/$(cputype)/tilt -c Top Top/mapfile
dump: FORCE
	$(master) -oBin/$(cputype)/dump -c DumpTop Top/mapfile
runtest: FORCE
	if test -d Test; then $(master) -oBin/$(cputype)/runtest -c Runtest Test/mapfile; fi

# Other targets

clean: FORCE
	-if test -d Test; then $(purge) Test/mapfile; fi
	-$(purge) Top/mapfile
	-$(purge) Util/arg.group
	-$(purge) Parser/Library/group
	-$(purge) Basis/Library/group
	-$(purge) -fBootstrap Basis/mapfile

slaves: FORCE
	$(slaves) -n 4/localhost

install: FORCE
	-mkdir $(PREFIX)/lib/tilt
	tar cf - Bin Lib Heaps | (cd $(PREFIX)/lib/tilt && tar xf -)
	cp Doc/tilt.1 $(PREFIX)/man/man1 && chmod 644 $(PREFIX)/man/man1
	for name in tilt tilt-nj tilt-slaves dump dump-nj; do \
		{echo '#!/bin/sh'; \
		 echo 'exec $(PREFIX)/'$$name' $${1+"$$@"}'; \
		}>$(PREFIX)/bin/$$name && chmod 755 $(PREFIX)/bin/$$name; \
	done
