# Ensure ./Bin/smlnj has the correct path to the SML/NJ compiler.  The
# TILT sources compile under SML/NJ versions 110.0.3 and 110.0.7 and
# under TILT.  If you are compiling with some other SML compiler, then
# you will probably need to modify our CM files, the script ./Bin/mkheap
# used to generate SML/NJ heaps, and perhaps some of the TILT code to
# account for Basis library changes.
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
mkheap=./Bin/mkheap
cputype=`./Bin/cputype`
tilt=./Bin/tilt-nj -vv
slaves=:

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
	$(MAKE) 'slaves=./Bin/tilt-slaves' all

FORCE:

# SML/NJ heaps

heap tilt-heap: FORCE
	$(mkheap) sources.cm Bin/heap/tilt Main.main
dump-heap: FORCE
	$(mkheap) sources.cm Bin/heap/dump Dump.main

# TILT-compiled libraries

basis: FORCE
	$(tilt) -fBootstrap -lLib/basis Basis/mapfile
smlnj-lib: FORCE
	$(tilt) -lLib/smlnj-lib Basis/Library/group
ml-yacc-lib: FORCE
	$(tilt) -lLib/ml-yacc-lib Parser/Library/group
arg: FORCE
	$(tilt) -lLib/arg Util/arg.group

# TILT runtime

runtime: FORCE
	(cd Runtime && $(MAKE) opt dbg)

# TILT-compiled binaries

tilt: FORCE
	$(tilt) -oBin/$(cputype)/tilt -c Top Top/mapfile
dump: FORCE
	$(tilt) -oBin/$(cputype)/dump -c DumpTop Top/mapfile
runtest: FORCE
	if test -d Test; then $(tilt) -oBin/$(cputype)/runtest -c Runtest Test/mapfile; fi

# Other targets

clean: FORCE
	-if test -d Test; then $(tilt) -pp Test/mapfile; fi
	-$(tilt) -pp Top/mapfile
	-$(tilt) -pp Util/arg.group
	-$(tilt) -pp Parser/Library/group
	-$(tilt) -pp Basis/Library/group
	-$(tilt) -pp -fBootstrap Basis/mapfile

slaves: FORCE
	$(slaves) -n 4/localhost

install: FORCE
	-mkdir $(PREFIX)/lib
	-mkdir $(PREFIX)/lib/tilt
	(cd Runtime && $(MAKE) DEST=$(PREFIX)/lib/tilt/Runtime install)
	tar cf - Bin Lib | (cd $(PREFIX)/lib/tilt && tar xf -)
	-mkdir $(PREFIX)/man
	-mkdir $(PREFIX)/man/man1
	-mkdir $(PREFIX)/man/man6
	cp Doc/tilt.1 $(PREFIX)/man/man1 && chmod 644 $(PREFIX)/man/man1/tilt.1
	cp Doc/project.6 $(PREFIX)/man/man6 && chmod 644 $(PREFIX)/man/man6/project.6
	-mkdir $(PREFIX)/bin
	for name in tilt tilt-nj tilt-slaves dump dump-nj; do \
		(echo '#!/bin/sh'; \
		 echo 'exec $(PREFIX)/lib/tilt/Bin/'$$name' $${1+"$$@"}'; \
		)>$(PREFIX)/bin/$$name && chmod 755 $(PREFIX)/bin/$$name; \
	done
