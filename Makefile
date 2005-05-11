PREFIX=/usr/local
RUNPREFIX=$(PREFIX)
mkheap=./Bin/mkheap
cputype=`./Bin/cputype`
tiltname=`./Bin/tiltname`
tiltdumpname=`./Bin/tiltdumpname`
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
	$(mkheap) sources.cm Bin/heap/$(tiltname) Main.main
dump-heap: FORCE
	$(mkheap) sources.cm Bin/heap/$(tiltdumpname) Dump.main

# TILT-compiled libraries

basis: FORCE
	$(tilt) -fBootstrap -lLib/basis Basis/project
smlnj-lib: FORCE
	$(tilt) -lLib/smlnj-lib Basis/Library/project
ml-yacc-lib: FORCE
	$(tilt) -lLib/ml-yacc-lib Parser/Library/project
arg: FORCE
	$(tilt) -lLib/arg Util/project-arg

# TILT runtime

runtime: FORCE
	(cd Runtime/$(cputype) && $(MAKE))

# TILT-compiled binaries

tilt: FORCE
	$(tilt) -oBin/$(cputype)/$(tiltname) -c Top Top/project
dump: FORCE
	$(tilt) -oBin/$(cputype)/$(tiltdumpname) -c DumpTop Top/project
runtest: FORCE
	if test -d Test; then $(tilt) -oBin/$(cputype)/runtest -c Runtest Test/project; fi

# Other targets

clean: FORCE
	-if test -d Test; then $(tilt) -pp Test/project; fi
	-$(tilt) -pp Top/project
	-$(tilt) -pp Util/project-arg
	-$(tilt) -pp Parser/Library/project
	-$(tilt) -pp Basis/Library/project
	-$(tilt) -pp -fBootstrap Basis/project
	(cd Runtime/$(cputype) && $(MAKE) nuke)

slaves: FORCE
	$(slaves) -n 4/localhost

install: FORCE
	-mkdir $(PREFIX)/lib
	-mkdir $(PREFIX)/lib/tilt
	(cd Runtime && $(MAKE) DEST=$(PREFIX)/lib/tilt/Runtime install)
	if test -f Bin/$(cputype)/tilt; then strip Bin/$(cputype)/tilt; fi
	if test -f Bin/$(cputype)/tilt-dump; then strip Bin/$(cputype)/tilt-dump; fi
	tar cf - License Bin Lib Doc | (cd $(PREFIX)/lib/tilt && tar xf -)
	-mkdir $(PREFIX)/man
	-mkdir $(PREFIX)/man/man1
	-mkdir $(PREFIX)/man/man4
	cp Doc/tilt.man $(PREFIX)/man/man1/tilt.1 && chmod 644 $(PREFIX)/man/man1/tilt.1
	cp Doc/tilt-project.man $(PREFIX)/man/man4/tilt-project.4 && chmod 644 $(PREFIX)/man/man4/tilt-project.4
	-mkdir $(PREFIX)/bin
	for name in tilt tilt-nj tilt-slaves; do \
		(echo '#!/bin/sh'; \
		 echo 'exec $(RUNPREFIX)/lib/tilt/Bin/'$$name' $${1+"$$@"}'; \
		)>$(PREFIX)/bin/$$name && chmod 755 $(PREFIX)/bin/$$name; \
	done
