PREFIX=/usr/local
RUNPREFIX=$(PREFIX)
mkheap=./Bin/mkheap
cputype=`./Bin/cputype`
os=`./Bin/ostype`
BIN=Bin/$(cputype)/$(os)
tiltname=`./Bin/tiltname`
tiltdumpname=`./Bin/tiltdumpname`
tilt=./Bin/tilt-nj -vv

DIRS=\
	$(BIN)

all:\
	tilt-heap\
	dump-heap\
	runtime\
	basis\
	smlnj-lib\
	ml-yacc-lib\
	arg\
	dirs\
	tilt\
	dump\
	runtest

# SML/NJ heaps

heap tilt-heap:
	$(mkheap) sources.cm Bin/heap/$(tiltname) Main.main
dump-heap:
	$(mkheap) sources.cm Bin/heap/$(tiltdumpname) Dump.main

# TILT-compiled libraries

basis:
	$(tilt) -fBootstrap -lLib/basis Basis/project
smlnj-lib:
	$(tilt) -lLib/smlnj-lib Basis/Library/project
ml-yacc-lib:
	$(tilt) -lLib/ml-yacc-lib Parser/Library/project
arg:
	$(tilt) -lLib/arg Util/project-arg

# TILT runtime

runtime:
	(cd Runtime/$(cputype) && $(MAKE) all clean)

# TILT-compiled binaries

dirs:
	-mkdir $(DIRS)

tilt:
	$(tilt) -o$(BIN)/$(tiltname) -c Top Top/project
dump:
	$(tilt) -o$(BIN)/$(tiltdumpname) -c DumpTop Top/project
runtest:
	if test -d Test; then $(tilt) -o$(BIN)/runtest -c Runtest Test/project; fi

# Other targets

clean:
	-if test -d Test; then $(tilt) -pp Test/project; fi
	-$(tilt) -pp Top/project
	-$(tilt) -pp Util/project-arg
	-$(tilt) -pp Parser/Library/project
	-$(tilt) -pp Basis/Library/project
	-$(tilt) -pp -fBootstrap Basis/project
	(cd Runtime/$(cputype) && $(MAKE) nuke)

install:
	-mkdir $(PREFIX)/lib
	-mkdir $(PREFIX)/lib/tilt
	if test -f $(BIN)/tilt; then strip $(BIN)/tilt; fi
	if test -f $(BIN)/tilt-dump; then strip $(BIN)/tilt-dump; fi
	tar cf - License Bin Lib Doc | (cd $(PREFIX)/lib/tilt && tar xf -)
	-mkdir $(PREFIX)/man
	-mkdir $(PREFIX)/man/man1
	-mkdir $(PREFIX)/man/man4
	cp Doc/tilt.man $(PREFIX)/man/man1/tilt.1
	chmod 644 $(PREFIX)/man/man1/tilt.1
	cp Doc/tilt-project.man $(PREFIX)/man/man4/tilt-project.4
	chmod 644 $(PREFIX)/man/man4/tilt-project.4
	-mkdir $(PREFIX)/bin
	for name in tilt tilt-nj tilt-slaves; do \
		(echo '#!/bin/sh'; \
		 echo 'exec $(RUNPREFIX)/lib/tilt/Bin/'$$name' $${1+"$$@"}'; \
		)>$(PREFIX)/bin/$$name && chmod 755 $(PREFIX)/bin/$$name; \
	done
