SMLNJ=SML_VERSION=110 /usr/local/bin/sml
TILTNJ=./Bin/tilt-nj -vv
m=

.PHONY: all with_slaves runtime heaps tilt_heap dump_heap slaves basis lib tilt
all: runtime heaps basis lib tilt

with_slaves:
	$(MAKE) 'm=-m' runtime heaps slaves basis lib tilt

runtime:
	case `uname -s` in SunOS|OSF1) (cd Runtime && gmake runtime) ;; *) ;; esac

heaps: tilt_heap dump_heap

tilt_heap:
	echo 'SMLofNJ.Internals.GC.messages false; CM.make(); SMLofNJ.exportFn ("Heaps/tilt", Main.main);' | $(SMLNJ)

dump_heap:
	echo 'SMLofNJ.Internals.GC.messages false; CM.make(); SMLofNJ.exportFn ("Heaps/dump", Dump.main);' | $(SMLNJ)

slaves:
	$(TILTNJ) -s 4/localhost

basis:
	$(TILTNJ) $m -b mapfile-basis

lib:
	$(TILTNJ) $m mapfile-lib

tilt:
	$(TILTNJ) $m mapfile-all
