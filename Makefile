SMLNJ=SML_VERSION=110 /usr/local/bin/sml
TILTNJ=./Bin/tilt-nj -vv

# Use slaves
#M=-M
#B=-B

# Do not use slaves
M=-m
B=-b

.PHONY: all with_slaves runtime heap slaves basis lib tilt
all: runtime heap basis lib tilt

with_slaves:
	$(MAKE) 'M=-M' 'B=-B' runtime heap slaves basis lib tilt

runtime:
	case `uname -s` in SunOS|OSF1) (cd Runtime && gmake runtime) ;; *) ;; esac

heap:
	echo 'SMLofNJ.Internals.GC.messages false; CM.make(); SMLofNJ.exportFn ("Heaps/tilt", Main.main);' | $(SMLNJ)
	echo 'SMLofNJ.Internals.GC.messages false; CM.make(); SMLofNJ.exportFn ("Heaps/dump", Dump.main);' | $(SMLNJ)

slaves:
	$(TILTNJ) -S4/localhost

basis:
	$(TILTNJ) $B mapfile-basis

lib:
	$(TILTNJ) $M mapfile-lib

tilt:
	$(TILTNJ) $M mapfile-all
