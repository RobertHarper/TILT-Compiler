SMLNJ=SML_VERSION=110 /usr/local/bin/sml
TILTNJ=./Test/tilt-nj -v

.PHONY: all runtime heap basis smlnj-lib ml-yacc-lib tilt
all: runtime heap basis smlnj-lib ml-yacc-lib tilt

runtime:
	(cd Runtime && gmake runtime)

heap:
	echo 'SMLofNJ.Internals.GC.messages false; CM.make(); SMLofNJ.exportFn ("Heaps/tilt", Main.main);' | $(SMLNJ)

basis:
	$(TILTNJ) -b mapfile-basis

smlnj-lib:
	$(TILTNJ) -m Basis/Library/group

ml-yacc-lib:
	$(TILTNJ) -m Parser/Library/group

tilt:
	$(TILTNJ) -m mapfile-all
