SMLNJ=SML_VERSION=110 /usr/local/bin/sml
TILTNJ=./Test/tilt-nj -v

# Use slaves (gmake slaves all to build)
MASTER=-M
BOOT=-B

# Do not use slaves
#MASTER=-m
#BOOT=-b

.PHONY: all runtime heap slaves basis smlnj-lib ml-yacc-lib tilt
all: runtime heap basis smlnj-lib ml-yacc-lib tilt

runtime:
	(cd Runtime && gmake runtime)

heap:
	echo 'SMLofNJ.Internals.GC.messages false; CM.make(); SMLofNJ.exportFn ("Heaps/tilt", Main.main);' | $(SMLNJ)

slaves:
	$(TILTNJ) -S4/localhost

basis:
	$(TILTNJ) $(BOOT) mapfile-basis

smlnj-lib:
	$(TILTNJ) $(MASTER) Basis/Library/group

ml-yacc-lib:
	$(TILTNJ) $(MASTER) Parser/Library/group

tilt:
	$(TILTNJ) $(MASTER) mapfile-all
