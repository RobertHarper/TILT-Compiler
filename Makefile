
# Version of SML/NJ to use for TILT heaps (SML misc collection version number)
# Should agree with the version hard-coded in ./Bin/tilt.
#
SML_VERSION_NEW=110b

#
# You shouldn't have to change anything that follows.
#

# Misc collection release area
MISC=/afs/.cs.cmu.edu/local/tilt

# TILT mapfile
MAPFILE=mapfile-all

# SML/NJ drivers
SML_NEW=SML_VERSION=$(SML_VERSION_NEW) /usr/local/bin/sml

# TILT drivers
TILT_NEW=TILT_VERSION=NJ TILT_LIBDIR=`pwd` ./Bin/tilt
TILT_OLD=TILT_VERSION=NJ TILT_LIBDIR=/usr/local/lib/tilt /usr/local/bin/tilt

.PHONY: all
all:
	@echo See README.CMU for instructions.

# Print current version of TILT.
.PHONY: version
version:
	$(TILT_OLD) -v

# Build tilt heap for the current platform.
.PHONY: heap 
heap:
	echo 'SMLofNJ.Internals.GC.messages false; CM.make(); SMLofNJ.exportFn ("Heaps/tilt", Main.main);' | $(SML_NEW)

# Purge old runtime, basis library, and tilt binaries.
.PHONY: purge purge_runtime purge_tilt
purge_runtime:
	(cd Runtime && gmake purge)
purge_tilt:
	$(TILT_NEW) -vc $(MAPFILE)
purge: purge_runtime purge_tilt

# Run a slave using the new compiler.
.PHONY: slave
slave:
	$(TILT_NEW) -vs

# Build runtime, basis libraries, and tilt binaries for current platform.
.PHONY: tilt tilt_runtime tilt_tilt
tilt_runtime:
	(cd Runtime && gmake runtime)
tilt_tilt:
	$(TILT_NEW) -vm $(MAPFILE)
tilt: tilt_runtime tilt_tilt

# Update misc collection's source tree from CVS.
.PHONY: misc_src
misc_src:
	(cd $(MISC)/src/ml96 && cvs update -d -P)
