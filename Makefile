# Change smlnj to reflect your installation of SML/NJ 110.0.7.  If you
# are compiling with a more recent version of SML/NJ, then you probably
# need to modify our CM files and the script ./Bin/mkheap used to generate
# SML/NJ heaps.
#
# Ensure that ./Bin/cputype prints sparc, alpha, or unsupported.  (After
# building an SML/NJ heap for TILT with "make tilt_heap", you can use
# ./Bin/tilt-nj to run TILT with the SML/NJ runtime on unsupported
# systems.)  If you are compiling on a sparc or alpha, then look at
# Runtime/Makefile too.
#
# To compile TILT, use "make" or "make with_slaves".  The latter assumes
# that ./Bin/til_slave works; you can copy (and update for your systems)
# ./Bin/til_slave_local_xterm or ./Bin/til_slave_remote_emacs.

smlnj=SML_VERSION=110 /usr/local/bin/sml
mkheap=./Bin/mkheap
cputype=`./Bin/cputype`
tiltnj=./Bin/tilt-nj
master=$(tiltnj) -vv
slave=:
purge=$(tiltnj) -vpp

all:\
	tilt_heap\
	dump_heap\
	slaves\
	basis\
	smlnj-lib\
	ml-yacc-lib\
	arg\
	runtime\
	tilt\
	dump\
	runtest

with_slaves: FORCE
	$(MAKE) 'master=$(tiltnj) -m' 'slave=$(tiltnj) -s' all

FORCE:

# SML/NJ heaps

tilt_heap: FORCE
	$(mkheap) Heaps/tilt Main.main | $(smlnj)
dump_heap: FORCE
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
	case $(cputype) in \
	unsupported) $(master) -c Top Top/mapfile ;; \
	*) $(master) -oBin/$(cputype)/tilt -c Top Top/mapfile ;; \
	esac
dump: FORCE
	$(master) -oBin/$(cputype)/dump -c DumpTop Top/mapfile
runtest: FORCE
	$(master) -oBin/$(cputype)/runtest -c Runtest Test/mapfile

# Other targets

clean: FORCE
	-$(purge) Test/mapfile
	-$(purge) Top/mapfile
	-$(purge) Util/arg.group
	-$(purge) Parser/Library/group
	-$(purge) Basis/Library/group
	-$(purge) -fBootstrap Basis/mapfile

slaves: FORCE
	$(slave) 4/localhost
