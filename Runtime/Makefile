# ------------------------------ Makefile Usage ------------------------
# While in this directory, execute "make target=foo out=goo"
#
# This will assume the existence of an "foo.s" file as input.
# The final executable will be deposited as "goo".
# The object file "foo.o" will be created as an intermediate.
# By default, the non-threading version is generated.
# Define "runtime_lib" to be "runtime.hprof.a" to turn on heapprofile.
# ----------------------------------------------------------------------

# ------------------------------ Top Targets ---------------------------
SHELL=/bin/sh

all:
	echo "Try gmake runtime"

purge:
	rm -rf obj*/*.o obj*/*.d *~ *.i

test:
	echo ${asm_name}
	echo ${META_ASFLAGS}
	echo ${ASFLAGS}
	echo ${DEBUG}
	echo ${DEBUG2}

# ------------------------------ General macros ------------------------
alpha_osf20_sys_name = alpha_osf
alpha_osf32_sys_name = alpha_osf
alpha_dux40_sys_name = alpha_osf
rs_aix32_sys_name = rs_aix
sun4_55_sys_name = solaris
sun4x_56_sys_name = solaris

alpha_osf20_chip_name = alpha
alpha_osf32_chip_name = alpha
alpha_dux40_chip_name = alpha
rs_aix32_chip_name = ppc
sun4_55_chip_name = sparc

full_sys_name = $(shell sys)
sys_name=${${full_sys_name}_sys_name}
chip_name=${${full_sys_name}_chip_name}

ASM=${sys_name}_asm
MAIN=main_nothread
ROOT_OBJ=posix gcstat  forward gc_semi gc_gen gc_large gc_para gc_semi_para gc_gen_para gc_semi_conc gc_gen_conc gc create ${MAIN} show service global hash queue stats stack bitmap platform memobj thread exn signal  gc_${ASM} service_${ASM} 
objdir=obj_${sys_name}
COREFILES_OBJ=${patsubst %,${objdir}/%.o,${ROOT_OBJ}}
COREFILES_HPROF_OBJ=${patsubst %,${objdir}/%.hprof.o,${ROOT_OBJ}}

show:
	echo ${COREFILES_OBJ}
	echo
	echo ${COREFILES_HPROF_OBJ}

out=ml.out
target=foo.sml.${ASM}.s
targetobjs=$(target:.s=.o)

ifeq ($(hprof),1)
	runtime_lib=runtime.${sys_name}.hprof.a
else 
ifeq ($(oldalloc),1)
	runtime_lib=runtime.${sys_name}.oldalloc.a
else
	runtime_lib=runtime.${sys_name}.a
endif
endif

ifeq ($(DEBUG),1)
	DODEBUG=-DDEBUG
else 
	DODEBUG=
endif

# --------------------------- M4 and CPP flags ---------------------------------

M4=m4
M4FLAGS=-D${sys_name} prelude.m4

# -std will define __STDC__ 
alpha_osf_cppflags=-newc -std
solaris_cppflags=
CPP=${CC} -E
CPPFLAGS=${${sys_name}_cppflags} -D${sys_name} ${DODEBUG}

# --------------------------- C flags ----------------------------------
alpha_osf_cc=cc
solaris_cc=gcc
# /usr/local/opt/SUNWspro/bin/cc Solaris ANSI C compiler
# /usr/lang/cc for SunOS 

# The xtaso flag instructs the Alpha compiler to honor #pragma pointer_size directives
# This allows the runtime to use pointer type that are 32-bit when interacting with ML.
alpha_osf_cflags=-warnprotos -xtaso -inline size -w0 -c -g3
solaris_cflags=-O2 -Winline -finline-functions -Wimplicit -Wa,-xarch=v8plus -c -g 
CC=${${sys_name}_cc}
CFLAGS=${CPPFLAGS} ${${sys_name}_cflags}

# --------------------------- assembler flag ---------------------------
alpha_osf_asflags=-O1 -g
rs_aix_asflags=-mppc -Drs_aix
solaris_asflags=-xarch=v8plus -P
AS=as
ASFLAGS=${CPPFLAGS} ${${sys_name}_asflags}

# --------------------------- linker flag, CRT, libs -------------------
alpha_osf_ldflags=-D 40000000 -T 20000000 -non_shared
rs_aix_ldflags=-H512 -T512
sparc_solaris_ldflags=
META_LDFLAGS=${sys_name}_ldflags
ifeq ($(ATOM),1)
	LDFLAGS=-r -N ${${META_LDFLAGS}}
else
	LDFLAGS=${${META_LDFLAGS}}
endif

alpha_osf_crt=/usr/lib/cmplrs/cc/crt0.o 
rs_aix_crt=/usr/lib/crt0.o 
sparc_solaris_crt=/usr/lib/crt0.o 
CRT=${${sys_name}_crt}

alpha_osf_libs=-lm -lsys5 -lc -lots -lpthreads
rs_aix_libs=-lm -lsys5 -lc -lots -lpthreads
sparc_solaris_libs=-lm -lsys5 -lc -lots -lpthreads -lperfmon
LIBS=${${sys_name}_libs}

# ------------------------------ Link Phase ----------------------------
runtime.${sys_name}.a: ${COREFILES_OBJ}
	rm -f ${runtime_lib}
	ar qc ${runtime_lib} ${COREFILES_OBJ} 


runtime.${sys_name}.hprof.a: ${COREFILES_HPROF_OBJ}
	rm -f ${runtime_lib}
	ar qc ${runtime_lib} ${COREFILES_HPROF_OBJ} 

runtime.${sys_name}.oldalloc.a: ${COREFILES_OBJ}
	rm -f ${runtime_lib}
	ar qc ${runtime_lib} ${COREFILES_OBJ} 

runtime: ${runtime_lib} ${objdir}/firstdata.o

prog: ${targetobjs} ${runtime_lib}
	rm -f ${out}
	ld ${LDFLAGS} -o ${out} ${CRT} ${targetobjs} ${runtime_lib} ${LIBS}
ifeq ($(ATOM),1)
	atom ${out} -32addr -tool gprof -o ${out}.gprof.exe
	atom ${out} -32addr -tool iprof -o ${out}.iprof.exe
	atom ${out} -32addr -tool liprof -o ${out}.liprof.exe
	atom ${out} -32addr -tool pipe -o ${out}.pipe.exe
	atom ${out} -32addr -tool lpipe -o ${out}.lpipe.exe
	atom ${out} -32addr -tool memsys -o ${out}.memsys.exe
else
endif

# ------------------------------ Compiling -----------------------------

# Note C compilers usually assume .i means already preprocessed.
${objdir}/%.o: %.c
	${CPP} ${CPPFLAGS} $< | ${M4} ${M4FLAGS} - > $(<:.c=.i)
	${CC} -o $@ ${CFLAGS} $(<:.c=.i)
	rm $(<:.c=.i)

${objdir}/%.hprof.o: %.c
	${CPP} ${CPPFLAGS} -DHEAPPROFILE $< | ${M4} ${M4FLAGS} - > $(<:.c=.i)
	${CC} -o $@ ${CFLAGS} $(<:.c=.i)
	rm $(<:.c=.i)

${objdir}/%.o: %.s
	$(AS) -o $@ ${ASFLAGS} $<

${objdir}/%.hprof.o: %.s
	$(AS) -o $@ ${ASFLAGS} $<

# ------------------------------ Dependency analysis -------------------

# We generate a dependency makefile for each source file.  The sed
# command adds dependency information for two additional targets: the
# generated makefile and the heap profiling object file.

ADD_MAKEFILE_DEP=sed 's,\($*\)\.o[ :]*,\1.o \1.hprof.o $@ : ,g'
ADD_MAKEFILE_DEP_ASM=sed 's,\($*${ASM}\)\.o[ :]*,\1.o \1.hprof.o $@ : ,g'

# Dependencies for C source.
${objdir}/%.d: %.c
	$(CPP) $(CPPFLAGS) -M $< | $(ADD_MAKEFILE_DEP) > $@

# Dependencies for assembler source.  ("as -M" doesn't work on solaris.)
${objdir}/%${ASM}.d: %${ASM}.s
	makedepend -f- -- $(ASFLAGS) -- $< | $(ADD_MAKEFILE_DEP_ASM) > $@

# Snarf up the dependency information.
include $(patsubst %,${objdir}/%.d,${ROOT_OBJ})
