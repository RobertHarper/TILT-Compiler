# ------------------------------ Makefile Usage ----------------------------
# While in this directory, execute "make target=foo out=goo"
#
# This will assume the existence of an "foo.s" file as input.
# The final executable will be deposited as "goo".
# The object file "foo.o" will be created as an intermediate.
# By default, the non-threading version is generated.
# Define "runtime_lib" to be "runtime.hprof.a" to turn on heapprofile.
# --------------------------------------------------------------------------

# ------------------------------ Top Targets ----------------------------
all:
	echo "Try gmake runtime"

purge:
	rm -rf obj*/*\.o *~

test:
	echo ${asm_name}
	echo ${META_ASFLAGS}
	echo ${ASFLAGS}


# ------------------------------ General macros --------------------------
alpha_osf20_sys_name = alpha_osf
alpha_osf32_sys_name = alpha_osf
alpha_dux40_sys_name = alpha_osf
rs_aix32_sys_name = rs_aix
sun4_55_sys_name = solaris

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
ROOT_OBJ=posix gcstat  forward gc_semi gc_gen gc_large gc_para gc_semi_para gc_gen_para gc_semi_conc gc_gen_conc gc create ${MAIN} show service global hash queue stats stack bitmap platform memobj thread exn signal  gc_${ASM} stack_${ASM} service_${ASM} 
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

# --------------------------- C and asm flags ---------------------------
#CC=gcc -D__C_ASM_H -DGCC
alpha_osf_cc=cc
solaris_cc=gcc
# /usr/local/opt/SUNWspro/bin/cc Solaris ANSI C compiler
# /usr/lang/cc for SunOS 

# The xtaso flag instructs the Alpha compiler to honor #pragma pointer_size directives
# This allows the runtime to use pointer type that are 32-bit when interacting with ML.
# -std will define __STDC__ 
alpha_osf_cflags=-newc -xtaso -w0 -warnprotos -std -c -inline size -g3 -D${sys_name}
solaris_cflags=-O2 -Winline -finline-functions -Wa,-xarch=v8plus -c -g -D${sys_name}
CC=${${sys_name}_cc}
CFLAGS=${${sys_name}_cflags}

# --------------------------- assembler flag ----------------------------
alpha_osf_asflags=-O1 -g -Dalpha_osf
rs_aix_asflags=-mppc -Drs_aix
solaris_asflags=-xarch=v8plus -P -Dsolaris
ASFLAGS=${${sys_name}_asflags}

# --------------------------- linker flag, CRT, libs -------------------------
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
sparc_solaris_libs=-lm -lsys5 -lc -lots -lpthreads
LIBS=${${sys_name}_libs}


# --------------------------- Make asm target with default rules --------
.s.o: thread.h
	as ${ASFLAGS} -o $@ $*.s



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

# ------------------------------ Core Code ----------------------------
${objdir}/firstdata.o: firstdata.c
	${CC} -o $@ ${CFLAGS} firstdata.c

${objdir}/posix.o: general.h posix.c posix.h
	${CC} -o $@ ${CFLAGS} posix.c

${objdir}/global.o: global.h general.h global.c general.h
	${CC} -o $@ ${CFLAGS} global.c

${objdir}/stack.o: stack.c queue.h general.h
	${CC} -o $@ ${CFLAGS} stack.c

${objdir}/stack_${ASM}.o: stack_${ASM}.s  general.h
	as -o $@ ${ASFLAGS} stack_${ASM}.s

${objdir}/queue.o: queue.c queue.h
	${CC} -o $@ ${CFLAGS} queue.c

${objdir}/hash.o: hash.c hash.h
	${CC} -o $@ ${CFLAGS} hash.c

${objdir}/thread.o: thread.c memobj.h tag.h general.h thread.h
	${CC} -o $@ ${CFLAGS} thread.c

${objdir}/signal.o: signal.c tag.h
	${CC} -o $@ ${CFLAGS} signal.c

${objdir}/forward.o: forward.c tag.h general.h platform.h forward.h thread.h
	${CC} -o $@ ${CFLAGS} forward.c

${objdir}/gc_semi.o: gc_semi.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc_semi.c

${objdir}/gc_gen.o: gc_gen.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc_gen.c

${objdir}/gc_large.o: gc_large.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc_large.c

${objdir}/gc_para.o: gc_para.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc_para.c

${objdir}/gc_semi_para.o: gc_semi_para.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc_semi_para.c

${objdir}/gc_gen_para.o: gc_gen_para.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc_gen_para.c

${objdir}/gc_semi_conc.o: gc_semi_conc.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc_semi_conc.c

${objdir}/gc_gen_conc.o: gc_gen_conc.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc_gen_conc.c

${objdir}/gc.o: gc.c tag.h general.h platform.h forward.h gc.h stack.h thread.h
	${CC} -o $@ ${CFLAGS} gc.c

${objdir}/gcstat.o: gcstat.c tag.h general.h
	${CC} -o $@ ${CFLAGS} gcstat.c

${objdir}/gc_${ASM}.o: gc_${ASM}.s  general.h
	as -o $@ ${ASFLAGS} gc_${ASM}.s

${objdir}/service.o: service.c tag.h  general.h
	${CC} -o $@ ${CFLAGS} service.c

${objdir}/service_${ASM}.o: service_${ASM}.s  general.h
	as -o $@ ${ASFLAGS} service_${ASM}.s

${objdir}/exn.o: exn.c tag.h general.h
	${CC} -o $@ ${CFLAGS} exn.c

${objdir}/create.o: create.c tag.h  general.h
	${CC} -o $@ ${CFLAGS} create.c

${objdir}/bitmap.o: bitmap.c tag.h
	${CC} -o $@ ${CFLAGS} bitmap.c

${objdir}/main_nothread.o: main.c tag.h general.h platform.h thread.h
	${CC} -o $@ ${CFLAGS} main.c -DTHREADED_VERSION=0


${objdir}/main_thread.o: main.c tag.h  general.h platform.h thread.h
	${CC} -o $@ ${CFLAGS} main.c -DTHREADED_VERSION=1


${objdir}/show.o: show.c tag.h  general.h
	${CC} -o $@ ${CFLAGS} show.c

${objdir}/stats.o: stats.c stats.h general.h
	${CC} -o $@ ${CFLAGS} stats.c

${objdir}/memobj.o: memobj.c tag.h general.h
	${CC} -o $@ ${CFLAGS} memobj.c

${objdir}/mllib_${ASM}.o: mllib_${ASM}.s  general.h
	as -o $@ ${ASFLAGS} mllib_${ASM}.s

${objdir}/mllib.o: mllib.c tag.h
	${CC} -o $@ ${CFLAGS} mllib.c

${objdir}/platform.o: platform.c platform.h
	${CC} -o $@ ${CFLAGS} platform.c


# ---------------------------- Core hprof Code ----------------------------
${objdir}/global.hprof.o: global.c global.h general.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE global.c

${objdir}/stack.hprof.o: stack.c queue.h general.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE stack.c

${objdir}/stack_${ASM}.hprof.o: stack_${ASM}.s  general.h
	as -o $@ ${ASFLAGS} stack_${ASM}.s

${objdir}/queue.hprof.o: queue.c queue.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE queue.c

${objdir}/hash.hprof.o: hash.c hash.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE hash.c

${objdir}/thread.hprof.o: thread.c memobj.h tag.h general.h thread.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE thread.c

${objdir}/signal.hprof.o: signal.c tag.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE signal.c

${objdir}/gc.hprof.o: gc.c tag.h general.h platform.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE gc.c

${objdir}/gcstat.hprof.o: gcstat.c tag.h general.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE gcstat.c

${objdir}/gc_${ASM}.hprof.o: gc_${ASM}.s  general.h
	as -o $@ ${ASFLAGS} gc_${ASM}.s

${objdir}/service.hprof.o: service.c tag.h  general.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE service.c

${objdir}/service_${ASM}.hprof.o: service_${ASM}.s  general.h
	as -o $@ ${ASFLAGS} service_${ASM}.s

${objdir}/exn.hprof.o: exn.c tag.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE exn.c

${objdir}/create.hprof.o: create.c tag.h  general.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE create.c

${objdir}/bitmap.hprof.o: bitmap.c tag.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE bitmap.c

${objdir}/main_nothread.hprof.o: main.c tag.h general.h platform.h thread.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE main.c -DTHREADED_VERSION=0


${objdir}/main_thread.hprof.o: main.c tag.h  general.h platform.h thread.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE main.c -DTHREADED_VERSION=1


${objdir}/show.hprof.o: show.c tag.h  general.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE show.c

${objdir}/stats.hprof.o: stats.c stats.h general.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE stats.c

${objdir}/memobj.hprof.o: memobj.c tag.h general.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE memobj.c

${objdir}/mllib_${ASM}.hprof.o: mllib_${ASM}.s  general.h
	as -o $@ ${ASFLAGS} mllib_${ASM}.s

${objdir}/mllib.hprof.o: mllib.c tag.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE mllib.c

${objdir}/platform.hprof.o: platform.c platform.h
	${CC} -o $@ ${CFLAGS} -DHEAPPROFILE platform.c


