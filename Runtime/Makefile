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
all: prog

purge:
	rm -rf *\.o *~

test:
	echo ${asm_name}
	echo ${META_ASFLAGS}
	echo ${ASFLAGS}





# ------------------------------ General macros --------------------------
alpha_osf20_sys_name = alpha_osf
alpha_osf32_sys_name = alpha_osf
alpha_dux40_sys_name = alpha_osf
rs_aix32_sys_name = rs_aix

alpha_osf20_chip_name = alpha
alpha_osf32_chip_name = alpha
alpha_dux40_chip_name = alpha
rs_aix32_chip_name = ppc

full_sys_name = $(shell sys)
sys_name=${${full_sys_name}_sys_name}
chip_name=${${full_sys_name}_chip_name}

objdir=obj_${sys_name}
ASM=${sys_name}_asm
MAIN=main_nothread.o
COREFILES_OBJ=${objdir}/posix.o ${objdir}/gcstat.o ${objdir}/gc.o ${objdir}/create.o ${objdir}/${MAIN} ${objdir}/show.o ${objdir}/mllib.o ${objdir}/service.o ${objdir}/global.o ${objdir}/hash.o ${objdir}/queue.o ${objdir}/stats.o ${objdir}/stack.o ${objdir}/bitmap.o ${objdir}/platform.o ${objdir}/memobj.o ${objdir}/gc_${ASM}.o ${objdir}/mllib_${ASM}.o ${objdir}/service_${ASM}.o ${objdir}/stack_${ASM}.o ${objdir}/thread.o ${objdir}/exn.o ${objdir}/signal.o 
COREFILES_HPROF_OBJ=$(COREFILES_OBJ:.o=.hprof.o)

preludes=
prelude_hprofs=
prelude_oldallocs=
prelude_objs=
prelude_hprof_objs=
prelude_oldalloc_objs=

#preludes=prelude0.sml.${chip_name}.s prelude1.sml.${chip_name}.s prelude2.sml.${chip_name}.s
#prelude_hprofs=prelude0.sml.hprof.${chip_name}.s prelude1.sml.hprof.${chip_name}.s prelude2.sml.hprof.${chip_name}.s
#prelude_oldallocs=prelude0.sml.oldalloc.${chip_name}.s prelude1.sml.oldalloc.${chip_name}.s prelude2.sml.oldalloc.${chip_name}.s
#prelude_objs=${objdir}/prelude0.sml.o ${objdir}/prelude1.sml.o ${objdir}/prelude2.sml.o
#prelude_hprof_objs=${objdir}/prelude0.sml.hprof.o ${objdir}/prelude1.sml.hprof.o ${objdir}/prelude2.sml.hprof.o
#prelude_oldalloc_objs=${objdir}/prelude0.sml.oldalloc.o ${objdir}/prelude1.sml.oldalloc.o ${objdir}/prelude2.sml.oldalloc.o

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
CFLAGS=-c -g -D${sys_name}
#CFLAGS=-c -O2 -D${sys_name}

alpha_osf_asflags=-O1 -g
rs_aix_asflags=-mppc 
META_ASFLAGS=${sys_name}_asflags
ASFLAGS=${${META_ASFLAGS}} -D${sys_name}

alpha_osf_ldflags=-D 40000000 -T 20000000 -non_shared
rs_aix_ldflags=-H512 -T512
META_LDFLAGS=${sys_name}_ldflags
ifeq ($(ATOM),1)
	LDFLAGS=-r -N ${${META_LDFLAGS}}
else
	LDFLAGS=${${META_LDFLAGS}}
endif


alpha_osf_crt=/usr/lib/cmplrs/cc/crt0.o 
rs_aix_crt=/usr/lib/crt0.o 
CRT=${${sys_name}_crt}



# --------------------------- Make asm target with default rules --------
.s.o:
	as ${ASFLAGS} -o $@ $*.s



# ------------------------------ Link Phase ----------------------------
runtime.${sys_name}.a: ${preludes} ${prelude_objs}  ${COREFILES_OBJ}
	rm -f ${runtime_lib}
	ar qc ${runtime_lib} ${COREFILES_OBJ} ${prelude_objs} 


runtime.${sys_name}.hprof.a: ${prelude_hprofs} ${prelude_hprof_objs}  ${COREFILES_HPROF_OBJ}
	rm -f ${runtime_lib}
	ar qc ${runtime_lib} ${COREFILES_HPROF_OBJ} ${prelude_hprof_objs} 

runtime.${sys_name}.oldalloc.a: ${prelude_oldallocs} ${prelude_oldalloc_objs}  ${COREFILES_OBJ}
	rm -f ${runtime_lib}
	ar qc ${runtime_lib} ${COREFILES_OBJ} ${prelude_oldalloc_objs} 


target_lib: ${runtime_lib}
	cp ${runtime_lib} ${target_lib}


prog: ${targetobjs} ${runtime_lib}
	rm -f ${out}
	ld ${LDFLAGS} -o ${out} ${CRT} ${targetobjs} ${runtime_lib} -lm -lc -lots
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
${objdir}/prelude0.sml.o: prelude0.sml.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude0.sml.${chip_name}.s

${objdir}/prelude1.sml.o: prelude1.sml.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude1.sml.${chip_name}.s

${objdir}/prelude2.sml.o: prelude2.sml.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude2.sml.${chip_name}.s

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

${objdir}/gc.o: gc.c tag.h general.h platform.h
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

# ---------------------------- Core oldalloc Code ----------------------------
${objdir}/prelude0.sml.oldalloc.o: prelude0.sml.oldalloc.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude0.sml.oldalloc.${chip_name}.s

${objdir}/prelude1.sml.oldalloc.o: prelude1.sml.oldalloc.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude1.sml.oldalloc.${chip_name}.s

${objdir}/prelude2.sml.oldalloc.o: prelude2.sml.oldalloc.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude2.sml.oldalloc.${chip_name}.s

# ---------------------------- Core hprof Code ----------------------------
${objdir}/prelude0.sml.hprof.o: prelude0.sml.hprof.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude0.sml.hprof.${chip_name}.s

${objdir}/prelude1.sml.hprof.o: prelude1.sml.hprof.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude1.sml.hprof.${chip_name}.s

${objdir}/prelude2.sml.hprof.o: prelude2.sml.hprof.${chip_name}.s
	as ${ASFLAGS} -o $@ prelude2.sml.hprof.${chip_name}.s

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


