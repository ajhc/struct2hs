PROG    = main
HSBUILD = hsbuild
HSSRC   = hssrc
HSCODE  = $(wildcard $(HSSRC)/*.hs $(HSSRC)/*/*.hs $(HSSRC)/*/*/*.hs $(HSSRC)/*/*/*/*.hs)
CFLAGS  = '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing -DNDEBUG -O3 '-D_JHC_GC=_JHC_GC_JGC' '-D_JHC_CONC=_JHC_CONC_NONE' '-D_JHC_STANDALONE=0'
INCLUDE = -I. -I${HSBUILD}/cbits -I${HSBUILD}
HSOUT   = ${HSBUILD}/rts/profile.c ${HSBUILD}/rts/rts_support.c ${HSBUILD}/rts/gc_none.c ${HSBUILD}/rts/jhc_rts.c ${HSBUILD}/lib/lib_cbits.c ${HSBUILD}/rts/gc_jgc.c ${HSBUILD}/rts/stableptr.c ${HSBUILD}/rts/conc.c  ${HSBUILD}/hsmain.c

all: ${PROG}
${PROG}: main.c ${HSBUILD}/hsmain.c
	gcc ${CFLAGS} ${INCLUDE} ${HSOUT} main.c -o $@

${HSBUILD}/hsmain.c: ${HSCODE}
	ajhc -fffi --include=$(HSSRC) --tdir=$(HSBUILD) -C -o $@ $(HSSRC)/Main.hs

clean:
	rm -rf ${HSBUILD}
	rm -f $(PROG)
