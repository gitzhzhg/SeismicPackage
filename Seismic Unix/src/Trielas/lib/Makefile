# Makefile for ..GBmod/lib

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(TRICFLAGS)

D = $(ITRI)/tri.h $(ITRI)/triP.h

LIB = $L/libtrielas.a

ARCH =				\
	$(LIB)(findPiso.o)	\
	$(LIB)(findSiso.o)	\
	$(LIB)(findqSH.o)	\
	$(LIB)(rottensh.o)	\
	$(LIB)(rottensor.o)	\
	$(LIB)(rotvector.o)	\
	$(LIB)(rt_SHa_real.o)	\
	$(LIB)(rt_ani_real.o)	\
	$(LIB)(rt_iso_cmplx.o)	\
	$(LIB)(rt_iso_real.o)	\
	$(LIB)(solveForSlowqP.o)\
	$(LIB)(solveForSlowqSV.o)

INSTALL:	$(LIB)
	@-rm -f INSTALL
	@touch $@

$(ARCH)	:	$D

$(LIB)	:	$(ARCH)
	$(RANLIB) $(LIB)

remake	:
	@-rm -f $(LIB) INSTALL
	@$(MAKE)

list	:
	$(AR) tv $(LIB)

clean:
	rm -f junk* core a.out

.PRECIOUS:	 $(LIB)
