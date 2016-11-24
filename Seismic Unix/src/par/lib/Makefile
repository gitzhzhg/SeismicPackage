# Makefile for ...par/lib

include $(CWPROOT)/src/Makefile.config

LFLAGS= -L$L -lcwp

LIB = $L/libpar.a

ARCH =				\
	$(LIB)(atopkge.o)	\
	$(LIB)(docpkge.o)	\
	$(LIB)(ealloc.o)	\
	$(LIB)(errpkge.o)	\
	$(LIB)(filestat.o)	\
	$(LIB)(fractal.o)	\
	$(LIB)(getpars.o)	\
	$(LIB)(lincoeff.o)	\
	$(LIB)(minfunc.o)	\
	$(LIB)(modeling.o)	\
	$(LIB)(refaniso.o)	\
	$(LIB)(rke.o)		\
	$(LIB)(smooth.o)	\
	$(LIB)(syscalls.o)	\
	$(LIB)(subcalls.o)	\
	$(LIB)(taup.o)		\
	$(LIB)(VND.o)		\
	$(LIB)(upweik.o)	\
	$(LIB)(wtlib.o)

INSTALL:	$(LIB)
	@-rm -f INSTALL
	@touch $@

$(ARCH)	: $D

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
