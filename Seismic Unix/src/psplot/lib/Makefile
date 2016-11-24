# Makefile for ...psplot/lib

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h $I/psplot.h $L/libcwp.a

LFLAGS= -L$L -lcwp

LIB = $L/libpsplot.a

ARCH =				\
	$(LIB)(basic.o)	\
	$(LIB)(psAxesBox.o)	\
	$(LIB)(psAxesBox3.o)	\
	$(LIB)(psContour.o)	\
	$(LIB)(psCAxesBox.o)	\
	$(LIB)(psDrawCurve.o)	\
	$(LIB)(psLegendBox.o)	\
	$(LIB)(psWiggle.o)

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
