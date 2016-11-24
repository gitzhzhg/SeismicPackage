# Makefile for ...xplot/lib

include $(CWPROOT)/src/Makefile.config

D = $I/xplot.h

CFLAGS = $(XCFLAGS) 

LFLAGS= -L$L

LIB = $L/libxplot.a

ARCH =				\
	$(LIB)(axesbox.o)	\
	$(LIB)(colormap.o)	\
	$(LIB)(drawcurve.o)	\
	$(LIB)(image.o)		\
	$(LIB)(legendbox.o)	\
	$(LIB)(xContour.o)	\
	$(LIB)(rubberbox.o)	\
	$(LIB)(window.o)	\
	$(LIB)(garnish.o)

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
