# Makefile for ...Xtcwp/lib

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(XCFLAGS)

D = $I/Xtcwp/Xtcwp.h  $I/Xtcwp/Axes.h $I/Xtcwp/AxesP.h

LFLAGS= -L$L

LIB = $L/libXtcwp.a

ARCH =				\
	$(LIB)(Axes.o)		\
	$(LIB)(fx.o)		\
	$(LIB)(colormap.o)	\
	$(LIB)(misc.o)		\
	$(LIB)(resconv.o)	\
	$(LIB)(rubberbox.o)

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
