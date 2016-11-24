# Makefile for ...par/lib

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(MGLCFLAGS)

LFLAGS= -L$L -lcwp

LIB = $L/libtrack.a

ARCH =				\
	$(LIB)(trackball.o)

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
