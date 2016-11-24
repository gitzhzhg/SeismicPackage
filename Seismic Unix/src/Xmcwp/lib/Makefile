# Makefile for ...Xmcwp/lib

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(XMCFLAGS) 

LFLAGS= -L$L

LIB = $L/libXmcwp.a

ARCH = 			\
	$(LIB)(radiobuttons.o)	\
	$(LIB)(samples.o)

INSTALL:	$(LIB)
	@-rm -f INSTALL
	@touch $@

$(ARCH)	: $D

$(LIB)	: 	$(ARCH)
	$(RANLIB) $(LIB)

remake	:
	@-rm -f $(LIB) INSTALL
	@$(MAKE)

list	:
	$(AR) tv $(LIB)

clean:
	rm -f junk* core a.out

.PRECIOUS:	 $(LIB)
