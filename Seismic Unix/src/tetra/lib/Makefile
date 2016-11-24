# Makefile for ..GBmod/lib

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(TETRACFLAGS)

D = $(ITETRA)/tetra.h

LIB = $L/libtetra.a

ARCH =			\
	$(LIB)(tetra.o)

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
