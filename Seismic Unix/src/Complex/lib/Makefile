# Makefile for ...Complex/lib

include $(CWPROOT)/src/Makefile.config

D = $I/Complex.h

LIB = $L/libComplex.a

ARCH =				\
	$(LIB)(Complex.o)

INSTALL:	$(LIB)
	@touch $@

$(ARCH)	: $D

$(LIB)	:	$(ARCH)
	ranlib $(LIB)

remake	:
	@-rm -f $(LIB)
	@$(MAKE)

clean:
	rm -f junk* core a.out

.PRECIOUS:	 $(LIB)
