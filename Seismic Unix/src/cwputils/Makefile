# Makefile for ...cwp/lib

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h

LIB = $L/libcwputils.a

ARCH =				\
	$(LIB)(wallsec.o)	\
	$(LIB)(walltime.o)	\
	$(LIB)(cpusec.o)	\
	$(LIB)(cputime.o)

INSTALL:	$(LIB) $L
	@touch $@

$(ARCH)	: $D

$(LIB)	:	$(ARCH)
	$(RANLIB) $(LIB)

remake	:
	@-rm -f $(LIB)
	@$(MAKE)

list	:
	$(AR) tv $(LIB)

clean:
	rm -f junk* core a.out

.PRECIOUS:	 $(LIB)
