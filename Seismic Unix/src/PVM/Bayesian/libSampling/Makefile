# Makefile for libPosteriori

include $(CWPROOT)/src/Makefile.config
include ../Makefile.bayes

CFLAGS = $(BAYESCFLAGS)

D = $(IBAYES)/ranlib.h

LIB = $L/librandom.a

ARCH =				\
	$(LIB)(cholesky.o)	\
	$(LIB)(linearAlg.o)	\
	$(LIB)(ranlib.o)	\
	$(LIB)(com.o)

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
