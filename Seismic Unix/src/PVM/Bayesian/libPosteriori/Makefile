# Makefile for libPosteriori

include $(CWPROOT)/src/Makefile.config
include ../Makefile.bayes

CFLAGS = $(BAYESCFLAGS)

D = $(IBAYES)/posteriori.h

LIB = $L/libposteriori.a

ARCH =				\
	$(LIB)(disp.o)		\
	$(LIB)(frechet.o)	\
	$(LIB)(grad.o)		\
	$(LIB)(input.o)		\
	$(LIB)(reflectivity.o)	\
	$(LIB)(surface.o)

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
