# Makefile for ...par/lib

include $(CWPROOT)/src/Makefile.config

LFLAGS= -L$L -lpar -lcwp

CFLAGS = $(WPCCFLAGS)

LIB = $L/libwpc.a

ARCH =				\
	$(LIB)(wpcbuffal.o)	\
	$(LIB)(wpc.o)		\
	$(LIB)(wpccoding.o)	\
	$(LIB)(wpcendec.o)	\
	$(LIB)(wpchuff.o)	\
	$(LIB)(wpcpack2.o)	\
	$(LIB)(wpcquant.o)	\
	$(LIB)(wpcsilence.o)

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
