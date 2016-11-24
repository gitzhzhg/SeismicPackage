# Makefile for ...par/lib

include $(CWPROOT)/src/Makefile.config

LFLAGS= -L$L -lpar -lcwp

CFLAGS = $(WPCCFLAGS)

LIB = $L/libwpc1.a

ARCH =				\
	$(LIB)(wbuffalloc.o)	\
	$(LIB)(wpc1coding.o)	\
	$(LIB)(wpc1trans.o)	\
	$(LIB)(wpc1quant.o)	\
	$(LIB)(wpc1.o)

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
