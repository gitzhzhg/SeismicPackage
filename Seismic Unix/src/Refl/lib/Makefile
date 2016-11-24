# Makefile for ...su/lib

include $(CWPROOT)/src/Makefile.config

LFLAGS= -L$L -lpar -lcwp -lm
TFLAGS = -g -I$I

LIB = $L/libreflect.a

ARCH =				\
	$(LIB)(compute.o)	\
	$(LIB)(convolve.o)	\
	$(LIB)(auxil.o)		\
	$(LIB)(psvreflect.o)	\
	$(LIB)(shreflect.o)	\
	$(LIB)(synthetic.o)


INSTALL:	$(LIB)
	@-rm -f INSTALL
	@touch $@

$(LIB)	:	$(ARCH)
	$(RANLIB) $(LIB)

remake	:
	@-rm -f $(LIB) INSTALL
	@$(MAKE)

list	:
	$(AR) tv $(LIB)
