# Makefile for ..GBmod/lib

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(TRICFLAGS)

D = $(ITRI)/tri.h $(ITRI)/triP.h

LIB = $L/libtriang.a

ARCH =			\
	$(LIB)(check.o)		\
	$(LIB)(circum.o)	\
	$(LIB)(colinear.o)	\
	$(LIB)(create.o)	\
	$(LIB)(delete.o)	\
	$(LIB)(distance.o)	\
	$(LIB)(fixedges.o)	\
	$(LIB)(inside.o)	\
	$(LIB)(nearest.o)	\
	$(LIB)(project.o)	\
	$(LIB)(readwrite.o)

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
