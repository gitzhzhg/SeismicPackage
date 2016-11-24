# Makefile for ...par/lib

include $(CWPROOT)/src/Makefile.config


CFLAGS = $(DCTCFLAGS)

LFLAGS = $(PRELFLAGS) -L$L -lpar -lcwp -lm $(POSTLFLAGS)

LIB = $L/libcomp.a

ARCH =				\
	$(LIB)(buffalloc.o)	\
	$(LIB)(huffman.o)	\
	$(LIB)(pcoding.o)	\
	$(LIB)(quant.o)		\
	$(LIB)(rle.o)		\
	$(LIB)(dct1.o)		\
	$(LIB)(dct2.o)		\
	$(LIB)(dctalloc.o)	\
	$(LIB)(getfilter.o)	\
	$(LIB)(lct1.o)		\
	$(LIB)(lpred.o)		\
	$(LIB)(wavepack1.o)	\
	$(LIB)(wavepack2.o)	\
	$(LIB)(wavetrans1.o)	\
	$(LIB)(wavetrans2.o)

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
