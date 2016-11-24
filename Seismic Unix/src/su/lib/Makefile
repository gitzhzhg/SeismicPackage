# Makefile for ...su/lib

include $(CWPROOT)/src/Makefile.config

LFLAGS= -L$L -lpar -lcwp -lm
TFLAGS = -g -I$I

LIB = $L/libsu.a

ARCH =	$(LIB)(elco_scalar.o)	\
	$(LIB)(fgettr.o)	\
	$(LIB)(fgethdr.o)	\
	$(LIB)(fputtr.o)	\
	$(LIB)(fgetgthr.o)	\
	$(LIB)(fputgthr.o)	\
	$(LIB)(hdrpkge.o)	\
	$(LIB)(valpkge.o)	\
	$(LIB)(tabplot.o)	\
	$(LIB)(xdrbhdrsub.o)	\
	$(LIB)(xdrhdrsub.o)

INSTALL:	$(LIB)
	@touch $@

$(LIB)	:	$(ARCH)
	$(RANLIB) $(LIB)


remake	:
	@-rm -f $(LIB)
	@$(MAKE)

list	:
	$(AR) tv $(LIB)


# Testing routines
tests: tgettr tputtr tarray

tgettr:	fgettr.c
	$(CC) $(TFLAGS) -DTEST -DGETTR fgettr.c $(LIB) $(LFLAGS) -o tgettr
	@echo tgettr ready--running benchmark
	suplane offset=-100 | sugethw offset
	@echo convert offset to absolute value
	suplane offset=-100 | tgettr | sugethw offset

tputtr:	fputtr.c
	$(CC) $(TFLAGS) -DTEST -DPUTTR fputtr.c $(LIB) $(LFLAGS) -o tputtr
	@echo tputtr ready--running benchmark
	suplane offset=-100 | sugethw offset
	@echo convert offset to absolute value
	suplane offset=-100 | tputtr | sugethw offset

tarray:	arraypkge.c
	$(CC) $(TFLAGS) -DTEST arraypkge.c $(LIB) $(LFLAGS) -o tarray
	@echo arraypkge ready--running benchmark
	-tarray

clean:
	rm -f ato err io tarray tgetname tgetpar tgettr tputtr tstatfil \
		junk* core a.out xdrbhdrsub.* xdrhdrsub.*

.PRECIOUS: $(LIB)
