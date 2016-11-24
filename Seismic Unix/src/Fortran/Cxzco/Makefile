# Makefile for ...src/prop/local/Cxzco

include $(CWPROOT)/src/Makefile.config


LFLAGS = libcxzco.a

PROGS	= $B/cxzco $B/cz1 $B/cz1in

LIB = libcxzco.a

ARCH =					\
		$(LIB)(amplit.o)	\
		$(LIB)(anglesv.o)	\
		$(LIB)(cuspln.o)	\
		$(LIB)(dophi.o)		\
		$(LIB)(dxdlam.o)	\
		$(LIB)(dxdrl.o)		\
		$(LIB)(eval.o)		\
		$(LIB)(guessx.o)	\
		$(LIB)(jacob.o)		\
		$(LIB)(magnitude.o)	\
		$(LIB)(newton.o)	\
		$(LIB)(plasol.o)	\
		$(LIB)(rayone.o)	\
		$(LIB)(reccon.o)	\
		$(LIB)(setn.o)		\
		$(LIB)(strat.o)		\
		$(LIB)(tridi.o)		\
		$(LIB)(ttime.o)		\
		$(LIB)(fftpack.o)


INSTALL:	$(LIB) $(PROGS)
	@touch $@


$(PROGS):	$(FTARGET)
	$(FC) $(FFLAGS) $(@F).f -o $@ $(LFLAGS)
	@$(MCHMODLINE)
	@echo $(@F) installed in $B


$(LIB)	: $(ARCH)
	$(RANLIB) $(LIB)

remake:
	@-rm $(LIB) INSTALL $(PROGS)
	$(MAKE)

clean	:
	rm -f *junk* *.a *.o cxzco
