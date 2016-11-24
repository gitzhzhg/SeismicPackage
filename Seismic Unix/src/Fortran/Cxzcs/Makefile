# Makefile for ../Cxzcs


include $(CWPROOT)/src/Makefile.config




PROGS = $B/cxzcs $B/cz1 $B/cz1in

LIB = libcxzcxs.a

ARCH	=				\
		$(LIB)(fftpack.o)	\
		$(LIB)(cxzcssub.o)

INSTALL: 	$(LIB)	$(PROGS)
	@touch $@

$(PROGS):	$(FTARGET) $(LIB)
	$(FC) $(FFLAGS) $(@F).f -o $@ $(LIB)
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

$(LIB)	: $(ARCH)
	$(RANLIB) $(LIB)
	
remake:
	-rm $(PROGS) INSTALL $(LIB)
	$(MAKE)

clean	:
	rm -f *junk* *.o cxzcs *.shot
