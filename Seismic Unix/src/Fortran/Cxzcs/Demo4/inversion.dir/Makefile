# Makefile for ../inversion.dir

include $(CWPROOT)/src/Makefile.config

B = .

PROGS	=	$(B)/getcosines


INSTALL:	$(PROGS)
	@touch $@


$(PROGS):	$(FTARGET)
	$(FC) $(FFLAGS) $(@F).f -o $@
