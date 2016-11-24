#makefile for vzest

include $(CWPROOT)/src/Makefile.config


LFLAGS = $(PRELFLAGS) -L$L -lpar -lcwp -lm $(POSTLFLAGS)

PROGS	=	$B/AzimVelAn

#if you don't have cwp and par  installed then switch the INSTALL lines below
INSTALL:	$(PROGS)
	@touch $@


$(PROGS):	$(FTARGET) $(LIB)
	$(FC) $(FFLAGS) $(@F).f -o $@ $(LIB)
	@$(MCHMODLINE)
	@echo installing $(@F) in $B

remake:
	@-rm  INSTALL $(PROGS)
	$(MAKE)
clean	:
	rm -f *junk* *.a *.o *.shot cshotplot
