# Makefile for ...par/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h $I/par.h

CFLAGS = $(MGLCFLAGS)

LFLAGS =  $(MGLLFLAGS)

PROGS =			\
	$B/trip		\
	$B/viewer3

INSTALL:	$(PROGS)
	@-rm -f INSTALL
	@touch $@

$(PROGS):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	$(MCHMODLINE)
	@echo $(@F) installed in $B

remake:
	@-rm -f $(PROGS) INSTALL
	@$(MAKE)

clean	:
	rm -f junk* core a.out *.o
