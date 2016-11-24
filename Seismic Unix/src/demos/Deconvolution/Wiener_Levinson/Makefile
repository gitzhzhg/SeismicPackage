# Makefile for ...cwp/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

B = .

LFLAGS = $(PRELFLAGS) -L$L -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/traces


INSTALL:	$(PROGS)
	@-rm -f INSTALL
	@touch $@

$(PROGS):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	$(MCHMODLINE)
	@echo $(@F) installed in $B
remake:
	@-rm -f $(PROGS) INSTALL
	@$(MAKE) INSTALL

clean	::
	rm -f *.o *.a junk*
