# Makefile for ...par/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(TETRACFLAGS)

LFLAGS= $(PRELFLAGS) -L$L -ltetra -lsu -lpar -lcwp -lm  $(POSTLFLAGS)

PROGS =			\
	$B/tetramod	\
	$B/sutetraray

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
