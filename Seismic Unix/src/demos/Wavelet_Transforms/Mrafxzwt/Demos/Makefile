# Makefile for ...par/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

B = .

D = $I/cwp.h $I/par.h $L/libcwp.a $L/libpar.a

LFLAGS= $(PRELFLAGS) -L$L -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =				\
	$B/createf

INSTALL:	$(PROGS)
	@-rm -f INSTALL
	@touch $@

$(PROGS):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	chmod 755 $@
	@echo $(@F) installed in $B

remake:
	@-rm -f $(PROGS) INSTALL
	@$(MAKE)

clean	:
	rm -f junk* core a.out *.o
