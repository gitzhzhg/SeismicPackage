# Makefile for ...par/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h $I/par.h $L/libcwp.a $L/libpar.a

J = $(IWPC)

CFLAGS = $(WPCCFLAGS)

LFLAGS= $(PRELFLAGS) -L$L -lwpc1 -lpar -lcwp -lm $(POSTLFLAGS)

PROGS = 		\
		$B/wpc1comp2	\
		$B/wpc1uncomp2	


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
