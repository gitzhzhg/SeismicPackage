# Makefile for ...par/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h $I/par.h $L/libcwp.a $L/libpar.a

J = $(IDCT)

CFLAGS = $(DCTCFLAGS)

LFLAGS = $(PRELFLAGS) -L$L  -lcomp -lpar  -lcwp -lm $(POSTLFLAGS)

PROGS = 			\
		$B/dctcomp	\
		$B/dctuncomp	\
		$B/entropy	\
		$B/wptcomp	\
		$B/wptuncomp	\
		$B/wtcomp	\
		$B/wtuncomp

INSTALL:	$(PROGS)
	@-rm -f INSTALL
	@touch $@

$(PROGS):	$(CTARGET) $D
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	$(MCHMODLINE)
	@echo $(@F) installed in $B

remake:
	@-rm -f $(PROGS) INSTALL
	@$(MAKE)

clean	:
	rm -f junk* core a.out *.o
