# Makefile for ...psplot/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h $I/par.h $I/psplot.h $L/libcwp.a $L/libpar.a $L/libpsplot.a

LFLAGS=  $(PRELFLAGS) -L$L -lpsplot -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/psbbox	\
	$B/pscontour	\
	$B/pscube	\
	$B/pscubecontour\
	$B/psepsi	\
	$B/psgraph	\
	$B/psimage	\
	$B/psmerge	\
	$B/psmovie	\
	$B/pslabel	\
	$B/psmanager	\
	$B/pswigb	\
	$B/pswigp

INSTALL:	$(PROGS)
	@touch $@

$(PROGS):	 $(CTARGET) $D
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	$(MCHMODLINE)
	@echo $(@F) installed in $B

remake:
	@-rm -rf $(PROGS)
	@$(MAKE)

clean	:
	rm -f junk* core a.out *.o
