# Makefile for ...psplot/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

D =	$I/cwp.h	\
	$I/par.h	\
	$(ITRI)/tri.h	\
	$(ITRI)/sloth.h	\
	$(ITRI)/triP.h	\
	$I/psplot.h	\
	$L/libcwp.a	\
	$L/libpar.a	\
	$L/libpsplot.a	\
	$L/libtriang.a

LFLAGS= $(PRELFLAGS) -L$L -ltriang -lpsplot -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/spsplot

INSTALL:	$(PROGS)
	@touch $@

$(PROGS):	 $(CTARGET) $D
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	$(MCHMODLINE)
	@echo $(@F) installed in $B

remake:
	@-rm -rf $(PROGS) INSTALL
	@$(MAKE)

clean	:
	rm -f junk* core a.out *.o
