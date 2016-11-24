# Makefile for ...par/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(TRICFLAGS)

D =	$I/cwp.h	\
	$I/par.h	\
	$(ITRI)/tri.h	\
	$(ITRI)/sloth.h	\
	$(ITRI)/triP.h	\
	$(ITRI)/elastic.h \
	$L/libcwp.a	\
	$L/libpar.a	\
	$L/libtrielas.a	\
	$L/libtriang.a

LFLAGS= $(PRELFLAGS) -L$L -ltrielas -ltriang -lpar -lcwp -lm  $(POSTLFLAGS)

PROGS =			\
	$B/gbbeam	\
	$B/elasyn	\
	$B/elacheck	\
	$B/elamodel	\
	$B/elaray	\
	$B/elatriuni	\
	$B/raydata

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
