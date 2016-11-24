# Makefile for ...par/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(TRICFLAGS)

D =	$I/cwp.h	\
	$I/par.h	\
	$(ITRI)/tri.h	\
	$(ITRI)/sloth.h	\
	$(ITRI)/triP.h	\
	$L/libcwp.a	\
	$L/libpar.a	\
	$L/libtriang.a

LFLAGS= $(PRELFLAGS) -L$L -ltriang -lpar -lcwp -lm  $(POSTLFLAGS)

PROGS =			\
	$B/gbbeam	\
	$B/triseis	\
	$B/trimodel	\
	$B/triray	\
	$B/normray	\
	$B/tri2uni	\
	$B/uni2tri

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
