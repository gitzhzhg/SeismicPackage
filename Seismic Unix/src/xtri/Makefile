# Makefile for ...xtri/
# Just say "make"

include $(CWPROOT)/src/Makefile.config


CFLAGS = $(XTRICFLAGS) 

D =	$I/cwp.h	\
	$I/par.h	\
	$(ITRI)/tri.h	\
	$(ITRI)/triP.h	\
	$(ITRI)/sloth.h	\
	$I/Xtcwp/Xtcwp.h	\
	$L/libcwp.a	\
	$L/libpar.a	\
	$L/libXtcwp.a	\
	$L/libtriang.a

LFLAGS=  $(PRELFLAGS) -L$L -L$(LX11) -lXtcwp -lXt -lX11 -ltriang -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/sxplot

INSTALL:	$(PROGS)
	@touch $@

$(PROGS):	$(CTARGET) $D
	$(CC) $(XCFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

remake:
	@-rm -f $(PROGS)
	@$(MAKE)

clean	:
	rm -f junk* core a.out *.o
