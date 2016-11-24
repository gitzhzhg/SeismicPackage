# Makefile for ...xplot/main
# Just say "make"
# xepsp, xepsb, xpsp are NOT automatically compiled (portability)

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h $I/par.h $I/xplot.h $L/libcwp.a $L/libpar.a $L/libxplot.a

LFLAGS= $(PRELFLAGS) -L$L -lxplot -L$(LX11) $(XLFLAGSADD) -lX11 -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/lcmap	\
	$B/lprop	\
	$B/scmap	\
	$B/xcontour	\
	$B/ximage	\
	$B/xwigb	\
	$B/xpicker

PSPROGS = $B/xepsp $B/xepsb

INSTALL:	$(PROGS)
	@-rm -f INSTALL
	@touch $@

$(PROGS):	$(CTARGET) $D
	$(CC) $(XCFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

# this one needs Display PostScript
# You will have to make these by hand by typing:
# make $CWPROOT/bin/xepsp   
# make $CWPROOT/bin/xepsb
# make $CWPROOT/bin/xpsp
$B/xepsp:	$(CTARGET) $D
	$(CC) $(CFLAGS) xepsp.c $(LFLAGS) -ldps -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

# this one needs Display PostScript
$B/xepsb:	$(CTARGET) $D
	$(CC) $(CFLAGS) xepsb.c $(LFLAGS) -ldps -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

# this one needs Display PostScript
$B/xpsp:	$(CTARGET) $D
	#$(CC) $(CFLAGS) xpsp.c $(LFLAGS) -bnso -bI:/lib/syscalls.exp -ldps -o $@
	$(CC) $(CFLAGS) xpsp.c -lX11 -ldps -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

remake:
	@-rm -f  $(PROGS) INSTALL
	@$(MAKE)

clean	:
	rm -f junk* core a.out *.o
