# Makefile for ...par/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

D = $I/cwp.h $I/par.h $L/libcwp.a $L/libpar.a

LFLAGS= $(PRELFLAGS) -L$L -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/a2b		\
	$B/a2i		\
	$B/addrvl3d	\
	$B/b2a		\
	$B/cshotplot	\
	$B/cellauto	\
	$B/cpftrend	\
	$B/dzdv		\
	$B/farith	\
	$B/float2ibm	\
	$B/ftnstrip	\
	$B/ftnunstrip	\
	$B/grm		\
	$B/h2b		\
	$B/hti2stiff	\
	$B/hudson	\
	$B/i2a		\
	$B/ibm2float	\
	$B/kaperture	\
	$B/linrort	\
	$B/lorenz	\
	$B/mkparfile	\
	$B/makevel	\
	$B/mrafxzwt	\
	$B/pdfhistogram	\
	$B/prplot	\
	$B/randvel3d	\
	$B/rayt2d	\
	$B/rayt2dan	\
	$B/recast	\
	$B/refRealAziHTI\
	$B/refRealVTI	\
	$B/regrid3	\
	$B/resamp	\
	$B/rossler	\
	$B/smooth2	\
	$B/smooth3d	\
	$B/smoothint2	\
	$B/stiff2vel	\
	$B/subset	\
	$B/swapbytes	\
	$B/thom2hti	\
	$B/thom2stiff	\
	$B/transp	\
	$B/transp3d	\
	$B/tvnmoqc	\
	$B/unif2	\
	$B/unif2aniso	\
	$B/unif2ti2	\
	$B/unisam	\
	$B/unisam2	\
	$B/utmconv	\
	$B/vel2stiff	\
	$B/velconv	\
	$B/velpert	\
	$B/velpertan	\
	$B/verhulst	\
	$B/vtlvz	\
	$B/wkbj		\
	$B/xy2z		\
	$B/z2xyz

INSTALL:	$(PROGS)
	@-rm -f INSTALL
	@touch $@

$(PROGS):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	$(MCHMODLINE)
	@echo $(@F) installed in $B

remake:
	@-rm -f $(PROGS) INSTALL
	@$(MAKE) INSTALL

clean	::
	rm -f junk* core a.out *.o
