# Makefile for ...Xmcwp/main
# The items here depend on the Motif Developers Package
# Just say "make"

include $(CWPROOT)/src/Makefile.config

CFLAGS = $(XMCFLAGS)

D = $L/libXmcwp.a $L/libXtcwp.a $L/libpar.a $L/libcwp.a

LDFLAGS= $(PRELFLAGS) -L$L -L$(LMOTIF) -lXmcwp -lXtcwp \
		-lpar -lcwp -L$(LX11) -lXm -lXt \
		-lX11 $(XLFLAGSADD)  -lm $(POSTLFLAGS)

PROGS = 	\
	$B/fftlab 

XAPPDEFS =		\
		$R/FFtlab

INSTALL:  $(PROGS) $(XAPPDEFS)
	@-rm -f INSTALL
	@touch $@

$(PROGS): $(CTARGET)  $D
	-$(CC) $(CFLAGS) $(@F).c $(LDFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

$(XAPPDEFS):	$(XAPPTARGET)
	@cp $(@F) $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $R

remake:
	@-rm -f $(PROGS) INSTALL
	@$(MAKE) 

clean	:
	rm -f junk* core a.out *.o
