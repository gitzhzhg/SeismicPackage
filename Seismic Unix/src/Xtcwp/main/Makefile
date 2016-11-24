# Makefile for ...Xtcwp/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

D = $L/libXtcwp.a $L/libpar.a $L/libcwp.a

# use the first LDFLAGS if on a non-motif system
LDFLAGS= $(PRELFLAGS) -L$L $(XLFLAGSADD) -lXtcwp -lpar -lcwp -L$(LX11) -lXt -lX11 -lm $(POSTLFLAGS)
#LDFLAGS= $(PRELFLAGS) -L$L -lXtcwp $(XLFLAGSADD) -lpar -lcwp -L$(LX11) -lXm -lXt -lX11 -lXaw -lm $(POSTLFLAGS)

PROGS = $B/xgraph \
	$B/xmovie \
	$B/xrects

XAPPDEFS =	$R/XGraph_XAPP_DEF \
	 	$R/XMovie_XAPP_DEF \
	 	$R/XRects_XAPP_DEF

INSTALL:  $(PROGS) $(XAPPDEFS)
	@-rm -f INSTALL
	@touch $@

$(PROGS): $(CTARGET)  $D
	-$(CC) $(XCFLAGS) $(@F).c $(LDFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

$(XAPPDEFS):	$(XAPPTARGET)
	@cp $(@F) $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $R

remake:
	@-rm -f $(PROGS) $(XAPPDEFS) INSTALL
	@$(MAKE) 

clean	:
	rm -f junk* core a.out *.o
