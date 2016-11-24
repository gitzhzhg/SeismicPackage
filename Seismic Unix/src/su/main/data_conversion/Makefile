# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config


D = $L/libcwp.a $L/libpar.a $L/libsu.a


LFLAGS= $(PRELFLAGS) -L$L -lsu -lpar -lcwp -lm $(POSTLFLAGS)


PROGS =			\
	$B/dt1tosu	\
	$B/segyclean	\
	$B/segyhdrmod	\
	$B/segyhdrs	\
	$B/segyread	\
	$B/segywrite	\
	$B/segyscan	\
	$B/setbhed	\
	$B/suascii	\
	$B/suintvel	\
	$B/suoldtonew	\
	$B/sustkvel	\
	$B/suswapbytes	\
	$B/swapbhed


INSTALL	:	$(PROGS)
	@-rm -f INSTALL
	@touch $@


$(PROGS):	$(CTARGET) $D 
	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

remake	:
	-rm -f $(PROGS) INSTALL
	$(MAKE) 
	
clean::
	rm -f a.out junk* JUNK* core
