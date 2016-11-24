# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config


D = $L/libcwp.a $L/libpar.a $L/libsu.a


LFLAGS= $(PRELFLAGS) -L$L -lsu -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/sulfaf	\
	$B/subfilt	\
	$B/sudipfilt	\
	$B/succfilt	\
	$B/sufilter	\
	$B/sufrac	\
	$B/sufwatrim	\
	$B/suk1k2filter	\
	$B/sukfrac	\
	$B/sukfilter	\
	$B/sumedian	\
	$B/susmgauss2	\
	$B/suphase	\
	$B/sutvband


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
