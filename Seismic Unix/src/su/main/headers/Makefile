# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config


D = $L/libcwp.a $L/libpar.a $L/libsu.a


LFLAGS= $(PRELFLAGS) -L$L -lsu -lpar -lcwp -lm $(POSTLFLAGS)


PROGS =			\
	$B/bhedtopar	\
	$B/su3dchart	\
	$B/suazimuth	\
	$B/suabshw	\
	$B/suahw	\
	$B/suaddhead	\
	$B/sucdpbin	\
	$B/suchart	\
	$B/suchw	\
	$B/sucliphead	\
	$B/sucountkey	\
	$B/sudumptrace	\
	$B/suedit	\
	$B/sugethw	\
	$B/suhtmath	\
	$B/sukeycount	\
	$B/sulcthw	\
	$B/sulhead	\
	$B/supaste	\
	$B/surange	\
	$B/surandhw	\
	$B/susehw	\
	$B/sushw	\
	$B/sustrip	\
	$B/sutrcount	\
	$B/suutm	\
	$B/suxedit	


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
