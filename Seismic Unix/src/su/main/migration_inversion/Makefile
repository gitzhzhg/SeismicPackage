# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config


D = $L/libcwp.a $L/libpar.a $L/libsu.a


LFLAGS= $(PRELFLAGS) -L$L -lsu -lpar -lcwp -lm $(POSTLFLAGS)


PROGS =			\
	$B/sugazmig	\
	$B/suinvvxzco	\
	$B/suinvzco3d	\
	$B/sukdmig2d	\
	$B/sukdmig3d	\
	$B/suktmig2d	\
	$B/sumigfd	\
	$B/sumigffd	\
	$B/sumiggbzoan	\
	$B/sumiggbzo	\
	$B/sumigprefd	\
	$B/sumigpreffd	\
	$B/sumigprepspi	\
	$B/sumigpresp	\
	$B/sumigps	\
	$B/sumigpspi	\
	$B/sumigpsti	\
	$B/sumigsplit	\
	$B/sumigtk	\
	$B/sumigtopo2d	\
	$B/sustolt	\
	$B/sutifowler


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
