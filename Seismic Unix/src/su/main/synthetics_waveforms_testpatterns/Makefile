# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config


D = $L/libcwp.a $L/libpar.a $L/libsu.a


LFLAGS= $(PRELFLAGS) -L$L -lsu -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/suaddevent	\
	$B/suea2df	\
	$B/sudgwaveform	\
	$B/sufctanismod	\
	$B/sufdmod1	\
	$B/sufdmod2	\
	$B/sufdmod2_pml	\
	$B/sugoupillaud	\
	$B/sugoupillaudpo	\
	$B/suimp2d	\
	$B/suimp3d	\
	$B/suimpedance	\
	$B/sukdsyn2d	\
	$B/sunhmospike	\
	$B/sunull	\
	$B/suplane	\
	$B/surandspike	\
	$B/suremac2d	\
	$B/suremel2dan	\
	$B/suspike	\
	$B/susyncz	\
	$B/susynlv	\
	$B/susynlvcw	\
	$B/susynlvfti	\
	$B/susynvxz	\
	$B/susynvxzcs	\
	$B/suvibro	\
	$B/suwaveform


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
