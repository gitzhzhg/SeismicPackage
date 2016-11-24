# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config


D = $L/libcwp.a $L/libpar.a $L/libsu.a


LFLAGS= $(PRELFLAGS) -L$L -lsu -lpar -lcwp -lm $(POSTLFLAGS)

PROGS =			\
	$B/suanalytic	\
	$B/suamp	\
	$B/succwt	\
	$B/succepstrum	\
	$B/sucepstrum	\
	$B/suicepstrum	\
	$B/suclogfft	\
	$B/suiclogfft	\
	$B/sucwt	\
	$B/sufft	\
	$B/sugabor	\
	$B/suhilb	\
	$B/suifft	\
	$B/suminphase	\
	$B/suradon	\
	$B/suslowft	\
	$B/suslowift	\
	$B/suspecfk	\
	$B/suspecfx	\
	$B/suspeck1k2	\
	$B/suphasevel	\
	$B/sutaup	\
	$B/suwfft	\
	$B/suzerophase
       


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
