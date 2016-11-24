# Makefile for ...su/graphics/x

include $(CWPROOT)/src/Makefile.config

D = $L/libcwp.a $L/libpar.a $L/libsu.a

LFLAGS = $(PRELFLAGS) -L$L -lsu -lpar -lcwp -lm $(POSTLFLAGS)

PROGS  =	$B/suxcontour	\
		$B/suximage	\
		$B/suxwigb	\
		$B/suxpicker	\
		$B/suxgraph	\
		$B/suxmax	\
		$B/suxmovie

INSTALL	: $(PROGS)
	@-rm -f INSTALL
	@touch $@

$(PROGS)	: $(CTARGET) $D
	@echo installing $(@F) in $B
	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)

remake	:
	@-rm -f $(PROGS) INSTALL
	$(MAKE)
	
clean:
	rm -f a.out junk* JUNK* core
