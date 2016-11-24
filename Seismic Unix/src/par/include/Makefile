# Makefile for ..par/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

INCLUDE =	$I/par.h	\
		$I/taup.h	\
		$I/VND.h	\
		$I/wt.h		\
		$I/anisotropy.h	\
		$I/rke.h

INSTALL	: $(INCLUDE)
	@-rm -f INSTALL	
	@touch $@

$(INCLUDE):	$(ITARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $I

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)
