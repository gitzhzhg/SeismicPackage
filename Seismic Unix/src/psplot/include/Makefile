# Makefile for ..psplot/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

INCLUDE = $I/psplot.h $I/cwpcmaps.h

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
