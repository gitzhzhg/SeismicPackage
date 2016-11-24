# Makefile for ..cwp/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

INCLUDE = $I/cwp.h

INSTALL	: $(INCLUDE) $I
	-rm -f INSTALL
	touch $@

$(INCLUDE):	$(ITARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $I

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE) INSTALL
