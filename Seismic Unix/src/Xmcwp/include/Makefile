# Makefile for ..Xmcwp/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

INCLUDE = $I/Xmcwp/Xmcwp.h 

INSTALL	: $(INCLUDE)
	@-rm -f INSTALL
	@touch $@

$(INCLUDE):	$(XMITARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $I/Xmcwp

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)
