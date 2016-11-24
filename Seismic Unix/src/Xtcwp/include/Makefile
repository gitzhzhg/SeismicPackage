# Makefile for ..Xtcwp/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

INCLUDE = $I/Xtcwp/Xtcwp.h $I/Xtcwp/Axes.h $I/Xtcwp/AxesP.h

INSTALL	: $(INCLUDE)
	@-rm -f INSTALL
	@touch $@

$(INCLUDE):	$(XTITARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $I/Xtcwp

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)
