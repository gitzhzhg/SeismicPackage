# Makefile for ..xplot/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

INCLUDE = $I/xplot.h $I/garnish.h $I/xwindow.h $I/picking.h

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
