# Makefile for ..cwp/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

J = $(IRFL)

INCLUDE = $J/reflpsvsh.h

INSTALL	: $(INCLUDE) $I
	-rm -f INSTALL
	touch $@

$(INCLUDE):	$(IRFLTARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $I

$J	:
	-mkdir $J

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)
