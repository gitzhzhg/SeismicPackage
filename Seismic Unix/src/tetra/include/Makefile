# Makefile for ..GBmod/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

J = $(ITETRA)

INCLUDE =	$(ITETRA)/tetra.h

INSTALL	: $J $(INCLUDE)
	@-rm -f INSTALL
	@touch $@

$(INCLUDE):	$(ITETRATARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $J

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)

$J	:
	@-mkdir $J
