# Makefile for ..GBmod/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

J = $(ITRI)

INCLUDE =	$(ITRI)/elastic.h

INSTALL	: $J $(INCLUDE)
	@-rm -f INSTALL
	@touch $@

$(INCLUDE):	$(ITRITARGET)
	@-rm -f $@
	@cp $? $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $J

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)

$J	:
	@-mkdir $J
