# Makefile for ..GBmod/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

J = $(ITRI)

INCLUDE =	$(ITRI)/tri.h	\
		$(ITRI)/sloth.h	\
		$(ITRI)/triP.h

INSTALL	: $J $(INCLUDE)
	@-rm -f INSTALL
	@touch $@

$(INCLUDE):	$(ITRITARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $J

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)

$J	:
	@-mkdir $J
