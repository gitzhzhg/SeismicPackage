# Makefile for ..GBmod/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

J = $(IWPC)

INCLUDE =				\
		$(IWPC)/wpc1.h		\
		$(IWPC)/wpc1bitbuff.h	\
		$(IWPC)/wpc1buffer.h	\
		$(IWPC)/wpc1config.h	\
		$(IWPC)/wpc1lib.h

INSTALL	: $J $(INCLUDE)
	@-rm -f INSTALL
	@touch $@

$(INCLUDE):	$(IWPCTARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $J

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)

$J	:
	@-mkdir $J
