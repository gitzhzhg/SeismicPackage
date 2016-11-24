# Makefile for ..GBmod/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

J = $(IWPC)

INCLUDE =				\
		$(IWPC)/wpc.h		\
		$(IWPC)/wpcbitbuff.h	\
		$(IWPC)/wpcbuffer.h	\
		$(IWPC)/wpcconfig.h	\
		$(IWPC)/wpclib.h

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
