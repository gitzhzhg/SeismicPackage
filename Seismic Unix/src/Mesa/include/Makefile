# Makefile for ..par/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

J = $(IMGL)

INCLUDE =	$J/viewer3.h 	\
		$J/trackball.h


INSTALL	: $(INCLUDE)
	@-rm -f INSTALL	
	@touch $@

$(INCLUDE):	$(IMGLTARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $I

remake	:
	@-rm -f $(INCLUDE) INSTALL
	@$(MAKE)
