# Makefile for ..Complex/include
# Just say "make"

include $(CWPROOT)/src/Makefile.config

INCLUDE = $I/Complex.h $I/Dcomplex.h

INSTALL	: $(INCLUDE) $I
	touch $@

$(INCLUDE):	$(ITARGET)
	@rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $I

$I	:
	-mkdir $I

remake	:
	@touch *.h
	@$(MAKE)

clean	:
	-rm -f core *.o *.a junk*

