# Makefile for ...par/shell

include $(CWPROOT)/src/Makefile.config

SHELLS =		\
	$B/gendocs	\
	$B/striptotxt	\
	$B/updatedoc	\
	$B/updatedocall	\
	$B/updatehead


INSTALL:	$(SHELLS)
	@touch $@

$(SHELLS):	$(SHTARGET)
	@-rm -f $@
	@cp $? $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

list:
	@ls $(SHELLS) | pr -5 -t -l22 | p 

remake:
	@-rm -f $(SHELLS)
	@$(MAKE)
