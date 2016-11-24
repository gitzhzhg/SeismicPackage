# Makefile for ...cwp/shell

include $(CWPROOT)/src/Makefile.config

SHELLS =		\
	$B/merge2	\
	$B/merge4	\
	$B/pstopdfcymk

LINKS =			\
	$B/merge2v

INSTALL:	$(SHELLS) $(LINKS)
	@touch $@


$(SHELLS):	$(SHTARGET)
	@-rm -f $@
	@cp $? $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

$(LINKS):	$B/merge2
	@-rm -f $@
	@$(LN) $B/merge2 $@
	@echo $(@F) installed in $B

remake:
	@-rm -f $(SHELLS) $(LINKS)
	@$(MAKE) ROOT=$(ROOT)
