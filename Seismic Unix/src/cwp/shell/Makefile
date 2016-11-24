# Makefile for ...cwp/shell

include $(CWPROOT)/src/Makefile.config

SHELLS =		\
	$B/argv		\
	$B/copyright	\
	$B/cpall	\
	$B/cwpfind	\
	$B/dirtree	\
	$B/filetype	\
	$B/CWPGrep	\
	$B/newcase	\
	$B/overwrite	\
	$B/precedence	\
	$B/rcslocks	\
	$B/replace	\
	$B/sys_arch	\
	$B/this_year	\
	$B/time_now	\
	$B/todays_date	\
	$B/weekday	\
	$B/usernames	\
	$B/varlist	\
	$B/why		\
	$B/zap

LINKS = $B/rcpall

INSTALL:	$(SHELLS) $(LINKS)
	@touch $@


$(SHELLS):	$(SHTARGET)
	@-rm -f $@
	@cp $? $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

$(LINKS):	$B/cpall
	@-rm -f $B/rcpall
	@cp $B/cpall $B/rcpall
	@echo $B/rcpall installed in $B

remake:
	@-rm -f $(SHELLS) $(LINKS) INSTALL
	@$(MAKE) INSTALL
