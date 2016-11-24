# Makefile for ...su/shell

include $(CWPROOT)/src/Makefile.config

SHELLS =		\
	$B/lookpar	\
	$B/maxdiff	\
	$B/recip	\
	$B/rmaxdiff	\
	$B/suagc	\
	$B/suband	\
	$B/sudiff	\
	$B/sudoc	\
	$B/suenv	\
	$B/suhelp	\
	$B/sufind	\
	$B/sufind2	\
	$B/sugendocs	\
	$B/sukeyword	\
	$B/suname	\
	$B/unglitch

PROGS = $B/percent
LINKS =			\
	$B/susum	\
	$B/suprod	\
	$B/suquo	\
	$B/suptsum	\
	$B/suptprod	\
	$B/suptquo	\
	$B/suptdiff

INSTALL:	$(SHELLS) $(PROGS) $(LINKS)
	@touch $@

$(SHELLS):	$(SHTARGET)
	@-rm -f $@
	@cp $? $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

$(LINKS):	$B/sudiff
	@-rm -f $(LINKS)
	@$(LN) $B/sudiff $B/susum
	@$(LN) $B/sudiff $B/suprod
	@$(LN) $B/sudiff $B/suquo
	@$(LN) $B/sudiff $B/suptdiff
	@$(LN) $B/sudiff $B/suptsum
	@$(LN) $B/sudiff $B/suptprod
	@$(LN) $B/sudiff $B/suptquo
	@echo links installed: $(LINKS)

$(PROGS):	$(CTARGET)
	@$(CC) $? -o $@ $(LFLAGS)
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

list:
	@ls $(SHELLS) | pr -5 -t -l22 | p 

remake:
	@-touch *.sh
	@-rm -f $(PROGS) $(SHELLS) $(LINKS)
	@$(MAKE)


.PRECIOUS	= print
