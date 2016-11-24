# Makefile for ...cwp/main
# Just say "make"

B = .

# Uncomment the following line for 64 bit machines
CFLAGS = -m32

LFLAGS = $(PRELFLAGS) -Bstatic   $(POSTLFLAGS)

PROGS =			\
	$B/seg2segy	\
	$B/seg2segy_local \
#	$B/seg2fix


INSTALL:	$(PROGS)
	@-rm -f INSTALL
	@touch $@

$(PROGS):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	$(MCHMODLINE)
	@echo $(@F) installed in $B
remake:
	@-rm -f $(PROGS) INSTALL
	@$(MAKE) INSTALL

clean	::
	rm -f *.o *.a junk* $(PROGS)
