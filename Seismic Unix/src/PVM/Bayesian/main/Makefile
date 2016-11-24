# Makefile for ...cwp/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config
include ../Makefile.bayes

CFLAGS = $(BAYESCFLAGS)

BPVM=$(PVM_ROOT)/bin/$(PVM_ARCH)

LFLAGS = $(PRELFLAGS) -L$L -lsu -lpar -lcwp -lm $(POSTLFLAGS)
LFLAGS1 = $(PRELFLAGS) -L$L -lposteriori -lpar -lcwp -lm $(POSTLFLAGS)
LFLAGS2 = $(PRELFLAGS) -L$L -L$(LPVM) -lstratinv -lpvm3 -lsu -lpar -lcwp -lm $(POSTLFLAGS)
LFLAGS3 = $(PRELFLAGS) -L$L -L$(LPVM) -lstratinv -lpvm3 -lpar -lcwp -lm $(POSTLFLAGS)
LFLAGS4 = $(PRELFLAGS) -L$L -L$(LPVM) -lreflectivity -lpvm3 -lsu -lpar -lcwp -lm $(POSTLFLAGS)
LFLAGS5 = $(PRELFLAGS) -L$L -lrandom -lpar -lcwp -lm $(POSTLFLAGS)



PROGS =			\
	$B/logBlock	\
	$B/covAdap	\
	$B/dataCovar

PROGS1 =		\
	$B/posteriori

PROGS2 =		\
	$B/stratInv

PROGS3 =				\
	$(BPVM)/modSlave		\
	$(BPVM)/frechetSlave

PROGS4 =			\
	$(BPVM)/sudrefsvmaster	\
	$(BPVM)/sudrefsvslave

PROGS5 =			\
	$B/gaussSampling

INSTALL:        $(PROGS) $(PROGS1) $(PROGS2) $(PROGS3) $(PROGS4) $(PROGS5)
	@touch $@

$(PROGS):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	chmod 755 $@
	@echo $(@F) installed in $B

$(PROGS1):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS1) -o $@
	chmod 755 $@
	@echo $(@F) installed in $B

$(PROGS2):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS2) -o $@
	chmod 755 $@
	@echo $(@F) installed in $B

$(PROGS3):	$(PVMBTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS3) -o $@
	chmod 755 $@
	@echo $(@F) installed in $(BPVM)

$(PROGS4):	$(PVMBTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS4) -o $@
	chmod 755 $@
	@echo $(@F) installed in $(BPVM)

$(PROGS5):	$(CTARGET)
	$(CC) $(CFLAGS) $(@F).c $(LFLAGS5) -o $@
	chmod 755 $@
	@echo $(@F) installed in $B

remake:
	@-rm -f $(PROGS)
	@-rm -f $(PROGS1)
	@-rm -f $(PROGS2)
	@-rm -f $(PROGS3)
	@-rm -f $(PROGS4)
	@-rm -f $(PROGS5)
	@$(MAKE)

clean	:
	rm *.o *.a junk*
