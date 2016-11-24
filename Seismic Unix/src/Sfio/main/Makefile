# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config

OPTC = -g

D = $L/libcwp.a $L/libpar.a $L/libsu.a

# put /usr/include explicitly before sfio/include to avoid picking
# up unwanted stdio emulation header.
CFLAGS = $(OPTC) -I$I -I/usr/include -I../include $(XDRFLAG) $(ENDIANFLAG)

LFLAGS= $(PRELFLAGS) -L../lib -L$L -lsfio -lsu -lpar -lcwp -lm $(POSTLFLAGS)


PROGS =				\
	$B/segbread		\
	$B/segdread


INSTALL:	$(PROGS)
	@-rm -f INSTALL
	@touch $@


$(PROGS):	$(CTARGET) $D
	-$(CC) $(CFLAGS) `pwd`/$(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

remake:
	-rm -f $(PROGS) INSTALL
	$(MAKE)

clean::
	-rm -f INSTALL a.out junk* JUNK* core
