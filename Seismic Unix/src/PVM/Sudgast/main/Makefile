# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config

# pvm RIOS directory on your system
# all executables will go there for this code
B = $(HOME)/pvm/RIOS

J = ../include
M = ../lib

CFLAGS = -I$I -I$J $(OPTC)

D = $L/libcwp.a $L/libpar.a $L/libsu.a

LFLAGS= -L$L -L$M -ldga -lpvm -lsu -lpar -lcwp -lm -lcurses

LFLAGS1= -L$M -lpvm -lsu -lpar -lcwp -lm

LFLAGS2= -L$L -L$M -lsu -lpar -lcwp -lm

PROGS =			\
	$B/process

PROGS1 =		\
	$B/sudgast

PROGS2 =		\
	$B/sustapply	\
	$B/sustxcor	\
	$B/suaddstatics


INSTALL	:	$(PROGS) $(PROGS1) $(PROGS2)
	touch $@


$(PROGS):	$(CTARGET) $D 
	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

$(PROGS1):	$(CTARGET) $D 
	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

$(PROGS2):	$(CTARGET) $D 
	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

remake	:
	-touch *.c 
	-rm -f $(PROGS)
	$(MAKE) 
	
clean:
	rm -f a.out junk* JUNK* core
