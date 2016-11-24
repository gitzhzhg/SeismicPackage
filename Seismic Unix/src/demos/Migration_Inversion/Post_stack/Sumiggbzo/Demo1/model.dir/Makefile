# Makefile for ...src/prop/local/Cshot

SHELL = /bin/sh

# IMPORTANT: Adapt the flags in the following paragraph to your system
ROOT = /usr/local/cwp
OPTC = -O

B = .
I = $(ROOT)/include
L = $(ROOT)/lib

CFLAGS= $(OPTC) -I$I
LFLAGS= $(OPTC) -s -L$L -lpar -lcwp -lm


PROGS	=		\
		makesyn 

#install
INSTALL:	$(PROGS) 
	@touch $@


$(PROGS):	$$(@F).c
	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS)  -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

# necessary suffix
.c.a:
	@$(CC) -c $(CFLAGS) $<
	@ar rv $@ $*.o
	@rm -f $*.o

remake:
	@-rm -f makevel makesyn
	@touch *.c
	make

clean	:
	rm -f INSTALL makevel makesyn *junk*

