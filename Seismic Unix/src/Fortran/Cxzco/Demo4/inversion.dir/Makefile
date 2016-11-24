# Makefile for ...src/prop/local/Cxzco/Demo4/inversion.dir

SHELL = /bin/sh

# IMPORTANT: Adapt the flags in the following paragraph to your system
OPT = -O

FFLAGS = $(OPT)

PROGS	=	getcosines


INSTALL:	$(PROGS)
	@touch $@


$(PROGS):	$$(@F).f
	$(FC) $(FFLAGS) $(@F).f -o $@
