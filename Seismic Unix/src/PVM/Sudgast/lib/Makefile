# Makefile for ...cwp/lib

include $(CWPROOT)/src/Makefile.config

J = ../include

CC = xlc
OPTC = -O

CFLAGS = -I$I -I$J $(OPTC)

D = $J/communication.h $J/define.h $J/extern.h	\
	$J/format.h $J/global.h $J/pvm.h

LIB = libdga.a

ARCH =			\
	$(LIB)(ngen_best.o)	\
	$(LIB)(ngen_converge.o)	\
	$(LIB)(ngen_convert.o)	\
	$(LIB)(ngen_cross.o)	\
	$(LIB)(ngen_display.o)	\
	$(LIB)(ngen_done.o)	\
	$(LIB)(ngen_elitist.o)	\
	$(LIB)(ngen_error.o)	\
	$(LIB)(ngen_evaluate.o)	\
	$(LIB)(ngen_exchange.o)	\
	$(LIB)(ngen_generate.o)	\
	$(LIB)(ngen_init.o)	\
	$(LIB)(ngen_input.o)	\
	$(LIB)(ngen_main.o)	\
	$(LIB)(ngen_measure.o)	\
	$(LIB)(ngen_monitor.o)	\
	$(LIB)(ngen_mutate.o)	\
	$(LIB)(ngen_random.o)	\
	$(LIB)(ngen_select.o)	\
	$(LIB)(ngen_smooth.o)	\
	$(LIB)(ngen_wsta.o)

INSTALL:	$(LIB) $L
	@touch $@

$(ARCH)	: $D

$(LIB)	:	$(ARCH)
	$(RANLIB) $(LIB)

$L	:
	-mkdir

remake	:
	@-rm -f $(LIB)
	@$(MAKE)

list	:
	$(AR) tv $(LIB)

clean:
	rm -f junk* core a.out

.PRECIOUS:	 $(LIB)
