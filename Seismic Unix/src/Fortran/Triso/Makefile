# Makefile for .../Triso

include $(CWPROOT)/src/Makefile.config


LFLAGS = $(PRELFLAGS) -L$L -lpar -lcwp -lm $(POSTLFLAGS)

PROGS	=	$B/triso
CPROGS	=	$B/triview $B/trisoplot

LIB = libtriso.a

ARCH =					\
 	$(LIB)(trisubs.o)		\
	$(LIB)(gplib.o)			\
 	$(LIB)(graphsubs.o)		\
 	$(LIB)(getparms.o)

INSTALL:	$(LIB) $(PROGS) $(CPROGS)
	@touch $@


$(PROGS):	$(FTARGET) $(LIB)
	$(FC) $(FFLAGS) $(@F).f -o $@ $(LIB)
	@$(MCHMODLINE)
	@echo installing $(@F) in $B

$(CPROGS):	$(CTARGET)
	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo installing $(@F) in $B

$(LIB)	: $(ARCH)
	$(RANLIB) $(LIB)

remake:
	@-rm INSTALL $(PROGS) $(CPROGS) $(LIB)
	$(MAKE)

clean	:
	rm -f *junk* *.a *.o triso triview trisoplot INSTALL
