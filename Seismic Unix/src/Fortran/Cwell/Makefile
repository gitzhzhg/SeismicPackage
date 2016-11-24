# Makefile for ...src/prop/local/Cwell
# Make sure that $(FTARGET) is set in Makefile.config

include $(CWPROOT)/src/Makefile.config

CFLAGS= $(OPTC) -I$I

LFLAGS= -L$L  -lpar -lcwp -lm

#CPROGS	=	$B/cshotplot
PROGS	=	$B/cwell

LIB = libcwell.a
ARCH =					\
		$(LIB)(shoot.o)		\
		$(LIB)(interp.o)	\
		$(LIB)(chkcst.o)	\
		$(LIB)(raydat.o)	\
		$(LIB)(misc.o)		\
		$(LIB)(splines.o)	\
		$(LIB)(bisect.o)	\
		$(LIB)(hwsrc.o) 	\
		$(LIB)(graphics.o)


INSTALL:	$(LIB) $(PROGS) $(CPROGS)
	@touch $@


$(PROGS):	$(FTARGET) $(LIB)
	$(FC) $(FFLAGS) $(@F).f -o $@ $(LIB) $(LFLAGS)
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

#$(CPROGS):     $(CTARGET)
#	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
#	@$(MCHMODLINE)
#	@echo $(@F) installed in $B


$(LIB)	: $(ARCH)
	$(RANLIB) $(LIB)

remake:
	@-rm $(LIB) $(PROGS) $(CPROGS)
	@-$(MAKE)

clean	:
	rm -f *junk* *.a *.o cwell? INSTALL
