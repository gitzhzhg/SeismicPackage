# Makefile for ...src/prop/local/Cshot

include $(CWPROOT)/src/Makefile.config

LFLAGS = $(PRELFLAGS) -L$L -lpar -lcwp -lm  $(POSTLFLAGS)

PROGS	=	$B/cshot1 $B/cshot2
CPROGS	=	$B/cshotplot

LIB = libcshot.a

ARCH =					\
		$(LIB)(contin.o)	\
		$(LIB)(shoot.o)		\
		$(LIB)(raydat.o)	\
		$(LIB)(hdwave.o)	\
		$(LIB)(misc.o)		\
		$(LIB)(splines.o)	\
		$(LIB)(fft.o)		\
		$(LIB)(addwav.o)	\
		$(LIB)(wavlet.o)	\
		$(LIB)(graphics.o)


#if you don't have cwp and par  installed then switch the INSTALL lines below
#INSTALL:	$(LIB) $(PROGS) 
INSTALL:	$(LIB) $(PROGS) $(CPROGS)
	@touch $@


$(PROGS):	$(FTARGET) $(LIB)
	$(FC) $(FFLAGS) $(@F).f -o $@ $(LIB)
	@$(MCHMODLINE)
	@echo installing $(@F) in $B


$(CPROGS):	$(CTARGET)
	$(CC) $(CFLAGS)  $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo installing $(@F) in $B


$(LIB)	: $(ARCH)
	$(RANLIB) $(LIB)


remake:
	@-rm libcshot.a INSTALL $(PROGS) $(CPROGS)
	$(MAKE)
clean	:
	rm -f *junk* *.a *.o cshot? *.shot cshotplot
