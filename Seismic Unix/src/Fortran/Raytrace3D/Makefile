# Makefile for .../RT

include $(CWPROOT)/src/Makefile.config


LFLAGS = $(PRELFLAGS) -L$L -lpar -lcwp -lm $(POSTLFLAGS)

PROGS	=	$B/tableq

LIB = librt.a

ARCH =					\
	$(LIB)(ktime_3d_rayq.o)	\
        $(LIB)(util_drt.o)     

INSTALL:	$(LIB) $(PROGS)
	@touch $@


$(PROGS):	$(FTARGET) $(LIB)
	$(FC) $(FFLAGS) $(@F).f -o $@ $(LIB)
	@$(MCHMODLINE)
	@echo installing $(@F) in $B

$(LIB)	: $(ARCH)
	$(RANLIB) $(LIB)

remake:
	@-rm INSTALL $(PROGS) $(LIB)
	$(MAKE)

clean	:
	rm -f *junk* *.a 
