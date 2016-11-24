# Makefile for ...cwp/main
# Just say "make"

include $(CWPROOT)/src/Makefile.config

LFLAGS= -L$L -lComplex -lcwp -lm -s
B = .

PROGS =			\
	$B/Complext

INSTALL:	$(PROGS)
	@touch $@

$(PROGS):	$(C++TARGET)
	$(C++) $(C++FLAGS) $(@F).cc $(LFLAGS) -o $@
	$(MCHMODLINE)
	@echo $(@F) installed in $B
remake:
	@-touch *.cc
	@-rm -f $(PROGS)
	@$(MAKE)

clean	:
	-rm -f *.o *.a junk* $(PROGS)
