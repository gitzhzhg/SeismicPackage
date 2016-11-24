# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config

D = $L/libcwp.a $L/libpar.a $L/libsu.a

B = .

LFLAGS= -L$L -lsu -lpar -lcwp -lm -L$(PVM_ROOT)/lib/LINUX -lpvm3

PI= -I$(PVM_ROOT)/include 


PROGS =			\
	$B/sumigpreffdmaster \
	$B/sumigpreffdslave

INSTALL :	$(PROGS)
	@-rm -f INSTALL
	@touch $@


$(PROGS):	$(CTARGET) $D 
	-$(CC) $(CFLAGS) $(PI) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

remake	:
	-touch *.c 
	$(MAKE) 
	
clean:
	rm -f a.out junk* JUNK* core
