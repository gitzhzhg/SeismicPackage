# Makefile for ...su/main

include $(CWPROOT)/src/Makefile.config

D = $L/libcwp.a $L/libpar.a $L/libsu.a

B = .

LFLAGS= -L$L -lsu -lpar -lcwp -lm

PROGS =			\
	$B/testtifowler	\


INSTALL	:	$(PROGS)
	touch $@


$(PROGS):	$(CTARGET) $D 
	-$(CC) $(CFLAGS) $(@F).c $(LFLAGS) -o $@
	@$(MCHMODLINE)
	@echo $(@F) installed in $B

remake	:
	-touch *.c 
	-rm -f $(PROGS)
	$(MAKE) 
	
clean:
	rm -f a.out junk* JUNK* core
