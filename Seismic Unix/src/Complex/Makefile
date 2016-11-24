# Master makefile for ./cwp++
# When porting use "make remake"

include $(CWPROOT)/src/Makefile.config

INSTALL:
	cd include ; $(MAKE)
	cd lib     ; $(MAKE) 
	cd main    ; $(MAKE)
	@touch $@

remake:
	cd include ; $(MAKE) remake
	cd lib     ; $(MAKE) remake
	cd main    ; $(MAKE) remake
	make

clean:
	cd include ; $(MAKE) clean
	cd lib     ; $(MAKE) clean
	cd main     ; $(MAKE) clean
