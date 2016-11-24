# Master makefile for ./par

include $(CWPROOT)/src/Makefile.config


INSTALL:
	cd include ; $(MAKE)
	cd lib     ; $(MAKE)
	cd main    ; $(MAKE)
	cd shell   ; $(MAKE)

remake:
	cd include ; $(MAKE) remake
	cd lib     ; $(MAKE) remake
	cd main    ; $(MAKE) remake
	cd shell   ; $(MAKE) remake
