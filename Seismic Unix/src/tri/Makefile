# Master makefile for ./cwp

include $(CWPROOT)/src/Makefile.config

INSTALL:
	cd include ; $(MAKE)
	cd lib     ; $(MAKE)
	cd main     ; $(MAKE)
	cd graphics ; $(MAKE)

remake:
	cd include ; $(MAKE) remake
	cd lib     ; $(MAKE) remake
	cd main     ; $(MAKE) remake
	cd graphics ; $(MAKE) remake
