# Master makefile for ./Xmcwp

include $(CWPROOT)/src/Makefile.config

INSTALL:
	cd include ; $(MAKE)
	cd lib     ; $(MAKE)
	cd main    ; $(MAKE)

remake:
	cd include ; $(MAKE) remake
	cd lib     ; $(MAKE) remake
	cd main    ; $(MAKE) remake
