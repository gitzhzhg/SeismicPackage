# Master makefile for ./cwp

include $(CWPROOT)/src/Makefile.config
include  ./Makefile.bayes

INSTALL:
	cd include ; $(MAKE)
	cd libPosteriori     ; $(MAKE)
	cd libStratInv       ; $(MAKE)
	cd libReflectivity   ; $(MAKE)
	cd libSampling       ; $(MAKE)
	cd main    ; $(MAKE)

remake:
	cd include ; $(MAKE) remake
	cd libPosteriori     ; $(MAKE) remake
	cd libStratInv       ; $(MAKE) remake
	cd libReflectivity   ; $(MAKE) remake
	cd libSampling       ; $(MAKE) remake
	cd main    ; $(MAKE) remake
