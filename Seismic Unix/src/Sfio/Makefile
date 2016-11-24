# Master makefile for ./cwp

include $(CWPROOT)/src/Makefile.config

INSTALL:
	cd ./src/lib/vthread; $(MAKE) install
	cd ./src/lib/sfio; $(MAKE) install
	cd ./main; $(MAKE) 


remake:
	$(MAKE) clean 
	$(MAKE)

clean:
	cd ./src/lib/vthread; $(MAKE) clean
	cd ./src/lib/sfio     ; $(MAKE) clean
	cd ./main    ; $(MAKE) clean

