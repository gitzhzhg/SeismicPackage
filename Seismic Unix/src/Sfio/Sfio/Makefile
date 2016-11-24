# Master makefile for ./cwp

include $(CWPROOT)/src/Makefile.config

INSTALL:
	cd ./src/lib/vthread     ; $(MAKE)
	cd ./src/lib/sfio     ; $(MAKE)
	cd main    ; $(MAKE)

remake:
	$(MAKE) clean 
	$(MAKE)

clean:
	cd ./src/lib/sfio     ; $(MAKE) clean
	cd ./main    ; $(MAKE) clean
	cd ./lib ; $(MAKE) clean
	cd ./include ; $(MAKE) clean

