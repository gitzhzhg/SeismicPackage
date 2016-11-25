# Makefile in the $(CPSEIS_HOME) toplevel directory.
# $(CPSEIS_HOME) currently is the same as $(CPSEIS_INSTALL_DIR).

# This makefile exists primarily for the convenience of those
# who wish to make the CPSeis modules and their wrappers for
# use in other applications (calling the modules from Fortran
# or C++ or java), including GeoPRO for example.

# Just type "make" in the $(CPSEIS_HOME) toplevel directory.
# This will make libcps and the required wrapper libraries.
# This will not make cfe or the cps programs or the spws programs. 
# Although GeoPRO uses only archive libraries (.a), this makefile
# also makes the corresponding shared libraries (.so) simply to
# allow some test programs for the wrappers to be run.

# The "all" target makes everything needed by GeoPRO.
# The "everything" target makes everything made by the install_cpseis script.
# The "again" target remakes everything except directories, dependencies, install, and wrappers.

all:
	cd platforms/$(CPSEIS_ARCH) && $(MAKE) libcps
	cd platforms/$(CPSEIS_ARCH) && $(MAKE) wrappers
	cd wrappers/src/org/cpseis/examples && $(MAKE) autorun

everything:
	cd platforms/$(CPSEIS_ARCH) && $(MAKE) depend all cfe install

again:
	cd platforms/$(CPSEIS_ARCH) && $(MAKE) objs static shared progs xmls cfe
	cd wrappers/src/org/cpseis/examples && $(MAKE)

clean:
	rm -f platforms/$(CPSEIS_ARCH)/lib/libcps.*
	cd platforms/$(CPSEIS_ARCH) && $(MAKE) clean
	cd platforms/$(CPSEIS_ARCH) && $(MAKE) clean_wrappers

libclean:
	rm -f platforms/$(CPSEIS_ARCH)/lib/libcps.*
	cd wrappers && $(MAKE) libclean

ldd:
	cd platforms/$(CPSEIS_ARCH) && $(MAKE) ldd
	cd wrappers && $(MAKE) ldd
