#----- Make cpseis for all platforms
#----- This does not make jobmon (cluster job monitoring tool)
#----- Bill Menger bill.menger@gmail.com

CPSEIS_HOME = $(CPSEIS_INSTALL_DIR)
PLATFORM = $(CPSEIS_ARCH)
SRCDIR = $(CPSEIS_HOME)/src
INCDIR = $(CPSEIS_HOME)/include
INCROOT = $(CPSEIS_HOME)/spws_home/include
#MOREINC = -I$(INCROOT) -I$(INCROOT)/HDF -I$(INCROOT)/Xbae -I$(INCROOT)/Xint -I$(INCROOT)/color -I$(INCROOT)/cube -I$(INCROOT)/cubeio -I$(INCROOT)/dp -I$(INCROOT)/fg2d -I$(INCROOT)/fggui -I$(INCROOT)/fgmap -I$(INCROOT)/fgqc -I$(INCROOT)/fgxp -I$(INCROOT)/geom -I$(INCROOT)/hardcopy -I$(INCROOT)/interp -I$(INCROOT)/ipc -I$(INCROOT)/net -I$(INCROOT)/oprim -I$(INCROOT)/pick -I$(INCROOT)/plot -I$(INCROOT)/sl -I$(INCROOT)/sp -I$(INCROOT)/stat -I$(INCROOT)/statgui -I$(INCROOT)/table -I$(INCROOT)/v3d -I$(INCROOT)/vaplots -I$(INCROOT)/vect -I$(INCROOT)/vf -I$(INCROOT)/vfgui -I$(INCROOT)/vu -I$(INCROOT)/wbox
MOREINC = -I$(INCROOT)
LIBDIR = $(CPSEIS_HOME)/platforms/$(PLATFORM)/lib
BINDIR = $(CPSEIS_HOME)/platforms/$(PLATFORM)/bin
MODDIR = $(CPSEIS_HOME)/platforms/$(PLATFORM)
OBJDIR = $(CPSEIS_HOME)/platforms/$(PLATFORM)
CFEDIR = $(CPSEIS_HOME)/opencfe/src
XMLDIR = $(CPSEIS_HOME)/platforms/$(PLATFORM)/xml
THIRDPARTY = $(THIRDPARTY_INSTALL_DIR)

ICPS =					$(BINDIR)/icps                # "interactive" cpseis (non parallel version)
EZCPS =					$(BINDIR)/ezcps               # Tool to build a template for adding a new module (tool) to cpseis
EZGUI =					$(BINDIR)/ezgui               # Tool for laying out the gui for your new module.
EZSUB =					$(BINDIR)/ezsub               # I can't remember what this is for, but I'll get back...
SPS_BUILD_SUPERPROC =			$(BINDIR)/sps_build_superproc # This builds wrapper code
CMDTIM =               			$(BINDIR)/command_timeout     # wraps a shell command with a signal timer to kill if it takes too long.
BLDSUP =				$(BINDIR)/bldsup			  # builds the super.f90 program for CFE building.
CIU =                                   $(BINDIR)/ciu                 # Cfe Interactive Utilities program.  Not sure what this is for, but will build it.
CFEBLD =				$(BINDIR)/cfebld              # Builds stand-alone jobs from work files.
CFESER =                		$(BINDIR)/cfeseries           # Creates a job series by using one workfile template and generating many.
CFESUB =                		$(BINDIR)/cfesub              # submits jobs from cfe front end gui.
CHECKC =				$(BINDIR)/checkc              # Looks over your code to see if it meets the old conoco standards.
CFE =					$(BINDIR)/cfe                 # This is the front end system (not built yet)
CKCOMLOGS =				$(BINDIR)/ckcomlogs           # extracts information from the log files.
COLOR2CGM =				$(BINDIR)/color2cgm           # for the splt and color tools, this creates cgm output for plotter hardcopy
CPSACCTPRG =				$(BINDIR)/cpsacct             # Looks at the accounting logs to do run statistics extraction.
FILERESERVE =				$(BINDIR)/filereserve         # reserves file space for an output by prewriting blocks of zeroes.
FILERESERVENA =				$(BINDIR)/filereservena       # Does the same on network appliance systems (requires root access to the NA)
FX2FR =					$(BINDIR)/fx2fr               # Can't remember what this does.
FSIZE =					$(BINDIR)/fsize               # Attempts to tell you how large your file will be based on number of traces.
JUNK2BLANK =				$(BINDIR)/junk2blank          # Strips out nonprintable characters from a file.
PERMSAVE =				$(BINDIR)/permsave            # Helps you create a file list for copy to tape for archiving.
PFTOOLS =				$(BINDIR)/pftools             # creates prm, pcp, psz... which allow you to do file operations on the
								      # multiple-extent files from cpseis.  If you just to an "rm" on one of these
	                                                              # then you will only remove the link that points to the first file extent and
	                                                              # you will leave lots of data orphaned on your file system.  Read the documentation
	                                                              # in pfio, cio, bfio, pftools.
PICKMERGE =				$(BINDIR)/pickmerge           # Merges pick files but I don't know what to do with this. 
REFRESHLOCK =				$(BINDIR)/refreshlock         # If you choose to start up lock files for your data, this is a daemon that
	                                                              # needs to be running on your lock file server.  It attempts to restart lock
	                                                              # servers that are hung.  Doesn't always work.  I LOVE nfs. :^(
RSH_TIMEOUT =				$(BINDIR)/rsh_timeout         # Wraps an rsh command in a timer that kills it after a wait period.
SD2FG =					$(BINDIR)/sd2fg               # This is for geometry set up for the fg module (field geometry).
SEGYFIX =				$(BINDIR)/segyfix             # Allows you to modify segy files (don't know how useful it is.)
SEGYVIEW =				$(BINDIR)/segyview            # REALLY cheezy non-graphical segy file viewer.  It will REMAP the segy headers
	                               	                              # into cpseis headers so it won't always tell you what you need, but it does
	                                                              # a tab plot shamelessly borrowed from cwp's su system.
CBYT =                                  $(BINDIR)/cbyt                # trace viewer and picker
VA   =                                  $(BINDIR)/va                  # velocity analysis
CFG =                                   $(BINDIR)/cfg                 # field geometry
CSV =                                   $(BINDIR)/csv                 # cube slice viewer
XMLDEP =                                $(BINDIR)/sps_xml_dependencies # create xml file dependencies
FORDEP =                                $(BINDIR)/sps_fortran_dependencies # create fortran file dependencies

STATIC_EXT = .a
SHARED_EXT = .so

SH_FFTW_LDFLAGS =  -L$(FFTW_DIR)/lib -lfftw -lrfftw
FFTW_LDFLAGS =  $(FFTW_DIR)/lib/libfftw.a $(FFTW_DIR)/lib/librfftw.a

ifeq "$(MPI)" "lam"
	NO_MPI=
	MPI_DIR=$(LAM_DIR)
	vpath mpif.h $(MPI_DIR)/include

	SH_MPI_LDFLAGS = -L$(LAM_DIR)/lib -lmpi -llam -llamf77mpi
	MPI_LDFLAGS = $(LAM_DIR)/lib/libmpi.a $(LAM_DIR)/lib/liblamf77mpi.a $(LAM_DIR)/lib/liblam.a $(LAM_DIR)/lib/libmpi.a $(LAM_DIR)/lib/liblam.a

	ifeq "$(CPSEIS_COMPILER_BASE)" "intel" 
		ifeq "$(HOST)" "achilles"
		  CC=gcc
		else 
		  ifeq "$(HOST)" "basalt"
		    CC=gcc
		  else
                    ifeq "$(HOST)" "fushou089"
		      CC=gcc
		    else
		      CC=icc
		    endif
		  endif
		endif
		F90=ifort
 		STATIC_LINKER = ifort -nofor-main -i-static
		SHARED_LINKER = ifort -shared 
	else
		CC=gcc
		F90=gfortran
		STATIC_LINKER = gfortran
		SHARED_LINKER = gfortran -shared
	endif
endif
ifeq "$(MPI)" "mpich2"
	NO_MPI=
	MPI_DIR=$(MPICH_DIR)
	vpath mpif.h $(MPI_DIR)/include
	SH_MPI_LDFLAGS =   -L$(MPICH_DIR)/lib -lmpich -lfmpich
	MPI_LDFLAGS =   $(MPICH_DIR)/lib/libmpich.a $(MPICH_DIR)/lib/libfmpich.a
	#CLIBS  = -L$(MPICH_DIR)/lib/shared -lmpich  -L$(MPICH_DIR)/lib -lfmpich # never used
	F90 = mpif90
	CC  = mpicc
	ifeq "$(CPSEIS_COMPILER_BASE)" "intel"
		STATIC_LINKER = mpif90 -nofor-main
	else
		STATIC_LINKER = mpif90
	endif
	SHARED_LINKER = mpif90 -shared
endif
ifeq "$(MPI)" "none"
	NO_MPI=$(MPI_STUBS)
	MPI_LDFLAGS=
	MPI_DIR=$(INCDIR)/mpi_stubs
	
	vpath mpif.h $(INCDIR)/mpi_stubs/include
	ifeq "$(CPSEIS_COMPILER_BASE)" "intel" 
		CC=icc
		F90=ifort
		STATIC_LINKER = ifort -nofor-main
		SHARED_LINKER = ifort -shared
	else
		CC=gcc
		F90=gfortran
		STATIC_LINKER = gfortran
		SHARED_LINKER = gfortran -shared
	endif
endif

F90_LINK_DIR=
F90_LINK=
ifeq "$(CPSEIS_COMPILER_BASE)" "intel"
	FFLAGS_PROG = -g -fPIC -I$(MPI_DIR)/include
	FFLAGS = -g -O2 -fPIC -heap-arrays 10000 -unroll -mcmodel=medium -parallel -threads -I$(MPI_DIR)/include
	#FFLAGS = -g -fPIC -heap-arrays 10000 -mcmodel=medium -I$(MPI_DIR)/include
	#CFLAGS = -g -O2 -fPIC -parallel -unroll -mcmodel=medium -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) $(MOREINC) -DLINUXGCC
	ifeq "$(HOST)" "achilles"
	  CFLAGS = -g -O2 -fPIC -unroll -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) -DLINUXGCC
	else
          ifeq "$(HOST)" "basalt"
	    CFLAGS = -g -O2 -fPIC -unroll -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) -DLINUXGCC
	  else
            ifeq "$(HOST)" "fushou089"
	      CFLAGS = -g -O2 -fPIC -unroll -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) -DLINUXGCC
	    else
	      CFLAGS = -g -O2 -fPIC -parallel -unroll -mcmodel=medium -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) -DLINUXGCC
	    endif
	  endif
	endif
        F90_LINK_DIR = $(CPSEIS_INSTALL_DIR)/intel_x86_64
	ifeq "$(MACHINE_ARCH)" "i686"
	        FFLAGS = -g -fPIC -I$(MPI_DIR)/include
	        #FFLAGS = -g -fPIC -heap-arrays 10000 -I$(MPI_DIR)/include
	        #CFLAGS = -g -fPIC -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) $(MOREINC) -DLINUXGCC
	        CFLAGS = -g -fPIC -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) -DLINUXGCC
                F90_LINK_DIR = $(CPSEIS_INSTALL_DIR)/intel_i686
	endif
        SH_F90_LINK=-L$(F90_LINK_DIR) -lguide -limf -lsvml
        F90_LINK=$(F90_LINK_DIR)/libguide.a $(F90_LINK_DIR)/libimf.a $(F90_LINK_DIR)/libsvml.a 
endif
ifeq "$(CPSEIS_COMPILER_BASE)" "gfortran"
	FFLAGS_PROG = -v -g -O2 -fPIC -fno-backslash -fno-range-check -ffree-form -ffree-line-length-none -I$(MPI_DIR)/include
	#FFLAGS = -g -fPIC -O2 -unroll -mcmodel=medium -fno-range-check -ffree-form -ffree-line-length-none -I$(MPI_DIR)/include
	FFLAGS = -g -fPIC -fno-backslash -fno-range-check -ffree-form -ffree-line-length-none -I$(MPI_DIR)/include
	#CFLAGS = -g -fPIC  -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) $(MOREINC) -DLINUXGCC
	CFLAGS = -v -g -fPIC -O2 -DLINUXI -DNEED_UNDERSCORE -D_REENTRANT -D_FILE_OFFSET_BITS=64 -I$(INCDIR) -DLINUXGCC
endif

#Linker flags
LDFLAGS = $(FFTW_LDFLAGS) $(MPI_LDFLAGS) $(F90_LINK) -lutil -lm -lpthread 
SH_LDFLAGS = $(SH_FFTW_LDFLAGS) $(SH_MPI_LDFLAGS)

STATIC_LIBCPS = $(LIBDIR)/libcps$(STATIC_EXT)
SHARED_LIBCPS = $(LIBDIR)/libcps$(SHARED_EXT)

STATIC_LIBCGM = $(LIBDIR)/libcgm$(STATIC_EXT)
SHARED_LIBCGM = $(LIBDIR)/libcgm$(SHARED_EXT)

STATIC_LIBSPWS = $(LIBDIR)/libspws$(STATIC_EXT)
SHARED_LIBSPWS = $(LIBDIR)/libspws$(SHARED_EXT)

INSTALL_DIR = $(BINDIR)

CCOBJS = 

.PHONY: all clean dirs static shared objs progs xmls \
        depend fordep xmldep util oop install \
        clean_all clean_wrappers clean_spws clean_static clean_shared \
        shared_libcps spws spws_depend ldd libcps wrappers list

# Note: By adding the line:     vpath % $(BINDIR)
# and by removing the program targets from .PHONY, the
# programs will be made only when they are missing or
# out of date.  Previously, they were always made.

all :	dirs objs static shared progs xmls wrappers

progs : objs icps cfebld ezcps ezgui command_timeout cfeseries \
	cfesub ckcomlogs cpsacct filereserve filereservena \
	color2cgm fx2fr fsize pftools pickmerge refreshlock \
	rsh_timeout sd2fg segyfix segyview ezsub junk2blank \
	permsave checkc sps_build_superproc depend cbyt va cfg csv \
	xmldep fordep msepita bldsup

# ciu deliberately omitted from progs because cannot find a main program in ciu files.

libcps: dirs $(STATIC_LIBCPS) $(SHARED_LIBCPS)

dirs:
	@echo ""
	@echo "making dirs"
	$(CPSEIS_HOME)/scripts/create_directories $(PLATFORM)

xmls:	$(XOBJS)

install:
	@echo ""
	@echo "making install"
	ln -s $(CPSEIS_HOME)/scripts/* $(BINDIR)

# The following "depend" target makes the dependencies for SPWS.
#
# The dependencies for CPS are in the file makefile_cpseis.depend which is
# not automatically made, and which is therefore in the svn repository.
# This dependency file must be manually edited when necessary.
#
# The dependencies for the WRAPPERS are made by the "wrappers" target.

depend : xmldep fordep spws_depend

clean : 
	@echo ""
	@echo "making clean"
	rm -f *.o
	rm -f *.mod
	rm -f *.d
	rm -f $(OBJDIR)/*.o
	rm -f $(MODDIR)/*.mod 

clean_all: clean_spws clean_wrappers clean
	@echo ""
	@echo "making clean_all"
	rm -f $(BINDIR)/* $(LIBDIR)/*

clean_wrappers:
	@echo ""
	@echo "making clean_wrappers"
	cd $(CPSEIS_WRAPPERS) && make allclean

clean_spws :
	@echo ""
	@echo "making clean_spws"
	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make clean

clean_static :
	@echo ""
	@echo "making clean_static"
	rm -f $(STATIC_LIBCPS)

clean_shared :
	@echo ""
	@echo "making clean_shared"
	rm -f $(SHARED_LIBCPS)

static : $(STATIC_LIBCPS) $(STATIC_LIBCGM)

shared : $(SHARED_LIBCPS) $(SHARED_LIBCGM)

shared_libcps : $(SHARED_LIBCPS)

vpath %.c $(SRCDIR) #$(CPSEIS_INSTALL_DIR)/spws_home/programs/cbyt
vpath %.cc $(SRCDIR) #$(CPSEIS_INSTALL_DIR)/spws_home/programs/cbyt
#vpath %.hh $(INCDIR) 
vpath %.h  $(INCDIR) #$(CPSEIS_INSTALL_DIR)/spws_home
vpath %.f90 $(SRCDIR) #$(CPSEIS_INSTALL_DIR)/spws_home
vpath %.f   $(SRCDIR) #$(CPSEIS_INSTALL_DIR)/spws_home
vpath %.o $(OBJDIR)
vpath %.mod $(MODDIR)
vpath % $(BINDIR) # allows program targets to be found in the bin directory.

include $(CPSEIS_HOME)/etc/makefile_cpseis.depend

OBJS = $(COBJS) $(FOBJS) $(FPROBJS) $(CPROBJS) $(CGMOBJS) $(MPIOBJS)

objs : $(OBJS)

cfe :	$(OBJS) $(SHARED_LIBCPS)
	@echo ""
	@echo "making cfe"
	cd $(CFEDIR) && make clean all install

wrappers :
	@echo ""
	@echo "making wrappers"
	cd $(CPSEIS_WRAPPERS) && make build

spws : 
	@echo ""
	@echo "making spws"
	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make util oop

spws_depend : 
	@echo ""
	@echo "making spws_depend"
	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make depend

icps : icps.o $(STATIC_LIBCPS) $(FPROBJS) $(NO_MPI)
	@echo ""
	@echo "making icps"
	$(STATIC_LINKER) $(FFLAGS) -o $(ICPS) icps.o engine.o $(F90_LINK) $(NO_MPI) $(STATIC_LIBCPS) $(LDFLAGS)

ezcps : ezcps.o $(STATIC_LIBCPS) $(FPROBJS) $(CPROBJS)
	@echo ""
	@echo "making ezcps"
	$(STATIC_LINKER) $(FFLAGS) -o $(EZCPS) ezcps.o $(STATIC_LIBCPS) $(LDFLAGS)

bldsup : bldsup.o $(STATIC_LIBCPS) $(FPROBJS) $(CPROBJS)
	@echo ""
	@echo "making bldsup"
	$(STATIC_LINKER) $(FFLAGS) -o $(BLDSUP) bldsup.o build_super.o \
	$(STATIC_LIBCPS) $(LDFLAGS)

checkc : checkc.o $(STATIC_LIBCPS)
	@echo ""
	@echo "making checkc"
	$(STATIC_LINKER) $(FFLAGS) -o $(CHECKC) checkc.o $(STATIC_LIBCPS) $(LDFLAGS)

ezgui : ezgui.o $(STATIC_LIBCPS) $(FPROBJS) $(CPROBJS)
	@echo ""
	@echo "making ezgui"
	$(STATIC_LINKER) $(FFLAGS) -o $(EZGUI) ezgui.o ezgui_frou.o $(STATIC_LIBCPS) -lm -lpthread
	#$(STATIC_LINKER) $(FFLAGS) -o $(EZGUI) ezgui.o ezgui_frou.o $(STATIC_LIBCPS) $(LDFLAGS)

ezsub : ezsub.o $(STATIC_LIBCPS)
	@echo ""
	@echo "making ezsub"
	$(STATIC_LINKER) $(FFLAGS) -o $(EZSUB) ezsub.o $(STATIC_LIBCPS) $(LDFLAGS)

permsave : permsave.o $(STATIC_LIBCPS)
	@echo ""
	@echo "making permsave"
	$(STATIC_LINKER) $(FFLAGS) -o $(PERMSAVE) permsave.o $(STATIC_LIBCPS) $(LDFLAGS)

junk2blank : junk2blank.o $(STATIC_LIBCPS)
	@echo ""
	@echo "making junk2blank"
	$(STATIC_LINKER) $(FFLAGS) -o $(JUNK2BLANK) junk2blank.o $(STATIC_LIBCPS) $(LDFLAGS)

command_timeout : command_timeout.o
	@echo ""
	@echo "making command_timeout"
	$(STATIC_LINKER) $(FFLAGS) -o $(CMDTIM) command_timeout.o $(LDFLAGS)

sps_build_superproc : sps_build_superproc.o $(FPROBJS) $(STATIC_LIBCPS)
	@echo ""
	@echo "making sps_build_superproc"
	$(STATIC_LINKER) $(FFLAGS) -o $(SPS_BUILD_SUPERPROC) sps_build_superproc.o \
	alphasort.o getsys.o putsys.o string.o named_constants.o \
	sizeof.o string_crou.o  sizeof_crou.o cgetsys.o str.o \
	cputsys.o cnfg.o cnfg_crou.o unix.o unix_crou.o $(LDFLAGS)

cfebld : cfebldbeta.c buildjob.f90 cfebld_frou.f90 $(CPROBJS) $(STATIC_LIBCPS)
	@echo ""
	@echo "making cfebld"
	$(STATIC_LINKER) $(FFLAGS) -o $(CFEBLD) cfebldbeta.o buildjob.o cfebld_frou.o $(STATIC_LIBCPS) $(LDFLAGS)

ciu : ciu.o ciu_crou.o prog.o window.o ciu_window.o windowlist.o master.o rptstats.o rptstats_crou.o ciu_gui.o $(STATIC_LIBCPS)
	@echo ""
	@echo "making ciu"
	$(STATIC_LINKER) $(FFLAGS) -o $(CIU) ciu.o prog.o windowlist.o master.o rptstats.o \
	rptstats_crou.o ciu_gui.o ciu_window.o window.o ciu_crou.o $(STATIC_LIBCPS) $(LDFLAGS)

cfeseries : cfeseries.o $(STATIC_LIBCPS)
	@echo ""
	@echo "making cfeseries"
	$(STATIC_LINKER) $(FFLAGS) -o $(CFESER) cfeseries.o cfeseries_frou.o $(STATIC_LIBCPS) $(LDFLAGS)

cfesub : cfesub.o $(STATIC_LIBCPS) $(FPROBJS) $(CPROBJS)
	@echo ""
	@echo "making cfesub"
	$(STATIC_LINKER) $(FFLAGS) -o $(CFESUB) cfesub.o cfesub_frou.o $(STATIC_LIBCPS) $(LDFLAGS)

ckcomlogs : ckcomlogs.o $(STATIC_LIBCPS)
	@echo ""
	@echo "making ckcomlogs"
	$(STATIC_LINKER) $(FFLAGS) -o $(CKCOMLOGS) ckcomlogs.o $(STATIC_LIBCPS) $(LDFLAGS)

cpsacct : $(CPSACCTPRG)
$(CPSACCTPRG) : cpsacctprg.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(CPSACCTPRG) cpsacctprg.o cnfg.o $(STATIC_LIBCPS) $(LDFLAGS)

filereserve : $(FILERESERVE)
$(FILERESERVE) : filereserve.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(FILERESERVE) filereserve.o pfio.o str.o $(STATIC_LIBCPS) $(LDFLAGS)

filereservena : $(FILERESERVENA)
$(FILERESERVENA) : filereservenetapps.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(FILERESERVENA) filereservenetapps.o pfio.o cnfg.o \
	$(STATIC_LIBCPS) $(LDFLAGS)

color2cgm : $(COLOR2CGM)
$(COLOR2CGM) : color2cgm.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(COLOR2CGM) color2cgm.o $(STATIC_LIBCPS) $(STATIC_LIBCGM) $(LDFLAGS)

fx2fr : fx2fr.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(FX2FR) fx2fr.o $(STATIC_LIBCPS) $(LDFLAGS)

fsize : fsize.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(FSIZE) fsize.o $(STATIC_LIBCPS) $(LDFLAGS)

pftools : pftools.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(PFTOOLS) pftools.o $(STATIC_LIBCPS) $(LDFLAGS)
	ln -f $(PFTOOLS) $(BINDIR)/psz
	ln -f $(PFTOOLS) $(BINDIR)/pcp
	ln -f $(PFTOOLS) $(BINDIR)/pchmod
	ln -f $(PFTOOLS) $(BINDIR)/pmv
	ln -f $(PFTOOLS) $(BINDIR)/prcp
	ln -f $(PFTOOLS) $(BINDIR)/prm
	ln -f $(PFTOOLS) $(BINDIR)/ulf
	ln -f $(PFTOOLS) $(BINDIR)/lfd

pickmerge : pickmerge.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(PICKMERGE) pickmerge.o $(STATIC_LIBCPS) $(LDFLAGS)

refreshlock : refreshlock.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(REFRESHLOCK) refreshlock.o $(STATIC_LIBCPS) $(LDFLAGS)

rsh_timeout : rsh_timeout.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(RSH_TIMEOUT) rsh_timeout.o $(STATIC_LIBCPS) $(LDFLAGS)

sd2fg : sd2fg.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(SD2FG) sd2fg.o $(STATIC_LIBCPS) $(LDFLAGS)

segyfix : segyfix.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(SEGYFIX) segyfix.o modgrid.o modgrid_frou.o \
	$(STATIC_LIBCPS) $(LDFLAGS)

segyview : segyview.o $(STATIC_LIBCPS)
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(SEGYVIEW) segyview.o cio.o named_constants.o \
	put.o segy.o \
	swap.o trcio.o wrdc.o put_crou.o segy_crou.o \
	$(STATIC_LIBCPS) $(LDFLAGS)

#   programs : 
#   	@echo ""
#   	@echo making $(notdir $@)
#   	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make util oop
#   	$(CPSEIS_INSTALL_DIR)/scripts/fixhelp.sh
#   	make cbyt
#   	make va
#   	make csv
#   	make cfg
#   	make msepita
#   	make slave_dpy
#   

cbyt : spws
	@echo ""
	@echo making $(notdir $@)
	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make cbyt 
	cp $(CPSEIS_INSTALL_DIR)/spws_home/programs/$@/$(CPSEIS_ARCH)/$@ $(BINDIR)

va : spws
	@echo ""
	@echo making $(notdir $@)
	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make va
	cp $(CPSEIS_INSTALL_DIR)/spws_home/programs/$@/$(CPSEIS_ARCH)/$@ $(BINDIR)

cfg : spws
	@echo ""
	@echo making $(notdir $@)
	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make cfg
	cp $(CPSEIS_INSTALL_DIR)/spws_home/programs/$@/$(CPSEIS_ARCH)/$@ $(BINDIR)

csv : spws
	@echo ""
	@echo making $(notdir $@)
	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make csv
	cp $(CPSEIS_INSTALL_DIR)/spws_home/programs/$@/$(CPSEIS_ARCH)/$@ $(BINDIR)

msepita : spws
	@echo ""
	@echo making $(notdir $@)
	cd $(CPSEIS_INSTALL_DIR)/spws_home/making/$(CPSEIS_ARCH) && make msepita
	cp $(CPSEIS_INSTALL_DIR)/spws_home/programs/$@/$(CPSEIS_ARCH)/$@ $(BINDIR)

xmldep : $(XMLDEP)
$(XMLDEP) : sps_xml_dependencies.o
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(XMLDEP) sps_xml_dependencies.o

fordep : $(FORDEP)
$(FORDEP) : sps_fortran_dependencies.o
	@echo ""
	@echo making $(notdir $@)
	$(STATIC_LINKER) $(FFLAGS) -o $(FORDEP) sps_fortran_dependencies.o

$(STATIC_LIBCPS) : $(COBJS) $(FOBJS)
	@echo ""
	@echo making $(notdir $@)
	ar crv $@ $?

$(SHARED_LIBCPS) : $(COBJS) $(FOBJS) $(MPIOBJS)
	@echo ""
	@echo making $(notdir $@)
#	$(SHARED_LINKER) $(CFLAGS) $(CLIBS) -o $@ $? 
	$(SHARED_LINKER) $(CFLAGS) -o $@ $^ $(SH_LDFLAGS) $(SH_F90_LINK) -lm -lutil -lpthread
	ldd -r $(SHARED_LIBCPS)

ldd :
	@echo ""
	@echo making $(notdir $@)
	ldd -r $(SHARED_LIBCPS)

$(STATIC_LIBCGM) : $(CGMOBJS) cgm.o
	@echo ""
	@echo making $(notdir $@)
	ar crv $@ $?

$(SHARED_LIBCGM) : $(CGMOBJS) cgm.o
	@echo ""
	@echo making $(notdir $@)
	$(SHARED_LINKER) -o $@ $? $(SH_LDFLAGS) $(SH_F90_LINK) -lm -lutil -lpthread
	ldd -r $(SHARED_LIBCGM)

%.d : %.c
	@echo ""
	@echo making $(notdir $@)
	@set -e; rm -f %@;\
	$(CC) -M $(CFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' <$@.$$$$ > $@; \
	rm -f $@.$$$$

%.d : %.f90
	@echo ""
	@echo making $(notdir $@)
	uses %@  $<

$(XOBJS) : %.xml : %.f90
	@echo ""
	@echo making $(notdir $@)
	ezgui $< $(XMLDIR)/$@

$(COBJS) : %.o : %.c
	@echo ""
	@echo making $(notdir $@)
	$(CC) -c $(CFLAGS) $< -o $@

$(CCOBJS) : %.o : %.cc
	@echo ""
	@echo making $(notdir $@)
	$(CC) -c $(CFLAGS) $< -o $@

$(FOBJS) : %.o : %.f90
	@echo ""
	@echo making $(notdir $@)
	$(F90) -c $(FFLAGS) $< -o $@

$(FPROBJS) : %.o : %.f90
	@echo ""
	@echo making $(notdir $@)
	$(F90) -c $(FFLAGS) $< -o $@

$(CPROBJS) : %.o : %.c
	@echo ""
	@echo making $(notdir $@)
	$(CC) -c $(CFLAGS) $< -o $@

$(MPIOBJS) : %.o : %.c
	@echo ""
	@echo making $(notdir $@)
	$(CC) -c $(CFLAGS) $< -o $@

$(CGMOBJS) : %.o : %.c
	@echo ""
	@echo making $(notdir $@)
	$(CC) -c $(CFLAGS) $< -o $@

%.f90 : 
	@echo ""
	@echo making $(notdir $@)
	ln -s $(SRCDIR)/$(@F) $@

mpif.h:
	@echo ""
	@echo making $(notdir $@)
	ln -sf $(MPI_DIR)/include/mpif.h .
	
%.h :
	@echo ""
	@echo making $(notdir $@)
	ln -s $(INCDIR)/$(@F) $@

%.c :
	@echo ""
	@echo making $(notdir $@)
	ln -s $(SRCDIR)/$(@F) $@

list:
#	@echo "COBJS ========== $(COBJS)"
#	@echo "FOBJS ========== $(FOBJS)"
#	@echo "CPROBJS ======== $(CPROBJS)"
#	@echo "FPROBJS ======== $(FPROBJS)"
#	@echo "CGMOBJS ======== $(CGMOBJS)"
#	@echo "MPIOBJS ======== $(MPIOBJS)"
	@echo "LDFLAGS ======== $(LDFLAGS)"
	@echo "LAM_DIR ======== $(LAM_DIR)"
