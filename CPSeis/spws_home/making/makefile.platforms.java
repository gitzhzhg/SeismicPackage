#<CPS_v1 type="SCRIPT" pretag="#"/>


#<copyright>
#*******************************************************************************
#***********                    COPYRIGHT NOTICE                     ***********
#*********** CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC. ***********
#***********  PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK  ***********
#*******************************************************************************
#</copyright>


#<brief_doc>
#-------------------------------------------------------------------------------
#                         C P S   S C R I P T
#
# Name       : makefile.platforms
# Category   : stand-alone
# Written    : 2003-05-13   by: Tom Stoeckley
# Revised    : 2004-10-14   by: Tom Stoeckley
# Maturity   : beta
# Purpose    : Include makefile which defines platforms supported by CPS.
# Portability: No known limitations.
#
#-------------------------------------------------------------------------------
#</brief_doc>


#<descript_doc>
#-------------------------------------------------------------------------------
#                          GENERAL DESCRIPTION
#
# This include file defines appropriate variables for each platform which
# is supported by CPS (which includes the ~sps and ~spws directories).
#
# This include file should be included in other makefiles to provide
# consistent definitions for the use of compilers and linkers on different
# platforms.  It is intended that this include file be used by ~sps managers,
# ~spws managers, and developers in their own directories so that all of
# the CPS seismic processing code (and related code) will be compiled and
# linked in a consistent manner.
#
# A platform is defined here as a software system.  There can be more than
# one platform on the same architecture (operating system and hardware).
# For example: In the linux case (linux operating system on intel machines),
# several different platforms are based on the Fortran-90 Absoft, Portland
# Group, and Intel compilers.  In the SGI case, two different platforms might
# use 32-bit and 64-bit addresses.  Separate platforms should also be created
# for different versions of compilers and for different optimization/debug
# levels.
#
# To use this include file, the following line should be put into the
# user's makefile:
#
#             include $(CFECUSTOMDIR)/makefile.platforms
#
# Input and output variables in this file are upper case.
# Internally-used variables in this file are lower case.
#
#-------------------------------------------------------------------------------
#                           INPUT REQUIRMENTS
#
# The following environment variables must be set:
#
#    SPSDIR              Absolute path to ~sps home directory.
#    CFECUSTOMDIR        Location of cfecustom makefiles.
#    CFECUSTOMSCRIPTS    Location of cfecustom scripts.
#    CFECUSTOMPROGRAMS   Location of cfecustom programs.
#    ARCHITECTURE        Needed only for validating the platform.
#    HOST                Name of host computer.
#
# The following makefile variables must be set:
#
#    INCDIRS        List of directories where include files can be found.
#    MODDIRS        List of directories where module files can be found.
#    DIRECTIVES     List of -D directives to define for C and C++ compilers.
#    MATURITY       Maturity of the ~sps library to link to.
#    NEED_MPI       Whether mpi libraries are needed (yes or no) (default no).
#    NEED_LANDMARK  Whether landmark libraries needed (yes or no) (default no).
#
# Notes:
#
#    INCDIRS must not include the -I flags.
#    MODDIRS must not include the module flags (-M or -p or whatever).
#    DIRECTIVES must not include the -D flags.
#    MATURITY must be either alpha or beta or production or none.
#    If MATURITY is none, no ~sps library will be linked to.
#    INCDIRS normally should include full paths (except for possibly ..).
#    MODDIRS normally should include full paths terminating with /$(PLATFORM)
#     (except for possibly .).
#
#    Additional directives representing platform and maturity will be added
#    to the C and C++ compile commands by this include file, so they should
#    not be included in the DIRECTIVES variable.
#
#-------------------------------------------------------------------------------
#                                 ERRORS
#
# This include file quits with an error message if any of the above
# environment variables are not set, or if MATURITY is not set to alpha
# or beta or production or none.  But it is OK for INCDIRS, MODDIRS, or
# DIRECTIVES to be missing or blank.
#
# This include file also quits with an error message if gmake is not
# executed in a supported platform-specific directory, or if that directory
# is not compatible with the host computer or its architecture.
#
#-------------------------------------------------------------------------------
#                           NEW OUTPUT VARIABLES
#
# The following variables are defined in this makefile:
#
#    PLATFORM          Platform-specific directory in which gmake was typed.
#    PARENT            Directory just above the platform-specific directory.
#
#    FC                Fortran compiler with flags.
#    FC_FLOATIO        Fortran compiler with flags for FLOATIO.
#    FC_KMIG           Fortran compiler with flags for KMIG.
#    FC_PPIO           Fortran compiler with flags for PPIO.
#    FC_UNDER     Fortran compiler with flags for Absoft lower case underscore.
#    FC_LINK           Fortran linker with flags.
#
#    CC                C compiler with flags.
#    CC_LINK           C linker with flags.
#
#    CXX               C++ compiler with flags.
#    CXX_DEPEND        C++ compiler with flags for generating dependencies.
#    CXX_LINK          C++ linker with flags.
#
#    JAVAC             Java byte code generator with flags.
#    JAR               Java byte code archiver with flags
#
#    PURE_LINK         Purify linker with flags.
#    AR_LINK           Archive library (.a) maker with flags.
#    SO_LINK           Shared library (.so) maker with flags.
#
#    MODEXT            Fortran90 module file extension (with period).
#    MODFLAG           Fortran90 compiler flag for module directories.
#
#    SPS_LIBS          Low level ~sps libraries.
#    FOR_LIBS          Fortran libraries (needed if using C or C++ linker).
#    GUI_LIBS          X/Motif libraries (needed for most ~spws programs).
#    MATH_LIBS         Math and miscellaneous libraries.
#    JAVA_LIBS         Java libraries
#    CPSLIB            The ~sps library (of requested maturity) to link to.
#
# Notes:
#
#    PLATFORM will not contain the full path.
#    PARENT   will not contain the full path.
#    SPS_LIBS will not include prodlib or betalib or alphalib.
#    CPSLIB will be the absolute path to prodlib or betalib or alphalib.
#    CPSLIB and SPS_LIBS will be blank if MATURITY is none.
#
#-------------------------------------------------------------------------------
#</descript_doc>


#<history_doc>
#-------------------------------------------------------------------------------
#                           REVISION HISTORY
#
#     Date        Author     Description
#     ----        ------     -----------
# 33. 2005-05-02  Corn       Remove -DNOFSEEKO flag where DLINUX used so
#                             large segy files may be accomodated
# 32. 2004-10-14  Stoeckley  Add variables NEED_MPI and NEED_LANDMARK and
#                             FC_UNDER; remove unused variable LMRKLNK and
#                             unneeded variable FC_PC; fix FOR_LIBS, GUI_LIBS,
#                             and INCDIRS for motif platforms ab80; add motif
#                             libs for pgf90; fix motif libs for SGI.
# 31. 2004-10-11  Goodger    Add platform linuxab80_debug_gcc3.
# 30. 2004-08-27  Goodger    Change motif platforms from ab75 to ab80.
# 29. 2004-05-19  Goodger    Add -xarch=v9 to link for 64sol62.
# 28. 2004-05-18  Goodger    Add platforms 64sol62 and 64sol62_debug.
# 27. 2004-04-30  Goodger    Removed the Rs and Rc flags from debug platforms.
#                            These options core dump some routines, and
#                            making special symbols for these routines is
#                            becoming too great.
# 26. 2004-04-29  Stoeckley  Modify Portland Group FOR_LIBS by removing -rpath
#                             and hardwiring -lpgc to use the .a library.
# 25. 2004-04-27  Stoeckley  Add -DLINUXA to Absoft platforms, and add
#                             -DLINUX to Intel and PGI platforms.  Now -DLINUX
#                             means any linux platform, and -DLINUXA, -DLINUXP,
#                             and -DLINUXI refer to the specific compilers.
# 24. 2004-04-23  Stoeckley  Add platforms linuxab75_motif_1_2 and
#                             linuxab75_motif_2_1.
# 23. 2004-04-23  Goodger    Add platform linuxab80_xeon.
# 22. 2004-04-05  Goodger    Adding -D_FILE_OFFSET_BITS=64 to remaining 
#                            linux and sol platforms.
# 21. 2004-03-25  Goodger    Adding -D_FILE_OFFSET_BITS=64 to linux and sol
#                            debug platforms.
# 20. 2004-03-16  Goodger    Add symbol FC_PC for special compile of
#                            pc.f90.
# 19. 2004-03-11  R Selzler  Added "-mt" Multi-Threaded option to Solaris
# 18. 2004-03-02  Goodger    Remove the -Rc (array conformance) flag
#                            on the FC_KMIG symbol for linuxab75_debug
#                            and linuxab80_debug.  The compiler sometimes
#                            core dumps with this flag.
# 17. 2004-01-27  Selzler    Incorporate new compler flags for gcc.
#                            -D_REENTRANT is needed to support pthreads
#                            correctly, especially errno references.
# 16. 2004-01-15  Goodger    Incorporate new compiler flages on absoft
#                            debug platforms from Randy Selzler.
# 15. 2003-12-01  Goodger    Use lmrk_stubs to make .so libs.  Get rid
#                            of OWHOME and LMRKLIB.
# 14. 2003-11-19  Goodger    Add -OPT:Olimit=6000 to 64sgi73 platform.
# 13. 2003-11-11  Goodger    Add special compile variable CC_IBSMA.
# 12. 2003-11-07  Stoeckley  Remove obsolete special treatment of Portland
#                             Group compiler regarding module files; set
#                             FOR_LIBS for pgf90 compiler; remove setting
#                             of OWHOME and LD_LIBRARY_PATH since now
#                             done in setup_sps_environment; add -dy to
#                             linux GUI_LIBS.
# 11. 2003-10-02  Goodger    Add inform=inform to portland compile.  This
#                            will get all types of messages.
#                            Change module flag on linuxp_debug to -I.
#                            kmig core dumped on linuxp_debug.  The -g flag
#                            had to be removed to get kmig to compile.
#                            Add profiling flag to c code on platform 
#                            linuxab80_prof.
# 10. 2003-09-08  Goodger    Changed module flag on linuxp to -I.
#  9. 2003-09-08  Goodger    Add platform linuxab80_prof
#  8. 2003-09-02  Goodger    Added links needed by new landmark library,
#                            lmrkbeta.  Add architecure specific area.
#                            Set LD_LIBRARY_PATH due to landmark now being in
#                            a shared library.
#  7. 2003-08-07  Goodger    Add directive NOFSEEKO to linuxab75 platforms.
#                            Add new platforms sol62-56 and sol62-56_debug.
#                            This is for solaris compiler version 6.2 and
#                            operating system version 5.6.  Currently machines
#                            odi74 and odi90 are on operating system version
#                            5.6 where other solaris machines are on 5.8.
#  6. 2003-07-17  Goodger    Correct absoft library path for linuxab80.
#  5. 2003-07-09  Goodger    Added xarch=generic to sol70 compile.
#  4. 2003-07-08  Goodger    Add platforms linuxab80, linuxab80_debug.
#  3. 2003-06-20  Stoeckley  Change CC in non-debug platforms to be optimized,
#                             and remove CC_OPT.
#  2. 2003-06-16  Stoeckley  Use new scripts for sun Fortran and C and C++
#                             compilers; add -compat=4 to sun C++ compiler
#                             flags; add MATURITY option "none" for when
#                             ~sps libraries are not needed; add altix71
#                             platform; add -lfortran and -lffio to sgi
#                             fortran libraries.
#  1. 2003-05-13  Stoeckley  Initial version, made from Karen's makefile in
#                            ~sps/alpha/spsmodules, and an older makefile
#                            with the same name as this one, which has been
#                            used in ~spws and Tom's directories for a couple
#                            of years.  Several obsolete platforms
#                            have been removed or renamed.  Four new SGI
#                            platforms have been added, but without correct
#                            contents which will be determined later.  The
#                            directive -DTESTLIB was removed.  An improved (and
#                            simplified) means of verifying the suitability
#                            of the platform-specific directory is implemented.
#
#-------------------------------------------------------------------------------
#</history_doc>


# RCS Id : $Id: makefile.platforms,v 1.29 2004/05/19 12:19:54 Goodger prod sps $


##---------------- verify presence of required variables -------------------##
##---------------- verify presence of required variables -------------------##
##---------------- verify presence of required variables -------------------##


ifeq ($(strip $(SPSDIR)),)
  $(error environment variable SPSDIR not set)
endif

ifeq ($(strip $(ARCHITECTURE)),)
  $(error environment variable ARCHITECTURE not set)
endif

ifeq ($(strip $(HOST)),)
  $(error environment variable HOST not set)
endif

ifeq ($(strip $(MATURITY)),)
  $(error makefile variable MATURITY not set)
endif

ifeq ($(strip $(JAVA_HOME)),)
  $(error makefile variable JAVA_HOME not set)
endif


##---------------- verify permitted value of maturity ---------------------##
##---------------- verify permitted value of maturity ---------------------##
##---------------- verify permitted value of maturity ---------------------##


whoops = true

ifeq ($(MATURITY), production)
  whoops = false
endif

ifeq ($(MATURITY), beta)
  whoops = false
endif

ifeq ($(MATURITY), alpha)
  whoops = false
endif

ifeq ($(MATURITY), none)
  whoops = false
endif

ifeq ($(whoops), true)
  $(error illegal MATURITY: $(MATURITY) \
       - must be alpha or beta or production or none)
endif


##--------------------------- initialization -------------------------------##
##--------------------------- initialization -------------------------------##
##--------------------------- initialization -------------------------------##

# assume format of PWD is $(LOCDIR)/sys/classes/com/conocophillips/$(SUBTREE)
SUBTREE = $(notdir $(PWD))
$(warning SUBTREE = $(SUBTREE))
$(warning CURDIR = $(dir $(CURDIR)))
COP     = $(notdir $(patsubst %/,%, $(dir $(CURDIR))))
$(warning COP = $(COP))
PWDM1   = $(dir    $(patsubst %/,%, $(dir $(CURDIR))))
$(warning PWDM1 = $(PWDM1))
COM     = $(notdir $(patsubst %/,%, $(dir $(PWDM1))))
$(warning COM = $(COM))
PWDM2   = $(dir    $(patsubst %/,%, $(dir $(PWDM1))))
$(warning PWDM2 = $(PWDM2))
CLAS    = $(notdir $(patsubst %/,%, $(dir $(PWDM2))))
$(warning CLASSES = $(CLAS))
PWDM3   = $(dir    $(patsubst %/,%, $(dir $(PWDM2))))
$(warning PWDM3 = $(PWDM3))
SYS    = $(notdir $(patsubst %/,%, $(dir $(PWDM3))))
$(warning SYS = $(SYS))
LOCDIR  = $(patsubst %/,%, $(dir $(patsubst %/,%, $(dir $(PWDM3)))))
$(warning LOCDIR = $(LOCDIR))
SRCDIR = $(LOCDIR)/port/$(COM)/$(COP)/$(SUBTREE)

### Note: CURDIR is the full path of the current directory.
###       CURDIR is a gmake variable which is always defined.
###       CURDIR must be used above instead of PWD for situations where rsh
###       is used, because rsh apparently retains the previous PWD.


##------------------ error detection initialization ------------------------##
##------------------ error detection initialization ------------------------##
##------------------ error detection initialization ------------------------##


permitted_language = unset
permitted_hosts    = unrestricted

### Note: permitted_language must be reset to one or more allowed
###       languages.

### Note: permitted_hosts must be reset to one or more allowed hosts, but
###       only in cases where some hosts with the correct architecture cannot
###       be used (e.g. missing compiler or wrong version of compiler or wrong
###       version of operating system).

ifeq ($(COP), conocophillips)
  ifeq ($(COM), com)
    ifeq ($(SYS), sys)
      JAVAC = $(JAVA_HOME)/bin/javac -classpath $(CLASS_PATH) -d $(CLASSDIR) -deprecation -g
      JAR   = $(JAVA_HOME)/bin/jar -cvf
      permitted_language = java
    endif
  endif
endif


##------------------------------ error -------------------------------------##
##------------------------------ error -------------------------------------##
##------------------------------ error -------------------------------------##


ifeq ($(permitted_language), unset)
  $(warning )
  $(warning ***************************************************************)
  $(warning $(HOST)  $(PORT)/$(COM)/$(COP)/$(SUBTREE)  $(ARCHITECTURE))
  $(warning ***************************************************************)
  $(warning wrong directory.)
  $(warning you should be in a /port/com/conocophillips subtree.)
  $(warning ***************************************************************)
  $(error fatal error)
endif

ifneq ($(permitted_hosts), unrestricted)
  host = $(findstring $(HOST), $(permitted_hosts))
  ifneq ($(host), $(HOST))
    $(warning )
    $(warning ***************************************************************)
    $(warning $(HOST)  $(SUBTREE)  $(ARCHITECTURE))
    $(warning ***************************************************************)
    $(warning wrong host for this java source subdirectory.)
    $(warning permitted hosts are: $(permitted_hosts).)
    $(warning ***************************************************************)
    $(error fatal error)
  endif
endif

##-------------------------------- end -------------------------------------##
##-------------------------------- end -------------------------------------##
##-------------------------------- end -------------------------------------##

