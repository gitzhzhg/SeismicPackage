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
# Name       : makefile.spws
# Category   : stand-alone
# Written    : 2003-06-10   by: Tom Stoeckley
# Revised    : 2006-01-09   by: Kruger Corn
# Maturity   : beta
# Purpose    : Comprehensive include makefile for CPS developers and managers.
# Portability: No known limitations.
#
#-------------------------------------------------------------------------------
#</brief_doc>


#<descript_doc>
#-------------------------------------------------------------------------------
#                          GENERAL DESCRIPTION
#
# This comprehensive include file should simplify the efforts of ~sps and
# ~spws developers, and ~spws managers.  This include file can be included
# in the user's makefile for simplicity and uniformity.
#
# An appropriate way to use this file is to simply include this file in
# the user's makefile as follows:
#
#                include $(SPWSMAKEDIR)/makefile.spws
#
# The following targets are supported by this makefile:
#
#       gmake               build TARGET_LIBRARY and TARGET_PROGRAMS and TARGET_JAR.
#       gmake all           build TARGET_LIBRARY and TARGET_PROGRAMS and TARGET_JAR.
#       gmake programs      build TARGET_PROGRAMS.
#       gmake program       build TARGET_PROGRAMS.
#       gmake progs         build TARGET_PROGRAMS.
#       gmake prog          build TARGET_PROGRAMS.
#       gmake library       build TARGET_LIBRARY.
#       gmake lib           build TARGET_LIBRARY.
#       gmake jar           build TARGET_JAR.
#       gmake depend        build makefile.depend.
#       gmake clean         remove all .o and .mod and .class files.
#       gmake help          print this help information.
#       gmake list          list macros used/created in this makefile.
#
# With the exception of java files, the gmake command must always be typed
# in a platform-specific directory regardless of the target (even for gmake
# depend and gmake help, which are not platform-specific operations).
#
#-------------------------------------------------------------------------------
#                         CONTENTS OF THIS FILE
#
# This include file is divided into four sections:
#
#   (1) Defaults:
#
#           Sets defaults for unspecified user-defined macros.
#
#   (2) Inclusion of makefile.platforms:
#
#           Uses a few user-defined macros.
#           Sets platform-specific macros.
#
#   (3) Commands:
#
#           Uses user-defined macros.
#           Uses platform-specific macros.
#           Contains commands for compiling, linking, etc.
#
#   (4) Inclusion of makefile.depend:
#
#           Contains dependencies.
#           Resides in the source code directory.
#           Generated automatically by typing "gmake depend" in any
#            platform-specific directory.
#
# The file called makefile.depend contains all dependencies for Fortran,
# C, C++, and XML files.  This makefile.depend file is useful for ~spws
# managers, and ~spws and ~sps developers, and is automatically created
# by executing "gmake depend" in any platform-specific directory below
# the source code code directory.  You should run "gmake depend" whenever
# you make source code changes which will change the dependencies.
#
#-------------------------------------------------------------------------------
#                           INPUT REQUIRMENTS
#
# The following environment variables must be set:
#
#    SPWSDIR            Absolute path to ~spws home directory.
#    SPSDIR             Absolute path to ~sps home directory.
#    CFECUSTOMDIR       Location of cfecustom makefiles.
#    CFECUSTOMSCRIPTS   Location of cfecustom scripts.
#    CFECUSTOMPROGRAMS  Location of cfecustom programs.
#    SPWSMAKEDIR        Location of spws scripts and makefiles.
#    ARCHITECTURE       Hardware/OS architecture.
#    HOST               Name of host computer.
#    JAVA_HOME          Absolute path to JDK
#
#    All of the above are set in the script called setup_sps_environment,
#    which is normally sourced from the user's .cshrc file.
#
# The following makefile variables must be set (for makefile.platforms):
#
#    MATURITY           Set to alpha or beta or production or none.
#    INCDIRS            List of directories where include files can be found.
#    MODDIRS            List of directories where module files can be found.
#    DIRECTIVES         List of -D directives for C and C++ compilers.
#    MYINCDIRS          List of a user's own include directories.
#    MYMODDIRS          List of a user's own module directories.
#    NEED_MPI           Whether mpi libraries needed (yes or no) (default no).
#    NEED_LANDMARK      Whether landmark libs needed (yes or no) (default no).
#
# The following makefile variables must be set (for commands):
#
#    LINKMODE           Type of linker and libraries required.
#    SPWSLIBS           List of ~spws libraries to link to.
#    SPWSJARS           List of ~spws jars.
#    SPSJARS            List of ~sps jars.
#    MYLIBS             List of a user's own libraries to link to.
#    MYJARS             List of a user's own jars.
#    SRCDIR             Single directory containing source files.
#    CLASSDIR           Single directory containing class files.
#    PREFIX             Filename prefix for all source code main programs.
#    EXCLUDE            List of source files to exclude.
#    TARGET_PROGRAMS    List of executables created by the makefile.
#    TARGET_LIBRARY     Single library created or updated by the makefile.
#    TARGET_JAR         Single jar created or updated by the makefile.
#    SRCS               List of source files in SRCDIR.
#    OBJS               List of object files in local directory to link to.
#    CLASSES            List of class files in local directory put in a jar.
#    MODPUT             Single directory where module files are put.
#    LINK_TO_LIBS_ONLY  Whether to link only to libraries and not OBJS.
#    REMOVE_AND_TOUCH   Whether to remove and touch all .o fnd .class files
#                        after updating the library or jar.
#
# The following scripts and include files are used:
#
#    $(SRCDIR)/makefile.depend
#    $(CFECUSTOMDIR)/makefile.platforms        <-- if no special version.
#    $(SPWSMAKEDIR)/makefile.platforms         <-- if special ~spws version.
#    $(CFECUSTOMSCRIPTS)/make_dependencies
#    $(CFECUSTOMSCRIPTS)/sps_fix_dependencies
#    $(CFECUSTOMPROGRAMS)/sps_fortran_dependencies
#    $(CFECUSTOMPROGRAMS)/sps_xml_dependencies
#
#-------------------------------------------------------------------------------
#                      MAKEFILE VARIABLE DEFAULTS
#
# Any makefile variables which have not been set in the user's makefile
# will be set to the following valid defaults:
#
#    MATURITY           production (must be alpha or beta or production or none)
#    INCDIRS            created from LINKMODE  MYINCDIRS  SRCDIR  MATURITY
#    MODDIRS            created from LINKMODE  MYMODDIRS    .     MATURITY
#    DIRECTIVES         (blank)
#    MYINCDIRS          (blank)
#    MYMODDIRS          (blank)
#    NEED_MPI           no
#    NEED_LANDMARK      no
#
#    LINKMODE           fortran (must be fortran or c or cxx or spws or motif
#                        or java or jspws)
#    SPWSLIBS           list of all spws libraries in correct order
#    SPWSJARS           list of all spws jars in correct order
#    SPSJARS            list of all sps jars in correct order
#    MYLIBS             (blank)
#    MYJARS             (blank)
#    SRCDIR             full path of parent directory
#    CLASSDIR           full path of class directory
#    PREFIX             (blank)
#    EXCLUDE            (blank)
#    TARGET_PROGRAMS    created from SRCDIR and PREFIX and EXCLUDE
#    TARGET_LIBRARY     (blank)
#    TARGET_JAR         (blank)
#    SRCS               created from SRCDIR and TARGET_PROGRAMS and EXCLUDE
#    OBJS               created from SRCS
#    CLASSES            created from SRCS
#    MODPUT             . (current directory)
#    LINK_TO_LIBS_ONLY  no (must be yes or no)
#    REMOVE_AND_TOUCH   no (must be yes or no)
#
# Any makefile variables which have been set in the user's makefile will
# not be changed.  Exception: TARGET_PROGRAMS and SRCS will be modified if
# EXCLUDE is not blank.
#
#-------------------------------------------------------------------------------
#                      MAKEFILE VARIABLE DETAILS
#
# MATURITY:
#
#   (1) Maturity of ~sps library to link to.
#   (2) If set to none, no ~sps library will be linked to.
#   (3) Used by makefile.platforms.
#   (4) ALLOWED: production or beta or alpha or none
#   (5) DEFAULT: production
#
# INCDIRS:
#
#   (1) List of directories where include files can be found.
#   (2) Needed only for the -I option for C and C++ compilers.
#   (3) This macro must not include the -I flags.
#   (4) This macro normally should include full paths (except for possibly ..).
#   (5) The order of the directories in INCDIRS must be the order in which
#        the directories should be searched to find include files.
#   (6) Normally for developers who keep their include files in the same
#        directory as the source files, INCDIRS should have the source file
#        directory (normally ..) listed as the first directory.
#   (7) Used by makefile.platforms and vpath.
#   (8) DEFAULT: depends on LINKMODE and MYINCDIRS and MATURITY as follows:
#        if LINKMODE == spws:  INCDIRS = SRCDIR MYINCDIRS SPWSDIR/include
#        otherwise:            INCDIRS = SRCDIR MYINCDIRS
#        plus one of the following lines:
#                                                  SPSDIR/production/include
#                              SPSDIR/beta/include SPSDIR/production/include
#         SPSDIR/alpha/include SPSDIR/beta/include SPSDIR/production/include
#
# MODDIRS:
#
#   (1) List of directories where fortran module files can be found.
#   (2) Needed only for the modules option for Fortran-90 compilers.
#   (3) This macro must not include the module flags (-M or -p or whatever).
#   (4) These directories should normally be appended with /$(PLATFORM) unless
#        they are the local directory.
#   (5) The order of the directories in MODDIRS must be the order in which
#        the directories should be searched to find module files.
#   (6) Used by makefile.platforms and vpath.
#   (7) MODDIRS normally should include full paths terminating with
#        /$(PLATFORM) (except for possibly .).
#   (8) Normally for developers who keep their module files in the same
#        directory as the object files, MODDIRS should have the object file
#        directory (normally .) listed as the first directory.
#   (9) For commands, this macro must include the local directory (.) as the
#        first directory, and also MODPUT as the second directory unless
#        MODPUT is the local directory (.).
#  (10) DEFAULT: depends on LINKMODE and MYMODDIRS and MATURITY as follows:
#        if LINKMODE == spws:  MODDIRS = . MYMODDIRS SPWSDIR/modules/PLATFORM
#        otherwise:            MODDIRS = . MYMODDIRS
#        plus one of the following lines:
#                                         SPSDIR/production/modules/PLATFORM
#                                         SPSDIR/beta/modules/PLATFORM
#                                         SPSDIR/alpha/modules/PLATFORM
#        SPS managers would need to use subdirectory spsmodules above.
#
# DIRECTIVES:
#
#   (1) Some directives to define for C and C++ compilers.
#   (2) This macro must not include the -D flags.
#   (3) Used by makefile.platforms.
#   (4) Additional directives representing platform and maturity will be
#        added to the C and C++ compile commands, so they should not be
#        included in the DIRECTIVES variable.
#   (5) DEFAULT: blank
#
# LINKMODE:  
#
#   (1)  Type of linker and libraries required.
#   (2)  Must be set to fortran if using a Fortran linker.
#   (3)  Must be set to    c    if using a C linker.
#   (4)  Must be set to   cxx   if using a C++ linker.
#   (5)  Must be set to  spws   if using a C++ linker for ~spws programs.
#   (6)  Must be set to  motif  if using a C++ linker and motif but not ~spws.
#   (7)  Must be set to  java   if using JAR but not ~spws programs.
#   (8)  Must be set to  jspws  if using JAR for ~spws programs.
#   (9)  Fortran libraries are linked to for C and C++ linkers.
#   (10) X-windows and Motif libraries are linked to for spws/motif programs.
#
# SPWSLIBS:
#
#   (1) List of ~spws libraries to link to.
#   (2) These libraries must be in the correct order for the linker.
#   (3) These libraries must include directory paths.
#   (4) These libraries cannot be specified with -L or -l because they are
#        used for dependencies as well as on the link command.
#   (5) These libraries must not include system libraries or alphalib or
#        betalib or prodlib.
#   (6) DEFAULT: all ~spws libraries in (hopefully) the correct order.
#
# SPWSJARS:
#
#   (1) List of ~spws jars referenced.
#   (2) These jars must be in the correct order.
#   (3) These jars must include directory paths.
#   (4) DEFAULT: all ~spws jars in (hopefully) the correct order.
#
# SPSJARS:
#
#   (1) List of ~sps jars referenced.
#   (2) These jars must be in the correct order.
#   (3) These jars must include directory paths.
#   (4) DEFAULT: all ~sps libraries in (hopefully) the correct order.
#
# MYLIBS:
#
#   (1) List of user libraries to link to.
#   (2) These libraries must be in the correct order for the linker.
#   (3) These libraries will precede any other libraries on the link command.
#   (4) These libraries must include directory paths.
#   (5) These libraries cannot be specified with -L or -l because they are
#        used for dependencies as well as on the link command.
#   (6) These libraries must not include system libraries or alphalib or
#        betalib or prodlib or ~spws libraries.
#   (7) DEFAULT: blank
#
# MYJARS:
#
#   (1) List of user libraries referenced.
#   (2) These jars must be in the correct order.
#   (3) These jars will precede any other jars.
#   (4) These jars must include directory paths.
#   (5) DEFAULT: blank
#
# SRCDIR:
#
#   (1) Directory containing source files.
#   (2) Needed for vpath and compiler commands.
#   (3) Needed to define SRCS if the user has not done so.
#   (4) Used by makefile.commands.
#   (5) DEFAULT: parent directory.
#
# CLASSDIR:
#
#   (1) Directory containing class files.
#   (2) Needed for vpath and compiler commands.
#   (3) Needed to define CLASSES if the user has not done so.
#   (4) Used by makefile.commands.
#   (5) DEFAULT: classes directory.
#
# PREFIX:
#
#   (1) A filename prefix identifying all source code files which are main
#        programs.
#   (2) All executables with names corresponding to source code files starting
#        with this prefix are added to TARGET_PROGRAMS.
#   (3) Used only to set the default for TARGET_PROGRAMS.
#   (4) DEFAULT: blank
#
# EXCLUDE:
#
#   (1) List of source files in the SRCDIR directory to be excluded from SRCS
#        and TARGET_PROGRAMS.
#   (2) These files must not include a directory path.
#   (3) Used only to set the defaults for TARGET_PROGRAMS and SCRS, and
#        indirectly for OBJS or CLASSES.
#   (4) DEFAULT: blank
#
# TARGET_PROGRAMS:
#
#   (1) List of executables.
#   (2) These files must include the directory path (with /$(PLATFORM)
#        appended) unless they reside in the local directory.
#   (3) Files defined by PREFIX are added to TARGET_PROGRAMS.
#   (4) Files defined by EXCLUDE are then removed from TARGET_PROGRAMS.
#   (5) These programs will be made if gmake is typed without a target
#        argument, or with the 'all' or 'programs' or 'program' or 'progs'
#        or 'prog' target.
#   (6) Used by commands for linking.
#   (7) DEFAULT: blank
#
# TARGET_LIBRARY:
#
#   (1) Single library created or updated by the makefile.
#   (2) This file must include the directory path (with /$(PLATFORM) appended)
#        unless it resides in the local directory.
#   (3) This library will be made if gmake is typed without a target argument,
#        or with the 'all' or 'library' or 'lib' target,
#   (4) Used by commands for library updates.
#   (5) DEFAULT: blank
#
# TARGET_JAR:
#
#   (1) Single jar created or updated by the makefile.
#   (2) This file must include the directory path unless it resides in the
#        local directory.
#   (3) This jar will be made if gmake is typed without a target argument,
#        or with the 'all' or 'jar' target,
#   (4) Used by commands for jar updates.
#   (5) DEFAULT: blank
#
# SRCS:
#
#   (1) List of source files in the SRCDIR directory.
#   (2) These files must not include a directory path.
#   (3) These files must not include any main programs.
#   (4) Files defined by TARGET_PROGRAMS are removed from SCRS.
#   (5) Files defined by EXCLUDE are then removed from SCRS.
#   (6) Used only to set the default for OBJS or CLASSES.
#   (7) DEFAULT: all .f90 and .f and .c and .cc and .java iles in SRCDIR.
#
# OBJS:
#
#   (1) List of object files in the local directory (where gmake is invoked).
#   (2) These files must not include a directory path.
#   (3) These files must not include any main programs.
#   (4) Used by commands for linking and library updates.
#   (5) DEFAULT: list of .o files made from all of the files listed in SRCS.
#
# CLASSES:
#
#   (1) List of class files via the local directory (where gmake is invoked).
#   (2) These files must not include a directory path.
#   (3) Used by commands for jar updates.
#   (4) DEFAULT: list of .class files made from all of the files listed in SRCS.
#
# MODPUT:
#
#   (1) Single directory where fortran module files are put.
#   (2) This directory must be appended with /$(PLATFORM) unless it is the
#        local directory.
#   (3) Needed for the commands which make an executable or a library.
#   (4) After a library update, all modules will be copied to the directory
#        specified by MODPUT.  This directory must also be the second directory
#        specified in MODDIRS (after .).  Modules are not copied at any other
#        time.
#   (5) DEFAULT: local directory
#
# LINK_TO_LIBS_ONLY:
#
#   (1) Whether to link only to LIBS and not OBJS.
#   (2) Used when updating a program in TARGET_PROGRAMS.
#   (3) If this macro is no, OBJS followed by LIBS will both be dependencies
#        and on the link command.
#   (4) If this macro is yes, just LIBS will both be dependencies and on the
#        link command.
#   (5) It makes sense for this macro to be yes only if TARGET_LIBRARY is
#        specified, is the first library in LIBS, and no subsequent library
#        in LIBS (if any) refers to anything in the first library.
#   (6) ALLOWED: yes or no
#   (7) DEFAULT: no
#
# REMOVE_AND_TOUCH:
#
#   (1) Whether to remove and touch all .o or .java files after updating
#        the library or jar.
#   (2) Used when updating the library or jar in TARGET_LIBRARY or TARGET_JAR. 
#   (3) If this macro is yes, after a library or jar update, all object or
#        byte code will be removed, then touched to create zero-length files
#        with the appropriate time stamp.  This is simply to save disk space
#        when the object code is not really needed after it placed onto the
#        library or jar.
#   (4) This macro can safely be yes in any circumstances EXCEPT when
#        both TARGET_LIBRARY and TARGET_PROGRAMS are specified, and
#        TARGET_LIBRARY is not in LIBS or not the only library listed in LIBS.
#   (5) If this macro is yes, the object code will not be included in the
#        link command because the libraries will satisfy all externals, and
#        because the object files will have zero length.
#        Otherwise, the object code will be included before the libraries.
#   (6) ALLOWED: yes or no
#   (7) DEFAULT: no
#
#-------------------------------------------------------------------------------
#                                ERRORS
#
# This include file quits with an error message if any required environment
# variables are not set, or if any makefile variable has been set to an
# illegal value.  But it is OK for any makefile variables to be missing or
# blank.
#
# This include file also quits with an error message if gmake is not
# executed in a supported platform-specific directory, or if that directory
# is not compatible with the host computer or its architecture.
#
#-------------------------------------------------------------------------------
#</descript_doc>


#<advice_doc>
#-------------------------------------------------------------------------------
#                   MAKEFILE EXAMPLE FOR ~SPS DEVELOPERS
#
# Example of a makefile which makes several executable test programs
# (whose filenames begin with test_) and a library for use with the test
# programs:
#
# (1) The source code and include files reside in the parent directory.
# (2) The object code and modules reside in the local platform-specific dir.
# (3) The executables and library reside in the local platform-specific dir.
# (4) Additional include files and modules reside in ~sps directories.
# (5) All source files with extensions .f90 and .f and .c and .cc are used.
# (6) The executables are linked to the target library and betalib.
#
#    LINKMODE        = fortran
#    MATURITY        = beta
#    PREFIX          = test_
#    TARGET_LIBRARY  = mylib.a
#
#    include ($SPWSMAKEDIR)/makefile.spws
#
#-------------------------------------------------------------------------------
#             MAKEFILE EXAMPLE FOR ~SPWS DEVELOPERS AND MANAGERS
#
# Example of a makefile which makes a single executable program:
#
# (1) The source code and include files reside in the parent directory.
# (2) The object code and modules reside in the local platform-specific dir.
# (3) The executable resides in the local platform-specific dir.
# (4) Additional include files and modules reside in ~spws and ~sps dirs.
# (5) All source files with extensions .f90 and .f and .c and .cc are used.
# (6) The executable is linked to ~spws libraries and prodlib.
#
#    LINKMODE        = spws
#    MATURITY        = production
#    TARGET_PROGRAMS = geopress
#
#    include ($SPWSMAKEDIR)/makefile.spws
#
#
#-------------------------------------------------------------------------------
#                   MAKEFILE EXAMPLE FOR ~SPWS MANAGERS
#
# Example of a makefile which makes a single jar:
#
# (1) The source code in the parent directory.
# (2) The byte code reside in the local class dir.
# (3) Additional jar files reside in ~spws and ~sps dirs.
# (4) All source files with extensions .java are used.
#
#    LINKMODE         = spws
#    MATURITY         = beta
#    TARGET_JAR       = $(SPWSDIR)/sys/com/conocophillips/jar/javaseis.jar
#
#    include ($SPWSMAKEDIR)/makefile.spws
#
#-------------------------------------------------------------------------------
#</advice_doc>


#<history_doc>
#-------------------------------------------------------------------------------
#                           REVISION HISTORY
#
#     Date        Author     Description
#     ----        ------     -----------
#  6. 2006-01-09  Corn       Add java stuff
#  5. 2004-10-05  Stoeckley  Add NEED_MPI and NEED_LANDMARK.
#  4. 2004-07-27  Done       Add libhdfmod.a, libdf.a, libjpeg.a, and libz.a
#                            to definition of SPWSLIBS.
#  3. 2004-05-19  Stoeckley  Add variables MYINCDIRS and MYMODDIRS; change to
#                             include makefile.platforms from ~sps instead of
#                             ~spws.
#  2. 2003-11-07  Stoeckley  Remove obsolete special treatment of modules for
#                             Portland Group compiler.
#  1. 2003-06-10  Stoeckley  Initial version.
#
#-------------------------------------------------------------------------------
#</history_doc>


#-------------------------------------------------------------------------------
# RCS Id : $Id: makefile.spws,v 1.4 2004/04/15 17:20:44 wjdone Exp $
#-------------------------------------------------------------------------------


##---------------- verify presence of required variables -------------------##
##---------------- verify presence of required variables -------------------##
##---------------- verify presence of required variables -------------------##

      # all other required variables are verified in makefile.platforms.

ifeq ($(strip $(SPWSDIR)),)
  $(error environment variable SPWSDIR not set)
endif


##----------------------- duplicate definition of PARENT -------------------##
##----------------------- duplicate definition of PARENT -------------------##
##----------------------- duplicate definition of PARENT -------------------##

   # PARENT is defined here even though it is defined in makefile.platforms,
   # because it is used by makefile.spws to define TARGET_PROGRAMS, and
   # must precede the := redefinition of TARGET_PROGRAMS below since that
   # redefinition is not recursive.

#PARENT   = $(notdir $(patsubst %/,%, $(dir $(CURDIR))))



##----------------------- set simple defaults -----------------------------##
##----------------------- set simple defaults -----------------------------##
##----------------------- set simple defaults -----------------------------##

            # CURDIR is a gmake variable which is always defined.

ifeq ($(strip $(MATURITY)),)
  MATURITY = production
endif

ifeq ($(strip $(MYJARS)),)
  MYJARS =
endif

ifeq ($(strip $(CLASSDIR)),)
  CLASSDIR = $(LOCDIR)/sys/classes
endif

ifeq ($(strip $(JARDIR)),)
  JARDIR = $(LOCDIR)/sys/jars
endif

ifeq ($(strip $(CLASS_PATH)),)
  CLASS_PATH = $(CLASSDIR):$(JARDIR)
endif

ifeq ($(strip $(PREFIX)),)
  PREFIX =
endif

ifeq ($(strip $(EXCLUDE)),)
  EXCLUDE =
endif

ifeq ($(strip $(TARGET_JAR)),)
  TARGET_JAR =
endif

ifeq ($(strip $(REMOVE_AND_TOUCH)),)
  REMOVE_AND_TOUCH = no
endif


##---------------------- set default for SPWSJARS -------------------------##
##---------------------- set default for SPWSJARS -------------------------##
##---------------------- set default for SPWSJARS -------------------------##


ifeq ($(strip $(SPWSJARS)),)

  SPWSJARS = \
               $(SPWSDIR)/sys/jars/spws0.jar \
               $(SPWSDIR)/sys/jars/spws1.jar

endif


##---------------------- set default for SPSJARS -------------------------##
##---------------------- set default for SPSJARS -------------------------##
##---------------------- set default for SPSJARS -------------------------##


ifeq ($(strip $(SPSJARS)),)

  SPSJARS = \
               $(SPSDIR)/$(MATURITY)/sys/jars/sps0.jar \
               $(SPSDIR)/$(MATURITY)/sys/jars/sps1.jar

endif


##------------------------ makefile.platforms -----------------------------##
##------------------------ makefile.platforms -----------------------------##
##------------------------ makefile.platforms -----------------------------##

include $(SPWSMAKEDIR)/makefile.platforms.java

## Output variables:
##
##    SUBTREE           Source directory in which gmake was typed.
##    LOCDIR            Directory above the package source directory.
##
##    JAVAC             Java byte code generator with flags
##    JAR               Java byte code archiver with flags
##
## Notes:
##
##    SUBTREE will not contain the full path.
##    LOCDIR  will contain the full path.



##------------------- set default for SRCDIR ----------------------##

#ifeq ($(strip $(SRCDIR)),)
#  SRCDIR = $(patsubst %/,%, $(dir $(CURDIR)))/$(SUBTREE)
#endif


##------------------- set default for TARGET_SUBTREES ----------------------##
##------------------- set default for TARGET_SUBTREES ----------------------##
##------------------- set default for TARGET_SUBTREES ----------------------##

               # also modify TARGET_SUBTREES based on EXCLUDE.

ifneq ($(strip $(PREFIX)),)
   target_subtrees := $(notdir $(wildcard $(SRCDIR)/$(PREFIX)*.java))
   target_subtrees := $(basename $(target_programs))
   TARGET_SUBTREES := $(TARGET_SUBTREES) $(target_subtrees)
endif

TARGET_SUBTREES := $(filter-out $(basename $(EXCLUDE)), $(TARGET_SUBTREES))


##---------------------- set default for SRCS ---------------------------##
##---------------------- set default for SRCS ---------------------------##
##---------------------- set default for SRCS ---------------------------##

                 # also modify SCRS based on EXCLUDE.

$(warning SUBTREE = $(SUBTREE))
$(warning SRCS before = $(SRCS))
ifeq ($(strip $(SRCS)),)
  SRCS     := $(notdir $(wildcard $(SRCDIR)/*.java))
  templist := $(addsuffix .java, $(TARGET_SUBTREES))
  SRCS     := $(filter-out $(templist), $(SRCS))
endif

SRCS := $(filter-out $(EXCLUDE), $(SRCS))
$(warning SRCS after = $(SRCS))


##---------------------- set default for CLASSES ---------------------------##
##---------------------- set default for CLASSES ---------------------------##
##---------------------- set default for CLASSES ---------------------------##

ifeq ($(strip $(CLASSES)),)
  CLASSES := $(addsuffix .class, $(basename $(SRCS)))
endif


##--------------------- check validity of MATURITY --------------------##
##--------------------- check validity of MATURITY --------------------##
##--------------------- check validity of MATURITY --------------------##


ifeq ($(MATURITY), none)
  $(error MATURITY cannot be none)
endif



##------------------------------ vpath ------------------------------------##
##------------------------------ vpath ------------------------------------##
##------------------------------ vpath ------------------------------------##


vpath
vpath %.java       $(SRCDIR)


##------------------------------ commands --------------------------------##
##------------------------------ commands --------------------------------##
##------------------------------ commands --------------------------------##


####.SUFFIXES: .class .java

.PHONY : all jar clean list help


all      :  $(TARGET_JAR)
jar      :  $(TARGET_JAR)


 
##---------------------- general compile commands ------------------------##
##---------------------- general compile commands ------------------------##
##---------------------- general compile commands ------------------------##


%.class : %.java
	@echo ""
	$(JAVAC) $(SRCDIR)/$*.java
 
 
##------------------- library create/update command ---------------------##
##------------------- library create/update command ---------------------##
##------------------- library create/update command ---------------------##

 
##------------------- jar create/update command ---------------------##
##------------------- jar create/update command ---------------------##
##------------------- jar create/update command ---------------------##

 
$(TARGET_JAR): $(CLASSES)
	@echo ""
	@echo "-------------------------------------------------------"
	@echo "making jar  $@"
	@echo "-------------------------------------------------------"
	$(JAR)     $@ $?
ifeq ($(REMOVE_AND_TOUCH),yes)
	-\rm -f    $?
	touch      $?
	touch      $@
endif
	@echo "-------------------------------------------------------"
	@echo "jar  $@  in  finished  `date`"
	@echo "-------------------------------------------------------"
	@echo ""
 

##------------------------ clean command ---------------------------------##
##------------------------ clean command ---------------------------------##
##------------------------ clean command ---------------------------------##


clean:
	-rm -f *.class *.jar


##-------------------------- help command ---------------------------------##
##-------------------------- help command ---------------------------------##
##-------------------------- help command ---------------------------------##

 
help:
	@echo ""
	@echo "--------------------------------------------------------------"
	@echo You must type the following in a platform-specific directory:
	@echo "--------------------------------------------------------------"
	@echo gmake ============= build TARGET_JAR.
	@echo gmake all ========= build TARGET_JAR.
	@echo gmake jar ========= build TARGET_JAR.
	@echo gmake clean ======= remove all .class and .jar files.
	@echo gmake help ======== print this help information.
	@echo gmake list ======== list macros used/created in this makefile.
	@echo "--------------------------------------------------------------"
	@echo ""


##-------------------------- list command ---------------------------------##
##-------------------------- list command ---------------------------------##
##-------------------------- list command ---------------------------------##

 
list:
	@echo ""
	@echo "----------------------------------------------"
	@echo "  environment variables:"
	@echo "----------------------------------------------"
	@echo ""
	@echo SPSDIR ================= $(SPSDIR)
	@echo SPWSDIR ================ $(SPWSDIR)
	@echo ARCHITECTURE =========== $(ARCHITECTURE)
	@echo SPWSMAKEDIR ============ $(SPWSMAKEDIR)
	@echo HOST =================== $(HOST)
	@echo ""
	@echo "----------------------------------------------"
	@echo "  user specified variables:"
	@echo "----------------------------------------------"
	@echo ""
	@echo MATURITY =============== $(MATURITY)
	@echo SPWSJARS =============== $(SPWSJARS)
	@echo MYJARS ================= $(MYJARS)
	@echo SRCDIR ================= $(SRCDIR)
	@echo CLASSDIR =============== $(CLASSDIR)
	@echo JARDIR ================= $(JARDIR)
	@echo PREFIX ================= $(PREFIX)
	@echo EXCLUDE ================ $(EXCLUDE)
	@echo TARGET_JAR ============= $(TARGET_JAR)
	@echo SRCS =================== $(SRCS)
	@echo CLASSES ================ $(CLASSES)
	@echo REMOVE_AND_TOUCH ======= $(REMOVE_AND_TOUCH)
	@echo ""
	@echo "-----------------------------------------------------"
	@echo "  variables input to makefile.platforms:"
	@echo "-----------------------------------------------------"
	@echo ""
	@echo MATURITY =============== $(MATURITY)
	@echo ""
	@echo "-----------------------------------------------------"
	@echo "  variables output from makefile.platforms:"
	@echo "-----------------------------------------------------"
	@echo ""
	@echo SUBTREE =============== $(SUBTREE)
	@echo LOCDIR ================ $(LOCDIR)
	@echo ""
	@echo ""
	@echo JAVAC ================== $(JAVAC)
	@echo JAR ==================== $(JAR)
	@echo ""
	@echo ""
	@echo "----------------------------------------------"
	@echo "  internal variables:"
	@echo "----------------------------------------------"
	@echo ""
	@echo CLASSPATH ============== $(CLASS_PATH)
	@echo SPSJARS ================ $(SPSJARS)
	@echo SYSJARS ================ $(SYSJARS)
	@echo ""
	@echo "----------------------------------------------"
	@echo "  end of list"
	@echo "----------------------------------------------"
	@echo ""

##-------------------------------- end -------------------------------------##
##-------------------------------- end -------------------------------------##
##-------------------------------- end -------------------------------------##

