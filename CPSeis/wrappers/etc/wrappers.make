##--------------------------- wrappers.make -------------------------------##
##--------------------------- wrappers.make -------------------------------##
##--------------------------- wrappers.make -------------------------------##

# This makefile for CPSEIS_WRAPPERS must be invoked in the relevant source
# code directory under CPSEIS_WRAPPERS/src.  Currently this includes the
# following directories:
#     CPSEIS_WRAPPERS/src/org/cpseis/util
#     CPSEIS_WRAPPERS/src/org/cpseis/wrappers
#     CPSEIS_WRAPPERS/src/org/cpseis/examples

#ifeq ($(strip $(CPSEIS_HOME)),)
#  $(error environment variable CPSEIS_HOME not set)
#endif

#ifeq ($(strip $(CPSEIS_WRAPPERS)),)
#  $(error environment variable CPSEIS_WRAPPERS not set)
#endif

ifeq ($(strip $(CPSEIS_ARCH)),)
  $(error environment variable CPSEIS_ARCH not set)
endif

ifeq ($(strip $(JAVA_HOME)),)
  $(error environment variable JAVA_HOME not set)
endif

### Need either LAM_DIR or MPICH_DIR depends on the subdirectory used.
#ifeq ($(strip $(LAM_DIR)),)
#  $(error environment variable LAM_DIR not set)
#endif

ifeq ($(strip $(FFTW_DIR)),)
  $(error environment variable FFTW_DIR not set)
endif

##------------------------------ init ------------------------------------##
##------------------------------ init ------------------------------------##
##------------------------------ init ------------------------------------##

CPSEIS_HOME     := $(shell fetch_cpseis_home)
CPSEIS_WRAPPERS := $(CPSEIS_HOME)/wrappers
CLASSPATH       := $(CPSEIS_WRAPPERS)/classes

export CPSEIS_HOME
export CPSEIS_WRAPPERS
export CLASSPATH

PACKAGE_SLASH      := $(shell fetch_package_name)
PACKAGE_DOT        := $(subst /,.,$(PACKAGE_SLASH))
PACKAGE_UNDERSCORE := $(subst /,_,$(PACKAGE_SLASH))

CLASS_DIR := $(CPSEIS_WRAPPERS)/classes/$(PACKAGE_SLASH)
OBJ_DIR   := $(CPSEIS_WRAPPERS)/platforms/$(CPSEIS_ARCH)/$(PACKAGE_SLASH)
LIB_DIR   := $(CPSEIS_WRAPPERS)/platforms/$(CPSEIS_ARCH)/lib
BIN_DIR   := $(CPSEIS_WRAPPERS)/platforms/$(CPSEIS_ARCH)/bin

LIBRARY_SO := $(LIB_DIR)/lib$(PACKAGE_UNDERSCORE)_library.so
LIBRARY_A  := $(LIB_DIR)/lib$(PACKAGE_UNDERSCORE)_library.a

##---------------------- source file variables -----------------------##
##---------------------- source file variables -----------------------##
##---------------------- source file variables -----------------------##

JAVA_FILES    := $(wildcard *.java)
SRC_FILES     := $(wildcard *.f90) $(wildcard *.c) $(wildcard *.cc) \
                 $(wildcard *.cpp) $(wildcard *.cxx) $(wildcard *.f)
SRC_UTILS     := $(filter-out $(USER_PROGRAMS), $(SRC_FILES))

# The following lines filter out all but the first instance of each file:
SRC_FILES     := $(sort $(SRC_FILES))
SRC_UTILS     := $(sort $(SRC_UTILS))
JAVA_FILES    := $(sort $(JAVA_FILES))

OBJ_FILES     := $(addsuffix .o    , $(basename $(SRC_FILES)))
OBJ_UTILS     := $(addsuffix .o    , $(basename $(SRC_UTILS)))
CLASS_FILES   := $(addsuffix .class, $(basename $(JAVA_FILES)))
PROGRAMS      := $(basename $(USER_PROGRAMS))

OBJ_FILES     := $(addprefix   $(OBJ_DIR)/, $(OBJ_FILES))
OBJ_UTILS     := $(addprefix   $(OBJ_DIR)/, $(OBJ_UTILS))
CLASS_FILES   := $(addprefix $(CLASS_DIR)/, $(CLASS_FILES))

##--------------------------- platform -----------------------------------##
##--------------------------- platform -----------------------------------##
##--------------------------- platform -----------------------------------##

# The following include file requires these variables:
#   LAM_DIR  JAVA_HOME  FFTW_DIR

include $(CPSEIS_WRAPPERS)/etc/makefile.$(CPSEIS_ARCH)

# The above include file defines these variables:
#   CC  CXX  F90  MOD_PREFIX  MOD_SUFFIX
#   MPI_INCS  JAVA_INCS  GUI_INCS
#   LD_SHARED  LD_STATIC  ARCHIVE
#   JAVA_LIBS  FFTW_LIBS  MPI_LIBS  GUI_LIBS
#   F90_LIBS  CXX_LIBS  MATH_LIBS  OTHER_LIBS

##----------------- includes and modules and libraries -------------------##
##----------------- includes and modules and libraries -------------------##
##----------------- includes and modules and libraries -------------------##

INC_DIRS := . $(CPSEIS_WRAPPERS)/src/org/cpseis/wrappers \
              $(CPSEIS_WRAPPERS)/src/org/cpseis/util \
              $(CPSEIS_HOME)/src \
              $(MPI_INCS) $(JAVA_INCS) $(GUI_INCS)

MOD_DIRS := $(OBJ_DIR) \
            $(CPSEIS_HOME)/platforms/$(CPSEIS_ARCH)

CPS_LIBS := -L$(CPSEIS_HOME)/platforms/$(CPSEIS_ARCH)/lib -lcps

ALL_LIBS := $(CPS_LIBS) $(JAVA_LIBS) $(FFTW_LIBS) $(MPI_LIBS) $(GUI_LIBS) \
            $(F90_LIBS) $(CXX_LIBS) $(MATH_LIBS) $(OTHER_LIBS)

ifeq ($(PACKAGE_SLASH),org/cpseis/wrappers)
#  ALL_LIBS := -L$(LIB_DIR) -lorg_cpseis_util_library
   ALL_LIBS := $(LIB_DIR)/liborg_cpseis_util_library.a $(ALL_LIBS)
endif

ifeq ($(PACKAGE_SLASH),org/cpseis/examples)
#  ALL_LIBS := -L$(LIB_DIR) -lorg_cpseis_wrappers_library $(ALL_LIBS)
   ALL_LIBS := $(LIB_DIR)/liborg_cpseis_wrappers_library.a \
               $(LIB_DIR)/liborg_cpseis_util_library.a $(ALL_LIBS)
endif

##-------------------------- general variables ---------------------------##
##-------------------------- general variables ---------------------------##
##-------------------------- general variables ---------------------------##

INC_FLAGS := $(patsubst %, -I%,            $(INC_DIRS))
MPI_FLAGS := $(patsubst %, -I%,            $(MPI_INCS))
MOD_FLAGS := $(patsubst %, $(MOD_PREFIX)%, $(MOD_DIRS))

CC  += $(INC_FLAGS)
CXX += $(INC_FLAGS)
F90 += $(MOD_FLAGS) $(MPI_FLAGS)

JAVAC := $(JAVA_HOME)/bin/javac -Xlint:unchecked -d $(CPSEIS_WRAPPERS)/classes -sourcepath $(CPSEIS_WRAPPERS)/src
JAVA  := $(JAVA_HOME)/bin/java -ea -Xcheck:jni -Djava.library.path=$(LD_LIBRARY_PATH)

##-------------------------- vpath -------------------------------##
##-------------------------- vpath -------------------------------##
##-------------------------- vpath -------------------------------##

vpath
vpath %.h             $(INC_DIRS)
vpath %.hh            $(INC_DIRS)
vpath %.hpp           $(INC_DIRS)
vpath %.hxx           $(INC_DIRS)
vpath %.o             $(OBJ_DIR)
vpath %.a             $(LIB_DIR)
vpath %.so            $(LIB_DIR)

##-------------------------- commands -------------------------------##
##-------------------------- commands -------------------------------##
##-------------------------- commands -------------------------------##

.PHONY: all build wrappers
.PHONY: run autorun pauserun
.PHONY: clean allclean
.PHONY: help list
.PHONY: java obj lib programs ldd depend html
.PHONY: javaclean objclean libclean progclean dependclean htmlclean autoclean

# Targets which make both the depend target and the lib target use separate make
# commands for each, because otherwise the dependencies are created too late to be
# used for compiling native code.

# Directory /tmp/$(HOST) is needed for cpstemp (for CPS modules) to work.

#all: java lib programs
all: java
	make lib
	make programs

build:
	make wrappers
	make java
	make depend
	make lib
	make programs
	@\mkdir -p /tmp/$(HOST)

##--------------------------- make wrappers -----------------------------##
##--------------------------- make wrappers -----------------------------##
##--------------------------- make wrappers -----------------------------##

ifeq ($(PACKAGE_SLASH),org/cpseis/wrappers)
wrappers:
	@echo ""
	@echo "BUILDING WRAPPERS  in  $(PACKAGE_DOT)"
	@$(CPSEIS_WRAPPERS)/scripts/make_cpseis_wrappers $(PACKAGE_DOT) $(PACKAGE_UNDERSCORE)
else
wrappers:
endif

##----------------------- make dependencies ----------------------------##
##----------------------- make dependencies ----------------------------##
##----------------------- make dependencies ----------------------------##

ifeq ($(SRC_FILES),)
depend:
else
depend:
	@echo ""
	@echo "CREATING DEPENDENCIES  in  $(PACKAGE_DOT)"
	@$(CPSEIS_WRAPPERS)/scripts/make_dependencies  "$(INC_FLAGS)"
endif

# Note that the file makefile.depend is not a target above, because
# any make command (even make clean) will attempt to make the file
# makefile.depend before doing anything else.  This is because
# the file makefile.depend is in an include statement.

-include makefile.depend

##----------------------- compile java code ---------------------------##
##----------------------- compile java code ---------------------------##
##----------------------- compile java code ---------------------------##

java: $(CLASS_DIR) $(CLASS_FILES)

$(CLASS_DIR):
	@\mkdir -p $(CLASS_DIR)

$(CLASS_FILES): $(JAVA_FILES)
	@echo ""
	@echo "COMPILING JAVA FILES  in  $(PACKAGE_DOT)"
	\rm -rf $(CLASS_DIR)
	$(JAVAC) $(JAVA_FILES)
	@$(CPSEIS_WRAPPERS)/scripts/make_jni_header_files . $(PACKAGE_DOT) $(PACKAGE_UNDERSCORE)

# Note: The above javac target also generates jni header files using javah.
# Note: The javah program requires the java files to have been compiled first.

##-------------------- compile native code --------------------------##
##-------------------- compile native code --------------------------##
##-------------------- compile native code --------------------------##

obj: $(OBJ_DIR) $(OBJ_FILES)

$(OBJ_DIR):
	@\mkdir -p $(OBJ_DIR)

$(OBJ_DIR)/%.o: %.c
	@echo ""
	@echo "COMPILING  $(notdir $<)  in  $(PACKAGE_DOT)"
	$(CC) $< -o $@

$(OBJ_DIR)/%.o: %.cpp
	@echo ""
	@echo "COMPILING  $(notdir $<)  in  $(PACKAGE_DOT)"
	$(CXX) $< -o $@

$(OBJ_DIR)/%.o: %.cxx
	@echo ""
	@echo "COMPILING  $(notdir $<)  in  $(PACKAGE_DOT)"
	$(CXX) $< -o $@

$(OBJ_DIR)/%.o: %.cc
	@echo ""
	@echo "COMPILING  $(notdir $<)  in  $(PACKAGE_DOT)"
	$(CXX) $< -o $@

$(OBJ_DIR)/%.o: %.f90
	@echo ""
	@echo "COMPILING  $(notdir $<)  in  $(PACKAGE_DOT)"
	$(F90) $< -o $@
	@-\mv *.$(MOD_SUFFIX) $(OBJ_DIR) >& /dev/null || echo >& /dev/null

$(OBJ_DIR)/%.o: %.f
	@echo ""
	@echo "COMPILING  $(notdir $<)  in  $(PACKAGE_DOT)"
	$(F90) $< -o $@
	@-\mv *.$(MOD_SUFFIX) $(OBJ_DIR) >& /dev/null || echo >& /dev/null

##----------------------- make libraries ----------------------------##
##----------------------- make libraries ----------------------------##
##----------------------- make libraries ----------------------------##

ifeq ($(SRC_UTILS),)
lib:
else
lib: $(OBJ_DIR) $(LIB_DIR) $(LIBRARY_A) $(LIBRARY_SO)
endif

$(LIB_DIR):
	@\mkdir -p $(LIB_DIR)

$(LIBRARY_A): $(OBJ_DIR) $(OBJ_UTILS) $(LIB_DIR)
	@echo ""
	@echo "BUILDING  $(notdir $@)  in  $(PACKAGE_DOT)"
	\rm -rf $(LIBRARY_A)
	$(ARCHIVE) $(LIBRARY_A) $(OBJ_UTILS)

$(LIBRARY_SO): $(OBJ_DIR) $(OBJ_UTILS) $(LIB_DIR)
	@echo ""
	@echo "BUILDING  $(notdir $@)  in  $(PACKAGE_DOT)"
	\rm -rf $(LIBRARY_SO)
	$(LD_SHARED) -o $(LIBRARY_SO) $(OBJ_UTILS) $(ALL_LIBS)
	@echo ""
	@echo "EXAMINING  $(notdir $@)  in  $(PACKAGE_DOT)"
	ldd -r $(LIBRARY_SO)

ldd:
	@echo ""
	@echo "EXAMINING  $(notdir $@)  in  $(PACKAGE_DOT)"
	ldd -r $(LIBRARY_SO)

##----------------------- make programs -----------------------------##
##----------------------- make programs -----------------------------##
##----------------------- make programs -----------------------------##

programs: $(OBJ_DIR) $(BIN_DIR) $(PROGRAMS)

$(BIN_DIR):
	@\mkdir -p $(BIN_DIR)

$(PROGRAMS): % : $(OBJ_DIR) $(BIN_DIR) $(OBJ_DIR)/%.o $(LIBRARY_A)
	@echo ""
	@echo "LINKING PROGRAM  $@  in  $(PACKAGE_DOT)"
	$(LD_STATIC) -o $@ $(OBJ_DIR)/$@.o $(LIBRARY_A) $(ALL_LIBS)
	@echo ""
	@echo "EXAMINING PROGRAM  $@  in  $(PACKAGE_DOT)"
	ldd -r $@
	\cp -f $@ $(BIN_DIR)

##------------------------ misc commands ----------------------------##
##------------------------ misc commands ----------------------------##
##------------------------ misc commands ----------------------------##

html:
	@echo ""
	@echo "CREATING JAVADOC HTML  in  $(PACKAGE_DOT)"
	@\mkdir -p html
	@$(CPSEIS_WRAPPERS)/scripts/make_javadoc_html

run:
	@echo ""
	@echo "RUNNING TEST PROGRAMS  in  $(PACKAGE_DOT)"
	@$(CPSEIS_WRAPPERS)/scripts/run_java_programs   "$(JAVA_FILES)" "$(JAVA)"
	@$(CPSEIS_WRAPPERS)/scripts/run_native_programs "$(PROGRAMS) $(USER_SCRIPTS)"

autorun:
	@echo ""
	@echo "RUNNING TEST PROGRAMS  in  $(PACKAGE_DOT)"
	@$(CPSEIS_WRAPPERS)/scripts/run_java_programs   "$(JAVA_FILES)" "$(JAVA)" AUTO
	@$(CPSEIS_WRAPPERS)/scripts/run_native_programs "$(PROGRAMS) $(USER_SCRIPTS)" AUTO

pauserun:
	@echo ""
	@echo "RUNNING TEST PROGRAMS  in  $(PACKAGE_DOT)"
	@$(CPSEIS_WRAPPERS)/scripts/run_java_programs   "$(JAVA_FILES)" "$(JAVA)" PAUSE
	@$(CPSEIS_WRAPPERS)/scripts/run_native_programs "$(PROGRAMS) $(USER_SCRIPTS)" PAUSE

##---------------------------- clean --------------------------------##
##---------------------------- clean --------------------------------##
##---------------------------- clean --------------------------------##

allclean:: clean htmlclean dependclean autoclean

clean:: javaclean progclean objclean
	\rm -f %HISTORY_RECORDS

javaclean:
	\rm -rf $(CLASS_DIR)

progclean:
	\rm -f $(PROGRAMS)
	\rm -f $(addprefix $(BIN_DIR)/, $(PROGRAMS))

objclean: libclean
	\rm -rf $(OBJ_DIR)

libclean:
	\rm -f $(LIBRARY_SO)
	\rm -f $(LIBRARY_A)

htmlclean:
	\rm -rf html

dependclean:
	\rm -f makefile.depend

autoclean:
	@$(CPSEIS_WRAPPERS)/scripts/clean_autogen_files

##-------------------------- list -------------------------------##
##-------------------------- list -------------------------------##
##-------------------------- list -------------------------------##

list:
	@echo
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "CPSEIS_HOME ============ $(CPSEIS_HOME)"
	@echo "CPSEIS_WRAPPERS ======== $(CPSEIS_WRAPPERS)"
	@echo "JAVA_HOME ============== $(JAVA_HOME)"
	@echo "CPSEIS_ARCH ============ $(CPSEIS_ARCH)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "CURDIR ================= $(CURDIR)"
	@echo "PWD ==================== $(PWD)"
	@echo "shell pwd ============== $(shell pwd)"
	@echo "PACKAGE_SLASH ========== $(PACKAGE_SLASH)"
	@echo "PACKAGE_DOT ============ $(PACKAGE_DOT)"
	@echo "PACKAGE_UNDERSCORE ===== $(PACKAGE_UNDERSCORE)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "CLASS_DIR ============== $(CLASS_DIR)"
	@echo "OBJ_DIR ================ $(OBJ_DIR)"
	@echo "LIB_DIR ================ $(LIB_DIR)"
	@echo "BIN_DIR ================ $(BIN_DIR)"
	@echo "LIBRARY_SO ============= $(LIBRARY_SO)"
	@echo "LIBRARY_A ============== $(LIBRARY_A)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo
	@echo "---> press enter to continue"
	@read xxx
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "CPS_LIBS =============== $(CPS_LIBS)"
	@echo "JAVA_LIBS ============== $(JAVA_LIBS)"
	@echo "FFTW_LIBS ============== $(FFTW_LIBS)"
	@echo "MPI_LIBS =============== $(MPI_LIBS)"
	@echo "GUI_LIBS =============== $(GUI_LIBS)"
	@echo "F90_LIBS =============== $(F90_LIBS)"
	@echo "CXX_LIBS =============== $(CXX_LIBS)"
	@echo "MATH_LIBS ============== $(MATH_LIBS)"
	@echo "OTHER_LIBS ============= $(OTHER_LIBS)"
	@print_variable ALL_LIBS   "$(ALL_LIBS)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@print_variable INC_DIRS   "$(INC_DIRS)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@print_variable MOD_DIRS   "$(MOD_DIRS)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo
	@echo "---> press enter to continue"
	@read xxx
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "JAVAC ================== $(JAVAC)"
	@echo "JAVA =================== $(JAVA)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "F90 ==================== $(F90)"
	@echo "MOD_PREFIX ============= $(MOD_PREFIX)"
	@echo "MOD_SUFFIX ============= $(MOD_SUFFIX)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "CC ===================== $(CC)"
	@echo "CXX ==================== $(CXX)"
	@echo "LD_SHARED ============== $(LD_SHARED)"
	@echo "LD_STATIC ============== $(LD_STATIC)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo
	@echo "---> press enter to continue"
	@read xxx
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@print_path CLASSPATH 6
	@print_path PATH 29
	@print_path LD_LIBRARY_PATH 29
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo
	@echo "---> press enter to continue"
	@read xxx
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "JAVA_FILES ============= $(JAVA_FILES)"
	@echo "CLASS_FILES ====notdir== $(notdir $(CLASS_FILES))"
	@echo "SRC_FILES ============== $(SRC_FILES)"
	@echo "OBJ_FILES ======notdir== $(notdir $(OBJ_FILES))"
	@echo "SRC_UTILS ============== $(SRC_UTILS)"
	@echo "OBJ_UTILS ======notdir== $(notdir $(OBJ_UTILS))"
	@echo "USER_PROGRAMS ========== $(USER_PROGRAMS)"
	@echo "PROGRAMS =============== $(PROGRAMS)"
	@echo "USER_SCRIPTS =========== $(USER_SCRIPTS)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo

##-------------------------- help -------------------------------##
##-------------------------- help -------------------------------##
##-------------------------- help -------------------------------##

help:
	@echo
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "Include this makefile in your Makefile:"
	@echo "   include $(CPSEIS_WRAPPERS)/etc/cpseis_wrappers.make"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "  make             (make java + lib)"
	@echo "  make all         (make java + lib)"
	@echo "  make build       (make java + depend + wrappers + lib)"
	@echo "  make java        (make class files and some auto-generated files)"
	@echo "  make wrappers    (make auto-generated wrapper files) (only in wrappers directory)"
	@echo "  make obj         (make object code)"
	@echo "  make lib         (make object code and shared library and run ldd on it)"
	@echo "  make programs    (make native main programs and run ldd on them)"
	@echo "  make ldd         (run ldd on shared library)"
	@echo "  make depend      (put C, C++, and Fortran90 dependencies into makefile.depend)"
	@echo "  make html        (make local javadoc html files so you can check them out)"
	@echo "  make pauserun    (run all java and native programs and scripts - with many prompts)"
	@echo "  make run         (run all java and native programs and scripts - with some prompts)"
	@echo "  make autorun     (run all java and native programs and scripts - without prompts)"
	@echo
	@echo "  make clean       (make javaclean + objclean + libclean + progclean)"
	@echo "  make allclean    (make clean + dependclean + autoclean + htmlclean)"
	@echo "  make javaclean   (remove java class files)"
	@echo "  make autoclean   (remove auto-generated files)"
	@echo "  make objclean    (remove object code)"
	@echo "  make libclean    (remove shared library)"
	@echo "  make progclean   (remove native main programs)"
	@echo "  make dependclean (remove makefile.depend)"
	@echo "  make htmlclean   (remove local javadoc html files)"
	@echo
	@echo "  make help        (list this documentation)"
	@echo "  make list        (list makefile variables)"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "The shared library name will be created from the package name as follows:"
	@echo "     package name:       org.cpseis.util"
	@echo "     library name:    liborg_cpseis_util_library.so"
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "Redefining the following targets (with a double colon) will add to the previous commands:"
	@echo "                     clean  allclean"
	@echo "Redefining any other targets (with a single colon) will replace the previous commands."
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo "Set USER_PROGRAMS to a list of source files for any native main programs you have."
	@echo "Set USER_SCRIPTS  to a list of any scripts you want to run."
	@echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
	@echo

##---------------------------- end ---------------------------------##
##---------------------------- end ---------------------------------##
##---------------------------- end ---------------------------------##
