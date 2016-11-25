###################### makefile in ~spws/making #############################
###################### makefile in ~spws/making #############################
###################### makefile in ~spws/making #############################

   # $Id: makefile,v 1.2 2004/03/04 14:07:43 wjdone Exp $

   # This makefile can be executed in any platform-specific subdirectory
   # under the ~spws/making directory to make any or all ~spws libraries or
   # programs in their respective subdirectories for the matching platform.
   # (HOST_TYPE is not needed.)
   #
   # Environment variable SPWSDIR is used to point to the ~spws directories.
   #
   # gmake                  makes all util and oop libraries and sys jars.
   # gmake all              makes all util and oop libraries and sys jars.
   # gmake util             makes all util libraries.
   # gmake oop              makes all oop libraries.
   # gmake programs         makes all programs.
   # gmake java             makes all sys jars from port *.java files.
   #
   # gmake depend           makes dependencies for all util and oop libraries.
   # gmake depend_util      makes dependencies for all util libraries.
   # gmake depend_oop       makes dependencies for all oop libraries.
   # gmake depend_programs  makes dependencies for all programs.
   #
   # gmake clean            cleans object code for all util and oop libraries.
   # gmake clean_util       cleans object code for all util libraries.
   # gmake clean_oop        cleans object code for all oop libraries.
   # gmake clean_programs   cleans object code for all programs.
   # gmake clean_classes    cleans class code for all sys *.class files.
   #
   # gmake xxxx             makes the specified library or program or jar,
   #                         where xxxx is the subdirectory name under util
   #                         or oop or programs or port.


########################### get started #####################################
########################### get started #####################################
########################### get started #####################################


ifeq ($(strip $(SPWSDIR)),)
  $(error environment variable SPWSDIR not set)
endif

ifeq ($(strip $(HOST)),)
  $(error environment variable HOST not set)
endif


PLATFORM = $(notdir $(PWD))


UTIL_TARGETS     = $(notdir $(wildcard $(SPWSDIR)/util/*))
OOP_TARGETS      = $(notdir $(wildcard $(SPWSDIR)/oop/*))
PROGRAMS_TARGETS = $(notdir $(wildcard $(SPWSDIR)/programs/*))
JAVA_TARGETS     = $(notdir $(wildcard $(SPWSDIR)/port/com/conocophillips/*))
CLASSES_TARGETS  = $(notdir $(wildcard $(SPWSDIR)/sys/com/conocophillips/classes/*))


SKIP = obsolete_or_reference cvmedit slave_dpy

UTIL_TARGETS     := $(filter-out $(SKIP), $(UTIL_TARGETS))
OOP_TARGETS      := $(filter-out $(SKIP), $(OOP_TARGETS))
PROGRAMS_TARGETS := $(filter-out $(SKIP), $(PROGRAMS_TARGETS))
JAVA_TARGETS     := $(filter-out $(SKIP), $(JAVA_TARGETS))


all      : $(UTIL_TARGETS) $(OOP_TARGETS) $(CLASSES_TARGETS)
util     : $(UTIL_TARGETS)
oop      : $(OOP_TARGETS)
programs : $(PROGRAMS_TARGETS)
java     : $(JAVA_TARGETS)
classes  : $(CLASSES_TARGETS)
none     :


########################### depend targets #################################
########################### depend targets #################################
########################### depend targets #################################


depend_util : none
	@echo ""
	@for target in $(UTIL_TARGETS); do \
          echo "--------------- building makefile.depend for  $$target --------------"; \
	  (cd $(SPWSDIR)/util/$$target/$(PLATFORM); \
                          touch ../makefile.depend; gmake depend); \
          echo ""; \
	done

depend_oop : none
	@echo ""
	@for target in $(OOP_TARGETS); do \
          echo "--------------- building makefile.depend for  $$target --------------"; \
	  (cd $(SPWSDIR)/oop/$$target/$(PLATFORM); \
                          touch ../makefile.depend; gmake depend); \
          echo ""; \
	done

depend_programs : none
	@echo ""
	@for target in $(PROGRAMS_TARGETS); do \
          echo "--------------- building makefile.depend for  $$target --------------"; \
	  (cd $(SPWSDIR)/programs/$$target/$(PLATFORM); \
                          touch ../makefile.depend; gmake depend); \
          echo ""; \
	done

depend : depend_util depend_oop depend_programs


############################# main targets ###################################
############################# main targets ###################################
############################# main targets ###################################


$(UTIL_TARGETS) : none
	@echo ""
	@echo ""
	@echo "----------------- making  util/$@ ---------------------";
	@$(MAKE) -C $(SPWSDIR)/util/$@/$(PLATFORM)

$(OOP_TARGETS) : none
	@echo ""
	@echo ""
	@echo "----------------- making  oop/$@ ---------------------";
	@$(MAKE) -C $(SPWSDIR)/oop/$@/$(PLATFORM)

$(PROGRAMS_TARGETS) : none
	@echo ""
	@echo ""
	@if [ -d $(SPWSDIR)/programs/$@/$(PLATFORM) ] ; then \
	echo "----------------- making  programs/$@ ---------------------"; \
	$(MAKE) -C $(SPWSDIR)/programs/$@/$(PLATFORM) ; \
	fi

$(JAVA_TARGETS) : none
	@echo ""
	@echo ""
	@if [ -d $(SPWSDIR)/port/com/conocophillips/$@ ] ; then \
	echo "--------- making  sys/com/conocophillips/classes/$@ ---------"; \
	$(MAKE) -C $(SPWSDIR)/port/com/conocophillips/$@ ; \
	fi


####### similar alternatives:
#######
####### $(MAKE) -C $(SPWSDIR)/oop/$@/$(PLATFORM)
#######  gmake  -C $(SPWSDIR)/oop/$@/$(PLATFORM)
#######        (cd $(SPWSDIR)/oop/$@/$(PLATFORM);   $(MAKE))
#######        (cd $(SPWSDIR)/oop/$@/$(PLATFORM) && $(MAKE))
#######        (cd $(SPWSDIR)/oop/$@/$(PLATFORM);    gmake )
#######        (cd $(SPWSDIR)/oop/$@/$(PLATFORM) &&  gmake )
#######
####### using $(MAKE) is preferable to gmake.
####### && means execute gmake only if the previous command was successful.


############################ clean targets #################################
############################ clean targets #################################
############################ clean targets #################################


clean_util : none
	@echo ""
	@for target in $(UTIL_TARGETS); do \
          echo "-------------- deleting objects for  $$target -------------"; \
	  (cd $(SPWSDIR)/util/$$target/$(PLATFORM); /bin/rm *.o); \
          echo ""; \
	done

clean_oop : none
	@echo ""
	@for target in $(OOP_TARGETS); do \
          echo "-------------- deleting objects for  $$target -------------"; \
	  (cd $(SPWSDIR)/oop/$$target/$(PLATFORM); /bin/rm *.o); \
          echo ""; \
	done

clean_programs : none
	@echo ""
	@for target in $(PROGRAMS_TARGETS); do \
          echo "-------------- deleting objects for  $$target -------------"; \
	  (cd $(SPWSDIR)/programs/$$target/$(PLATFORM); /bin/rm *.o); \
          echo ""; \
	done

clean_classes : none
	@echo ""
	@for target in $(CLASSES_TARGETS); do \
          echo "-------------- deleting classes for  $$target -------------"; \
	  (cd $(SPWSDIR)/sys/com/conocophillips/classes/$$target; /bin/rm *.class); \
          echo ""; \
	done

clean : clean_util clean_oop clean_classes


######################### end of makefile ##################################
######################### end of makefile ##################################
######################### end of makefile ##################################

