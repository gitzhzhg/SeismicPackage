##---------------------- subdirectories.make --------------------------##
##---------------------- subdirectories.make --------------------------##
##---------------------- subdirectories.make --------------------------##

# Include this makefile in your Makefile in any directories which must call
# makefiles in subdirectories.  For example:
#
#    SUBDIRS := aaaa bbbb/dev cccc/dev dddd/beta
#    include $(CPSEIS_WRAPPERS)/etc/subdirectories.make
#
# The variable SUBDIRS must be set to a list of the subdirectories to be made.
#
# The list of subdirectories must be in the correct order for the makes to
# occur, in case some subdirectories must be made before others are made.
#
# This makefile contains only a few commonly-used targets which must also
# exist in the makefiles which are called from this makefile.

##---------------------- how this makefile works -----------------------##
##---------------------- how this makefile works -----------------------##
##---------------------- how this makefile works -----------------------##

# If you type "make dddddd", where "dddddd" is one of the directories listed
# in SUBDIRS, then the SUBDIRS target will be used to make the directory by
# going to that directory and executing "make" with no arguments.
#
# If you type "make" with no arguments, then the "all" target is used,
# which in turn makes all the directories listed in SUBDIRS using the
# SUBDIR target for each directory listed, by going to each directory
# and executing "make" with no arguments.
#
# If you type "make tttttt", where "tttttt" is any of the targets explicitly
# listed below, then that target is made first, by executing the command which
# says to invoke "make" again with no target specified, and with the specified
# SUBTARGET overriding the "SUBTARGET :=" line in this makefile.  The
# "all" target will then be used, which in turn makes all the directories
# listed in SUBDIRS using the SUBTARGET target for each directory listed,
# by going to each directory and executing "make" with the target SUBTARGET
# (your original target).
#
# If you type "make dddddd SUBTARGET=tttttt", where "dddddd" is one of the
# directories listed in SUBDIRS, and where "tttttt" is any of the targets
# explicitly listed below, then the SUBDIRS target will be used to make the
# directory by going to that directory and executing "make" with your original
# target, which overrides the "SUBTARGET :=" line in this makefile,

##-------------------------- makefile contents ---------------------------##
##-------------------------- makefile contents ---------------------------##
##-------------------------- makefile contents ---------------------------##

SUBTARGET :=

.PHONY: $(SUBDIRS)

all: $(SUBDIRS) 

$(SUBDIRS):
	$(MAKE) -C $@ $(SUBTARGET)

build:
	$(MAKE) SUBTARGET=build 

clean:
	$(MAKE) SUBTARGET=clean 

libclean:
	$(MAKE) SUBTARGET=libclean 

allclean:
	$(MAKE) SUBTARGET=allclean 

ldd:
	$(MAKE) SUBTARGET=ldd 

run:
	$(MAKE) SUBTARGET=run 

##-------------------------------- end ----------------------------------##
##-------------------------------- end ----------------------------------##
##-------------------------------- end ----------------------------------##
