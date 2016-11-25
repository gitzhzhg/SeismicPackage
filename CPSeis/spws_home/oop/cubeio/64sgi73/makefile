
     # makefile_for_spws_libraries in ~spws/making

     # $Id: makefile_for_spws_libraries,v 1.2 2004/03/04 14:07:43 wjdone Exp $

     # This makefile can be used by any ~spws directory
     # which makes ~spws libraries.  Any such ~spws directory
     # need only have a link to this makefile.  For example,
     # you can type the following in the source code directory
     # (under oop or util) for any library:
     #   ln -s  $SPWSMAKEDIR/makefile_for_spws_libraries  makefile


LINKMODE       = spws
#######MATURITY       = production
MATURITY       = beta
MODPUT         = $(SPWSDIR)/modules/$(PLATFORM)
TARGET_LIBRARY = $(SPWSDIR)/spws/lib/$(PLATFORM)/lib$(PARENT).a
EXCLUDE        = rutil.f

-include ../makefile.special

include $(SPWSMAKEDIR)/makefile.spws

