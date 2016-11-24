# Just say "make"

include $(CWPROOT)/src/Makefile.config
include ../Makefile.bayes

J = $(IBAYES)

INCLUDE =	 		\
	$(IBAYES)/covAdap.h	\
	$(IBAYES)/dataCovar.h	\
	$(IBAYES)/posteriori.h	\
	$(IBAYES)/posterioriMain.h	\
	$(IBAYES)/definePosteriori.h	\
	$(IBAYES)/externPosteriori.h	\
	$(IBAYES)/globalPosteriori.h	\
	$(IBAYES)/PVMLib.h		\
	$(IBAYES)/defineStratInv.h	\
	$(IBAYES)/externStratInv.h	\
	$(IBAYES)/globalStratInv.h	\
	$(IBAYES)/protocolStratInv.h	\
	$(IBAYES)/stratInv.h		\
	$(IBAYES)/stratInvMain.h	\
	$(IBAYES)/modSlave.h		\
	$(IBAYES)/frechetSlave.h	\
	$(IBAYES)/extModSinv.h		\
	$(IBAYES)/extFrechetSinv.h	\
	$(IBAYES)/refMaster.h		\
	$(IBAYES)/refSlave.h		\
	$(IBAYES)/protocolRef.h		\
	$(IBAYES)/globalRef.h		\
	$(IBAYES)/PVMLibRef.h		\
	$(IBAYES)/defineRef.h		\
	$(IBAYES)/ranlib.h		\
	$(IBAYES)/sample.h

INSTALL	: $J $(INCLUDE)
	touch $@

$(INCLUDE):	$(IBTARGET)
	@-rm -f $@
	@cp $? $@
	@chmod 664 $@
	@echo $(@F) installed in $J

remake	:
	@-rm -f $(INCLUDE)
	@$(MAKE)

$J	:
	@-mkdir $J

