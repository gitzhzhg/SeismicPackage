# Description file for SU includes
# Note that header.h and hdr.h are CREATED, not retrieved
#
# $Author: jkc $
# $Revision: 1.9 $ ; $Date: 88/05/15 06:37:27 $

include $(CWPROOT)/src/Makefile.config


INCLUDE = 	$I/segy.h	\
		$I/segd.h	\
		$I/hdr.h	\
		$I/header.h	\
		$I/su.h		\
		$I/tapesegy.h   \
		$I/tapebhdr.h  \
		$I/tapehdr.h   \
		$I/bheader.h   \
		$I/bhdr.h    \
		$I/su_xdr.h

INSTALL	: header.h hdr.h mkxdrhdrsub mkxdrbhdrsub $(INCLUDE) ../lib/xdrhdrsub.c ../lib/xdrbhdrsub.c 
	touch $@

header.h	: mkheader
	./mkheader >header.h
mkheader	: mkheader.c prehdr.h
	$(CC) $(CFLAGS) mkheader.c -o mkheader

hdr.h	:mkhdr
	./mkhdr >hdr.h

mkhdr	:mkhdr.c offsets.h prehdr.h
	$(CC) $(CFLAGS) mkhdr.c -o mkhdr

offsets.h	:mkoffs.sh segy.h
	$(CPP) $(CFLAGS) segy.h | ./mkoffs.sh >offsets.h
prehdr.h	:mkprehdr.sh segy.h
	$(CPP) $(CFLAGS) segy.h | ./mkprehdr.sh >prehdr.h

$(INCLUDE):	$(ITARGET)
	@-rm -f $@
	@cp $? $@
	@$(ICHMODLINE)
	@echo $(@F) installed in $I

mkxdrhdrsub	:hdr.h su.h mkxdrhdrsub.c
	$(CC) $(CFLAGS) mkxdrhdrsub.c -o mkxdrhdrsub

../lib/xdrhdrsub.c	:mkxdrhdrsub.c
	./mkxdrhdrsub > ../lib/xdrhdrsub.c

mkxdrbhdrsub	:bhdr.h su.h mkxdrbhdrsub.c
	$(CC) $(CFLAGS) mkxdrbhdrsub.c -o mkxdrbhdrsub

../lib/xdrbhdrsub.c	:mkxdrbhdrsub.c
	./mkxdrbhdrsub > ../lib/xdrbhdrsub.c

clean	::
	-rm -f mkheader mkhdr mkxdrhdrsub mkxdrbhdrsub offsets.h prehdr.h *.o core

remake	:
	@$(MAKE) clean
	@-touch *.h
	@-rm -f $(INCLUDE) ../lib/xdrbhdrsub.c ../lib/xdrhdrsub.c
	@$(MAKE) ROOT=$(ROOT) INSTALL
