# This Makefile builds both uni-threaded and multi-threaded versions
# of the libraries. It can be used as:
#	1. make install:	build the uni-threaded version
#	2. make install_mt:	build the multi-threaded version
#
# Written by Kiem-Phong Vo

LIBTYPE= -Dvt_threaded=0

INCDIR=	../../../include
LIBDIR=	../../../lib
BINDIR=	../../../bin

# Compiler and flags to use
CXFLAGS=
CCMODE=	-O
CCFLAGS= -I. $(CCMODE) $(LIBTYPE) $(CXFLAGS)
# AR=	ar
# CC=	cc

# sfio
HDRS=	sfio.h sfio_s.h sfio_t.h sfhdr.h vthread.h
SRCS=	sfclose.c sfclrlock.c sfcvt.c sfdisc.c sfdlen.c sfexcept.c \
	sfextern.c sffilbuf.c sfflsbuf.c sfprints.c sfgetd.c sfgetl.c \
	sfgetr.c sfgetu.c sfllen.c sfmode.c sfmove.c sfnew.c \
	sfnotify.c sfnputc.c sfopen.c sfpkrd.c sfpool.c sfpopen.c \
	sfprintf.c sfputd.c sfputl.c sfputr.c sfputu.c sfrd.c sfread.c \
	sfscanf.c sfseek.c sfset.c sfsetbuf.c sfsetfd.c sfsize.c sfsk.c \
	sfstack.c sfsync.c sftable.c sftell.c sftmp.c \
	sfungetc.c sfvprintf.c sfvscanf.c sfwr.c sfwrite.c sfexit.c \
	sfpurge.c sfpoll.c sfreserve.c sfswap.c sfraise.c sfmutex.c \
	sfgetm.c sfputm.c sfresize.c sffrexp.c \
	_sfclrerr.c _sfdlen.c _sfeof.c _sferror.c _sffileno.c _sfgetc.c \
	_sfgetl.c _sfgetl2.c _sfgetu.c _sfgetu2.c _sfllen.c _sfopen.c \
	_sfputc.c _sfputd.c _sfputl.c _sfputm.c _sfputu.c _sfslen.c \
	_sfstacked.c _sfulen.c _sfvalue.c
OBJS=	sfclose.o sfclrlock.o sfcvt.o sfdisc.o sfdlen.o sfexcept.o \
	sfextern.o sffilbuf.o sfflsbuf.o sfprints.o sfgetd.o sfgetl.o \
	sfgetr.o sfgetu.o sfllen.o sfmode.o sfmove.o sfnew.o \
	sfnotify.o sfnputc.o sfopen.o sfpkrd.o sfpool.o sfpopen.o \
	sfprintf.o sfputd.o sfputl.o sfputr.o sfputu.o sfrd.o sfread.o \
	sfscanf.o sfseek.o sfset.o sfsetbuf.o sfsetfd.o sfsize.o sfsk.o \
	sfstack.o sfsync.o sftable.o sftell.o sftmp.o \
	sfungetc.o sfvprintf.o sfvscanf.o sfwr.o sfwrite.o sfexit.o \
	sfpurge.o sfpoll.o sfreserve.o sfswap.o sfraise.o sfmutex.o \
	sfgetm.o sfputm.o sfresize.o sffrexp.o \
	_sfclrerr.o _sfdlen.o _sfeof.o _sferror.o _sffileno.o _sfgetc.o \
	_sfgetl.o _sfgetl2.o _sfgetu.o _sfgetu2.o _sfllen.o _sfopen.o \
	_sfputc.o _sfputd.o _sfputl.o _sfputm.o _sfputu.o _sfslen.o \
	_sfstacked.o _sfulen.o _sfvalue.o

# stdio-source compatibility code
STDIO_S=Stdio_s/stdfgetc.o Stdio_s/stdfprintf.o Stdio_s/stdfputc.o \
	Stdio_s/stdfscanf.o Stdio_s/stdgetc.o Stdio_s/stdgets.o \
	Stdio_s/stdgetw.o Stdio_s/stdfdopen.o Stdio_s/stdprintf.o \
	Stdio_s/stdputc.o Stdio_s/stdputw.o Stdio_s/stdscanf.o \
	Stdio_s/stdsprintf.o Stdio_s/stdsetvbuf.o Stdio_s/stdtmpfile.o \
	Stdio_s/stdfopen.o Stdio_s/stdpopen.o Stdio_s/stdfreopen.o \
	Stdio_s/stdfwrite.o Stdio_s/stdfread.o Stdio_s/stdfflush.o

# Thread-safety
MTSAFE=	../vthread/vtextern.o ../vthread/vtmtxclrlock.o \
	../vthread/vtmtxclose.o ../vthread/vtmtxlock.o \
	../vthread/vtmtxopen.o ../vthread/vtmtxtrylock.o \
	../vthread/vtmtxunlock.o ../vthread/vtonce.o

# IO disciplines
SFIO_DC=Sfio_dc/sfdcdio.o Sfio_dc/sfdcdos.o Sfio_dc/sfdcfilter.o \
	Sfio_dc/sfdclzw.o Sfio_dc/sfdcseekable.o Sfio_dc/sfdcslow.o \
	Sfio_dc/sfdcsubstream.o Sfio_dc/sfdctee.o Sfio_dc/sfdcunion.o

.c.o:
	$(CC) -c $(CCFLAGS) $*.c

# this target builds the uni-threaded version of Sfio
install:
	echo""; echo "BUILDING UNI-THREADED libsfio.a"
	$(SHELL) ./Makestate.sh unithreaded
	$(MAKE) LIBTYPE="-Dvt_threaded=0" SHELL="$(SHELL)" CC="$(CC)" \
		CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)" install_unithreaded

# this target builds the multi-threaded version of Sfio
install_mt: vthread
	echo""; echo "BUILDING MULTI-THREADED libsfio-mt.a";
	$(SHELL) ./Makestate.sh multithreaded
	$(MAKE) LIBTYPE="-Dvt_threaded=1" SHELL="$(SHELL)" CC="$(CC)" \
		CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)" install_multithreaded

# this target builds the Vthread library
vthread:
	cd ../vthread; $(MAKE) -f Makefile SHELL="$(SHELL)" CC="$(CC)" \
		CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)" install

install_unithreaded: libsfio.a libstdio.a
	cp ast_common.h sfio.h sfio_s.h sfio_t.h Stdio_s/stdio.h Sfio_dc/sfdisc.h \
		$(INCDIR)
	cp libsfio.a $(LIBDIR)
	-(ranlib $(LIBDIR)/libsfio.a; exit 0) >/dev/null 2>&1
	cp Stdio_b/libstdio.a $(LIBDIR)
	-(ranlib $(LIBDIR)/libstdio.a; exit 0) >/dev/null 2>&1

libsfio.a: FEATURES $(OBJS) mkstdio_s mksfio_dc
	$(AR) cr libsfio.a $(OBJS) $(STDIO_S) $(SFIO_DC)
	-(ranlib libsfio.a; exit 0) >/dev/null 2>&1

libstdio.a:
	cd Stdio_b; $(MAKE) -f Makefile LIBTYPE="$(LIBTYPE)" SHELL="$(SHELL)" \
		CC="$(CC)" CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)" libstdio.a

install_multithreaded: libsfio-mt.a libstdio-mt.a
	cp ast_common.h sfio.h sfio_s.h sfio_t.h Stdio_s/stdio.h Sfio_dc/sfdisc.h \
		$(INCDIR)
	cp libsfio-mt.a $(LIBDIR)
	-(ranlib $(LIBDIR)/libsfio-mt.a; exit 0) >/dev/null 2>&1
	cp Stdio_b/libstdio-mt.a $(LIBDIR)
	-(ranlib $(LIBDIR)/libstdio-mt.a; exit 0) >/dev/null 2>&1

libsfio-mt.a: FEATURES $(OBJS) mkstdio_s mksfio_dc
	$(AR) cr libsfio-mt.a $(OBJS) $(STDIO_S) $(SFIO_DC) $(MTSAFE)
	-(ranlib libsfio-mt.a; exit 0) >/dev/null 2>&1

libstdio-mt.a:
	cd Stdio_b; $(MAKE) -f Makefile LIBTYPE="$(LIBTYPE)" SHELL="$(SHELL)" \
		 CC="$(CC)" CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)" libstdio-mt.a

# the make goals below are common to both uni&multi-threaded versions
FEATURES: FEATURE/common FEATURE/float FEATURE/sfio FEATURE/iffeio \
	FEATURE/sfinit FEATURE/mmap
FEATURE/iffeio: FEATURE/common FEATURE/stdio.lcl
	$(BINDIR)/iffe set cc $(CC) $(CCFLAGS) : run features/iffeio
FEATURE/stdio.lcl: features/stdio_lcl
	$(BINDIR)/iffe - set cc $(CC) $(CCFLAGS) : run features/stdio_lcl > \
		FEATURE/stdio.lcl
FEATURE/common: features/common
	$(BINDIR)/iffe set cc $(CC) $(CCFLAGS) : run features/common
	-(rm ast_common.h; ln -s FEATURE/common ast_common.h; exit 0) >/dev/null 2>&1
FEATURE/float: features/float
	$(BINDIR)/iffe set cc $(CC) $(CCFLAGS) : run features/float
FEATURE/sfinit: features/sfinit.c
	$(BINDIR)/iffe set cc $(CC) $(CCFLAGS) : run features/sfinit.c
FEATURE/sfio: features/sfio
	$(BINDIR)/iffe set cc $(CC) $(CCFLAGS) : run features/sfio
FEATURE/mmap: features/mmap
	$(BINDIR)/iffe set cc $(CC) $(CCFLAGS) : run features/mmap
mkstdio_s:
	cd Stdio_s; $(MAKE) -f Makefile LIBTYPE="$(LIBTYPE)" SHELL="$(SHELL)" \
		CC="$(CC)" CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)"
mksfio_dc:
	cd Sfio_dc; $(MAKE) -f Makefile LIBTYPE="$(LIBTYPE)" SHELL="$(SHELL)" \
		CC="$(CC)" CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)"

3dupdate:
	cp Makefile $(SRCS) $(HDRS) ...
	cd features; cp common float iffeio sfinit.c sfio stdio stdio_lcl ...
	cd Stdio_s; $(MAKE) -f Makefile SHELL="$(SHELL)" 3dupdate
	cd Stdio_b; $(MAKE) -f Makefile SHELL="$(SHELL)" 3dupdate
	cd Sfio_dc; $(MAKE) -f Makefile SHELL="$(SHELL)" 3dupdate

clean:
	-(rm -rf FEATURE $(OBJS) ast_common.h Makefile.state; exit 0) >/dev/null 2>&1
	-(rm libsfio.a libstdio.a libsfio-mt.a libstdio-mt.a; exit 0) >/dev/null 2>&1
	cd Stdio_s; $(MAKE) -f Makefile SHELL="$(SHELL)" clean
	cd Stdio_b; $(MAKE) -f Makefile SHELL="$(SHELL)" clean
	cd Sfio_dc; $(MAKE) -f Makefile SHELL="$(SHELL)" clean
