#	libvthread.a makefile
#	Written by Kiem-Phong Vo

INCDIR=	../../../include
LIBDIR=	../../../lib
BINDIR=	../../../bin

CXFLAGS=
CCMODE= -O
CCFLAGS= $(CCMODE) -I. $(CXFLAGS)
# CC=cc

HDRS=	vthread.h vthdr.h
SRCS=	vtclose.c vtextern.c vtkill.c vtmtxclose.c vtmtxclrlock.c \
	vtmtxlock.c vtmtxopen.c vtmtxtrylock.c vtmtxunlock.c \
	vtonce.c vtopen.c vtrun.c vtself.c vtset.c vtwait.c
OBJS=	vtclose.o vtextern.o vtkill.o vtmtxclose.o vtmtxclrlock.o \
	vtmtxlock.o vtmtxopen.o vtmtxtrylock.o vtmtxunlock.o \
	vtonce.o vtopen.o vtrun.o vtself.o vtset.o vtwait.o

.c.o :
	$(CC) -c $(CCFLAGS) $*.c

all:	install

libvthread.a : ast_common.h FEATURE/vthread $(OBJS)
	ar cr libvthread.a $(OBJS)
	-(ranlib libvthread.a; exit 0) >/dev/null 2>&1

ast_common.h : features/common
	$(BINDIR)/iffe - set cc $(CC) $(CCMODE) $(CXFLAGS) : \
		run features/common > ast_common.h

FEATURE/vthread : features/vthread
	$(BINDIR)/iffe set cc $(CC) $(CCMODE) $(CXFLAGS) : run features/vthread

install : libvthread.a
	cp vthread.h ast_common.h $(INCDIR);
	cp libvthread.a $(LIBDIR);
	-(ranlib $(LIBDIR)/libvthread.a; exit 0) >/dev/null 2>&1

clean :
	-(rm ast_common.h $(OBJS) libvthread.a FEATURE/*; exit 0) >/dev/null 2>&1
