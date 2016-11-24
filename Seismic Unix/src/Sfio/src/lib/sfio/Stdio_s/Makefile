# makefile for stdio-source compatibility code
#
# Written by Kiem-Phong Vo

# Compiler and flags to use
# LIBTYPE=
# AR=	ar
# CC=	cc
CXFLAGS=
CCMODE=	-O
CCFLAGS= -I. -I.. $(CCMODE) $(LIBTYPE) $(CXFLAGS)

HDRS=	stdio.h
SRCS=	stdfgetc.c stdfprintf.c stdfputc.c stdfscanf.c stdgetc.c stdgets.c \
	stdgetw.c stdfdopen.c stdprintf.c stdputc.c stdputw.c stdscanf.c \
	stdsprintf.c stdsetvbuf.c stdfopen.c stdfreopen.c stdpopen.c stdtmpfile.c \
	stdfwrite.c stdfread.c stdfflush.c

OBJS=	stdfgetc.o stdfprintf.o stdfputc.o stdfscanf.o stdgetc.o stdgets.o \
	stdgetw.o stdfdopen.o stdprintf.o stdputc.o stdputw.o stdscanf.o \
	stdsprintf.o stdsetvbuf.o stdfopen.o stdfreopen.o stdpopen.o stdtmpfile.o \
	stdfwrite.o stdfread.o stdfflush.o

.c.o:
	$(CC) -c $(CCFLAGS) $*.c

must:	$(OBJS)

3dupdate:
	cp Makefile $(HDRS) $(SRCS) ...

clean:
	-(rm $(OBJS); exit 0) >/dev/null 2>&1
