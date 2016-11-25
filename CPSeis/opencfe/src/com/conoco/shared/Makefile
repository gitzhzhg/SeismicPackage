SRCDIR   = ../
INC1DIR  = ${SRCDIR}
OBJ1DIR  = ${SRCDIR}/${PLATFORM}
LKLIBS   = -lX11

CC     = gcc
CFL    =
ifeq "$(MACHINE_ARCH)" "x86_64"
	CFL=-m64
	LK     = gcc -fPIC $(CFL) -shared -L/usr/X11R6/lib64
endif
ifeq "$(MACHINE_ARCH)" "i686"
	CFL=-m32
	LK     = gcc -fPIC $(CFL) -shared -L/usr/X11R6/lib
endif
CFLAGS = -c -g $(CFL) -fPIC -Wall -DLINUX -DLINUXI -DNEED_UNDERSCORE \
	-I${INC1DIR} -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux 

vpath
vpath %.cc ${SRCDIR}
vpath %.c ${SRCDIR}
vpath %.h ${SRCDIR}
vpath %.h ${INC1DIR}
vpath %.o ${OBJ1DIR}

all: libXClipboard.so

libXClipboard.so:                   XClipboard.o
	${LK} -o libXClipboard.so XClipboard.o ${LKLIBS}
	ldd -r libXClipboard.so

XClipboard.o:          XClipboard.c
	${CC} ${CFLAGS}  ${SRCDIR}/XClipboard.c 

clean:
	rm -f *.o *.so

install:
	cp libXClipboard.so \
	$(CPSEIS_INSTALL_DIR)/platforms/$(CPSEIS_ARCH)/lib/libXClipboard.so

list:
	@echo PLATFORM ==================== $(PLATFORM)
	@echo SRCDIR ====================== $(SRCDIR)
	@echo CFLAGS ====================== $(CFLAGS)
