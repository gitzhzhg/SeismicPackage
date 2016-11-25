SRCDIR   = ../
CC     = gcc
CPP    = g++
CFL    =
ifeq "$(MACHINE_ARCH)" "x86_64"
	CFL=-m64
endif
ifeq "$(MACHINE_ARCH)" "i686"
	CFL=-m32
endif
CFLAGS = $(CFL) -c -g -fPIC -Wall -DLINUXI -DNEED_UNDERSCORE \
	-I${SRCDIR} -I$(CPSEIS_INSTALL_DIR)/include \
	-I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux
LK     = gcc $(CFL) -fPIC -shared -ldl \
	-L/usr/lib/gcc-lib/i386-redhat-linux/3.2.3 -lsupc++ -lstdc++

vpath
vpath %.cpp ${SRCDIR}
vpath %.c   ${SRCDIR}
vpath %.h   ${SRCDIR}

libopencfe.so: JNICfeApi.o JNICfeApiHelper.o set.o
	${LK} -o libopencfe.so $^
	ldd -r libopencfe.so

.cpp.o:
	${CPP} ${CFLAGS} $<

.c.o:
	${CC} ${CFLAGS} $<

install:
	cp libopencfe.so \
	$(CPSEIS_INSTALL_DIR)/platforms/$(CPSEIS_ARCH)/lib/libopencfe.so

clean:
	-rm -f *.o *.so
