# Make file for the main jar for opencfe 

BASEDIR = $(CPSEIS_INSTALL_DIR)/opencfe

SHELL    = /bin/bash
JHOME    = $(JAVA_HOME)
PATH       = .:${JHOME}/bin:/usr/app/bin:/bin:/usr/bin
JSERV_JARS = $(BASEDIR)/jars
HERE       = ${PWD}

CLASSPATH = ${HERE}:${JHOME}/jre/lib:${JSERV_JARS}/xml-apis.jar:${JSERV_JARS}/resolver.jar:${JSERV_JARS}/serializer.jar:${JSERV_JARS}/xercesImpl.jar:${JSERV_JARS}/jsdk.jar
JAVAC     = ${JHOME}/bin/javac -O  -Xlint:deprecation


all: dirs workarounds xml misc gui controls clientserver application jar libopencfe.so xmlviewer libXClipboard.so

dirs:
	$(CPSEIS_INSTALL_DIR)/scripts/create_directories $(CPSEIS_ARCH)

jar:
	jar -cvf opencfe.jar                  \
		com/conoco/*/*.class               \
		com/conoco/*/*/*.class             \
		com/conoco/*/*/*/*.class           \
		com/conoco/*/*/*/*/*.class         \
		com/conoco/*/*/*/*/*/*.class

xmlviewer:
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/xmlviewer; ${JAVAC} *.java )

workarounds:
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/workarounds/javax/swing/text; ${JAVAC} *.java )

application:
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/application; ${JAVAC} *.java )

xml:
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/xml; ${JAVAC} *.java )

misc:
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/shared; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/utils; ${JAVAC} *.java )

gui:
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui/builder; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui/controller; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui/controller/actions; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui/messaging; ${JAVAC} *.java )

controls:
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui/controls; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui/controls/table; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui/controls/text; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/gui/controls/ui; ${JAVAC} *.java )

clientserver:
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/client/messaging; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/server/messaging; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/interfaces; ${JAVAC} *.java )
	(export CLASSPATH=${CLASSPATH}; cd com/conoco/cfe/servlet; ${JAVAC} *.java )

libopencfe.so:
	(cd ${BASEDIR}/src/jni/${CPSEIS_ARCH}; make -f ../Makefile )

libXClipboard.so:
	(cd com/conoco/shared/${CPSEIS_ARCH}; make -f ../Makefile )

clean:
	rm -f opencfe.jar
	(cd com/conoco/cfe/workarounds/javax/swing/text; rm -f *.class )
	(cd com/conoco/shared; rm -f *.class )
	(cd com/conoco/cfe/client/gui/controls/table; rm -f *.class )
	(cd com/conoco/cfe/client/application; rm -f *.class )
	(cd com/conoco/xml; rm -f *.class )
	(cd com/conoco/cfe/utils; rm -f *.class )
	(cd com/conoco/cfe/client; rm -f *.class )
	(cd com/conoco/cfe/client/gui; rm -f *.class )
	(cd com/conoco/cfe/client/gui/builder; rm -f *.class )
	(cd com/conoco/cfe/client/gui/controller; rm -f *.class )
	(cd com/conoco/cfe/client/gui/controller/actions; rm -f *.class )
	(cd com/conoco/cfe/client/gui/controls; rm -f *.class )
	(cd com/conoco/cfe/client/gui/controls/text; rm -f *.class )
	(cd com/conoco/cfe/client/gui/controls/ui; rm -f *.class )
	(cd com/conoco/cfe/client/gui/messaging; rm -f *.class )
	(cd com/conoco/cfe/client/messaging; rm -f *.class )
	(cd com/conoco/cfe/interfaces; rm -f *.class )
	(cd com/conoco/cfe/server/messaging; rm -f *.class )
	(cd com/conoco/cfe/servlet; rm -f *.class )
	(cd com/conoco/xmlviewer; rm -f *.class )
	(cd com/conoco/shared/${CPSEIS_ARCH}; make -f ../Makefile clean )
	(cd ${BASEDIR}/src/jni/${CPSEIS_ARCH}; make -f ../Makefile clean )

install:
	cp opencfe.jar $(BASEDIR)/jars/opencfe.jar
	(cd ${BASEDIR}/src/jni/${CPSEIS_ARCH}; make -f ../Makefile install )
	(cd com/conoco/shared/${CPSEIS_ARCH}; make -f ../Makefile install )

list:
	@echo JAVAC ==================================== $(JAVAC)
	@echo JHOME ==================================== $(JHOME)
	@echo ""
	@echo CLASSPATH ================================ $(CLASSPATH)
