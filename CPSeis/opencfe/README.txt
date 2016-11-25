#!/bin/bash
# cfe components from cfebeta script:

CLIENT_HOME=/d/sipdocs/htdocs/SaHPC/projects/CPSeis/int/int


CLASSPATH=$CLIENT_HOME/jars/conocobeta.jar:$CLIENT_HOME/jars/xml4j.jar
LD_LIBRARY_PATH=/home/sps/lib/linuxab80:$CLIENT_HOME/lib/jdk14/ix86-intel-linux/beta
LD_ASSUME_KERNEL=2.4.0

JAVA_HOME=/home/sps/java/linux/jdk
JAVA_OPTS="-ms64m -mx64m -ss256k -Djava.util.prefs.syncInterval=2000000"
JAVA=$JAVA_HOME/bin/java


APP  = com.conoco.cfe.client.application.DirectApplication
PREFSFILE = file:///home/mengewm/web/projects/CPSeis/prefs.xml.direct.file


echo "+-----------------------------------------------------------+"
echo "| This is the test version of cfe that is going open source |"
echo "+-----------------------------------------------------------+"
sleep 2


$JAVA $JAVA_OPTS $APP $PREFSFILE >>./rmi.$$ 2>&1
