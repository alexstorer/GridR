#!/bin/sh
export BROKER_LOCATION=.
CLASSPATH=
for i in $BROKER_LOCATION/lib/*.jar;do CLASSPATH=$i:$CLASSPATH; done
for i in $BROKER_LOCATION/*.jar;do CLASSPATH=$i:$CLASSPATH; done
java -Xmx512m -cp ${CLASSPATH} de.fhg.iais.kd.djm.ServerStartup $1
