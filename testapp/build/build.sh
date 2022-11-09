#!/bin/ksh
echo "Building MPH Test Application"
#Setup the environment
PATH=$PATH:/bin:/sbin:/usr/bin
. ./ant.properties

#JAVA_HOME=/vob/thirdParty/java/jdk1.6.0_17
JAVA_HOME=/usr/jdk1.7.0_76

if [ $# -ne 1 ]
then
    echo "${bold}Usage: build.sh [CAS-INSTALLROOT]${offbold}"
    exit 1
fi
        
#echo "Using JAVA_HOME="${JAVA_HOME}
export CASINSTALLROOT=$1
export CLASSPATH
export JAVA_HOME
export ANT_HOME
echo "CAS INSTALLROOT:${CASINSTALLROOT}"
#echo "Using CLASSPATH="${CLASSPATH}

$ANT_HOME/bin/ant -version;echo ""

#execute the build
$ANT_HOME/bin/ant -DJAVA_HOME=${JAVA_HOME} -DCASINSTALLROOT=${CASINSTALLROOT} $2
