#   Copyright (c) 2016 Agnity, Inc. All rights reserved.
#
#   This is proprietary source code of Agnity, Inc.
#
#   Agnity, Inc. retains all intellectual property rights associated
#   with this source code. Use is subject to license terms.
#
#   This source code contains trade secrets owned by Agnity, Inc.
#   Confidentiality of this computer program must be maintained at
#   all times, unless explicitly authorized by Agnity, Inc.
#
#   Author	  - Ankit Singhal

#!/bin/ksh

#Setting the constants
typeset -r bold=``
typeset -r offbold=``
typeset -r VERSION_FILE_NAME="mph_version.dat"
typeset -r BACKUP_VERSION_FILE_NAME="backup_mph_version.dat"
typeset -r START_PATCH_VERIFICATION_LOG="*******Starting Patch Verification*********"
typeset -r END_PATCH_VERIFICATION_LOG="*******Ending Patch Verification*********"

#Reading the latest mph_version.dat to determine the current patch level
export VERSION=`grep VERSION ${VERSION_FILE_NAME} |cut -d= -f2`
export MPHNAME=MPH$VERSION

function checkIfPatchAlreadyApplied {
    if [ ! -f ${MPHNAME}_backup.tar ]
    then
       echo "No backup file exists for version: ${VERSION}!! "
       #exit 1
    fi
}

function rollbackMphJars {
   if [ -f ${MPHNAME}_backup.tar ]
   then 
	    if [ -f bpjars/mph.jar ]
    then
      #    rm -rf bpjars/mph.jar bpjars/mph_*.jar
	    tar xf ${MPHNAME}_backup.tar bpjars/mph.jar
 
    fi

	    if [ -f bpjars/mph_*.jar ]
    then
          rm -rf bpjars/mph.jar bpjars/mph_*.jar
    tar xf ${MPHNAME}_backup.tar bpjars

    fi



   else
    echo "backup file donot exists for version: ${VERSION}!!so can not rollback mph " 
    fi  
}

function verifyPatchRollback {
if [ -f ${MPHNAME}_backup.tar ]
then 
    #Untar only backup version file for checksum verification
    tar xf ${MPHNAME}_backup.tar ${BACKUP_VERSION_FILE_NAME}

if [ -f bpjars/mph.jar ]
    then
       APPLIED_MPH_FILESIZE=`cksum bpjars/mph.jar |awk '{print $2}'`
    MPH_FILESIZE=`grep FILESIZE ${BACKUP_VERSION_FILE_NAME} |cut -d= -f2`
 
    fi
	
	    if [ -f bpjars/mph_*.jar ]
    then
	APPLIED_MPH_FILESIZE=`cksum bpjars/mph_*.jar |awk '{print $2}'`
    MPH_FILESIZE=`grep FILESIZE ${BACKUP_VERSION_FILE_NAME} |cut -d= -f2`

    fi



    if [ ${APPLIED_MPH_FILESIZE} -ne ${MPH_FILESIZE} ]
       then
       echo "Patch rollback failed please contact the AGNITY support team!!"
    fi
    mv ${BACKUP_VERSION_FILE_NAME} ${VERSION_FILE_NAME} 
    rm ${MPHNAME}_backup.tar
else
   echo "Patch rollback failed please contact the AGNITY support team!!"
  fi 
}

echo "##################################################################################"
echo "############## ${bold}Rolling Back ${MPHNAME}${offbold} ##########################"
echo "##################################################################################"
echo ""
echo ""

CURRDIR=`pwd`

. $HOME/profile.cas.*

cd $INSTALLROOT/ASESubsystem

PATCH_DIR=`pwd`

if [ $PATCH_DIR = $CURRDIR ]
then
	echo "Rollingback MPH ......"
else
	echo "${bold}You are not in $INSTALLROOT/ASESubsystem dir... Exiting${offbold}"
	exit;
fi

        checkIfPatchAlreadyApplied 

        rollbackMphJars
  
        echo ${START_PATCH_VERIFICATION_LOG}
        verifyPatchRollback
        echo ${END_PATCH_VERIFICATION_LOG}

        export PATCH_HISTORY_DATA="$(date) MPH-ALL : $VERSION rollbacked successfully ."

if [ $? -eq 0 ]
then
    echo ${PATCH_HISTORY_DATA} >> $INSTALLROOT/ASESubsystem/patch_history.log
    echo "-----------------------------------" >> $INSTALLROOT/ASESubsystem/patch_history.log

    echo "${bold}MPH Rollbacked Successfully....${offbold}"
fi

