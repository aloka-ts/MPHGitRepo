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
typeset -r VPN_SERVICE_ID=1
typeset -r ATF_SERVICE_ID=2
typeset -r AC_SERVICE_ID=1012
typeset -r CSA_SERVICE_ID=7
typeset -r E911_SERVICE_ID=40 
typeset -r LNP_SERVICE_ID=10
typeset -r VERSION_FILE_NAME="mph_version.dat"
typeset -r BACKUP_VERSION_FILE_NAME="backup_mph_version.dat"
typeset -r START_PATCH_VERIFICATION_LOG="*******Starting Patch Verification*********"
typeset -r END_PATCH_VERIFICATION_LOG="*******Ending Patch Verification*********"

#Reading the latest mph_version.dat to determine the current patch level
export VERSION=`grep VERSION ${VERSION_FILE_NAME} |cut -d= -f2`
export MPHNAME=MPH$VERSION

function checkIfPatchAlreadyAppliedForService {
    SERVICE_ID=$1
    if [ ! -f ${MPHNAME}_${SERVICE_ID}_backup.tar ]
    then
       echo "No backup file exists for service id ${SERVICE_ID} and version: ${VERSION}!! "
       #exit 1
    fi
}

function rollbackMphJarsForService {
    SERVICE_ID=$1
   if [ -f ${MPHNAME}_${SERVICE_ID}_backup.tar ]
   then 
    tar xf ${MPHNAME}_${SERVICE_ID}_backup.tar appjars/${SERVICE_ID}/mph.jar
   else
    echo "backup file donot exists for service id ${SERVICE_ID} and version: ${VERSION}!!so can not rollback mph for service ${SERVICE_ID}" 
    fi  
}

function verifyPatchRollbackForService {
    SERVICE_ID=$1
if [ -f ${MPHNAME}_${SERVICE_ID}_backup.tar ]
then 
    #Untar only backup version file for checksum verification
    tar xf ${MPHNAME}_${SERVICE_ID}_backup.tar ${BACKUP_VERSION_FILE_NAME}

    APPLIED_MPH_FILESIZE=`cksum appjars/${SERVICE_ID}/mph.jar |awk '{print $2}'`
    MPH_FILESIZE=`grep FILESIZE ${BACKUP_VERSION_FILE_NAME} |cut -d= -f2`

    if [ ${APPLIED_MPH_FILESIZE} -ne ${MPH_FILESIZE} ]
       then
       echo "Patch rollback failed for service id ${SERVICE_ID}! if you are not using this service then ignore it otherwise please contact the AGNITY support team!!"
    fi
    rm ${BACKUP_VERSION_FILE_NAME}
    rm ${MPHNAME}_${SERVICE_ID}_backup.tar
else
   echo "Patch rollback failed for service id ${SERVICE_ID}! if you are not using this service then ignore it otherwise please contact the AGNITY support team!!"
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

        checkIfPatchAlreadyAppliedForService ${ATF_SERVICE_ID}
        checkIfPatchAlreadyAppliedForService ${AC_SERVICE_ID}
        checkIfPatchAlreadyAppliedForService ${VPN_SERVICE_ID}
        checkIfPatchAlreadyAppliedForService ${CSA_SERVICE_ID}
        checkIfPatchAlreadyAppliedForService ${E911_SERVICE_ID}    
        checkIfPatchAlreadyAppliedForService ${LNP_SERVICE_ID} 

        rollbackMphJarsForService ${ATF_SERVICE_ID}
        rollbackMphJarsForService ${AC_SERVICE_ID}
        rollbackMphJarsForService ${VPN_SERVICE_ID}
        rollbackMphJarsForService ${CSA_SERVICE_ID}
        rollbackMphJarsForService ${E911_SERVICE_ID}     
        rollbackMphJarsForService ${LNP_SERVICE_ID}
  
        echo ${START_PATCH_VERIFICATION_LOG}
        verifyPatchRollbackForService ${ATF_SERVICE_ID}
        verifyPatchRollbackForService ${AC_SERVICE_ID}
        verifyPatchRollbackForService ${VPN_SERVICE_ID}
        verifyPatchRollbackForService ${CSA_SERVICE_ID}
        verifyPatchRollbackForService ${E911_SERVICE_ID}  
        verifyPatchRollbackForService ${LNP_SERVICE_ID} 
        echo ${END_PATCH_VERIFICATION_LOG}

        export PATCH_HISTORY_DATA="$(date) MPH-ALL : $VERSION rollbacked successfully for All Services."

if [ $? -eq 0 ]
then
    echo ${PATCH_HISTORY_DATA} >> $INSTALLROOT/ASESubsystem/patch_history.log
    echo "-----------------------------------" >> $INSTALLROOT/ASESubsystem/patch_history.log

    echo "${bold}MPH Rollbacked Successfully....${offbold}"
fi

