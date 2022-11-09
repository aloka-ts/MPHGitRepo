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

#Reading the latest mph_version.dat to determine the current patch level and other properties
export VERSION=`grep VERSION ${VERSION_FILE_NAME} |cut -d= -f2`
export MPH_FILESIZE=`grep FILESIZE ${VERSION_FILE_NAME} |cut -d= -f2`

export MPHNAME=MPH$VERSION

function putMPHJarDetailsInBackupVersionFile {
    export MPH_BACKUP_FILESIZE=`cksum appjars/${SERVICE_ID}/mph.jar |awk '{print $2}'`
    echo "FILESIZE="${MPH_BACKUP_FILESIZE} > ${BACKUP_VERSION_FILE_NAME}
}

function addMPHJarsInBackupForService {
    SERVICE_ID=$1
    if [ -f appjars/${SERVICE_ID}/mph.jar ]
    then
        putMPHJarDetailsInBackupVersionFile
        tar -cf ${MPHNAME}_${SERVICE_ID}_backup.tar appjars/${SERVICE_ID}/mph.jar ${BACKUP_VERSION_FILE_NAME}
        rm appjars/${SERVICE_ID}/mph.jar
        rm ${BACKUP_VERSION_FILE_NAME}
    fi

    if [ $? -ne 0 ]
    then
        echo "${bold}ERROR in taking backup for service id ${SERVICE_ID}:${offbold} if you are not using this service you can ignore it !!!"
        if test -s ${MPHNAME}_${SERVICE_ID}_backup.tar
            then
            rm ${MPHNAME}_${SERVICE_ID}_backup.tar
        fi
        exit
    fi

    #Make the directory if not already exists
        mkdir -p appjars/${SERVICE_ID}
}

function applyMPHJarsPatchForService {
    SERVICE_ID=$1
    tar xf $CURRDIR/mph_patch.tar

    if [ -f appjars/mph.jar ]
    then
        if [ -d appjars/${SERVICE_ID}/ ];
        then
            cp -rf appjars/mph.jar appjars/${SERVICE_ID}/mph.jar
        fi
        rm -rf appjars/mph.jar
    fi

    #Rollback if patch failed
    if [ $? -ne 0 ]
    then
        echo "${bold}Problem in Applying MPH; Rolling Back patch for service id ${SERVICE_ID}...${offbold}"
        tar xf ${MPHNAME}_${SERVICE_ID}_backup.tar appjars/${SERVICE_ID}/mph.jar
        if [ $? -ne 0 ]
        then
            echo "${bold}Rollback failed for service id ${SERVICE_ID}...${offbold}"
            exit
        fi
        rm -rf ${MPHNAME}_${SERVICE_ID}_backup.tar
        exit
    fi
}

function checkAndAbortIfPatchAlreadyAppliedForService {
    SERVICE_ID=$1
    if [ -f ${MPHNAME}_${SERVICE_ID}_backup.tar ]
    then
        echo "${bold}MPH ${MPHNAME} already applied for service id ${SERVICE_ID}. If you want to apply again first run rollback script for service id ${SERVICE_ID} and then try to apply MPH again ${offbold}"
        exit 1
    fi
}

function verifyPatchApplicationForService {
    SERVICE_ID=$1
    APPLIED_MPH_FILESIZE=`cksum appjars/${SERVICE_ID}/mph.jar |awk '{print $2}'`
    if [ ${APPLIED_MPH_FILESIZE} -ne ${MPH_FILESIZE} ]
    then
        echo "Patch verification failed for service id ${SERVICE_ID}! if you are not using this service then ignore it otherwise please contact the support team!!"
    fi
}

echo "##################################################################################"
echo "################## ${bold}Applying ${MPHNAME}${offbold} ##########################"
echo "##################################################################################"
echo ""
echo ""

CURRDIR=`pwd`

. $HOME/profile.cas.*

cd $INSTALLROOT/ASESubsystem

PATCH_DIR=`pwd`

if [ $PATCH_DIR = $CURRDIR ]
then
	echo "Updating MPH ......"
else
	echo "${bold}You are not in $INSTALLROOT/ASESubsystem dir... Exiting${offbold}"
	exit;
fi

        #Make the appjars directory if not already exists
        #mkdir -p appjars // not needed
        
        checkAndAbortIfPatchAlreadyAppliedForService ${ATF_SERVICE_ID}
        checkAndAbortIfPatchAlreadyAppliedForService ${AC_SERVICE_ID}
        checkAndAbortIfPatchAlreadyAppliedForService ${VPN_SERVICE_ID}
        checkAndAbortIfPatchAlreadyAppliedForService ${CSA_SERVICE_ID}
        checkAndAbortIfPatchAlreadyAppliedForService ${E911_SERVICE_ID}   
        checkAndAbortIfPatchAlreadyAppliedForService ${LNP_SERVICE_ID}
     
        addMPHJarsInBackupForService ${ATF_SERVICE_ID}
        addMPHJarsInBackupForService ${AC_SERVICE_ID}
        addMPHJarsInBackupForService ${VPN_SERVICE_ID}
        addMPHJarsInBackupForService ${CSA_SERVICE_ID}
        addMPHJarsInBackupForService ${E911_SERVICE_ID}
        addMPHJarsInBackupForService ${LNP_SERVICE_ID} 
 
        applyMPHJarsPatchForService ${ATF_SERVICE_ID}
        applyMPHJarsPatchForService ${AC_SERVICE_ID}
        applyMPHJarsPatchForService ${VPN_SERVICE_ID}
        applyMPHJarsPatchForService ${CSA_SERVICE_ID}
        applyMPHJarsPatchForService ${E911_SERVICE_ID} 
        applyMPHJarsPatchForService ${LNP_SERVICE_ID}         

 
        echo ${START_PATCH_VERIFICATION_LOG}
        verifyPatchApplicationForService ${ATF_SERVICE_ID}
        verifyPatchApplicationForService ${AC_SERVICE_ID}
        verifyPatchApplicationForService ${VPN_SERVICE_ID}
        verifyPatchApplicationForService ${CSA_SERVICE_ID}
        verifyPatchApplicationForService ${E911_SERVICE_ID} 
        verifyPatchApplicationForService ${LNP_SERVICE_ID} 
        echo ${END_PATCH_VERIFICATION_LOG}

        export PATCH_HISTORY_DATA="$(date) MPH-ALL : $VERSION applied successfully for All Services."

if [ $? -eq 0 ]
then
    echo ${PATCH_HISTORY_DATA} >> $INSTALLROOT/ASESubsystem/patch_history.log
    echo "-----------------------------------" >> $INSTALLROOT/ASESubsystem/patch_history.log

    echo "${bold}MPH Updated Successfully....${offbold}"
fi

