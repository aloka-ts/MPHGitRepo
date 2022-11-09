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

#Reading the latest mph_version.dat to determine the current patch level and other properties
export VERSION=`grep VERSION ${VERSION_FILE_NAME} |cut -d= -f2`
export MPH_FILESIZE=`grep FILESIZE ${VERSION_FILE_NAME} |cut -d= -f2`

export MPHNAME=MPH$VERSION

function putMPHJarDetailsInBackupVersionFile {

    if [ -f bpjars/mph.jar ]
    then

    export MPH_BACKUP_FILESIZE=`cksum bpjars/mph.jar |awk '{print $2}'`
    echo "VERSION="${VERSION}>${BACKUP_VERSION_FILE_NAME}  
    echo "FILESIZE="${MPH_BACKUP_FILESIZE}>${BACKUP_VERSION_FILE_NAME}
    fi


    if [ -f bpjars/mph_*.jar ]
    then

    export MPH_BACKUP_FILESIZE=`cksum bpjars/mph_*.jar |awk '{print $2}'`
    echo "VERSION="${VERSION}>${BACKUP_VERSION_FILE_NAME}  
    echo "FILESIZE="${MPH_BACKUP_FILESIZE}>${BACKUP_VERSION_FILE_NAME}
    fi

}

function addMPHJarsInBackup {
    if [ -f bpjars/mph_*.jar ]
    then
        putMPHJarDetailsInBackupVersionFile
        tar -cf ${MPHNAME}_backup.tar bpjars/mph_*.jar ${BACKUP_VERSION_FILE_NAME}
        rm bpjars/mph_*.jar
        rm ${BACKUP_VERSION_FILE_NAME}
    fi

    if [ -f bpjars/mph.jar ]
    then
        putMPHJarDetailsInBackupVersionFile
        tar -cf ${MPHNAME}_backup.tar bpjars/mph.jar ${BACKUP_VERSION_FILE_NAME}
        rm bpjars/mph.jar
        rm ${BACKUP_VERSION_FILE_NAME}
    fi

    if [ $? -ne 0 ]
    then
        echo "${bold}ERROR in taking backup of mph !!!"
        if test -s ${MPHNAME}_backup.tar
            then
            rm ${MPHNAME}_backup.tar
        fi
        exit
    fi

}

function applyMPHJarsPatch {
    tar xf $CURRDIR/mph_patch.tar

    if [ -f bpjars/mph.jar ]
    then
      echo "${bold}MPH patch applied successfully ${offbold}" 
    fi


    if [ -f bpjars/mph_*.jar ]
    then
      echo "${bold}MPH patch applied successfully ${offbold}" 
    fi
	


    #Rollback if patch failed
    if [ $? -ne 0 ]
    then
        echo "${bold}Problem in Applying MPH; Rolling Back patch for service id ${SERVICE_ID}...${offbold}"
        tar xf ${MPHNAME}_backup.tar bpjars/mph.jar
        if [ $? -ne 0 ]
        then
            echo "${bold}Rollback failed ${offbold}"
            exit
        fi
        rm -rf ${MPHNAME}_backup.tar
        exit
    fi
}

function checkAndAbortIfPatchAlreadyApplied {
    if [ -f ${MPHNAME}_backup.tar ]
    then
        echo "${bold}MPH ${MPHNAME} already applied . If you want to apply again first run rollback script  and then try to apply MPH again ${offbold}"
        exit 1
    fi
}

function verifyPatchApplication {

    if [ -f bpjars/mph.jar ]
    then
      APPLIED_MPH_FILESIZE=`cksum bpjars/mph.jar |awk '{print $2}'` 
    fi

    if [ -f bpjars/mph_${VERSION}.jar ]
    then
      APPLIED_MPH_FILESIZE=`cksum bpjars/mph_${VERSION}.jar |awk '{print $2}'` 
    fi


#    APPLIED_MPH_FILESIZE=`cksum bpjars/mph_${VERSION}.jar |awk '{print $2}'`
    if [ ${APPLIED_MPH_FILESIZE} -ne ${MPH_FILESIZE} ]
    then
        echo "Patch verification failed please contact the support team!!"
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
        
        checkAndAbortIfPatchAlreadyApplied
     
        addMPHJarsInBackup 
 
        applyMPHJarsPatch

 
        echo ${START_PATCH_VERIFICATION_LOG}
        verifyPatchApplication
        echo ${END_PATCH_VERIFICATION_LOG}

        export PATCH_HISTORY_DATA="$(date) MPH-ALL : $VERSION applied successfully for All Services."

if [ $? -eq 0 ]
then
    echo ${PATCH_HISTORY_DATA} >> $INSTALLROOT/ASESubsystem/patch_history.log
    echo "-----------------------------------" >> $INSTALLROOT/ASESubsystem/patch_history.log

    echo "${bold}MPH Updated Successfully....${offbold}"
fi

