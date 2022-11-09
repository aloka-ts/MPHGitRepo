/*******************************************************************************
 *   Copyright (c) 2011 Agnity, Inc. All rights reserved.
 *   
 *   This is proprietary source code of Agnity, Inc. 
 *   
 *   Agnity, Inc. retains all intellectual property rights associated 
 *   with this source code. Use is subject to license terms.
 *   
 *   This source code contains trade secrets owned by Agnity, Inc.
 *   Confidentiality of this computer program must be maintained at 
 *   all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.mphdata.common;


/**
 *CAP V2 PH Possible Call states
 *
 */
public enum CapV2CallStates{
	NULL,
	INIT, 
	SERVICE_LOGIC, 
	TERM_CONNECT_IN_PROGRESS, 
	TERM_CONNECTED,
	TERM_CONNECTED_ACR,
	USER_INTREACTION_IN_PROGRESS,
	CALL_HB_IN_PROGRESS,
	TERMINATION_IN_PROGRESS, 
	TERMINATED, 
	TERMINATED_ACR, // This is case in OCS where ACR + ERBCSM is received.
	ASSIST,
	HANDOFF,
	MS_CONNECT_INPROGRESS,
	MS_CONNECTED, 
	MS_DISCONNECTED,
	PSX_ROUTING,
	AC_SENT,      // Apply CHarging Sent
    DIRECT_MS, 
    PLAY_SUCCSS, 
    PLAY_SUCCESS_SRR
}
