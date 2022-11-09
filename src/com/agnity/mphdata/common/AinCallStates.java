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
 *AIN Possible Call states
 *
 */
public enum AinCallStates{
	NULL,
	INIT, 
	SERVICE_LOGIC, 
	TERM_CONNECT_IN_PROGRESS, 
	TERM_CONNECTED,
	//IP_CONNECT_IN_PROGRESS,
	USER_INTREACTION_IN_PROGRESS,
	CALL_HB_IN_PROGRESS,
	TERMINATION_IN_PROGRESS, 
	TERMINATED, 
	ASSIST,
	HANDOFF,
	MS_CONNECT_INPROGRESS,
	MS_CONNECTED,
	DIRECT_MS,
	//PROXY_STATE,
	MS_DISCONNECTED,
	MS_DISCONNECT_INPROGRESS,
	MS_PLAY,
	MS_PLAYCOLLECT,
	MS_PLAY_APP_REQUEST,
 	MS_PLAY_CIFR,
	MS_PLAY_COLLECT_CIFR,
 	BNS_QUERY,
	GN_QUERY,
	BNS_APP_ERR,
	OLNS_QUERY,
	GET_DATA_QUERY,
	CC1_QUERY,
	CC2_QUERY,
	TLNS_QUERY,
	INTERCEPT_QUERY,
	ICDC_QUERY,
	LIDB_PROTOCOL_ERR,
	LIDB_APP_ERR_PROBLEM_DATA,
 	PSX_ROUTING,
 	PROVIDE_INSTRUCTION,
	AC_QUERY, 
	ISVM_QUERY,
	MRS_RELAY_IN_PROGRESS, 
	ACCOUNT_CODE_QUERY
 }
