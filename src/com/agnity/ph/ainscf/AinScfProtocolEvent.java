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


package com.agnity.ph.ainscf;

/*
 * This enum defines the event expected from network or CAS that include dialogue and messages.
 * TCAP Dialogue for ANSI - Unidirectional, Query With Response, Query without Response, 
 * 						    Response, Conversation with Permission, Conversation without Permission,
 * 							Abort.
 * JAIN TCAP API specification doesn't provide TCAP dialogue handling for ANSI, therefore it is mapped
 * with ITUT TCAP dialogue events. QwP and QwoP will be received as BEGIN, END as Response and 
 * CwP and CwoP as Continue.  
 * AIN Messages handled are - InfoAnalyze, InfoCollected, SendToResource, ResourceClear, RRBCSM,
 * oAnswer, oNoAnswer, NetworkBusy, oAbandon, Disconnect, Close, EventReportBCSM
 * Application Specific - RRBCSM Timeout, CDR Timeout, Correlation Timeout.
 * 
 * @author reeta
 *
 */
public enum AinScfProtocolEvent {
	BEGIN,
	CONTINUE,
	END,
	UABORT,
	PABORT,
	NOTICE,
	INFO_ANALYZE,
	INFO_COLLECT,
	NTWK_BUSY,
	ERB,
	ERB_ROUTESELECTFAILURE,
	ERB_BUSY,
	ERB_NOANS,
	ERB_ANS,
	ERB_TERM_SEIZED,
	ERB_DISCONNECT,
	ERB_ABANDON,
	CONT,
	RES_CLR,
	CLOSE,		
	AT_TIMEOUT,
	RETURNERROR,
	RETURNRESULT,
	UREJECT,
	UNKNOWN,
	RRBCSM_TIMEOUT,
	CDR_TIMEOUT,
	CORRELATION_TIMEOUT,
	TERMINATION_ATTEMPT,
	AUTHORIZE_TERMINATION,
	ORIGINATION_ATTEMPT,
	TERMINATION_NOTIFICATION,
	UNIDIRECTIONAL,
 	CALL_INFO_FRM_RESRC,
 	LIDB_QUERY,
	BNS_QUERY,
	GN_QUERY,
	OLNS_QUERY,
	TLNS_QUERY,
	CC1_QUERY,
	CC2_QUERY,
	PROVIDE_INSTRUCTION,
	AC_QUERY,
	ISVM_QUERY;
 }
