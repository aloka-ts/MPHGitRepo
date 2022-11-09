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
package com.agnity.ph.common.measurement.enums;

/**
 * This enum includes all SIP messages.
 * @author rbehl
 *
 */
public enum SipMessage {
	REQ_INVITE,
	REQ_UPDATE,
	REQ_PRACK,
	REQ_CANCEL,
	REQ_REFER,
	REQ_ACK,
	REQ_BYE,
	REQ_INFO,
	REQ_NOTIFY,
	REQ_OPTIONS,
	RES_1XX,
	RES_2XX,
	RES_3XX,
	RES_4XX,
	RES_5XX,
	RES_6XX;
}
