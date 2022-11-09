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
package com.agnity.ph.mapscf;

/**
 * This enum defines the event expected from network or CAS that include dialogue and messages
 * TCAP Dialogue - BEGIN, CONTINUE, END, UABORT, PABORT, NOTICE, Return Error, Return Result, Reject, 
 * Incoming Messages - IDP, ERB (Event Report BCSM), ACR (Apply Charging Report), CONT (Continue), 
 *                     SRR (Specialised Resource Report)
 * Application Specific - RRBCSM Timeout, CDR Timeout, Correlation Timeout. 
 */
public enum MapScfProtocolEvent {
	BEGIN, CONTINUE, END, UABORT, PABORT, NOTICE, NSDM, CDR_TIMEOUT, RETURNERROR, RETURNRESULT, UREJECT,SSIN
}
