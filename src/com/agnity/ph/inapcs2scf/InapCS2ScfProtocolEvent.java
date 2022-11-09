/*******************************************************************************
 * Copyright (c) 2011 Agnity, Inc. All rights reserved.
 * <p>
 * This is proprietary source code of Agnity, Inc.
 * <p>
 * Agnity, Inc. retains all intellectual property rights associated
 * with this source code. Use is subject to license terms.
 * <p>
 * This source code contains trade secrets owned by Agnity, Inc.
 * Confidentiality of this computer program must be maintained at
 * all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.ph.inapcs2scf;

/**
 * This enum defines the event expected from network or CAS that include dialogue and messages
 * TCAP Dialogue - BEGIN, CONTINUE, END, UABORT, PABORT, NOTICE, Return Error, Return Result, Reject,
 * Incoming Messages - IDP, ERB (Event Report BCSM), ACR (Apply Charging Report), CONT (Continue),
 * SRR (Specialised Resource Report)
 * Application Specific - RRBCSM Timeout, CDR Timeout, Correlation Timeout.
 */
public enum InapCS2ScfProtocolEvent {
    BEGIN,
    CONTINUE,
    END,
    UABORT,
    PABORT,
    NOTICE,
    IDP,
    ENC,
    ERB,
    ERB_ROUTESELECTFAILURE,
    ERB_BUSY,
    ERB_NOANS,
    ERB_ANS,
    ERB_DISCONNECT,
    ERB_ABANDON,
    ACR,
    CONT,
    SRR,
    AT_TIMEOUT,
    RETURNERROR,
    RETURNRESULT,
    UREJECT,
    UNKNOWN,
    RRBCSM_TIMEOUT,
    CDR_TIMEOUT,
    CORRELATION_TIMEOUT,
    ARI, 
    ERB_MIDCALL
}
