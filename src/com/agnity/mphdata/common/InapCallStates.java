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
package com.agnity.mphdata.common;

/**
 * This class is used to define INAP Call states
 */
public enum InapCallStates {
    NULL,
    INIT,
    SERVICE_LOGIC,
    TERM_CONNECT_IN_PROGRESS,
    TERM_CONNECTED,
    TERM_CONNECTED_ACR,
    TERMINATION_IN_PROGRESS,
    TERMINATED,
    ASSIST,
    HANDOFF,
    MS_CONNECT_INPROGRESS,
    MS_CONNECTED,
    MS_DISCONNECTED,
    MS_DISCONNECTED_END,
    USER_INTREACTION_IN_PROGRESS, 
	MS_PLAY,
	MS_PLAYCOLLECT,
    PSX_ROUTING, 
    OTG_INITIATE_CALL
}
