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
package com.agnity.ph.common;

import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.DialogueIndEvent;

/**
 * This interface is implemented by SS7 Protocol handlers. it extends the
 * functionality of Protocol handler interface as well. 
 *
 */
public interface SS7ProtocolHandler extends ProtocolHandler {

    void processDialogueIndEvent(DialogueIndEvent dialogueIndEvent, ServiceInterface serviceHandler);

    void processComponentIndEvent(ComponentIndEvent componentIndEvent, ServiceInterface serviceHandler);
}
