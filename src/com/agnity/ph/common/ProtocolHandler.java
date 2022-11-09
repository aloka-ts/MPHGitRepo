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
package com.agnity.ph.common;

import javax.servlet.sip.ServletTimer;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;

/**
 * This is Protocol handler interface whcih is implemented by all the protocol
 * handlers 
 *
 */
public interface ProtocolHandler {
	void executeAction(CallData callData, Action action) throws Exception;
	void timeout(ServletTimer timer);
}
