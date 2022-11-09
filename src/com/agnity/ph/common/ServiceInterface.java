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

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;

/**
 *This interface is used by services to process a event. this method is called by the ProtocolRouter
 * when a protocol handler calls execute action method of Protocol router.
 */
public interface ServiceInterface {
	
	public static final String  APP_CHAIN_MANAGER="com.baypackets.ase.router.customize.servicenode.AseAppChainManager";
	public static final String PRI_NUMBER_MGR="com.baypackets.ase.util.AsePRINumberManager";

	public Action[] processEvent(Event event, CallData callData);

	public String getServletName();

	public String getApplicationName();

	public String[] getServiceCdr(CallData callData);

}
