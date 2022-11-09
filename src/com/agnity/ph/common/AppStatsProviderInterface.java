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

/**
 *  This is the interface used by services to provide stats to HTTPProtocolHandler
 * this method is used by HTTP Protocol Handler on HTTP GET request with getStatistics=SERVICE_NAME
 *
 */
public interface AppStatsProviderInterface {
	
	/**
	 * provides the application stats 
	 * @return
	 */
	public abstract String provideStats();

}
