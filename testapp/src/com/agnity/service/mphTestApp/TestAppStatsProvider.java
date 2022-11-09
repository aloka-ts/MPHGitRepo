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
package com.agnity.service.mphTestApp;


public class TestAppStatsProvider implements com.agnity.ph.common.AppStatsProviderInterface{

	
	public static final String SERVICE_NAME="TSP";
	@Override
	public String provideStats() {
		return "Hey u are in test app stats provider ,:) i m doing well";
	}

}
