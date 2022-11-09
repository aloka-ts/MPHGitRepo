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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;

/**
 * This is a singleton class used to  register stats provider nterface for a specific service
 *
 */
public enum AppStatsRegistery {
	
	APP_STATS_REGISTRY;
	
	private static final Logger logger= Logger.getLogger(AppStatsRegistery.class);
	
    private Map<String,AppStatsProviderInterface> statsProviderList=new ConcurrentHashMap<String ,AppStatsProviderInterface>();
	
	
	public  void registerStatsProvider( String ServiceName ,AppStatsProviderInterface provider){
		
		if(logger.isDebugEnabled()){
			logger.debug("Register StatsProvider "+ provider +" for "+ServiceName +"  in" +statsProviderList);
		}
		if(ServiceName!=null && statsProviderList.get(ServiceName)==null){
			
			if(logger.isDebugEnabled()){
				logger.debug("Registering StatsProvider ");
			}
			statsProviderList.put(ServiceName,provider);
		}	
	}
	
	
public AppStatsProviderInterface getStatsProviders(String ServiceName){
		
		
	if(logger.isDebugEnabled()){
		logger.debug("getStatsProviders  for "+ServiceName +"  from" +statsProviderList);
	}
			return statsProviderList.get(ServiceName);
	}
	

}
