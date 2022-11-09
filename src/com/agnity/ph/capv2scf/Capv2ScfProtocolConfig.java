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

package com.agnity.ph.capv2scf;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.Ss7ProtocolConfig;


/*
 * This file contains CAP startup configuration in the form of key value pairs. For example service key 
 * Local PC and other information. Service can change these values.If service wants to change these values
 * then service will add the respective key-value in callData. For any configuration if key-value
 * entry is present in CallData hashmap then preference will be given to this key-value. Otherwise
 * default of that key-value will be used from this file.
 */
public class Capv2ScfProtocolConfig implements Ss7ProtocolConfig {
	
	private static Logger						logger		= Logger.getLogger(Capv2ScfProtocolConfig.class);
	
	private static final Capv2ScfProtocolConfig	capV2ScfProtocolConfig	= new Capv2ScfProtocolConfig();
	
	
	private static final transient HashMap<String, String>	config	= new HashMap<String, String>();
	
//	private static List<SccpUserAddress>		sSccpLocalAddrList = new LinkedList<SccpUserAddress>();
//	private static String []                    sLocalSsnList;
//	private static String [] 					sLocalPcList;
//	private static List<String>                 sServiceKeyList = new LinkedList<String>();
	
	public static final String	PROTOCOL_VARIANT			= "CAPV2SCF";
	public static final String	CLUSTER_NUMBER			= "CLUSTER_NUMBER";
	public static final String CAPV2_FLOW = "CAPV2FLOW";
	public static final String SEND_TERMINATING_RRBCSM = "CAPV2_SEND_TM_RRBCSM";
	public static final String CAP_DELAY_BW_TWO_DIALOGUE = "CAP_DELAY_BW_TWO_DIALOGUE";
	private static Map<String,List<SccpUserAddress>> sccpLocalMapAddrList= new HashMap<String,List<SccpUserAddress>>();
	private static Map<String,List<String>> serviceKeyMapList= new HashMap<String,List<String>>();
	static {
		setConfigData(PROTOCOL_VARIANT, "1");
		setConfigData(CLUSTER_NUMBER, "12");
		setConfigData(CAPV2_FLOW, "OCS");
		setConfigData(SEND_TERMINATING_RRBCSM, "TRUE");
	}
	
	/**
	 * @return
	 */
	public static Capv2ScfProtocolConfig getInstance() {
		return capV2ScfProtocolConfig;
	}
	
	/*
	 * Application has the responsibility to initialise SS7 parameters for respective protocol 
	 * adaptor. Application shall provide Local SSN list (delimited by , in case of more than 1
	 * SSN), local PC List (delimited by , in case of more than 1). SSN and PC are mapper 1:1
	 * For example, if user define SSN as 190,191 and PC as 2-3-4,4-5-6 then 190 will be mapped
	 * with 2-3-4 and 191 with 4-5-6 respectively. 
	 * Also application need to pass PC in Cluster-Zone-Member format. 
	 */
	/**
	 * Application has the responsibility to initialise SS7 parameters for respective protocol 
	 * adaptor. Application shall provide Local SSN list (delimited by , in case of more than 1
	 * SSN), local PC List (delimited by , in case of more than 1). SSN and PC are mapper 1:1
	 * For example, if user define SSN as 190,191 and PC as 2-3-4,4-5-6 then 190 will be mapped
	 * with 2-3-4 and 191 with 4-5-6 respectively. 
	 * Also application need to pass PC in Cluster-Zone-Member format. 
	 * @param localSsnList  Represents SSN List
	 * @param localPcList   Represents Point Code List 
	 * @param serviceKeyList Represents Service Key List
	 */
	public static void initializeSS7Param(String serviceId,String localSsnList, String localPcList, String serviceKeyList) {
		if(logger.isDebugEnabled()){
			logger.debug("[PH] initializeSS7Param() Enter");
		}
		  List<SccpUserAddress> sSccpLocalAddrList =null;// new LinkedList<SccpUserAddress>();
	  	   String[] sLocalSsnList=null;
	  	    String[] sLocalPcList=null;
	  	   List<String> sServiceKeyList=null;// new LinkedList<String>();
		try {
			if (localSsnList != null) {
				sLocalSsnList = localSsnList.split(",");
			}

			if (localPcList != null) {
				sLocalPcList = localPcList.split(",");
			}
			
			if(serviceKeyList != null) {
				String [] skList = serviceKeyList.split(",");
				
				for (String key: skList) {
					sServiceKeyList.add(key);
				}
			}
			 serviceKeyMapList.put(serviceId, sServiceKeyList);
			SccpUserAddress localAddr = null;
			SignalingPointCode localSpc = null;
			for (int i = 0; i < sLocalSsnList.length; i++) {
				localSpc = null;
				localAddr = null;
				String localPc = sLocalPcList[i];
				if (localPc != null) {
					String[] tmp = localPc.split("-");
					if (tmp.length == 3) {
						localSpc = new SignalingPointCode(Integer.parseInt(tmp[2]), Integer
							.parseInt(tmp[1]), Integer.parseInt(tmp[0]));
					}
				}

				// creating default spc if local SPC is null
				if (localSpc == null) {
					localSpc = new SignalingPointCode(6, 64, 2); 
					logger.error("[PH] initializeSS7Param: CAP v2 PC was null, using default value 6-64-2");
				}

				String localSsn = sLocalSsnList[i];

				if (localSsn == null) {
					localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, (short) 190));
				} else {
					try {
						localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, Short
							.parseShort(localSsn)));
						
					} catch (Exception e) {
						logger.error("[PH] Incorrect local ssn, using default value");
						localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, (short) 190));
					}
					
					int protocolVariant = Integer.parseInt(getConfigData(PROTOCOL_VARIANT));
					
					localAddr.setProtocolVariant(protocolVariant);
					if(logger.isDebugEnabled()){
					// logging local PC/SSN
					logger.debug("[PH] Local PC/SSN "
							+ localAddr.getSubSystemAddress().getSignalingPointCode().getZone()
							+ "-"
							+ localAddr.getSubSystemAddress().getSignalingPointCode()
								.getCluster()
							+ "-"
							+ localAddr.getSubSystemAddress().getSignalingPointCode()
								.getMember() + " "
							+ localAddr.getSubSystemAddress().getSubSystemNumber());
					}
					sSccpLocalAddrList.add(localAddr);
					 sccpLocalMapAddrList.put(serviceId,sSccpLocalAddrList);
				}
			}
			
		} catch (Exception ex){
			logger.error("[PH] Failed to initialize local PC parameter", ex);
		}
		if(logger.isDebugEnabled()){
			logger.debug("[PH] initializeSS7Param() Exit");
		}
	}
	
	
	/* (non-Javadoc)
	 * @see com.agnity.ph.common.Ss7ProtocolConfig#getServiceKeyList()
	 */
//	public List<String> getServiceKeyList() {
//		return sServiceKeyList;
//	}

	/**
	 * @param key
	 * @return
	 */
	public static String getConfigData(String key) {
		return config.get(key);
	}

	/**
	 * @param key
	 * @param value
	 */
	public static void setConfigData(String key, String value) {
		if (logger.isDebugEnabled()) {
			logger.debug("setConfigData  key: "+ key+" value: "+value);
		}
		config.put(key, value);
	}

	/* (non-Javadoc)
	 * @see com.agnity.ph.common.Ss7ProtocolConfig#getSccpLocalAddressList()
	 */
//	@Override
//	public List<SccpUserAddress>  getSccpLocalAddressList() {
//		return sSccpLocalAddrList;
//	}

	/* (non-Javadoc)
	 * @see com.agnity.ph.common.Ss7ProtocolConfig#getProtocol()
	 */
	@Override
	public Protocol getProtocol() {
		return Protocol.CAPV2_SCF;
				
	}
	
	@Override
	public List<String> getServiceKeyList(String serviceId) {
	if(logger.isDebugEnabled()) {
		logger.debug("Fetching service key for service id :- "+ serviceId);
	}
	return	serviceKeyMapList.get(serviceId);
	}

	@Override
	public List<SccpUserAddress> getSccpLocalAddressList(String serviceId) {
		if(logger.isDebugEnabled()) {
			logger.debug("Fetching local address for service id :- "+ serviceId);
		}
		return sccpLocalMapAddrList.get(serviceId);
		
	}

}
