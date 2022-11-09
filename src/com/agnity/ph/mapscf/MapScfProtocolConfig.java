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

import jain.ParameterNotSetException;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;
import jain.protocol.ss7.tcap.dialogue.DialoguePortion;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.asngenerated.Outgoing;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.Ss7ProtocolConfig;


/**
 * This file contains MAP startup configuration in the form of key value pairs. For example service key 
 * Local PC and other information. Service can change these values.If service wants to change these values
 * then service will add the respective key-value in callData. For any configuration if key-value
 * entry is present in CallData hashmap then preference will be given to this key-value. Otherwise
 * default of that key-value will be used from this file. 
 */
public class MapScfProtocolConfig implements Ss7ProtocolConfig {

	private static short MAP_SCF_SSN = 147;
	private static Logger	logger	= Logger.getLogger(MapScfProtocolConfig.class);
	private static final MapScfProtocolConfig	mapScfProtocolConfig	= new MapScfProtocolConfig();
	private static final transient HashMap<String, String>	config	= new HashMap<String, String>();
	
	private static final transient HashMap<String, SccpUserAddress>	svcKeySUAMap	= new HashMap<String, SccpUserAddress>();
	private static List<SccpUserAddress> sSccpLocalAddrList;
//	private static String[] sLocalSsnList;
//	private static String[] sLocalPcList;
//	private static List<String> sServiceKeyList;
	private static DialoguePortion dp = null;
	private static String outgoingSsn= null;
	private static SccpUserAddress localSua = null;

	public static final String PROTOCOL_VARIANT = "MAPSCF";
	public static final String CLUSTER_NUMBER = "CLUSTER_NUMBER";

	public static final String DESTINATION_SSN = "DESTINATION_SSN";
	public static final String DESTINATION_PC = "DESTINATION_PC";
	public static final String DESTINATION_GT_IND = "DESTINATION_GT_IND";
	public static final String DESTINATION_TT = "DESTINATION_TT";
	public static String DESTINATION_ROUTING_IND="DESTINATION_ROUTING_IND";
	public static final String DESTINATION_GT_DIGITS = "DESTINATION_GT_DIGITS";
	public static final String DESTINATION_SVC_KEY = "DESTINATION_SVC_KEY";
	public static final String MAP_DIALOGUE_PORTION = "MAP_DIALOGUE_PORTION";
	public static final String MAP_OUTGOING_DLG_SSN = "MAP_OUTGOING_DLG_SSN";
	
	private static Map<String,List<SccpUserAddress>> sccpLocalMapAddrList= new HashMap<String,List<SccpUserAddress>>();
	private static Map<String,List<String>> serviceKeyMapList= new HashMap<String,List<String>>();

	static {
		setConfigData(PROTOCOL_VARIANT, "1");
		setConfigData(CLUSTER_NUMBER, "12");
	}

	public static MapScfProtocolConfig getInstance() {
		return mapScfProtocolConfig;
	}

	/**
	 * Application has the responsibility to initialize SS7 parameters for respective protocol 
	 * adaptor. Application shall provide Local SSN list (delimited by , in case of more than 1
	 * SSN), local PC List (delimited by , in case of more than 1). SSN and PC are mapped 1:1
	 * For example, if user define SSN as 190,191 and PC as 2-3-4,4-5-6 then 190 will be mapped
	 * with 2-3-4 and 191 with 4-5-6 respectively. 
	 * Also application need to pass PC in Cluster-Zone-Member format. 
	 */
	public static void initializeSS7Param(String serviceId,String localSsnList, String localPcList, String serviceKeyList) {
		if(logger.isDebugEnabled()){
			logger.debug("[PH] initializeSS7Param() Enter");
		}
		
		  //List<SccpUserAddress> sSccpLocalAddrList =null;// new LinkedList<SccpUserAddress>();
	  	   String[] sLocalSsnList=null;
	  	    String[] sLocalPcList=null;
	  	   List<String> sServiceKeyList=null;// new LinkedList<String>();
		try {
			//			if (localSsnList != null) {
			//				sLocalSsnList = localSsnList.split(",");
			//			}

			if (localPcList != null) {
				sLocalPcList = localPcList.split(",");
			}

			if(serviceKeyList != null) {
				String[] skList = serviceKeyList.split(",");

				sServiceKeyList = new LinkedList<String>();
				for (String key : skList) {
					sServiceKeyList.add(key);
				}
			}
			 serviceKeyMapList.put(serviceId, sServiceKeyList);
			SccpUserAddress localAddr = null;
			SignalingPointCode localSpc = null;

			if (localSsnList != null) {	

				sLocalSsnList = localSsnList.split(",");
				sSccpLocalAddrList = new LinkedList<SccpUserAddress>();

				for (int i = 0; i < sLocalSsnList.length; i++) {
					localSpc = null;
					localAddr = null;
					String localPc = sLocalPcList[i];
					if (localPc != null) {
						String[] tmp = localPc.split("-");
						if (tmp.length == 3) {
							localSpc = new SignalingPointCode(Integer.parseInt(tmp[2]), Integer
									.parseInt(tmp[1]), Integer.parseInt(tmp[0]));
						} else {
							logger.error("Invalid local point code configured: "+localPc);
						}
					}

					// creating default spc if local SPC is null
					if (localSpc == null) {
						localSpc = new SignalingPointCode(6, 64, 2);
						logger.error("[PH] initializeSS7Param: MAP PC was null, using default value 6-64-2");
					}

					String localSsn = sLocalSsnList[i];

					if (localSsn == null) {
						logger.info("MAP Local SSN not configured, defaulting to "+MAP_SCF_SSN);
						localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, MAP_SCF_SSN));
					} else {
						try {
							localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, Short
									.parseShort(localSsn)));
						} catch (Exception e) {
							logger.error("[PH]MAP  Local SccpUserAddress initialization failed", e);
							logger.info("[PH] Using default PC: ["+localSpc+"], SSN: ["+MAP_SCF_SSN+"]");
							localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, MAP_SCF_SSN));
						}

						int protocolVariant = Integer.parseInt(getConfigData(PROTOCOL_VARIANT));
						logger.info("[PH] MAP Protocol Variant Configured: "+protocolVariant);
						localAddr.setProtocolVariant(protocolVariant);
						if(logger.isDebugEnabled()){
							logger.debug("[PH] MAP Local PC/SSN "
									+ localAddr.getSubSystemAddress().getSignalingPointCode().getZone()
									+ "-"
									+ localAddr.getSubSystemAddress().getSignalingPointCode().getCluster()
									+ "-"
									+ localAddr.getSubSystemAddress().getSignalingPointCode().getMember() 
									+ " / "
									+ localAddr.getSubSystemAddress().getSubSystemNumber() + " "
									+ "Service Key List = " + sServiceKeyList
									);
						}
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


//	public List<String> getServiceKeyList() {
//		return sServiceKeyList;
//	}

	public static String getConfigData(String key) {
		return config.get(key);
	}

	public static void setConfigData(String key, String value) {
		config.put(key, value);
		
		// check if DIalogue Portion is being set in mph.properties
		// if yes then creation dialogue portion and set. It will 
		// be static and shall be used for all outgoing dialogues 
		// begin
		if(StringUtils.equalsIgnoreCase(key, MAP_DIALOGUE_PORTION)){
			logger.debug("MAP_DIALOGUE_PORTION value set:" + value);
			setDialoguePortion(CommonUtils.createDaloguePortion(value));
		}else if(StringUtils.equalsIgnoreCase(key, MAP_OUTGOING_DLG_SSN)){
			logger.debug("MAP_OUTGOING_DLG_SSN value set:" + value);
			if(StringUtils.isNotBlank(value)){
				outgoingSsn = value;
				Integer otgSsn = -1;
				try{
					otgSsn = Integer.parseInt(outgoingSsn);
				}catch(Exception ex){
					logger.error("Exception in integer conversion");
				}
				if(sSccpLocalAddrList != null){
					for(SccpUserAddress temp:sSccpLocalAddrList){
						try {
							if(temp.getSubSystemAddress() != null &&
									temp.getSubSystemAddress().getSubSystemNumber() == otgSsn){
								localSua = temp;
								logger.debug("MAP Local SUA for configured SSN:" + otgSsn);
								break;
							}
						} catch (ParameterNotSetException e) {
							logger.error("MAP Error in sccp local address list");
						}
					}
				}
			}
		}
	}

//	@Override
//	public List<SccpUserAddress>  getSccpLocalAddressList() {
//		return sSccpLocalAddrList;
//	}

	@Override
	public Protocol getProtocol() {
		return Protocol.MAP_SCF;
	}

	public static DialoguePortion getDialoguePortion() {
		return dp;
	}

	public static void setDialoguePortion(DialoguePortion dp) {
		MapScfProtocolConfig.dp = dp;
	}

	public static SccpUserAddress getLocalSua() {
		if(logger.isDebugEnabled()){
			logger.debug("GetLocalSua from MAP Config:" + localSua);
		}
		return localSua;
	}
	public static void setLocalSua(SccpUserAddress localSua) {
		MapScfProtocolConfig.localSua = localSua;
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
