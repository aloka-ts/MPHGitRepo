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
package com.agnity.ph.inapcs1scf;

import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.Ss7ProtocolConfig;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;

import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;


/**
 * This file contains INAP startup configuration in the form of key value pairs. For example service key
 * Local PC and other information. Service can change these values.If service wants to change these values
 * then service will add the respective key-value in callData. For any configuration if key-value
 * entry is present in CallData hashmap then preference will be given to this key-value. Otherwise
 * default of that key-value will be used from this file.
 */
public class InapCS1ScfProtocolConfig implements Ss7ProtocolConfig {

	public static final String PROTOCOL_VARIANT = "INAPCS1SCF";
	public static final String CLUSTER_NUMBER = "CLUSTER_NUMBER";
	public static final String MAX_CALL_DURATION = "MAX_CALL_DURATION";
	public static final String SHARED_TOKEN_POOL_ENABLED = "SHARED_TOKEN_POOL_ENABLED";
	private static final InapCS1ScfProtocolConfig inapCS1ScfProtocolConfig = new InapCS1ScfProtocolConfig();
	private static final transient HashMap<String, String> config = new HashMap<String, String>();
	private static Logger logger = Logger.getLogger(InapCS1ScfProtocolConfig.class);
//	private static List<SccpUserAddress> sSccpLocalAddrList ;// new LinkedList<SccpUserAddress>();
//	private static String[] sLocalSsnList;
//	private static String[] sLocalPcList;
//	private static List<String> sServiceKeyList ;// new LinkedList<String>();
	public static String SERVICE_CHAINING_ENABLED="SERVICE_CHAINING_ENABLED";
	public static final String SEND_OPTIONAL_PARAMS_CONNECT = "SEND_OPTIONAL_PARAMS_CONNECT"; // Added to avoid CallingParty Number 
	// and ServiceInteractionIndicatorTwo in CONNECT Message
	public static final String SEND_OPTIONAL_PARAMS_PA = "SEND_OPTIONAL_PARAMS_PA";

	// configuration parameters t handle ETC parameters. 
	public static final String SEND_OPTIONAL_PARAMS_ETC = "SEND_OPTIONAL_PARSMS_ETC";
	public static final String SEND_RESET_TIMER_AFTER_ETC = "SEND_RESET_TIMER_AFTER_ETC";
	public static final String RESET_TIMER_VALUE = "RESET_TIMER_VALUE";
	public static final String AT_ACK_TIMER = "AT_ACK_TIMER";
	public static final String SEND_AT_PERIODICALLY = "SEND_AT_PERIODICALLY";
	public static final String FSM_RRBCSM_FCI_CON_SEPARATE = "FSM_RRBCSM_FCI_CON_SEPERATE";
	public static final String ERB_MODE_AND_LEG_INFO = "INAP_ERB_MODE_AND_LEG_INFO";
	public static final String DESTINATION_SSN = "DESTINATION_SSN";
	public static final String DESTINATION_PC = "DESTINATION_PC";
	public static final String DESTINATION_SVC_KEY = "DESTINATION_SVC_KEY";
	public static final String EXTENSION4_ENABLED = "EXTENSION4_ENABLED";
	
	public static final String ITUT_SEND_PROTOCOL_VER="ITUT_SEND_PROTOCOL_VER";
	
	private static Map<String,List<SccpUserAddress>> sccpLocalMapAddrList= new HashMap<String,List<SccpUserAddress>>();
	private static Map<String,List<String>> serviceKeyMapList= new HashMap<String,List<String>>();

	static {
		setConfigData(PROTOCOL_VARIANT, "1");
		setConfigData(CLUSTER_NUMBER, "12");
		setConfigData(MAX_CALL_DURATION, null);
		setConfigData(SERVICE_CHAINING_ENABLED, "FALSE");
		setConfigData(SEND_OPTIONAL_PARAMS_CONNECT, PhConstants.TRUE);  
		setConfigData(SEND_OPTIONAL_PARAMS_PA, PhConstants.FALSE);
		setConfigData(SEND_OPTIONAL_PARAMS_ETC, PhConstants.FALSE);
		setConfigData(SEND_RESET_TIMER_AFTER_ETC, PhConstants.TRUE);
		setConfigData(RESET_TIMER_VALUE, "300");
		setConfigData(AT_ACK_TIMER, "10000");
		setConfigData(SEND_AT_PERIODICALLY, PhConstants.TRUE);
		setConfigData(FSM_RRBCSM_FCI_CON_SEPARATE, PhConstants.TRUE);
		setConfigData(EXTENSION4_ENABLED, PhConstants.FALSE);
		// 1 - Notify and continue, 0 - Interrupted
		// Leg1, Leg2, Both
		setConfigData(ERB_MODE_AND_LEG_INFO, 
				"ERB_ROUTESELECTFAILURE:0:Leg2|ERB_BUSY:0:Leg2|ERB_NO_ANSWER:0:null|ERB_ANSWER:1:Leg2|ERB_DISCONNECT:0:Both|ERB_ABANDON:0:Leg1");
		setConfigData(ITUT_SEND_PROTOCOL_VER,"");
	}

	public static InapCS1ScfProtocolConfig getInstance() {
		return inapCS1ScfProtocolConfig;
	}

	/**
	 * Application has the responsibility to initialise SS7 parameters for respective protocol
	 * adaptor. Application shall provide Local SSN list (delimited by , in case of more than 1
	 * SSN), local PC List (delimited by , in case of more than 1). SSN and PC are mapper 1:1
	 * For example, if user define SSN as 190,191 and PC as 2-3-4,4-5-6 then 190 will be mapped
	 * with 2-3-4 and 191 with 4-5-6 respectively.
	 * Also application need to pass PC in Cluster-Zone-Member format.
	 */
	public static void initializeSS7Param(String serviceId,String localSsnList, String localPcList, String serviceKeyList) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: initializeSS7Param() Enter");
		}
		
		  List<SccpUserAddress> sSccpLocalAddrList =null;// new LinkedList<SccpUserAddress>();
	  	   String[] sLocalSsnList=null;
	  	    String[] sLocalPcList=null;
	  	   List<String> sServiceKeyList=null;// new LinkedList<String>();
		try {
			//            if (localSsnList != null) {
			//                sLocalSsnList = localSsnList.split(",");
			//            }

			if (localPcList != null) {
				sLocalPcList = localPcList.split(",");
			}

			if (serviceKeyList != null) {
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
							localSpc = new SignalingPointCode(Integer.parseInt(tmp[2]), Integer.parseInt(tmp[1]), Integer.parseInt(tmp[0]));
						}
					}

					// creating default spc if local SPC is null
					if (localSpc == null) {
						localSpc = new SignalingPointCode(6, 64, 2);
						logger.error("[PH]:: initializeSS7Param: INAP CS1 PC was null, using default value 6-64-2");
					}

					String localSsn = sLocalSsnList[i];


					if (localSsn == null) {
						localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, (short) 190));
					} else {
						try {
							localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, Short.parseShort(localSsn)));
						} catch (Exception e) {
							logger.error("[PH]:: Incorrect local ssn, using default value");
							//code review comment:: change default SSN to 190-->saneja
							localAddr = new SccpUserAddress(new SubSystemAddress(localSpc, (short) 190));
						}
						int protocolVariant = Integer.parseInt(getConfigData(PROTOCOL_VARIANT));
						localAddr.setProtocolVariant(protocolVariant);
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: Local PC/SSN " +
									localAddr.getSubSystemAddress().getSignalingPointCode().getZone()
									+ "-"
									+ localAddr.getSubSystemAddress().getSignalingPointCode().getCluster()
									+ "-"
									+ localAddr.getSubSystemAddress().getSignalingPointCode().getMember()
									+ " "
									+ localAddr.getSubSystemAddress().getSubSystemNumber());
						}
						// sSccpLocalAddrList.add(localAddr);
					}
					sSccpLocalAddrList.add(localAddr);
					  sccpLocalMapAddrList.put(serviceId,sSccpLocalAddrList);
				}

				if(logger.isDebugEnabled()){
					logger.debug("[PH]:: Local AddrList :: " + sSccpLocalAddrList);
				}
			}

		} catch (Exception ex) {
			logger.error("[PH]:: Failed to initialize local PC parameter", ex);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: initializeSS7Param() Exit");
		}
	}

	public static String getConfigData(String key) {
		return config.get(key);
	}

	public static void setConfigData(String key, String value) {
		if (logger.isDebugEnabled()) {
			logger.debug("InapCS1Scf setConfigData  key: "+ key+" value: "+value);
		}
		config.put(key, value);
	}

//	public List<String> getServiceKeyList() {
//		return sServiceKeyList;
//	}
//
//	@Override
//	public List<SccpUserAddress> getSccpLocalAddressList() {
//		return sSccpLocalAddrList;
//	}

	@Override
	public Protocol getProtocol() {
		return Protocol.ITUINAPCS1_SCF;
	}

//	@Override
//	public String toString() {
//
//		StringBuffer buffer = new StringBuffer();
//		buffer.append("service keylist: " + sServiceKeyList);
//		buffer.append("sLocalSsnList: " + sLocalSsnList);
//		buffer.append("sLocalPcList: " + sLocalPcList);
//
//		return buffer.toString();
//
//	}
//	
	
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
