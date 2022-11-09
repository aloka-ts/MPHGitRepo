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
package com.agnity.ph.ainscf;

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
 * This file contains AIN startup configuration in the form of key value pairs. For example 
 * Local PC and other information. Service can change these values.If service wants to change these values
 * then service will add the respective key-value in callData. For any configuration if key-value
 * entry is present in CallData hashmap then preference will be given to this key-value. Otherwise
 * default of that key-value will be used from this file.
 * 
 * @author reeta
 *
 */
public class AinScfProtocolConfig implements Ss7ProtocolConfig {

	private static Logger logger= Logger.getLogger(AinScfProtocolConfig.class);

	public static final String SHARED_TOKEN_POOL_ENABLED = "SHARED_TOKEN_POOL_ENABLED";
	public static String SERVICE_CHAINING_ENABLED="SERVICE_CHAINING_ENABLED";
	public static final String DEFAULT_ANSI_ERB_SET = "DEFAULT_ERB_SET";
	public static final String CAIN_ENABLED = "CAIN_ENABLED";
	public static final String TR533_ENABLED = "TR533_ENABLED";
	public static final String ENABLE_EXTENDED_ASCII_CNAM="ENABLE_EXTENDED_ASCII_CNAM";
	public static final String AIN_ACCOUNT_CODE_STRIP_99_ENABLED="AIN_ACCOUNT_CODE_STRIP_99_ENABLED";

	private static final AinScfProtocolConfig	ainCS1ScfProtocolConfig	= new AinScfProtocolConfig();
	private static final transient HashMap<String, String>	ainConfig	= new HashMap<String, String>();

	/*
	 * List of SCCP Local addresses and associated SSN value. 
	 * These values are used to register with TCAPProvider. 
//	 */
//	private static List<SccpUserAddress>		sSccpLocalAddrList;
//	private static String []                    sLocalSsnList;
//	private static String [] 					sLocalPcList;

	
	private static Map<String,List<SccpUserAddress>> sccpLocalMapAddrList= new HashMap<String,List<SccpUserAddress>>();
	/*
	 * Protocol Variant as AIN SCF as this protocol handler implements state machine for SCF
	 */
	public static final String	PROTOCOL_VARIANT			= "AINSCF";

	/*
	 * Cluster Number is used to identify cluster and is required in case multi cluster 
	 * Deployment. This field is used in CDR. 
	 */
	public static final String	CLUSTER_NUMBER			= "CLUSTER_NUMBER";
	

	static {
		setConfigData(PROTOCOL_VARIANT, "2");
		setConfigData(CLUSTER_NUMBER, "12");
		setConfigData(SERVICE_CHAINING_ENABLED, "FALSE");
		setConfigData(DEFAULT_ANSI_ERB_SET, "ERB_BUSY|ERB_NO_ANSWER|ERB_ANSWER|ERB_DISCONNECT|ERB_NETWORK_BUSY");
		setConfigData(CAIN_ENABLED, "FALSE");
		setConfigData(TR533_ENABLED,"FALSE");
		setConfigData(ENABLE_EXTENDED_ASCII_CNAM,"1");
		setConfigData(AIN_ACCOUNT_CODE_STRIP_99_ENABLED,"FALSE");
	}

	public static AinScfProtocolConfig getInstance() {
		return ainCS1ScfProtocolConfig;
	}
	/**
	 * Application has the responsibility to initialize SS7 parameters for respective protocol 
	 * adaptor. Application shall provide Local SSN list (delimited by , in case of more than 1
	 * SSN), local PC List (delimited by , in case of more than 1). SSN and PC are mapped 1:1
	 * For example, if user define SSN as 190,191 and PC as 2-3-4,4-5-6 then 190 will be mapped
	 * with 2-3-4 and 191 with 4-5-6 respectively. 
	 * Also application need to pass PC in Cluster-Zone-Member format. 
	 * 
	 * @param localSsnList Sub-system number in CSV format, in case more then 1 SSN are involved 
	 * @param localPcList  Point code in Zone-Cluster-Member format and CSV format. SSN and Point code 
	 *                     should have one to one mapping. 
	 * @param nullServiceKeyList Service Key list. For AIN this value should be set to null.
	 */
	public static void initializeSS7Param(String serviceId, String localSsnList, String localPcList, String nullServiceKeyList) {
		if(logger.isDebugEnabled()){
			logger.debug("[PH] initializeSS7Param() Enter");
		}
		 List<SccpUserAddress> sSccpLocalAddrList =null;// new LinkedList<SccpUserAddress>();
	  	   String[] sLocalSsnList=null;
	  	    String[] sLocalPcList=null;
		try {
			if (localSsnList != null) {
				sLocalSsnList = localSsnList.split(",");
			}

			if (localPcList != null) {
				sLocalPcList = localPcList.split(",");
			}

			SccpUserAddress localAddr = null;
			SignalingPointCode localSpc = null;
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
					}
				}

				/*
				 *  creating default spc if local SPC is null
				 */
				if (localSpc == null) {
					localSpc = new SignalingPointCode(6, 64, 2);
					logger.error("[PH] initializeSS7Param: AIN CS1 PC was null, using default value 6-64-2");
				}

				String localSsn = sLocalSsnList[i];

				if (localSsn == null) {
					/*
					 * code review comment:: change default SSN to 190-->saneja
					 */
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
						/*
						 *  logging local PC/SSN
						 */
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
//		return null;
//	}

	/**
	 * Maintains configuration values as key-value pair. This method can be used to fetch param value 
	 * using param key. 
	 * @param key Identifier for param 
	 * @return Param value for respective param key
	 */
	public static String getConfigData(String key) {
		return ainConfig.get(key);
	}

	/**
	 * This method can be used to set key value pair
	 * @param key identifier for param 
	 * @param value Param value 
	 */
	public static void setConfigData(String key, String value) {
		if (logger.isDebugEnabled()) {
			logger.debug("setConfigData  key: "+ key+" value: "+value);
		}
		ainConfig.put(key, value);
	}

	/* (non-Javadoc)
	 * @see com.agnity.ph.common.Ss7ProtocolConfig#getSccpLocalAddressList()
	 */
//	@Override
//	public List<SccpUserAddress>  getSccpLocalAddressList() {
//		return sSccpLocalAddrList;
//	}

	@Override
	public List<String> getServiceKeyList(String serviceId) {
	return	null;
	}

	@Override
	public List<SccpUserAddress> getSccpLocalAddressList(String serviceId) {
		if(logger.isDebugEnabled()) {
			logger.debug("Fetching local address for service id :- "+ serviceId);
		}
		return sccpLocalMapAddrList.get(serviceId);
		
	}
	/* (non-Javadoc)
	 * @see com.agnity.ph.common.Ss7ProtocolConfig#getProtocol()
	 */
	@Override
	public Protocol getProtocol() {
		// TODO Auto-generated method stub
		return Protocol.AIN_SCF;
	}

}
