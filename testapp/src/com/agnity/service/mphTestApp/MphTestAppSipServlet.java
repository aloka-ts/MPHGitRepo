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

import jain.protocol.ss7.tcap.TcapUserAddress;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.ainscf.AinScfProtocolConfig;
import com.agnity.ph.capv2scf.Capv2ScfProtocolConfig;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.ProtocolHandlerServlet;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.Ss7ProtocolConfig;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolConfig;
import com.agnity.ph.mapscf.MapScfProtocolConfig;
import com.agnity.ph.sip.SipProtocolConfig;
import com.baypackets.ase.ra.enumserver.message.EnumRequest;

/*
 * This class should only implement the abstract methods of ProtocolHandlerServlet
 */
public class MphTestAppSipServlet extends ProtocolHandlerServlet {
	/**
	 * 
	 */
	private static final long serialVersionUID = -861946151766246563L;
	// logger : logger instance
	private static Logger logger = Logger.getLogger(MphTestAppSipServlet.class);

	private static final String SSN = "SSN";
	private static final String SPC = "SPC";
	private static final String SERVICE_KEY = "SERVICE_KEY";
	private static final String SS7_PROTOCOL= "SS7_PROTOCOL";
	private static final String APP_VERSION = "APP_VERSION";
	private static final String CAPV2_SSN = "CAPV2_SSN";
	private static final String AIN_SSN = "AIN_SSN";
	private static final String AIN_SPC = "AIN_SPC";
	private static final String CAPV2_SPC = "CAPV2_SPC";
	private static final String CAPV2_SKEY = "CAPV2_SKEY";
	private static final String INAP_SSN = null;
	private static final String INAP_SPC = null;
	private static final String FCI_DUMP = "FCI_DUMP";
	
	private static final String MAP_SSN = "MAP_SSN";
	private static final String MAP_SPC = "MAP_SPC";
    private static final String MAP_SKEY = "MAP_SKEY";
	

	String ssn =DEFAULT_SSN;
	String spc = DEFAULT_SPC;
	String serviceKey = DEFAULT_SRVC_KEY; // 2 for lnp
	
	public static final String DEFAULT_SSN="146";
	public static final String DEFAULT_SPC="0-3-25";
	public static final String DEFAULT_SRVC_KEY="2";
	private static final String DESTINATION_LIST = "DESTINATION_LIST";
	
	Protocol protocolUsed = Protocol.ITUINAPCS1_SCF;
	String version="2.0";
	public static String fci="";
	static List<String> destinationslist=null;
	
	List<Protocol> protocolsSupported= new ArrayList<Protocol>();

	@Override
	public ServiceInterface getServiceInstance() {
		logger.debug("Inside getServiceInstance");
			return  MphTestAppMainImpl.getInstance();

	}

	@Override
	public boolean initialize() {
		logger.debug("Inside initialize");

		String testAppProFile = System.getProperty("ase.home")
				+ "/conf/mph.properties";

		Properties p = new Properties();
		boolean isInitialized = false;
		try {
			p.load(new FileInputStream(new File(testAppProFile)));
			isInitialized = true;
		} catch (FileNotFoundException e) {
			logger.error(testAppProFile + " Not found");
		} catch (IOException e) {
			logger.error(testAppProFile + " IOException");
			
		}

		SipProtocolConfig.setConfigData(SipProtocolConfig.NOTIFY_180_WITHOUT_SDP, PhConstants.TRUE);
		SipProtocolConfig.setConfigData(SipProtocolConfig.REL_PROVISIONAL_SUPPORTED, PhConstants.TRUE);
		SipProtocolConfig.setConfigData(SipProtocolConfig.NO_INVITE_WITHOUT_SDP, PhConstants.TRUE);
		if (isInitialized) {
			if (p.getProperty(SSN) != null) {
				ssn = p.getProperty(SSN);
			}
			if (p.getProperty(SPC) != null) {
				spc = p.getProperty(SPC);
			}
			if (p.getProperty(SERVICE_KEY) != null) {
				serviceKey = p.getProperty(SERVICE_KEY);
			}
			
			if (p.getProperty(APP_VERSION) != null) {
				version = p.getProperty(APP_VERSION);
			}
			
		if (p.getProperty(DESTINATION_LIST) != null) {
			String destinations = p.getProperty(DESTINATION_LIST);
			if (!destinations.isEmpty()) {
				String[] dests = destinations.split(",");
				destinationslist = java.util.Arrays.asList(dests);
				
				logger.debug("The destinations list is  ---->"+destinationslist);
			}
		}
		
			if (p.getProperty(FCI_DUMP) != null) {
				fci = p.getProperty(FCI_DUMP);
			}
			if (logger.isDebugEnabled()) {
				logger.debug("The SSN,SPC,ServiceKey is as "+ ssn+" , "+ spc+" , "+serviceKey);
				logger.debug("The FCI dump is"+ fci);
			}
			
			String protocolsUsed = p.getProperty(SS7_PROTOCOL);

			if (protocolsUsed != null) {
				logger.info("The Supported Protocol list is  "+protocolsUsed);
				String[] protocols=protocolsUsed.split(",");
				String localssn;
				String localspc;
				for (String protocolUsedStr : protocols) {
					if (protocolUsedStr.equals(Protocol.AIN_SCF.name())) {
						this.protocolUsed = Protocol.AIN_SCF;
						if (p.getProperty(AIN_SSN) != null) {
							localssn = p.getProperty(AIN_SSN);
						}else{
							localssn=ssn;
						}
						
						if (p.getProperty(AIN_SPC) != null) {
							localspc = p.getProperty(AIN_SPC);
						}else{
							localspc=spc;
						}
						logger.info("AIN_SCF configuration: ssn = ["+localssn+"], spc = ["+spc+"]");
						AinScfProtocolConfig.initializeSS7Param(localssn, localspc, null);
						protocolsSupported.add(protocolUsed);
					} else if (protocolUsedStr.equals(Protocol.MAP_SCF.name())) {
						if(p.getProperty(MAP_SKEY) != null){
							serviceKey = p.getProperty(MAP_SKEY);
						} 
						if (p.getProperty(MAP_SSN) != null) {
							localssn = p.getProperty(MAP_SSN);
						}else{
							localssn=ssn;
						}
						if (p.getProperty(MAP_SPC) != null) {
							localspc = p.getProperty(MAP_SPC);
						}else{
							localspc=spc;
						}
						this.protocolUsed = Protocol.MAP_SCF;
						protocolsSupported.add(protocolUsed);
						logger.info("MAP_SCF configuration: skey = ["+serviceKey+"], ssn = ["+localssn+"], spc = ["+localspc+"]");
						MapScfProtocolConfig.initializeSS7Param(localssn, localspc, serviceKey);
					} else if (protocolUsedStr.equals(Protocol.CAPV2_SCF.name())) {
						if(p.getProperty(CAPV2_SKEY) != null){
							serviceKey = p.getProperty(CAPV2_SKEY);
						} 
						if (p.getProperty(CAPV2_SSN) != null) {
							localssn = p.getProperty(CAPV2_SSN);
						}else{
							localssn=ssn;
						}
						if (p.getProperty(CAPV2_SPC) != null) {
							localspc = p.getProperty(CAPV2_SPC);
						}else{
							localspc=spc;
						}
						this.protocolUsed = Protocol.CAPV2_SCF;
						protocolsSupported.add(protocolUsed);
						logger.info("CAPV2_SCF configuration: skey = ["+serviceKey+"], ssn = ["+localssn+"], spc = ["+localspc+"]");
						Capv2ScfProtocolConfig.initializeSS7Param(localssn, localspc, serviceKey);
					} else if (protocolUsedStr.equals(Protocol.ITUINAPCS1_SCF.name())) {
						if (p.getProperty(INAP_SSN) != null) {
							localssn = p.getProperty(INAP_SSN);
						}else{
							localssn=ssn;
						}
						
						if (p.getProperty(INAP_SPC) != null) {
							localspc = p.getProperty(INAP_SPC);
						}else{
							localspc=spc;
						}
						this.protocolUsed = Protocol.ITUINAPCS1_SCF;
						protocolsSupported.add(protocolUsed);
						logger.info("ITUINAPCS1_SCF configuration: skey = ["+serviceKey+"], ssn = ["+localssn+"], spc = ["+spc+"]");
						InapCS1ScfProtocolConfig.initializeSS7Param(localssn, localspc, serviceKey);
					}
	            }
			} else {
				
				logger.debug(" not configuring any ss7 ");
//				protocolsSupported.add(protocolUsed);
//				logger.debug("Config: pc" + spc + " ssn" + ssn + " service key "
//						+ serviceKey + " Protocol used is INAP");
//				InapCS1ScfProtocolConfig.initializeSS7Param(ssn, spc, serviceKey);
			}
		}
		
		return isInitialized;
		
	}

	@Override
	public String getApplicationVersion() {
		logger.debug("Inside getApplicationVersion with value " + version);
		return version;
	}

	public List<Ss7ProtocolConfig> getSs7ProtocolConfig() {
		logger.debug("Inside getSS7ProtocolConfig");

		List<Ss7ProtocolConfig> lst = new LinkedList<Ss7ProtocolConfig>();

		for (Protocol protocolUsed : protocolsSupported) {
			if (protocolUsed.equals(Protocol.AIN_SCF)) {
				lst.add(AinScfProtocolConfig.getInstance());
			} else if (protocolUsed.equals(Protocol.ITUINAPCS1_SCF)) {
				lst.add(InapCS1ScfProtocolConfig.getInstance());
			} else if (protocolUsed.equals(Protocol.CAPV2_SCF)) {
				lst.add(Capv2ScfProtocolConfig.getInstance());
			} else if (protocolUsed.equals(Protocol.MAP_SCF)) {  /* Remove MAP SCF sua so that we reg. with single PC*/
				lst.add(MapScfProtocolConfig.getInstance());
			}
		}
		return lst;
	}
	
	/**
	 * 
	 */
	protected static List<String> getDestinationList(){
		return destinationslist;
	}

	@Override
	public void removeUserAddress(TcapUserAddress arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean destroyed() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void handleError(int arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void handleException(Exception arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void receiveUriList(List arg0) {
		// TODO Auto-generated method stub
		
	}

}
