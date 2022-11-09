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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.sip.SipFactory;
import javax.servlet.sip.SipSessionsUtil;
import javax.servlet.sip.TimerService;

import com.baypackets.ase.enumclient.EnumClient;
import com.baypackets.ase.monitor.AseComponentMonitorManager;
import com.baypackets.ase.monitor.CallStatsProcessor;
//import com.baypackets.ase.ra.diameter.rf.RfResourceFactory;
import com.baypackets.ase.ra.diameter.ro.RoResourceFactory;
import com.baypackets.ase.ra.enumserver.EnumResourceAdaptor;
import com.baypackets.ase.ra.enumserver.EnumResourceFactory;
import com.baypackets.ase.ra.http.HttpResourceFactory;
import com.baypackets.ase.ra.diameter.sh.ShResourceFactory;
import com.baypackets.ase.ra.smpp.SmppResourceFactory;
import com.baypackets.ase.router.acm.AseAppChainManager;
import com.baypackets.ase.sbb.OutboundGatewaySelector;
import com.baypackets.ase.spi.replication.appDataRep.AppDataReplicator;
import com.baypackets.ase.util.CallTraceService;
import com.baypackets.ase.util.stpool.AseSharedTokenPool;
import com.genband.tcap.provider.TcapFactory;
import com.genband.tcap.provider.TcapListener;
import com.genband.tcap.provider.TcapProvider;

import fr.marben.diameter.DiameterMessageFactory;
import fr.marben.diameter._3gpp.ro.DiameterRoMessageFactory;

/**
 * This is a singleton class. It contains the SAS services that can be used by
 * protocol handlers 
 * 
 */
public class PhUtilityServices {

	private static final PhUtilityServices phUtilityServices = new PhUtilityServices();
	
	private static Map<String,PhUtilityServices> instances = new HashMap<String,PhUtilityServices>();

	/*
	 *  Utility service references
	 */
	
	/*
	 * cTimerService: Reference to SAS Timer service
	 */
	private transient TimerService timerService;

	protected transient CallTraceService callTraceService;

	/*
	 * PH Servlet Context
	 */
	private transient ServletContext servletContext;

	/*
	 * Set this flag to true only if service has been properly initialized.
	 */
	private boolean serviceInitialized;

	/*
	 * SAS correlation MAP which contains correlationId and App-Session for Sip
	 * REDIRECT+B2BUA calls
	 */
	private transient Map<String, Object> correlationMap;

	// SipSessionsUtil: Utility to obtain AppSession from AppSessionId
	private transient SipSessionsUtil sipSessionsUtil;

	// Data replicator reference
	private transient AppDataReplicator appDataReplicator;

	// Sip Factory: To create new requests like initial INVITE and SIP URIs
	private transient SipFactory sipFactory;

	// TCAP Provider reference
	private transient TcapProvider tcapProvider;
	// Service instance
	private transient ServiceInterface serviceHandler;
	
	private transient OutboundGatewaySelector obgwSelector;
	
	private transient AseAppChainManager appChainManager;
	
	private transient CallStatsProcessor callStatsProcessor;
	
	protected transient HttpResourceFactory		httpResFactory;
	
	
	private transient boolean newCallsAllowed=true;
	
	AseComponentMonitorManager aseCompMonMgr;
	
	List<String> activeAppSessionIdsList=null;
	
	EnumClient enumClient=null;
	
	//RfResourceFactory rfResFactory=null;

	EnumResourceFactory enumResFactory=null;
	
	public EnumResourceFactory getEnumResFactory() {
		return enumResFactory;
	}

	public EnumClient getEnumClient() {
		return enumClient;
	}

	public void setEnumClient(EnumClient enumClnt) {
		this.enumClient = enumClnt;
	}
	
	public ShResourceFactory getShResourceFactory() {
		return shResourceFactory;
	}

	public void setShResourceFactory(ShResourceFactory shResourceFactory) {
		this.shResourceFactory = shResourceFactory;
	}


	public AseComponentMonitorManager getAseCompMonMgr() {
		return aseCompMonMgr;
	}

	public void setAseCompMonMgr(AseComponentMonitorManager aseCompMonMgr) {
		this.aseCompMonMgr = aseCompMonMgr;
	}

	public boolean isNewCallsAllowed() {
		return newCallsAllowed;
	}

	public HttpResourceFactory getHttpResFactory() {
		return httpResFactory;
	}

	public void setHttpResFactory(HttpResourceFactory httpResourceFactory) {
		this.httpResFactory = httpResourceFactory;
	}

	public AseSharedTokenPool getSharedTokenPool() {
		return sharedTokenPool;
	}

	public void setSharedTokenPool(AseSharedTokenPool sharedTokenPool) {
		this.sharedTokenPool = sharedTokenPool;
	}

	private transient AseSharedTokenPool sharedTokenPool;
	
	
	public OutboundGatewaySelector getObgwSelector() {
		return obgwSelector;
	}

	public void setObgwSelector(OutboundGatewaySelector obgwSelector) {
		this.obgwSelector = obgwSelector;
	}

	// TCAP Factory 
	private transient TcapFactory tcapFactory;
	
	// Tcap Listener reference
	
	private transient TcapListener tcapListener;

	private RoResourceFactory roResFactory;

	private SmppResourceFactory smppResFactory;
	
	private ShResourceFactory shResourceFactory;
	
	public SmppResourceFactory getSmppResFactory() {
		return smppResFactory;
	}

	public void setSmppResFactory(SmppResourceFactory smppResourceFactory) {
		this.smppResFactory = smppResourceFactory;
	}

	public static PhUtilityServices getInstance(String serviceId) {

		PhUtilityServices pus = null;
		if (instances.get(serviceId) == null) {
			pus = new PhUtilityServices();
			instances.put(serviceId, pus);
		} else {
			pus = instances.get(serviceId);
		}
		return pus;
		// return phUtilityServices;
	}

	public TcapListener getTcapListener() {
		return tcapListener;
	}
	
	public void setTcapListener(TcapListener tcapListener) {
		this.tcapListener = tcapListener;
	}
	
	
	public TcapFactory getTcapFactory() {
		return tcapFactory;
	}
	
	public void setTcapFactory(TcapFactory tcapFactory) {
		this.tcapFactory = tcapFactory;
	}
	
	public TimerService getTimerService() {
		return timerService;
	}

	public void setTimerService(TimerService cTimerService) {
		this.timerService = cTimerService;
	}

	public ServletContext getServletContext() {
		return servletContext;
	}

	public void setServletContext(ServletContext servletContext) {
		this.servletContext = servletContext;
	}

	public boolean isServiceInitialized() {
		return serviceInitialized;
	}

	public void setServiceInitialized(boolean serviceInitialized) {
		this.serviceInitialized = serviceInitialized;
	}

	public Map<String, Object> getCorrelationMap() {
		return correlationMap;
	}

	public void setCorrelationMap(Map<String, Object> correlationMap) {
		this.correlationMap = correlationMap;
	}

	public SipSessionsUtil getSipSessionsUtil() {
		return sipSessionsUtil;
	}

	public void setSipSessionsUtil(SipSessionsUtil sipSessionsUtil) {
		this.sipSessionsUtil = sipSessionsUtil;
	}

	public AppDataReplicator getAppDataReplicator() {
		return appDataReplicator;
	}

	public void setAppDataReplicator(AppDataReplicator appDataReplicator) {
		this.appDataReplicator = appDataReplicator;
	}

	public SipFactory getSipFactory() {
		return sipFactory;
	}

	public void setSipFactory(SipFactory sipFactory) {
		this.sipFactory = sipFactory;
	}

	public CallTraceService getCallTraceService() {
		return callTraceService;
	}

	public void setCallTraceService(CallTraceService callTraceService) {
		this.callTraceService = callTraceService;
	}

	public TcapProvider getTcapProvider() {
		return tcapProvider;
	}

	public void setTcapProvider(TcapProvider tcapProvider) {
		this.tcapProvider = tcapProvider;
	}

	public ServiceInterface getServiceHandler() {
		return serviceHandler;
	}

	public void setServiceHandler(ServiceInterface serviceHandler) {
		this.serviceHandler = serviceHandler;
	}

	public void setAppChainManager(AseAppChainManager acm) {
		this.appChainManager=acm;
		
	}

	public AseAppChainManager getAppChainManager() {
		return appChainManager;
	}

	public CallStatsProcessor getCallStatsProcessor() {
		return callStatsProcessor;
	}

	public void setCallStatsProcessor(CallStatsProcessor callStatsProcessor) {
		this.callStatsProcessor=callStatsProcessor;
		
	}

	public void setNewCallsAllowed(boolean callAllowed) {
		newCallsAllowed=callAllowed;
		
	}

	public void setActiveAppSessionIdList(List<String> activeAppSessionIdsList) {
		this.activeAppSessionIdsList=activeAppSessionIdsList;
	}
	
	public List<String> getActiveAppSessionIdsList() {
		return activeAppSessionIdsList;
	}

//	public void setRfResFactory(RfResourceFactory rfResFactry) {
//		rfResFactory=rfResFactry;	
//	}
//	
//	public RfResourceFactory getRfResFactory() {
//		return rfResFactory;
//	}
	
	public void setRoResFactory(RoResourceFactory roResFactry) {
		roResFactory=roResFactry;	
	}
	
	public RoResourceFactory getRoResFactory() {
		return roResFactory;
	}

	public void setEnumResFactory(EnumResourceFactory enumResFactory) {
		this.enumResFactory=enumResFactory;
		
	}
	
	public DiameterRoMessageFactory getDiameterRoMesageFactory(){
		if(roResFactory!=null){
			return roResFactory.getDiameterRoMessageFactory();
		}
		return null;
		
	}
	
	public DiameterMessageFactory getDiameterBaseMesageFactory(){
		if(roResFactory!=null){
			return roResFactory.getDiameterBaseMessageFactory();
		}
		return null;
		
	}
}
