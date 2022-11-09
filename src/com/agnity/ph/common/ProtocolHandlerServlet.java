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

import jain.InvalidAddressException;
import jain.ListenerNotRegisteredException;
import jain.MandatoryParameterNotSetException;
import jain.protocol.ss7.JainSS7Factory;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;
import jain.protocol.ss7.UserAddressEmptyException;
import jain.protocol.ss7.UserAddressLimitException;
import jain.protocol.ss7.sccp.StateIndEvent;
import jain.protocol.ss7.sccp.management.NPCStateIndEvent;
import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.DialogueIndEvent;
import jain.protocol.ss7.tcap.TcapErrorEvent;
import jain.protocol.ss7.tcap.TcapUserAddress;
import jain.protocol.ss7.tcap.TimeOutEvent;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EventObject;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.ServletException;
import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipApplicationSessionActivationListener;
import javax.servlet.sip.SipApplicationSessionEvent;
import javax.servlet.sip.SipApplicationSessionListener;
import javax.servlet.sip.SipErrorEvent;
import javax.servlet.sip.SipErrorListener;
import javax.servlet.sip.SipFactory;
import javax.servlet.sip.SipServlet;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSessionsUtil;
import javax.servlet.sip.TimerListener;
import javax.servlet.sip.TimerService;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.apache.log4j.MDC;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.Enum.EnumProtocolHandler;
import com.agnity.ph.ainscf.AinScfProtocolConfig;
import com.agnity.ph.capv2scf.Capv2ScfProtocolConfig;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.measurement.MeasurementCounter;
import com.agnity.ph.common.measurement.PhMeasurementService;
import com.agnity.ph.common.measurement.enums.SS7Message;
import com.agnity.ph.common.measurement.enums.SipMessage;
import com.agnity.ph.diameter.DiameterConstants;
import com.agnity.ph.diameter.DiameterProtocolHandler;
import com.agnity.ph.diameter.sh.DiameterShProtocolHandler;
import com.agnity.ph.diameter.sh.DiameterShProtocolUtil;
import com.agnity.ph.http.HttpEvent;
import com.agnity.ph.http.HttpProtocolHandler;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolConfig;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil;
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolConfig;
import com.agnity.ph.mapscf.MapScfProtocolConfig;
import com.agnity.ph.sip.SipProtocolConfig;
import com.agnity.ph.sip.SipProtocolHandler;
import com.agnity.ph.sip.SipProtocolUtil;
import com.agnity.ph.smpp.SmppProtocolHandler;
import com.baypackets.ase.cdr.CDRImpl;
import com.baypackets.ase.container.AseApplicationSession;
import com.baypackets.ase.container.AseContext;
import com.baypackets.ase.enumclient.EnumClient;
import com.baypackets.ase.enumclient.EnumListener;
import com.baypackets.ase.monitor.AseCompMonitorCallbackListener;
import com.baypackets.ase.monitor.AseComponentMonitorManager;
import com.baypackets.ase.monitor.CallStatsProcessor;
import com.baypackets.ase.ra.diameter.ro.CreditControlRequest;
import com.baypackets.ase.ra.diameter.ro.RoMessage;
import com.baypackets.ase.ra.diameter.ro.RoMessageHandler;
import com.baypackets.ase.ra.diameter.ro.RoResourceFactory;
import com.baypackets.ase.ra.diameter.sh.ShResourceFactory;
import com.baypackets.ase.ra.diameter.sh.ShResponse;
import com.baypackets.ase.ra.enumserver.EnumResourceFactory;
import com.baypackets.ase.ra.enumserver.message.EnumMessage;
import com.baypackets.ase.ra.enumserver.message.EnumMessageHandler;
import com.baypackets.ase.ra.enumserver.message.EnumRequest;
import com.baypackets.ase.ra.http.HttpResourceFactory;
import com.baypackets.ase.ra.http.event.HttpResourceEvent;
import com.baypackets.ase.ra.http.message.HttpResponse;
import com.baypackets.ase.ra.smpp.SmppRequest;
import com.baypackets.ase.ra.smpp.SmppResourceFactory;
import com.baypackets.ase.ra.smpp.SmppResponse;
import com.baypackets.ase.resource.Message;
import com.baypackets.ase.resource.MessageHandler;
import com.baypackets.ase.resource.ResourceEvent;
import com.baypackets.ase.resource.ResourceException;
import com.baypackets.ase.resource.ResourceListener;
import com.baypackets.ase.router.acm.AseAppChainManager;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.OutboundGatewaySelector;
import com.baypackets.ase.spi.replication.appDataRep.AppDataReplicator;
import com.baypackets.ase.util.CallTraceService;
import com.baypackets.ase.util.stpool.AseSharedTokenPool;
import com.genband.tcap.provider.TcapListener;
import com.genband.tcap.provider.TcapProvider;
import com.genband.tcap.provider.TcapSession;
//import com.baypackets.ase.ra.diameter.rf.RfAccountingRequest;
//import com.baypackets.ase.ra.diameter.rf.RfMessageHandler;
//import com.baypackets.ase.ra.diameter.rf.RfResourceFactory;

/**
 * This class is the SIP Protocol handler servlet class . This class is the
 * abstract class which is extended by services to handle SIP events.This class
 * also implements TcapListener interface to handle TCAP traffic as well. it
 * implements processDialogIndEvent and processComponentIndEvent and delegates
 * their processing to particular protocol handlers .while initialising SS7
 * configurations this class creates mapping of SSN-SPC with the specified
 * protocol and delegates the processing of dialog and component event to the
 * specific protocol( handler) registered for the destination SSN-SPC (SCCP user
 * Address)
 */
public abstract class ProtocolHandlerServlet extends SipServlet implements
TcapListener, MessageHandler, ResourceListener, TimerListener,
SipApplicationSessionActivationListener, SipApplicationSessionListener,
ServletContextListener, SipErrorListener, 
AseCompMonitorCallbackListener, EnumListener, RoMessageHandler,
EnumMessageHandler {// RfMessageHandler

	private static final long serialVersionUID = -4044329981675286503L;

	private static Logger logger = Logger
			.getLogger(ProtocolHandlerServlet.class);

	public static final String NAME_APP_CHAIN_MGR = "com.baypackets.ase.router.acm.AseAppChainManager";
	public static final String NAME_SHARED_TOKEN_POOL = "com.baypackets.ase.util.stpool.AseSharedTokenPool";
	public static final String PROP_CALL_STATS_PROCESSOR = "com.baypackets.ase.monitor.CallStatsProcessor";
	public static final String NAME_ACMM = "com.baypackets.ase.monitor.AseComponentMonitorManager";

	public static final String CDR_KEY = "com.baypackets.ase.sbb.CDR";
	/**
	 * This method returns the Service Interface implemented by the application
	 * 
	 * @return ServiceInterface
	 */
	public abstract ServiceInterface getServiceInstance();

	/**
	 * This method is implemented by the application for initialising its
	 * properties
	 * 
	 * @return boolean if initialised Successfully or not
	 */
	public abstract boolean initialize();

	/**
	 * This method is implemented by the application for initialising its
	 * properties
	 * 
	 * @return boolean if initialised Successfully or not
	 */
	public abstract boolean destroyed();

	/**
	 * This method returns the application version
	 * 
	 * @return String
	 */
	public abstract String getApplicationVersion();

	/**
	 * This method is used to get SS7 configuration of the SS7 protocol used by
	 * application it returns list because application can use multiple SS7
	 * protocols
	 * 
	 * @return List<Ss7ProtocolConfig>
	 */
	public abstract List<Ss7ProtocolConfig> getSs7ProtocolConfig();

	protected static TcapProvider sTcapProvider;

	Map<String, Protocol> map = new HashMap<String, Protocol>();
	private Map<String, Protocol> sccpUserAddToProtocolMapping = Collections
			.synchronizedMap(map);

	static Map<String, Protocol> mapforAllApp = new HashMap<String, Protocol>();
	private static Map<String, Protocol> sccpUserAddToProtocolMappingForAllApp = Collections
			.synchronizedMap(mapforAllApp);

	ArrayList<SccpUserAddress> list = new ArrayList<SccpUserAddress>();
	private List<SccpUserAddress> sccpUserAddressList = Collections
			.synchronizedList(list);
	
	ArrayList<String> activeAppSessionsIds = new ArrayList<String>();
	private List<String> activeAppSessionIdsList = Collections
			.synchronizedList(activeAppSessionsIds);
	
	
	static Map<String, Integer> congMap = new HashMap<String, Integer>();
	static private Map<String, Integer> congestedDPCLevelMap = Collections
			.synchronizedMap(congMap);
	
	
	private boolean contextIsCreated=true;

	/**
	 * This method initialises the servlet.it creates utility services object
	 * and sets it in PhUtilityServices This method also initialises the SS7
	 * configuration provided by service and registers it self to the tcap
	 * provider to listen to SS7 protocol (INAP,AIN,CAP) events.
	 */
	@SuppressWarnings("unchecked")
	public void init() throws ServletException {
		System.out.println("Inside init method of servlet" + getName());
		super.init();

		/**
		 * Adding support for mph properties which upload sip properties
		 * currently
		 */
		
		String mphProFile = System.getProperty("ase.home")
				+ "/conf/mph.properties";

		File mphpropFileHandle = new File(mphProFile);
		if (mphpropFileHandle.exists()) {
			
			
			if (logger.isInfoEnabled()) {

				logger.info("mphprofILE EXITST insdie the exit block ....BYP");
			}
			
			System.out.println("Read mph properties  from property file !!!");
			Properties p = new Properties();
			try {
				p.load(new FileInputStream(mphpropFileHandle));
				if (!p.isEmpty()) {
					System.out
					.println("Read available properties  from property file !!!");
					Set<Entry<Object, Object>> properties = p.entrySet();
					Iterator<Entry<Object, Object>> propItr = properties
							.iterator();
					while (propItr.hasNext()) {
						Entry<Object, Object> entry = propItr.next();
						System.out.println("Reading mph property name: "
								+ entry.getKey() + " value: "
								+ entry.getValue());
						SipProtocolConfig.setConfigData(
								(String) entry.getKey(),
								(String) entry.getValue());
						InapCS1ScfProtocolConfig.setConfigData(
								(String) entry.getKey(),
								(String) entry.getValue());
						InapCS2ScfProtocolConfig.setConfigData(
								(String) entry.getKey(),
								(String) entry.getValue());
						AinScfProtocolConfig.setConfigData(
								(String) entry.getKey(),
								(String) entry.getValue());
						Capv2ScfProtocolConfig.setConfigData(
								(String) entry.getKey(),
								(String) entry.getValue());
						MapScfProtocolConfig.setConfigData(
								(String) entry.getKey(),
								(String) entry.getValue());
					}
				}
			} catch (IOException e) {
				logger.error(mphProFile + " IOException !!!!");
			}
		} 
		
		
		else {
			
			
			if (logger.isInfoEnabled()) {

				logger.info(" insdie else part of  the exit block ....BYP");
			}
			
			System.out
			.println("mph.properties file not provided taking static mph configurations");
		}
    if(contextIsCreated) 	{

    	if(logger.isDebugEnabled()) {
    		logger.debug("Context is not inilized yet so inilizing it now");
    	}
    	
    	contextIsCreated=true;
		if (logger.isDebugEnabled()) {

			logger.debug("Application name is "
					+ getServiceInstance().getApplicationName());
		}
		
		PhUtilityServices phUtilityServices = PhUtilityServices
				.getInstance(getServiceInstance().getApplicationName());

		phUtilityServices.setActiveAppSessionIdList(activeAppSessionIdsList);

		AseComponentMonitorManager monMgr = (AseComponentMonitorManager) getServletContext()
				.getAttribute(NAME_ACMM);

		if (monMgr != null) {
			SipProtocolConfig.setConfigData(SipProtocolConfig.SERVER_SITE_NAME,
					monMgr.getSiteName());
			phUtilityServices.setAseCompMonMgr(monMgr);
		}

		phUtilityServices.setServiceInitialized(initialize());
		phUtilityServices.setTimerService((TimerService) getServletContext()
				.getAttribute("javax.servlet.sip.TimerService"));
		phUtilityServices
		.setCallTraceService((CallTraceService) (getServletContext()
				.getAttribute("CallTraceService")));
		phUtilityServices.setServletContext(getServletContext());
		phUtilityServices
		.setCorrelationMap((Map<String, Object>) getServletContext()
				.getAttribute(PhConstants.CORRELATION_MAP));
		phUtilityServices
		.setSipSessionsUtil((SipSessionsUtil) getServletContext()
				.getAttribute("javax.servlet.sip.SipSessionsUtil"));
		phUtilityServices.setAppDataReplicator(new AppDataReplicator());
		phUtilityServices.setSipFactory((SipFactory) getServletContext()
				.getAttribute(SIP_FACTORY));
		PhUtilityServices
		.getInstance(getServiceInstance().getApplicationName())
		.setServiceHandler(getServiceInstance());
		PhUtilityServices
		.getInstance(getServiceInstance().getApplicationName())
		.setObgwSelector(
				(OutboundGatewaySelector) getServletContext()
				.getAttribute(
						OutboundGatewaySelector.class.getName()));
		phUtilityServices
		.setHttpResFactory((HttpResourceFactory) getServletContext()
				.getAttribute(PhConstants.HTTP_FACTORY));
		phUtilityServices.setEnumClient((EnumClient) getServletContext()
				.getAttribute(PhConstants.ENUM_CLIENT));
		// phUtilityServices.setRfResFactory((RfResourceFactory)
		// getServletContext().getAttribute(PhConstants.RF_FACTORY));
		phUtilityServices
		.setRoResFactory((RoResourceFactory) getServletContext()
				.getAttribute(PhConstants.RO_FACTORY));
		phUtilityServices
		.setEnumResFactory((EnumResourceFactory) getServletContext()
				.getAttribute(PhConstants.ENUM_FACTORY));
		phUtilityServices
		.setSmppResFactory((SmppResourceFactory) getServletContext()
				.getAttribute(PhConstants.SMPP_FACTORY));
		phUtilityServices.setShResourceFactory((ShResourceFactory) getServletContext()
				.getAttribute(PhConstants.DIAMETER_SH_FACTORY));

		if (logger.isDebugEnabled()) {

			logger.debug("DIAMETER SH Factory is :- "
					+ (ShResourceFactory) getServletContext().getAttribute(
							PhConstants.DIAMETER_SH_FACTORY));
		}
		if (logger.isDebugEnabled()) {

			logger.debug("Smpp Factory is "
					+ (SmppResourceFactory) getServletContext().getAttribute(
							PhConstants.SMPP_FACTORY));
		}
		if (logger.isDebugEnabled()) {

			logger.debug("Http resource is "
					+ (HttpResourceFactory) getServletContext().getAttribute(
							PhConstants.HTTP_FACTORY));
		}
		// set self reference: required for creating new tcap session
		phUtilityServices.setTcapListener(this);

		JainSS7Factory ss7Factory = JainSS7Factory.getInstance();
		System.out.println("Got SS7 factory instance");

		/*
		 * Initialize SS7 Protocol Provided application needs SS7 interaction.
		 */
		List<Ss7ProtocolConfig> ss7Config = getSs7ProtocolConfig();

		registerPhMeasurementCounters(ss7Config, getServiceInstance()
				.getApplicationName());

		System.out.println("Got SS7 configuration from application "
				+ getServiceInstance().getApplicationName() + " as :"
				+ ss7Config);

		if (ss7Config != null && !ss7Config.isEmpty()) {
			try {
				// Creating TCAP Provider from factory
				ss7Factory.setPathName("com.genband");
				sTcapProvider = (TcapProvider) ss7Factory
						.createSS7Object("jain.protocol.ss7.tcap.JainTcapProviderImpl");
				phUtilityServices.setTcapProvider(sTcapProvider);

				/*
				 * There could be multiple SS7 Protocols providing configuration
				 * information like service key list, OPC etc which needs to be
				 * registered.
				 */
				List<SccpUserAddress> sccpAddrList = new ArrayList<SccpUserAddress>();

				/**
				 * create set of keys to avoid duplicate keys
				 */
				Set<String> keyList = null;// new ArrayList<String>();

				for (Ss7ProtocolConfig ss7Cfg : ss7Config) {

					if (ss7Cfg.getProtocol() != Protocol.AIN_SCF) {

						if(keyList == null){
							keyList = new HashSet<String>();
						}
						if (logger.isInfoEnabled()) {
							logger.info("service key list is "+ss7Cfg.getServiceKeyList(getName()));
						}
						keyList.addAll(ss7Cfg.getServiceKeyList(getName()));		
					}
					sccpAddrList.addAll(ss7Cfg.getSccpLocalAddressList(getName()));
					sccpUserAddressList
					.addAll(ss7Cfg.getSccpLocalAddressList(getName()));
					List<SccpUserAddress> sccpUserAddList = ss7Cfg
							.getSccpLocalAddressList(getName());

					/*
					 * Adding entry of SCCP local address with the protocol used
					 * in mapping table so that this servlet can identify
					 */
					for (SccpUserAddress sccpUserAdd : sccpUserAddList) {
						SubSystemAddress subAdd = sccpUserAdd
								.getSubSystemAddress();

						StringBuilder sb = new StringBuilder();
						sb.append(subAdd.getSubSystemNumber());
						sb.append(subAdd.getSignalingPointCode().toString());

						sccpUserAddToProtocolMapping.put(sb.toString(),
								ss7Cfg.getProtocol());

						sccpUserAddToProtocolMappingForAllApp.put(
								sb.toString(), ss7Cfg.getProtocol());

						if (logger.isDebugEnabled()) {
							logger.debug("Protocol Map: key: " + sb.toString()
							+ ", value: " + ss7Cfg.getProtocol());
						}
					}
				}

				List<String> keys =null;
				if(keyList != null){

					keys = new ArrayList<String>();
					keys.addAll(keyList);
				}
				// List<String> keyList= new ArrayList<String>();
				// keyList.addAll(keySet);

				System.out
				.println("SCCP User to protocol map is configured as: "
						+ sccpUserAddToProtocolMapping);
				System.out
				.println("SCCP User to protocol map for All Apps is configured as: "
						+ sccpUserAddToProtocolMappingForAllApp);
				System.out.println("Service Key List = " + keys);

				System.out.println("SccpUserAddress List : "
						+ sccpUserAddressList + "hashcode : "
						+ sccpUserAddressList.hashCode());

				if (logger.isDebugEnabled()) {
					logger.debug("adding jainTcapListner for sccpAddrList : "
							+ sccpAddrList);
				}


				sTcapProvider.addJainTcapListener(this, sccpAddrList, keys);

				phUtilityServices.setTcapFactory(sTcapProvider
						.getTcapFactory(this));
				System.out
				.println("Setting the tcap factory in phutility services");

				System.out.println("Activating TcapListener with TcapProvider");
				sTcapProvider.tcapListenerActivated(this);
			} catch (Exception ex) {
				logger.error("Failed to register as TCAP listener", ex);
				throw new ServletException(ex);
			}
		}
	
		String isChainingEnabled = SipProtocolConfig
				.getConfigData(SipProtocolConfig.SERVICE_CHAINING_ENABLED);

		if (PhConstants.TRUE.equals(isChainingEnabled)) {

			if (logger.isDebugEnabled()) {

				logger.debug("Add service to  App chain manager" + getName());
			}

			AseAppChainManager acm = (AseAppChainManager) this
					.getServletContext().getAttribute(NAME_APP_CHAIN_MGR);

			if (logger.isDebugEnabled()) {

				logger.debug("App chain manager is : " + acm);
			}

			if (acm != null) {

				acm.addService(getName(), getServiceInstance(),
						getServletName(), this, this.getServletContext());

				phUtilityServices.setAppChainManager(acm);
			}
		}

		AseSharedTokenPool stpool = (AseSharedTokenPool) this
				.getServletContext().getAttribute(NAME_SHARED_TOKEN_POOL);

		if (logger.isDebugEnabled()) {

			logger.debug("add shared token pool " + stpool);
		}

		phUtilityServices.setSharedTokenPool(stpool);

		CallStatsProcessor callStatsProcessor = (CallStatsProcessor) this
				.getServletContext().getAttribute(PROP_CALL_STATS_PROCESSOR);

		if (logger.isDebugEnabled()) {

			logger.debug("setCallStatsProcessor " + callStatsProcessor);
		}

		phUtilityServices.setCallStatsProcessor(callStatsProcessor);

		if (logger.isDebugEnabled()) {

			logger.debug("setCallStatsProcessor " + callStatsProcessor);
		}

		if (logger.isDebugEnabled()) {

			logger.debug("Add state change listener to CM " + this);
		}
		monMgr.addStateChangeListener(this);

		if (logger.isDebugEnabled()) {

			logger.debug("leaving init of " + getName());
		}
    }
	}

	/**
	 * This method is used to initialize a servlet
	 */
	public void init(ServletContext context) {
		System.out.println("Inside init method with context of servlet");
		System.out
		.println("Do nothing as RAs are initialized in init() method");
	}

	/**
	 * This method is called when the application is undeployed on the server
	 */
	public void destroy() {
		if (logger.isInfoEnabled()) {
			logger.info("Inside destroy method of servlet . call destroyed on app as well");
		}

		
		super.destroy();

		destroyed();
	}
	

	@Override
	public final void doResponse(SipServletResponse response)
			throws ServletException, IOException {

		if (logger.isInfoEnabled()) {
			logger.info("Inside doResponse of ProtocolHandlerServlet");
		}
		try {
			switch (response.getStatus() / 100) {
			case 1:
				doProvisionalResponse(response);
				break;
			case 2:
				doSuccessResponse(response);
				break;
			case 3:
				doRedirectResponse(response);
				break;
			case 4:
			case 5:
			case 6:
				doErrorResponse(response);
				break;
			default:
				logger.error("Unknown sip response status.");
				throw new Exception("Response code : " + response.getStatus());
			}
		} catch (Exception ex) {
			logger.error("Exception in processing sip response ");
		}
	}

	/**
	 * This method is used to findout the protocol handler available for
	 * particular SCCP user Address
	 * 
	 * @param sccpUser
	 * @param dialogueId
	 * @return
	 */
	private Protocol getSS7Protocol(String sccpUser, int dialogueId) {
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId
					+ "::Find ProtocolHandler available for SCCP User : ["
					+ sccpUser + "]");
		}
		Protocol protocol = sccpUserAddToProtocolMapping.get(sccpUser);
		if (protocol == null) {
			protocol = sccpUserAddToProtocolMappingForAllApp.get(sccpUser);
		}
		return protocol;
	}

	/**
	 * This method is used to handle incoming SIP UPDATE request
	 * 
	 * @param sipRequest
	 */
	@Override
	protected void doUpdate(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Update received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doUpdate(sipRequest);
	}

	/**
	 * This method is used to handle incoming INFO SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doInfo(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: INFO received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doInfo(sipRequest);
	}

	/**
	 * This method is used to handle incoming ACK SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doAck(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: ACK received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doAck(sipRequest);
	}

	/**
	 * This method is used to handle incoming INVITE SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doInvite(SipServletRequest sipRequest)
			throws ServletException, IOException {

		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: INVITE received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);

		if (ph == null) {
			logger.error(callId
					+ ":: SIP Protocol handler not found. Disconnect call");
			sipRequest
			.createResponse(SipServletResponse.SC_SERVICE_UNAVAILABLE)
			.send();
			return;
		}
		ServiceInterface serviceHandler = getServiceInstance();
		if (serviceHandler == null) {
			logger.error(callId
					+ ":: Service instance not found for SIP call. Disconnect call");
			sipRequest
			.createResponse(SipServletResponse.SC_SERVICE_UNAVAILABLE)
			.send();
			return;
		}

		Boolean dumpCounters = Boolean.valueOf(SipProtocolConfig
				.getConfigData(SipProtocolConfig.DUMP_COUNTERS));
		if (dumpCounters) {
			PhMeasurementService
			.getInstance()
			.getMeasurementCounter(Protocol.SIP)
			.incrementServiceTriggeredCount(
					serviceHandler.getApplicationName(), false);
			PhMeasurementService.getInstance()
			.getMeasurementCounter(Protocol.SIP)
			.incrementSipMessageCount(SipMessage.REQ_INVITE);
		}

		ph.doInvite(sipRequest, serviceHandler);
	}

	/**
	 * This method is used to handle incoming CANCEL SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doCancel(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: CANCEL received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doCancel(sipRequest);
	}

	/**
	 * This method is used to handle incoming BYE SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doBye(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: BYE received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doBye(sipRequest);
	}

	/**
	 * This method is used to handle incoming ERROR SIP Response
	 * 
	 * @param sipResponse
	 */
	@Override
	public final void doErrorResponse(SipServletResponse sipResponse)
			throws ServletException, IOException {
		String callId = sipResponse.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Error response received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doErrorResponse(sipResponse);
	}

	/**
	 * This method is used to handle incoming PRACK SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doPrack(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: PRACK received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doPrack(sipRequest);
	}

	/**
	 * This method is used to handle incoming SIP Provisional Response
	 * 
	 * @param sipResponse
	 */
	@Override
	public final void doProvisionalResponse(SipServletResponse sipResponse)
			throws ServletException, IOException {
		String callId = sipResponse.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Provisional response received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doProvisionalResponse(sipResponse);
	}

	/**
	 * This method is used to handle incoming Success SIP Response
	 * 
	 * @param sipResponse
	 */
	@Override
	public final void doSuccessResponse(SipServletResponse sipResponse)
			throws ServletException, IOException {
		String callId = sipResponse.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Success response received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doSuccessResponse(sipResponse);
	}

	/**
	 * This method is used to handle incoming Redirect(302) SIP Response
	 * 
	 * @param sipResponse
	 */
	@Override
	public final void doRedirectResponse(SipServletResponse sipResponse)
			throws ServletException, IOException {
		String callId = sipResponse.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Redirect response received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doRedirectResponse(sipResponse);
	}

	/**
	 * This method is used to handle incoming Refer SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doRefer(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: doRefer received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doRefer(sipRequest);
	}

	/**
	 * This method is used to handle incoming NOTIFY SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doNotify(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: doNotify received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doNotify(sipRequest);
	}

	/**
	 * This method is used to handle incoming Options SIP Request
	 * 
	 * @param sipRequest
	 */
	@Override
	public final void doOptions(SipServletRequest sipRequest)
			throws ServletException, IOException {
		String callId = sipRequest.getCallId();
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: doOptions received");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.doOptions(sipRequest);
	}

	/**
	 * This method is used to handle Sip timer timeout processing
	 * 
	 * @param timer
	 */
	@Override
	public final void timeout(ServletTimer timer) {
		// Added for protocol specific event firing
		Object phTimerInfoObj = timer.getInfo();
		if (logger.isDebugEnabled()) {
			logger.debug("Timeout received for Timer info: " + phTimerInfoObj);
		}
		if (phTimerInfoObj != null) {
			if (phTimerInfoObj instanceof PhTimerInfo) {
				PhTimerInfo phTimerInfo = (PhTimerInfo) phTimerInfoObj;
				if (logger.isDebugEnabled()) {
					logger.debug("timeout received for TIMER: "
							+ phTimerInfo.getTimerName());
				}
				ProtocolHandler ph = ProtocolHandlerFactory
						.getProtocolHandler(phTimerInfo.getProtocol());
				ph.timeout(timer);
			} else if (phTimerInfoObj instanceof AppTimerListenerInterface) {
				AppTimerListenerInterface phTimerInfo = (AppTimerListenerInterface) phTimerInfoObj;
				if (logger.isDebugEnabled()) {
					logger.debug("timeout received for TIMER "
							+ timer.getInfo());
				}
				phTimerInfo.timerExpired(timer);
			} else if (phTimerInfoObj instanceof String) {
				// This is done because this timer is started by container which
				// does not have any info about
				// PhTimerInfo. This is Specifically ACTIVITY_TEST_TIMER.
				if (phTimerInfoObj
						.equals(com.baypackets.ase.util.Constants.ACTIVITY_TEST_TIMER)) {
					logger.debug("Got AT Timer.. Will trigger ITUINAPCS1_SCF protocolHandler");
					ProtocolHandler ph = ProtocolHandlerFactory
							.getProtocolHandler(Protocol.ITUINAPCS1_SCF);
					ph.timeout(timer);
				} else {
					logger.warn("Unsupported timer-info object: "
							+ phTimerInfoObj);
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("timerinfo not valid  so can not give timeout callback");
				}
			}
		}
	}

	/**
	 * This method is called when a session gets Passive
	 * 
	 * @param appSessionEvent
	 */
	@Override
	public void sessionWillPassivate(SipApplicationSessionEvent appSessionEvent) {

	}

	/**
	 * This method is called when a session gets action after FT
	 * 
	 * @param appSessionEvent
	 */
	@Override
	public void sessionDidActivate(SipApplicationSessionEvent appSessionEvent) {
		if (logger.isDebugEnabled()) {
			logger.debug("sessionDidActivate");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.sessionDidActivate(appSessionEvent);
	}

	/**
	 * This method is called when a PRACK is not received for a 183 response
	 * 
	 * @param sipErrorEvent
	 */
	@Override
	public void noPrackReceived(SipErrorEvent sipErrorEvent) {
		if (logger.isDebugEnabled()) {
			logger.debug("noPrackReceived");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.noPrackReceived(sipErrorEvent);
	}

	/**
	 * This method is called when ACK is not received for a 200 OK response
	 * 
	 * @param sipErrorEvent
	 */
	@Override
	public void noAckReceived(SipErrorEvent sipErrorEvent) {
		if (logger.isDebugEnabled()) {
			logger.debug("noAckReceived");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.noAckReceived(sipErrorEvent);
	}

	/**
	 * This method is called when application session gets expired
	 * 
	 * @param sase
	 */
	@Override
	public void sessionExpired(SipApplicationSessionEvent sase) {
		if (logger.isDebugEnabled()) {
			logger.debug("sessionExpired");
		}
		SipProtocolHandler ph = (SipProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.SIP);
		ph.sessionExpired(sase);
	}

	/**
	 * This method is called when an application session is created.Not used
	 * currently
	 * 
	 * @param sipapplicationsessionevent
	 */
	@Override
	public void sessionCreated(
			SipApplicationSessionEvent sipapplicationsessionevent) {

		if (logger.isDebugEnabled()) {
			logger.debug("sessionCreated add it to activeAppSessionIdsList ");
		}

		// Iterator<?>
		// sipSessions=sipapplicationsessionevent.getApplicationSession().getSessions("SIP");
		//
		// if(sipSessions!=null && sipSessions.hasNext()){
		//
		// if (logger.isDebugEnabled()) {
		// logger.debug("AppSession with SIP session found add it to active maps ");
		// }
		// activeAppSessionIdsList.add(sipapplicationsessionevent.getApplicationSession().getId());
		// }

	}

	/**
	 * This method is called when an application session is destroyed.Not used
	 * currently
	 * 
	 * @param sipapplicationsessionevent
	 */
	@Override
	public void sessionDestroyed(
			SipApplicationSessionEvent sipapplicationsessionevent) {

		if (logger.isDebugEnabled()) {
			logger.debug("sessionDestroyed remove it from activeAppSessionIdsList ");
		}
		// activeAppSessionIdsList.remove(sipapplicationsessionevent.getApplicationSession().getId());

	}

	/**
	 * This method is called when an application session is invalidated.Not used
	 * currently
	 * 
	 * @param sipapplicationsessionevent
	 */
	@Override
	public void sessionReadyToInvalidate(
			SipApplicationSessionEvent sipapplicationsessionevent) {

	}

	/**
	 * This method is called when a application context is destroyed on
	 * application undeployment
	 * 
	 * @param contextEvent
	 */
	@Override
	public void contextDestroyed(ServletContextEvent contextEvent) {
		logger.warn(getServiceInstance().getApplicationName()
				+ " servletcontext listener:: contextDestroyed");
		
		try {
			/**
			 * get ServiceKey List from SS7ProtocolConfig
			 */
			if(logger.isDebugEnabled()) {
				logger.debug("Entering stop"); 	
			}
		if(contextIsCreated) {
			contextIsCreated=false;
			List<Ss7ProtocolConfig> ss7ConfigList = getSs7ProtocolConfig();
		
			List<String> serviceKeyList = null;
			
			if(logger.isDebugEnabled()) {
				logger.debug("ss7Config list:-"+ ss7ConfigList); 	
			}
			
			for (Ss7ProtocolConfig ss7Config : ss7ConfigList) {
				if(serviceKeyList==null) {
					if (ss7Config.getProtocol() != Protocol.AIN_SCF) {
						serviceKeyList=ss7Config.getServiceKeyList(getName());	
					}	
					
				}else {
					if (ss7Config.getProtocol() != Protocol.AIN_SCF) {
						serviceKeyList.addAll(ss7Config.getServiceKeyList(getName()));	
					}
					
				}
			}
			
			if(logger.isDebugEnabled()) {
				logger.debug("serviceKeyList:- "+ ss7ConfigList); 	
			}
			if(serviceKeyList!=null && serviceKeyList.isEmpty()){
				serviceKeyList=null;
			}

			if (logger.isInfoEnabled()) {
				logger.info("serviceKeyList which was registered is "+serviceKeyList);
			}
			if (sTcapProvider != null) {
				if (logger.isInfoEnabled()) {
					logger.info("tcap provider is not null call removeTcpListener");
				}
					sTcapProvider.removeJainTcapListener(this, serviceKeyList);	
			}

			/**
			 * UnRegistering the service Id.
			 */
			PhMeasurementService measurementService = PhMeasurementService
					.getInstance();
			measurementService.getMeasurementCounter(Protocol.SIP)
			.unRegisterService(getName());
		}	
		} catch (Exception e) {
			logger.error("Error removing tcap listener " + e);
		}

//		if (sTcapProvider != null) {
//			
//			if (logger.isDebugEnabled()) {
//				logger.debug("Inside the stacprovide is not null block");
//			}
//			
//			try {
//
//				sTcapProvider.removeJainTcapListener(this, null);//.removeJainTcapListener(this);
//			} catch (ListenerNotRegisteredException e) {
//				logger.error("removeJainTcapListener ListenerNotRegisteredException"
//						+ e);
//			} catch (IOException e) {
//				logger.error("removeJainTcapListener IOException" + e);
//			}
//		}
	}

	/**
	 * This method is called when a application context is initialized
	 * 
	 * @param contextEvent
	 */
	@Override
	public void contextInitialized(ServletContextEvent contextEvent) {
		logger.warn(getServiceInstance().getApplicationName()
				+ " servletcontext listener:: contextInitialized");
		
		if (logger.isDebugEnabled()) {

			logger.debug("Application name is "
					+ getServiceInstance().getApplicationName());
		}
	    if(!contextIsCreated) 	{

	    	if(logger.isDebugEnabled()) {
	    		logger.debug("Context is not inilized yet so inilizing it now");
	    	}
	    	
	    	contextIsCreated=true;
			if (logger.isDebugEnabled()) {

				logger.debug("Application name is "
						+ getServiceInstance().getApplicationName());
			}
			
			PhUtilityServices phUtilityServices = PhUtilityServices
					.getInstance(getServiceInstance().getApplicationName());

			phUtilityServices.setActiveAppSessionIdList(activeAppSessionIdsList);

			
			// set self reference: required for creating new tcap session
			phUtilityServices.setTcapListener(this);

			JainSS7Factory ss7Factory = JainSS7Factory.getInstance();
			System.out.println("Got SS7 factory instance");

			/*
			 * Initialize SS7 Protocol Provided application needs SS7 interaction.
			 */
			List<Ss7ProtocolConfig> ss7Config = getSs7ProtocolConfig();

			registerPhMeasurementCounters(ss7Config, getServiceInstance()
					.getApplicationName());

			System.out.println("Got SS7 configuration from application "
					+ getServiceInstance().getApplicationName() + " as :"
					+ ss7Config);

			if (ss7Config != null && !ss7Config.isEmpty()) {
				try {
					// Creating TCAP Provider from factory
					ss7Factory.setPathName("com.genband");
					sTcapProvider = (TcapProvider) ss7Factory
							.createSS7Object("jain.protocol.ss7.tcap.JainTcapProviderImpl");
					phUtilityServices.setTcapProvider(sTcapProvider);

					/*
					 * There could be multiple SS7 Protocols providing configuration
					 * information like service key list, OPC etc which needs to be
					 * registered.
					 */
					List<SccpUserAddress> sccpAddrList = new ArrayList<SccpUserAddress>();

					/**
					 * create set of keys to avoid duplicate keys
					 */
					Set<String> keyList = null;// new ArrayList<String>();

					for (Ss7ProtocolConfig ss7Cfg : ss7Config) {

						if (ss7Cfg.getProtocol() != Protocol.AIN_SCF) {

							if(keyList == null){
								keyList = new HashSet<String>();
							}
							if (logger.isInfoEnabled()) {
								logger.info("service key list is "+ss7Cfg.getServiceKeyList(getName()));
							}
							keyList.addAll(ss7Cfg.getServiceKeyList(getName()));		
						}
						sccpAddrList.addAll(ss7Cfg.getSccpLocalAddressList(getName()));
						sccpUserAddressList
						.addAll(ss7Cfg.getSccpLocalAddressList(getName()));
						List<SccpUserAddress> sccpUserAddList = ss7Cfg
								.getSccpLocalAddressList(getName());

						/*
						 * Adding entry of SCCP local address with the protocol used
						 * in mapping table so that this servlet can identify
						 */
						for (SccpUserAddress sccpUserAdd : sccpUserAddList) {
							SubSystemAddress subAdd = sccpUserAdd
									.getSubSystemAddress();

							StringBuilder sb = new StringBuilder();
							sb.append(subAdd.getSubSystemNumber());
							sb.append(subAdd.getSignalingPointCode().toString());

							sccpUserAddToProtocolMapping.put(sb.toString(),
									ss7Cfg.getProtocol());

							sccpUserAddToProtocolMappingForAllApp.put(
									sb.toString(), ss7Cfg.getProtocol());

							if (logger.isDebugEnabled()) {
								logger.debug("Protocol Map: key: " + sb.toString()
								+ ", value: " + ss7Cfg.getProtocol());
							}
						}
					}

					List<String> keys =null;
					if(keyList != null){

						keys = new ArrayList<String>();
						keys.addAll(keyList);
					}
					// List<String> keyList= new ArrayList<String>();
					// keyList.addAll(keySet);

					System.out
					.println("SCCP User to protocol map is configured as: "
							+ sccpUserAddToProtocolMapping);
					System.out
					.println("SCCP User to protocol map for All Apps is configured as: "
							+ sccpUserAddToProtocolMappingForAllApp);
					System.out.println("Service Key List = " + keys);

					System.out.println("SccpUserAddress List : "
							+ sccpUserAddressList + "hashcode : "
							+ sccpUserAddressList.hashCode());

					if (logger.isDebugEnabled()) {
						logger.debug("adding jainTcapListner for sccpAddrList : "
								+ sccpAddrList);
					}
              
					sTcapProvider.addJainTcapListener(this, sccpAddrList, keys);

					phUtilityServices.setTcapFactory(sTcapProvider
							.getTcapFactory(this));
					logger.debug("Setting the tcap factory in phutility services");

					logger.debug("Activating TcapListener with TcapProvider");
					sTcapProvider.tcapListenerActivated(this);
				} catch (Exception ex) {
					logger.error("Failed to register as TCAP listener", ex);
				}
			}
	    }	
	
	}

	/**
	 * This method is not used currently
	 */
	@Override
	public String getInviteSessionId() {
		return null;
	}

	/**
	 * This method is used to get application name
	 */
	@Override
	public String getName() {

		String deployWithversion = SipProtocolConfig
				.getConfigData(SipProtocolConfig.DEPLOY_APP_NAME_WITH_VERSION);

		if (PhConstants.TRUE.equalsIgnoreCase(deployWithversion)) {
			return getServiceInstance().getApplicationName() + "_"
					+ getApplicationVersion();
		}
		return getServiceInstance().getApplicationName();

	}

	@Override
	public String getDisplayName() {

		if (logger.isDebugEnabled()) {
			logger.debug("getDisplayName --> "+ getServletContext().getServletContextName());
		}
		return getServletContext().getServletContextName();

	}

	/**
	 * This method is used to get the version of the application
	 * 
	 * @return
	 */
	@Override
	public String getVersion() {
		return getApplicationVersion();
	}

	/**
	 * This method is used to handle HTTP events from HTTP Resource
	 */
	// @Override
	// public void handleHttpEvents(AseHttpEvent aseHttpEvent) {
	//
	// }

	/**
	 * This method is currently not used.
	 */
	@Override
	public void addUserAddress(SccpUserAddress arg0)
			throws UserAddressLimitException {

	}

	/**
	 * This method is currently not used.
	 */
	@Override
	public SccpUserAddress[] getUserAddressList()
			throws UserAddressEmptyException {
		if (sccpUserAddressList != null) {
			SccpUserAddress[] addressesArray = new SccpUserAddress[sccpUserAddressList
			                                                       .size()];
			sccpUserAddressList.toArray(addressesArray);
			return addressesArray;
		}
		return null;
	}

	/**
	 * This method is currently not used.
	 */
	@SuppressWarnings("deprecation")
	@Override
	public void addUserAddress(TcapUserAddress arg0) {

	}

	/**
	 * // * This method is currently not used. //
	 */
	// @SuppressWarnings("deprecation")
	// @Override
	// public void removeUserAddress(TcapUserAddress arg0) {
	//
	// }

	/**
	 * This method is currently not used.
	 */
	@Override
	public SccpUserAddress[] processRSNUniDirIndEvent(TcapSession arg0,
			DialogueIndEvent arg1) {
		return null;
	}

	/**
	 * This method is used to handle Resource Message from a Resource This
	 * method is currently not used.
	 */
	@Override
	public void handleMessage(Message message) throws ResourceException {

		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId + ":: Inside handleMessage");
		}

		try {

			if (message instanceof HttpResponse) {
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Message is HTTP Response"
							+ message);
				}
				String httpheaderfield = "";
				HttpResponse httpResponse = (HttpResponse) message;
				appSession = httpResponse.getApplicationSession();
				callData = SipProtocolUtil.getCallData(appSession);
				origLegCallId = (String) callData
						.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

				if (origLegCallId == null) {
					origLegCallId = appSession.getId();
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Received HTTP response header is "
							+ httpheaderfield);
					logger.debug(origLegCallId
							+ ":: Received HTTP response code  is "
							+ httpResponse.getResponseCode());
					logger.debug(origLegCallId
							+ ":: Received HTTP response data is "
							+ httpResponse.getData());
					logger.debug(origLegCallId
							+ ":: Notify service that HTTP GET operation successfull");
				}

				HttpProtocolHandler ph = (HttpProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(Protocol.HTTP);
				ph.processResponse(callData, message);
			} else if (message instanceof RoMessage) {
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Diameter Message is Received");
				}
				appSession = message.getApplicationSession();
				callData = SipProtocolUtil.getCallData(appSession);
				origLegCallId = (String) callData
						.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

				if (origLegCallId == null) {
					origLegCallId = appSession.getId();
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Notify service that Diameter Answer is received...");
				}

				DiameterProtocolHandler ph = (DiameterProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(Protocol.DIAMETER);
				ph.handleResponse(message, callData, getServiceInstance());
			}else if (message instanceof ShResponse) {
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Diameter SH Message is Received");
				}
				appSession = message.getApplicationSession();
				callData = SipProtocolUtil.getCallData(appSession);
				origLegCallId = (String) callData
						.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

				if (origLegCallId == null) {
					origLegCallId = appSession.getId();
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Notify service that Diameter SH Answer is received...");
				}

				DiameterShProtocolHandler ph = (DiameterShProtocolHandler) ProtocolHandlerFactory.getProtocolHandler(Protocol.DIAMETER_SH);
				ph.handleResponse(message, callData, getServiceInstance());
				DiameterShProtocolUtil.setAppSessionTimeout(appSession, PhConstants.CLEANUP_APPSESSION_TIMEOUT, origLegCallId);
			}
			else if(message instanceof SmppResponse){
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Smpp Response is Received");
				}
				appSession = message.getApplicationSession();
				callData = SipProtocolUtil.getCallData(appSession);
				origLegCallId = (String) callData
						.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

				if (origLegCallId == null) {
					origLegCallId = appSession.getId();
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Notify service that Smpp Response is received...");
				}

				SmppProtocolHandler ph = (SmppProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(Protocol.SMPP);
				ph.handleResponse(message, callData, getServiceInstance());
			}
			
			else if(message instanceof SmppRequest){
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Smpp Request is Received");
				}
				appSession = message.getApplicationSession();
				callData = SipProtocolUtil.getCallData(appSession);
				
				if (callData == null) {
					callData = new CallData();

					callData.set(CallDataAttribute.SERVICE_ID,
							this.getName());
					callData.set(CallDataAttribute.P_APP_SESSION_ID,
							appSession.getId());

					callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
							origLegCallId);
					
					LegData leg1 = new LegData();
					callData.set(CallDataAttribute.P_LEG1, leg1);
				}

				
				CDR cdrRef = (CDR) message.getSession().getAttribute(CDR_KEY);
				
				
				if(cdrRef==null){
					cdrRef=getCDR((AseApplicationSession)appSession, message.getSession().getId());
				}
				
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: CDR ref is " + cdrRef);
				}
				
				callData.set(CallDataAttribute.P_CDR_REF, cdrRef);
				
				origLegCallId = (String) callData
						.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

				if (origLegCallId == null) {
					origLegCallId = appSession.getId();
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Notify service that Smpp Request is received...");
				}

				SmppProtocolHandler ph = (SmppProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(Protocol.SMPP);
				ph.handleRequest((SmppRequest)message, callData, getServiceInstance());
			}
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Error in handling message, drop the call "
					+ ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Error is ", ex);
			}
		}
	}

	@Override
	public void receiveEnumMessage(EnumMessage message) {

		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "" + message.getMessageId();

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId + ":: Inside receiveEnumMessage");

			try {
				if (message instanceof EnumRequest) {

					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ ":: Message is ENUM Request" + message);
					}

					EnumRequest enumReq = (EnumRequest) message;
					appSession = enumReq.getApplicationSession();
					callData = SipProtocolUtil.getCallData(appSession);
					if (callData == null) {
						callData = new CallData();

						callData.set(CallDataAttribute.SERVICE_ID,
								this.getName());
						callData.set(CallDataAttribute.P_APP_SESSION_ID,
								appSession.getId());

						callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
								origLegCallId);
					}

					
					CDR cdrRef = (CDR) message.getSession().getAttribute(CDR_KEY);
					
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ ":: CDR ref is " + cdrRef);
					}
					callData.set(CallDataAttribute.P_CDR_REF, cdrRef);
					
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Received Enum Request ");
						logger.debug(origLegCallId
								+ ":: Notify service for incoming ENUMRequest");
					}
					EnumProtocolHandler ph = (EnumProtocolHandler) ProtocolHandlerFactory
							.getProtocolHandler(Protocol.ENUM);
					ph.handleRequest(callData, enumReq, getServiceInstance());
				}
			} catch (Exception ex) {
				logger.error(origLegCallId
						+ ":: Error in handling message, drop the call "
						+ ex.getMessage());
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Error is ", ex);
				}
			}
		}
	}

	@Override
	public void receiveEnumMessage(EnumRequest request) {
		// TODO Auto-generated method stub

	}

	/**
	 * This method is used to handle CCR event
	 */
	public void handleEventCCRRequest(CreditControlRequest ccRequest)
			throws ResourceException {
		if (logger.isInfoEnabled()) {
			logger.info("Entering handleEventCCRRequest of RoMessageHandler");
		}//
		SipApplicationSession appSession = ccRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);

		if (callData == null) {
			callData = new CallData();

			LegData leg1 = new LegData();
			leg1.set(LegDataAttributes.DIAMETER_IN_CCR_REQ, ccRequest);
			callData.set(CallDataAttribute.P_LEG1, leg1);
			callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
					ccRequest.getSessionId());
			callData.set(CallDataAttribute.SERVICE_ID, this.getName());
			callData.set(CallDataAttribute.P_APP_SESSION_ID, appSession.getId());
			callData.set(CallDataAttribute.P_PROTOCOL, Protocol.DIAMETER);
			appSession.setAttribute(CallData.CALL_DATA, callData);
			
            CDR cdrRef = (CDR) ccRequest.getSession().getAttribute(CDR_KEY);
			
			
			if(cdrRef==null){
				cdrRef=getCDR((AseApplicationSession)appSession, ccRequest.getSession().getId());
			}
			
			if (logger.isInfoEnabled()) {
				logger.info(":: CDR ref is " + cdrRef);
			}
			callData.set(CallDataAttribute.P_CDR_REF, cdrRef);
		}

		DiameterProtocolHandler ph = (DiameterProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.DIAMETER);
		try {
			ph.handleRequest(callData, ccRequest,
					DiameterConstants.CCR_IN_EVENT, getServiceInstance());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * This method is used to handle initial CCR request
	 */
	public void handleInitialCCRRequest(CreditControlRequest ccRequest)
			throws ResourceException {
		if (logger.isInfoEnabled()) {
			logger.info("Entering handleInitialCCRRequest of RoMessageHandler");
		}

		SipApplicationSession appSession = ccRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);

		if (callData == null) {
			callData = new CallData();

			LegData leg1 = new LegData();
			leg1.set(LegDataAttributes.DIAMETER_IN_CCR_REQ, ccRequest);
			callData.set(CallDataAttribute.P_LEG1, leg1);
			callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
					ccRequest.getSessionId());
			callData.set(CallDataAttribute.SERVICE_ID, this.getName());
			callData.set(CallDataAttribute.P_APP_SESSION_ID, appSession.getId());
			callData.set(CallDataAttribute.P_PROTOCOL, Protocol.DIAMETER);
			appSession.setAttribute(CallData.CALL_DATA, callData);
			
			
			CDR cdrRef = (CDR) ccRequest.getSession().getAttribute(CDR_KEY);
			
			
			if(cdrRef==null){
				cdrRef=getCDR((AseApplicationSession)appSession, ccRequest.getSession().getId());
			}
			
			if (logger.isInfoEnabled()) {
				logger.info(":: CDR ref is " + cdrRef);
			}
			callData.set(CallDataAttribute.P_CDR_REF, cdrRef);
			
		}

		DiameterProtocolHandler ph = (DiameterProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.DIAMETER);
		try {
			ph.handleRequest(callData, ccRequest,
					DiameterConstants.CCR_IN_INITIAL, getServiceInstance());
		} catch (Exception e) {
			logger.error("handleInitialCCRRequest: Error in Processing Diameter Request "
					+ e);
			//e.printStackTrace();
		}

	}

	/**
	 * This method is used to handle interim CCR request
	 */
	public void handleInterimCCRRequest(CreditControlRequest ccRequest)
			throws ResourceException {
		if (logger.isInfoEnabled()) {
			logger.info("Entering handleInterimCCRRequest of RoMessageHandler");
		}
		SipApplicationSession appSession = ccRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);

		if (callData == null) {
			callData = new CallData();

			LegData leg1 = new LegData();

			callData.set(CallDataAttribute.P_LEG1, leg1);
			callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
					ccRequest.getSessionId());
			callData.set(CallDataAttribute.SERVICE_ID, this.getName());
			callData.set(CallDataAttribute.P_APP_SESSION_ID, appSession.getId());
			callData.set(CallDataAttribute.P_PROTOCOL, Protocol.DIAMETER);
			appSession.setAttribute(CallData.CALL_DATA, callData);
		}else{

			LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
			leg1.set(LegDataAttributes.DIAMETER_IN_CCR_REQ, ccRequest);
//			callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
//					ccRequest.getSessionId());
		}
		DiameterProtocolHandler ph = (DiameterProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.DIAMETER);
		try {
			ph.handleRequest(callData, ccRequest,
					DiameterConstants.CCR_IN_INERIM, getServiceInstance());
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("handleInterimCCRRequest: Error in Processing Diameter Request: "
					+ ccRequest.getSessionId() +" Exception is "+e.getMessage());
		}
	}

	/**
	 * This method is used to handle CCR termination request
	 */
	public void handleTerminationCCRRequest(CreditControlRequest ccRequest)
			throws ResourceException {
		if (logger.isInfoEnabled()) {
			logger.info("Entering handleTerminationCCRRequest of RoMessageHandler");
		}

		SipApplicationSession appSession = ccRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);

		if (callData == null) {
			callData = new CallData();

			LegData leg1 = new LegData();
			leg1.set(LegDataAttributes.DIAMETER_IN_CCR_REQ, ccRequest);
			callData.set(CallDataAttribute.P_LEG1, leg1);
			callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
					ccRequest.getSessionId());
			callData.set(CallDataAttribute.SERVICE_ID, this.getName());
			callData.set(CallDataAttribute.P_APP_SESSION_ID, appSession.getId());
			callData.set(CallDataAttribute.P_PROTOCOL, Protocol.DIAMETER);
			appSession.setAttribute(CallData.CALL_DATA, callData);
		}else{

			LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
			leg1.set(LegDataAttributes.DIAMETER_IN_CCR_REQ, ccRequest);
//			callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
//					ccRequest.getSessionId());
		}
		DiameterProtocolHandler ph = (DiameterProtocolHandler) ProtocolHandlerFactory
				.getProtocolHandler(Protocol.DIAMETER);
		try {
			ph.handleRequest(callData, ccRequest,
					DiameterConstants.CCR_IN_TERMINATION, getServiceInstance());
		} catch (Exception e) {
			logger.error("handleTerminationCCRRequest: Error in Processing Diameter Request: "
					+ ccRequest.getSessionId()+" Exception is "+e);
			e.printStackTrace();
		}
	}

	// /**
	// * This method is used to handle event record
	// */
	// public void handleEventRecordRequest(RfAccountingRequest accountingReq)
	// throws ResourceException {
	//
	// if (logger.isInfoEnabled()) {
	// logger.info("Entering handleEventRecordRequest");
	// }
	// SipApplicationSession appSession = accountingReq.getApplicationSession();
	// CallData callData = SipProtocolUtil.getCallData(appSession);
	//
	// if(callData==null){
	// callData= new CallData();
	// }
	//
	// DiameterProtocolHandler ph = (DiameterProtocolHandler)
	// ProtocolHandlerFactory.getProtocolHandler(Protocol.DIAMETER);
	// try {
	// ph.handleRequest(callData, accountingReq,
	// DiameterConstants.AR_EVENT_RECORD,getServiceInstance());
	// } catch (Exception e) {
	// logger.error("handleEventRecordRequest: Error in getting Diameter Protocol Handler ");
	// e.printStackTrace();
	// }
	// }
	//
	// /**
	// * This method is used to handle start record request
	// */
	// public void handleStartRecordRequest(RfAccountingRequest accountingReq)
	// throws ResourceException {
	// if (logger.isInfoEnabled()) {
	// logger.info("Entering handleStartRecordRequest");
	// }
	// SipApplicationSession appSession = accountingReq.getApplicationSession();
	// CallData callData = SipProtocolUtil.getCallData(appSession);
	//
	// if(callData==null){
	// callData= new CallData();
	// }
	//
	// DiameterProtocolHandler ph = (DiameterProtocolHandler)
	// ProtocolHandlerFactory.getProtocolHandler(Protocol.DIAMETER);
	// try {
	// ph.handleRequest(callData, accountingReq,
	// DiameterConstants.AR_START_RECORD,getServiceInstance());
	// } catch (Exception e) {
	// logger.error("handleStartRecordRequest: Error in getting Diameter Protocol Handler ");
	// e.printStackTrace();
	// }
	// }
	//
	// /**
	// * This method is used to handle interim record request
	// */
	// public void handleInterimRecordRequest(RfAccountingRequest accountingReq)
	// throws ResourceException {
	// if (logger.isInfoEnabled()) {
	// logger.info("Entering handleInterimRecordRequest");
	// }
	// SipApplicationSession appSession = accountingReq.getApplicationSession();
	// CallData callData = SipProtocolUtil.getCallData(appSession);
	//
	// if(callData==null){
	// callData= new CallData();
	// }
	//
	// DiameterProtocolHandler ph = (DiameterProtocolHandler)
	// ProtocolHandlerFactory.getProtocolHandler(Protocol.DIAMETER);
	// try {
	// ph.handleRequest(callData, accountingReq,
	// DiameterConstants.AR_INERIM_RECORD,getServiceInstance());
	// } catch (Exception e) {
	// logger.error("handleInterimRecordRequest: Error in getting Diameter Protocol Handler ");
	// e.printStackTrace();
	// }
	// }
	//
	// /**
	// * This method is used to handle stop record request
	// */
	// public void handleStopRecordRequest(RfAccountingRequest accountingReq)
	// throws ResourceException {
	// if (logger.isInfoEnabled()) {
	// logger.info("Entering handleStopRecordRequest ");
	// }
	// SipApplicationSession appSession = accountingReq.getApplicationSession();
	// CallData callData = SipProtocolUtil.getCallData(appSession);
	//
	// if(callData==null){
	// callData= new CallData();
	// }
	//
	// DiameterProtocolHandler ph = (DiameterProtocolHandler)
	// ProtocolHandlerFactory.getProtocolHandler(Protocol.DIAMETER);
	// try {
	// ph.handleRequest(callData, accountingReq,
	// DiameterConstants.AR_STOP_RECORD,getServiceInstance());
	// } catch (Exception e) {
	// logger.error("handleStopRecordRequest: Error in getting Diameter Protocol Handler ");
	// e.printStackTrace();
	// }
	// }
	/**
	 * This method is used to handle Resource event generated by Resource
	 * Adaptor in case of any errors in performing operations using resource
	 * This method is currently not used.
	 */
	@Override
	public void handleEvent(ResourceEvent event) throws ResourceException {

		String origLegCallId = "0";

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId + ":: Inside handleMessage");
		}

		try {

			if (event instanceof HttpResourceEvent) {

				HttpResourceEvent resEvent = (HttpResourceEvent) event;

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: HTP Event received is : "
							+ event.getType());
				}

				HttpProtocolHandler ph = (HttpProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(Protocol.HTTP);
				ph.processResourceEvent(resEvent);
			}
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Error in handling message, drop the call "
					+ ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Error is ", ex);
			}
		}

	}

	/**
	 * Tcaplistener API implemented by the service for receiving a callback on a
	 * dialogue indication event occurring on the node.This method check the
	 * primitive type for the dialogue indication event.
	 * <ul>
	 * <li>If Primitive Type is PRIMITIVE_BEGIN for a new call then New calldata
	 * is created and set in TCAP session.Store the tcap session reference in
	 * appSession as in timeout it would be required. If this PRIMITIVE_BEGIN is
	 * for an existing call then validate the message for FSM state. After
	 * validation call the processBegin method of InapSmHelper to get next
	 * action to be executed. For Invalid dialog BEGIN, drop call.
	 * <li>For primitive type PRIMITIVE_CONTINUE validate the FSM state and call
	 * processContinue method of InapSmHelper to get next action to be executed.
	 * For Invalid dialogue CONTINUE, drop call.
	 * <li>For primitive type PRIMITIVE_END validate the FSM state and call
	 * processEnd method of InapSmHelper to get next action to be executed.For
	 * end message no need to execute processing failure action
	 * executeAction(tcapSession, action).Just check if call needs to be cleaned
	 * now or after processing components,drop call if no component is
	 * present.For Invalid dialogue END, drop call.
	 * <li>For primitive type PRIMITIVE_USER_ABORT action not executed as
	 * already term message is received executeAction(tcapSession, action).Clean
	 * call if no component is present and notify service and write CDRs.
	 * <li>For primitive type PRIMITIVE_PROVIDER_ABORT and PRIMITIVE_NOTICE
	 * action not executed as already term message is received
	 * executeAction(tcapSession, action).Clean call if no component is present
	 * and notify service and write CDRs.
	 * <li>For unrecognised Primitive Type execute drop action with force false.
	 * </ul>
	 * Always store dialogue primitive type and Increment Interconnection Count
	 * before returning from the method. For MandatoryParameterNotSetException
	 * for dialogue event or Failed to process Dialogue Indication event release
	 * the call with CV=41.
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @see jain.protocol.ss7.tcap.JainTcapListener#processDialogueIndEvent(jain.protocol.ss7.tcap.DialogueIndEvent)
	 */
	@Override
	public void processDialogueIndEvent(DialogueIndEvent dialogueIndEvent) {
		int dialogueId = 0;

		try {
			dialogueId = dialogueIndEvent.getDialogueId();
			// check for outgoing dialogue. In case the dialogue id starts 
			// with 99 then fetch the original Dialogue id after removing 99 
			// from it 
	       String dialogIdStr=""+dialogueId;
			
			//incoming Dlg ID:991100104 correlated DlgId:1100104 for dialout idp case
			
			if(dialogIdStr.startsWith("99")&& dialogIdStr.length()>7){
				
				dialogueId = checkForCorrelatedDlgId(dialogueId);
			}
			
	    	        
			MDC.put(PhConstants.MDC_CALL_ID_CONST, "[" + dialogueId + "]");

			if (logger.isDebugEnabled()) {
				logger.debug("Processing Dialogue Indication Event of type: ["
						+ dialogueIndEvent.getPrimitiveType() + "]");
			}

			SS7ProtocolHandler ph = null;
			Protocol protocol = null;
			SccpUserAddress userAdd = dialogueIndEvent.getDestinationAddress();
			if (userAdd == null) {
				TcapSession tcapSession = PhUtilityServices
						.getInstance(getServiceInstance().getApplicationName())
						.getTcapProvider().getTcapSession(dialogueId);
				CallData callData = (CallData) tcapSession
						.getAttribute(CallData.CALL_DATA);
				if (callData != null) {
					if (callData.get(CallDataAttribute.P_PROTOCOL) != null) {
						protocol = (Protocol) callData
								.get(CallDataAttribute.P_PROTOCOL);
						if (logger.isDebugEnabled()) {
							logger.debug("Processing Dialog Indication Event for Protocol :["
									+ protocol + "]");
						}
						ph = (SS7ProtocolHandler) ProtocolHandlerFactory
								.getProtocolHandler(protocol);
					} else if (callData
							.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS) != null) {
						SccpUserAddress sscpUserAdd = (SccpUserAddress) callData
								.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);
						if (logger.isDebugEnabled()) {
							logger.debug("Processing Dialog Indication Event for SUA :["
									+ sscpUserAdd + "]");
						}
						SubSystemAddress subAdd = sscpUserAdd
								.getSubSystemAddress();
						StringBuilder sb = new StringBuilder();
						sb.append(subAdd.getSubSystemNumber());
						sb.append(subAdd.getSignalingPointCode().toString());
						protocol = getSS7Protocol(sb.toString(), dialogueId);
					}
				}
			} else {
				SubSystemAddress subAdd = userAdd.getSubSystemAddress();
				StringBuilder sb = new StringBuilder();
				sb.append(subAdd.getSubSystemNumber());
				sb.append(subAdd.getSignalingPointCode().toString());

				protocol = getSS7Protocol(sb.toString(), dialogueId);
			}

			ph = (SS7ProtocolHandler) ProtocolHandlerFactory
					.getProtocolHandler(protocol);
			if (ph == null) {
				logger.error("Protocol handler not found for protocol "
						+ protocol + ". Disconnect call");
				// dropCall with sending Abort.
				return;
			}

			ServiceInterface serviceHandler = getServiceInstance();
			if (serviceHandler == null) {
				logger.error("Service instance not found for ITUINAPCS1_SCF call. Disconnect call");
				// dropCall with sending Abort.
				return;
			}

			incrementTcapCounters(protocol, dialogueIndEvent);
			ph.processDialogueIndEvent(dialogueIndEvent, serviceHandler);
		} catch (MandatoryParameterNotSetException e) {
			logger.error(
					"MandatoryParameterNotSetException for dialogue event", e);
		} catch (Exception ex) {
			logger.error("Failed to process Dialogue Indication event", ex);
		} finally {
			MDC.remove(PhConstants.MDC_CALL_ID_CONST);
		}
	}

	/**
	 * This method is used to process component indication event for a SS7
	 * message. The processing of component is delegated to particular SS7
	 * protocol handler depdning upon its destination SCCP addess registered for
	 * particular protocol
	 */
	@Override
	public void processComponentIndEvent(ComponentIndEvent componentIndEvent) {
		int dialogueId = 0;
		try {
			dialogueId = componentIndEvent.getDialogueId();
			
			// check for outgoing dialogue. In case the dialogue id starts 
			// with 99 then fetch the original Dialogue id after removing 99 
			// from it 
	       String dialogIdStr=""+dialogueId;
			
			//incoming Dlg ID:991100104 correlated DlgId:1100104 for dialout idp case
			
			if(dialogIdStr.startsWith("99")&& dialogIdStr.length()>7){
				
				dialogueId = checkForCorrelatedDlgId(dialogueId);
			}
			
			
			MDC.put(PhConstants.MDC_CALL_ID_CONST, "[" + dialogueId + "]");

			if (logger.isDebugEnabled()) {
				logger.debug("Processing Component Indication Event of type: ["
						+ componentIndEvent.getPrimitiveType() + "]");
			}

			SS7ProtocolHandler ph = null;
			TcapSession tcapSession = PhUtilityServices
					.getInstance(getServiceInstance().getApplicationName())
					.getTcapProvider().getTcapSession(dialogueId);
			CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);

			if (callData.get(CallDataAttribute.P_PROTOCOL) != null) {
				Protocol protocol = (Protocol) callData
						.get(CallDataAttribute.P_PROTOCOL);
				if (logger.isDebugEnabled()) {
					logger.debug("Processing Component Indication Event for Protocol :["
							+ protocol + "]");
				}
				ph = (SS7ProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(protocol);
			} else if (callData
					.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS) != null) {
				SccpUserAddress sscpUserAdd = (SccpUserAddress) callData
						.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);

				if (logger.isDebugEnabled()) {
					logger.debug("Processing Component Indication Event for SUA :["
							+ sscpUserAdd + "]");
				}
				SubSystemAddress subAdd = sscpUserAdd.getSubSystemAddress();
				StringBuilder sb = new StringBuilder();
				sb.append(subAdd.getSubSystemNumber());
				sb.append(subAdd.getSignalingPointCode().toString());
				Protocol protocol = getSS7Protocol(sb.toString(), dialogueId);
				ph = (SS7ProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(protocol);
			}
			if (ph == null) {
				logger.error("ITUINAPCS1_SCF Protocol handler not found. Disconnect call");
				return;
			}
			ServiceInterface serviceHandler = getServiceInstance();
			if (serviceHandler == null) {
				logger.error("Service instance not found for ITUT INAP call. Disconnect call");
				return;
			}
			ph.processComponentIndEvent(componentIndEvent, serviceHandler);
		} catch (MandatoryParameterNotSetException e) {
			logger.error(
					"MandatoryParameterNotSetException for dialogue event", e);
		} catch (Exception ex) {
			logger.error("Failed to process Component Indication event", ex);
		} finally {
			MDC.remove(PhConstants.MDC_CALL_ID_CONST);
		}
	}

	/**
	 * This method is used to handle TCAP State event This method is currently
	 * not used.
	 */
	@Override
	public void processStateIndEvent(StateIndEvent arg0) {
		
//		if (logger.isDebugEnabled()) {
//			logger.debug("processStateIndEvent :["
//					+ arg0 + "]");
//		}
//		if(arg0 instanceof NPCStateIndEvent){
//			
//			NPCStateIndEvent stateInd=(NPCStateIndEvent)arg0;
//			int level=stateInd.getCongestionLevel();
//			SignalingPointCode  affectedDPC=stateInd.getAffectedDpc();
//			String zcmFormat="";
//			try {
//				zcmFormat = affectedDPC.getZone() + "-" + affectedDPC.getCluster() + "-" + affectedDPC.getMember();
//			} catch (MandatoryParameterNotSetException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//			congestedDPCLevelMap.put(zcmFormat, Integer.valueOf(level));
			
//			if (logger.isDebugEnabled()) {
//				logger.debug("processStateIndEvent updated map is :["
//						+ congMap + "]");
//			}
//		}

	}
	
	/**
	 * This metod is ued to get congestion level for a destination SPC
	 * @param spc
	 * @return
	 */
	  public static  int getCongestionLevel(SignalingPointCode spc){
	    	
		   if(logger.isDebugEnabled())
				logger.info("getCongestionLevel ");
	    	int level=-1;
			String zcmFormat="";
			try {
				zcmFormat = spc.getZone() + "-" + spc.getCluster() + "-" + spc.getMember();
				
				if(logger.isDebugEnabled())
					logger.info("getCongestionLevel for "+ zcmFormat);
				level=congestedDPCLevelMap.get(zcmFormat);
			} catch (MandatoryParameterNotSetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return level;
	    	
	    }


	/**
	 * This method is called whne a tcap session is activated This method is
	 * currently not used.
	 */
	@Override
	public void processTcapSessionActivationEvent(TcapSession tcapSession) {

	}

	/**
	 * This method is used to process TCAP error This method is currently not
	 * used.
	 */
	@Override
	public void processTcapError(TcapErrorEvent tcapevent) {

	}

	/**
	 * This method is currently not used.
	 */
	@Override
	public void removeUserAddress(SccpUserAddress arg0)
			throws InvalidAddressException {

	}

	/**
	 * This method is currently not used.
	 */
	@Override
	public void processTimeOutEvent(TimeOutEvent timeOutEvent) {

	}

	@Override
	public void notifyNewCallsAccepted(boolean callAllowed) {
		if (logger.isDebugEnabled()) {
			logger.debug("notifyNewCallsAccepted :[" + callAllowed + "]");
		}
		PhUtilityServices
		.getInstance(getServiceInstance().getApplicationName())
		.setNewCallsAllowed(callAllowed);

	}

	/**
	 * This method is used to get all active app sessions for the application
	 * 
	 * @return
	 */
	public List<String> getActiveAppSessionIdsList() {
		if (logger.isDebugEnabled()) {
			logger.debug("getActiveAppSessionIdsList");
		}
		return activeAppSessionIdsList;
	}

	/**
	 * This method is used to get an application session for a specific
	 * applicationsessionId provided by the application
	 * 
	 * @param appSessionId
	 * @return
	 */
	public SipApplicationSession getActiveAppSessionForId(String appSessionId) {
		if (logger.isDebugEnabled()) {
			logger.debug("getActiveAppSessionForId");
		}
		return PhUtilityServices.getInstance(this.getName())
				.getSipSessionsUtil().getApplicationSessionById(appSessionId);
	}

	private void registerPhMeasurementCounters(
			List<Ss7ProtocolConfig> ss7Configs, String serviceId) {
		Boolean dumpCounters = Boolean.valueOf(SipProtocolConfig
				.getConfigData(SipProtocolConfig.DUMP_COUNTERS));
		if (!dumpCounters) {
			return;
		}
		if (logger.isDebugEnabled()) {
			logger.debug("going to register measurement counter for serviceId : "
					+ serviceId);
		}
		Protocol ss7Protocol = null;
		PhMeasurementService measurementService = PhMeasurementService
				.getInstance();
		if (ss7Configs != null) {
			for (Ss7ProtocolConfig ss7ProtocolConfig : ss7Configs) {
				ss7Protocol = ss7ProtocolConfig.getProtocol();
				measurementService
				.registerMeasurementCounters(ss7ProtocolConfig
						.getProtocol());
			}
		}
		measurementService.registerMeasurementCounters(Protocol.SIP);

		MeasurementCounter sipMeasurementCounter = measurementService
				.getMeasurementCounter(Protocol.SIP);
		sipMeasurementCounter.registerService(serviceId);

		if (ss7Protocol != null) {
			MeasurementCounter ss7MeasurementCounter = measurementService
					.getMeasurementCounter(ss7Protocol);
			ss7MeasurementCounter.registerService(serviceId);
		}

	}

	private void incrementTcapCounters(Protocol protocol,
			EventObject eventObject) {
		int primitiveType = -1;
		if (eventObject instanceof DialogueIndEvent) {
			primitiveType = ((DialogueIndEvent) eventObject).getPrimitiveType();
		} else if (eventObject instanceof ComponentIndEvent) {
			primitiveType = ((ComponentIndEvent) eventObject)
					.getPrimitiveType();
		}

		MeasurementCounter measurementCounter = PhMeasurementService
				.getInstance().getMeasurementCounter(protocol);
		if (measurementCounter == null) {
			return;
		}
		SS7Message ss7Message = SS7Message.valueOf(primitiveType);
		switch (ss7Message) {

		case PRIMITIVE_BEGIN: {
			measurementCounter.incrementSS7MessageCount(
					SS7Message.PRIMITIVE_BEGIN, true);
			break;
		}

		case PRIMITIVE_END: {
			measurementCounter.incrementSS7MessageCount(
					SS7Message.PRIMITIVE_END, true);
			break;
		}

		case PRIMITIVE_USER_ABORT: {
			measurementCounter.incrementSS7MessageCount(
					SS7Message.PRIMITIVE_USER_ABORT, true);
			break;
		}

		}
	}
	
	/**
	 * handles incoming http event if Asehttpservlet is used
	 * @param aseHttpEvent
	 */
	public void handleHttpEvents(HttpEvent aseHttpEvent) {
		String httpEventid = aseHttpEvent.getEventId();

		String origLegCallId = "";
		try {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::handleHttpEvents");
			}

			CallData callData = null;

			SipApplicationSession appSession = aseHttpEvent.getAppSession();

			if (appSession != null) {
				callData = SipProtocolUtil.getCallData(appSession);

				origLegCallId = appSession.getId();

			}

			if (callData == null) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: CallData is null seems like new HTTP request update appsessionId "
							+ aseHttpEvent.getAppSession() + " and  ServiceId "
							+ getServiceInstance().getApplicationName()
							+ " in calldata ");
				}

				callData = new CallData();
				callData.set(CallDataAttribute.SERVICE_ID, getServiceInstance()
						.getApplicationName());
				callData.set(CallDataAttribute.P_APP_SESSION_ID, aseHttpEvent
						.getAppSession().getId());
				callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID,
						origLegCallId);
				appSession.setAttribute(CallData.CALL_DATA, callData);

			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::P_HTTP_REMOTE_HOST ... "
						+ aseHttpEvent.getRemoteHost());
				logger.debug(origLegCallId + "::P_HTTP_REMOTE_PORT ... "
						+ aseHttpEvent.getRemotePort());
			}
			
			
			callData.set(CallDataAttribute.NP_HTTP_RES, aseHttpEvent.getHttpResponse());
			callData.set(CallDataAttribute.NP_HTTP_REQ, aseHttpEvent.getHttpRequest());

				HttpProtocolHandler hph = (HttpProtocolHandler) ProtocolHandlerFactory
						.getProtocolHandler(Protocol.HTTP);

				if (httpEventid.equals(HttpEvent.EVENT_HTTP_GET)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Handle HTTP GET");
					}
					hph.handleHttpGet(aseHttpEvent, callData,appSession);
				} else if (httpEventid.equals(HttpEvent.EVENT_HTTP_POST)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Handle HTTP POST");
					}
					hph.handleHttpPost(aseHttpEvent, callData,appSession);
				} else if (httpEventid.equals(HttpEvent.EVENT_HTTP_PUT)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Handle HTTP PUT");
					}
					hph.handleHttpPut(aseHttpEvent, callData);
				} else {
					throw new Exception(origLegCallId
							+ ":: Unsupported HTTP Event");
				}
			
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Http Event processing failed for event Id "
					+ httpEventid + ". Error is " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(origLegCallId
						+ ":: Error in handleHttpEvents for event id "
						+ httpEventid, ex);
			}
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::handleHttpEvents Leaving");
		}
	}
	
	/**
	 * This method is called in case of outgoing dialogue 
	 * id especially if original call leg is ss7. While 
	 * generating dialogue id for Begin, application adds
	 * 99 to incoming dialogue id. This method check for 99 in incoming dialogue id
	 * and remove it so that original call leg can be hit. 
	 * @return
	 */
	private int checkForCorrelatedDlgId(int dialogueId){
		int originalDlgId= 0;
		
		// check for outgoing dialogue. In case the dialogue id starts 
		// with 99 then fetch the original Dialogue id after removing 99 
		// from it 
        if(StringUtils.startsWith(Integer.toString(dialogueId), "99")){
        	originalDlgId = Integer.parseInt(StringUtils.substring(Integer.toString(dialogueId), 2));
        	
        	if(logger.isDebugEnabled()){
        		logger.debug("Incoming Dlg ID:" + dialogueId + " correlated DlgId:" + originalDlgId);
        	}
        }else{
    		originalDlgId= dialogueId;
        }
        
        return originalDlgId;
	}
	
	
	
public static  CDR getCDR(AseApplicationSession appSession, String sessionId) {
		
		if(logger.isDebugEnabled())
			logger.info("getCDR() called to get CDR ref");
		// Get the CDRContext of the app that this session is associated with...
		if(appSession==null){
			if(logger.isDebugEnabled())
				logger.info("getCDR() called appsesion is currently null will add cdr ref later on");
			return null;
		}
		//AseApplicationSession appSession = (AseApplicationSession)this.getApplicationSession();
		AseContext app = appSession.getContext();
		CDR cdr = app.getCDRContext(sessionId).createCDR();

		// Populate the CDR with the initial values...
		//Marking it as default CDR
		cdr.set(CDR.DEFAULT_CDR,CDR.DEFAULT_CDR);
	//	cdr.set(CDR.CORRELATION_ID, appSession.getAttribute(Constants.CORRELATION_ID));
		cdr.set(CDR.SESSION_ID, sessionId);
///		cdr.set(CDR.ORIGINATING_NUMBER, ((SipURI)m_localParty.getURI()).getUser()); /coomenting for axtel it is coming as null for tcap
//		cdr.set(CDR.TERMINATING_NUMBER, ((SipURI)m_remoteParty.getURI()).getUser());
		//cdr.set(CDR.CALL_START_TIMESTAMP, String.valueOf(this.getSession().getCreationTime()));
//		cdr.set(CDR.BILL_TO_NUMBER, ((SipURI)m_localParty.getURI()).getUser());

		// Special case for handling the default CDR implementation:
		// We associate the host and app name with the CDR here so that the 
		// CDRContext can be looked up and re-associated with the CDR after 
		// it has been replicated.
		// We also set a flag indicating if the CDR is associated with a
		// distributable app or not.  If it is distributable, only Serializable
		// attribute values will be allowed to be set on it.
		if (cdr instanceof CDRImpl) {
			((CDRImpl)cdr).setHostName(app.getParent().getName());
			((CDRImpl)cdr).setAppName(app.getName());
			((CDRImpl)cdr).setDistributable(app.isDistributable());
		}
		if(logger.isDebugEnabled())
			logger.info("getCDR() Leaving with ref "+cdr);
		return cdr;
	}

}