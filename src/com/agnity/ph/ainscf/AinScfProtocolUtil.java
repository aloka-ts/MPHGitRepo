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

import jain.MandatoryParameterNotSetException;
import jain.ParameterNotSetException;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;
import jain.protocol.ss7.tcap.GTIndicator0001;
import jain.protocol.ss7.tcap.GTIndicator0010;
import jain.protocol.ss7.tcap.GTIndicator0011;
import jain.protocol.ss7.tcap.GTIndicator0100;
import jain.protocol.ss7.tcap.GlobalTitle;

import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.TimerService;

import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.util.Util;
import com.agnity.mphdata.common.*;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;

import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.sip.SipProtocolConfig;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.CDRWriteFailedException;
import com.baypackets.ase.util.stpool.Token;
import com.genband.tcap.provider.TcapSession;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
/**
 * This class provides utility.
 * @author reeta
 *
 */
public class AinScfProtocolUtil {

	private static Logger logger = Logger
			.getLogger(AinScfProtocolUtil.class);
	/**
	 * This method returns the calldata from the
	 * reference saved in tcap session.
	 * @param tcapSession represents an instance of TcapSession
	 * @return an instance of CallData
	 */	
	public static CallData getCallData(TcapSession tcapSession) {
		return (CallData) tcapSession.getAttribute(CallData.CALL_DATA);
	}


	/**
	 * This method extracts CallData from appSessions
	 * 
	 * @param appSession
	 * @return
	 */
	public static CallData getCallData(SipApplicationSession appSession) {
		CallData callData = (CallData) appSession.getAttribute(CallData.CALL_DATA);
		if (callData == null) {
			if (logger.isInfoEnabled()) {
				logger.info("CallData is null");
			}
		}
		return callData;
	}

	/**
	 * This method returns the sip application session using reference save in tcap session.
	 * It returns Tcap Notify AppSession.
	 * @param tcapSession represents an instance of TcapSession
	 * @return an instance of SipApplicationSession
	 */	
	public static SipApplicationSession getAppSession(TcapSession tcapSession) {
		String appSessionId = (String) tcapSession.getAttribute(PhConstants.APPLICATION_SESSION_ID);
		return tcapSession.getAppSession(appSessionId);
	}


	/**
	 * This method fetch TCAPSession based on TCAP_SESSION_ID from Sip Application Session
	 * @param appSession SipApplicationSession 
	 * @return TcapSession object
	 */
	public static TcapSession getTcapSession(SipApplicationSession appSession) {
		Integer dialogId = (Integer) appSession.getAttribute(PhConstants.TCAP_SESSION_ID);

		String serviceId = (String) appSession.getAttribute(PhConstants.SERVICE_ID);
		TcapSession tcapSession = null;
		/*
		 * Following dialogId null check is required as in SIP-T calls, when initial INVITE is
		 * received then dialogId would be null. But TcapProvider.getTcapSession method takes
		 * primitive int as its argument. And if we try to pass null to this method, NullpointerEx
		 * is raised
		 */
		if (dialogId != null) {
			tcapSession = PhUtilityServices.getInstance(serviceId).getTcapProvider().getTcapSession(dialogId);
		}
		return tcapSession;
	}

	/**
	 * This method starts the timer
	 *
	 * @param tcapSession
	 *            represents an instance of SipApplicationSession
	 * @param timeInMillies
	 *            represents long value of time
	 * @param persistable
	 *            represents boolean flag
	 * @param timerName
	 *            represents an instance of String
	 */
	public static void startTimer(TcapSession tcapSession, long timeInMillies,
			boolean persistable, String timerName) {
		CallData callData =  getCallData(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		SipApplicationSession appSession=getAppSession(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Start " + timerName + " timer for " + timeInMillies
					+ " milliseconds");
		}
		TimerService timerService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTimerService();

		PhTimerInfo phTimerInfo =new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.AIN_SCF);
		phTimerInfo.setTimerName(timerName);
		ServletTimer appTimer = timerService.createTimer(appSession, timeInMillies, persistable,
				phTimerInfo);

		appSession.setAttribute(timerName , appTimer.getId());

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::" + timerName + " Timer started; id="
					+ appTimer.getId() + ", time remaining = "
					+ appTimer.getTimeRemaining());
		}
	}

	/*
	 * Get DPs armed by an application from call Data
	 */
	public static Set<Action.ERB_TYPE> getErbSetByApplication(CallData callData, String legID) {

		if (logger.isDebugEnabled()) {
			logger.debug("getErbSetByApplication for Leg "+legID);
		}

		Set<Action.ERB_TYPE> erbSet=null;

		//First try to find ERB set in the Leg set in Action
		//If not found, get the ERB from configuration
		if (legID != null) {
			LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legID));
			if(legData.get(LegDataAttributes.P_ERB_SET) !=null){
				String erbString = (String) legData.get(LegDataAttributes.P_ERB_SET);
				erbSet = parseErbFromApplication(erbString);
			}
		}
		if (CollectionUtils.isEmpty(erbSet)) {
			logger.debug("[PH]:: Application has not set any ERB. Trying to set as per configuration..");
			erbSet = getErbSetInConfiguration();
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getErbSetByApplication Leaving with set " + erbSet);
		}
		return erbSet;		
	}

	/**
	 * This method is used to generate invoke ID to be sent in next SS7 message. 
	 *
	 * @param callData  specific to call
	 * @return Invoke ID to be coded in next outgoing SS7 message
	 */
	public static int getNextInvokeId(CallData callData){
		int nextInvokeId= callData.get(CallDataAttribute.P_INVOKE_ID)!=null?(Integer)callData.get(CallDataAttribute.P_INVOKE_ID):0;
		nextInvokeId++;
		callData.set(CallDataAttribute.P_INVOKE_ID,nextInvokeId);
		return nextInvokeId;
	}

	/**
	 * This method returns the last invoke ID sent. 
	 * @param callData  specific to call
	 * @return Last Invoke ID 
	 */
	public static int getLastInvokeId(CallData callData){
		int lastInvokeId= callData.get(CallDataAttribute.P_INVOKE_ID)!=null?(Integer)callData.get(CallDataAttribute.P_INVOKE_ID):0;
		return lastInvokeId;
	}

	/**
	 * This method returns Invoke ID configured in end range. 
	 * @param callData callData  specific to call
	 * @return
	 */
	public static int getLastInvokeIdEndRange(CallData callData){
		int lastInvokeIdendRange=callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END)!=null? (Integer)callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END):0;
		return lastInvokeIdendRange;
	}


	/**
	 * This method returns Invoke ID configured in start range. 
	 * @param callData callData  specific to call
	 * @return
	 */
	public static int getLastInvokeIdStartRange(CallData callData){
		int invokeIDRangeStart=(Integer)callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_START);
		return invokeIDRangeStart;
	}


	/**
	 * This method store the invoke ID into Call Data as Invoke ID start Range
	 * @param lastInvokeIdStartRange Invoke ID
	 * @param callData Call Data specific to call. 
	 */
	public static void setLastInvokeIdStartRange(int lastInvokeIdStartRange,CallData callData){
		callData.set(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_START,lastInvokeIdStartRange);
	}


	/**
	 * This method stores the invoke ID into call Data as Invoke ID end range
	 * @param lastInvokeIdEndRange Invoke ID
	 * @param callData Call Data specific to call. 
	 */
	public static void setLastInvokeIdEndRange(int lastInvokeIdEndRange,CallData callData){
		callData.set(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END,lastInvokeIdEndRange);
	}

	/**
	 * This method returns he invoke ID received in incoming SS7 message from Call Data
	 * @param callData  Call Data specific to call. 
	 * @return Invoke ID
	 */
	public static int getLastRxInvokeId(CallData callData){
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int lastRxInvokeId = (Integer) legData
				.get(LegDataAttributes.P_LAST_RX_INVOKE_ID);

		return lastRxInvokeId;
	}


	/**
	 * This utility method is for writing service cdr with tcap session.
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void writeServiceCdr(com.genband.tcap.provider.TcapSession tcapSession) {
		CallData callData = null;
		Integer dialogId = null;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside writeServiceCdr with tcapSession");
		}

		try {
			callData = getCallData(tcapSession);
			SipApplicationSession appSession=getAppSession(tcapSession);
			dialogId = (Integer) callData
					.get(CallDataAttribute.P_DIALOG_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Inside writeServiceCdr with appSession");
			}

			CDR cdrREf=getCDRRefernceObject(callData,appSession,dialogId);

			if (callData.get(CallDataAttribute.P_CDR_REF) != null) {
				CDR cdrRef = (CDR) callData
						.get(CallDataAttribute.P_CDR_REF);
				writeServiceCdr(cdrRef, callData);
			}else{
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: CDR Refrence object not found not writing any CDR");
				}
			}
		} catch (Exception ex) {
			logger.error(dialogId + ":: Failed to write service cdr.", ex);
		}
	}


	/**
	 * This method is used to get CDR object reference for tcap session
	 * @param callData Call Data specific to Call
	 * @param appSession  SipApplicationSession 
	 * @param dialogId Dialogue ID 
	 * @return CDR reference object. 
	 */
	private static CDR getCDRRefernceObject(CallData callData,
			SipApplicationSession appSession, Integer dialogId) {

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside getCDRRefernceObject");
		}
		CDR cdrRef = null;

		Object cdrRefObject = callData
				.get(CallDataAttribute.P_CDR_REF);

		if (cdrRefObject == null) {
			Iterator<SipSession> sipSessions = (Iterator<SipSession>) appSession
					.getSessions();

			while (sipSessions.hasNext()) {
				SipSession sipSession = sipSessions.next();

				if (sipSession.getAttribute(PhConstants.TCAP_CDR_REF) != null) {
					cdrRef = (CDR) sipSession
							.getAttribute(PhConstants.TCAP_CDR_REF);
					break;
				}
			}
			callData.set(CallDataAttribute.P_CDR_REF, cdrRef);
		} else {
			cdrRef = (CDR) cdrRefObject;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: leaving getCDRRefernceObject with "
					+ cdrRef);
		}
		return cdrRef;

	}

	/**
	 * This method writes the SN CDR if applicable
	 * 
	 * @param cdr
	 *            represents an instance of CDR
	 * @param callData
	 *            represents an instance of CallData
	 * @throws CDRWriteFailedException
	 */
	private static void writeServiceCdr(CDR cdr, CallData callData) throws CDRWriteFailedException {
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger
			.debug(dialogId
					+ " :: writeServiceCdr");
		}
		if (PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {
			if (logger.isDebugEnabled()) {
				logger
				.debug(dialogId
						+ " :: Final CDR already written. So returning without writing CDR");
			}
			return;
		}
		ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		String[] cdrArr = serviceHandler.getServiceCdr(callData);

		if (cdr!=null && cdrArr != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: Invoking write method on CDR utility");
			}
			cdr.write(cdrArr);
			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN, PhConstants.TRUE);
		}

	}

	/**
	 * This method is used to get Activity Test timer time saved by application in CallData
	 * @param callData Call Data specific to call 
	 * @return long returns activity test time
	 */
	public static long getActivityTestTime(CallData callData){

		if (logger.isDebugEnabled()) {
			logger.debug("::getActivityTestTime ");
		}
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);

		Object atTimerDuration =legData
				.get(LegDataAttributes.P_AT_EXPIRY_TIME);

		long atTime=30000;

		if(atTimerDuration!=null){
			atTime=(Long)atTimerDuration;
		}
		if (logger.isDebugEnabled()) {
			logger.debug("::getActivityTestTime "+atTime);
		}
		return atTime;		
	}

	/**
	 * This method is used to get correlation timer time saved by application in CallData
	 * @param callData Call Data specific to call
	 * @return long Returns correlation time
	 */
	public static long getCorrelationTime(CallData callData){

		int dialogId=(Integer)callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::getCorrelationTime ");
		}
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);

		Object corrTimerDuration =legData
				.get(LegDataAttributes.P_CORRELATION_EXPIRY_TIME);

		long corrTime=60000;

		if(corrTimerDuration!=null){
			corrTime=(Long)corrTimerDuration;
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::getCorrelationTime  returnng with value "+ corrTime);
		}
		return corrTime;		
	}


	/**
	 * This method is called to start the CDR timer for the intermediate CDRs
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void startCdrTimer(TcapSession tcapSession) {

		CallData callData = getCallData(tcapSession);
		SipApplicationSession appSession = getAppSession(tcapSession);
		int dialogId=(Integer)callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::startCdrTimer ");
		}

		Date callConnectDateTime=(Date)callData.get(CallDataAttribute.P_CALL_CONNECT_TIME);
		long initialDelay = CommonUtils.getInitialDelay(callConnectDateTime);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Start Intermediate CDR Timer");
		}

		/*
		 * moved before timer creation to replicate updated value
		 */
		CommonUtils.setAppSessionTimeout(appSession, (int) (initialDelay / 60000) + 5,
				dialogId);

		startTimer(PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTimerService(), tcapSession, initialDelay, 86400000L, false, true,
				PhConstants.CDR_TIMER);
	}


	/**
	 * This method start the timer
	 * @param timerService represents an instance of TimerService
	 * @param tcapSession represents an instance of TcapSession
	 * @param initialDelay represents long value of initial delay
	 * @param timeInMillies represents long value of time
	 * @param delayRecovery represents boolean flag
	 * @param persistable represents boolean flag
	 * @param timerName represents an instance of String
	 */
	public static void startTimer(TimerService timerService, TcapSession tcapSession,
			long initialDelay, long timeInMillies, boolean delayRecovery,
			boolean persistable, String timerName) {
		CallData callData = getCallData(tcapSession);
		SipApplicationSession appSession = getAppSession(tcapSession);

		int dialogId=(Integer)callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Start" + timerName + " timer for " + timeInMillies
					+ " milliseconds");
			logger.debug(dialogId + ":: Initial Delay is " + initialDelay
					+ ", delayRecovery flag is " + delayRecovery + " persistable flag is "
					+ persistable);
		}

		PhTimerInfo phTimerInfo =new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.AIN_SCF);
		phTimerInfo.setTimerName(timerName);

		ServletTimer appTimer = timerService.createTimer(appSession, initialDelay, timeInMillies,
				delayRecovery, persistable, phTimerInfo);

		appSession.setAttribute(timerName , appTimer.getId());

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::" + timerName + " Timer started; id="
					+ appTimer.getId() + ", time remaining = "
					+ appTimer.getTimeRemaining());
		}
	}


	/**
	 * This method stops the timer.
	 * 
	 * @param tcapSession
	 *            represents an instance of SipApplicationSession
	 * @param timerName
	 *            represents an instance of String
	 */
	public static void stopTimer(TcapSession tcapSession, String timerName) {
		CallData callData = getCallData(tcapSession);
		SipApplicationSession appSession= getAppSession(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside stopTimer for " + timerName);
		}

		appSession.removeAttribute(timerName);

		/*
		 *  Cancel at last to replicate the app session changes
		 */
		Collection<ServletTimer> timers = appSession.getTimers();
		for (ServletTimer timer : timers) {
			if (timer != null){
				PhTimerInfo timerInfo=(PhTimerInfo)timer.getInfo();

				if (timerInfo.getTimerName().equals(timerName)) {
					timer.cancel();
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: " + timerName
								+ " stopped");
					}
					return;
				}
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: " + timerName + " not found in all timers");
		}
	}



	/**
	 * This method cleanup the resources used for correlation such as remove the
	 * entry from correlation map, stop correlation timer etc
	 * 
	 * @param tcapSession TCAP Session 
	 */
	public static void cleanupCorrelationResources(TcapSession tcapSession) {
		try {
			CallData callData = getCallData(tcapSession);

			LegData legData=(LegData)callData.get(CallDataAttribute.P_LEG1);

			AinCallStates state=(AinCallStates)legData.get(LegDataAttributes.P_LEG_SS7_STATE);

			int dialogId = (Integer) callData
					.get(CallDataAttribute.P_DIALOG_ID);
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Inside cleanupCorrelationResources in orig state "+state);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Stop correlation timer if running");
			}

			stopTimer(tcapSession, PhConstants.CORRELATION_TIMER);
			Object corrIdObj=callData.get(CallDataAttribute.P_CORRELATION_ID);

			if (corrIdObj != null && !(corrIdObj.toString().isEmpty())) {
				String correlationId = (String)corrIdObj;

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Remove the correlation id " + correlationId);
				}

				/*
				 *  changes done to support cleanup on FT;as we are replicating servlet cCorrelationMap might be null
				 */
				Map<String, Object> cCorrelationMap = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap();
				if (cCorrelationMap != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Cleanup corr resources correlation map is not null");
					}
					cCorrelationMap.remove(correlationId);
				}

				AinScfProtocolUtil.returnTokenToSharedPool(callData);
			}


		} catch (Exception e) {
			logger.warn("Error in correlation resource cleanup " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info("Error in cleanupCorrelationResources.", e);
			}
		}
	}


	private static Set<Action.ERB_TYPE> getErbSetInConfiguration() {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Fetching getErbSetInConfiguration");
		}
		Set<Action.ERB_TYPE> erbSet = new LinkedHashSet<Action.ERB_TYPE>();
		String configuredERBSet = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.DEFAULT_ANSI_ERB_SET);

		if (StringUtils.isNotEmpty(configuredERBSet)) {
			String[] configuredEvents = StringUtils.split(configuredERBSet, "|");
			for (String event : configuredEvents) {
				try {    
					Action.ERB_TYPE configuredERBType = Action.ERB_TYPE.valueOf(event);
					erbSet.add(configuredERBType);
				} catch (IllegalArgumentException iae) {
					logger.error("[PH]:: Configured ERB is not a valid Enum value : " + event);
				}
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getErbSetInConfiguration Leaving with set " + 
					erbSet + ", defaultERB:" + configuredERBSet);
		}
		return erbSet;
	}
	
	/**
	 * Method parses the string corresponding to ERB to be sent by application. 
	 * Application must set this value in following format. 
	 * ERB_BUSY|ERB_NO_ANSWER|ERB_ANSWER|ERB_DISCONNECT|ERB_NETWORK_BUSY
	 * @return
	 */
	private static Set<Action.ERB_TYPE> parseErbFromApplication(String erbString) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Fetching getErbSetInConfiguration");
		}
		Set<Action.ERB_TYPE> erbSet = new LinkedHashSet<Action.ERB_TYPE>();
		if (StringUtils.isNotEmpty(erbString)) {
			String[] configuredEvents = StringUtils.split(erbString, "|");
			for (String event : configuredEvents) {
				try {    
					Action.ERB_TYPE configuredERBType = Action.ERB_TYPE.valueOf(event);
					erbSet.add(configuredERBType);
				} catch (IllegalArgumentException iae) {
					logger.error("[PH]:: Configured ERB is not a valid Enum value : " + event);
				}
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getErbSetInConfiguration Leaving with set " + 
					erbSet + ", defaultERB:" + erbString);
		}
		return erbSet;
	}

	/**
	 * This method is used to get a token form shared pool if it is enabled
	 * @param dialogId
	 * @param callData
	 * @return
	 */
	public static String getTokenFromSharedPool(CallData callData) {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: AinScfProtocolUtil: getTokenFromSharedPool............");
		}
		String correlationId = (String) callData.get(CallDataAttribute.P_CORRELATION_ID);

		String corrKey=(String)callData.get(CallDataAttribute.CORRELATION_KEY);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: getTokenFromSharedPool use correlationId as key "+ correlationId);
		}

		if (corrKey == null && correlationId != null) {
			corrKey = correlationId;
			callData.set(CallDataAttribute.CORRELATION_KEY, correlationId);
		}

		String serviceId=(String) callData.get(CallDataAttribute.SERVICE_ID);

		Token token = PhUtilityServices.getInstance(serviceId).getSharedTokenPool().pop(corrKey,serviceId);

		if (token != null) {
			correlationId = token.getValue() + "";
			callData.set(CallDataAttribute.P_CORRELATION_ID, correlationId);
			callData.set(CallDataAttribute.P_CORRELATION_TOKEN, token);

		} else {
			return null;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: getTokenFromSharedPool  return correlation id as " + correlationId);
		}

		return correlationId;
	}	


	/**
	 * This method is used to get a token form shared pool if it is enabled
	 * @param dialogId
	 * @param callData
	 * @return
	 */
	public static void returnTokenToSharedPool(CallData callData) {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		String sharedPoolEnabled = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.SHARED_TOKEN_POOL_ENABLED);

		if (PhConstants.TRUE.equals(sharedPoolEnabled)) {

			String correlationKey = (String) callData.get(CallDataAttribute.CORRELATION_KEY);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: returnTokenToSharedPool for key " + correlationKey);
			}

			Token correlationToken = (Token) callData.get(CallDataAttribute.P_CORRELATION_TOKEN);

			if (correlationToken != null) {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: returnTokenToSharedPool use correlationId as key " + correlationKey);
				}

				String serviceId=(String) callData.get(CallDataAttribute.SERVICE_ID);

				PhUtilityServices.getInstance(serviceId).getSharedTokenPool().push(correlationKey, correlationToken,serviceId);

				callData.set(CallDataAttribute.CORRELATION_KEY, null);
				callData.set(CallDataAttribute.P_CORRELATION_TOKEN, null);
				callData.set(CallDataAttribute.P_CORRELATION_ID, null);
			} 
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: returnTokenToSharedPool  return correlation id as "
						+ correlationToken);
			}
		}else{

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: returnTokenToSharedPool sharedtoken pool is not enabled");
			}
		}
	}
	
	/**
	 * @param callData
	 */
	public static void removeCorrelationId(CallData callData){
		
		if(logger.isDebugEnabled()){
			logger.debug("Inside removeCorrelationId");
		}
		
		if(callData.get(CallDataAttribute.P_CORRELATION_ID) != null){
			callData.remove(CallDataAttribute.P_CORRELATION_ID);
			
			if(logger.isDebugEnabled()){
				logger.debug("Correlation ID removed");
			}
		}
	}
	
	public static String getUniqueCallIDForTracing(CallData callData) {

		String dialogId = String.valueOf(callData
				.get(CallDataAttribute.P_DIALOG_ID));
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: getUniqueCallIDForTracing");
		}

		long time = -1;
		if (callData.get(CallDataAttribute.P_CALL_START_TIME) != null) {
			Date date = (Date) callData
					.get(CallDataAttribute.P_CALL_START_TIME);
			time = date.getTime();
		}
		if (time != -1) {
			dialogId = dialogId + "_" + time;
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: getUniqueCallIDForTracing return "
					+ dialogId);
		}
		return dialogId;
	}

	/**
	 * This method is used for setting the Error code and ProblemData fields for TCAP RETURN ERROR PDU, in case of TCAP query decoding exception due to erroneous data element. This is as per GR-1149 Table 8-4.
	 * 	  */
	public static void setApplicationErrorProblemData(CallData callData, byte[] input, int dataElementIdIndex, int dataElementIdLength, int dataElementLength)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {
		if (logger.isDebugEnabled()) {
			logger.debug("setApplicationErrorProblemData::" + Util.formatBytes(input));
		}
		if (callData.get(CallDataAttribute.P_LEG1) == null) {
			if (logger.isDebugEnabled()) {
				logger.debug("setApplicationErrorProblemData:Setting LEG1 data as P_LEG1 is null");
			}
			LegData setLegData = new LegData();
			callData.set(CallDataAttribute.P_LEG1, setLegData);
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_APP_ERR_PROBLEM_DATA);

		int totalProblemDataLen = dataElementIdLength + dataElementLength + 1; //1 for data element length field
		byte[] problemData = new byte[totalProblemDataLen + 2]; //2 for problemDataIdentifier and problemData length fields
		problemData[0] = (byte) 0x86; //problemDataIdentifier
		problemData[1] = (byte) (totalProblemDataLen); //problemData length
		for (int i = 0; i < totalProblemDataLen; i++) {
			// putting the current data element identifier, length and data element to problemData byte array. This will
			// be used to send ProblemData in the Return Error PDU
			logger.info("setApplicationErrorProblemData:Setting problem data index " + (i+2) + "- " + input[dataElementIdIndex + i]);
			problemData[i+2] = (byte) input[dataElementIdIndex + i];
		}
		legData.set(LegDataAttributes.P_APP_ERR_CODE, "2"); // 2 -> Unexpected data value
		legData.set(LegDataAttributes.P_APP_ERR_PROBLEM_DATA,problemData);
	}
	
	
	/**
	 * method to encode ACG parameters for TCAP query as per GR 1149 (Table 8-26. TCAP Information for ACG Component)
	 * 
	 * @param callData
	 * @throws AINCodecException
	 */
	public static byte[] encodeAcgParameters(CallData callData) throws AINCodecException, InvalidInputException {

		if (logger.isDebugEnabled()) {
			logger.info("encodeAcgParameters:Enter");
		}

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int index = 0;
		int totalLength = 0;
		int DIGITS_BASE_LENGTH = 4; //Total bytes for encoding Type of digits + NON + Num.Plan/Encoding + Number of digits
		//int ccBillingNumType = 0;
		byte PARAMETER_SET_ID = (byte) 0xF2;
		byte DIGITS_IDENTIFIER = (byte) 0x84;
		byte ACG_INDICATOR_ID_TAG1 =(byte) 0xDF;
		byte ACG_INDICATOR_ID_TAG2 = (byte) 0x47;
		byte ACG_INDICATOR_LENGTH = (byte) 0x03;
		byte typeOfDigitsCalled = (byte) 0x01;
		byte typeOfDigitsCalling = (byte) 0x02;
		byte typeOfDigitsBilling = (byte) 0x05;
		byte typeOfDigitsCallingDirNum = (byte) 0x0B;
		byte typeOfDigitsQueriedNumber = (byte) 0x17; //GR2838 - R4-253 [77]
		byte encodeTypeOfDigits = (byte) 0x00;
		byte encodeControlledCodeNatureOfNumber = (byte) 0x00; //National no presentation restriction
		byte encodeControlledCodeNumberingPlan = (byte) 0x21; //ISDN numbering plan + BCD
		byte encodeControlledCodeNumberOfDigits = (byte) 0x00;

		byte encodedParameterSetIdentifier = PARAMETER_SET_ID;
		totalLength++;
		
		byte encodeDigitsIdentifier = DIGITS_IDENTIFIER;
		totalLength++;
		
		byte encodeDigitsLength = (byte) 0x00;
		totalLength++;

		PhoneNumber controlledCode = null;
 if (MESSAGE.BNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
 		if (leg2Data.get(LegDataAttributes.P_BNS_O_ACG_BILLED_NUMBER) != null) {
			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_BNS_O_ACG_BILLED_NUMBER);
			encodeTypeOfDigits = typeOfDigitsBilling;
		}
// } else if (MESSAGE.GN_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
// 		if (leg2Data.get(LegDataAttributes.P_GN_M_CALLING_PARTY) != null) {
//			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GN_M_CALLING_PARTY);
 		encodeTypeOfDigits = typeOfDigitsCallingDirNum;		
//		}
 } else if (MESSAGE.OLNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
 		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ACG_CALLING_NUMBER) != null) {
			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_OLNS_O_ACG_CALLING_NUMBER);		
			encodeTypeOfDigits = (byte) leg2Data.get(LegDataAttributes.P_OLNS_SERVICEKEY_DIGITS_TYPE);
		}
 } else if (MESSAGE.GET_DATA_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
 		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ACG_QUERIED_NUM) != null) {
			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GETDATA_O_ACG_QUERIED_NUM);
			encodeTypeOfDigits = typeOfDigitsQueriedNumber;
		}
 } else if (MESSAGE.CC1_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
 		if (leg2Data.get(LegDataAttributes.P_CC_O_ACG_BILLED_NUMBER) != null) {
			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_CC_O_ACG_BILLED_NUMBER);		
			encodeTypeOfDigits = typeOfDigitsBilling;
			if (leg2Data.get(LegDataAttributes.CC1_BILLING_NUM_TYPE) != null) {
				Integer ccBillingNumType = (Integer) leg2Data.get(LegDataAttributes.CC1_BILLING_NUM_TYPE);
				if (ccBillingNumType.intValue() == 1) { //ACCAN type
					encodeControlledCodeNumberingPlan = (byte) 0x01; //Not applicable + BCD
				} else {
					encodeControlledCodeNumberingPlan = (byte) 0x11; //ISDN numbering plan + BCD
				}
			}
		}
 } else if (MESSAGE.CC2_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
 		if (leg2Data.get(LegDataAttributes.P_CC_O_ACG_BILLED_NUMBER) != null) {
			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_CC_O_ACG_BILLED_NUMBER);
			encodeTypeOfDigits = typeOfDigitsBilling;
			if (leg2Data.get(LegDataAttributes.CC2_BILLING_NUM_TYPE) != null) {
				Integer ccBillingNumType = (Integer) leg2Data.get(LegDataAttributes.CC2_BILLING_NUM_TYPE);
				if (ccBillingNumType.intValue() == 1) { //ACCAN type
					encodeControlledCodeNumberingPlan = (byte) 0x01; //Not applicable + BCD
				} else {
					encodeControlledCodeNumberingPlan = (byte) 0x11; //ISDN numbering plan + BCD
				}
			}
		}
 } else if (MESSAGE.TLNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
 		if (leg2Data.get(LegDataAttributes.P_TLNS_O_ACG_CALLED_NUMBER) != null) {
			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_TLNS_O_ACG_CALLED_NUMBER);		
			encodeTypeOfDigits = typeOfDigitsCalled;
		}
 } else if (MESSAGE.ICDC_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
 		if (leg2Data.get(LegDataAttributes.P_ICDC_O_ACG_BILLED_NUMBER) != null) {
			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_ICDC_O_ACG_BILLED_NUMBER);
			encodeTypeOfDigits = typeOfDigitsBilling;
		}
 } else if (MESSAGE.INTERCEPT_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
 		if (leg2Data.get(LegDataAttributes.P_INTERCEPT_O_ACG_CALLED_NUMBER) != null) {
			controlledCode = (PhoneNumber) leg2Data.get(LegDataAttributes.P_INTERCEPT_O_ACG_CALLED_NUMBER);
			encodeTypeOfDigits = typeOfDigitsCalled;
		}
 }


		byte[] encodedControlledCode = null;
		int digitsLength = 0;
		if (controlledCode != null) {
			if (controlledCode.getAddress().length() < 6) {
				logger.info("digits [AIN calling party] length should be at least 6");
				throw new AINCodecException("digits [AIN calling party] length should be at least 6");
			}
			encodedControlledCode = encodeAdrsSignal(controlledCode.getAddress());
			totalLength += (DIGITS_BASE_LENGTH + controlledCode.getAddress().length());
			if (controlledCode.getAddress().length() % 2 == 0) {
				digitsLength = (controlledCode.getAddress().length() / 2);
			} else {
				digitsLength = (controlledCode.getAddress().length() + 1) / 2;
			}			
			encodeDigitsLength = (byte) (DIGITS_BASE_LENGTH + digitsLength);
			encodeControlledCodeNumberOfDigits = (byte) controlledCode.getAddress().length();
		}

		
		// 
		byte encodedAcgIndicatorIdTag1 = ACG_INDICATOR_ID_TAG1;
		totalLength++;

		byte encodedAcgIndicatorIdTag2 = ACG_INDICATOR_ID_TAG2;
		totalLength++;

		byte encodedAcgIndicatorLength = ACG_INDICATOR_LENGTH;
		totalLength++;

		if (leg2Data.get(LegDataAttributes.ACG_GAP_DURATION) == null
				|| leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE) == null) {
			logger.info("encodeAcgParameters: throwing exception ");
			logger.error("encodeAcgParameters: duration and gap values are Mandatory");
			throw new AINCodecException("duration and gap values are Mandatory");
		}

		if (logger.isDebugEnabled()) {
			logger.info("encodeAcgParameters:ACG duration and gap values are " +leg2Data.get(LegDataAttributes.ACG_GAP_DURATION) + ", " + leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
		}
		
		int duration = 0;

		if (leg2Data.get(LegDataAttributes.ACG_GAP_DURATION) != null) {
			duration = Integer.parseInt((String) leg2Data.get(LegDataAttributes.ACG_GAP_DURATION));
		}
		totalLength++;

		int gap = 0;

		if (leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE) != null) {
			gap = Integer.parseInt((String) leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
		}

//		byte[] encodedDuration = CommonUtils.formatIntToByte((getPowerForGnACG(duration)));
//		byte[] encodedGap = CommonUtils.formatIntToByte((getPowerForGnACG(gap)));
		byte[] encodedDuration = CommonUtils.formatIntToByte(duration);
		byte[] encodedGap = CommonUtils.formatIntToByte(gap);

		String[] parameterSetLength = convertHexRepersentaion(Integer.toHexString(totalLength));
		byte[] outputArray = new byte[totalLength + 2];

		outputArray[index++] = encodedParameterSetIdentifier;
		outputArray[index++] = (byte) Byte.decode(parameterSetLength[0]);

		outputArray[index++] = encodeDigitsIdentifier;
		outputArray[index++] = encodeDigitsLength;
		outputArray[index++] = encodeTypeOfDigits;
		outputArray[index++] = encodeControlledCodeNatureOfNumber; //national no presentation restriction
		outputArray[index++] = encodeControlledCodeNumberingPlan;
		outputArray[index++] = encodeControlledCodeNumberOfDigits;

		if (encodedControlledCode != null) {
			for (byte val : encodedControlledCode) {
				outputArray[index++] = val;
			}
		}
		outputArray[index++] = encodedAcgIndicatorIdTag1;
		outputArray[index++] = encodedAcgIndicatorIdTag2;
		outputArray[index++] = encodedAcgIndicatorLength;

		outputArray[index++] = 0x03;
		for (byte val : encodedDuration) {
			outputArray[index++] = val;
		}
		for (byte val : encodedGap) {
			outputArray[index++] = val;
		}
		if (logger.isInfoEnabled()) {
			logger.info("Exit: encodeAcgParameters ::" + CommonUtils.formatBytes(outputArray));
		}
		return outputArray;
	}


	/**
	 * method use to convert into hex format e.g. input "FAE" output= 0x0F 0xAE
	 * 
	 * @param input
	 * @return
	 */
	public static String[] convertHexRepersentaion(String input) {
		if (input.length() % 2 != 0) {
			input = 0 + input;
		}
		char[] output = new char[5 * (input.length() / 2)];
		int top = 0;
		int temp = 0;
		for (int i = 0; i < input.length() / 2; i++) {
			output[top++] = '0';

			output[top++] = 'x';

			output[top++] = input.charAt(temp++);

			output[top++] = input.charAt(temp++);

			output[top++] = ' ';
		}
		String tmpStr = new String(output);
		String[] hexString = tmpStr.split(" ");
		return hexString;

	}
 	/**
	 * This method is used for encode Address Signal into byte array
	 * 
	 * @param addrSignal String addrSignal
	 * @return a byte array
	 */
	private static byte[] encodeAdrsSignal(String addrSignal) throws InvalidInputException {

		if (logger.isDebugEnabled()) {
			logger.debug("Enter: encodeAdrsSignal:Input--> addrSignal:" + addrSignal);
		}
		if (addrSignal == null || addrSignal.equals(" ")) {
			logger.error("encodeAdrsSignal: InvalidInputException(AddressSignal is null or blank)");
			throw new InvalidInputException("AddressSignal is null or blank");
		}
		int len = addrSignal.length();
		int size = (len + 1) / 2;
		byte[] out = new byte[size];
		for (int i = 0, j = 0; i < len; i += 2, j++) {
			byte b1 = (byte) (addrSignal.charAt(i) - '0');
			byte b2 = 0;
			if ((i + 1) < len) {
				b2 = (byte) (addrSignal.charAt(i + 1) - '0');
			}
			out[j] = (byte) ((b2 << 4) | b1);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("Exit: encodeAdrsSignal:Output<-- byte[]:" + Util.formatBytes(out));
		}
		return out;
	}

	public static void resetNetworkTransactions(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("resetNetworkTransactions from current value.."+callData
					.get(CallDataAttribute.P_NETWORK_TRANSACTION));
		}
		callData
		.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(0));
	}
	
	
	/**
	 * 
	 * @param callData
	 * @param leg2Data
	 * @return
	 */
       public static SccpUserAddress updateCallingAddress(CallData callData,LegData leg2Data){
    	   
    	   if (logger.isDebugEnabled()) {
				logger.debug(" [PH]:: updateCallingAddress SUA ");
			}
    	   
		SccpUserAddress selfSUA=(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);
		
		if (logger.isDebugEnabled()) {
			logger.debug(" [PH]:: Routing Indicator on P_SCCP_LOCAL_USER_ADDRESS is  "+ selfSUA.getRoutingIndicator());
		}
		
		String ssnStr=(String) leg2Data.get(LegDataAttributes.NP_CALLING_SSN);
		
		String pc=(String) leg2Data.get(LegDataAttributes.NP_CALLING_PC);
		
		String routingInd=(String) leg2Data.get(LegDataAttributes.NP_CALLING_ROUTING_IND);
		
		String gtInd=(String) leg2Data.get(LegDataAttributes.NP_CALLING_GT_IND);
		
		String gtDigits=(String) leg2Data.get(LegDataAttributes.NP_CALLING_GT_DIGITS);
		
		String tt=(String) leg2Data.get(LegDataAttributes.NP_CALLING_TRANS_TYPE);
		
		SignalingPointCode callingSpc=null;
		short ssn=-1;
		
		boolean createPc=false;
		try {
			if (StringUtils.isNotBlank(pc)) {
				
				createPc=true;
				String[] tmp = pc.split("-");
				if (tmp.length == 3) {
					callingSpc = new SignalingPointCode(
							Integer.parseInt(tmp[2]), Integer.parseInt(tmp[1]),
							Integer.parseInt(tmp[0]));
				}
			} else {
				callingSpc = selfSUA.getSubSystemAddress()
						.getSignalingPointCode();
			}

			if (StringUtils.isNotBlank(ssnStr)) {
				createPc=true;
				ssn = Short.parseShort(ssnStr);
			} else {
				ssn = (short) selfSUA.getSubSystemAddress()
						.getSubSystemNumber();
			}

		} catch (MandatoryParameterNotSetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParameterNotSetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		if (createPc && callingSpc != null && ssn != -1) {
			SccpUserAddress newSUA = new SccpUserAddress(new SubSystemAddress(
					callingSpc, ssn));
			newSUA.setProtocolVariant(selfSUA.getProtocolVariant());
			if (selfSUA.isRoutingIndicatorPresent())
				newSUA.setRoutingIndicator(selfSUA.getRoutingIndicator());

			if (selfSUA.isGlobalTitlePresent()) {
				try {
					newSUA.setGlobalTitle(selfSUA.getGlobalTitle());
				} catch (MandatoryParameterNotSetException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ParameterNotSetException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}

			selfSUA = newSUA;
			if (logger.isDebugEnabled()) {
				logger.debug(" [PH]::New created  SUA is" + selfSUA);
			}
		}


			if(StringUtils.isNotBlank(routingInd)){
//				selfSUA.setRoutingIndicator(1);
//			}else{
				if (logger.isDebugEnabled()) {
					logger.debug(" [PH]:: Update  routing indicator in  SUA " + routingInd);
				}
				selfSUA.setRoutingIndicator(Integer.parseInt(routingInd));
			}
        
		
		GlobalTitle gt=updateGlobalTitle(selfSUA, gtInd, tt, gtDigits);
		selfSUA.setGlobalTitle(gt);
		
		return selfSUA;
	}
       
       /**
   	 * 
   	 * @param callData
   	 * @param leg2Data
   	 * @return
   	 */
	public static SccpUserAddress updateCalledAddress(CallData callData,
			LegData leg2Data) {

		if (logger.isDebugEnabled()) {
			logger.debug(" [PH]:: updateCalledAddress SUA ");
		}

		SccpUserAddress selfSUA = (SccpUserAddress) callData
				.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS);
		
		if (logger.isDebugEnabled()) {
			logger.debug(" [PH]:: Routing Indicator on P_SCCP_REMOTE_USER_ADDRESS is  "+ selfSUA.getRoutingIndicator());
		}

		String ssnStr = (String) leg2Data.get(LegDataAttributes.NP_CALLED_SSN);

		String pc = (String) leg2Data.get(LegDataAttributes.NP_CALLED_PC);

		String routingInd = (String) leg2Data
				.get(LegDataAttributes.NP_CALLED_ROUTING_IND);

		String gtInd = (String) leg2Data
				.get(LegDataAttributes.NP_CALLED_GT_IND);

		String gtDigits = (String) leg2Data
				.get(LegDataAttributes.NP_CALLED_GT_DIGITS);

		String tt = (String) leg2Data
				.get(LegDataAttributes.NP_CALLED_TRANS_TYPE);

		SignalingPointCode callingSpc = null;
		short ssn = -1;

		boolean createPc = false;
		try {
			if (StringUtils.isNotBlank(pc)) {

				createPc = true;
				String[] tmp = pc.split("-");
				if (tmp.length == 3) {
					callingSpc = new SignalingPointCode(
							Integer.parseInt(tmp[2]), Integer.parseInt(tmp[1]),
							Integer.parseInt(tmp[0]));
				}
			} else {
				callingSpc = selfSUA.getSubSystemAddress()
						.getSignalingPointCode();
			}

			if (StringUtils.isNotBlank(ssnStr)) {
				createPc = true;
				ssn = Short.parseShort(ssnStr);
			} else {
				ssn = (short) selfSUA.getSubSystemAddress()
						.getSubSystemNumber();
			}

		} catch (MandatoryParameterNotSetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParameterNotSetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		if (createPc && callingSpc != null && ssn != -1) {
			SccpUserAddress newSUA = new SccpUserAddress(new SubSystemAddress(
					callingSpc, ssn));
			newSUA.setProtocolVariant(selfSUA.getProtocolVariant());
			if (selfSUA.isRoutingIndicatorPresent())
				newSUA.setRoutingIndicator(selfSUA.getRoutingIndicator());

			if (selfSUA.isGlobalTitlePresent()) {
				try {
					newSUA.setGlobalTitle(selfSUA.getGlobalTitle());
				} catch (MandatoryParameterNotSetException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ParameterNotSetException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}

			selfSUA = newSUA;
			if (logger.isDebugEnabled()) {
				logger.debug(" [PH]::New created  SUA is" + selfSUA);
			}
		}

		if (StringUtils.isNotBlank(routingInd)) {
			if (logger.isDebugEnabled()) {
				logger.debug(" [PH]:: Update routing indicator in self SUA "
						+ routingInd);
			}
			selfSUA.setRoutingIndicator(Integer.parseInt(routingInd));
		}

		GlobalTitle gt = updateGlobalTitle(selfSUA, gtInd, tt, gtDigits);
		
		if (gt != null) {
			selfSUA.setGlobalTitle(gt);
		}else{
			if (logger.isDebugEnabled()) {
				logger.debug(" [PH]:: GT is not set on SUA");
			}
		}

		return selfSUA;
	}
       
	public static GlobalTitle updateGlobalTitle(SccpUserAddress userAdd,
			String gtInd, String tt, String gtDigits) {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: updateGlobalTitle Enter TranslationType "
					+ tt + " GT Digits " + gtDigits + " GT Indicator " + gtInd);
		}

		byte translationType = -1;
		int gtIndvalue = -1;
		GlobalTitle gt = null;
		// specific GT params

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: updateGlobalTitle Enter translationType "
					+ translationType);
		}
		try {
			/**
			 * GT indicator is updated so create gt again
			 */
			if (StringUtils.isNotBlank(gtInd)) {
				gtIndvalue = Integer.parseInt(gtInd);

				if (gtIndvalue != -1) {
					gt = createGlobalTitle(gtIndvalue);
					userAdd.setGlobalTitle(gt);
				}
			}else{
				if (userAdd.isGlobalTitlePresent()) {
					gt= userAdd.getGlobalTitle();
					
					if(gt.isAddressInformationPresent()){
					if (logger.isDebugEnabled()) {
						logger.debug(" [PH]:: GT digits on address is  "+CommonUtils.convertByteArrayToString(gt.getAddressInformation()));
					}
					}
					
				}
			}

			if (logger.isDebugEnabled()) {
				logger.debug("[PH] :: updateGlobalTitle Global title to be updated is  "
						+ gt);
			}
			if (gt!=null && StringUtils.isNotBlank(tt)) {

				if (logger.isDebugEnabled()) {
					logger.debug("[PH] :: updateGlobalTitle set translationType "
							+ tt);
				}
				
				Integer destt = Integer.parseInt(tt);
				translationType = destt.byteValue();

				if (userAdd.getGlobalTitle().getGTIndicator() == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0001) {
					GTIndicator0001 gt1 = (GTIndicator0001) userAdd
							.getGlobalTitle();
					gt1.setTranslationType(translationType);
					gt = gt1;

				} else if (userAdd.getGlobalTitle().getGTIndicator() == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0010) {
					GTIndicator0010 gt2 = (GTIndicator0010) userAdd
							.getGlobalTitle();
					gt2.setTranslationType(translationType);
					gt = gt2;
				} else if (userAdd.getGlobalTitle().getGTIndicator() == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0011) {
					GTIndicator0011 gt3 = (GTIndicator0011) userAdd
							.getGlobalTitle();

					gt3.setTranslationType(translationType);
					gt = gt3;
					// adding Translation Type
				} else if (userAdd.getGlobalTitle().getGTIndicator() == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0100) {
					GTIndicator0100 gt4 = (GTIndicator0100) userAdd
							.getGlobalTitle();
					gt4.setTranslationType(translationType);
					gt = gt4;
				}
			}

			if (gt != null && StringUtils.isNotBlank(gtDigits)) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH] :: updateGlobalTitle set gt digits "
							+ gtDigits);
				}
				byte[] digits = CommonUtils.encodeDigitsInBcdFormat(gtDigits);
				gt.setAddressInformation(digits);
			}

		} catch (MandatoryParameterNotSetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParameterNotSetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: updateGlobalTitle exit  " + gt);
		}
		return gt;

	}

	private static GlobalTitle createGlobalTitle(int gtInd) {
		GlobalTitle gt = null;
		// specific GT params

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: createGlobalTitle for GT Indicator " + gtInd);
		}
		if (gtInd == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0001) {
			GTIndicator0001 gt1 = new GTIndicator0001();
			gt = gt1;

		} else if (gtInd == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0010) {
			GTIndicator0010 gt2 = new GTIndicator0010();
			gt = gt2;
		} else if (gtInd == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0011) {
			GTIndicator0011 gt3 = new GTIndicator0011();
			gt = gt3;
			// adding Translation Type
		} else if (gtInd == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0100) {
			GTIndicator0100 gt4 = new GTIndicator0100();
			gt = gt4;
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: createGlobalTitle exit  " + gt);
		}
		return gt;

	}
	
	
	public static SignalingPointCode createSignallingPointCode(String mtp3){
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: createSignallingPointCode   " + mtp3);
		}
		SignalingPointCode mtp3dpc=null;
		if (mtp3 != null && !mtp3.isEmpty()) {
			String[] tmp = mtp3.split("-");
			if (tmp.length == 3) {
				mtp3dpc = new SignalingPointCode(Integer.parseInt(tmp[2]), Integer
						.parseInt(tmp[1]), Integer.parseInt(tmp[0]));
			}
		}else{

			if (logger.isDebugEnabled()) {
				logger.debug( " [PH]::  MTP3_DPC signialling point code cant be created");
			}
		}
		return mtp3dpc;
	}
}
