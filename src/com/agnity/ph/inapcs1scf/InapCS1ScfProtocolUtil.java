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
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.SipSessionsUtil;
import javax.servlet.sip.TimerService;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;

import com.agnity.inapitutcs2.asngenerated.LegType;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.InapCallStates;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.SS7ErbModeAndLegInfo;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.CDRWriteFailedException;
import com.baypackets.ase.util.stpool.Token;
import com.genband.tcap.provider.TcapSession;

/**
 * This class is the utility class used by INAP protocol handler to perform inap protocol
 * specific processing.
 */
public class InapCS1ScfProtocolUtil {

	private static Logger logger = Logger.getLogger(InapCS1ScfProtocolUtil.class);

	public static Object src = "source".intern();
	// Leg Types
	public static LegType leg1Type = new LegType(new byte[]{0x01});
	public static LegType leg2Type = new LegType(new byte[]{0x02});
	
	private static HashMap<String, Integer> protocolVersionMap;
	static {
		protocolVersionMap = new HashMap<>();
		
		String protcolVersionList = InapCS1ScfProtocolConfig.getConfigData(InapCS1ScfProtocolConfig.ITUT_SEND_PROTOCOL_VER);
		if (StringUtils.isNotBlank(protcolVersionList)) {
			String[] apps = StringUtils.split(protcolVersionList, ",");
			if(apps.length != 0){
				for(int i=0; i < apps.length; ++i){
					String[] app = StringUtils.split(apps[i], ":");
					String appId = app[0];
					Integer value = Integer.parseInt(app[1]);
					if(logger.isDebugEnabled()){
						logger.debug("app to be added:" + appId + " with value "+ value);
					}
					protocolVersionMap.put(appId, value);
				}
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: protocolVersionMap is " + protocolVersionMap);
		}
	}
	/**
	 * This method starts the timer
	 *
	 * @param timeInMillies represents long value of time
	 * @param persistable   represents boolean flag
	 * @param timerName     represents an instance of String
	 */
	public static void startTimer(TcapSession tcapSession, long timeInMillies, boolean persistable, String timerName) {
		CallData callData = getCallData(tcapSession);
		SipApplicationSession appSession = getAppSession(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Start " + timerName + " timer for " + timeInMillies + " milliseconds");
		}
		TimerService timerService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTimerService();

		PhTimerInfo phTimerInfo = new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.ITUINAPCS1_SCF);
		phTimerInfo.setTimerName(timerName);
		ServletTimer appTimer = timerService.createTimer(appSession, timeInMillies, persistable, phTimerInfo);

		appSession.setAttribute(timerName, appTimer.getId());

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: " + timerName + " Timer started; id=" + appTimer.getId() + ", time remaining = " + appTimer.getTimeRemaining());
		}
	}

	/**
	 * This method returns the calldata from the reference saved in tcap
	 * session.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @return an instance of CallData
	 */
	public static CallData getCallData(TcapSession tcapSession) {
		return (CallData) tcapSession.getAttribute(CallData.CALL_DATA);
	}

	/**
	 * This method returns the sip application session using reference save in
	 * tcap session. It returns Tcap Notify AppSession.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @return an instance of SipApplicationSession
	 */
	public static SipApplicationSession getAppSession(TcapSession tcapSession) {
		String appSessionId = (String) tcapSession.getAttribute(PhConstants.APPLICATION_SESSION_ID);
		return tcapSession.getAppSession(appSessionId);
	}

	/**
	 * This method returns the sip application session of the correlated INVITE
	 * received for outgoing ASSIST
	 *
	 * @param tcapSession    represents an instance of TcapSession
	 * @param sipSessionUtil represents an instance of SipSessionsUtil
	 * @return an instance of SipApplicationSession
	 */
	public static SipApplicationSession getAssistAppSession(TcapSession tcapSession, SipSessionsUtil sipSessionUtil) {
		SipApplicationSession sipApplicationSession = null;
		String appSessionId = (String) tcapSession.getAttribute(PhConstants.ASSIST_APP_SESSION_ID);
		if (appSessionId != null) {
			sipApplicationSession = sipSessionUtil.getApplicationSessionById(appSessionId);
		}
		return sipApplicationSession;
	}

	/**
	 * Get DPs armed by an application from call Data
	 *
	 * @param callData
	 * @param legID
	 * @return
	 */
	public static Set<Action.ERB_TYPE> getErbSetByApplication(CallData callData, String legID) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getErbSetByApplication for Leg: " + legID);
		}
		Set<Action.ERB_TYPE> erbSet = null;

		//First try to find ERB set in the Leg set in Action
		//If not found, get the ERB from configuration
		if (legID != null) {
			LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legID));
			String erbString = (String) legData.get(LegDataAttributes.P_ERB_SET);
			erbSet = parseErbFromApplication(erbString);
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

	private static Set<Action.ERB_TYPE> getErbSetInConfiguration() {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Fetching getErbSetInConfiguration");
		}
		Set<Action.ERB_TYPE> erbSet = new LinkedHashSet<Action.ERB_TYPE>();
		String configuredERBSet = InapCS1ScfProtocolConfig.getConfigData(InapCS1ScfProtocolConfig.ERB_MODE_AND_LEG_INFO);
		if (StringUtils.isNotBlank(configuredERBSet)) {
			String[] configuredEvents = StringUtils.split(configuredERBSet, "|");
			if(configuredEvents.length != 0){
				for(int i=0; i < configuredEvents.length; ++i){
					String[] erbConfig = StringUtils.split(configuredEvents[i], ":");
					String event = erbConfig[0];

					if(logger.isDebugEnabled()){
						logger.debug("Event to be added:" + event);
					}

					try {    
						Action.ERB_TYPE configuredERBType = Action.ERB_TYPE.valueOf(event);
						erbSet.add(configuredERBType);
					} catch (IllegalArgumentException iae) {
						logger.error("[PH]:: Configured ERB is not a valid Enum value : " + event);
					}
				}
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getErbSetInConfiguration Leaving with set " + erbSet);
		}
		return erbSet;
	}


	/**
	 * get next invoke id for inap message
	 *
	 * @param callData
	 * @return
	 */
	public static int getNextInvokeId(CallData callData) {
		int nextInvokeId = callData.get(CallDataAttribute.P_INVOKE_ID) != null ? (Integer) callData.get(CallDataAttribute.P_INVOKE_ID) : 0;
		nextInvokeId++;
		callData.set(CallDataAttribute.P_INVOKE_ID, nextInvokeId);
		return nextInvokeId;
	}
	
	/**
	 * get next invoke id for outgoing inap message
	 *
	 * @param callData
	 * @return
	 */
	public static int getOutgoingNextInvokeId(CallData callData) {
		int nextInvokeId = callData.get(CallDataAttribute.P_OUTGOING_INVOKE_ID) != null ? (Integer) callData.get(CallDataAttribute.P_OUTGOING_INVOKE_ID) : 0;
		nextInvokeId++;
		callData.set(CallDataAttribute.P_OUTGOING_INVOKE_ID, nextInvokeId);
		return nextInvokeId;
	}

	/**
	 * get last invoke id
	 *
	 * @param callData
	 * @return
	 */
	public static int getLastInvokeId(CallData callData) {
		int lastInvokeId = callData.get(CallDataAttribute.P_INVOKE_ID) != null ? 
				(Integer) callData.get(CallDataAttribute.P_INVOKE_ID) : 0;
		return lastInvokeId;
	}
	
	/**
	 * get Last outgoing invoke id
	 * @param callData
	 * @return
	 */
	public static int getOutgoingLastInvokeId(CallData callData) {
		int lastInvokeId = callData.get(CallDataAttribute.P_OUTGOING_INVOKE_ID) != null ? 
				(Integer) callData.get(CallDataAttribute.P_OUTGOING_INVOKE_ID) : 0;
		return lastInvokeId;
	}

	/**
	 * get last invoke id range
	 *
	 * @param callData
	 * @return
	 */
	public static int getLastInvokeIdEndRange(CallData callData) {
		int lastInvokeIdendRange = callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END) != null ? (Integer) callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END) : 0;
		return lastInvokeIdendRange;
	}

	/**
	 * Gets last invoke id range starts
	 *
	 * @param callData
	 * @return
	 */
	public static int getLastInvokeIdStartRange(CallData callData) {
		int invokeIDRangeStart = (Integer) callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_START);
		return invokeIDRangeStart;
	}
	
	/**
	 * @param callData
	 * @return
	 */
	public static int getOutgoingLastInvokeIdEndRange(CallData callData) {
		int lastInvokeIdendRange = callData.get(CallDataAttribute.P_OUTGOING_LAST_INVOKE_ID_RANGE_END) != null ? 
							(Integer) callData.get(CallDataAttribute.P_OUTGOING_LAST_INVOKE_ID_RANGE_END) : 0;
		return lastInvokeIdendRange;
	}
	
	/**
	 * @param callData
	 * @return
	 */
	public static int getOutgoingLastInvokeIdStartRange(CallData callData) {
		int invokeIDRangeStart = (Integer) callData.get(CallDataAttribute.P_OUTGOING_LAST_INVOKE_ID_RANGE_START);
		return invokeIDRangeStart;
	}

	/**
	 * sets last invoke id start range
	 *
	 * @param lastInvokeIdStartRange
	 * @param callData
	 */
	public static void setLastInvokeIdStartRange(int lastInvokeIdStartRange, CallData callData) {
		callData.set(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_START, lastInvokeIdStartRange);
	}

	/**
	 * sets last invoke id range
	 *
	 * @param lastInvokeIdEndRange
	 * @param callData
	 */
	public static void setLastInvokeIdEndRange(int lastInvokeIdEndRange, CallData callData) {
		callData.set(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END, lastInvokeIdEndRange);
	}

	/**
	 * @param lastInvokeIdStartRange
	 * @param callData
	 */
	public static void setOutgoingLastInvokeIdStartRange(int lastInvokeIdStartRange, CallData callData) {
		callData.set(CallDataAttribute.P_OUTGOING_LAST_INVOKE_ID_RANGE_START, lastInvokeIdStartRange);
	}
	
	/**
	 * @param lastInvokeIdEndRange
	 * @param callData
	 */
	public static void setOutgoingLastInvokeIdEndRange(int lastInvokeIdEndRange, CallData callData) {
		callData.set(CallDataAttribute.P_OUTGOING_LAST_INVOKE_ID_RANGE_END, lastInvokeIdEndRange);
	}
	
	/**
	 * gets last RX invoke id
	 *
	 * @param callData
	 * @return
	 */
	public static int getLastRxInvokeId(CallData callData) {
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		return (Integer) legData.get(LegDataAttributes.P_LAST_RX_INVOKE_ID);
	}
	
	/**
	 * method to get last invoke id for outgoing dialogue
	 * @param callData
	 * @return
	 */
	public static int getOutgoingLastRxInvokeId(CallData callData) {
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		return (Integer) legData.get(LegDataAttributes.P_OUTGOING_LAST_RX_INVOKE_ID);
	}

	public static void updateSS7CallState(LegData legData, InapCallStates newCallState) {
		InapCallStates currentState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("[PH]:: Transitioning SS7 State :: Current State: [%s] ===> New State: [%s]", currentState, newCallState));
		}
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, newCallState);
	}

	/**
	 * This utility method is for writing service cdr with tcap session.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void writeServiceCdr(com.genband.tcap.provider.TcapSession tcapSession) {
		CallData callData = null;
		Integer dialogId = null;

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside writeServiceCdr with tcapSession");
		}

		try {
			callData = getCallData(tcapSession);
			SipApplicationSession appSession = getAppSession(tcapSession);
			dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Inside writeServiceCdr with appSession");
			}

			CDR cdrREf = getCDRRefernceObject(callData, appSession, dialogId);
			if (callData.get(CallDataAttribute.P_CDR_REF) != null) {
				CDR cdrRef = (CDR) callData.get(CallDataAttribute.P_CDR_REF);
				writeServiceCdr(cdrRef, callData);

				// mark final cdr written in order to avoid writing 2 CDRS
				// however it needs to disabled in case of service chaining 
				// so that next applicaiton can write CDRs
				callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN, PhConstants.TRUE);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: CDR Refrence object not found not writing any CDR");
				}
			}
		} catch (Exception ex) {
			logger.error("[PH]:: Failed to write service cdr.", ex);
		}
	}

	/**
	 * This method is used to get CDR object reference for tcap session
	 *
	 * @param appSession
	 * @return
	 */
	private static CDR getCDRRefernceObject(CallData callData, SipApplicationSession appSession, Integer dialogId) {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getCDRRefernceObject");
		}
		CDR cdrRef = null;
		Object cdrRefObject = callData.get(CallDataAttribute.P_CDR_REF);
		if (cdrRefObject == null&& appSession!=null) {
			Iterator<SipSession> sipSessions = (Iterator<SipSession>) appSession.getSessions();
			while (sipSessions.hasNext()) {
				SipSession sipSession = sipSessions.next();
				if (sipSession.getAttribute(PhConstants.TCAP_CDR_REF) != null) {
					cdrRef = (CDR) sipSession.getAttribute(PhConstants.TCAP_CDR_REF);
					break;
				}
			}
			callData.set(CallDataAttribute.P_CDR_REF, cdrRef);
		} else {
			cdrRef = (CDR) cdrRefObject;
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: leaving getCDRRefernceObject with " + cdrRef);
		}
		return cdrRef;

	}

	/**
	 * This method writes the SN CDR if applicable
	 *
	 * @param cdr      represents an instance of CDR
	 * @param callData represents an instance of CallData
	 * @throws CDRWriteFailedException
	 */
	private static void writeServiceCdr(CDR cdr, CallData callData) throws CDRWriteFailedException {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: writeServiceCdr");
		}
		//        if (PhConstants.TRUE.equals(callData.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {
		//            if (logger.isDebugEnabled()) {
		//                logger.debug("[PH]:: Final CDR already written. So returning without writing CDR");
		//            }
		//            return;
		//        }
		ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		String[] cdrArr = serviceHandler.getServiceCdr(callData);

		if (cdr != null && cdrArr != null) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Invoking write method on CDR utility");
			}
			cdr.write(cdrArr);
			// callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN, PhConstants.TRUE);// service will set it if donot want to write cDR on drop call
		}

	}

	/**
	 * This method is used to get Activity Test timer time saved by application
	 * in CallData
	 *
	 * @param callData
	 * @return
	 */
	public static long getActivityTestTime(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getActivityTestTime ");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		Object atTimerDuration = legData.get(LegDataAttributes.P_AT_ACK_EXPIRY_TIME);
		long atTime = 30000;
		if (atTimerDuration != null) {
			atTime = (Long) atTimerDuration;
		} else {
			String timeVal = (String) InapCS1ScfProtocolConfig.getConfigData(InapCS1ScfProtocolConfig.AT_ACK_TIMER);
			atTime = Integer.valueOf(timeVal);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getActivityTestTime for ACK: " + atTime);
		}
		return atTime;
	}

	/**
	 * This method is used to get correlation timer time saved by application in
	 * CallData
	 *
	 * @param callData
	 * @return
	 */
	public static long getCorrelationTime(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getCorrelationTime ");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		Object corrTimerDuration = legData.get(LegDataAttributes.P_CORRELATION_EXPIRY_TIME);
		long corrTime = 30000;
		if (corrTimerDuration != null) {
			corrTime = (Long) corrTimerDuration;
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getCorrelationTime returning with value: " + corrTime);
		}
		return corrTime;
	}

	/**
	 * This method is called to start the CDR timer for the intermediate CDRs
	 *
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void startCdrTimer(TcapSession tcapSession) {

		CallData callData = getCallData(tcapSession);
		SipApplicationSession appSession = getAppSession(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: startCdrTimer ");
		}

		Date callConnectDateTime = (Date) callData.get(CallDataAttribute.P_CALL_CONNECT_TIME);
		long initialDelay = CommonUtils.getInitialDelay(callConnectDateTime);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Start Intermediate CDR Timer");
		}

		/*
		 * moved before timer creation to replicate updated value
		 */
		CommonUtils.setAppSessionTimeout(appSession, (int) (initialDelay / 60000) + 5, dialogId);

		startTimer(PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTimerService(), tcapSession, initialDelay, 86400000L, false, true, PhConstants.CDR_TIMER);
	}

	/**
	 * This method start the timer
	 *
	 * @param timerService  represents an instance of TimerService
	 * @param tcapSession   represents an instance of TcapSession
	 * @param initialDelay  represents long value of initial delay
	 * @param timeInMillies represents long value of time
	 * @param delayRecovery represents boolean flag
	 * @param persistable   represents boolean flag
	 * @param timerName     represents an instance of String
	 */
	public static void startTimer(TimerService timerService, TcapSession tcapSession, long initialDelay, long timeInMillies, boolean delayRecovery, boolean persistable, String timerName) {
		CallData callData = getCallData(tcapSession);
		SipApplicationSession appSession = getAppSession(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Start" + timerName + " timer for " + timeInMillies + " milliseconds");
			logger.debug("[PH]:: Initial Delay is " + initialDelay + ", delayRecovery flag is " + delayRecovery + " persistable flag is " + persistable);
		}

		PhTimerInfo phTimerInfo = new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.ITUINAPCS1_SCF);
		phTimerInfo.setTimerName(timerName);

		ServletTimer appTimer = timerService.createTimer(appSession, initialDelay, timeInMillies, delayRecovery, persistable, phTimerInfo);

		appSession.setAttribute(timerName, appTimer.getId());

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: " + timerName + " Timer started; id=" + appTimer.getId() + ", time remaining = " + appTimer.getTimeRemaining());
		}
	}

	/**
	 * This method cleanup the resources used for correlation such as remove the
	 * entry from correlation map, stop correlation timer etc
	 *
	 * @param tcapSession
	 */
	public static void cleanupCorrelationResources(TcapSession tcapSession) {
		try {
			CallData callData = getCallData(tcapSession);
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			InapCallStates state = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Inside cleanupCorrelationResources in orig state " + state);
			}

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Stop correlation timer if running");
			}

			stopTimer(tcapSession, PhConstants.CORRELATION_TIMER);
			Object corrIdObj = callData.get(CallDataAttribute.P_CORRELATION_ID);
			if (corrIdObj != null && !(corrIdObj.toString().isEmpty())) {
				String correlationId = (String) corrIdObj;
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Remove the correlation id " + correlationId);
				}
				/*
				 * changes done to support cleanup on FT;as we are replicating
				 * servlet cCorrelationMap might be null
				 */
				Map<String, Object> cCorrelationMap = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap();
				if (cCorrelationMap != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Cleanup corr resources correlation map is not null");
					}
					cCorrelationMap.remove(correlationId);
				}

				InapCS1ScfProtocolUtil.returnTokenToSharedPool(callData);
			}
		} catch (Exception e) {
			logger.warn("[PH]:: Error in correlation resource cleanup " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Error in cleanupCorrelationResources.", e);
			}
		}
	}

	/**
	 * This method stops the timer.
	 *
	 * @param tcapSession represents an instance of SipApplicationSession
	 * @param timerName   represents an instance of String
	 */
	public static void stopTimer(TcapSession tcapSession, String timerName) {
		CallData callData = getCallData(tcapSession);
		SipApplicationSession appSession = getAppSession(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside stopTimer for " + timerName);
		}

		appSession.removeAttribute(timerName);

		/*
		 * Cancel at last to replicate the app session changes
		 */
		Collection<ServletTimer> timers = appSession.getTimers();
		for (ServletTimer timer : timers) {
			if (timer != null) {
				Object timerInfo = timer.getInfo();
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: timerInfo is " + timerInfo);
				}
				if (timerInfo != null && timerInfo instanceof PhTimerInfo) {
					PhTimerInfo phTimerInfo = (PhTimerInfo) timer.getInfo();
					if (timerInfo != null && phTimerInfo.getTimerName().equals(timerName)) {
						timer.cancel();
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: " + timerName + " stopped");
						}
						return;
					}
				}
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: " + timerName + " not found in all timers");
		}
	}

	/**
	 * This method is used to do processing of input string
	 *
	 * @param input
	 * @param resultSize
	 */
	public static String lPad(String input, int resultSize) {
		if (input == null) {
			return input;
		}
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < (resultSize - input.length()); i++) {
			result.append("0");
		}
		result.append(input);
		return result.toString();
	}

	public static int getNumberInRange(int min, int max, int value) {
		int returnValue = value;
		if (value < min) {
			returnValue = min;
		} else if (value > max) {
			returnValue = max;
		} else {
			returnValue = value;
		}
		return returnValue;
	}

	public static boolean isActionExistsInArray(Action[] actions, Action.ActionType actionTypeToSearch) {
		for (Action action : actions) {
			if (action.getActionType() == actionTypeToSearch) {
				return true;
			}
		}
		return false;
	}

	/**
	 * This method is used to start maximum call duration timer configured for a call
	 * @param origLegCallId
	 * @param appSession
	 * @param timeInMillies
	 * @param persistable
	 * @param timerName
	 */
	public static void startMaxCallDurationTimer(int dialogId,
			CallData callData, TcapSession  tcapSession) {

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "startMaxCallDurationTimer............check if timer is configured by service if yes then start it");
		}
		Long maxDuration =null;
		
		if(callData.get(CallDataAttribute.MAX_CALL_DURATION)!=null)
		    maxDuration = ((Number) callData.get(CallDataAttribute.MAX_CALL_DURATION)).longValue();
				
		if (maxDuration == null) {
			String mxDur = InapCS1ScfProtocolConfig
					.getConfigData(InapCS1ScfProtocolConfig.MAX_CALL_DURATION);
			if (mxDur != null) {
				maxDuration = Long.parseLong(mxDur);
			}
		}
		
		if (maxDuration != null && maxDuration.longValue() > -1) {

			callData.set(CallDataAttribute.MAX_CALL_DURATION, maxDuration);
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ "startMaxCallDurationTimer for value " + maxDuration);
			}
			maxDuration = maxDuration*1000;     //convert second into millisecond
			startTimer(tcapSession, maxDuration, true, PhConstants.MAX_CALL_DURATION_TIMER);
		}

	}


	/**
	 * This method is used to get a token form shared pool if it is enabled
	 * @param dialogId
	 * @param callData
	 * @return
	 */
	public static String getTokenFromSharedPool(CallData callData) {

		if (logger.isDebugEnabled()) {
			logger.debug("getTokenFromSharedPool............");
		}
		String correlationId = (String) callData
				.get(CallDataAttribute.P_CORRELATION_ID);

		String corrKey=(String)callData.get(CallDataAttribute.CORRELATION_KEY);

		if (logger.isDebugEnabled()) {
			logger.debug("getTokenFromSharedPool use correlationId as key "+ correlationId);
		}

		if (corrKey == null && correlationId != null) {
			corrKey = correlationId;
			callData.set(CallDataAttribute.CORRELATION_KEY, correlationId);
		}

		String serviceId=(String) callData.get(CallDataAttribute.SERVICE_ID);

		Token token = PhUtilityServices
				.getInstance(
						serviceId)
				.getSharedTokenPool().pop(corrKey,serviceId);

		if (token != null) {
			correlationId = token.getValue() + "";
			callData.set(CallDataAttribute.P_CORRELATION_ID, correlationId);
			callData.set(CallDataAttribute.P_CORRELATION_TOKEN, token);

		} else {
			return null;
		}

		if (logger.isDebugEnabled()) {
			logger.debug("getTokenFromSharedPool  return correlation id as "
					+ correlationId);
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

		String sharedPoolEnabled = InapCS1ScfProtocolConfig
				.getConfigData(InapCS1ScfProtocolConfig.SHARED_TOKEN_POOL_ENABLED);

		if (PhConstants.TRUE.equals(sharedPoolEnabled)) {

			String correlationKey = (String) callData
					.get(CallDataAttribute.CORRELATION_KEY);

			if (logger.isDebugEnabled()) {
				logger.debug("returnTokenToSharedPool for key "
						+ correlationKey);
			}

			Token correlationToken = (Token) callData
					.get(CallDataAttribute.P_CORRELATION_TOKEN);

			if (correlationToken != null) {

				if (logger.isDebugEnabled()) {
					logger.debug("returnTokenToSharedPool use correlationId as key "
							+ correlationKey);
				}

				String serviceId=(String) callData.get(CallDataAttribute.SERVICE_ID);

				PhUtilityServices
				.getInstance(
						serviceId)
				.getSharedTokenPool().push(correlationKey, correlationToken,serviceId);

				callData.set(CallDataAttribute.CORRELATION_KEY, null);
				callData.set(CallDataAttribute.P_CORRELATION_TOKEN, null);
			} 
			if (logger.isDebugEnabled()) {
				logger.debug("returnTokenToSharedPool  return correlation id as "
						+ correlationToken);
			}
		}else{

			if (logger.isDebugEnabled()) {
				logger.debug("returnTokenToSharedPool sharedtoken pool is not enabled");
			}

		}

	}

	/**
	 * This method is used to reset N/W transactions count
	 * @param callData
	 */
	public static void resetNetworkTransactions(CallData callData) {

		if (logger.isDebugEnabled()) {
			logger.debug("resetNetworkTransactions from current value.."+callData
					.get(CallDataAttribute.P_NETWORK_TRANSACTION));
		}
		callData
		.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(0));

	}

	/**
	 * This method is used to start Activity Test timer only if application wants to 
	 * to do that. In order to start the timer, application must set timer in millisec in 
	 * LegDataAttributes.P_AT_EXPIRY_TIME in Leg1 Data
	 * @param dialogueId
	 * @param callData
	 * @param tcapSession
	 */
	public static void startActivityTestTimer(int dialogueId, CallData callData, TcapSession tcapSession) {

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "AT: Inside startActivityTestTimer");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if(legData.get(LegDataAttributes.P_AT_EXPIRY_TIME) == null){
			if(logger.isDebugEnabled()) {
				logger.debug(dialogueId + "AT: Not starting Activity Timer as P_AT_EXPIRY_TIME not set in Leg1");
			}
			return;
		}

		Long maxDuration =null;
        try {
       	 maxDuration	=  Long.parseLong((String) legData.get(LegDataAttributes.P_AT_EXPIRY_TIME));
        }catch(Exception e) {
       	 logger.error("unable to parse P_AT_EXPIRY_TIME in Long" );
        }


		if (maxDuration != null && maxDuration.longValue() > -1) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId
						+ "AT: startActivityTestTimer for value " + maxDuration);
			}
			startTimer(tcapSession, maxDuration, true, com.baypackets.ase.util.Constants.ACTIVITY_TEST_TIMER);
		} else {
			if(logger.isDebugEnabled()) {
				logger.debug(dialogueId + "AT: Not starting Activity Timer as P_AT_EXPIRY_TIME is set in Leg1 but value is null");
			}
		}
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
			// after splitting it shall be like in below format 
			// Application has not set any ERB. Trying to
			String[] configuredEvents = StringUtils.split(erbString, "|");
			if(configuredEvents.length != 0){
				for(int i=0; i < configuredEvents.length; ++i){
					String[] erbConfig = StringUtils.split(configuredEvents[i], ":");
					String event = erbConfig[0];

					if(logger.isDebugEnabled()){
						logger.debug("Event to be added:" + event);
					}

					try {    
						Action.ERB_TYPE configuredERBType = Action.ERB_TYPE.valueOf(event);
						erbSet.add(configuredERBType);
					} catch (IllegalArgumentException iae) {
						logger.error("[PH]:: Configured ERB is not a valid Enum value : " + event);
					}
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
	 * @param callData
	 * @param leg
	 * @return
	 */
	public static Set<SS7ErbModeAndLegInfo> getErbSetByApplicationWithMode(CallData callData, String legID) {
		// fetch ErbModeAndLegInfo set through properties 
		String erbModeAndLegInfo = InapCS1ScfProtocolConfig
				.getConfigData(InapCS1ScfProtocolConfig.ERB_MODE_AND_LEG_INFO);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: getErbSetByApplicationWithMode for Leg: " + legID + ", configured mode:" +
					erbModeAndLegInfo	);
		}

		Set<SS7ErbModeAndLegInfo> erbSetWithMode = new LinkedHashSet<SS7ErbModeAndLegInfo>();
		Set<Action.ERB_TYPE> erbSet = null;
		SS7ErbModeAndLegInfo erbObj = null;

		//First try to find ERB set in the Leg set in Action
		//If not found, get the ERB from configuration
		if (legID != null) {
			LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legID));
			String erbString = (String) legData.get(LegDataAttributes.P_ERB_SET);
			if(StringUtils.isNotBlank(erbString)){
				erbSet = parseErbFromApplication(erbString);
				erbModeAndLegInfo = erbString;
			}
		}
		if (CollectionUtils.isEmpty(erbSet)) {
			logger.debug("[PH]:: Application has not set any ERB. Trying to set as per configuration..");
			erbSet = getErbSetInConfiguration();
		}

		if(erbSet != null){
			String[] erbList = StringUtils.split(erbModeAndLegInfo, "|");

			for (int i=0; i < erbList.length; ++i){
				String[] erbVal = StringUtils.split(erbList[i], ":");
				int mode = 0;
				int legType = 0;

				if(StringUtils.isNotBlank(erbVal[1]) && StringUtils.equalsIgnoreCase(erbVal[1], "1")){
					mode = 1;
				}

				if(StringUtils.isNotBlank(erbVal[2])){
					if(StringUtils.equalsIgnoreCase(erbVal[2], "Leg1")){
						legType = 1;
					} else if(StringUtils.equalsIgnoreCase(erbVal[2], "Leg2")){
						legType = 2;
					}else if(StringUtils.equalsIgnoreCase(erbVal[2], "Both")){
						legType = 3;
					}
				}

				if(erbSet.contains(Action.ERB_TYPE.valueOf(erbVal[0]))){
					erbObj = new SS7ErbModeAndLegInfo(Action.ERB_TYPE.valueOf(erbVal[0]), mode, legType);
					erbSetWithMode.add(erbObj);
					
					if(logger.isDebugEnabled()){
						logger.debug("Final ERB Added: " + erbVal[0] + ", mode:" + 
								((mode==1)?"NotifyAndContinue":"Interrupt") + ", legType:" + 
								((legType==1)?"Leg1":((legType==2)?"Leg2":"Both")));
					}
				}else{
					logger.error("Did not find ERBTYPE getErbSetByApplicationWithMode: "+ erbVal[0]);
				}
			}
		}
		
		if(logger.isDebugEnabled()){
			logger.debug("getErbSetByApplicationWithMode Exit: total configured:"+ erbSetWithMode.size());
		}
		return erbSetWithMode;
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
	 * 
	 * @param callData
	 * @param leg2Data
	 * @return
	 */
       public static SccpUserAddress updateCallingAddress(CallData callData,LegData leg2Data){
    	   
    	   if (logger.isDebugEnabled()) {
				logger.debug(" [PH]:: updateCallingAddress SUA ");
			}
    	   
		SccpUserAddress selfSUA=(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS);
		
		if (logger.isDebugEnabled()) {
			logger.debug(" [PH]:: Routing Indicator on P_SCCP_REMOTE_USER_ADDRESS is  "+ selfSUA.getRoutingIndicator());
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
				.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);
		
		if (logger.isDebugEnabled()) {
			logger.debug(" [PH]:: Routing Indicator on P_SCCP_LOCAL_USER_ADDRESS is  "+ selfSUA.getRoutingIndicator());
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
       
	private static GlobalTitle updateGlobalTitle(SccpUserAddress userAdd,
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
      			logger.debug("[PH] :: createGlobalTitle for GT Indicator "+ gtInd);
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
      			logger.debug("[PH] :: createGlobalTitle exit  "+gt);
      		}
      		return gt;

      	}

	public static HashMap<String, Integer> getProtocolVersionMap() {
		return protocolVersionMap;
	}
 
}
