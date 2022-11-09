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

import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.TimerService;

import com.agnity.mphdata.common.*;

import org.apache.log4j.Logger;

import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ServiceInterface;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.CDRWriteFailedException;
import com.genband.tcap.provider.TcapSession;

/**
 * This class is the utility class used by CAP protocol handler to perform CAP protocol
 * specific processing.
 * 
 */

public class Capv2CS1ScfProtocolUtil {

	private static Logger logger = Logger
			.getLogger(Capv2CS1ScfProtocolUtil.class);
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
	 * @return an instance of CallData
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
	 * This method returns the TCAP application session using reference save in SIP appsession.
	 * It returns Tcap Notify AppSession.
	 * @param appSession represents an instance of SipApplicationSession
	 * @return an instance of TcapSession
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
	 *            represents an instance of tcap session
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
		phTimerInfo.setProtocol(Protocol.CAPV2_SCF);
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

	/**
	 * @param callData
	 * @param legID
	 * @return Returns ERB set added by application
	 */
	public static Set<Action.ERB_TYPE> getErbSetByApplication(CallData callData,String legID) {

		if (logger.isDebugEnabled()) {
			logger.debug("getErbSetByApplication for Leg "+legID);
		}
		Set<Action.ERB_TYPE> erbSet=null;
		if (legID != null) {
			LegData legData = (LegData) callData
					.get(CallDataAttribute.valueOf(legID));
			erbSet = (Set<Action.ERB_TYPE>) legData
					.get(LegDataAttributes.P_ERB_SET);
		}else{
			LegData legData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
			erbSet = (Set<Action.ERB_TYPE>) legData
					.get(LegDataAttributes.P_ERB_SET);
		}


		if (logger.isDebugEnabled()) {
			logger.debug("getErbSetByApplication Leaving with set "+erbSet);
		}
		return erbSet;
	}

	/**
	 * @param callData
	 * @return Returns invoke id parameter
	 */
	public static int getNextInvokeId(CallData callData){
		int nextInvokeId= callData.get(CallDataAttribute.P_INVOKE_ID)!=null?(Integer)callData.get(CallDataAttribute.P_INVOKE_ID):0;
		nextInvokeId++;
		callData.set(CallDataAttribute.P_INVOKE_ID,nextInvokeId);
		return nextInvokeId;
	}

	/**
	 * @param callData
	 * @return Returns last invoke id parameter
	 */
	public static int getLastInvokeId(CallData callData){
		int lastInvokeId= callData.get(CallDataAttribute.P_INVOKE_ID)!=null?(Integer)callData.get(CallDataAttribute.P_INVOKE_ID):0;
		return lastInvokeId;
	}

	/**
	 * @param callData
	 * @return 
	 */
	public static int getLastInvokeIdEndRange(CallData callData){
		int lastInvokeIdendRange=callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END)!=null? (Integer)callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END):0;
		return lastInvokeIdendRange;
	}


	/**
	 * @param callData
	 * @return
	 */
	public static int getLastInvokeIdStartRange(CallData callData){
		int invokeIDRangeStart=(Integer)callData.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_START);
		return invokeIDRangeStart;
	}


	/**
	 * @param lastInvokeIdStartRange
	 * @param callData
	 */
	public static void setLastInvokeIdStartRange(int lastInvokeIdStartRange,CallData callData){
		callData.set(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_START,lastInvokeIdStartRange);
	}


	/**
	 * @param lastInvokeIdEndRange
	 * @param callData
	 */
	public static void setLastInvokeIdEndRange(int lastInvokeIdEndRange,CallData callData){
		callData.set(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END,lastInvokeIdEndRange);
	}

	/**
	 * @param callData
	 * @return
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
	 * @param callData
	 * @param appSession
	 * @param dialogId
	 * @return CDR object
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
	 * @param callData
	 * @return 
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
	 * @param callData
	 * @return
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

		long corrTime=30000;

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
		phTimerInfo.setProtocol(Protocol.CAPV2_SCF);
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
				Object timerInfo = timer.getInfo();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: timerInfo is " + timerInfo);
				}
				if (timerInfo != null && timerInfo instanceof PhTimerInfo) {

					PhTimerInfo phTimerInfo = (PhTimerInfo) timer.getInfo();
					if (timerInfo != null
							&& phTimerInfo.getTimerName().equals(timerName)) {
						timer.cancel();
						if (logger.isDebugEnabled()) {
							logger.debug(dialogId + ":: " + timerName
									+ " stopped");
						}
						return;
					}
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
	 * @param tcapSession
	 */
	public static void cleanupCorrelationResources(TcapSession tcapSession) {
		try {
			CallData callData = getCallData(tcapSession);

			LegData legData=(LegData)callData.get(CallDataAttribute.P_LEG1);

			CapV2CallStates state=(CapV2CallStates)legData.get(LegDataAttributes.P_LEG_SS7_STATE);

			int dialogId = (Integer) callData
					.get(CallDataAttribute.P_DIALOG_ID);
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Inside cleanupCorrelationResources in orig state "+state);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Stop correlation timer if running");
			}

			stopTimer(tcapSession, PhConstants.CORRELATION_TIMER);

			Object corrIdObj=callData
					.get(CallDataAttribute.P_CORRELATION_ID);

			if (corrIdObj != null) {

				String correlationId = (String)corrIdObj;

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Remove the correlation id " + correlationId);
				}

				/*
				 *  changes done to support cleanup on FT;as we are replicating servlet cCorrelationMap might be null
				 */
				Map<String, Object> cCorrelationMap = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap();
				if (cCorrelationMap != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Cleanup corr resources correlation map is not null");
					}
					cCorrelationMap.remove(correlationId);
				}
			}


		} catch (Exception e) {
			logger.warn("Error in correlation resource cleanup " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info("Error in cleanupCorrelationResources.", e);
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

}
