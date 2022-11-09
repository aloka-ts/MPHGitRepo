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

import jain.MandatoryParameterNotSetException;
import jain.ParameterNotSetException;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;
import jain.protocol.ss7.tcap.GlobalTitle;

import java.util.Collection;
import java.util.Iterator;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.TimerService;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AddressSignal;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.diameter.sh.DiameterShProtocolUtil;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.CDRWriteFailedException;
import com.baypackets.ase.spi.container.SasApplicationSession;
import com.genband.tcap.provider.TcapSession;

/**
 * 
 *This class is the utility class used by MAP protocol handler to perform inap protocol
 *specific processing.
 */
public class MapScfProtocolUtil {

	private static Logger logger = Logger
			.getLogger(MapScfProtocolUtil.class);

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
	 * This method extracts CallData from appSessions
	 * 
	 * @param appSession
	 * @return
	 */
	public static CallData getCallData(SipApplicationSession appSession) {
		CallData callData = (CallData) appSession
				.getAttribute(CallData.CALL_DATA);
		if (callData == null) {
			if (logger.isInfoEnabled()) {
				logger.info("CallData is null");
			}
		}
		return callData;
	}

	/**
	 * This method returns the sip application session using reference save in
	 * tcap session. It returns Tcap Notify AppSession.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @return an instance of SipApplicationSession
	 */
	public static SipApplicationSession getAppSession(TcapSession tcapSession) {
		String appSessionId = (String) tcapSession
				.getAttribute(PhConstants.APPLICATION_SESSION_ID);
		return tcapSession.getAppSession(appSessionId);
	}

	public static TcapSession getTcapSession(SipApplicationSession appSession) {
		Integer dialogId = (Integer) appSession
				.getAttribute(PhConstants.TCAP_SESSION_ID);
		String serviceId = (String) appSession.getAttribute(PhConstants.SERVICE_ID);
		TcapSession tcapSession = null;
		/*
		 * Following dialogId null check is required as in SIP-T calls, when
		 * initial INVITE is received then dialogId would be null. But
		 * TcapProvider.getTcapSession method takes primitive int as its
		 * argument. And if we try to pass null to this method, NullpointerEx is
		 * raised
		 */
		if (dialogId != null) {
			tcapSession = PhUtilityServices.getInstance(serviceId).getTcapProvider()
					.getTcapSession(dialogId);
		}
		return tcapSession;
	}

	/**
	 * This method starts the timer
	 * 
	 * @param timerService
	 *            represents an instance of TimerService
	 * @param appSession
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
		CallData callData = getCallData(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		SipApplicationSession appSession = getAppSession(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Start " + timerName + " timer for "
					+ timeInMillies + " milliseconds");
		}
		TimerService timerService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getTimerService();

		PhTimerInfo phTimerInfo = new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.MAP_SCF);
		phTimerInfo.setTimerName(timerName);
		ServletTimer appTimer = timerService.createTimer(appSession,
				timeInMillies, persistable, phTimerInfo);

		appSession.setAttribute(timerName, appTimer.getId());

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::" + timerName + " Timer started; id="
					+ appTimer.getId() + ", time remaining = "
					+ appTimer.getTimeRemaining());
		}
	}


	/**
	 * get next invoke id for inap message
	 * @param callData
	 * @return
	 */
	public static int getNextInvokeId(CallData callData) {
		int nextInvokeId = callData
				.get(CallDataAttribute.P_INVOKE_ID) != null ? (Integer) callData
				.get(CallDataAttribute.P_INVOKE_ID) : 0;
		nextInvokeId++;
		callData.set(CallDataAttribute.P_INVOKE_ID, nextInvokeId);
		return nextInvokeId;
	}

	/**
	 * get last invoke id
	 * @param callData
	 * @return
	 */
	public static int getLastInvokeId(CallData callData) {
		int lastInvokeId = callData
				.get(CallDataAttribute.P_INVOKE_ID) != null ? (Integer) callData
				.get(CallDataAttribute.P_INVOKE_ID) : 0;
		return lastInvokeId;
	}

	/**
	 * get last invoke id range
	 * @param callData
	 * @return
	 */
	public static int getLastInvokeIdEndRange(CallData callData) {
		int lastInvokeIdendRange = callData
				.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END) != null ? (Integer) callData
				.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END)
				: 0;
		return lastInvokeIdendRange;
	}

	/**
	 * Gets last invoke id range starts
	 * @param callData
	 * @return
	 */
	public static int getLastInvokeIdStartRange(CallData callData) {
		int invokeIDRangeStart = (Integer) callData
				.get(CallDataAttribute.P_LAST_INVOKE_ID_RANGE_START);
		return invokeIDRangeStart;
	}

	/**
	 * sets last invoke id start range
	 * @param lastInvokeIdStartRange
	 * @param callData
	 */
	public static void setLastInvokeIdStartRange(int lastInvokeIdStartRange,
			CallData callData) {
		callData.set(
				CallDataAttribute.P_LAST_INVOKE_ID_RANGE_START,
				lastInvokeIdStartRange);
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
        
		
		GlobalTitle gt=AinScfProtocolUtil.updateGlobalTitle(selfSUA, gtInd, tt, gtDigits);
		selfSUA.setGlobalTitle(gt);
		
		return selfSUA;
	}
       
       
       
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

   		GlobalTitle gt =AinScfProtocolUtil.updateGlobalTitle(selfSUA, gtInd, tt, gtDigits);
   		
   		if (gt != null) {
   			selfSUA.setGlobalTitle(gt);
   		}else{
   			if (logger.isDebugEnabled()) {
   				logger.debug(" [PH]:: GT is not set on SUA");
   			}
   		}

   		return selfSUA;
   	}
          

	/**
	 * sets last invoke id range
	 * @param lastInvokeIdEndRange
	 * @param callData
	 */
	public static void setLastInvokeIdEndRange(int lastInvokeIdEndRange,
			CallData callData) {
		callData.set(
				CallDataAttribute.P_LAST_INVOKE_ID_RANGE_END,
				lastInvokeIdEndRange);
	}

	/**
	 * gets last RX invoke id
	 * @param callData
	 * @return
	 */
	public static int getLastRxInvokeId(CallData callData) {
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int lastRxInvokeId = (Integer) legData
				.get(LegDataAttributes.P_LAST_RX_INVOKE_ID);

		return lastRxInvokeId;
	}

	/**
	 * This utility method is for writing service cdr with tcap session.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 */
	public static void writeServiceCdr(
			com.genband.tcap.provider.TcapSession tcapSession) {
		CallData callData = null;
		Integer dialogId = null;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside writeServiceCdr with tcapSession");
		}

		try {
			callData = getCallData(tcapSession);
			synchronized (callData) {
				SipApplicationSession appSession = getAppSession(tcapSession);
				dialogId = (Integer) callData
						.get(CallDataAttribute.P_DIALOG_ID);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Inside writeServiceCdr with appSession");
				}

				CDR cdrREf = getCDRRefernceObject(callData, appSession,
						dialogId);

				if (callData.get(CallDataAttribute.P_CDR_REF) != null) {
					CDR cdrRef = (CDR) callData
							.get(CallDataAttribute.P_CDR_REF);
					writeServiceCdr(cdrRef, callData);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: CDR Refrence object not found not writing any CDR");
					}
				}
				MapScfProtocolUtil.setAppSessionTimeout(appSession, 1, ""+dialogId);
			}
		} catch (Exception ex) {
			logger.error(dialogId + ":: Failed to write service cdr.", ex);
		}
	}

	/**
	 * This method is used to get CDR object reference for tcap session
	 * 
	 * @param appSession
	 * @return
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
	private static void writeServiceCdr(CDR cdr, CallData callData)
			throws CDRWriteFailedException {
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: writeServiceCdr");
		}
		if (PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: Final CDR already written. So returning without writing CDR");
			}
			return;
		}
		ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();
		String[] cdrArr = serviceHandler.getServiceCdr(callData);

		if (cdr != null && cdrArr != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: Invoking write method on CDR utility");
			}
			cdr.write(cdrArr);
			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN,
					PhConstants.TRUE);
		}

	}

	/**
	 * This method start the timer
	 * 
	 * @param timerService
	 *            represents an instance of TimerService
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param initialDelay
	 *            represents long value of initial delay
	 * @param timeInMillies
	 *            represents long value of time
	 * @param delayRecovery
	 *            represents boolean flag
	 * @param persistable
	 *            represents boolean flag
	 * @param timerName
	 *            represents an instance of String
	 */
	public static void startTimer(TimerService timerService,
			TcapSession tcapSession, long initialDelay, long timeInMillies,
			boolean delayRecovery, boolean persistable, String timerName) {
		CallData callData = getCallData(tcapSession);
		SipApplicationSession appSession = getAppSession(tcapSession);

		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Start" + timerName + " timer for "
					+ timeInMillies + " milliseconds");
			logger.debug(dialogId + ":: Initial Delay is " + initialDelay
					+ ", delayRecovery flag is " + delayRecovery
					+ " persistable flag is " + persistable);
		}

		PhTimerInfo phTimerInfo = new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.MAP_SCF);
		phTimerInfo.setTimerName(timerName);

		ServletTimer appTimer = timerService.createTimer(appSession,
				initialDelay, timeInMillies, delayRecovery, persistable,
				phTimerInfo);

		appSession.setAttribute(timerName, appTimer.getId());

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
		SipApplicationSession appSession = getAppSession(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside stopTimer for " + timerName);
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
			logger.debug(dialogId + ":: " + timerName
					+ " not found in all timers");
		}
	}

	/**
	 * This utility method is for writing service cdr with appSession.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 */
	public static void writeServiceCdr(CallData callData,Action action) {
		// CallData callData = null;
		String origLegCallId = null;
		try {
			// callData = SipProtocolUtil.getCallData(appSession);
			synchronized (callData) {
				origLegCallId = (String) callData
						.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Inside writeServiceCdr with appSession");
				}

				CDR cdrRef = (CDR) callData.get(CallDataAttribute.P_CDR_REF);

				if (action != null) {
					writeServiceCdr(cdrRef, callData,
							action.getApplicationName());
				} else {
					writeServiceCdr(cdrRef, callData, null);
				}

				// mark final cdr written in order to avoid writing 2 CDRS
				// however it needs to disabled in case of service chaining
				// so that next applicaiton can write CDRs
				callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN,
						PhConstants.TRUE);
			}

		} catch (Exception ex) {
			logger.error(origLegCallId + ":: Failed to write service cdr.", ex);
		}
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
	private static void writeServiceCdr(CDR cdr, CallData callData,String appName)
			throws CDRWriteFailedException {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		String prevSvcId=(String) callData
				.get(CallDataAttribute.PREV_SERVICE_ID);
		if (PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: Final CDR already written. So returning without writing CDR");
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: writing final CDR");
			}
			
			if(prevSvcId==null){
			return;
			}
		}

		/**
		 * if appname no specified then take current app name
		 */
		if(appName==null){
			appName = (String) callData
					.get(CallDataAttribute.SERVICE_ID);
		}
		ServiceInterface serviceHandler = PhUtilityServices.getInstance(appName)
				.getServiceHandler();
		String[] cdrArr = serviceHandler.getServiceCdr(callData);

		if (cdrArr != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: Invoking write method on CDR utility");
			}
			try {
				cdr.write(cdrArr);
				//				callData.set(
				//						CallDataAttribute.P_FINAL_CDR_WRITTEN, PhConstants.TRUE);
			} catch (Exception e) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + " :: could not write CDR ..."
							+ e.getMessage());
				}

			}
		}

	}
	
	/**
	 * Method to encode Digits in BCD format
	 * @param digits
	 * @return
	 */
	public static byte[] encodeDigitsInBcdFormat(String digits){
		byte [] retVal = null;
		if(StringUtils.isNotBlank(digits)){
			try {
				retVal = AddressSignal.encodeAdrsSignal(digits);
			} catch (InvalidInputException e) {
				logger.error("encodeDigitsBcdForamt: exception in encoding:" + digits);
			}
		}

		return retVal;
	}
	
	
	/**
	 * 
	 * @param callData
	 * @param leg2Data
	 * @return
	 */
       public static SccpUserAddress updateCallingAddress(CallData callData,LegData leg2Data,SccpUserAddress selfSUA){
    	   
    	   if (logger.isDebugEnabled()) {
				logger.debug(" [PH]:: updateCallingAddress SUA ");
			}
    	   
//		SccpUserAddress selfSUA=(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);
//		
//		if (logger.isDebugEnabled()) {
//			logger.debug(" [PH]:: Routing Indicator on P_SCCP_LOCAL_USER_ADDRESS is  "+ selfSUA.getRoutingIndicator());
//		}
		
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
        
		
//		GlobalTitle gt=updateGlobalTitle(selfSUA, gtInd, tt, gtDigits);
//		selfSUA.setGlobalTitle(gt);
		
		return selfSUA;
	}
       
       /**
   	 * This method resets the appsession idle timeout. This is required to
   	 * change the appsession timeout from default 5 to some other non-zero
   	 * value.
   	 * 
   	 * @param appSession
   	 *            represents an instance of SipApplicationSession
   	 * @param timeoutInMinutes
   	 *            represents integer value of timeout
   	 * @param origLegCallId
   	 *            represents an instance of String
   	 */
   	public static void setAppSessionTimeout(SipApplicationSession appSession,
   			int timeoutInMinutes, String origLegCallId) {
   		if (logger.isDebugEnabled()) {
   			logger.debug(origLegCallId
   					+ " :: Inside setAppSessionTimeout with value "
   					+ timeoutInMinutes);
   		}
   		if (appSession.isValid() && timeoutInMinutes >= 0) {
   			// Set session expires in minutes
   			((SasApplicationSession) appSession).setTimeout(timeoutInMinutes);
   			if (logger.isDebugEnabled()) {
   				logger.debug(origLegCallId
   						+ " :: AppSession expiry time in minutes "
   						+ timeoutInMinutes);
   			}
   		} else {
   			if (logger.isDebugEnabled()) {
   				logger.debug(origLegCallId
   						+ " :: Not increasing app session timeout as its either already invalidated or you r trying to set its expiry to -ve value "
   						+ timeoutInMinutes);
   			}
   		}
   	}

}
