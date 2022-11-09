package com.agnity.ph.http;

import java.util.Collection;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.TimerService;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.baypackets.ase.cdr.CDRImpl;
import com.baypackets.ase.container.AseApplicationSession;
import com.baypackets.ase.container.AseContext;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.CDRWriteFailedException;
import com.baypackets.ase.spi.container.SasApplicationSession;
import com.genband.tcap.provider.TcapSession;

public class HttpProtocolUtil {
	
	
	private static Logger logger = Logger.getLogger(HttpProtocolUtil.class);
	
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
	public static void startTimer(SipApplicationSession appSession,
			long timeInMillies, boolean persistable, String timerName,Object httpEvent) {

		CallData callData = getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (timeInMillies < 0) {
			logger.error(origLegCallId + ":: Not starting " + timerName
					+ " timer for " + timeInMillies
					+ " milliseconds  value should be a +ve value");
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Start " + timerName
					+ " timer for " + timeInMillies + " milliseconds");
		}
		TimerService timerService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getTimerService();

		PhTimerInfo phTimerInfo = new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.HTTP);
		phTimerInfo.setTimerName(timerName);
		phTimerInfo.setData(httpEvent);
		ServletTimer appTimer = timerService.createTimer(appSession,
				timeInMillies, persistable, phTimerInfo);

		appSession.setAttribute(timerName, appTimer.getId());

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::" + timerName
					+ " Timer started; id=" + appTimer.getId()
					+ ", time remaining = " + appTimer.getTimeRemaining());
		}
	}
	
	
	/**
	 * This method stops the timer.
	 * 
	 * @param appSession
	 *            represents an instance of SipApplicationSession
	 * @param timerName
	 *            represents an instance of String
	 */
	public static void stopTimer(SipApplicationSession appSession,
			String timerName) {
		CallData callData = getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside stopTimer for " + timerName);
		}

		appSession.removeAttribute(timerName);

		// Cancel at last to replicate the app session changes
		Collection<ServletTimer> timers = appSession.getTimers();
		
		for (ServletTimer timer : timers) {
			if (timer != null) {

				Object timerInfo = timer.getInfo();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: timerInfo is " + timerInfo);
				}
				if (timerInfo != null && timerInfo instanceof PhTimerInfo) {

					PhTimerInfo phTimerInfo = (PhTimerInfo) timer.getInfo();
					if (timerInfo != null
							&& phTimerInfo.getTimerName().equals(timerName)) {
						timer.cancel();
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + ":: " + timerName
									+ " stopped");
						}
						return;
					}
				}
			}
		}

		logger.debug(origLegCallId + ":: " + timerName + " not found in all timers");
	}

	/**
	 * This method start the timer
	 * 
	 * @param timerService
	 *            represents an instance of TimerService
	 * @param appSession
	 *            represents an instance of SipApplicationSession
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
			SipApplicationSession appSession, long initialDelay,
			long timeInMillies, boolean delayRecovery, boolean persistable,
			String timerName) {
		CallData callData = getCallData(appSession);

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Start " + timerName + " timer for "
					+ timeInMillies + " milliseconds");
			logger.debug(origLegCallId + ":: Initial Delay is " + initialDelay
					+ ", delayRecovery flag is " + delayRecovery
					+ " persistable flag is " + persistable);
		}

		PhTimerInfo phTimerInfo = new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.HTTP);
		phTimerInfo.setTimerName(timerName);

		ServletTimer appTimer = timerService.createTimer(appSession,
				initialDelay, timeInMillies, delayRecovery, persistable,
				phTimerInfo);

		appSession.setAttribute(timerName, appTimer.getId());

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::" + timerName
					+ " Timer started; id=" + appTimer.getId()
					+ ", time remaining = " + appTimer.getTimeRemaining());
		}
	}
	
	
	/**
	 * This method returns the call data from the reference saved in app
	 * session. For the ASSIST case or timeout cases of INAP call, it might be
	 * possible that appsession does not have call data reference. In this case
	 * call data is retrieved from tcap session.
	 * 
	 * @param appSession
	 *            represents an instance of SipApplicationSession
	 * @return an instance of CallData
	 */
	public static CallData getCallData(SipApplicationSession appSession) {

		CallData callData = (CallData) appSession.getAttribute(CallData.CALL_DATA);

		if (callData == null) {
			TcapSession tcapSession = getTcapSession(appSession);
			if (tcapSession != null) {
				callData = getCallData(tcapSession);
			}
		}
		if (callData == null) {
			if (logger.isInfoEnabled()) {
				logger.info("CallData is null");
			}
		}
		return callData;
	}
	
	
	/**
	 * This method returns the calldata from the reference saved in tcap
	 * session.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an instance of CallData
	 */
	public static CallData getCallData(TcapSession tcapSession) {
		return (CallData) tcapSession.getAttribute(CallData.CALL_DATA);
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
			tcapSession = PhUtilityServices.getInstance(serviceId).getTcapProvider().getTcapSession(dialogId);
		}
		return tcapSession;
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
			synchronized(callData){
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside writeServiceCdr with appSession");
			}

			CDR cdrRef = (CDR) callData
					.get(CallDataAttribute.P_CDR_REF);
			
			if(action!=null){
				writeServiceCdr(cdrRef, callData ,action.getApplicationName());
			}else{
				writeServiceCdr(cdrRef, callData ,null);
			}
			

			// mark final cdr written in order to avoid writing 2 CDRS
			// however it needs to disabled in case of service chaining 
			// so that next applicaiton can write CDRs
			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN,PhConstants.TRUE);
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
	 * Called by the "getAttribute" method to get the CDR object
	 * to be put into this session's attribute map.
	 */
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
