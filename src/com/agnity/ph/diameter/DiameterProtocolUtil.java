package com.agnity.ph.diameter;

import javax.servlet.sip.SipApplicationSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.http.HttpProtocolUtil;
import com.baypackets.ase.cdr.CDRImpl;
import com.baypackets.ase.container.AseApplicationSession;
import com.baypackets.ase.container.AseContext;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.CDRWriteFailedException;

public class DiameterProtocolUtil {

	private static Logger logger = Logger
			.getLogger(DiameterProtocolUtil.class);

	/**
	 * This utility method is for writing service cdr with appSession.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 */
	public static void writeServiceCdr(CallData callData, Action action) {
		// CallData callData = null;
		String origLegCallId = null;
		try {
			// callData = SipProtocolUtil.getCallData(appSession);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside writeServiceCdr with appSession");
			}

			CDR cdrRef = (CDR) callData.get(CallDataAttribute.P_CDR_REF);

			if (action != null) {
				writeServiceCdr(cdrRef, callData, action.getApplicationName());
			} else {
				writeServiceCdr(cdrRef, callData, null);
			}

			// mark final cdr written in order to avoid writing 2 CDRS
			// however it needs to disabled in case of service chaining
			// so that next applicaiton can write CDRs
			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN,
					PhConstants.TRUE);

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
	private static void writeServiceCdr(CDR cdr, CallData callData,
			String appName) throws CDRWriteFailedException {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		String prevSvcId = (String) callData
				.get(CallDataAttribute.PREV_SERVICE_ID);
		if (PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: Final CDR already written. So returning without writing CDR");
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: writing final CDR");
			}

			if (prevSvcId == null) {
				return;
			}
		}

		/**
		 * if appname no specified then take current app name
		 */
		if (appName == null) {
			appName = (String) callData.get(CallDataAttribute.SERVICE_ID);
		}
		ServiceInterface serviceHandler = PhUtilityServices
				.getInstance(appName).getServiceHandler();
		String[] cdrArr = serviceHandler.getServiceCdr(callData);

		if (cdrArr != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: Invoking write method on CDR utility");
			}
			try {
				cdr.write(cdrArr);
				// callData.set(
				// CallDataAttribute.P_FINAL_CDR_WRITTEN, PhConstants.TRUE);
			} catch (Exception e) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + " :: could not write CDR ..."
							+ e.getMessage());
				}

			}
		}

	}

	/**
	 * Called by the "getAttribute" method to get the CDR object to be put into
	 * this session's attribute map.
	 */
	public static CDR getCDR(AseApplicationSession appSession, String sessionId) {

		if (logger.isDebugEnabled())
			logger.info("getCDR() called to get CDR ref");
		// Get the CDRContext of the app that this session is associated with...
		if (appSession == null) {
			if (logger.isDebugEnabled())
				logger.info("getCDR() called appsesion is currently null will add cdr ref later on");
			return null;
		}
		// AseApplicationSession appSession =
		// (AseApplicationSession)this.getApplicationSession();
		AseContext app = appSession.getContext();
		CDR cdr = app.getCDRContext(sessionId).createCDR();

		// Populate the CDR with the initial values...
		// Marking it as default CDR
		cdr.set(CDR.DEFAULT_CDR, CDR.DEFAULT_CDR);
		// cdr.set(CDR.CORRELATION_ID,
		// appSession.getAttribute(Constants.CORRELATION_ID));
		cdr.set(CDR.SESSION_ID, sessionId);
		// / cdr.set(CDR.ORIGINATING_NUMBER,
		// ((SipURI)m_localParty.getURI()).getUser()); /coomenting for axtel it
		// is coming as null for tcap
		// cdr.set(CDR.TERMINATING_NUMBER,
		// ((SipURI)m_remoteParty.getURI()).getUser());
		// cdr.set(CDR.CALL_START_TIMESTAMP,
		// String.valueOf(this.getSession().getCreationTime()));
		// cdr.set(CDR.BILL_TO_NUMBER,
		// ((SipURI)m_localParty.getURI()).getUser());

		// Special case for handling the default CDR implementation:
		// We associate the host and app name with the CDR here so that the
		// CDRContext can be looked up and re-associated with the CDR after
		// it has been replicated.
		// We also set a flag indicating if the CDR is associated with a
		// distributable app or not. If it is distributable, only Serializable
		// attribute values will be allowed to be set on it.
		if (cdr instanceof CDRImpl) {
			((CDRImpl) cdr).setHostName(app.getParent().getName());
			((CDRImpl) cdr).setAppName(app.getName());
			((CDRImpl) cdr).setDistributable(app.isDistributable());
		}
		if (logger.isDebugEnabled())
			logger.info("getCDR() Leaving with ref " + cdr);
		return cdr;
	}
	
	
	/**
	 * This method is used to start a timer requested by application
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void startApplicationTimer(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception{
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside startApplicationTimer"
					+ action.getTimerName());
		}

		String timerName = PhConstants.APP_TIMER;

		if (action.getTimerName() != null) {
			timerName = timerName + action.getTimerName();
		}
		HttpProtocolUtil.startTimer(appSession, action.getTimerTimeInMills(),
				false, timerName,null);
		
		ServiceInterface serviceHandler = PhUtilityServices
				.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		Event appEvent = new Event(EventType.EVENT_APP_TIMER_STARTED, Protocol.HTTP, action.getLeg());

		ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
	}

	
	/**
	 * This method is used to stop a timer requested by application
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void stopApplicationTimer(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception{
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside stopApplicationTimer"
					+ action.getTimerName());
		}

		String timerName = PhConstants.APP_TIMER;

		if (action.getTimerName() != null) {
			timerName = timerName + action.getTimerName();
		}
		HttpProtocolUtil.stopTimer(appSession, timerName);

		try{
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event appEvent = new Event(EventType.EVENT_APP_TIMER_STOPPED, Protocol.HTTP, action.getLeg());

			ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
		}catch(Exception ex){
			logger.error(origLegCallId +"Error occured while stopping application timer : " + action.getTimerName() );
			logger.error(origLegCallId +"Error occured : " + ex);
			throw ex;
		}
	}

}
