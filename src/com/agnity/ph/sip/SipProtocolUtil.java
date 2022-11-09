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
package com.agnity.ph.sip;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;
import java.util.concurrent.atomic.AtomicLong;

import javax.mail.BodyPart;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.internet.MimeMultipart;
import javax.servlet.sip.Address;
import javax.servlet.sip.ServletParseException;
import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletMessage;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.SipURI;
import javax.servlet.sip.TelURL;
import javax.servlet.sip.TimerService;
import javax.servlet.sip.URI;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;

import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.AnnSpec.PlayMessage;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CapV2CallStates;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.InapCallStates;
import com.agnity.mphdata.common.InviteAttributes;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.MultiChoiceContact;
import com.agnity.mphdata.common.MultipartBody;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.SignalingTypeEnum;
import com.agnity.mphdata.common.TermRedirectionContact;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.isup.SipIsupParser;
import com.baypackets.ase.sbb.GroupedMsSessionController;
import com.baypackets.ase.sbb.MsCollectPattern;
import com.baypackets.ase.sbb.MsCollectSpec;
import com.baypackets.ase.sbb.MsPlaySpec;
import com.baypackets.ase.sbb.MsVarAnnouncement;
import com.baypackets.ase.sbb.ProcessMessageException;
import com.baypackets.ase.sbb.SBBFactory;
import com.baypackets.ase.sbb.util.Constants;
import com.baypackets.ase.sipconnector.AseDialogManager;
import com.baypackets.ase.sipconnector.AseSipSession;
import com.baypackets.ase.spi.container.SasApplicationSession;
import com.dynamicsoft.DsLibs.DsSdpObject.DsSdpAttribute;
import com.dynamicsoft.DsLibs.DsSdpObject.DsSdpAttributeField;
import com.dynamicsoft.DsLibs.DsSdpObject.DsSdpConnectionField;
import com.dynamicsoft.DsLibs.DsSdpObject.DsSdpField;
import com.dynamicsoft.DsLibs.DsSdpObject.DsSdpMediaDescription;
import com.dynamicsoft.DsLibs.DsSdpObject.DsSdpMediaField;
import com.dynamicsoft.DsLibs.DsSdpObject.DsSdpMsg;
import com.dynamicsoft.DsLibs.DsSdpObject.DsSdpOriginField;
import com.genband.isup.exceptions.InvalidInputException;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.SccpUserAddress;

/**
 * This class is utility class used by SIP protocol handler to perform utility
 * operations
 *
 */
public class SipProtocolUtil {
	private static Logger logger = Logger.getLogger(SipProtocolUtil.class);

	/*
	 * Parameters to calculate average processing time of initial request
	 * response
	 */
	protected static AtomicLong sInitialRequestCount = new AtomicLong(0);
	protected static AtomicLong sTotalTimeFirstReqResp = new AtomicLong(0);

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

	/*
	 * This method returns the calldata from the reference saved in tcap session
	 */
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

	public static SipApplicationSession getAppSession(String appSessionId, String serviceId) {

		if (logger.isDebugEnabled()) {
			logger.debug(":: Inside getAppSession " + appSessionId);
		}

		return PhUtilityServices.getInstance(serviceId).getSipSessionsUtil()
				.getApplicationSessionById(appSessionId);

	}

	/**
	 * This method iterates through legData ojects in callData and returns the
	 * LegData object for passed sip-session
	 * 
	 * @param sipSession
	 * @param callData
	 * @return
	 */
	public static LegData getLegDataForSipSession(
			SipApplicationSession appSession, SipSession sipSession,
			CallData callData) {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside getLegDataForSipSession()");
		}
		String legId = (String) sipSession.getAttribute(PhConstants.LEG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Sipsession LegId is " + legId);
		}
		if (legId != null) {
			return ((LegData) callData.get(CallDataAttribute
					.valueOf(legId)));
		}
		LegData legData = null;
		LegData tmpLegData = null;
		String sipSessionId = sipSession.getId();
		for (Map.Entry<CallDataAttribute, Object> entry : callData
				.getPersitableData().entrySet()) {
			if (entry.getValue() instanceof LegData) {
				tmpLegData = (LegData) entry.getValue();
				if (sipSessionId.equals(tmpLegData
						.get(LegDataAttributes.P_SESSION_ID))
						|| sipSessionId
						.equals(tmpLegData
								.get(LegDataAttributes.P_IVR_SESSION_ID))) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Leg Data found for "
								+ tmpLegData
								.get(LegDataAttributes.P_CONNECTION_TYPE));
					}
					legData = tmpLegData;
					break;
				}
			}
		}
		return legData;
	}

	/**
	 * This method iterates through legData ojects in callData and returns the
	 * LegData object for passed connection type
	 * 
	 * @param sipSession
	 * @param connectionType
	 * @param callData
	 * @return
	 */
	public static LegData getLegDataForConnectionType(
			SipApplicationSession appSession, ConnectionType connectionType,
			CallData callData) {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside getLegDataForConnectionType()");
		}
		LegData legData = null;
		LegData tmpLegData = null;

		for (Map.Entry<CallDataAttribute, Object> entry : callData
				.getPersitableData().entrySet()) {
			if (entry.getValue() instanceof LegData) {
				tmpLegData = (LegData) entry.getValue();
				if (connectionType == (ConnectionType) tmpLegData
						.get(LegDataAttributes.P_CONNECTION_TYPE)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Leg Data found for "
								+ connectionType);
					}
					legData = tmpLegData;
					break;
				}
			}
		}
		return legData;
	}

	/**
	 * This method finds and returns the legData of peer-leg i.e. if passed
	 * currentLegType is ORIG connection then this method would return LegData
	 * of term leg
	 * 
	 * @param callData
	 * @param currentLegType
	 * @return
	 */
	public static LegData getPeerLegData(CallData callData,
			ConnectionType currentLegType) {
		LegData peerLegData = null;
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside getPeerLegData");
			logger.debug(origLegCallId + ":: Finding peer leg for "
					+ currentLegType);
		}
		switch (currentLegType) {
		case ORIG_CONNECTION: {
			LegData tmpPeerLegData = null;
			for (Map.Entry<CallDataAttribute, Object> entry : callData
					.getPersitableData().entrySet()) {
				if (entry.getValue() instanceof LegData) {
					tmpPeerLegData = (LegData) entry.getValue();
					ConnectionType tmpConnectionType = (ConnectionType) tmpPeerLegData
							.get(LegDataAttributes.P_CONNECTION_TYPE);
					if (tmpConnectionType == ConnectionType.TERM_CONNECTION) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Peer leg found. Peer connection type is "
									+ tmpConnectionType);
						}
						peerLegData = tmpPeerLegData;
						break;
					}
				}
			}
			break;
		}
		case TERM_CONNECTION: {
			peerLegData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
			break;
		}
		default: {
			logger.error(origLegCallId
					+ ":: Incorrect current connection type " + currentLegType
					+ ". Failed to find peer leg.");
			break;
		}
		}
		return peerLegData;
	}

	/**
	 * This utility method is called to stop the session refresh timer.
	 * 
	 * @param appSession
	 *            represents an instance of SipApplicationSession
	 * @param LegData
	 *            represents an instance of LegData
	 * @throws Exception
	 */
	public static void stopSessionRefreshTimer(
			SipApplicationSession appSession, LegData legData) throws Exception {
		CallData callData = getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String timerName = (String) legData
				.get(LegDataAttributes.P_SESSION_REFRESH_TIMER_NAME);
		if (timerName == null || timerName.isEmpty()) {
			return;
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside stopSessionRefreshTimer for " + timerName);
		}
		stopTimer(appSession, timerName);
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
	 * This method stops all the timers those are running in the call.
	 * 
	 * @param appSession
	 *            represents an instance of SipApplicationSession
	 */
	public static void stopAllTimers(SipApplicationSession appSession) {
		CallData callData = getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside stopCallTimers");
		}
		// Stop all Session refreh timers
		Iterator<?> it = appSession.getSessions("SIP");

		Collection<ServletTimer> timers = appSession.getTimers();

		Iterator<ServletTimer> servlettimers = timers.iterator();

		while (servlettimers.hasNext()) {
			ServletTimer timer = servlettimers.next();

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: stopCallTimers Cancel timer "
						+ timer.getId());
			}

			if (timer != null) {

				Object timerInfo = timer.getInfo();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: timerInfo is " + timerInfo);
				}
				if (timerInfo != null && timerInfo instanceof PhTimerInfo) {

					PhTimerInfo phTimerInfo = (PhTimerInfo) timer.getInfo();

					String timerName = phTimerInfo.getTimerName();

					timer.cancel();
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: " + timerName
								+ " stopped");
					}
					return;
				}
			}
		}

		// SipSession sipSession = null;
		// LegData legData = null;
		// while (it.hasNext()) {
		// sipSession = (SipSession) it.next();
		// legData = getLegDataForSipSession(appSession, sipSession, callData);
		//
		// if (legData != null) {
		// Object sessRefTimerName = legData
		// .getPersistableData(LegDataAttributes.P_SESSION_REFRESH_TIMER_NAME);
		// if (sessRefTimerName != null) {
		// stopTimer(appSession, (String) sessRefTimerName);
		// }
		// }
		//
		// }
		//
		// // Stop No-Answer timer
		// stopTimer(appSession, PhConstants.NO_ANSWER_TIMER);
	}

	public static void startSessionRefreshTimerAfterFt(
			SipApplicationSession appSession, CallData callData,
			LegData legData, String origLegCallId) {
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		if (logger.isDebugEnabled()) {
			logger.debug("Inside startSessionRefreshTimerAfterFt");
			logger.debug("Starting Session refresh timer after FT for "
					+ connType);
		}

		long sysCurrentTime = System.currentTimeMillis();
		/*
		 * sessionRefreshTimerDuration is the session refresh timer value that
		 * was nagotiated with a and B parties before FT
		 */
		String sessionRefreshTimerDuration = null;
		/*
		 * sessionRefreshTimerStartTime is the time at which session refresh
		 * timer was started at primary before FT
		 */
		String sessionRefreshTimerStartTime = null;

		// Start orig Session refresh timer
		sessionRefreshTimerDuration = (String) legData
				.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);

		if (sessionRefreshTimerDuration == null
				|| sessionRefreshTimerDuration.isEmpty()) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ " :: Session expire duration information is missing for "
						+ connType);
			}
		} else {
			sessionRefreshTimerStartTime = (String) legData
					.get(LegDataAttributes.P_SESSION_EXPIRE_TIMER_START_TIME);
			long beforeFtSesssionRefreshTimerDuration = 0;
			long beforeFtSesssionRefreshTimerStartTime = 0;
			long sessionRefreshGracePeriod = Long.parseLong(SipProtocolUtil
					.getConfig(SipProtocolConfig.SESSION_REF_GRACE_PERIOD));

			try {
				beforeFtSesssionRefreshTimerDuration = Long
						.parseLong(sessionRefreshTimerDuration);
				beforeFtSesssionRefreshTimerStartTime = sessionRefreshTimerStartTime == null ? sysCurrentTime
						: Long.parseLong(sessionRefreshTimerStartTime);
			} catch (Exception ex) {
				logger.error(origLegCallId
						+ " :: Error in session refresh duration/start time value");
				if (logger.isInfoEnabled()) {
					logger.info(
							origLegCallId
							+ " :: Session refresh duration/start time is not numeric",
							ex);
					logger.info(origLegCallId
							+ " :: Session refresh timer can not be started");
				}
			}

			if (logger.isDebugEnabled()) {
				logger.debug("beforeFtSesssionRefreshTimerDuration= "
						+ beforeFtSesssionRefreshTimerDuration
						+ ", sysCurrentTime=" + sysCurrentTime
						+ ", beforeFtSesssionRefreshTimerStartTime="
						+ beforeFtSesssionRefreshTimerStartTime
						+ ", sSessionRefreshGracePeriod="
						+ sessionRefreshGracePeriod + " Orig connection");
			}

			long sessionExpiryTime = beforeFtSesssionRefreshTimerDuration
					- (sysCurrentTime - beforeFtSesssionRefreshTimerStartTime)
					/ 1000 + (sessionRefreshGracePeriod / 1000);
			/*
			 * If the value of calculated session refresh timer is negative or
			 * zero then re-start session refresh timer for the initially
			 * nagotiated session refresh duration
			 */
			if (sessionExpiryTime <= 0) {
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ " :: Calculated session refresh timer value is negative or 0. So starting session refresh timer for = "
							+ beforeFtSesssionRefreshTimerDuration);
				}
				sessionExpiryTime = beforeFtSesssionRefreshTimerDuration;
			}
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: Final Session Expity Time="
						+ sessionExpiryTime);
			}
			String sessionExpTimeStr = Long.toString(sessionExpiryTime);
			legData.set(LegDataAttributes.P_SESSION_EXPIRE_TIME,
					sessionExpTimeStr);
			try {
				startSessionRefreshTimer(appSession, legData);
			} catch (Exception e) {
				logger.error(origLegCallId
						+ " :: Failed to start session refreh timer for "
						+ connType + " after FT");
			}
		}

	}

	/**
	 * This utility method is called to start the session refresh timer.
	 * 
	 * @param sipMessage
	 *            represents an instance of SipServletMessage
	 * @param timerService
	 *            represents an instance of TimerService
	 * @param timerName
	 *            represents an instance of String
	 * @throws Exception
	 */
	public static void startSessionRefreshTimer(
			SipApplicationSession appSession, LegData legData) throws Exception {
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside startSessionRefreshTimer");
		}
		String timerName = (String) legData
				.get(LegDataAttributes.P_SESSION_REFRESH_TIMER_NAME);

		if (legData
				.get(LegDataAttributes.P_SESSION_REFRESH_TIMER_NAME) == null) {
			timerName = PhConstants.SESSION_REFRESH_TIMER
					+ legData
					.get(LegDataAttributes.P_SESSION_ID);
			legData.set(
					LegDataAttributes.P_SESSION_REFRESH_TIMER_NAME, timerName);
		}
		/*
		 * For orig and term session refresh timers, store the time when session
		 * refresh timer is started. Reason: Session refresh timer is not
		 * replicated on Secondary SAS. So when FT happens, PH on secondary sas
		 * will get sessionDidActivate callback. In this callback, PH will start
		 * two new session refresh timer [orig and term] for the remaining time.
		 */
		String timerStartTime = Long.toString(System.currentTimeMillis());
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Timer started at "
					+ timerStartTime);
		}
		legData.set(
				LegDataAttributes.P_SESSION_EXPIRE_TIMER_START_TIME,
				timerStartTime);

		// First stop the timer if running
		stopSessionRefreshTimer(appSession, legData);

		String timerValue = (String) legData
				.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);

		if (timerValue == null || timerValue.isEmpty()) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Session expire information is missing");
				logger.debug(origLegCallId
						+ ":: Dont start timer as session refresh is not supported");
			}
			return;
		}

		long duration = 0;
		try {
			duration = Long.parseLong(timerValue);
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Error in session refresh duration value");
			if (logger.isInfoEnabled()) {
				logger.error(origLegCallId
						+ ":: Session refresh duration is not numeric", ex);
				logger.info(origLegCallId
						+ ":: Session refresh timer can not be started");
			}
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Session expire duration is "
					+ duration);
		}

		if (duration <= 0) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Dont start timer as session refresh is not supported");
			}
			return;
		}

		startTimer(appSession, duration * 1000, false, timerName);
	}

	/**
	 * This utility method is called to start the Invite pending transaction
	 * timer which is started by mPH if it received 491 response for Re-INVITE.
	 * When this timer times out then mPH resends this INVITE request
	 * 
	 * @param sipResponse
	 * 
	 * @param sipMessage
	 *            represents an instance of SipServletMessage
	 * @param timerService
	 *            represents an instance of TimerService
	 * @param timerName
	 *            represents an instance of String
	 * @throws Exception
	 */
	public static void startInvitePendingTransTimer(
			SipApplicationSession appSession, LegData legData,
			SipServletResponse sipResponse) throws Exception {
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside startInvitePendingTransTimer");
		}
		String timerName = (String) legData
				.get(LegDataAttributes.P_INV_PENDING_TRANS_TIMER_NAME);

		if (legData
				.get(LegDataAttributes.P_INV_PENDING_TRANS_TIMER_NAME) == null) {
			timerName = PhConstants.INV_PNDING_TRANS_TIMER
					+ legData
					.get(LegDataAttributes.P_SESSION_ID);
			legData.set(
					LegDataAttributes.P_INV_PENDING_TRANS_TIMER_NAME, timerName);
		}

		// First stop the timer if running
		stopTimer(appSession, timerName);

		String timerValue = (String) SipProtocolUtil
				.getConfig(SipProtocolConfig.INV_PENDING_TRANS_TIMER_DURATION);

		String retryAfter = sipResponse.getHeader(PhConstants.RETRY_AFTER);
		// e.g. Retry-After: 18000;duration=3600
		// Retry-After: 120 (I'm in a meeting)

		if (retryAfter != null) {
			String[] retryafarr = retryAfter.split(";");

			String[] retryAfValu = retryafarr[0].trim().split(" ");

			timerValue = "" + Integer.parseInt(retryAfValu[0]) * 1000;

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Retry after timer value is "
						+ timerValue);
			}
		}

		long duration = 0;
		try {
			duration = Long.parseLong(timerValue);
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Error in session refresh duration value");
			if (logger.isInfoEnabled()) {
				logger.error(origLegCallId
						+ ":: Session refresh duration is not numeric", ex);
				logger.info(origLegCallId
						+ ":: Session refresh timer can not be started");
			}
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "::Invite pending transaction timer duration is "
					+ duration);
		}

		if (duration <= 0) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Dont start Invite pending transaction timer asits configured duration is "
						+ duration);
			}
			return;
		}

		startTimer(appSession, duration, true, timerName);
	}

	/**
	 * This method is used to again send the 18x response when 100 rel failure is received for second
	 * 18x response to orig when prack for first is not received yet
	 * @param appSession
	 * @param legData
	 * @param sipResponse
	 * @throws Exception
	 */
	public static void start100relFailureRetryTimer(
			SipApplicationSession appSession, LegData legData,
			SipServletResponse sipResponse) throws Exception {
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside start100relFailureRetryTimer");
		}
		String timerName = (String) legData
				.get(LegDataAttributes.P_100_REL_RETRY_TIMER_NAME);

		if (timerName == null) {
			timerName = PhConstants.REL_FAILURE_RETRY_TIMER
					+ legData.get(LegDataAttributes.P_SESSION_ID);
			legData.set(
					LegDataAttributes.P_100_REL_RETRY_TIMER_NAME, timerName);
			legData.set(
					LegDataAttributes.REL_RETRY_PROV_RESPONSE, sipResponse);
		}

		// First stop the timer if running
		stopTimer(appSession, timerName);

		String timerValue = (String) SipProtocolUtil
				.getConfig(SipProtocolConfig.REL_FAILURE_RETRY_TIMER_DURATION);

		long duration = 0;
		try {
			duration = Long.parseLong(timerValue);
		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Error in start100relFailureRetryTimer duration value");
			if (logger.isInfoEnabled()) {
				logger.error(origLegCallId
						+ ":: start100relFailureRetryTimer duration is not numeric", ex);
				logger.info(origLegCallId
						+ ":: start100relFailureRetryTimer can not be started");
			}
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "::start100relFailureRetryTimer  duration is "
					+ duration);
		}

		if (duration <= 0) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Dont start 100relFailureRetryTimer its configured duration is "
						+ duration);
			}
			return;
		}

		startTimer(appSession, duration, true, timerName);
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
	public static void startTimer(SipApplicationSession appSession,
			long timeInMillies, boolean persistable, String timerName) {

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
		phTimerInfo.setProtocol(Protocol.SIP);
		phTimerInfo.setTimerName(timerName);
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
			logger.debug(origLegCallId + ":: Start" + timerName + " timer for "
					+ timeInMillies + " milliseconds");
			logger.debug(origLegCallId + ":: Initial Delay is " + initialDelay
					+ ", delayRecovery flag is " + delayRecovery
					+ " persistable flag is " + persistable);
		}

		PhTimerInfo phTimerInfo = new PhTimerInfo();
		phTimerInfo.setProtocol(Protocol.SIP);
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
	 * This method is called to start the CDR timer for the intermediate CDRs
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 */
	private void startCdrTimer(SipApplicationSession appSession) {

		CallData callData = getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		Date callConnectDateTime = (Date) callData
				.get(CallDataAttribute.P_CALL_CONNECT_TIME);
		long initialDelay = CommonUtils.getInitialDelay(callConnectDateTime);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Start Intermediate CDR Timer");
		}

		// moved before timer creation to replicate updated value
		CommonUtils.setAppSessionTimeout(appSession,
				(int) (initialDelay / 60000) + 5, origLegCallId);

		startTimer(PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTimerService(),
				appSession, initialDelay, 86400000L, false, true,
				PhConstants.CDR_TIMER);
	}

	/**
	 * This method sets the received SDP in session and return true/false based
	 * on whether SDP received or not.
	 * 
	 * @param sipMessage
	 *            represents an instance of SipServletMessage
	 * @param origLegCallId
	 *            represents an instance of String
	 * @return boolean representation of operation status
	 * @throws Exception
	 */
	public static boolean setReceivedSdp(SipServletMessage sipMessage,
			CallData callData) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		// Store received SDP for future reference
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside setReceivedSdp");
		}
		
		boolean isinvite=false;
		if(sipMessage!=null && sipMessage instanceof SipServletRequest){
			SipServletRequest req= (SipServletRequest)sipMessage;
			isinvite=req.getMethod().equals(PhConstants.INVITE_REQUEST);
			
		}
		LegData legData = getLegDataForSipSession(
				sipMessage.getApplicationSession(), sipMessage.getSession(),
				callData);
		MultipartBody sdpContent = getSdpContent(sipMessage, origLegCallId);

		Object lastReceivedSdp = legData
				.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

		if (lastReceivedSdp != null) {
			legData.set(
					LegDataAttributes.P_LAST_RECEIVED_SDP, lastReceivedSdp);
		}
		/*
		 * moving it in content null chk because of level3 SF-124013 issue
		 */
		//	legData.set(
		//		LegDataAttributes.P_CURRENT_RECEIVED_SDP, sdpContent);

		if (sdpContent != null && sdpContent.getContent() != null) {

			legData.set(
					LegDataAttributes.P_CURRENT_RECEIVED_SDP, sdpContent);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: SDP received, for Call Id = "
						+ sipMessage.getCallId()
						+ ". Store it for future reference. Content="
						+ new String(sdpContent.getContent()) + " \nLength="
						+ new String(sdpContent.getContent()).length());
			}
			return true;
		}else if(isinvite){ //if last received was null then set it null else let current rcived sdp same as last
			legData.set(
					LegDataAttributes.P_CURRENT_RECEIVED_SDP, null);
		}
		return false;
	}

	/**
	 * This method checks if sdp exists in incoming msg or not
	 * @param sipMessage
	 * @param origLegCallId
	 * @return
	 * @throws Exception
	 */
	protected static  boolean isSDPExists(SipServletMessage sipMessage,
			String origLegCallId) throws Exception {
		boolean sdpExists = true;
 
		if (getSdpContent(sipMessage, origLegCallId) == null) {
			sdpExists = false;
		}
		return sdpExists;
	}
	// /**
	// * This method returns t
	// /**
	// * This method returns the SDP content form the message if present
	// *
	// * @param msg
	// * represents an instance of SipServletMessage
	// * @param callId
	// * represents an instance of String
	// * @return an instance of MultipartBody
	// * @throws Exception
	// */
	// public static MultipartBody getSdpContent(SipServletMessage msg, String
	// callId)
	// throws Exception {
	// if (logger.isDebugEnabled()) {
	// logger.debug(callId + ":: Inside getSdpContent");
	// }
	//
	// MultipartBody sdpMultipartBody = null;
	//
	// if (msg.getContentType() != null
	// && msg.getContentType().startsWith(PhConstants.APPLICATION_SDP)) {
	// if (logger.isDebugEnabled()) {
	// logger.debug(callId + ":: Retrieving message sdp content");
	// }
	// String contentDisposition =
	// msg.getHeader(PhConstants.CONTENT_DISPOSITION_HDR) == null ? null : msg
	// .getHeader(PhConstants.CONTENT_DISPOSITION_HDR);
	// sdpMultipartBody = new MultipartBody((byte[]) msg.getContent(),
	// msg.getContentType());
	// sdpMultipartBody.setContentDisposition(contentDisposition);
	// } else {
	// if (logger.isDebugEnabled()) {
	// logger
	// .debug(callId +
	// ":: Ignore as message content type is invalid or not present");
	// }
	// }
	//
	// if (sdpMultipartBody == null) {
	// if (logger.isInfoEnabled()) {
	// logger.info(callId + ":: No SDP content found");
	// }
	// }
	//
	// return sdpMultipartBody;
	// }
	//
	/*
	 * This method returns the SDP content form the message if present
	 */
	/**
	 * This method returns the SDP content form the message if present
	 * 
	 * @param msg
	 *            represents an instance of SipServletMessage
	 * @param callId
	 *            represents an instance of String
	 * @return an instance of MultipartBody
	 * @throws Exception
	 */
	public static MultipartBody getSdpContent(SipServletMessage msg,
			String callId) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Inside getSdpContent");
		}

		javax.mail.BodyPart bodyPart = null;
		MultipartBody sdpMultipartBody = null;

		if (msg.getContentType() != null
				&& msg.getContentType().startsWith(PhConstants.MULTIPART_MIXED)) {
			if (logger.isDebugEnabled()) {
				logger.debug(callId + ":: Retrieve Content from mime multipart");
			}
			MimeMultipart mimeMultipart = (MimeMultipart) msg.getContent();
			for (int indx = 0; indx < mimeMultipart.getCount(); indx++) {
				bodyPart = mimeMultipart.getBodyPart(indx);
				if (logger.isDebugEnabled()) {
					logger.debug(callId + ":: Body part content type is "
							+ bodyPart.getContentType());
				}
				if (bodyPart.getContentType().startsWith(
						PhConstants.APPLICATION_SDP)) {
					String contentDisposition = bodyPart
							.getHeader(PhConstants.CONTENT_DISPOSITION_HDR) == null ? null
									: bodyPart
									.getHeader(PhConstants.CONTENT_DISPOSITION_HDR)[0];
					sdpMultipartBody = new MultipartBody(
							getByteArrayFromStream(
									(ByteArrayInputStream) bodyPart
									.getContent(),
									callId), bodyPart.getContentType());
					sdpMultipartBody.setContentDisposition(contentDisposition);
					break;
				}
			}
		} else if (msg.getContentType() != null
				&& msg.getContentType().startsWith(PhConstants.APPLICATION_SDP)) {
			if (logger.isDebugEnabled()) {
				logger.debug(callId + ":: Retrieving message sdp content");
			}
			String contentDisposition = msg
					.getHeader(PhConstants.CONTENT_DISPOSITION_HDR) == null ? null
							: msg.getHeader(PhConstants.CONTENT_DISPOSITION_HDR);
			sdpMultipartBody = new MultipartBody((byte[]) msg.getContent(),
					msg.getContentType());
			sdpMultipartBody.setContentDisposition(contentDisposition);
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(callId
						+ ":: Ignore as message content type is invalid or not present");
			}
		}

		if (sdpMultipartBody == null) {
			if (logger.isInfoEnabled()) {
				logger.info(callId + ":: No SDP content found");
			}
		}

		return sdpMultipartBody;
	}

	/**
	 * get ISUP content from orig request
	 * @param origLegCallId
	 * @param sipRequest
	 * @param callData
	 * @throws MessagingException
	 * @throws IOException
	 * @throws InvalidInputException
	 */
	public static byte[] getIsupContent(String origLegCallId,
			SipServletMessage sipMessage, CallData callData)
					throws MessagingException, IOException, InvalidInputException {

		MimeMultipart mimeMultipart = (MimeMultipart) sipMessage.getContent();

		for (int indx = 0; indx < mimeMultipart.getCount(); indx++) {
			BodyPart bodyPart = mimeMultipart.getBodyPart(indx);

			if (bodyPart.getContentType().startsWith(
					PhConstants.APPLICATION_ISUP)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Body part content type is ISUP");
				}
				String isupContentDisposition = bodyPart
						.getHeader(PhConstants.CONTENT_DISPOSITION_HDR) == null ? PhConstants.CONTENT_DISPOSITION_VALUE_FOR_ISUP
								: bodyPart
								.getHeader(PhConstants.CONTENT_DISPOSITION_HDR)[0];
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: parsed isup content from siprequest");
				}
				ByteArrayInputStream byteArrInpStream = (ByteArrayInputStream) bodyPart
						.getContent();
				int byteArrLen = byteArrInpStream.available();
				byte[] byteArr = new byte[byteArrLen];
				byteArrInpStream.read(byteArr, 0, byteArrLen);
				return byteArr;
			}
		}

		return null;
	}
	/*
	 * This method returns content from ByteArrayInputStream.
	 */
	/**
	 * This method returns content from ByteArrayInputStream.
	 * 
	 * @param byteArrInpStream
	 *            represents an instance of ByteArrayInputStream
	 * @param callId
	 *            represents an instance of String
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] getByteArrayFromStream(
			ByteArrayInputStream byteArrInpStream, String callId)
					throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Inside getByteArrayFromStream");
		}
		byte[] byteArr = null;
		int byteArrLen = byteArrInpStream.available();
		byteArr = new byte[byteArrLen];
		byteArrInpStream.read(byteArr, 0, byteArrLen);
		return byteArr;
	}

	/**
	 * This method compares the two SDP content
	 * 
	 * @param oldSdp
	 *            represents the instance of MultipartBody
	 * @param newSdp
	 *            represents the instance of MultipartBody
	 * @param origLegCallId
	 *            represents the instance of String
	 * @return boolean representation of operation status
	 * @throws Exception
	 */
	public static boolean isSdpChanged(MultipartBody oldSdp,
			MultipartBody newSdp, String origLegCallId) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside isSdpChanged");
		}
		String newSdpContentString = null;
		String oldSdpContentString = null;

		if (oldSdp != null) {
			oldSdpContentString = (new String(oldSdp.getContent())).trim();
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: isSdpChanged oldSdp "
					+ oldSdpContentString);
		}
		if (newSdp != null) {
			newSdpContentString = (new String(newSdp.getContent())).trim();
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: isSdpChanged newSdp "
					+ newSdpContentString);
		}

		if (newSdpContentString == null && oldSdpContentString == null) {
			return false;
		} else {
			if (newSdpContentString == null || oldSdpContentString == null) {
				return true;
			} else {
				/*
				 * Return false if oldSdpContentString = newSdpContentString.
				 * Coz SDP has not changed
				 */
				return (!isSdpMediaAndConnectionEquals(oldSdpContentString,newSdpContentString) );//oldSdpContentString.equals(newSdpContentString));
			}
		}
	}
	
	
	  public static void main(String[] args){
		  
		  
		  String oldSDP="v=+\n"
                         +"o=SST 0 1 IN IP4 172.25.140.11\n"
                         + "s=-\n"
                         +"c=IN IP4 172.25.140.11\n"
                         +"t=0 0\n"
                         +"m=audio 32640 RTP/AVP 0 8 18 101\n"
                         +"a=ptime:20\n"
                         +"a=fmtp:18 annexb=no\n"
                         +"a=rtpmap:101 telephone-event/8000\n"
                         +"a=fmtp:101 0-15\n"
                         +"a=sendrecv";
		  String newSDP="v=+\n"
                  +"o=SST 0 0 IN IP4 172.25.140.11\n"
                  + "s=-\n"
                  +"c=IN IP4 172.25.140.11\n"
                  +"t=0 0\n"
                  +"m=audio 32640 RTP/AVP 0 8 18 108\n"
                  +"a=ptime:20\n"
                  +"a=fmtp:18 annexb=no\n"
                  +"a=rtpmap:101 telephone-event/8000\n"
                  +"a=fmtp:101 0-15\n"
                  +"a=sendrecv";
		  
		  
//		    String oldSDP="v=0\n"
//		    				+"o=- 11946869 11946869 IN IP4 172.25.167.44\n"
//		    				+"s=media server session\n"
//		    				+"b=AS:80\n"
//		    				+"t=0 0\n"
//		    				+"c=IN IP4 172.25.167.44\n"
//		    				+"m=audio 9686 RTP/AVP 0\n"
//		    				+"b=AS:80\n"
//		    				+"b=RR:0\n"
//		    				+"b=RS:0\n"
//		    				+"a=rtpmap:0 PCMU/8000\n"
//		    				+"a=ptime:20\n"
//		    				+"a=maxptime:40\n"
//		    				+"a=sendrecv";
//
//			String newSDP="v=0\n"
//			              +"o=- 547764004 690525911 IN IP4 172.19.98.56\n"
//			              +"s=-\n"
//			              +"c=IN IP4 172.19.89.188\n"
//			              +"t=0 0\n"
//			              +"m=audio 1500 RTP/AVP 0 101\n"
//			              +"b=AS:80\n"
//			              +"b=RS:0\n"
//			              +"b=RR:0\n"
//			              +"a=rtpmap:0 PCMU/8000\n"
//			              +"a=rtpmap:101 telephone-event/8000\n"
//			              +"a=ptime:20\n"
//			              +"a=maxptime:20";
					
		  try {
			boolean samesdp=isSdpMediaAndConnectionEquals(oldSDP, newSDP);
			System.out.println(" is same sdp "+samesdp);
		} catch (ProcessMessageException e) {
			System.out.println(" xception wile comparing sdp");
			e.printStackTrace();
		}
	  }
	
	/**
	 * This method compares Media and connection fields in a sdp to eclare it same or diffrent sdp
	 * not just the simple string compare 
	 * @param oldSDP
	 * @param newSDP
	 * @return
	 * @throws ProcessMessageException
	 */
	private static final boolean isSdpMediaAndConnectionEquals(String oldSDP,
			String newSDP) throws ProcessMessageException {

		if (logger.isDebugEnabled())
			logger.debug(" Entering isSdpMediaAndConnectionEquals-->");

		try {

			/*
			 * get SDP from Servlet Request
			 */
			DsSdpMsg dsOldSdpMsg = new DsSdpMsg((String) oldSDP);
			DsSdpMsg dsNewSdpMsg = new DsSdpMsg((String) newSDP);
			;

			/*
			 * check for 0.0.0.0 in c line and media descriptions
			 */
			DsSdpConnectionField oldConnField = dsOldSdpMsg
					.getConnectionField();
			DsSdpConnectionField newConnField = dsNewSdpMsg
					.getConnectionField();
		
			boolean connFieldSame = false;
			boolean mediFieldSame = false;
			if (oldConnField != null && newConnField != null) {
				
				if (logger.isDebugEnabled())
					logger.debug("<SBB> Connection field in SDP message is "
							+ oldConnField.getAddr() + "  new Connection field "
							+ newConnField.getAddr());
				
				if (oldConnField.getAddr().equals(newConnField.getAddr())
						&& oldConnField.getAddrType().equals(
								newConnField.getAddrType())
						&& oldConnField.getNetType().equals(
								newConnField.getNetType())
						&& oldConnField.getType() == newConnField.getType()) {
					
					//System.out.println(" connection filed is true");
					if (logger.isDebugEnabled()) {
						logger.debug(":: Connection Field is same");
					}
					connFieldSame = true;
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(":: Connection Field is different");
					}
					connFieldSame = false;
				}

			}

			DsSdpMediaDescription[] oldmediaFields = dsOldSdpMsg
					.getMediaDescriptionList();
			DsSdpMediaDescription[] newmediaFields = dsNewSdpMsg
					.getMediaDescriptionList();
			for (int i = 0; oldmediaFields != null && i < oldmediaFields.length; i++) {

				DsSdpMediaField oldmediaField = null;
				DsSdpMediaDescription oldMediaDescription = null;
				if (oldmediaFields[i] != null) {
					oldMediaDescription = oldmediaFields[i];
					oldmediaField = oldmediaFields[i].getMediaField();
				}

				for (int j = 0; newmediaFields != null
						&& j < newmediaFields.length; j++) {

					DsSdpMediaField newmediaField = null;
					DsSdpMediaDescription newMediaDescription = null;
					if (newmediaFields[j] != null) {
						newMediaDescription = newmediaFields[j];
						newmediaField = newmediaFields[j].getMediaField();
					}

					if (oldmediaField.getMediaType().equals(
							newmediaField.getMediaType())
							&& (oldmediaField.getPort() == newmediaField
									.getPort())
							&& oldmediaField.getTransport().equals(
									newmediaField.getTransport())
							&& oldmediaField.getNumberOfPorts() == newmediaField
									.getNumberOfPorts()
							&& oldmediaField.getType() == newmediaField
									.getType()) {

						String[] oldMediaFormats = oldmediaField
								.getMediaFormats();
						String[] newMStrings = newmediaField.getMediaFormats();

						mediFieldSame = Arrays.equals(oldMediaFormats,newMStrings);
						//System.out.println(" media field is true");
						if (logger.isDebugEnabled()) {
							logger.debug(":: Media Field is Same");
						}

					} else {
						//System.out.println(" media field is false");
						if (logger.isDebugEnabled()) {
							logger.debug(":: Media Field is Different");
						}
						mediFieldSame = false;
						break;
					}

					DsSdpAttributeField[] oldattributeFields = oldMediaDescription
							.getAttributeFields();
					DsSdpAttributeField[] newattributeFields = newMediaDescription
							.getAttributeFields();

					for (int k = 0; oldattributeFields != null
							&& k < oldattributeFields.length; k++) {

						if (oldattributeFields[k] != null
								&& newattributeFields[k] != null) {

							DsSdpAttribute oldAttr=oldattributeFields[k].getValue();
							DsSdpAttribute newAttr =newattributeFields[k].getValue();
							
							if (oldattributeFields[k].getAttribute().equals(
									newattributeFields[k].getAttribute())) {

								if (oldAttr != null && newAttr != null) {

									if (oldAttr.getData().equals(
											newAttr.getData())) {

										// System.out.println(" media attributes same");
										// if (logger.isDebugEnabled()) {
										// logger.debug(":: Media Attribute is Same");
										// }
										mediFieldSame = true;
									}else{
										mediFieldSame=false;
									}
								}
								mediFieldSame = true;
							} else {

								// System.out.println(" media attributes diff");
								if (logger.isDebugEnabled()) {
									logger.debug(":: Media Attribute is Different");
								}
								mediFieldSame = false;
								break;
							}
							
							
						}

					}
					
				}

			}
			
			if(mediFieldSame&&connFieldSame){
				if (logger.isDebugEnabled()) {
					logger.debug(":: Leaving isSdpMediaAndConnectionEquals with value true--> SDP is same");
				}
				return true;
			}

		} catch (Exception e) {
			throw new ProcessMessageException(e.getMessage(), e);
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(":: Leaving isSdpMediaAndConnectionEquals  with value false-->SDP is not same");
		}
		return false;
	}

	  
	  
//	  private static final boolean isSdpMediaAndConnectionEquals(String oldSDP,
//				String newSDP) throws ProcessMessageException {
//
//			if (logger.isDebugEnabled())
//				logger.debug(" Entering isSdpMediaAndConnectionEquals-->");
//
//			try {
//
//				/*
//				 * get SDP from Servlet Request
//				 */
//				DsSdpMsg dsOldSdpMsg = new DsSdpMsg((String) oldSDP);
//				DsSdpMsg dsNewSdpMsg = new DsSdpMsg((String) newSDP);
//				;
//
//				/*
//				 * check for 0.0.0.0 in c line and media descriptions
//				 */
//				DsSdpConnectionField oldConnField = dsOldSdpMsg
//						.getConnectionField();
//				DsSdpConnectionField newConnField = dsNewSdpMsg
//						.getConnectionField();
//				if (logger.isDebugEnabled())
//					logger.debug("<SBB> Connection field in SDP message is "
//							+ oldConnField.getAddr() + "  new Connection field "
//							+ newConnField.getAddr());
//				boolean connFieldSame = false;
//				boolean mediFieldSame = false;
//				if (oldConnField != null && newConnField != null) {
//					if (oldConnField.getAddr().equals(newConnField.getAddr())
//							&& oldConnField.getAddrType().equals(
//									newConnField.getAddrType())
//							&& oldConnField.getNetType().equals(
//									newConnField.getNetType())
//							&& oldConnField.getType() == newConnField.getType()) {
//						
//						//System.out.println(" connection filed is true");
//						if (logger.isDebugEnabled()) {
//							logger.debug(":: Connection Field is same");
//						}
//						connFieldSame = true;
//					} else {
//						if (logger.isDebugEnabled()) {
//							logger.debug(":: Connection Field is different");
//						}
//						connFieldSame = false;
//					}
//
//				}
//
//				DsSdpMediaDescription[] oldmediaFields = dsOldSdpMsg
//						.getMediaDescriptionList();
//				DsSdpMediaDescription[] newmediaFields = dsNewSdpMsg
//						.getMediaDescriptionList();
//				for (int i = 0; oldmediaFields != null && i < oldmediaFields.length; i++) {
//
//					DsSdpMediaField oldmediaField = null;
//					DsSdpMediaDescription oldMediaDescription = null;
//					if (oldmediaFields[i] != null) {
//						oldMediaDescription = oldmediaFields[i];
//						oldmediaField = oldmediaFields[i].getMediaField();
//					}
//
//					for (int j = 0; newmediaFields != null
//							&& j < newmediaFields.length; j++) {
//
//						DsSdpMediaField newmediaField = null;
//						DsSdpMediaDescription newMediaDescription = null;
//						if (newmediaFields[j] != null) {
//							newMediaDescription = newmediaFields[j];
//							newmediaField = newmediaFields[j].getMediaField();
//						}
//
//						if (oldmediaField.getMediaType().equals(
//								newmediaField.getMediaType())
//								&& (oldmediaField.getPort() == newmediaField
//										.getPort())) {
//							mediFieldSame=true;
//
////							String[] oldMediaFormats = oldmediaField
////									.getMediaFormats();
////							String[] newMStrings = newmediaField.getMediaFormats();
////
////							mediFieldSame = Arrays.equals(oldMediaFormats,newMStrings);
//							//System.out.println(" media field is true");
//							if (logger.isDebugEnabled()) {
//								logger.debug(":: Media Field is Same");
//							}
//
//						} else {
//							//System.out.println(" media field is false");
//							if (logger.isDebugEnabled()) {
//								logger.debug(":: Media Field is Different");
//							}
//							mediFieldSame = false;
//							break;
//						}
//
////						DsSdpAttributeField[] oldattributeFields = oldMediaDescription
////								.getAttributeFields();
////						DsSdpAttributeField[] newattributeFields = newMediaDescription
////								.getAttributeFields();
//
////						for (int k = 0; oldattributeFields != null
////								&& k < oldattributeFields.length; k++) {
////
////							if (oldattributeFields[k] != null
////									&& newattributeFields[k] != null) {
////
////								DsSdpAttribute oldAttr=oldattributeFields[k].getValue();
////								DsSdpAttribute newAttr =newattributeFields[k].getValue();
////								
////								if (oldattributeFields[k].getAttribute().equals(
////										newattributeFields[k].getAttribute())) {
////
////									if (oldAttr != null && newAttr != null) {
////
////										if (oldAttr.getData().equals(
////												newAttr.getData())) {
////
////											// System.out.println(" media attributes same");
////											// if (logger.isDebugEnabled()) {
////											// logger.debug(":: Media Attribute is Same");
////											// }
////											mediFieldSame = true;
////										}else{
////											mediFieldSame=false;
////										}
////									}
////									mediFieldSame = true;
////								} else {
////
////									// System.out.println(" media attributes diff");
////									if (logger.isDebugEnabled()) {
////										logger.debug(":: Media Attribute is Different");
////									}
////									mediFieldSame = false;
////									break;
////								}
////								
////								
////							}
////
////						}
//						
//					}
//
//				}
//				
//				if(mediFieldSame&&connFieldSame){
//					if (logger.isDebugEnabled()) {
//						logger.debug(":: Leaving isSdpMediaAndConnectionEquals with value true--> SDP is same");
//					}
//					return true;
//				}
//
//			} catch (Exception e) {
//				throw new ProcessMessageException(e.getMessage(), e);
//			}
//			
//			if (logger.isDebugEnabled()) {
//				logger.debug(":: Leaving isSdpMediaAndConnectionEquals  with value false-->SDP is not same");
//			}
//			return false;
//		}
	/**
	 * This method parses the initial invite to extract the necessary call
	 * processing parameters. It creates a CallData object from the parameters
	 * and sets it in the session.
	 * 
	 * @param inviteRequest
	 *            represents an instance of SipServletRequest
	 * @throws UnsupportedEncodingException
	 * @throws IOException
	 * @throws MessagingException
	 * @throws ServletParseException
	 * @throws InvalidInputException
	 */
	public static void parseInvite(CallData callData, LegData legData,
			SipServletRequest inviteRequest)
					throws UnsupportedEncodingException, IOException,
					MessagingException, ServletParseException, InvalidInputException {

		String origLegCallId = inviteRequest.getCallId();

		InviteAttributes inviteAttributes=new InviteAttributes();

		inviteAttributes.setFrom(inviteRequest.getFrom());
		inviteAttributes.setTo(inviteRequest.getTo());
		inviteAttributes.setRequestURI(inviteRequest.getRequestURI());

		legData.set(LegDataAttributes.INVITE_ATTRIBUTES, inviteAttributes);

		legData.set(LegDataAttributes.APP_SESSION_ID,
				inviteRequest.getApplicationSession().getId());

		callData.set(CallDataAttribute.APP_SESSION_ID,
				inviteRequest.getApplicationSession().getId());

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside parseInvite");
		}


		if (SignalingTypeEnum.SIGNALING_TYPE.SIP_T.equals(legData
				.get(LegDataAttributes.P_SIGNALING_TYPE_KEY))) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Signaling type is SIPT");
			}

			String readIsupParams= SipProtocolConfig.getConfigData(SipProtocolConfig.READ_PARAM_FROM_ISUP_CONTENT);

			if (readIsupParams != null
					&& readIsupParams.equals(PhConstants.TRUE)) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Read isup params from isup content received");
				}
				byte[] isupcontent = SipProtocolUtil.getIsupContent(
						origLegCallId, inviteRequest, callData);
				SipIsupParser.parseIam(callData, isupcontent, true);
			}
		}

		/*
		 * If CPC is present in PAI, then use that CPC,
		 * Also check for calling Isub. If present in PAI then override
		 */
		String fromUser = "";
		String fromHdrCpc = null;
		String fromClgIsub = null;
		boolean fieldPresent = false;

		if (inviteRequest.getFrom() != null) {
			URI fromUri = inviteRequest.getFrom().getURI();
			if (fromUri.isSipURI()) {
				String fromuser = ((SipURI) fromUri).getUser();
				if(null != fromuser && !fromuser.isEmpty()) {
					for (String userpart : fromuser.split(";")) {
						fieldPresent = userpart.startsWith("cpc=");
						if (fieldPresent) {
							fromHdrCpc = userpart.substring("cpc=".length());
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: CPC  parameter from  from header user part is "
										+ fromHdrCpc);
							}
							//break;
						}
						
						// Calling Isub
						fieldPresent = userpart.startsWith("isub=");
						if (fieldPresent) {
							fromClgIsub = userpart.substring("isub=".length());
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: Calling Isub  parameter from  from header user part is "
										+ fromClgIsub);
							}
						}
					}
				}
			}

			callData.set(CallDataAttribute.FROM_URI, fromUri.toString());
			
			// Set Calling ISUB in leg data
			if(fromClgIsub != null){
				legData.set(LegDataAttributes.P_CALLING_ISUB, fromClgIsub);
				
				if(logger.isDebugEnabled()){
					logger.debug(origLegCallId + ":: Calling Isub from FROM Header setting in leg data:" +
							fromClgIsub);
				}
			}

			/*
			 * SBTM-UAT-287: If CPC is present in PAI, then use that CPC,
			 * otherwise use cpc from FROM header
			 */
			if (fromHdrCpc == null) {
				fromHdrCpc = fromUri
						.getParameter(PhConstants.CPC_PARAM);
			}

			if(fromHdrCpc==null){
				fromHdrCpc = inviteRequest.getFrom().getParameter(PhConstants.CPC_PARAM);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: FROM header Calling Party Category is "
						+ fromHdrCpc);
			}
			if (fromUri.isSipURI()) {
				String tmp = ((SipURI) fromUri).getUser();
				tmp = (tmp == null) ? "" : tmp;
				fromUser = tmp.split(";")[0];

				if (tmp != null && tmp.startsWith("+")
						&& !fromUser.startsWith("+")) {
					fromUser = "+" + fromUser;
				}
			} else {
				String tmp = ((TelURL) fromUri).getPhoneNumber();
				tmp = (tmp == null) ? "" : tmp;
				fromUser = tmp.split(";")[0];

				if (tmp != null && tmp.startsWith("+")
						&& !fromUser.startsWith("+")) {
					fromUser = "+" + fromUser;
				}
			}

			/**
			 * Adding below code for extracting originating trunk group
			 * parameter from sip from uri
			 */
			String origTrunkGroup = fromUri.getParameter(PhConstants.OTG_PARAM);

			if (origTrunkGroup != null) {
				callData.set(
						CallDataAttribute.P_ORIG_TRUNK_GROUP, origTrunkGroup);
			}

			/**
			 * Adding below code for extracting originating trunk group
			 * parameter from sip from uri
			 */
			String isupOli = fromUri.getParameter(PhConstants.ISUP_OLI_PARAM);

			if (PhConstants.FALSE.equals(SipProtocolConfig
					.getConfigData(SipProtocolConfig.SET_OLI_AS_CPC))) {
				if (isupOli != null) {
					callData.set(CallDataAttribute.P_ORIG_LINE_INFO, isupOli);
				}
			}

			/**
			 * Adding below code for extracting JIP parameter as jip/rn
			 * parameter in fromuri
			 */

			String jipParam = fromUri.getParameter(PhConstants.JIP_PARAM);
			String rnParam = fromUri.getParameter(PhConstants.RN_PARAM);

			if (jipParam != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: JIP parameter from from header jip param is "
							+ jipParam);
				}
				legData.set(LegDataAttributes.P_JIP, jipParam);
			} else if (rnParam != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: RN parameter from from header rn param is "
							+ rnParam);
				}
				legData.set(LegDataAttributes.P_JIP, rnParam);
			}

			/**
			 * Adding below code for extracting CIC parameter as cic parameter
			 * in request uri
			 */

			String user="";
			if(inviteRequest.getRequestURI().isSipURI()){
				
				 user = ((SipURI) inviteRequest.getRequestURI()).getUser();
			}else{
				user = ((TelURL) inviteRequest.getRequestURI()).getPhoneNumber();
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: test..Request URI.. " +user);

				}
				if(user == null){
					user = "";
				}
			}
			
			
			String cic = null;
			String rn = null;
			String npdi = null;
			String cldIsub = null;

			String[] userparts = user.split(";");

			/*
			 * e.g. String user = "8661120002;cic=1234;npdi=yes;rn=8661120003";
			 */
			for (String userpart : userparts) {

				/*
				 * parse CIC parameter
				 */
				int cicIndex = userpart.indexOf("cic=");

				if (cicIndex != -1) {
					cic = userpart.substring(cicIndex + 4);
				}

				if (cic != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: CIC  parameter from request uri is "
								+ cic);
					}
					legData.set(LegDataAttributes.P_CIC, cic);
				}

				/*
				 * parse RN parameter
				 */
				int rnIndex = userpart.indexOf("rn=");

				if (rnIndex != -1) {
					rn = userpart.substring(rnIndex + 3);
				}

				if (rn != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: RN  parameter from request uri is " + rn);
					}
					legData.set(LegDataAttributes.P_RN, rn);
				}

				/*
				 * parse NPDI parameter
				 */

				int npdiIndex = userpart.indexOf("npdi=");

				if (npdiIndex != -1) {
					npdi = userpart.substring(npdiIndex + 5);
				}

				if (npdi != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: NPDI  parameter from request uri is "
								+ npdi);
					}
					legData.set(LegDataAttributes.P_NPDI, npdi);
				}

				// Called party Sub-address
				// Format: INVITE sip:4178938888;isub=1234@10.7.6.40:6555 SIP/2.0.
				int cldIsubIndex = userpart.indexOf("isub=");

				if (cldIsubIndex != -1) {
					cldIsub = userpart.substring(cldIsubIndex + 5);
				}

				if (cldIsub != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Called ISUB  parameter from request uri is "
								+ cldIsub);
					}
					legData.set(LegDataAttributes.P_CALLED_ISUB, cldIsub);
				}
			}
			/**
			 * Adding below code for extracting NOA parameter
			 */
			String noa = fromUri.getParameter(PhConstants.NOA_PARAM);
			if (noa != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: NOA  parameter from from header is " + noa);
				}
				legData.set(LegDataAttributes.P_NOA, noa);
			}

		}
		// }
		PhoneNumber callingNumberFromHeader = new PhoneNumber();
		callingNumberFromHeader.setAddress(fromUser);
		callingNumberFromHeader.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
		callingNumberFromHeader
		.setNatureOfAddress(fromUser != null && !fromUser.isEmpty()
		&& fromUser.startsWith("+") ? PhoneNumber.NOA_INTERNATIONAL
				: PhoneNumber.NOA_NATIONAL);
		legData.set(
				LegDataAttributes.P_CALLING_PARTY_FROM_HEADER,
				callingNumberFromHeader);
		legData.set(
				LegDataAttributes.P_CALLING_PARTY,callingNumberFromHeader);

		String isupcpc=(String)callData.get(CallDataAttribute.P_ISUP_CPC);

		String readIsupParams= SipProtocolConfig.getConfigData(SipProtocolConfig.READ_PARAM_FROM_ISUP_CONTENT);

		if (PhConstants.TRUE.equals(readIsupParams) && isupcpc != null) {
			legData.set(LegDataAttributes.P_CPC, isupcpc);
		} else if (fromHdrCpc != null) {

			String fromcpc = getCpcFromString(fromHdrCpc, legData);

			legData.set(LegDataAttributes.P_CPC_FROM_HEADER, fromcpc);
			legData.set(LegDataAttributes.P_CPC, fromcpc);
		}

		// Privacy Asserted Identity Header - Calling Party Number
		String paiUser = null;
		String paiCpc = null;
		String cic = null;
		String noa = null;
		String jipParam = null;
		String paiIsub = null;
		PhoneNumber paiNumber = null;
		String paiTelCPC = null;
		
		try {
			ListIterator<Address> paiAddressIter = inviteRequest
					.getAddressHeaders(PhConstants.PAI_HEADER);
			if (paiAddressIter != null) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: paiAddressIter "
							+ paiAddressIter);
				}

				Address paiAddress = null;
				boolean isTel=false;
				
				while (paiAddressIter.hasNext()) {

					paiAddress = paiAddressIter.next();
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: paiAddress "
								+ paiAddress);
					}

					if (paiAddress != null && paiAddress.getURI() != null) {

						URI paiUri = paiAddress.getURI();
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + ":: paiUri is " + paiUri);
						}
						/*
						 * SBTM-UAT-287: If CPC is present in PAI, then use that
						 * CPC, otherwise use cpc from FROM header
						 */

						String cpcPolicy = SipProtocolConfig
								.getConfigData(SipProtocolConfig.CPC_PARSING_POLICY);

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + ":: CPC_PARSING_POLICY is " + cpcPolicy);
						}

						String[] userparts = null;

						if(paiUri != null){
							if(paiUri.isSipURI()){
								String paiuser = ((SipURI) paiUri).getUser();
								userparts = paiuser.split(";");
							}
						}

						if ((isupcpc==null || PhConstants.FALSE.equals(readIsupParams))  && 
								(PhConstants.DEFAULT.equals(cpcPolicy) || (PhConstants.CPC_POLICY_FROM_FIRST
										.equals(cpcPolicy) && fromHdrCpc == null))) {

							if (userparts != null) {

								for (String userpart : userparts) {
									/*
									 * parse CPC parameter
									 */
									boolean cpcPresent = userpart.startsWith("cpc=");

									if (cpcPresent) {
										paiCpc = userpart.substring("cpc=".length());
										if (logger.isDebugEnabled()) {

											logger.debug(origLegCallId
													+ ":: CPC  parameter from  PAI header user part is "
													+ paiCpc);
										}
										break;
									}
								}
							}


							if (paiCpc == null) {
								paiCpc = paiUri
										.getParameter(PhConstants.CPC_PARAM);
							}

							if(paiCpc==null){
								paiCpc = paiAddress.getParameter(PhConstants.CPC_PARAM);
							}

							if (paiCpc != null) {

								String cpc = getCpcFromString(paiCpc, legData);
								legData.set(
										LegDataAttributes.P_CPC_PAI_HEADER, cpc);
								legData.set(LegDataAttributes.P_CPC, cpc);
							}

							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: PAI Calling Party Category is "
										+ paiCpc);
							}
						}

						// Extract Calling Subaddress ISUB
						// Format: P-Asserted-Identity: sip:4178938888;isub=1234@10.7.6.40:6555 SIP/2.0.
						if (userparts != null) {

							for (String userpart : userparts) {

								boolean cldIsubPresent = userpart.startsWith("isub=");

								if (cldIsubPresent) {
									paiIsub = userpart.substring("isub=".length());
									if (logger.isDebugEnabled()) {

										logger.debug(origLegCallId
												+ ":: Called ISUB  parameter from  PAI header user part is "
												+ paiIsub);
									}
									// set the vlaue in leg data
									legData.set(LegDataAttributes.P_CALLING_ISUB, paiIsub);
								}
							}
						}


						/**
						 * Extract JIP param from pai jip/rn parameter if
						 * available
						 */

						jipParam = paiUri.getParameter(PhConstants.JIP_PARAM);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: PAI JIP parameter is " + jipParam
									+ " PAI RN param is " + jipParam
									+ "This will be used as Isup JIP");
						}

						if (jipParam == null) {
							jipParam = paiUri
									.getParameter(PhConstants.RN_PARAM);
						}

						/**
						 * Adding below code for extracting CIC parameter
						 */
						// cic = paiUri.getParameter(PhConstants.CIC_PARAM);

						/**
						 * Adding below code for extracting NOA parameter
						 */
						noa = paiUri.getParameter(PhConstants.NOA_PARAM);

						if (paiUri.isSipURI()) {
							String tmp = ((SipURI) paiUri).getUser();
							tmp = (tmp == null) ? "" : tmp;
							paiUser = tmp.split(";")[0];

						} else {
							isTel=true;
							String tmp = ((TelURL) paiUri).getPhoneNumber();
							tmp = (tmp == null) ? "" : tmp;
							paiUser = tmp.split(";")[0];
							
							paiTelCPC = paiUri
									.getParameter(PhConstants.CPC_PARAM);

							if(paiTelCPC==null){
								paiTelCPC = paiAddress.getParameter(PhConstants.CPC_PARAM);
							}

							if (paiTelCPC != null) {

								String cpc = getCpcFromString(paiTelCPC, legData);
								legData.set(
										LegDataAttributes.P_CPC_PAI_TEL_HEADER, cpc);
								
							}

							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: PAI Tel Calling Party Category is "
										+ paiTelCPC);
							}

						}

						/*
						 * adding + here because tel phonenumber donot returns +
						 * sign
						 */
						if (paiUri.toString().indexOf("+") != -1
								&& !paiUser.startsWith("+")) {
							paiUser = "+" + paiUser;

							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: PAI user after adding plus  "
										+ paiUser);
							}
						}

					}
				
					paiNumber = new PhoneNumber();
					paiNumber.setAddress(paiUser);
					paiNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
					paiNumber.setNatureOfAddress(paiUser != null
									&& !paiUser.isEmpty()
									&& paiUser.startsWith("+") ? PhoneNumber.NOA_INTERNATIONAL
									: PhoneNumber.NOA_NATIONAL);
					
					String parseTelSip=SipProtocolConfig.getConfigData(SipProtocolConfig.PARSE_PAI_TEL_SIP_USER);
					
					if (isTel && PhConstants.TRUE.equals(parseTelSip)) {
						legData.set(LegDataAttributes.P_CALLING_PARTY_TEL,
								paiNumber);
					} else {
						legData.set(
								LegDataAttributes.P_CALLING_PARTY_PAI_HEADER,
								paiNumber);
						legData.set(LegDataAttributes.P_CALLING_PARTY,
								paiNumber);
					}


					if (jipParam != null) {
						legData.set(LegDataAttributes.P_JIP,
								jipParam);
					}

					if (noa != null) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: NOA  parameter from PAI header is "
									+ noa
									+ " This will be used as Nature of address");
						}
						legData.set(LegDataAttributes.P_NOA, noa);
					}
					

				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: pai user is " + paiUser);
			}

		} catch (ServletParseException e) {
			logger.error(origLegCallId
					+ ":: Unable to parse P-Asserted-Identity Header. "
					+ e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Error in parsing P-Asserted-Identity", e);
			}
		}

		// P-Sig-Info Header
		PhoneNumber contractorNumber = null;
		String pSigInfoHdr = inviteRequest
				.getHeader(PhConstants.P_SIG_INFO_HEADER);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: P-Sig-Info is " + pSigInfoHdr);
		}

		if (pSigInfoHdr != null && !pSigInfoHdr.trim().isEmpty()) {
			String cntrctNumber = pSigInfoHdr.substring(pSigInfoHdr
					.indexOf("=") + 1);
			contractorNumber = new PhoneNumber();
			contractorNumber.setAddress(cntrctNumber);
			contractorNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
			contractorNumber
			.setNatureOfAddress(fromUser != null && !fromUser.isEmpty()
			&& fromUser.startsWith("+") ? PhoneNumber.NOA_INTERNATIONAL
					: PhoneNumber.NOA_NATIONAL);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Contractor number detail is "
						+ contractorNumber);
			}
		}
		if (contractorNumber != null) {
			legData.set(LegDataAttributes.P_CONTRACTOR_NUMBER,
					contractorNumber);
		}

		// To Header //changing this for level3 issue R-URI and to user are diffrent so using R-URI instead of To for called number

		String readCalledFromRuri= SipProtocolConfig.getConfigData(SipProtocolConfig.READ_CALLED_NUM_FROM_RURI);

		URI toUri=null;

		if (PhConstants.TRUE.equals(readCalledFromRuri)) {

			toUri = inviteRequest.getRequestURI();
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Reading toUri from URI " + toUri);
			}
		}else{
			toUri = inviteRequest.getTo().getURI();
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Reading toUri from To Hdr " + toUri);
			}
		}

		callData.set(CallDataAttribute.TO_URI, toUri);

		/**
		 * Set destination number as per configuration
		 */
		setDestinationNumber(origLegCallId, callData, legData, toUri);

		String iscHeader = inviteRequest.getHeader(PhConstants.X_ISC_SVC);
		if (iscHeader != null && !iscHeader.isEmpty()) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Inter service communication call " + iscHeader);
			}
			callData.set(CallDataAttribute.P_X_ISC_SVC,
					PhConstants.TRUE);
		}

		/*
		 * SBTM-UAT-365: Calling Party PI should be decided on the basis of
		 * Privacy header value. If its value is "id", then PI is restricted.
		 */
		// Privacy Header - Presentation Indicator of Calling Number
		String privacyHeader = inviteRequest.getHeader("Privacy");
		
		legData.set(LegDataAttributes.NP_PRIVACY_HEADER,privacyHeader);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: privacyHeader is " + privacyHeader);
		}
		if (privacyHeader != null) {
			if ("id".equals(privacyHeader.trim())) {
				callingNumberFromHeader.setPresentationIndicator(1);
				if (paiNumber != null) {
					paiNumber.setPresentationIndicator(1);
				}

			} else {
				callingNumberFromHeader.setPresentationIndicator(0);
				if (paiNumber != null) {
					paiNumber.setPresentationIndicator(0);
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Calling Number PI is "
						+ callingNumberFromHeader.getPresentationIndicator());
			}
		}

		//P-Charge-Info header parsing
		Address chargeInfoHeader = inviteRequest.getAddressHeader(PhConstants.P_CHARGE_INFO);
		PhoneNumber phoneNumber = getPhoneNumberFromChargeInfoHeader(origLegCallId, chargeInfoHeader);
		if(null != phoneNumber){
			legData.set(LegDataAttributes.P_CHARGE_INFO,	phoneNumber);
		}

		String carrierInfoHeader = inviteRequest
				.getHeader(PhConstants.P_CARRIER_INFO);
		if (carrierInfoHeader != null) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: carrier info is  "
						+ carrierInfoHeader);
			}

			String[] cicHeaders = carrierInfoHeader.split(";");

			if (cicHeaders.length > 0) {
				int cicindex = cicHeaders[0].indexOf("cic=");

				if (cicindex != -1) {
					String cicSub = cicHeaders[0].substring(cicindex + "cic=".length());

					cic = cicSub.substring(0, cicSub.indexOf(","));
				}
			}

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Set CIC in legdata  " + cic);
			}

			legData.set(LegDataAttributes.P_CIC, cic);
		}

		String pChargeVect = inviteRequest
				.getHeader(PhConstants.P_CHARGE_VECTOR);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: P-Charge-Vector is:-  " + pChargeVect);
		}

		if (pChargeVect != null) {
			callData.set(CallDataAttribute.P_CHARGE_VECTOR,
					pChargeVect);
		}
		
		String P_CHARGING_VECTOR_ORIG_IOI = null;
		String P_CHARGING_VECTOR_TERM_IOI = null; 
		
		if (StringUtils.isNotEmpty(pChargeVect)){
			if(StringUtils.containsIgnoreCase(pChargeVect, "term-ioi") || StringUtils.contains(pChargeVect, "orig-ioi")) {
				String paramVect[]= pChargeVect.split(";");
		        for(int i=0; i<paramVect.length; i++){
		            if(paramVect[i].contains("=")){
		                String pChVect[]=paramVect[i].split("=");
		                
		                if(pChVect[0].trim().equals("term-ioi")){
		                	P_CHARGING_VECTOR_TERM_IOI =pChVect[1];
		                }
		                else if(pChVect[0].trim().equals("orig-ioi")){
		                	P_CHARGING_VECTOR_ORIG_IOI = pChVect[1];
		                } 
		            }

		        }
			}
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(":: P_CHARGE_VECTOR params are  " + P_CHARGING_VECTOR_ORIG_IOI +" ,  "+ P_CHARGING_VECTOR_TERM_IOI);
		}
		
		callData.set(CallDataAttribute.P_CHARGING_VECTOR_ORIG_IOI , P_CHARGING_VECTOR_ORIG_IOI);
		
		callData.set(CallDataAttribute.P_CHARGING_VECTOR_TERM_IOI , P_CHARGING_VECTOR_TERM_IOI);
		
		/**
		 * Following fields extracted for new mph1.0 enhncemnets
		 */

		String origContactUri = inviteRequest
				.getHeader(PhConstants.CONTACT_HEADER);
		callData.set(CallDataAttribute.P_ORIG_CONTACT_URI,
				origContactUri);

		String tgrp = inviteRequest.getRequestURI().getParameter(
				PhConstants.TGRP_PARAM);
		String trunkContext = inviteRequest.getRequestURI().getParameter(
				PhConstants.TRUNK_CONTEXT);

		if (tgrp != null && trunkContext != null) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: fetched tgrp and trunkcontext from request uri "
						+ tgrp + " " + trunkContext);
			}
			callData.set(CallDataAttribute.P_DEST_TGRP, tgrp);
			callData.set(CallDataAttribute.P_DEST_TRUNK_CONTEXT,
					trunkContext);
		} else {
			String dtgParam = inviteRequest.getRequestURI().getParameter(
					PhConstants.DTG_PARAM);
			if (dtgParam != null) {

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: DTG param  in request uri is  " + dtgParam);
				}
				callData.set(
						CallDataAttribute.P_DEST_TRUNK_GROUP, dtgParam);
			}
		}

		String sendOliAsCpc= SipProtocolConfig.getConfigData(SipProtocolConfig.SET_OLI_AS_CPC);

		if (PhConstants.TRUE.equals(sendOliAsCpc)
				&& callData.get(CallDataAttribute.P_ORIG_LINE_INFO) == null) {

			String origCpc = (String) legData
					.get(LegDataAttributes.P_CPC_ORIGINAL);

			if (origCpc != null && origCpc.equals(PhConstants.PAY_PHONE)) {

				String oliAsCpcPayphoneVal = SipProtocolConfig
						.getConfigData(SipProtocolConfig.OLI_AS_CPC_PAYPHONE_VALUE);
				if (oliAsCpcPayphoneVal != null) {
					callData.set(CallDataAttribute.P_ORIG_LINE_INFO,
							oliAsCpcPayphoneVal);
				}
			} else {
				callData.set(CallDataAttribute.P_ORIG_LINE_INFO, "0");
			}

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: set P_ORIG_LINE_INFO as  " + callData.get(CallDataAttribute.P_ORIG_LINE_INFO));
			}

		}else{

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: P_ORIG_LINE_INFO is  " + callData.get(CallDataAttribute.P_ORIG_LINE_INFO));
			}
		}
		
		ListIterator<String> historyInfoIterator = inviteRequest.getHeaders(PhConstants.HISTORY_INFO);
		if(historyInfoIterator != null) {
			List<String> historyInfoHeaders = new ArrayList<String>();
			String temp = null;

			while(historyInfoIterator.hasNext()) {
				temp = historyInfoIterator.next();
				String[] historyInfos = temp.split(",");
				if(historyInfos.length > 1) {
					for(String historyInfo : historyInfos) {
						historyInfoHeaders.add(historyInfo);
					}
				}else {
					historyInfoHeaders.add(historyInfos[0]);
				}
			}
			
			legData.set(LegDataAttributes.P_HISTORY_INFO, historyInfoHeaders);
			
			if(logger.isDebugEnabled()) {
				logger.debug(":: P_HISTORY_INFO is:- " + legData.get(LegDataAttributes.P_HISTORY_INFO));
			}
		}
		
		
		/**
		 * if the RAT-Type is UTRAN/GERAN:
		 * 
		 * P-Access-Network-Info: 3GPP-UTRAN;utran-cell-id-3gpp=C359A3913B20E
		 * P-Access-Network-Info: 3GPP-GERAN;cgi-3gpp=62F8100005C599
		 * 
		 * If the RAT-Type is EUTRAN:
		 * 
		 * P-Access-Network-Info: 3GPP-E-UTRAN;utran-cell-id-3gpp=C359A3913B20E
		 */
		
		parseCellIDFromPANIHeader(inviteRequest,legData);
		
		
		parsePServedUserHeader(inviteRequest,legData);
	}
	
	
	private static void parsePServedUserHeader(SipServletRequest inviteRequest,
			LegData legData) {

		if (logger.isDebugEnabled()) {
			logger.debug(":: parsePServedUserHeader is ");
		}
		//---------------------------------------- P-Served-User Header
		
		String pSerSessCase = null;
		String pSerPriority = null;
		String pServdUserPart = null;  
		//P-Served-User: <sip:user@example.com>; sescase=orig;user-part=CGPN;Priority=emergency
		
		String pSerUser = inviteRequest.getHeader(PhConstants.P_SERVED_USER);
		pSerPriority = inviteRequest.getHeader(PhConstants.HDR_PRIORITY);
		
		if (logger.isDebugEnabled()) {
			logger.debug(":: Priority in To hdr  is "+ pSerPriority);
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(":: parsePServedUserHeader is "+ pSerUser);
		}
		
		if (pSerUser != null && !pSerUser.trim().isEmpty()){
			String paramHeader[]=pSerUser.split(";");
	        for(int i=0; i<paramHeader.length; i++){
	            if(paramHeader[i].contains("=")){
	                String pServdCases[]=paramHeader[i].split("=");
	                
	                if(pServdCases[0].equals(" sescase") || pServdCases[0].equals("sescase")){
	                     pSerSessCase = pServdCases[1];
//    					if(paramSessCase.equals("orig"))
//    						pSerSessCase = "A-party";
//    					else
//    						pSerSessCase = "B-party";
	                }else if(pServdCases[0].equals(" user-part") || pServdCases[0].equals("user-part")){
	                    pServdUserPart =pServdCases[1];
	                }
//	                else if(pServdCases[0].equalsIgnoreCase("priority")){
//	                    String paramPriority = pServdCases[1];
//    					//if(paramPriority.equals("emergency"))
//    						pSerPriority = paramPriority ;//"present";
//    					//else
//    					//	pSerPriority = "proxy-mode";
//	                }
	            }
	        }
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(":: P-Served-User params are  " + pSerSessCase +" ,"+ pServdUserPart+ " ,"+ pSerPriority);
		}
		
		legData.set(LegDataAttributes.P_SERVD_USER_SESS_CASE , pSerSessCase);
		
		legData.set(LegDataAttributes.P_SERVD_USER_USERPART , pServdUserPart);
		
		legData.set(LegDataAttributes.P_SERVD_USER_PRIORITY , pSerPriority);
	}


	/**
	 * 
	 * @param inviteRequest
	 * @param leg2Data
	 */
	protected static void updatePaniCellIdInOutgoing(SipServletRequest inviteRequest,
				LegData leg2Data) {

			if(logger.isDebugEnabled()) {
				logger.debug("Entering updatePaniCellIdInOutgoing ");
			}
			
			ListIterator<String> paniHeaders = inviteRequest
					.getHeaders(PhConstants.PANI_HEADER);
			String paniAddon=(String) leg2Data.get(LegDataAttributes.P_PANI_ADD_ON_PART);
			
			List<String> updatedPANIHeaders=new ArrayList<String>();
			
		if (paniAddon != null && paniHeaders!=null) {
			
			while (paniHeaders.hasNext()) {

				String paniHeader = paniHeaders.next();

				if (logger.isDebugEnabled()) {
					logger.debug("PANI header is  " + paniHeader);
				}
				if (paniHeader != null) {

					if (paniHeader.indexOf(PhConstants.PANI_CELL_ID) != -1
							|| paniHeader
									.indexOf(PhConstants.PANI_CGI_3GPP) != -1
							|| paniHeader
									.indexOf(PhConstants.PANI_SAI_3GPP) != -1
							|| paniHeader
									.indexOf(PhConstants.PANI_XXXXXXX_CELL_ID) != -1) {

						paniHeader=paniHeader+paniAddon;

						if (logger.isDebugEnabled()) {
							logger.debug("add PANI ADDOn to PANI header "
									+ paniHeader);
						}

						updatedPANIHeaders.add(paniHeader);
					}
				}
			}
			
			if (!updatedPANIHeaders.isEmpty()) {
				
				if (logger.isDebugEnabled()) {
					logger.debug("Updated list of pani headers is  "+ updatedPANIHeaders);
				}
				
				if (logger.isDebugEnabled()) {
					logger.debug("remove Existing  PANI to add updated PANI and update with updated ");
				}
				inviteRequest.removeHeader(PhConstants.PANI_HEADER);
				
				Iterator<String> updatedPaniItr=updatedPANIHeaders.iterator();

				while (updatedPaniItr.hasNext()) {
					
					String paniHeader = updatedPaniItr.next();

					if (logger.isDebugEnabled()) {
						logger.debug("add updated PANI to outgoing request "
								+ paniHeader);
					}
					inviteRequest
							.addHeader(PhConstants.PANI_HEADER, paniHeader);
				}

				if (logger.isDebugEnabled()) {
					logger.debug("Leaving updatePaniCellIdInOutgoing ");
				}
			}

		}
	}
	     
	
	
	/**
	 * 
	 * @param inviteRequest
	 * @param legData
	 */
	private static void parseCellIDFromPANIHeader(SipServletRequest inviteRequest,
			LegData legData) {

		if(logger.isDebugEnabled()) {
			logger.debug("Entering parseCellIDFromPANIHeader ");
		}
		
		ListIterator<String> paniHeaders = inviteRequest
				.getHeaders(PhConstants.PANI_HEADER);

		// Radio Access Type and Access Type Mapping
		// 3GPP-UTRAN utran-cell-id-3gpp
		// 3GPP-UTRAN cgi-3gpp
		// 3GPP-E-UTRAN-FDD utran-cell-id-3gpp
		// 3GPP-UTRAN-TDD utran-cell-id-3gpp
		// 3GPP-UTRAN-FDD utran-sai-3gpp
		
		while (paniHeaders.hasNext()) {
			boolean cellIdFound = false;

			String paniHeader = paniHeaders.next();
			
			if(logger.isDebugEnabled()) {
				logger.debug("PANI header is  "+paniHeader);
			}
			String paniCellId = "";
			if (paniHeader != null) {

				String[] paniFields = paniHeader.split(";");
				List<String> paniFieldsList = Arrays.asList(paniFields);

				int fetchCellIdField=-1;
				
				String accesstype=null;
				
				for (String paniField : paniFieldsList) {
					
					if(logger.isDebugEnabled()) {
						logger.debug("PANI field is  "+paniField);
					}
					
					switch(paniField){
					
					case PhConstants.PANI_3GPP_GERAN:{
					//	fetchCgi = true;
						fetchCellIdField= PhConstants.FETCH_PANI_CGI_3GPP;
						accesstype=paniField;
						continue;				
					}
					case PhConstants.PANI_3GPP_UTRAN_FDD: {
					//	fetchSai = true;
						fetchCellIdField= PhConstants.FETCH_PANI_SAI_3GPP;
						accesstype=paniField;
						continue;
					}
					case PhConstants.PANI_3GPP_UTRAN_TDD: {
					//	fetchSai = true;
						fetchCellIdField= PhConstants.FETCH_PANI_SAI_3GPP;
						accesstype=paniField;
						continue;
					}

					case PhConstants.PANI_3GPP_E_UTRAN_FDD: {
						//fetchCellId = true;
						fetchCellIdField= PhConstants.FETCH_PANI_CELL_ID;
						accesstype=paniField;
						continue;
					}

					case PhConstants.PANI_3GPP_E_UTRAN_TDD:{
					//	fetchCellId = true;
						fetchCellIdField= PhConstants.FETCH_PANI_CELL_ID;
						accesstype=paniField;
						continue;
					}
					case PhConstants.PANI_3GPP_NR_FDD:{
						
						//fetchXXX = true;
						fetchCellIdField= PhConstants.FETCH_PANI_XXXXXXX_CELL_ID;
						accesstype=paniField;
						continue;
					  }
                  case PhConstants.PANI_3GPP_NR_TDD:{
						
						//fetchXXX = true;
						fetchCellIdField= PhConstants.FETCH_PANI_XXXXXXX_CELL_ID;
						accesstype=paniField;
						continue;
					  }
					}
					
					if(logger.isDebugEnabled()) {
						logger.debug("PANI access type  is  "+accesstype +" For PANI field " + paniField);
					}
					
					switch(fetchCellIdField){// (fetchSai) {
					
					case PhConstants.FETCH_PANI_SAI_3GPP:{
						if(logger.isDebugEnabled()) {
							logger.debug("Fetch Cell id from   "+PhConstants.PANI_SAI_3GPP);
						}
						if (paniField.indexOf(PhConstants.PANI_SAI_3GPP) != -1) {

							paniCellId = paniField.substring(
									paniField.length() - 8, paniField.length());
							cellIdFound = true;
						}
						break;
					}
					case PhConstants.FETCH_PANI_CELL_ID:{
						
						if(logger.isDebugEnabled()) {
							logger.debug("Fetch Cell id from   "+PhConstants.PANI_CELL_ID);
						}
						if (paniField.indexOf(PhConstants.PANI_CELL_ID) != -1) {

							paniCellId = paniField.substring(
									paniField.length() - 7, paniField.length());
							cellIdFound = true;
						}
						break;
					}

					case PhConstants.FETCH_PANI_CGI_3GPP:{
						
						if(logger.isDebugEnabled()) {
							logger.debug("Fetch Cell id from   "+PhConstants.PANI_CGI_3GPP);// +" OR "+ PhConstants.PANI_CELL_ID);
						}
						if (paniField.indexOf(PhConstants.PANI_CGI_3GPP) != -1) {

							paniCellId = paniField.substring(
									paniField.length() - 8, paniField.length());
							cellIdFound = true;
						}
						break;
					}
					
					case PhConstants.FETCH_PANI_XXXXXXX_CELL_ID:{
						if(logger.isDebugEnabled()) {
							logger.debug("Fetch Cell id from   "+PhConstants.PANI_XXXXXXX_CELL_ID);// +" OR "+ PhConstants.PANI_CELL_ID);
						}
						if (paniField.indexOf(PhConstants.PANI_XXXXXXX_CELL_ID) != -1) {

							paniCellId = paniField.substring(
									paniField.length() - 9, paniField.length());
							cellIdFound = true;
						}
						break;
					}
					default:
						if(logger.isDebugEnabled()) {
							logger.debug("inside default cellid not found");// +" OR "+ PhConstants.PANI_CELL_ID);
						}
						cellIdFound=false;
				}

					if (cellIdFound) {
						break;
					}
				}

				if (cellIdFound) {
					
					if(logger.isDebugEnabled()) {
						logger.debug("Cell id is found  "+paniCellId);
					}
					legData.set(LegDataAttributes.NP_PANI_CELL_ID, paniCellId);
					legData.set(LegDataAttributes.NP_PANI_ACCESS_TYPE, accesstype);
					legData.set(LegDataAttributes.NP_PANI_HEADER, paniHeader);
					break;
				}

			}
		}

	}

	/**
	 * Method to get cpc from string.
	 * 
	 * @param cpcStr
	 *            represents the instance of String
	 * @param legData
	 *            TODO
	 * @return integer equivalent of the CPC String
	 */
	protected static String getCpcFromString(String cpcStr, LegData legData) {
		String cpc = "0";

		legData.set(LegDataAttributes.P_CPC_ORIGINAL, cpcStr);

		if (cpcStr.equalsIgnoreCase("unknown")) {
			cpc = "0";
			// }else if (cpcStr.equalsIgnoreCase("french")) {
			// cpc = 1;
			// }else if (cpcStr.equalsIgnoreCase("english")) {
			// cpc = 2;
			// }else if (cpcStr.equalsIgnoreCase("german")) {
			// cpc = 3;
			// }else if (cpcStr.equalsIgnoreCase("french")) {
			// cpc = 4;
			// }else if (cpcStr.equalsIgnoreCase("spanish")) {
			// cpc = 5;
		} else if (cpcStr.equalsIgnoreCase("ordinary")) {
			cpc = "10";
		} else if (cpcStr.equalsIgnoreCase("test")) {
			cpc = "13";
		} else if (cpcStr.equalsIgnoreCase("operator")) {
			cpc = "1";
		} else if (cpcStr.equalsIgnoreCase("spare")) {
			cpc = "14";
		} else if (cpcStr.equalsIgnoreCase("payphone")) {
			cpc = "15";
		} else if (cpcStr.equalsIgnoreCase("data")) {
			cpc = "12";
		} else if (cpcStr.equalsIgnoreCase("priority")) {
			cpc = "11";
		}
		return cpc;
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

	/**
	 * This method sets the session expiry time received in INVITE in its sip
	 * session for future reference.
	 * 
	 * @param sipRequest
	 *            represents an instance of SipServletRequest
	 * @param origLegCallId
	 *            represents an instance of String
	 */
	public static void setSessionExpiryTime(SipServletRequest sipRequest,
			LegData legData, String origLegCallId) {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside setSessionExpiryTime");
		}
		String sessionExpiryHeader = sipRequest
				.getHeader(PhConstants.SESSION_EXPIRE_HEADER);

		ConnectionType legType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		int sTermSessionRefresh = Integer.parseInt(SipProtocolUtil.getConfig(
				SipProtocolConfig.TERM_SESSION_REF_SUPPORT));

		if(ConnectionType.TERM_CONNECTION.equals(legType)&& sTermSessionRefresh==0){

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: session refresh not supported on term leg so not fetching sesison expiry headers");
			}
			return;
		}
		if (sessionExpiryHeader != null && !sessionExpiryHeader.isEmpty()) {
			String[] sessionExpiryHeaderArray = sessionExpiryHeader.split(";");
			if (sessionExpiryHeaderArray[0] != null
					&& !sessionExpiryHeaderArray[0].isEmpty()) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Store "
							+ sessionExpiryHeaderArray[0]
									+ " as session refresh time for reference");
				}
				legData.set(
						LegDataAttributes.P_SESSION_EXPIRE_TIME,
						sessionExpiryHeaderArray[0]);
			}
		}

		String minSEHeader = sipRequest.getHeader(PhConstants.MIN_SE_HEADER);
		if (minSEHeader != null && !minSEHeader.isEmpty()) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Store " + minSEHeader
						+ " as Min-SE for reference");
			}
			legData.set(
					LegDataAttributes.P_SESSION_EXPIRE_MIN_SE, minSEHeader);
		}
	}

	/**
	 * This method sets the session expiry time received in 200 OK in its sip
	 * session for future reference.
	 * 
	 * @param sipResponse
	 *            represents an instance of SipServletResponse
	 * @param origLegCallId
	 *            represents an instance of String
	 */
	public static void setSessionExpiryTime(SipServletResponse sipResponse,
			LegData legData, String origLegCallId) {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside setSessionExpiryTime");
		}

		ConnectionType legType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		int sTermSessionRefresh = Integer.parseInt(SipProtocolUtil.getConfig(
				SipProtocolConfig.TERM_SESSION_REF_SUPPORT));

		if(ConnectionType.TERM_CONNECTION.equals(legType)&& sTermSessionRefresh==0){

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: session refresh not supported on term leg");
			}
			return;
		}
		String sessionExpiryHeader = sipResponse
				.getHeader(PhConstants.SESSION_EXPIRE_HEADER);
		if (sessionExpiryHeader != null && !sessionExpiryHeader.isEmpty()) {
			String[] sessionExpiryHeaderArray = sessionExpiryHeader.split(";");
			if (sessionExpiryHeaderArray[0] != null
					&& !sessionExpiryHeaderArray[0].isEmpty()) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Store "
							+ sessionExpiryHeaderArray[0]
									+ " as session refresh time for reference");
				}
				legData.set(
						LegDataAttributes.P_SESSION_EXPIRE_TIME,
						sessionExpiryHeaderArray[0]);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Session Refresh duration is missing in response");
					logger.debug(origLegCallId
							+ ":: Disable session refresh on this leg");
				}
				legData.remove(LegDataAttributes.P_SESSION_EXPIRE_TIME);
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Session Refresh header is missing in response");
				logger.debug(origLegCallId
						+ ":: Disable session refresh on this leg");
			}
			legData.remove(LegDataAttributes.P_SESSION_EXPIRE_TIME);
		}
	}

	/**
	 * Returns Max of session refresh time in seconds from legData1 and legData2
	 * 
	 * @param appSession
	 *            represents an instance of SipApplicationSession
	 * @param origLegCallId
	 *            represents an instance of String
	 * @param legData1
	 *            represents an instance of LegData
	 * @param legData2
	 *            represents an instance of LegData
	 * @return integer value of maximum session expire time
	 */
	public static int getMaxSessionExpireTime(SipApplicationSession appSession,
			String origLegCallId, LegData legData1, LegData legData2) {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside getMaxSessionExpireTime");
		}

		String leg1SeValue = (String) legData1
				.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);

		String leg2SeValue = null;
		if (legData2 != null) {
			leg2SeValue = (String) legData2
					.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);
		}

		int leg1SessionRefreshTime = (leg1SeValue == null || leg1SeValue
				.isEmpty()) ? 0 : Integer.parseInt(leg1SeValue);
		int leg2SessionRefreshTime = (leg2SeValue == null || leg2SeValue
				.isEmpty()) ? 0 : Integer.parseInt(leg2SeValue);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Leg1 SE time is "
					+ leg1SessionRefreshTime);
			logger.debug(origLegCallId + " :: Leg2 leg SE time is "
					+ leg2SessionRefreshTime);
		}

		return (leg1SessionRefreshTime > leg2SessionRefreshTime) ? leg1SessionRefreshTime
				: leg2SessionRefreshTime;
	}

	/**
	 * This method extract and return the Initial invite from the
	 * SipApplicationSession passed.
	 * 
	 * @param appSession
	 *            represents an instance of SipApplicationSession
	 * @param connectionType
	 *            integer equivalent of connection type
	 * @return an instance of SipServletRequest
	 */
	public static SipServletRequest getInitialInvite(
			SipApplicationSession appSession, LegData legData) {
		SipServletRequest sipServletRequest = null;
		if (legData == null) {
			return sipServletRequest;
		}

		CallData callData=SipProtocolUtil.getCallData(appSession);
		if (legData.get(LegDataAttributes.APP_SESSION_ID) != null) {
			String appSessionId = (String) legData
					.get(LegDataAttributes.APP_SESSION_ID);

			if (!appSession.getId().equals(appSessionId)) {
				appSession = PhUtilityServices
						.getInstance(
								(String) callData
								.get(CallDataAttribute.SERVICE_ID))
								.getSipSessionsUtil()
								.getApplicationSessionById(appSessionId);
			}
		} 

		Iterator<?> it = appSession.getSessions("SIP");
		SipSession sipSession = null;
		if (legData
				.get(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND) != null) {
			return sipServletRequest;
		}
		String sipSessionId = (String) legData
				.get(LegDataAttributes.P_SESSION_ID);

		while (it.hasNext()) {
			sipSession = (SipSession) it.next();
			if (!sipSession.isValid()) {
				logger.debug("Invalid Sip Session , Returning orig request as null");
				return sipServletRequest;
			}
			if (sipSession.getId().equals(sipSessionId)) {
				if (logger.isDebugEnabled()) {
					logger.debug("Return sip request for sipSession "
							+ sipSession.getId());
				}
				sipServletRequest = (SipServletRequest) sipSession
						.getAttribute(PhConstants.ORIG_REQUEST);
				break;
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("Returning sip request  " + sipServletRequest);
		}
		return sipServletRequest;
	}

	public static SipServletRequest getOrigInitialInvite(
			SipApplicationSession appSession, CallData callData) {
		return getInitialInvite(appSession,
				(LegData) callData.get(CallDataAttribute.P_LEG1));

	}

	/**
	 * This method iterates through all sip sessions in passwd app-session and
	 * returns the sip-session matching to the passedsession Id
	 * 
	 * @param appSession
	 * @param sessionId
	 * @return sipSession
	 */
	public static SipSession getSipSessionFromSessionId(String origLegCallId,
			SipApplicationSession appSession, String sessionId) {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside getSipSessionFromSessionId()");
		}
		SipSession sipSession = null;
		if (sessionId == null) {
			return sipSession;
		}

		Iterator<?> it = appSession.getSessions("SIP");
		while (it.hasNext()) {
			sipSession = (SipSession) it.next();
			if (sipSession.getId().equals(sessionId)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: SipSession found for "
							+ sessionId);
				}
				break;
			}
			sipSession = null;
		}
		return sipSession;
	}


	public static SipApplicationSession getAppSessionForLegId(String origLegCallId,
			LegData legData,CallData callData) {

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside getAppSessionFromAppSessionId() for "+legData);
		}
		if (legData.get(LegDataAttributes.APP_SESSION_ID) != null) {
			String appSessionId = (String) legData
					.get(LegDataAttributes.APP_SESSION_ID);
			return PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil()
					.getApplicationSessionById(appSessionId);
		} else {
			return null;
		}

	}

	// COMMENTED BY Ankit. There cannot be a case when the property will be
	// found in CallData since
	// call data map is restricted with CallDataAttributes. Thus only
	// considering SipProtocolConfig
	// /**
	// * This method extracts and returns config value. Initially it checks
	// * callData for the passed property name. If it is found in callData then
	// it
	// * is returned otherwise default value of that property from
	// * SipProtocolConfig is returned
	// *
	// * @param proertyName
	// * @return
	// */
	// public static String getConfig(CallData callData, String proertyName) {
	// String propValue = null;
	// /*
	// * Initially when a new call is received then callData will be null
	// */
	// try {
	// CallDataAttribute propertyNameEnum =
	// CallDataAttribute.valueOf(proertyName);
	// if (callData != null) {
	// propValue = callData.getPersistableData(propertyNameEnum) == null ? null
	// : (String) callData
	// .getPersistableData(propertyNameEnum);
	// if (propValue != null) {
	// return propValue;
	// }
	// }
	// } catch(IllegalArgumentException iae){
	// return SipProtocolConfig.getConfigData(proertyName);
	// }
	// return SipProtocolConfig.getConfigData(proertyName);
	// }

	public static String getConfig(String propertyName) {
		return SipProtocolConfig.getConfigData(propertyName);
	}

	/**
	 * This method is to get sdp content for hold as per RFC 3264, to put
	 * another party on hold set a= inactive and do not change c line.
	 * 
	 * @param origLegCallId
	 *            represents an instance of String
	 * @param sdpContent
	 *            represents an instance of MultipartBody
	 * @param needToSendHoldAsInactive
	 * 			  if true, Hold will be sent in INACTIVE mode otherwise it will be SENDONLY.
	 * @return an instance of MultipartBody
	 * @throws Exception
	 */
	public static MultipartBody getSdpContentForHold(String origLegCallId,
			MultipartBody sdpContent, boolean needToSendHoldAsInactive) throws Exception {
		ByteArrayOutputStream baos = null;

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside getSdpContentForHold. Hold type is inactive");
		}

		if (sdpContent == null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Sdp content is null");
			}
			return null;
		}
		/*
		 * As per RFC 3264, to put another party on hold, set a= inactive and do
		 * not change c line. so commenting below code where c line is being
		 * modified to 0.0.0.0. Fix for SBTM-UAT-748
		 */
		// updating SDP to make to put party-A on hold
		DsSdpMsg dsSdpMsg = new DsSdpMsg(sdpContent.getContent());

		/*
		 * if (holdType == PhConstant.HOLD_TYPE_INACT_IP || holdType ==
		 * PhConstant.HOLD_TYPE_INACT_IP_AND_A) { if (logger.isDebugEnabled()) {
		 * logger.debug(origLegCallId + ":: Change connection field"); }
		 * 
		 * // setting IP-Address in c line to 0.0.0.0 DsSdpConnectionField
		 * connField = dsSdpMsg.getConnectionField();
		 * 
		 * if (connField != null) { if (logger.isDebugEnabled()) {
		 * logger.debug(origLegCallId + ":: Replace " + connField.getAddr() +
		 * " with " + PhConstant.INACTIVE_IP); }
		 * connField.setAddr(PhConstant.INACTIVE_IP); }
		 * 
		 * 
		 * if (logger.isDebugEnabled()) { logger.debug(origLegCallId +
		 * ":: Change connetion field in media field"); }
		 * 
		 * DsSdpMediaDescription[] mediaFields =
		 * dsSdpMsg.getMediaDescriptionList(); for (int i = 0; mediaFields !=
		 * null && i < mediaFields.length; i++) { connField = mediaFields[i] !=
		 * null ? (DsSdpConnectionField) mediaFields[i]
		 * .getField(DsSdpField.CONNECTION_FIELD_INDICATOR) : null; if
		 * (connField != null) { logger.debug(origLegCallId + ":: Replace " +
		 * connField.getAddr() + " with " + PhConstant.INACTIVE_IP);
		 * connField.setAddr(PhConstant.INACTIVE_IP); } } }
		 */

		// Adding attribute a=inactive to all media descriptions
		boolean addedInactive = false;
		boolean addedSendOnly = false;
		DsSdpMediaDescription[] mediaFields = dsSdpMsg
				.getMediaDescriptionList();
		if (mediaFields != null) {
			for (int i = 0; i < mediaFields.length; i++) {
				if (mediaFields[i] != null) {
					DsSdpAttributeField[] attribField = mediaFields[i]
							.getAttributeFields();
					String value = null;
					for (int j = 0; j < attribField.length; j++) {
						value = attribField[j].getAttribute();
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: The attribute value is " + value);
						}
						if (PhConstants.A_SENDRECV.equals(value)
								|| PhConstants.A_SENDONLY.equals(value)
								|| PhConstants.A_RECVONLY.equals(value)
								|| PhConstants.A_INACTIVE.equals(value)) {
							if(needToSendHoldAsInactive){
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ ":: Replacing attribute with "
											+ PhConstants.A_INACTIVE);
								}
								attribField[j].setAttribute(PhConstants.A_INACTIVE);
								addedInactive = true;
							}else{
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ ":: Replacing attribute with "
											+ PhConstants.A_SENDONLY);
								}
								attribField[j].setAttribute(PhConstants.A_SENDONLY);
								addedSendOnly = true;
							}

						}
					}
				}
			}
		}
		/*
		 * If a line is not present in SDP then add a=inactive in SDP.
		 */
		if (!addedInactive && needToSendHoldAsInactive) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Adding attribute a="
						+ PhConstants.A_INACTIVE);
			}
			DsSdpAttributeField attrFiled = new DsSdpAttributeField(
					PhConstants.A_INACTIVE);
			mediaFields[0].addField(attrFiled);
		}

		/*
		 * If a line is not present in SDP add a=sendonly in SDP.
		 */
		if(!addedSendOnly && !needToSendHoldAsInactive){
			if(logger.isDebugEnabled()){
				logger.debug(origLegCallId + ":: Adding attribute a=" + PhConstants.A_SENDONLY);
			}
			DsSdpAttributeField attributeField = new DsSdpAttributeField(PhConstants.A_SENDONLY);
			mediaFields[0].addField(attributeField);
		}

		/*
		 * Fix for UAT 753
		 */
		DsSdpOriginField origField = dsSdpMsg.getOriginField();
		String oldSessVer = origField.getSessVersion();
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Existing session version="
					+ oldSessVer);
		}
		if (oldSessVer != null) {
			Long newSessionVer = Long.valueOf(oldSessVer);
			newSessionVer++;
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: New session version="
						+ newSessionVer);
			}
			origField.setSessVersion(Long.toString(newSessionVer));
		}

		baos = new ByteArrayOutputStream();
		dsSdpMsg.serialize(baos);
		MultipartBody multipartBody = new MultipartBody(baos.toByteArray(),
				sdpContent.getContentType());
		multipartBody.setContentDisposition(sdpContent.getContentDisposition());
		return multipartBody;
	}

	/**
	 * This method cleanup the resources used for correlation such as remove the
	 * entry from correlation map, stop correlation timer etc
	 * 
	 * @param appSession
	 */
	public static void cleanupCorrelationResources(SipApplicationSession appSession) {
		try {
			CallData callData = getCallData(appSession);
			TcapSession tcapSession = getTcapSession(appSession);
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

			Object ss7State = legData.get(LegDataAttributes.P_LEG_SS7_STATE);

			String origLegCallId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Inside cleanupCorrelationResources");
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Stop correlation timer if running");
			}
			stopTimer(appSession, PhConstants.CORRELATION_TIMER);
			String correlationId = (String) callData.get(CallDataAttribute.P_CORRELATION_ID);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Remove the correlation id " + correlationId);
			}

			// Removing entry which was added for ETC
			// AseUtils.removeCorrDialIdMapping(Integer.toString(callData
			// .getCorrelationId()));

			// changes done to support cleanup on FT;
			// as we are replicating servlet cCorrelationMap might be null
			Map<String, Object> cCorrelationMap = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap();
			if (cCorrelationMap != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Cleanup corr resources correlation map is not null");
				}
				cCorrelationMap.remove(correlationId);
			}

			// invalidate appsession and tcapseeion for handoff cases
			try {
				if (tcapSession != null
						&& ss7State.equals(InapCallStates.HANDOFF)
						|| ss7State.equals(AinCallStates.HANDOFF)
						|| ss7State.equals(CapV2CallStates.HANDOFF)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Invalidate the tcap sesssion here");
					}

					tcapSession.invalidate();
					/*
					 * Dont make tcapSession reference null as might be used in
					 * further processing If it is required to be set to null,
					 * make it when this method is specially called after
					 * receiving the INITE for handoff
					 */
					// Commented as doubted for some null call data related
					// exceptions in production
					// tcapSession = null;
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Invalidate the Appsession of Tcap Leg");
					}
					setAppSessionTimeout(appSession, 1, origLegCallId);
				}
			} catch (Exception ex) {
				logger.warn(origLegCallId + ":: Failed to invalidate tcap session. Error is " + ex.getMessage());
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Error invalidating tcap session.", ex);
				}
			}
		} catch (Exception e) {
			logger.warn("Error in correlation resource cleanup " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info("Error in cleanupCorrelationResources.", e);
			}
		}
	}

	/*
	 * This method returns the sip application session using reference save in tcap session.
	 * This method returns Tcap Notify AppSession
	 */
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
	 * This method calculate average time taken by PH in giving response to
	 * initial request
	 * 
	 * @param startTimeMilliSec
	 *            represents the long value of time in milliseconds
	 */

	public static void calculateAvgTime(int callsToCalcAvgResTime,
			long startTimeMilliSec, String callId) {
		try {
			if (callsToCalcAvgResTime == 1) {
				logger.error(callId
						+ ":: Response time [milli-sec] for this initial reqests is "
						+ (System.currentTimeMillis() - startTimeMilliSec));
			} else {
				long totalCount = 1;
				long totalTimeTaken = System.currentTimeMillis()
						- startTimeMilliSec;
				if (sInitialRequestCount
						.compareAndSet(callsToCalcAvgResTime, 1)) {
					sTotalTimeFirstReqResp.set(totalTimeTaken);
				} else {
					totalTimeTaken = sTotalTimeFirstReqResp
							.addAndGet(totalTimeTaken);
					totalCount = sInitialRequestCount.incrementAndGet();
				}
				if (totalCount % callsToCalcAvgResTime == 0) {
					logger.error(callId
							+ ":: Average response time [milli-sec] for "
							+ totalCount + " initial reqests is "
							+ (totalTimeTaken / totalCount));
				}
			}
		} catch (Exception ex) {
			logger.error(callId + ":: Error in caluclating avg time. "
					+ ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(callId + ":: Error in average time calculation", ex);
			}
		}
	}

	/**
	 * Utility method to return terminating ip address from call data
	 * 
	 * @param callData
	 *            represents an instance of CallData
	 * @return string value of outgoing ip address
	 */
	public static String getTerminatingIp(CallData callData, LegData legData) {
		String result = (String) legData
				.get(LegDataAttributes.P_REMOTE_IP);

		if (result == null || result.isEmpty()) {
			result = (String) callData
					.get(CallDataAttribute.P_ORIGIN_IP);
		}

		callData.set(CallDataAttribute.P_TERM_IP, result);

		return result;
	}

	/**
	 * Utility method to return terminating port from call data
	 * 
	 * @param callData
	 *            represents an instance of CallData
	 * @return int value of terminating port
	 */
	public static int getTerminatingPort(CallData callData, LegData legData) {
		Integer result = (Integer) legData
				.get(LegDataAttributes.P_REMOTE_PORT);

		if (result == null) {
			result = (Integer) callData
					.get(CallDataAttribute.P_ORIGIN_PORT);
		}

		callData.set(CallDataAttribute.P_TERM_PORT, result);

		return result;
	}

	/**
	 * This private method uses SBB Factory to retreive originating Media server
	 * controller.
	 * 
	 * @param appSession
	 *            represents an instance of GroupedMsSessionController
	 * @return groupedMsControllerOrig
	 */
	public static GroupedMsSessionController getMsController(
			SipApplicationSession appSession, LegData legData, String legId) {
		SBBFactory sbbFactory = (SBBFactory) appSession
				.getAttribute(PhConstants.SBB_FACTORY);
		CallData callData = SipProtocolUtil.getCallData(appSession);
		return (GroupedMsSessionController) sbbFactory
				.getSBB(GroupedMsSessionController.class.getName(),
						"groupedMsController"
								+ legId
								+ legData
								.get(LegDataAttributes.P_IVR_CONNECTION_COUNT),
								appSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
								.getServletContext());
	}

	/**
	 * Invalidate the Sip Application Session if FSM state machines are either
	 * in INIT or TERMINATED state.
	 * 
	 * @param appSession
	 *            represents SipApplicationSession
	 */
	public static void invalidateAppSession(SipApplicationSession appSession) {
		String origLegCallId = "0";
		try {
			CallData callData = SipProtocolUtil.getCallData(appSession);

			if (callData != null) {
				origLegCallId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			}
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Inside invalidateAppSession(). Id is " + appSession.getId());
			}
			boolean isReadyToInvalidate = true;
			Iterator<?> it = appSession.getSessions("SIP");
			SipSession sipSession;
			while (it.hasNext()) {
				try {
					sipSession = (SipSession) it.next();
					Object readyToInvalidate = sipSession.getAttribute(PhConstants.SESSION_READY_TO_INVALIDATE);
					if (!PhConstants.TRUE.equals(readyToInvalidate)) {
						isReadyToInvalidate = false;
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + "Session Id " + sipSession.getId() + "is not ready to invalidate.");
						}
						break;
					}
				} catch (Exception ex) {
					logger.warn(origLegCallId + ":: Error checking ready to invalidate flag on session, " + ex.getMessage());
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId + ":: Error checking ready to invalidate flag on session", ex);
					}
				}
			}
			if (isReadyToInvalidate && appSession.isValid()) {
				appSession.invalidate();
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Sip App Session invalidated");
				}
			}
		} catch (Exception ex) {
			logger.warn("Error invalidating app session " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info("Error in invalidateAppSession", ex);
			}
		}
	}

	/**
	 * This method set the SDP sent for future reference
	 * 
	 * @param sipMessage
	 *            represents the instance of SipServletMessage
	 * @param origLegCallId
	 *            represents the instance of String
	 * @return boolean representation of operation status
	 * @throws Exception
	 */
	public static boolean setSentSdp(SipServletMessage sipMessage,
			LegData legData, String origLegCallId) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside setSentSdp");
		}
		MultipartBody sdpContent = getSdpContent(sipMessage, origLegCallId);

		if (sdpContent != null && sdpContent.getContent() != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: LAST_SENT_SDP updated for session with callid = "
						+ sipMessage.getCallId() + " new SDP="
						+ new String(sdpContent.getContent()) + "\n Size="
						+ new String(sdpContent.getContent()).length());
			}
			// Update attribute for future reference
			legData.set(LegDataAttributes.NP_IS_SDP_SENT,
					PhConstants.TRUE);
			legData.set(LegDataAttributes.P_LAST_SENT_SDP,
					sdpContent);
			return true;
		} 
//		else {
//			if (logger.isDebugEnabled()) {
//				logger.debug(origLegCallId + ":: Did not sent sdp");
//			}
//			legData.set(LegDataAttributes.P_LAST_SENT_SDP,
//					null);
//		}
		return false;
	}

	/**
	 * This method forms the play specs for the announcement specified by the
	 * service.
	 * 
	 * @param callData
	 *            represents an instance of CallData
	 * @param annSpec
	 *            represents an instance of AnnSpec
	 * @param baseUriStr
	 *            represents an instance of String
	 * @param msPlaySpec
	 *            represents an instance of MsPlaySpec
	 * @param srvTypeSepInLang
	 *            represents an instance of String
	 * @throws Exception
	 */
	public static void formPlaySpec(CallData callData, LegData legData,
			String baseUriStr, MsPlaySpec msPlaySpec, String srvTypeSepInLang)
					throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside formPlaySpec");
		}
		AnnSpec annSpec = (AnnSpec) legData
				.get(LegDataAttributes.NP_ANN_SPEC);
		if (annSpec == null) {
			logger.error(origLegCallId
					+ ":: No announmcement spec specified by Service");
			throw new Exception(origLegCallId
					+ ":: Malformed Play Specification for Announcement");
		}
		ArrayList<PlayMessage> annList = (ArrayList<PlayMessage>) annSpec
				.getPlayMsgList();

		if ((annList == null) || (annList.isEmpty())) {
			logger.error(origLegCallId
					+ ":: No announmcement specified by Service");
			throw new Exception(origLegCallId
					+ ":: Malformed Ann List for Announcement");
		}
		Iterator<PlayMessage> annEntry = annList.iterator();
		// msPlaySpec.setBaseURL(baseUriStr + PhConstants.FILE_SEPARATOR);
		PlayMessage playMessage = null;
		String msgToPlay = null;
		MsVarAnnouncement var = null;
		while (annEntry.hasNext()) {
			playMessage = annEntry.next();
			AnnSpec.ANN_TYPE annType = playMessage.getAnnType();
			msgToPlay = playMessage.getMessageId();
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: annType is " + annType
						+ " msgToPlay is " + msgToPlay);
			}
			if (annType == AnnSpec.ANN_TYPE.ANN) {
				/*
				 * If we use com.baypackets.ase.msadaptor.msml.MsmlAdaptor
				 * adapter in media-server-config file then that does not
				 * support providing BaseURL and AnnouncementURI. So we have to
				 * concatenate these two and set in AnnouncementURI
				 */
				msPlaySpec.addAnnouncementURI(new java.net.URI(baseUriStr
						+ PhConstants.FILE_SEPARATOR + msgToPlay));
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Ann added");
				}
			} else if (annType == AnnSpec.ANN_TYPE.VAR) {
				var = new MsVarAnnouncement();
				var.setType(playMessage.getVarAnnType());
				var.setSubType(playMessage.getVarAnnSubType());
				var.setValue(msgToPlay);
				
				if(playMessage.getVarAnnLang() != null && !playMessage.getVarAnnLang().isEmpty()) {
					var.setLanguage(playMessage.getVarAnnLang());
				}else if(annSpec.getAnnLanguage() != null) {
					var.setLanguage(annSpec.getAnnLanguage());
				}
				msPlaySpec.addVariableAnnouncement(var);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: VAR Ann added");
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Ignore invalid Ann Type");
				}
			}
		}
		msPlaySpec.setClearDigitBuffer(annSpec.isClearDigitBuffer());
		/*
		 * Interval is the delay between two consecutive announcements in one
		 * playSpec
		 */
		msPlaySpec.setInterval(0);

		msPlaySpec.setBarge(annSpec.isBarge());
		//msPlaySpec.setPlayExit("src", "moml.exi, "play.amt play.end");

		if (annSpec.getAnnLanguage() != null) {
			msPlaySpec.setLanguage(annSpec.getAnnLanguage());
		} else {
			msPlaySpec.setLanguage(AnnSpec.ANN_LANG_EN_US);
		}

		if (srvTypeSepInLang != null && !srvTypeSepInLang.isEmpty()) {
			msPlaySpec.setLanguage(msPlaySpec.getLanguage() + srvTypeSepInLang
					+ "ngin");
		}

		if (annSpec.getAnnLength() > 0) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set announcement duration "
						+ (annSpec.getAnnLength() * 1000));
			}
			msPlaySpec.setDuration(annSpec.getAnnLength() * 1000);
		}

		if (annSpec.getAnnIteration() > 0) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set announcement iterations "
						+ annSpec.getAnnIteration());
			}
			msPlaySpec.setIterations(annSpec.getAnnIteration());
			msPlaySpec.setInfiniteAnn(annSpec.isInfiniteAnn());
		}
	}

	/**
	 * This method forms the collect specs for the play and collect scenario
	 * specified by the service.
	 * 
	 * @param callData
	 *            represents an instance of CallData
	 * @param annSpec
	 *            represents an instance of AnnSpec
	 * @param baseUriStr
	 *            represents an instance of String
	 * @param msCollectSpec
	 *            represents an instance of MsCollectSpec
	 * @throws Exception
	 */
	public static void formCollectSpec(CallData callData, LegData legData,
			String baseUriStr, MsCollectSpec msCollectSpec,
			GroupedMsSessionController msController) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside formCollectSpec");
		}
		AnnSpec annSpec = (AnnSpec) legData
				.get(LegDataAttributes.NP_ANN_SPEC);
		int firstDigTimer = annSpec.getFirstDigitTimer();
		int interDigTimer = annSpec.getInterDigitTimer();
		int extraDigitTimer = annSpec.getExtraDigitTimer();

		/*
		 * SBTM-UAT-1371 fix - As 0 is not supported on MS SBB, it has been
		 * decided to use 1 millisecond which is almost equivalent to 0. In
		 * annSpec fdt & idt are int with default value as 0 and if these
		 * parameters are not set by service, default values
		 * DEFAULT_PNC_FIRST_DIG_TIME_MS & DEFAULT_PNC_INTER_DIG_TIME_MS has to
		 * be used. That is why < 0 check is used here as work around to fix
		 * this UAT without impacting older functionality. For 0 FDT support,
		 * service has to set values less then 0 in it which will be set to 1
		 * millisecond by following logic.
		 */

		if (firstDigTimer < 0) {
			firstDigTimer = 1;
		} else {
			firstDigTimer = firstDigTimer > 0 ? (firstDigTimer * 1000)
					: PhConstants.DEFAULT_PNC_FIRST_DIG_TIME_MS;
		}

		if (interDigTimer < 0) {
			interDigTimer = 1;
		} else {
			interDigTimer = interDigTimer > 0 ? (interDigTimer * 1000)
					: PhConstants.DEFAULT_PNC_INTER_DIG_TIME_MS;
		}

		if (extraDigitTimer < 0) {
			extraDigitTimer = 1;
		} else {
			extraDigitTimer = extraDigitTimer > 0 ? (extraDigitTimer * 1000)
					: PhConstants.DEFAULT_PNC_INTER_DIG_TIME_MS;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Set firstDigTimer="
					+ firstDigTimer + ", interDigTimer=" + interDigTimer
					+ " extraDigitTimer " + extraDigitTimer);
		}

		msCollectSpec.setFirstDigitTimer(firstDigTimer);
		msCollectSpec.setInterDigitTimer(interDigTimer);
		msCollectSpec.setExtraDigitTimer(extraDigitTimer);

		if (annSpec.getEsacpeKey() != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set esacpeKey "
						+ annSpec.getEsacpeKey());
			}
			msCollectSpec.setEscapeKey(annSpec.getEsacpeKey());
		}

		// msCollectSpec.setClearDigitBuffer(annSpec.isClearDigitBuffer());
		msCollectSpec.setClearDigitBuffer(false);
		String termKey = annSpec.getTerminationKey();
		int minDigits = annSpec.getMinDigits();
		minDigits = minDigits > 0 ? minDigits
				: PhConstants.DEFAULT_PNC_MIN_DIGIT;
		int maxDigits = annSpec.getMaxDigits();
		maxDigits = maxDigits > 0 ? maxDigits
				: PhConstants.DEFAULT_PNC_MAX_DIGIT;
		/*
		 * In case of MSML based MS like XMS, term key is also part of user
		 * response returned by MS. So if termKey is present then maxLength
		 * should be set to allowed input length + termKey length
		 */
		if (termKey != null
				&& msController.getMediaServer().getAdaptorClassName()
				.contains(PhConstants.MSML_TYPE)) {
			maxDigits = maxDigits + 1;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Set minDigits=" + minDigits
					+ ", maxDigits=" + maxDigits + ", termKey=" + termKey
					+ ", clearDigitBuffer=" + annSpec.isClearDigitBuffer());
		}

		// bug 1674 changes commenting for now. will uncommen tlater on when it
		// will go in release currently ms not behaving well on cancel
		if (msController.getMediaServer().getAdaptorClassName()
				.contains(PhConstants.MSML_TYPE)) {

			msCollectSpec.clearPatternList();

			MsCollectPattern pattern = new MsCollectPattern();

			StringBuilder digitPattern = new StringBuilder();

			digitPattern.append(PhConstants.MIN_DIGITS).append(PhConstants.EQUALS_STR)
			.append(minDigits).append(PhConstants.SEMI_COLON_STR)
			.append(PhConstants.MAX_DIGITS).append(PhConstants.EQUALS_STR)
			.append(maxDigits);

			if (termKey != null) {
				digitPattern.append(PhConstants.SEMI_COLON_STR).append(PhConstants.RETURN_KEY)
				.append(PhConstants.EQUALS_STR).append(termKey);
			}

			if (annSpec.getEsacpeKey() != null) {
				digitPattern.append(PhConstants.SEMI_COLON_STR)
				.append(PhConstants.CANCEL_KEY).append(PhConstants.EQUALS_STR)
				.append(annSpec.getEsacpeKey());
			}

			pattern.setDigits(digitPattern.toString());

			pattern.setSendTag("source", "app.done", "dtmf.digits dtmf.end");
			msCollectSpec.addToPatternList(pattern);

		}else{		
			msCollectSpec.applyPattern(minDigits, maxDigits, termKey);
		}
		msCollectSpec.addDefaultNoInput_send();
		msCollectSpec.addDefaultNoMatch_send();

		msCollectSpec.setStarttimer(true);
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
	 * This method is used to get cleanupp time for correlated orig party when
	 * ETC is used for creating sip connection by inap
	 * 
	 * @param appSession
	 * @return
	 */
	public static long getCorrelatedOrigCleanupTime(
			SipApplicationSession appSession) {

		long cleanupTime = 10000;
		CallData callData = getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		try {

			LegData LegData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);

			Object cleanupTimeObj = LegData
					.get(LegDataAttributes.P_CORRELATED_ORIG_LEG_CLEANUP_TIME);

			if (cleanupTimeObj != null) {
				cleanupTime = (Long) cleanupTimeObj;
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside getCorrelatedOrigCleanupTime ");
			}

		} catch (Exception ex) {
			logger.error(origLegCallId
					+ ":: Failed to getCorrelatedOrigCleanupTime.", ex);
		}
		return cleanupTime;
	}

	/**
	 * This method is used to add custom headers in a sip request
	 * 
	 * @param callData
	 * @param legData
	 * @param request
	 */
	public static void addCustomHeaders(CallData callData, LegData legData,
			SipServletRequest request) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside addCustomHeaders ");
		}
		if (legData
				.get(LegDataAttributes.NP_CUSTOM_HEADERS_MAP) != null) {
			@SuppressWarnings("unchecked")
			Map<String, String> customHeaders = (Map<String, String>) legData
			.get(LegDataAttributes.NP_CUSTOM_HEADERS_MAP);

			java.util.Set<Entry<String, String>> enteries = customHeaders
					.entrySet();
			Iterator<Entry<String, String>> iterator = enteries.iterator();

			while (iterator.hasNext()) {

				Entry<String, String> entry = iterator.next();
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "Add custom header  Name: "
							+ entry.getKey() + " Value :" + entry.getValue());
				}
				request.addHeader(entry.getKey(), entry.getValue());
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Leaving addCustomHeaders ");
		}
	}

	/**
	 * This method is used to copy headers from incoming sip message to outgoing
	 * sipmessage custom headers in a sip request
	 * 
	 * @param callData
	 * @param legData
	 * @param request
	 */
	public static void copyHeaders(CallData callData,
			SipServletMessage incoming, SipServletMessage outgoing) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside copyHeaders() ");
		}
		if (incoming != null && outgoing != null) {

			Iterator<String> hdrNamesitr = incoming.getHeaderNames();

			while (hdrNamesitr.hasNext()) {

				String hdrName = hdrNamesitr.next();
				if(logger.isDebugEnabled()) {
					logger.debug("processing header : " + hdrName);
				}
				if (!hdrName.equalsIgnoreCase(PhConstants.TO_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.FROM_HEADER)
						&& !hdrName
						.equalsIgnoreCase(PhConstants.CONTACT_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.ROUTE_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.VIA_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.CSEQ_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.PATH_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.RACK_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.R_SEQ_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.RECORD_ROUTE)
						&& !hdrName.equalsIgnoreCase(PhConstants.CALLID_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.CONTENT_TYPE)
						&& !hdrName
						.equalsIgnoreCase(PhConstants.CONTENT_LENGTH_HEADER)
						&& !hdrName.equalsIgnoreCase(PhConstants.REQUIRE_HEADER)) {

					ListIterator<String> hdrValueIterator = incoming.getHeaders(hdrName);
					while(hdrValueIterator.hasNext()) {
						String hdrValue = hdrValueIterator.next();
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + "Add header Name: "
									+ hdrName + " Value :" + hdrValue);
						}

						outgoing.addHeader(hdrName, hdrValue);
					}

				}else if(hdrName.equalsIgnoreCase(PhConstants.REQUIRE_HEADER)) {
					/**
					 * Value of header will be modifies if header contains value 
					 * other than 100Rel like precondition and to skip 100Rel
					 * as 100Rel will be added during sending the response reliably. 
					 */
					String hdrValue = incoming.getHeader(hdrName);
					if(logger.isDebugEnabled()) {
						logger.debug("received value from incoming : " + hdrValue);
					}

					if(hdrValue.contains("100rel") && hdrValue.contains(",")) {
						String[] values = hdrValue.split(",");
						StringBuilder sb = new StringBuilder();

						for(String temp : values) {
							if(!temp.trim().equalsIgnoreCase("100rel")) {
								if(temp.equals(values[values.length-1])){
									sb.append(temp);
								}
								else {
									sb.append(temp).append(",");
								}
							}
						}
						String value = sb.toString();
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + "Add header Name for require: "
									+ hdrName + " Value :" + value);
						}
						outgoing.addHeader(hdrName, value);

					}
				}
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Leaving copyHeaders() ");
		}
	}

	/**
	 * This method is used to check if calling party override is applicable if
	 * yes then apply it
	 * 
	 * @param termRequest
	 * @param termlegData
	 * @throws ServletParseException
	 */
	protected static void checkAndApplyCallingPartyOverride(
			SipServletRequest origRequest, String origLegCallId,
			LegData origLegData, SipURI fromUri, LegData termlegData)
					throws ServletParseException {

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::checkAndApplyCallingPartyOverride for From Uri : "+ fromUri);
		}

		String isOverride = (String) termlegData
				.get(LegDataAttributes.NP_CALLING_PARTY_OVERRIDE);


		PhoneNumber chargeInfo = (PhoneNumber) origLegData
				.get(LegDataAttributes.P_CHARGE_INFO);

		Address paiHeader = origRequest
				.getAddressHeader(PhConstants.PAI_HEADER);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "::checkAndApplyCallingPartyOverride check service flag "
					+ isOverride);
		}

		if (PhConstants.TRUE.equals(isOverride)) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "::checkAndApplyCallingPartyOverride service override flag is ON check for PAI header availability "
						+ paiHeader);
			}

			if (fromUri.getUser().toLowerCase().startsWith(PhConstants.ANONYMOUS)
					|| fromUri.getUser().toLowerCase().startsWith(
							PhConstants.RESTRICTED)) {

				if (paiHeader != null) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: PAI header is present !!" + paiHeader);
					}
					URI paiUri = paiHeader.getURI();

					if (paiUri.isSipURI()) {
						SipURI paiSip = (SipURI) paiUri;

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::Overriding calling party in From header from PAI header sip user !!"
									+ paiSip.getUser());
						}
						String updatedUser=updatePhoneNumberInFrom(origLegCallId, new StringBuffer(fromUri.getUser()), paiSip.getUser());
						fromUri.setUser(updatedUser);

					} else {

						TelURL paiTelUrl=((TelURL) paiUri);

						//						Iterator paiParamNames=paiTelUrl.getParameterNames();
						//						
						//						while (paiParamNames.hasNext()) {
						//							String paramName = (String) paiParamNames.next();
						//							tmp = tmp + PhConstants.SEMI_COLON_STR + paramName
						//									+ PhConstants.EQUALS_STR
						//									+ paiTelUrl.getParameter(paramName);
						//						}


						String updatedUser=updatePhoneNumberInFrom(origLegCallId, new StringBuffer(fromUri.getUser()), paiTelUrl.getPhoneNumber());

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::Overriding calling party in From header from PAI header tel user !!"
									+ updatedUser);
						}

						if (paiUri.toString().indexOf("+") != -1) {
							fromUri.setUser("+" + updatedUser);
						} else {
							fromUri.setUser(updatedUser);
						}

					}

				} else if (chargeInfo != null) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Overriding calling party in From header from P-Charge-Info header !!"
								+ chargeInfo);
					}
					fromUri.setUser(updatePhoneNumberInFrom(origLegCallId, new StringBuffer(fromUri.getUser()), chargeInfo.getAddress()));

				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Calling party from PAI and charge-info is not avaialable so can not override calling party !!");
					}
				}

			} else {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::No Need to apply calling party override  anonymous/restricted not present in from !!");
				}
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "::calling party override is not applied !!!");
			}
		}

	}


	/**
	 * This method is used to update user part of PAI/P-charge-Info headers in case of calling
	 * party override is enabled
	 * @param origLegCallId
	 * @param fromUser
	 * @param paiPciUser
	 * @return
	 */
	private static String updatePhoneNumberInFrom(String origLegCallId,
			StringBuffer fromUser, String paiPciUser) {

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId
					+ "::updatePhoneNumberInFrom Entering fromUser :>  "
					+ fromUser +" paiPciUser"+ paiPciUser);
		}

		String paipciUser =paiPciUser;

		if(paiPciUser.indexOf(";")!=-1){
			paipciUser = paiPciUser.substring(0, paiPciUser.indexOf(";"));
		}

		int index = fromUser.indexOf(";");
		if (index != -1) {
			fromUser.replace(0, index, paipciUser);
		} else {
			fromUser = new StringBuffer(paipciUser);
		}
		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId
					+ "::updatePhoneNumberInFrom updated  From user :>  "
					+ fromUser);
		}

		return fromUser.toString();

	}

	/**
	 * This method is used to check if the call is transfered or not
	 * 
	 * @param sipRequest
	 * @param origLegCallId
	 * @return
	 */
	static boolean isTransferedCall(SipServletRequest sipRequest,
			String origLegCallId, Map m_ReferDialogMap) {

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId
					+ ":: isTransferedCall() check if transfered call dialog id available For Request uri "
					+ sipRequest.getRequestURI().toString());
		}

		String replaces = sipRequest.getHeader(PhConstants.REPLACES_HEADER);

		String transferorDlgId = (String) m_ReferDialogMap.get(sipRequest
				.getRequestURI().toString());

		if (transferorDlgId != null || replaces != null) {

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ "::isTransferedCall()  INVITE for REFER received . it seems a call transfer invite transfered dialog id is "
						+ transferorDlgId + " and Replaces is " + replaces);
			}

			return true;

		}

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId
					+ ":: isTransferedCall() --> its not a transfered call");
		}
		return false;
	}

	/**
	 * This method is used to find out the transfered call session
	 * 
	 * @param sipRequest
	 * @param origLegCallId
	 * @param legData
	 * @return
	 */
	static SipSession getTransferedSession(SipServletRequest sipRequest,
			String origLegCallId, Map m_ReferDialogMap,
			AseDialogManager dialogManager, CallData callData) {

		SipSession transferedSession = null;

		String replaces = sipRequest.getHeader(PhConstants.REPLACES_HEADER);

		String transferorDlgId = (String) m_ReferDialogMap.get(sipRequest
				.getRequestURI().toString());

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId
					+ ":: getTransferedSession() for transfered dialog Id from dialogmap-> "
					+ transferorDlgId + " For Request uri "
					+ sipRequest.getRequestURI().toString());
		}

		if (transferorDlgId != null || replaces != null) {

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Call transfer  transfered dialog id found is "
						+ transferorDlgId);
			}

			if (replaces == null) {
				/*
				 * Blind transfer case
				 */
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: It seems a blind call transfer invite as replaces header not there in transfered invite");
				}

				/**
				 * Attended transfer case
				 */
				// transferorDlgId = (String) m_ReferDialogMap.get(sipRequest
				// .getRequestURI().toString());

			}
			//
			// else {
			// /**
			// * We can here remove code of saving dialog id with replaces and
			// reading it back because it can be directly fetched on basis of
			// replaces
			// * from dialog manager as in else case below . will think about it
			// because substring work will increase in that case
			// * but putting in hashmap means more read through hashmap thats
			// also costly
			// */
			// transferorDlgId = (String) m_ReferDialogMap.get(replaces);
			// }

			if (transferorDlgId == null && replaces != null) {

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: It seems a Consutant call transfer invite . Replaces header is -> "
							+ replaces);
				}
				/**
				 * Consulted transfer case when no refer is received directly
				 * invite is received with replaces header replaces comes as
				 * e.g. 425928@bobster.example.org;to-tag=7743;from-tag=6472
				 */

				String[] transferDlg = replaces.split(";");

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Call ID:  "
							+ transferDlg[0] + " from-tag= : "
							+ transferDlg[2].substring(9) + " to-tag= : "
							+ transferDlg[1].substring(7));
				}
				transferedSession = dialogManager.getSession(transferDlg[0],
						transferDlg[2].substring(9),
						transferDlg[1].substring(7));

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Trasnfered session from replaces header is "
							+ transferedSession);
				}

				callData.set(
						CallDataAttribute.NP_TRANSFERED_SESSION,
						transferedSession);
				callData.set(
						CallDataAttribute.NP_IS_CONSULTED_TRANSFER,
						PhConstants.TRUE);
				callData.set(
						CallDataAttribute.NP_TRANSFERED_PENDING_INVITE_REQ,
						sipRequest);
			} else {



				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: It seems a blind/attended call transfer invite !!!");
				}

				/**
				 * Cleanup dialog from map
				 */
				m_ReferDialogMap.remove(sipRequest.getRequestURI().toString());

				// dialog id is ->from tag,to tag, callid
				String[] transferDlg = transferorDlgId.split(",");
				transferedSession = dialogManager.getSession(transferDlg[2],
						transferDlg[0], transferDlg[1]);

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: trasnfered session from refer-to mapping is  "
							+ transferedSession);
				}

				callData.set(
						CallDataAttribute.NP_TRANSFERED_SESSION,
						transferedSession);
				callData.set(
						CallDataAttribute.NP_TRANSFERED_PENDING_INVITE_REQ,
						sipRequest);
				callData.set(
						CallDataAttribute.NP_IS_CONSULTED_TRANSFER,
						PhConstants.FALSE);
			}

		}

		if (transferorDlgId != null) {

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Remove refer-to mapping from m_ReferDialogMap");
			}
			m_ReferDialogMap.remove(sipRequest.getRequestURI().toString());
		}
		return transferedSession;
	}

	/**
	 * This method is used to insert session for refer to request n dialog map
	 * 
	 * @param referRequest
	 * @param origLegCallId
	 * @param m_ReferDialogMap
	 * @param dialogManager
	 * @throws ServletParseException
	 */
	static String updateDialogMap(SipServletRequest referRequest,
			String origLegCallId, Map m_ReferDialogMap,
			AseDialogManager dialogManager) throws ServletParseException {

		Address referToHdr = referRequest
				.getAddressHeader(PhConstants.REFER_TO_HEADER);

		/*
		 * example Refer-to header Refer-To:
		 * <sip:dave@denver.example.org?Replaces=12345%40192.168 .118.3%3B
		 * to-tag%3D12345%3Bfrom-tag%3D5FFE-3994>
		 */

		// if(replacesHdr!=null){
		// m_ReferDialogMap.put(replacesHdr,
		// dialogManager.getDialogId((AseSipSession) sipRequest.getSession()));
		// }else

		String uri = ((SipURI) referToHdr.getURI()).toString();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: insertIntoDialogMap() Refer-To header found is   "
					+ uri);
		}

		String[] headers = uri.split("Replaces=");

		String referToHdrStr = null;
		String replacesHdrStr = null;

		if (headers.length > 1) {
			referToHdrStr = headers[0].substring(0, headers[0].length() - 1);
			replacesHdrStr = headers[1];

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Extracted Replaces Header as  " + replacesHdrStr);
			}

		} else {
			referToHdrStr = headers[0];

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: No Replaces Header found in REFER");
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Refer-To header extracted is   "
					+ referToHdrStr);
		}

		String dialogId = dialogManager
				.getDialogId((AseSipSession) referRequest.getSession());

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Putting refer to " + referToHdrStr
					+ " in dialog map For DialogId " + dialogId);
		}

		m_ReferDialogMap.put(referToHdrStr, dialogId);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: insertIntoDialogMap() Leaving");
		}
		return referToHdrStr;

	}

	/**
	 * This method is used to parse PAI from contact uri in 300 MC response from
	 * EQS
	 * 
	 * @param origLegCallId
	 * @param contactUri
	 * @return
	 */
	static String[] parsePAIFromMCContact(String origLegCallId,
			String contactUri) {

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: parsePAIFromMCContact() Entering with contact address :  "
					+ contactUri);
		}

		if (contactUri.indexOf("?") != -1) {

			String[] headers = contactUri.split(PhConstants.PAI_HEADER + "=");

			String contactHdrStr = null;
			String paiHdrStr = null;

			if (headers.length > 1) {
				contactHdrStr = headers[0]
						.substring(0, headers[0].length() - 1);
				paiHdrStr = headers[1];
			} else {
				contactHdrStr = headers[0];
				return null;

			}

			headers[0] = contactHdrStr;

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: parsePAIFromMCContact() Contact is " + headers[0]);
			}

			if (paiHdrStr != null) {
				try {

					paiHdrStr = paiHdrStr.replace(" ", "aeiou");
					String paiAfterDecoding = URLDecoder
							.decode(paiHdrStr, "UTF-8").replace(" ", "+")
							.replace("aeiou", " ");

					headers[1] = paiAfterDecoding;
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: parsePAIFromMCContact() PAI "
								+ headers[1]);
					}
				} catch (UnsupportedEncodingException e) {
					logger.error(" PAI parsing exception could not parse !!!"
							+ e);
				}
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: parsePAIFromMCContact() Leaving with contact parsed ");
			}
			return headers;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: parsePAIFromMCContact() Leaving");
		}

		return null;
	}

	/**
	 * This method is used to parse param from user portion of any sip Uri
	 * 
	 * @param origLegCallId
	 * @param user
	 * @param paramName
	 * @return
	 */
	static String parseParamFromUserPortion(String origLegCallId, String user,
			String paramName) {
		// String user = ((SipURI) inviteRequest.getRequestURI()).getUser();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: parseParamFromUserPortion Entering with  user "
					+ user + " Extract param " + paramName);
		}
		String paramValue = null;

		if (user != null) {
			int paramIndex = user.indexOf(paramName + "=");

			if (paramIndex != -1) {
				paramValue = user
						.substring(paramIndex + paramName.length() + 1);
			}
			
            if(paramValue!=null && paramValue.indexOf(";")!=-1){
				
				paramValue = paramValue
						.substring(0,paramValue.indexOf(";"));
			}


			if (paramValue != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Parameter extracted from the User portion is after removing params after ;"
							+ paramValue);
				}
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: parseParamFromUserPortion returing with  value "
					+ paramValue);
		}
		return paramValue;
	}

	/**
	 * This method is used to check if the specific timer is currently running
	 * or not
	 * 
	 * @param appSession
	 * @param timerName
	 * @return
	 */
	static boolean isTimerRunning(SipApplicationSession appSession,
			String timerName) {
		return appSession.getAttribute(timerName) != null ? true : false;
	}

	static PhoneNumber getPhoneNumberFromChargeInfoHeader(String origLegCallId, Address chargeInfoHeader) {
		PhoneNumber phoneNumber = null;
		if (chargeInfoHeader != null) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: charge info header is: " + chargeInfoHeader);
			}
			phoneNumber = new PhoneNumber();
			phoneNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
			int noaIntValue = PhoneNumber.NOA_UNKNOWN;

			String uriScheme = chargeInfoHeader.getURI().getScheme();
			if (uriScheme.equals(PhConstants.URI_SCHEME_SIP)) {
				String user = ((SipURI) chargeInfoHeader.getURI()).getUser();
				if (user != null) {
					String[] userparts = user.split(";");
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId + ":: set charge info as   "
								+ userparts[0]);
					}
					String noa = null;
					String npi = null;
					for (String userpart : userparts) {
						boolean noaPresent = userpart.startsWith(PhoneNumber.NOA_PARAM_KEY);
						if (noaPresent) {
							//Get the data after "noa=". +1 done because of last "="
							noa = userpart.substring(PhoneNumber.NOA_PARAM_KEY.length() + 1);
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: NOA  parameter from  Charge-info header user part is "
										+ noa);
							}
							continue;
						}
						boolean npiPresent = userpart.startsWith(PhoneNumber.NPI_PARAM_KEY);
						if (npiPresent) {
							//Get the data after "npi=". +1 done because of last "="
							npi = userpart.substring(PhoneNumber.NPI_PARAM_KEY.length() + 1);
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: NPI  parameter from  Charge-info header user part is "
										+ npi);
							}
							continue;
						}
					}
					phoneNumber.setAddress(userparts[0]);
					if (null != noa && !noa.isEmpty()) {
						
						if(noa.equals("calling_national")){
							phoneNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
						}
						else if(noa.equals("calling_international")){
							phoneNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
						}else{
						   noaIntValue = Integer.parseInt(noa);
						   phoneNumber.setNatureOfAddress(noaIntValue);
						}
					} else {
						phoneNumber
						.setNatureOfAddress(userparts[0] != null
						&& !userparts[0].isEmpty()
						&& userparts[0].startsWith("+") ? PhoneNumber.NOA_INTERNATIONAL
								: PhoneNumber.NOA_NATIONAL);
					}
					phoneNumber.setPresentationIndicator(PhoneNumber.getIntegerForPresentationIndicator(npi));
				}
			} else if (uriScheme.equals(PhConstants.URI_SCHEME_TEL)) {
				TelURL telURLObject = (TelURL) chargeInfoHeader.getURI();
				String telPhoneNumber = telURLObject.getPhoneNumber();
				String noa = telURLObject.getParameter(PhoneNumber.NOA_PARAM_KEY);
				String npi = telURLObject.getParameter(PhoneNumber.NPI_PARAM_KEY);

				phoneNumber.setAddress(telPhoneNumber);
				phoneNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);

				if (null != noa && !noa.isEmpty()) {
					noaIntValue = Integer.parseInt(noa);
					phoneNumber.setNatureOfAddress(noaIntValue);
				}

				if(telURLObject.toString().contains("+")){
					phoneNumber.setAddress("+"+telPhoneNumber);
					phoneNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
				}

				phoneNumber.setPresentationIndicator(PhoneNumber.getIntegerForPresentationIndicator(npi));
			} else {
				logger.error("Unknown URI scheme for chargeInfoHeader: " + chargeInfoHeader);
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: incoming charge info header not received");
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: returning with charge info number details: " + phoneNumber);
		}
		return phoneNumber;
	}

	/**
	 * This method is used to set loop back address or origin ip and port in calldata
	 * @param callData
	 * @param sipRequest
	 * @throws ServletParseException 
	 */
	public static void setLoopBackAddress(String origLegCallId,CallData callData, SipServletRequest sipRequest) throws ServletParseException{
		String loopbackVia= SipProtocolConfig.getConfigData(SipProtocolConfig.LOOPBACK_VIA_HEADER);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::setLoopBackAddress:  LOOPBACK_VIA_HEADER configuration is " + loopbackVia);
		}

		if (loopbackVia != null
				&& loopbackVia.equals(PhConstants.DEFAULT)) {
			callData.set(CallDataAttribute.P_ORIGIN_IP,
					sipRequest.getRemoteAddr());
			callData.set(CallDataAttribute.P_ORIGIN_PORT,
					sipRequest.getRemotePort());
		} else {
			ListIterator<String> viaheaders = sipRequest
					.getHeaders(PhConstants.VIA_HEADER);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::setLoopBackAddress:  viaheaders is " + viaheaders);
			}
			int second=0;
			while (viaheaders.hasNext()) {	
				String viaheader = viaheaders.next();
				if(second==1){	
					// Via: SIP/2.0/[transport] 10.32.8.153:5070;branch=[branch]  
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + "::setLoopBackAddress:  second Via header is " + viaheader);
					}
					String[] viaheaderArr=viaheader.split(" ");				
					String[] ipport=viaheaderArr[1].split(";");
					String[] ipportArr=ipport[0].split(":");
					callData.set(CallDataAttribute.P_ORIGIN_IP,
							ipportArr[0]);
					callData.set(CallDataAttribute.P_ORIGIN_PORT,
							Integer.parseInt(ipportArr[1]));
					break;
				}
				++second;
			}

			if(callData.get(CallDataAttribute.P_ORIGIN_IP)==null){

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::setLoopBackAddress:  set first via as loopback origin address no second via available ");
				}
				callData.set(CallDataAttribute.P_ORIGIN_IP,
						sipRequest.getRemoteAddr());
				callData.set(CallDataAttribute.P_ORIGIN_PORT,
						sipRequest.getRemotePort());
			}

		}

	}

	/**
	 * This method is used to set destination number in legdata
	 * @param origLegCallId
	 * @param legData
	 * @param destUri
	 */
	protected static void  setDestinationNumber(String origLegCallId,CallData callData,LegData legData , URI destUri){

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::setDestinationNumber:   parse user from destination uri "+destUri);
		}

		String toUser = "";

		if (destUri != null) {			
			if (destUri != null) {
				if (destUri.isSipURI()) {
					String tmp = ((SipURI) destUri).getUser();
					tmp = (tmp == null) ? "" : tmp;
					toUser = tmp.split(";")[0];
				} else {
					String tmp = ((TelURL) destUri).getPhoneNumber();
					tmp = (tmp == null) ? "" : tmp;
					toUser = tmp.split(";")[0];

					if (((TelURL) destUri).isGlobal()) {
						toUser = "+" + toUser;
					}

				}
			}
		}

		PhoneNumber calledNumber = new PhoneNumber();
		calledNumber.setAddress(toUser);
		calledNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
		calledNumber.setNatureOfAddress(toUser != null && !toUser.isEmpty()
				&& toUser.startsWith("+") ? PhoneNumber.NOA_INTERNATIONAL
				: PhoneNumber.NOA_NATIONAL);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Called number detail is "
					+ calledNumber);
		}

		if ((callData.get(CallDataAttribute.P_CORRELATION_ID) == null)) {
			legData.set(LegDataAttributes.P_CALLED_PARTY,
					calledNumber);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Set called number as dialed digits");
		}
		legData.set(LegDataAttributes.P_DIALED_DIGITS,
				calledNumber);
	}


	/**
	 * This method is used to check if the sdp is hold sdp or not
	 * @param sdp
	 * @return
	 * @throws ProcessMessageException
	 */
	public static boolean isHoldSdp(Object sdp) throws ProcessMessageException {

		try {
			// updating SDP to make to put party-A on hold

			if (logger.isDebugEnabled()){
				logger.debug("isHoldSdp  sdp is " + sdp);
			}
			DsSdpMsg dsSdpMsg = null;

			if (sdp instanceof String) {
				if (logger.isDebugEnabled()){
					logger.debug("SDP is String=" + sdp);
				}
				dsSdpMsg = new DsSdpMsg((String) sdp);
			} else if (sdp instanceof byte[]) {
				if (logger.isDebugEnabled()){
					logger.debug("SDP is byte[]=" + new String((byte[]) sdp));
				}

				StringTokenizer stt = new StringTokenizer(new String(
						(byte[]) sdp), "\n");

				java.util.List<String> list = new ArrayList<String>();

				int tI = 0;
				int cI = 0;
				while (stt.hasMoreElements()) {

					String e = (String) stt.nextElement();
					list.add(e);

					if (e.startsWith("t=") && tI == 0) {
						tI = list.size();
					} else if (e.startsWith("c=") && cI == 0) {
						cI = list.size();
					}
				}

				if (tI < cI) {
					String tFiled = list.get(tI - 1);
					String cFiled = list.get(cI - 1);

					list.remove(tFiled);
					list.remove(cFiled);
					list.add(tI - 1, cFiled);
					list.add(tI, tFiled);
				}

				String mSDP = "";

				for (int i = 0; i < list.size(); i++) {

					mSDP = mSDP + list.get(i) + "\n";
				}

				byte[] sd = mSDP.getBytes();
				if (logger.isDebugEnabled()){
					logger.debug("SDP is new byte[]=" + new String((byte[]) sd));
				}
				dsSdpMsg = new DsSdpMsg(sd);
				// dsSdpMsg = new DsSdpMsg((byte[])sdp);
			} else if (sdp instanceof Multipart) {
				ByteArrayOutputStream bos = new ByteArrayOutputStream();
				try {
					((Multipart) sdp).writeTo(bos);
					if (logger.isDebugEnabled()){
						logger.debug("SDP is multi part=" + bos);
					}
					dsSdpMsg = new DsSdpMsg(bos.toByteArray());
				} catch (Exception exp) {
					throw new ProcessMessageException(exp.getMessage());
				}
			} else {
				throw new ProcessMessageException(
						"Error: Unknown Content Type of SDP");
			}

			// dsSdpMsg.removeField('t'); // reeta
			// setting IP-Address in c line to 0.0.0.0
			com.dynamicsoft.DsLibs.DsSdpObject.DsSdpConnectionField connField = dsSdpMsg
					.getConnectionField();
			if (logger.isDebugEnabled()){
				logger.debug("<SBB> Connection field in SDP message is "
						+ connField.getAddr());
			}
			if (connField != null
					&& connField.getAddr().equals(Constants.INACTIVE_IP)) {
				return true;
			}

			DsSdpMediaDescription[] mediaFields = dsSdpMsg
					.getMediaDescriptionList();
			for (int i = 0; mediaFields != null && i < mediaFields.length; i++) {
				connField = mediaFields[i] != null ? (DsSdpConnectionField) mediaFields[i]
						.getField(DsSdpField.CONNECTION_FIELD_INDICATOR) : null;
						if (connField != null
								&& connField.getAddr().equals(Constants.INACTIVE_IP)) {
							return true;
						}
			}

			String value = Constants.INACTIVE;
			// adddig attribute a = INACTIVE to all media descriptions
			mediaFields = dsSdpMsg.getMediaDescriptionList();
			for (int i = 0; mediaFields != null && i < mediaFields.length; i++) {
				if (mediaFields[i] != null) {
					DsSdpAttributeField[]  sdpFields=mediaFields[i].getAttributeFields();

					for (int j = 0; sdpFields != null && j < sdpFields.length; j++) {

						DsSdpAttributeField dssdpf= sdpFields[j];
						if(dssdpf.getValue()!=null &&dssdpf.getValue().equals(value)){
							return true;
						}
					}
				}
			}

		} catch (Exception e) {
			throw new ProcessMessageException(e.getMessage(), e);
		}
		return false;
	}


	/**
	 * this method is used to maupulate sdp for web rtc
	 * @param origInvite
	 * @param outgoingInvite
	 * @throws IOException
	 */
	public static void updateWebRtcSdp(SipServletRequest origInvite, SipServletRequest outgoingInvite) throws IOException{

		Object sdpObj = origInvite.getContent();
		if (sdpObj != null) {

			//webrtc sdp manipulation
			if (origInvite.getHeader("User-Agent") != null && origInvite.getHeader("User-Agent").startsWith("webrtc2sip")) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH] Processing for webrtc2sip headers removing");
				}
				byte[] sdpBytes = (byte[]) sdpObj;
				String content = new String(sdpBytes);
				String[] linesSDP = content.split("\n");
				StringBuffer newSDP = new StringBuffer();
				int lines = linesSDP.length;
				int count = 1;
				for (String s : linesSDP) {
					if (!s.startsWith("a=candidate")) {
						newSDP.append(s);
						if (count < lines) {
							newSDP.append("\n");
						}
					}
					count++;
					lines++;
				}
				outgoingInvite.setContent(newSDP.toString(), origInvite.getContentType());
			} else {
				outgoingInvite.setContent(sdpObj, origInvite.getContentType());
			}
		}
	}


	/**
	 * This method is used to get peerReplaces attribute that need to sent in Refer-To header for consultative transfer
	 * @param origLegCallId
	 * @param incomingReferUri
	 * @param peerdialogId
	 * @return
	 * @throws ServletParseException
	 * @throws UnsupportedEncodingException 
	 */
	public static String getPeerReplacesAttribute(String origLegCallId,String incomingReferUri,String peerdialogId) throws ServletParseException, UnsupportedEncodingException{


		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::getPeerReplacesAttribute:   incomingRefer  "+incomingReferUri+" peerdialogId: "+peerdialogId);
		}
		String peerReferToHdr = null;

		String[] headers = incomingReferUri.split(PhConstants.REPLACES+PhConstants.EQUALS_STR);

		String referToHdrStr = null;
		if (headers.length > 1) {
			referToHdrStr = headers[0].substring(0,
					headers[0].length() - 1);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: referToHdrStr  "
						+ referToHdrStr );
			}
		}

		peerReferToHdr=referToHdrStr;

		//		if (peerdialogId != null) {
		//		String[] dialogId = peerdialogId.split(",");

		// <sip:dave@denver.example.org?Replaces=12345%40192.168
		// .118.3%3Bto-tag%3D12345%3Bfrom-tag%3D5FFE-3994>

		//			if (dialogId.length == 3) {
		//				peerdialogId = dialogId[2] + "%3Bto-tag%3D" + dialogId[1]
		//						+ "%3Bfrom-tag%3D" + dialogId[0];
		//				peerdialogId = peerdialogId.replaceAll(";", "%3B")
		//						.replaceAll("=", "%3D").replaceAll("@", "%40");

		peerdialogId = URLEncoder.encode(headers[1], "UTF-8");
		//     peerdialogId = headers[1].replaceAll(";", "%3B").replaceAll("=", "%3D").replaceAll("@", "%40");
		peerReferToHdr = "<"+peerReferToHdr + "?" + PhConstants.REPLACES
				+ PhConstants.EQUALS_STR + peerdialogId+">";

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "::getPeerReplacesAttribute:   peerReferToHdr  "
					+ peerReferToHdr);
		}
		//		}

		//			return PhUtilityServices.getInstance().getSipFactory()
		//					.createAddress(peerReferToHdr);
		return peerReferToHdr;
		//	}
		//return null;

	}

//	public static void main(String[] args) throws URISyntaxException, IOException {
//
//		try {
//
//			String peerId=getPeerReplacesAttribute("1234", "sip:146@ankit.dev1.com;user=phone?Replaces=70a7337e-483d18dd-fb09be34@10.32.7.16;to-tag=ds70277c;from-tag=D3B36CBD9-8E73DD10", null);
//			System.out.println(" peer rplaces is "+peerId);
//
//			peerId.getBytes();
//
//			String abc=  new String(peerId.getBytes());
//
//			System.out.println(" peer refer  is "+abc);
//
//		} catch (ServletParseException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//
//	}


	/**
	 * This method is used to add custom headers provided in invite attributes
	 * @param origLegCallId
	 * @param customHeaders
	 * @param request
	 */
	public static void addInviteAttributesCustomHeaders(String origLegCallId,
			InviteAttributes inviteAttr, SipServletRequest request) {

		if (inviteAttr!=null && inviteAttr.getCustomHeaders() != null) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "addInviteAttributesCustomHeaders............"+inviteAttr.getCustomHeaders());
			}
			Map<String, String> customHeaders = inviteAttr.getCustomHeaders();
			java.util.Set<Entry<String, String>> enteries = customHeaders
					.entrySet();
			Iterator<Entry<String, String>> iterator = enteries.iterator();

			while (iterator.hasNext()) {

				Entry<String, String> entry = iterator.next();
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "Add custom header  Name: "
							+ entry.getKey() + " Value :" + entry.getValue());
				}
				request.addHeader(entry.getKey(), entry.getValue());
			}
		}

	}

	/**
	 * This method is used to start maximum call duration timer configured for a call
	 * @param origLegCallId
	 * @param appSession
	 * @param timeInMillies
	 * @param persistable
	 * @param timerName
	 */
	public static void startMaxCallDurationTimer(String origLegCallId,
			CallData callData, SipApplicationSession appSession) {

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "startMaxCallDurationTimer............check if timer is configured by service if yes then start it");
		}
		Long maxDuration =null;
		
		if(callData.get(CallDataAttribute.MAX_CALL_DURATION)!=null)
		    maxDuration = ((Number) callData.get(CallDataAttribute.MAX_CALL_DURATION)).longValue();

		if (maxDuration == null) {
			String mxDur = SipProtocolConfig
					.getConfigData(SipProtocolConfig.MAX_CALL_DURATION);
			if (mxDur != null) {
				maxDuration = Long.parseLong(mxDur);
			}	
		}
		
		if (maxDuration != null && maxDuration.longValue() > -1) {
			callData.set(CallDataAttribute.MAX_CALL_DURATION, maxDuration);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "startMaxCallDurationTimer for value " + maxDuration);
			}
			maxDuration = maxDuration*1000;     //convert second into millisecond
			startTimer(appSession, maxDuration, true, PhConstants.MAX_CALL_DURATION_TIMER);
		}

	}


	/**
	 * This method is used to increment n/w transactions count
	 * @param incrementValue
	 */
	public static void incrementNetworkTransactions(CallData callData,
			int incrementValue) {

		MutableInt networkTransactions = (MutableInt) callData
				.get(CallDataAttribute.P_NETWORK_TRANSACTION);
		if (logger.isDebugEnabled()) {
			logger.debug("incrementNetworkTransactions for value "
					+ incrementValue+ " Current value is "+networkTransactions);
		}

		for (int i = 0; i <incrementValue; i++) {
			networkTransactions.increment();
		}

		if (logger.isDebugEnabled()) {
			logger.debug("incrementNetworkTransactions leaving with value "+networkTransactions);
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
	
	/*
	 * method to parse PSX tag in
	 */
//	public static String[] parsePSXTagFrmContactHdr(CallData callData, String contactUri){
//		String [] hdrtag = new String[3];
//		String [] psxhdrTag = contactUri.split(";");
//		for (int i = 1; i < psxhdrTag.length; i++) {
//			logger.info("INSIDE parsePSXTagFrmContactHdr ::"+psxhdrTag[i]);
//			String []temp = psxhdrTag[i].split("=");
//			if(PhConstants.PSX_RN.equals(temp[0])){
//				hdrtag[0] =temp[1];
//				logger.info("INSIDE parsePSXTagFrmContactHdr PSX_RN::"+temp[1]);
//			}else if(PhConstants.PSX_DPC.equals(temp[0])){
//				hdrtag[1] =temp[1];
//				logger.info("INSIDE parsePSXTagFrmContactHdr PSX_DPC::"+temp[1]);
//			}else if(PhConstants.PSX_SSN.equals(temp[0])){
//				hdrtag[2] =temp[1];
//				logger.info("INSIDE parsePSXTagFrmContactHdr PSX_SSN::"+temp[1]);
//			}
//		}
//		return hdrtag;
//	}
//	
	/**
	 * this method use to perform set callData value 
	 * when 300 MC receive and set tag  like PSX_SSN,PSX_RN and PSX_DDN
	 * creating term leg data
	 */
	
	public static void fetchPSXAttributes(CallData callData,int statuscode){
		//, Address contact
		
		LegData leg2 = (LegData)callData.get(CallDataAttribute.P_LEG2);
		if (statuscode == 300) {
			List<MultiChoiceContact> mccList = (List<MultiChoiceContact>) callData
					.get(CallDataAttribute.P_MC_CONTACTS_LIST);

			if (logger.isDebugEnabled()) {
				logger.debug("INSIDE fetchPSXAttributes():: for sttus code "+ statuscode);
			}
			for (MultiChoiceContact multiChoiceContact : mccList) {
				if (multiChoiceContact.getPsx_rn() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Going to set psx tag::P_PSX_IS_PORTED as ::1");

					}
					leg2.set(LegDataAttributes.P_PSX_IS_PORTED, 1);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug("LRN is not found from PSX query::");
						logger.debug("Going to set psx tag::P_PSX_IS_PORTED as ::0");

					}
					leg2.set(LegDataAttributes.P_PSX_IS_PORTED, 0);
				}

				// logger.info("Inside fetchPSXAttributes:: SSN inside contact header "+multiChoiceContact.getPsx_ssn());
				// if(multiChoiceContact.getContactUri() != null){
				// String[] temp =
				// multiChoiceContact.getContactUri().getUser().split(";");
				// logger.info("Inside fetchPSXAttributes::uesr address is::"+temp[0]);
				// logger.info("Inside fetchPSXAttributes::uesr address is::"+multiChoiceContact.getTermIp()+":"+multiChoiceContact.getTermPort());
				// if(temp[0] != null){
				// PhoneNumber called =
				// (PhoneNumber)leg2.get(LegDataAttributes.P_DESTINATION_NUMBER);
				// called.setAddress(temp[0]);
				// }
				// if(multiChoiceContact.getTermIp() != null &&
				// multiChoiceContact.getTermPort() != 0){
				// leg2.set(LegDataAttributes.P_REMOTE_IP,
				// multiChoiceContact.getTermIp());
				// leg2.set(LegDataAttributes.P_REMOTE_PORT,
				// multiChoiceContact.getTermPort());
				// }

				// callData.set(LegDataAttributes.NP_RE, value);
				// }
				if (multiChoiceContact.getPsx_dpc() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Setting DPC tag value in LEG2 as::"
								+ multiChoiceContact.getPsx_dpc());
					}
					leg2.set(LegDataAttributes.P_PSX_DPC,
							multiChoiceContact.getPsx_dpc());
				}
				if (multiChoiceContact.getPsx_rn() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Setting RN tag value in LEG2 as::"
								+ multiChoiceContact.getPsx_rn());
					}
					leg2.set(LegDataAttributes.P_PSX_RN,
							multiChoiceContact.getPsx_rn());
				}
				if (multiChoiceContact.getPsx_ssn() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Setting SSN tag value in LEG2 as::"
								+ multiChoiceContact.getPsx_ssn());
					}
					leg2.set(LegDataAttributes.P_PSX_SSN,
							multiChoiceContact.getPsx_ssn());
				}

				if (multiChoiceContact.getPsxspid() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Setting SPID tag value in LEG2 as::"
								+ multiChoiceContact.getPsxspid());
					}
					logger.info("Inside fetchPSXAttributes:: for creating term data");
					leg2.set(LegDataAttributes.P_PSX_SPID,
							multiChoiceContact.getPsxspid());
				}
			}
		} else {
			List<TermRedirectionContact> redirectContacts = (List<TermRedirectionContact>) callData
					.get(CallDataAttribute.NP_REDIRECT_ADDRESS_LIST);
			
			TermRedirectionContact redirectContact=redirectContacts.get(0);// taking first only
			//for (TermRedirectionContact redirectContact : redirectContacts) {
				if (redirectContact.getRn()!= null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Going to set psx tag::P_PSX_IS_PORTED as ::1");

					}
					leg2.set(LegDataAttributes.P_PSX_IS_PORTED, 1);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug("LRN is not found from PSX query::");
						logger.debug("Going to set psx tag::P_PSX_IS_PORTED as ::0");

					}
					leg2.set(LegDataAttributes.P_PSX_IS_PORTED, 0);
				}

				
				if (redirectContact.getDpc() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Setting DPC tag value in LEG2 as::"
								+ redirectContact.getDpc());
					}
					leg2.set(LegDataAttributes.P_PSX_DPC,
							redirectContact.getDpc());
				}
				if (redirectContact.getRn() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Setting RN tag value in LEG2 as::"
								+redirectContact.getRn());
					}
					leg2.set(LegDataAttributes.P_PSX_RN,
							redirectContact.getRn());
				}
				if (redirectContact.getSsn() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Setting SSN tag value in LEG2 as::"
								+ redirectContact.getSsn());
					}
					leg2.set(LegDataAttributes.P_PSX_SSN,
							redirectContact.getSsn());
				}

				if (redirectContact.getSpid() != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Setting SPID tag value in LEG2 as::"
								+ redirectContact.getSpid() );
					}
					logger.info("Inside fetchPSXAttributes:: for creating term data");
					leg2.set(LegDataAttributes.P_PSX_SPID,
							redirectContact.getSpid());
				}
			//}
		}

	}
	
	/**
	 * get transport from VIA header
	 * @param origLegCallId
	 * @param sipRequest
	 * @return
	 */
	public static String getTransportFromVia(String origLegCallId ,SipServletRequest sipRequest){
		
		ListIterator<String> viaheaders = sipRequest
				.getHeaders(PhConstants.VIA_HEADER);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::getTransportFromVia:  viaheaders is " + viaheaders);
		}
	
		String transport=PhConstants.TRANSPORT_UDP;
		
		while (viaheaders.hasNext()) {	
			String viaheader = viaheaders.next();
			
			if(viaheader.indexOf("TCP")!=-1){
				
				// Via: SIP/2.0/[transport] 10.32.8.153:5070;branch=[branch]  
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::getTransportFromVia:  return TCP ");
				}
				transport= PhConstants.TRANSPORT_TCP;
			}else{
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::getTransportFromVia:  return UDP ");
				}
				transport= PhConstants.TRANSPORT_UDP;
			}
			break;
			
		}
		return transport;
	}

}


