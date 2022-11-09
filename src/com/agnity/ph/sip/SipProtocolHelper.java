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

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import javax.mail.Multipart;
import javax.servlet.sip.Address;
import javax.servlet.sip.AuthInfo;
import javax.servlet.sip.Proxy;
import javax.servlet.sip.ServletParseException;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipFactory;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.SipURI;
import javax.servlet.sip.TelURL;
import javax.servlet.sip.TooManyHopsException;
import javax.servlet.sip.URI;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.Action.CONNECTIONMODE;
import com.agnity.mphdata.common.Action.EQS_TYPE;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CallPickupData;
import com.agnity.mphdata.common.CapV2CallStates;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.Destination;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.InapCallStates;
import com.agnity.mphdata.common.InviteAttributes;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.MultiChoiceContact;
import com.agnity.mphdata.common.MultipartBody;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.SignalingTypeEnum;
import com.agnity.mphdata.common.State;
import com.agnity.mphdata.common.TermRedirectionContact;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallChainedAttributes;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.measurement.MeasurementCounter;
import com.agnity.ph.common.measurement.PhMeasurementService;
import com.agnity.ph.common.measurement.enums.MSConnectionMode;
import com.agnity.ph.inapcs1scf.flowhelper.InapCS1MediaServerHelper;
import com.agnity.ph.sip.conference.ConferenceHandler;
import com.agnity.ph.sip.isup.SipIsupHelper;
import com.baypackets.ase.container.AseContext;
import com.baypackets.ase.router.acm.AseAppChainManager;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.CDRWriteFailedException;
import com.baypackets.ase.sbb.ConferenceController;
import com.baypackets.ase.sbb.ConferenceParticipant;
import com.baypackets.ase.sbb.GroupedMsSessionController;
import com.baypackets.ase.sbb.MediaServer;
import com.baypackets.ase.sbb.MediaServerException;
import com.baypackets.ase.sbb.MediaServerSelector;
import com.baypackets.ase.sbb.MsCollectSpec;
import com.baypackets.ase.sbb.MsConferenceSpec;
import com.baypackets.ase.sbb.MsPlaySpec;
import com.baypackets.ase.sbb.MsRecordSpec;
import com.baypackets.ase.sbb.MsSessionController;
import com.baypackets.ase.sbb.OutboundGateway;
import com.baypackets.ase.sbb.OutboundGatewaySelector;
import com.baypackets.ase.sbb.SBB;
import com.baypackets.ase.sbb.SBBFactory;
import com.baypackets.ase.sbb.impl.SBBOperationContext;
import com.baypackets.ase.sipconnector.AseDialogManager;
import com.baypackets.ase.sipconnector.AseSipSession;

/**
 * This class is the helper class for Sip Protocol Handler used to delegate the
 * processing of incoming SIP messages and creation of out going SIP
 * messages.After initial level analysis, SipProtocolHandler passes all incoming
 * messages to this file for further processing
 */
public class SipProtocolHelper {
	private static Logger logger = Logger.getLogger(SipProtocolHelper.class);

	/*
	 * Ms-Sbb event listener
	 */
	protected transient static final MediaEventListener cMsEventListener = new MediaEventListener();

	private SipProtocolHelper() {

	}

	public static void handleReinvite(SipServletRequest sipRequest)
			throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				sipRequest.getSession(), callData);

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId
					+ ":: Received Re-INVITE request with call id "
					+ sipRequest.getCallId());
		}
		ConnectionType legType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);

		/*
		 * Fix for SBTM-UAT-745.If there is a outstanding INVITE on this sip leg
		 * then send 491 for received Re-Invite
		 */
		if (PhConstants.TRUE.equals(legData
				.get(LegDataAttributes.P_INVITE_PENDING_TRANS))) {

			logger.error(origLegCallId
					+ ":: Send 491 for this Re-Invite. Outstanding INVITE transaction on "
					+ sipRequest.getCallId());
			// Restart session refresh timer
			SipProtocolUtil.stopSessionRefreshTimer(appSession, legData);

			// sipRequest.createResponse(SipServletResponse.SC_REQUEST_PENDING)
			// .send();
			SipProtocolMessageCreator.createResponse(origLegCallId, sipRequest,
					SipServletResponse.SC_REQUEST_PENDING, callData).send();
			SipProtocolUtil.startSessionRefreshTimer(appSession, legData);
			return;
		}

		/*
		 * If SN sends 491 for re-invite then we should start session refresh
		 * timer with old nagotiated value. So calling setSessionExpiryTime
		 * method after checking whether PH needs to send 491 or not. Because
		 * setSessionExpiryTime updates the sipSession attributes with new
		 * session refresh values in this new received Re-INVITE
		 */
		// Store session expire duration for future reference
		SipProtocolUtil
				.setSessionExpiryTime(sipRequest, legData, origLegCallId);

		MultipartBody lastReceivedSdp = (MultipartBody) legData
				.get(LegDataAttributes.P_LAST_RECEIVED_SDP);
		MultipartBody currentReceivedSdp = (MultipartBody) legData
				.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

		boolean isSdpChanged = SipProtocolUtil.isSdpChanged(lastReceivedSdp,
				currentReceivedSdp, origLegCallId);
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		switch (state) {
		case CONNECTED:
		case CALL_TRANSFER_IN_PROGRESS:
		case CALL_TRANSFERED: {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::" + legType
						+ " State is CONNECTED/CALL_TRANSFER_IN_PROGRESS");
				logger.debug(origLegCallId
						+ ":: Stop session refresh timer of " + legType);
			}
			// Stop session refresh timer
			SipProtocolUtil.stopSessionRefreshTimer(appSession, legData);

			if (isSdpChanged && !(currentReceivedSdp==null)) {// session refresh case of current received invite with empty sdp
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: ReInvite is for SDP change");
				}
				/*
				 * Fix for 15249. Send 100 trying for Re-Invite if SDP has
				 * changed in this Re-Invite
				 */
				if (PhConstants.TRUE.equals(SipProtocolUtil
						.getConfig(SipProtocolConfig.REINVITE_100_TRYING_FLAG))) {
					// sipRequest.createResponse(SipServletResponse.SC_TRYING)
					// .send();

					SipProtocolMessageCreator.createResponse(origLegCallId,
							sipRequest, SipServletResponse.SC_TRYING, callData)
							.send();
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Send 100 Trying for Re-Invite");
					}
				}

				LegData peerLegData = SipProtocolUtil.getPeerLegData(callData,
						legType);
				ConnectionType peerLegType = (ConnectionType) peerLegData
						.get(LegDataAttributes.P_CONNECTION_TYPE);

				State peerLegSipState = (State) peerLegData
						.get(LegDataAttributes.P_LEG_SIP_STATE);

				if ((peerLegSipState) == State.CONNECTED
						|| (peerLegSipState) == State.CALL_TRANSFER_IN_PROGRESS
						|| (peerLegSipState) == State.CALL_TRANSFERED) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + peerLegType
								+ " state is CONNECTED");
						logger.debug(origLegCallId
								+ ":: Store success response of the reinvite in session");
						logger.debug(origLegCallId + ":: Send reinvite to "
								+ peerLegType);
					}

					/**
					 * is the sdp received is hold sdp then chekc if we need to
					 * play music to other party on hold ellse send hold reivite
					 * to other party
					 */
					String putMusicOnHold = SipProtocolConfig
							.getConfigData(SipProtocolConfig.PUT_MUSIC_ON_HOLD);
					SipServletRequest peerInitialInviteReq = SipProtocolUtil
							.getInitialInvite(appSession, peerLegData);

					MultipartBody peersdp = (MultipartBody) peerLegData
							.get(LegDataAttributes.P_LAST_RECEIVED_SDP);

					SipServletResponse successResponse = SipProtocolMessageCreator
							.createSuccessResponseInvite(sipRequest, null);

					if (currentReceivedSdp != null
							&& SipProtocolUtil.isHoldSdp(sipRequest
									.getContent())
							&& PhConstants.TRUE.equals(putMusicOnHold)) {

						if (peersdp != null) {
							successResponse.setContent(peersdp.getContent(),
									peersdp.getContentType());
						}

						successResponse.send();

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "start music on hold on other party "
									+ peerLegType);
						}
						peerLegData.set(LegDataAttributes.HOLD_ON_MUSIC_PLAY,
								PhConstants.TRUE);
						initiateMediaServerConnection(appSession, callData,
								(String) peerInitialInviteReq.getSession()
										.getAttribute(PhConstants.LEG_ID));

					} else {
						// Create success response and save it in session as
						// pending success response

						legData.set(LegDataAttributes.P_PENDING_SUCC_RESP,
								successResponse);

						// Create reinvite for term leg and send it

						SipServletRequest peerLegReinvite = SipProtocolMessageCreator
								.createReinviteRequest(peerInitialInviteReq,
										currentReceivedSdp);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "Stop session refresh timer of "
									+ peerLegType);
						}

						// Stop session refresh timer
						SipProtocolUtil.stopSessionRefreshTimer(appSession,
								peerLegData);

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId + "Send reinvite to "
									+ peerLegType);
						}
						// Send reinvite
						peerLegReinvite.send();
						
						if (peerLegReinvite.getContentType() != null) {
							peerLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
									PhConstants.TRUE);
						}
						SipProtocolUtil.setSentSdp(peerLegReinvite,
								peerLegData, origLegCallId);
						peerLegData.set(
								LegDataAttributes.P_INVITE_PENDING_TRANS,
								PhConstants.TRUE);

					}

				} else {

					/*
					 * Fix for Bug#15631. If SDP change ReInvite is received
					 * from Orig leg and Term is not in connected state then
					 * send LAST send SDP to orig side in 200 OK response.
					 * Although thi case will never happen for A-Leg. But this
					 * issue is faced with B-Leg in Service Guidance flow. So
					 * for symmetry changing code for orig leg also
					 */
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "ReInvite is for SDP change and peer leg not in connected state");
						logger.debug(origLegCallId
								+ "Send success response with last sent sdp to "
								+ legType);
					}

					MultipartBody lastSentSdpContent = (MultipartBody) legData
							.get(LegDataAttributes.P_LAST_SENT_SDP);

					SipServletResponse successResponse = SipProtocolMessageCreator
							.createSuccessResponseInvite(sipRequest,
									lastSentSdpContent);

					// Start Session refresh timer
					SipProtocolUtil.startSessionRefreshTimer(appSession,
							legData);
					successResponse.send();
				}

			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "ReInvite is not for SDP change");
					logger.debug(origLegCallId
							+ "Send success response with last sent sdp");
				}

				MultipartBody lastSentSdpContent = (MultipartBody) legData
						.get(LegDataAttributes.P_LAST_SENT_SDP);

				SipServletResponse successResponse = SipProtocolMessageCreator
						.createSuccessResponseInvite(sipRequest,
								lastSentSdpContent);

				// Start Session refresh timer
				SipProtocolUtil.startSessionRefreshTimer(appSession, legData);
				successResponse.send();
			}
			break;
		}
		default: {
			/*
			 * Handle the race condition where re-invite of session refresh from
			 * orig come just after disconnecting the IVR. In this case if SDP
			 * is not changed by orig leg, last sent SDP should be sent.
			 */
			if (!isSdpChanged) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "ReInvite is not for SDP change");
					logger.debug(origLegCallId
							+ "Send success response with last sent sdp");
				}
				MultipartBody lastSentSdpContent = (MultipartBody) legData
						.get(LegDataAttributes.P_LAST_SENT_SDP);

				SipServletResponse successResponse = SipProtocolMessageCreator
						.createSuccessResponseInvite(sipRequest,
								lastSentSdpContent);

				// Start Session refresh timer
				SipProtocolUtil.startSessionRefreshTimer(appSession, legData);

				successResponse.send();
			} else {
				/*
				 * Fix for SBTM-UAT-745.
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "ReInvite is for SDP change");
					logger.debug(origLegCallId
							+ "Send success response with last sent sdp after changing c line to 0.0.0.0");
				}

				MultipartBody lastSentSdpContent = (MultipartBody) legData
						.get(LegDataAttributes.P_LAST_SENT_SDP);
				/*
				 * Fix for SBTM-UAT-745. If re-invite of SDP change from orig
				 * comes just after disconnecting the IVR. Then last sent SDP
				 * should be sent after changing a line to inactive
				 */
				lastSentSdpContent = SipProtocolUtil.getSdpContentForHold(
						origLegCallId, lastSentSdpContent, true);

				SipServletResponse successResponse = SipProtocolMessageCreator
						.createSuccessResponseInvite(sipRequest,
								lastSentSdpContent);

				/*
				 * Start Session refresh timer
				 */
				SipProtocolUtil.startSessionRefreshTimer(appSession, legData);

				successResponse.send();
				if (lastSentSdpContent != null) {
					legData.set(LegDataAttributes.P_LAST_SENT_SDP,
							lastSentSdpContent);
				}

			}
		}
		}

	}

	/**
	 * This method sends the redirection response It writes internal CDR[SRVEND]
	 * if service is sending 302 due to CONNECTIONMODE.REDIRECTION case and
	 * notify service that call is not more on SN. Set appsession timeout to 2
	 * minutes to cleanup call if INVITE does not come. It is require as the
	 * correlation timer is not used for this call flow.
	 * 
	 * @param appSession
	 *            represents an instance of SipApplicationSession
	 * @param connectMode
	 *            represents an enum type Action.CONNECTIONMODE
	 * @throws IOException
	 */
	public static void sendRedirectionResponse(
			SipApplicationSession appSession, CallData callData, Action action)
			throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside sendRedirectionResponse");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));
		SipServletRequest sipRequest = SipProtocolUtil.getInitialInvite(
				appSession, legData);
		SipSession legSipSession = SipProtocolUtil.getSipSessionFromSessionId(
				origLegCallId, appSession,
				(String) legData.get(LegDataAttributes.P_SESSION_ID));

		SipServletResponse sipResponse = SipProtocolMessageCreator
				.createRedirectionResponse(sipRequest, callData, legData);
		sipResponse.send();
		legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.TERMINATED);
		callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
				SipProtocolRelReasonCode.CALL_REDIRECTED);

		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());

		legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE,
				PhConstants.TRUE);
		// /*
		// * Set appSession timeout to 2 minute so that it gets invalidated
		// after
		// * 2 minute automatically if ACK is not received for 302 sent.
		// */
		SipProtocolUtil.setAppSessionTimeout(appSession,
				PhConstants.CLEANUP_APPSESSION_TIMEOUT, origLegCallId);
	}

	/**
	 * This method is used to Inform service that call is dropped.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 */
	public static void notifyCallDropped(CallData callData) {
		// CallData callData = null;
		String origLegCallId = null;

		try {
			// callData = SipProtocolUtil.getCallData(appSession);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inform service that call is dropped");
			}

		} catch (Exception ex) {
			logger.error(origLegCallId + ":: Error in notifyCallDropped ", ex);
		} finally {

			ServiceInterface serviceHandler = PhUtilityServices.getInstance(
					(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();
			Event event = new Event(EventType.EVENT_CALL_DROPPED, Protocol.SIP,
					null);

			try {
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				logger.error(origLegCallId + ":: Error in notifyCallDropped ",
						e);
			}

		}
	}

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
		
		String prevSvcId=(String) callData
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

			
			if(prevSvcId==null){
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
						+ " :: Invoking write method on CDR utility write Service CDR as --> " + cdrArr.length);
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
	 * This method called when call needs to be dropped for a particular session id , 
	 * set in CallData forceCallCleanup 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 */
	public static void forceCallCleanup(SipApplicationSession appSession,
			CallData callData, Action action) {
	
		if(logger.isDebugEnabled()) {
			logger.debug("Inside forceCallCleanup in SipProtocolHanlder");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));
		
	String appSessionId=(String)legData.get(LegDataAttributes.FORCE_CALL_CLEANUP_SESSION_ID);
	String appId = (String)legData.get(LegDataAttributes.FORCE_CALL_CLEANUP_APP_ID);
	
	if(logger.isDebugEnabled()) {
		logger.debug("AppSession id retirieved from LegData:- "+ appSessionId + " appId retrieved:- " + appId);
	}
	
	PhUtilityServices phUtilityServices=	PhUtilityServices.getInstance(appId);
	
	SipApplicationSession sipApplicationSession= phUtilityServices.getSipSessionsUtil().getApplicationSessionById(appSessionId);
	if(logger.isDebugEnabled()) {
		logger.debug("Got SipApplicationSession from appId  "+ sipApplicationSession);
	}
	CallData callDataFromAppSession= SipProtocolUtil.getCallData(sipApplicationSession);
	if(logger.isDebugEnabled()) {
		logger.debug("Retireved callData from sipApplicationSession  "+ callDataFromAppSession + "dropping call now");
	}
	action.setServiceComplete(true);
	SipProtocolHelper.dropCall(sipApplicationSession, callDataFromAppSession, action);
	
	}

	/**
	 * This method called when call needs to be dropped due to some error in the
	 * PH logic.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 */
	public static void dropCall(SipApplicationSession appSession) {
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside dropCall with appSession");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		dropCall(appSession, callData, action);
	}

	/**
	 * This method performs the call cleanup for intra-network call Based on
	 * different orig and term sip call state, appropriate messages are sent for
	 * cleanup the sip call legs.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 * @param action
	 *            represents the instance of Action
	 */
	public static void dropCall(SipApplicationSession appSession,
			CallData callData, Action action) {
		String origLegCallId = null;

		try {
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Inside dropCall with appSession and action");
			}
			/*
			 * Cleanup all sip leg according to their sip session state
			 */
			cleanupSipSession(appSession, action);
		} catch (Exception ex) {
			logger.error(origLegCallId + ":: Failed to drop the call.", ex);
			Iterator<?> it = appSession.getSessions("SIP");
			while (it.hasNext()) {
				SipSession sipSession = (SipSession) it.next();
				sipSession.setAttribute(
						PhConstants.SESSION_READY_TO_INVALIDATE,
						PhConstants.TRUE);

				LegData legData = (LegData) callData.get(CallDataAttribute
						.valueOf((String) sipSession
								.getAttribute(PhConstants.LEG_ID)));

				if (legData != null) {
					legData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.TERMINATED);
				}
			}
		}

		/*
		 * Notify service that call is dropped
		 */

		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME,
				new Date());
		SipProtocolHelper.notifyCallDropped(callData);


		if (!PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Write CDR");
			}
			SipProtocolHelper.writeServiceCdr(callData, action);
		} else {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Donot write CDR it is already written");
			}
		}

		// Return correlation id back to pool, if hold it
		if (callData != null
				&& callData.get(CallDataAttribute.CORRELATION_KEY) != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Returning correlation Key back to pool");
			}

			// currently using AinScfProtocolUtil. It should be common.
			AinScfProtocolUtil.returnTokenToSharedPool(callData);
		}

		// Remove Triggered Services
		String serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		if (PhUtilityServices.getInstance(serviceId).getAppChainManager() != null) {
			boolean isRemoved = PhUtilityServices.getInstance(serviceId)
					.getAppChainManager()
					.removeTriggeredServices(serviceId, appSession, null);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: triggeredServices removed : " + isRemoved);
			}
		}

		/*
		 * Invalidate appsession at last after wr
		 */
		if (PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.NP_ORIG_BYE_ON_IVR))) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside dropCall  app session will get cleaned up automatically after some time as orig hungup while on ivr so 200 ok should be sent to it first");
			}
			SipProtocolUtil.setAppSessionTimeout(appSession, 1, origLegCallId);
		} else {
			if (appSession.isValid()) {
				SipProtocolUtil.invalidateAppSession(appSession);
			}
		}

	}

	/**
	 * This method handles the invalid INVITE request.If it is an initial
	 * request so drop the call. otherwise Reject Re-invite.
	 * 
	 * @param sipRequest
	 *            represents an instance of SipServletRequest
	 * @throws Exception
	 */
	private static void handleInvalidInvite(SipServletRequest sipRequest)
			throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleInvalidInvite");
		}

		if (sipRequest.isInitial()) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Initial request so drop the call");
			}
			dropCall(appSession);
		} else {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Reject Re-invite");
			}

			// SipServletResponse response = sipRequest
			// .createResponse(SipServletResponse.SC_NOT_ACCEPTABLE_HERE);
			SipServletResponse response = SipProtocolMessageCreator
					.createResponse(origLegCallId, sipRequest,
							SipServletResponse.SC_NOT_ACCEPTABLE_HERE, callData);
			response.send();
		}
	}

	/**
	 * This method cleanup all the SIP legs. It is also used for INAP calls to
	 * cleanup SIP leg created for ASSIST. Iterate over all the sipsessions and
	 * perform following activities: All transaction completed on the sip
	 * session(marked by setting READY_TO_INVALIDATE flag). Do not send any
	 * message to this leg.Don't cleanup the ASSIST sip leg, it should be
	 * cleanedup by NTT with BYE message.
	 * <p>
	 * For it is ORIG_CONNECTION_TYPE, Disconnect orig sip leg.If sipsession is
	 * INITIAL or EARLY, Send Error response to orig leg.If sipsession is
	 * CONFIRMED, Send BYE request. If sipsession is TERMINATED, do nothing.
	 * <p>
	 * For it is TERM_CONNECTION_TYPE, Disconnect orig sip leg. If sipsession is
	 * INITIAL or EARLY, Send Cancel response to term leg. If sipsession is
	 * CONFIRMED, Send BYE request. If sipsession is TERMINATED, do nothing. For
	 * it is TERM_CONNECTION_TYPE, Disconnect orig MS leg. If sipsession is
	 * INITIAL or EARLY or CONFIRMED, disconnectMediaServer should not be called
	 * for terminated SIP-sessions.
	 * <p>
	 * For it is TERM_IVR_CONNECTION_TYPE, Disconnect orig sip leg. If
	 * sipsession is INITIAL or EARLY or CONFIRMED, disconnectMediaServer should
	 * not be called for terminated SIP-sessions.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 */
	private static void cleanupSipSession(SipApplicationSession appSession,
			Action action) {
		CallData callData = null;
		String origLegCallId = null;

		callData = SipProtocolUtil.getCallData(appSession);
		origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: inside cleanupSipSession");
			logger.debug(origLegCallId + ":: appSessionId: "
					+ appSession.getId());
		}

		SipServletRequest initInviteRequest = null;

		Iterator<?> it = appSession.getSessions("SIP");
		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();
		LegData legData = null;
		SipSession sipSession = null;
		/*
		 * This method is invoked by executeAction for Sip and Tcap Session
		 * methods. In both cases, timers should be stopped.
		 */
		SipProtocolUtil.stopAllTimers(appSession);

		while (it.hasNext()) {
			try {
				sipSession = (SipSession) it.next();

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: The Sip Session id to be cleaned is  "
							+ sipSession.getId());
				}
				initInviteRequest = (SipServletRequest) sipSession
						.getAttribute(PhConstants.ORIG_REQUEST);

				legData = SipProtocolUtil.getLegDataForSipSession(appSession,
						sipSession, callData);

				String legId = (String) sipSession
						.getAttribute(PhConstants.LEG_ID);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: The legId in current sip session is  "
							+ legId);
				}

				/**
				 * Following check has been done for stale term leg session in
				 * case of alternte routing scenario in which first term leg
				 * will have legdata as leg2 data but in legdata sipsession id
				 * will be updated of next term leg .
				 */
				if (legId != null
						&& legId.equals(CallDataAttribute.P_LEG2.name())
						&& legData != null) {

					String legSessionId = (String) legData
							.get(LegDataAttributes.P_SESSION_ID);
					if (legSessionId != null
							&& !legSessionId.equals(sipSession.getId())) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: The sip Session looks like a stale sip session as sip session id in legadat donot matches with current session id "
									+ sipSession.getId());
						}
						continue;
					}
				}

				ConnectionType connType = null;
				/*
				 * It might be possible that FT happens before receiving 200 OK
				 * from IVR. In that case, IVR_SESSION_ID would not be available
				 * in legData. To handle this case, LEG_ID_CONNECTED_TO_IVR is
				 * set in callData while initiating media server connection.
				 * This attribute is removed after setting IVR_SESSION_ID in
				 * legDtaa. Following logic is to find the legData from callData
				 * in case current sip-session doesnot match to SESSION_ID and
				 * IVR_SESSION_ID.
				 */
				if (legData == null) {

					legId = (String) callData
							.get(CallDataAttribute.P_LEG_ID_CONNECTED_TO_IVR);

					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ " LegData not found for "
								+ initInviteRequest.getCallId()
								+ ". This leg may be of IVR conn type get legdata for connection "
								+ legId);
					}
					legData = (LegData) callData.get(CallDataAttribute
							.valueOf(legId));
				}

				if (PhConstants.TRUE.equals(sipSession
						.getAttribute(PhConstants.IVR_LEG))) {
					connType = ConnectionType.IVR_CONNECTION;
				} else {
					connType = (ConnectionType) legData
							.get(LegDataAttributes.P_CONNECTION_TYPE);
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Connection type is "
							+ connType + " for Sip Session Id = "
							+ sipSession.getId());
					logger.debug(origLegCallId + ":: Sip session state is "
							+ sipSession.getState());
				}

				/*
				 * 
				 * In SAS platform, the sip session does not get removed from
				 * the session list of appsession on the standby SAS, even if it
				 * gets invalidated on active SAS. Due to the above problem,
				 * while cleaning the sip session in this method, a exception is
				 * raised related to invalid transaction state. The following
				 * logic is to avoid that exception. There are still some
				 * chances to get the exception, in case the attribute set in
				 * sip session does not get replicated to the standby SAS before
				 * FT.
				 */
				// FIX jira bug 1603
				if (PhConstants.TRUE.equals(sipSession
						.getAttribute(PhConstants.SESSION_READY_TO_INVALIDATE))) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: All transaction completed on this sip session: "
								+ sipSession.getCallId()
								+ ". Do not send any message to this leg");
					}

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: check if there is linked session of this session then disconnect that also");
					}

					SipServletRequest req = (SipServletRequest) sipSession
							.getAttribute(PhConstants.CALLPICKUP_LINKED_REQUEST);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: linked request is "
								+ req);
					}
					if (req != null) {
						SipSession linkedSession = req.getSession();

						disconnectLinkedSession(linkedSession, action);
					}

					continue;
				}

				switch (connType) {
				case ORIG_CONNECTION: {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Disconnect orig sip leg");
					}
					// Set the handler on sip session
					sipSession.setHandler(serviceHandler.getServletName());
					/*
					 * sendPendingRequests() is added to fix for the error
					 * scanrios like - if flexible charging is applied, then
					 * while sending 183+CHG to orig PH creates 200 OK for orig.
					 * But if PRACK is not received from orig and PRACK timeout
					 * happens, then PH is not able to send error response to
					 * orig as final response has already been created on this
					 * sip session. - If SDP change Re-INVITE is received from
					 * Leg A then PH creates 200 OK for leg A and stores that in
					 * Leg A sip session. After this PH sends Re-INVITE to B
					 * leg. B leg does not answer this INVITE. Then 408 is
					 * generated for leg B and it goes into terminated state.
					 * But because 200 OK has already been created and stored
					 * for leg A , so PH is direcly sending BYE to leg A,
					 * without any final response to leg A.
					 */
					if (!PhConstants.TRUE.equals(legData
							.get(LegDataAttributes.P_IS_CANCEL_RECEIVED))) {
						sendPendingRequests(callData, sipSession, legData);
					}
					switch (sipSession.getState()) {
					case INITIAL:
					case EARLY: {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Session state is EARLY or INITIAL");
							logger.debug(origLegCallId
									+ ":: Send Error response to orig leg");
						}
						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.TERMINATED);
						SipServletResponse errorResponse = null;
						/*
						 * SBTM-UAT-555: If IS_CANCEL_RECEIVED attribute is set
						 * on orig SIP Session, then
						 * SipInapIsupHandlerServlet.cleanupSipSession() will
						 * send 487 for INVITE on orig leg
						 */
						if (PhConstants.TRUE.equals(legData
								.get(LegDataAttributes.P_IS_CANCEL_RECEIVED))) {
							// errorResponse = initInviteRequest
							// .createResponse(487);
							errorResponse = SipProtocolMessageCreator
									.createResponse(
											origLegCallId,
											initInviteRequest,
											SipServletResponse.SC_REQUEST_TERMINATED,
											callData);
						} else {
							errorResponse = SipProtocolMessageCreator
									.createErrorResponse(initInviteRequest,
											action);
						}
						errorResponse.send();

						sipSession.setAttribute(
								PhConstants.SESSION_READY_TO_INVALIDATE,
								PhConstants.TRUE); // LEV-3764
						break;
					}
					case CONFIRMED: {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Session state is CONFIRMED");
							logger.debug(origLegCallId + ":: Send BYE request");
						}

						SipServletRequest pendingAck = (SipServletRequest) legData
								.get(LegDataAttributes.P_PENDING_ACK);
						if (pendingAck != null) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: ACK is pending so send it first");
							}
							pendingAck.send();
						}

						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.TERMINATED);

						/*
						 * The 31 should be used as cause value in REL if ANM
						 * has already been sent [SBTM-UAT-192]. The only
						 * exception to this case is that REL received from
						 * party B would be sent transparently to party A.
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Change cause value to 31 as ANM has already been sent");
						}
						SipProtocolMessageCreator.createByeRequest(
								origLegCallId, sipSession,
								PhConstants.BYE_CV_ANM_ALREADY_SENT,
								action.isAddCustomHeader()).send();
						break;
					}
					case TERMINATED: {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Session state is TERMINATED, do nothing");
						}
						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.TERMINATED);
						break;
					}
					}
					break;
				}
				case TERM_CONNECTION: {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Disconnect term sip leg");
					}
					// Set the handler on sip session
					sipSession.setHandler(serviceHandler.getServletName());
					/*
					 * sendPendingRequests() is added to fix for the error
					 * scanrios like - If SDP change Re-INVITE is received from
					 * Leg A then PH creates 200 OK for leg A and stores that in
					 * Leg A sip session. After this PH sends Re-INVITE to B
					 * leg. B leg does not answer this INVITE. Then 408 is
					 * generated for leg B and it goes into terminated state.
					 * But because 200 OK has already been created and stored
					 * for leg A , so PH is direcly sending BYE to leg A,
					 * without any final response to leg A.
					 */
					sendPendingRequests(callData, sipSession, legData);

					/*
					 * Send Cancel/Bye on term leg only if Cancel has already
					 * not been sent on term leg. This is to take care of the
					 * following use case: When No-Answer timer times-out, PH
					 * sends Cancel/Bye to B-Party and informs service. If
					 * service returns drop_call, then PH iterates through all
					 * sip sessions. But for term sip session, 487 for INVITE
					 * has not received till now. So that sip session is in
					 * INITIAL state. So this method again tries to send cancel
					 */
					String cancelOrByeSent = (String) legData
							.get(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND);
					switch (sipSession.getState()) {
					case INITIAL:
					case EARLY: {
						/*
						 * Check if the no answer timeout was not received for
						 * term and also no error response was received from
						 * term (cuase code will be null)
						 */
						if (!PhConstants.TRUE.equals(cancelOrByeSent)) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: Session state is EARLY or INITIAL");
								logger.debug(origLegCallId
										+ ":: Send Cancel request to term leg");
							}
							// Old PH was setting TERMINATION_IN_PROGRESS state
							// here
							legData.set(LegDataAttributes.P_LEG_SIP_STATE,
									State.TERMINATED);
							// legData.setPersistableData(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND,PhConstants.TRUE);
							SipProtocolMessageCreator.createCancelRequest(
									origLegCallId, initInviteRequest,
									PhConstants.NO_ANS_TIMEOUT_CV_TERM_LEG)
									.send();
						}
						break;
					}
					case CONFIRMED: {
						if (!PhConstants.TRUE.equals(cancelOrByeSent)) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: Session state is CONFIRMED");
								logger.debug(origLegCallId
										+ ":: Send BYE request");
							}
							SipServletRequest pendingAck = (SipServletRequest) legData
									.get(LegDataAttributes.P_PENDING_ACK);
							if (pendingAck != null) {
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ ":: ACK is pending so send it first");
								}
								pendingAck.send();
							}

							// Old PH was setting TERMINATION_IN_PROGRESS state
							// here
							legData.set(LegDataAttributes.P_LEG_SIP_STATE,
									State.TERMINATED);
							SipProtocolMessageCreator.createByeRequest(
									origLegCallId, sipSession,
									PhConstants.BYE_CV_ANM_ALREADY_SENT,
									action.isAddCustomHeader()).send();
						}
						break;
					}
					case TERMINATED: {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Session state is TERMINATED, do nothing");
						}
						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.TERMINATED);
						break;
					}
					}
					break;
				}
				case IVR_CONNECTION: {
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId + ":: Disconnect MS leg");
					}
					/*
					 * disconnectMediaServer should not be called for terminated
					 * SIP-sessions
					 */
					switch (sipSession.getState()) {
					/*
					 * disconnectMediaServer should not be called for terminated
					 * SIP-sessions
					 */
					case INITIAL:
					case EARLY:
					case CONFIRMED:
						String ivrLegId = (String) sipSession
								.getAttribute(PhConstants.LEG_ID);
						if (logger.isInfoEnabled()) {
							logger.info(origLegCallId
									+ ":: Disconnect IVR connected to leg "
									+ ivrLegId);
						}
						GroupedMsSessionController msController = SipProtocolUtil
								.getMsController(appSession, legData, ivrLegId);
						/*
						 * Sometimes B is already available to SAS after FT, in
						 * that case SAS throws runtime exception. So adding
						 * following try catch block to ignore that exception
						 */
						try {
							if (PhConstants.TRUE.equals(callData
									.get(CallDataAttribute.NP_FT_CALL))
									|| msController.getB() == null) {
								msController.addB(sipSession);
							}
						} catch (Throwable e) {
							logger.warn(origLegCallId
									+ " B already added in Orig MS Sip Session");
						}
						msController.setApplicationSession(appSession);
						msController.setEventListener(cMsEventListener);
						SipProtocolUtil.incrementNetworkTransactions(callData,
								1);
						sipSession.setAttribute(
								PhConstants.MS_BYE_TRXN_INCREMENTED,
								PhConstants.TRUE);
						msController.disconnectMediaServer();
						break;
					case TERMINATED:
						// This is needed because on receiving EVENT_DISCONNECT
						// from MediaServer, sbb.getB() returns null,
						// and there is no reference of IVR session at that
						// point. Thus we were not able to set invalidate flag
						// there
						// thus setting it here.
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Setting SESSION_READY_TO_INVALIDATE as true for MS Session");
						}
						sipSession.setAttribute(
								PhConstants.SESSION_READY_TO_INVALIDATE,
								PhConstants.TRUE);
						break;
					}
					break;
				}
				default: {
					logger.warn(origLegCallId
							+ ":: Unrecognized connection type " + connType
							+ ", do nothing");
				}
				}

			} catch (Exception ex) {
				logger.warn(origLegCallId + ":: Error in releasing sip leg is "
						+ ex.getMessage());
				if (logger.isInfoEnabled()) {
					logger.info(
							origLegCallId + ":: Error in releasing sip leg", ex);
				}
				/*
				 * SBTM-UAT-1583 Set ready to invalidate for this leg if
				 * exception comes while cleaning it so that appSession can be
				 * invalidated.
				 */
				sipSession.setAttribute(
						PhConstants.SESSION_READY_TO_INVALIDATE,
						PhConstants.TRUE);
			}
		}
		/*
		 * Set App-Session time-out to 2 minute. So that if PH could not
		 * invalidate the appsession, it would be invalidated automatically
		 * after 2 minute.
		 */
		SipProtocolUtil.setAppSessionTimeout(appSession,
				PhConstants.CLEANUP_APPSESSION_TIMEOUT, origLegCallId);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Cleanup of sip legs completed");
		}
	}

	private static void sendPendingRequests(CallData callData,
			SipSession sipSession, LegData legData) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside sendPendingRequests()");
		}
		if (legData == null) {
			return;
		}
		SipServletResponse pendingSuccResp = null;
		try {
			// Send pending response to Orig leg
			pendingSuccResp = (SipServletResponse) legData
					.get(LegDataAttributes.P_PENDING_SUCC_RESP);
			if (pendingSuccResp != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Send pending response");
				}
				// Send success response to orig leg and update sent sdp

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: PendingSuccResp Content "
							+ pendingSuccResp.getContent());
					logger.debug(origLegCallId
							+ ":: PendingSuccResp Content Length "
							+ pendingSuccResp.getContentLength());
				}
				/*
				 * Set content on pendingSuccResp if its content is null. It
				 * will happen in following scenario: - If SDP change Re-INVITE
				 * is received from Leg A then PH creates 200 OK for leg A and
				 * stores that in Leg A sip session. After this PH sends
				 * Re-INVITE to B leg. If B leg does not answer this INVITE.
				 * Then 408 is generated for leg B and its sip-session goes into
				 * terminated state. In this case to clean leg A sip session PH
				 * should first send stored pending response to leg-A and then
				 * BYE to leg-A. But in case of Re-Invite this stored pending
				 * 200 OK response does not has SDP content in it. So setting
				 * SDP in following if condition
				 */
				if (pendingSuccResp.getContent() == null) {
					LegData peerLegData = SipProtocolUtil.getPeerLegData(
							callData, (ConnectionType) legData
									.get(LegDataAttributes.P_CONNECTION_TYPE));
					if (peerLegData != null) {
						MultipartBody currentReceviedSdp = (MultipartBody) peerLegData
								.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
						if (currentReceviedSdp != null) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: Set content on pending success response");
							}
							pendingSuccResp.setContent(
									currentReceviedSdp.getContent(),
									currentReceviedSdp.getContentType());
						}
					}
				}
				pendingSuccResp.send();
			}
		} catch (Exception e) {
			logger.error(origLegCallId
					+ ":: Failed to send pending success response.Error "
					+ e.getMessage());
		} finally {
			if (pendingSuccResp != null) {
				legData.remove(LegDataAttributes.P_PENDING_SUCC_RESP);
			}
		}
	}

	/**
	 * This method perform some basic validation of INVITE request.
	 * 
	 * @param sipRequest
	 *            represents the instance of SipServletRequest
	 * @return boolean represents a boolean variable
	 * @throws Exception
	 */
	public static boolean validateInviteRequest(SipServletRequest sipRequest)
			throws Exception {
		SipApplicationSession appSession = sipRequest.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside validateInviteRequest");
		}

		// Check to drop the correlated call after SAS switch-over
		if (sipRequest.isInitial()
				&& PhConstants.TRUE.equals(callData
						.get(CallDataAttribute.NP_FT_CALL))) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ " :: INVITE is received after SAS switchover, drop call");
			}
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.INVITE_RCVD_AFTER_FT);
			LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
			leg1.set(LegDataAttributes.P_CAUSE_CODE,
					Integer.parseInt(SipProtocolUtil.getConfig(

					SipProtocolConfig.SIP_RES_CODE_INVITE_RCVD_AFTER_FT)));

			/*
			 * The dropCall method with tcapSession would be invoked internally
			 * if inap call state is ASSIST. Its responsibility of dropCall with
			 * tcapSession to cleanup the SIP leg too for ASSIST case.
			 */
			dropCall(appSession);
			return true;
		}

		/*
		 * Validate refresher tag 1. Invite/Re-Invite from A party should not
		 * contain refresher=UAS 2. Re-Invite from B-Party should not contain
		 * refresher=UAS
		 */
		String unsupportedRefresher = PhConstants.REFRESHER_UAS;
		String sessionExpiresHeaderVal = sipRequest
				.getHeader(PhConstants.SESSION_EXPIRE_HEADER);
		String requireHeaderVal = sipRequest
				.getHeader(PhConstants.REQUIRE_HEADER);
		if (sessionExpiresHeaderVal != null
				&& sessionExpiresHeaderVal.contains(unsupportedRefresher)
				&& (requireHeaderVal != null && requireHeaderVal
						.contains("timer"))) {
			// As of now, SN does not support INVITE with session refresher UAS
			logger.error(origLegCallId + ":: INVITE with refresher "
					+ unsupportedRefresher + " not supported");
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.UNSUPPORTED_SE_HEADER);

			handleInvalidInvite(sipRequest);
			return true;
		}

		/**
		 * check min SE value allowed for initial invite
		 */
		// if (sipRequest.isInitial()) {
		//
		// String origMinSE =
		// sipRequest.getHeader(PhConstants.SESSION_EXPIRE_HEADER);
		//
		// if (origMinSE != null) {
		//
		// long origMinSEValue = Long.parseLong(origMinSE);
		//
		// String origMinSEallowed = SipProtocolConfig
		// .getConfigData(SipProtocolConfig.ORIG_ALLOWED_MIN_SE);
		//
		// long origMinSEAllowedValue = Long.parseLong(origMinSEallowed);
		//
		// if (origMinSEValue < origMinSEAllowedValue) {
		//
		// logger.error(origLegCallId +
		// ":: INVITE with this lower min SE is not allowed");
		//
		// SipServletResponse
		// response=sipRequest.createResponse(SipServletResponse.SC_INTERVAL_TOO_BRIEF);
		// response.setHeader(PhConstants.MIN_SE_HEADER, origMinSEallowed);
		// response.send();
		//
		// return true;
		// }
		//
		// }
		//
		// }

		// Store received SDP for future reference
		boolean isSdpReceived = SipProtocolUtil.setReceivedSdp(sipRequest,
				callData);

		/**
		 * commenting it for now and allowing invite without sdp
		 */
		// if (!isSdpReceived
		// && !PhConstants.TRUE.equals(appSession
		// .getAttribute(PhConstants.X_ISC_SVC))) {
		// // As of now, SN does not support INVITE without SDP except
		// // inter-service communication
		// logger.error(origLegCallId
		// + ":: INVITE without SDP is not supported.");
		//
		// callData.set(
		// CallDataAttribute.NP_REASON_FOR_RELEASE,
		// SipProtocolRelReasonCode.INVITE_RCVD_WO_SDP);
		// handleInvalidInvite(sipRequest);
		// return true;
		// }
		LegData legData = SipProtocolUtil.getLegDataForSipSession(appSession,
				sipRequest.getSession(), callData);
		if (isSdpReceived) {
			legData.set(LegDataAttributes.NP_IS_OFFER_RECEIVED,
					PhConstants.TRUE);
		}
		return false;
	}

	/**
	 * This method initiate media server interaction for orig leg
	 * 
	 * @param appSession
	 *            represents an instance of SipApplicationSession
	 * @param earlyMedia
	 *            represent a boolean variable
	 * @throws IllegalStateException
	 * @throws MediaServerException
	 * @throws IllegalArgumentException
	 * @throws MalformedURLException
	 * @throws Exception
	 */
	public static void initiateMediaServerConnection(
			SipApplicationSession appSession, CallData callData, String legId)
			throws Exception {
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(legId));

		boolean earlyMedia = (!PhConstants.TRUE.equals(legData
				.get(LegDataAttributes.NP_IS_CHARGEABLE_ANN)));
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside initiateMediaServerConnection");
			logger.debug(origLegCallId + ":: Connect " + legId + " to MS");
			logger.debug(origLegCallId + ":: Increment IVR connection count");
		}
		try {
			Integer ivrConnectCnt = (Integer) legData
					.get(LegDataAttributes.P_IVR_CONNECTION_COUNT);
			if (ivrConnectCnt == null) {
				ivrConnectCnt = 0;
			}
			ivrConnectCnt++;
			legData.set(LegDataAttributes.P_IVR_CONNECTION_COUNT, ivrConnectCnt);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Setting the IS_SDP_EXCHANGED_WITH_ORIG as true.");
			}
			// appSession.setAttribute(PhConstants.IS_SDP_EXCHANGED_WITH_ORIG,
			// PhConstants.TRUE);

			// Stop Session Refresh timer for this leg as this will be taken
			// care by Ms-Sbb now
			SipProtocolUtil.stopSessionRefreshTimer(appSession, legData);

			/*
			 * In case of CONNECT_TERM_IVR: ACK would be pending on term leg.
			 * Because on receiving 200 OK from term, mPH calls service without
			 * sending ACK to term.So send that ACK
			 */
			SipServletRequest legPendingAck = (SipServletRequest) legData
					.get(LegDataAttributes.P_PENDING_ACK);
			boolean isDialOutCall = false;
			if (callData.get(CallDataAttribute.P_DIALOUT) != null) {
				isDialOutCall = (Boolean) callData
						.get(CallDataAttribute.P_DIALOUT);
			}

			if (legPendingAck != null && !isDialOutCall) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Send pending ACK on leg "
							+ legId);
				}
				legPendingAck.send();
				legData.remove(LegDataAttributes.P_PENDING_ACK);
			} else if (legPendingAck != null && isDialOutCall) {
				appSession.setAttribute("P_PENDING_ACK",
						legData.get(LegDataAttributes.P_PENDING_ACK));
				legData.remove(LegDataAttributes.P_PENDING_ACK);
			}

			GroupedMsSessionController msController = SipProtocolUtil
					.getMsController(appSession, legData, legId);

			/*
			 * This attribute will be used when mPH receives callBacks from
			 * Ms-Sbb. mPH uses this attribute to obtain LegData from callData
			 */
			msController.setAttribute(PhConstants.LEG_ID, legId);

			/*
			 * Ms-Sbb after sending INFO to MS, starts a timer of 100 sec by
			 * default and if response INFO is not received within this time
			 * period it sends PLAY_FAILED to service. To avoid to start that
			 * timeout setting timer=0 at msController
			 */
			/*
			 * Comment below line. Fix for SBTM-UAT-780 and SBTM-UAT-795
			 */
			// msController.setTimeout(0);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Media Server EARLY_MEDIA flag = " + earlyMedia);
			}
			SipServletRequest legInitialReq = SipProtocolUtil.getInitialInvite(
					appSession, legData);
			SipSession legSipSession = legInitialReq.getSession();

			/*
			 * Fix for SBTM-UAT-745. If any leg is already connected to PH like
			 * in Service Guidance B leg is already connected to PH, then for
			 * that leg A/B can send Re-Invite for session refresh/SDP change.
			 * So while connecting that leg[A/B] to MS we need to paas the SDP
			 * that PH last sent on that leg. So that in future Ms-Sbb can
			 * respond to these SDP change requests from A/B leg. So
			 * precautionary setting these attributes here also.
			 */

			MultipartBody lastSentSdpOnTermLeg = (MultipartBody) legData
					.get(LegDataAttributes.P_LAST_SENT_SDP);
			if (lastSentSdpOnTermLeg != null) {
				msController.setAttribute(
						SBBOperationContext.ATTRIBUTE_SDP_PARTY_B,
						lastSentSdpOnTermLeg.getContent());
				msController.setAttribute(
						SBBOperationContext.ATTRIBUTE_SDP_PARTY_B_CONTENT_TYPE,
						lastSentSdpOnTermLeg.getContentType());
			}

			/*
			 * This attribute is required by MS-Sbb to take care of the case,
			 * when service has called MsSbb.connect() and Ms-sbb receives SDP
			 * change request from A/B Party. Then MS-Sbb need to check if the
			 * new received sdp has changed or the same as the old SDP. So we
			 * need to set this attribute so that MS-Sbb can compare new
			 * received sdp in Re-INVITE with this atttribute sdp
			 */
			MultipartBody currentReceivedSdpOnOrigLeg = (MultipartBody) legData
					.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

			if (logger.isDebugEnabled()) {
				logger.debug("currentReceivedSdpOnOrigLeg : "
						+ currentReceivedSdpOnOrigLeg.getContent());
				logger.debug("currentReceivedSdpOnOrigLeg.getContentType() : "
						+ currentReceivedSdpOnOrigLeg.getContentType());
			}
			if (currentReceivedSdpOnOrigLeg != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("setting sttrib11");
				}
				legSipSession.setAttribute(SBBOperationContext.ATTRIBUTE_SDP,
						currentReceivedSdpOnOrigLeg.getContent());
				appSession.setAttribute("B_PARTY_SDP",
						currentReceivedSdpOnOrigLeg.getContent());

			}

			String rtpTunnellingEnabled = SipProtocolConfig
					.getConfigData(SipProtocolConfig.RTP_TUNNELLING);
			if (rtpTunnellingEnabled != null
					&& rtpTunnellingEnabled.equals("1")) {
				msController.setAttribute(SBB.RTP_TUNNELLING, 1);
			}

			String rtpTunnellingCode = SipProtocolConfig
					.getConfigData(SipProtocolConfig.RTP_TUNNELLING_18X_CODE);
			if (rtpTunnellingCode != null) {
				msController.setAttribute(SBB.RTP_TUNNELLING_18X_CODE,
						rtpTunnellingCode);
			}

			Boolean relay2xxReponse = Boolean.valueOf(SipProtocolConfig
					.getConfigData(SipProtocolConfig.RELAY_2XX_IN_EARLY_MEDIA));
			if (relay2xxReponse) {
				msController.setAttribute(SBB.RELAY_2XX_IN_EARLY_MEDIA,
						relay2xxReponse);
			}

			String pChargingVector = (String) callData
					.get(CallDataAttribute.P_CHARGE_VECTOR);
			if (pChargingVector != null) {

				if (logger.isDebugEnabled()) {
					logger.debug("setting P_CHARGE_VECTOR in appSession : "
							+ pChargingVector);
				}
				appSession.setAttribute(MsSessionController.P_CHARGE_VECTOR,
						pChargingVector);
			}

			String pChargingVectorForSuccess = (String) callData
					.get(CallDataAttribute.P_CHARGE_VECTOR_SUCCESS);
			if (pChargingVectorForSuccess != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("setting P_CHARGE_VECTOR_SUCCESS in appSession : "
							+ pChargingVectorForSuccess);
				}
				appSession.setAttribute("P_CHARGE_VECTOR_SUCCESS",
						pChargingVectorForSuccess);
			}

			if (PhConstants.TRUE.equals(legData
					.get(LegDataAttributes.NP_IS_CHARGEABLE_ANN))
					|| legSipSession.getState() == javax.servlet.sip.SipSession.State.CONFIRMED) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Forcefully setting early media flag as FALSE");
					logger.debug(origLegCallId
							+ ":: Because either it is ChargeableAnn case OR "
							+ legId + " is confirmed");
				}
				earlyMedia = false;
			}

			if (relay2xxReponse) {
				if (logger.isDebugEnabled()) {
					logger.debug("Forcefully setting early media as true, as need to relay 2xx response in early media");
				}

				earlyMedia = true;
			}

			if (earlyMedia) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Set early media flag as TRUE on msController");
				}
				msController.setAttribute(SBB.EARLY_MEDIA, "true");
			}

			boolean isSdpSentEarlier = PhConstants.TRUE.equals(legData
					.get(LegDataAttributes.NP_IS_SDP_SENT));

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: NP_IS_SDP_SENT "
						+ isSdpSentEarlier);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: NP_IS_OFFER_RECEIVED "
						+ legData.get(LegDataAttributes.NP_IS_OFFER_RECEIVED));
			}

			Object ss7State = legData.get(LegDataAttributes.P_LEG_SS7_STATE);

			// boolean isNoInviteWithoutSdp =
			// PhConstants.TRUE.equals(SipProtocolUtil
			// .getConfig(
			// SipProtocolConfig.NO_INVITE_WITHOUT_SDP));

			// if (!isNoInviteWithoutSdp &&

			if (ss7State != null
					&& (ss7State.equals(InapCallStates.ASSIST) || ss7State
							.equals(AinCallStates.ASSIST))) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: invite with SDP should go to IVR in ASSIST Flow "
							+ legId);
				}

			} else if (isSdpSentEarlier
					|| !PhConstants.TRUE.equals(legData
							.get(LegDataAttributes.NP_IS_OFFER_RECEIVED))) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SDP has already been sent on leg " + legId);
					logger.debug(origLegCallId
							+ ":: Set MS_INVITE_WITHOUT_SDP as TRUE on msController");
				}
				msController.setAttribute(
						MsSessionController.MS_INVITE_WITHOUT_SDP,
						PhConstants.TRUE);
			}
//<<<<<<< HEAD
			// else{
			// if (logger.isDebugEnabled()) {
			// logger.debug(origLegCallId
			// + ":: Send invite to ms with sdp only "
			// + legId);
			// }
			// msController.getServletContext().setInitParameter("NO_INVITE_WITHOUT_SDP",
			// PhConstants.TRUE);
			// }
//=======
			
			 String noinviteWOSDP=SipProtocolConfig.getConfigData(SipProtocolConfig.NO_INVITE_WITHOUT_SDP);
			 
			 msController.getServletContext().setInitParameter(SipProtocolConfig.NO_INVITE_WITHOUT_SDP, noinviteWOSDP);
			
			if (PhConstants.TRUE.equals(legData
					.get(LegDataAttributes.NP_IS_OFFER_RECEIVED))) {
				legData.set(LegDataAttributes.NP_IS_OFFER_RECEIVED,
						PhConstants.FALSE);
			}
//						else{
//							if (logger.isDebugEnabled()) {
//								logger.debug(origLegCallId
//										+ ":: Send invite to ms with sdp only "
//										+ legId);
//							}
//							msController.getServletContext().setInitParameter("NO_INVITE_WITHOUT_SDP", PhConstants.TRUE);
//						}
//>>>>>>> MPH10.1_DEV

			msController.setEventListener(cMsEventListener);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Sending Connection request to Media Controller SBB for "
						+ legId);
			}

			/*
			 * Fix for SBTM-UAT-928. As per new Service guidance call-flow, orig
			 * sip session will be in connected state while invoking Ms-Sbb to
			 * connect caller to MS
			 */
			int msCapabilities = PhConstants.MS_CAPABILITIES;

			// MediaServer.CAPABILITY_DIGIT_COLLECTION
			// | MediaServer.CAPABILITY_VAR_ANNOUNCEMENT
			// | MediaServer.CAPABILITY_AUDIO_RECORDING;
			String vxmlFilePath = (String) legData
					.get(LegDataAttributes.NP_VXML_FILE_PATH);
			if (legSipSession.getState() == javax.servlet.sip.SipSession.State.CONFIRMED) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Send connect request to msController[Dialout]");
				}

				legSipSession.setAttribute(PhConstants.DIALOG_STATE,
						PhConstants.DIALOG_STATE_CONFIRMED);
				/*
				 * Sometimes SAS throws runtime exception. So precautionary
				 * adding following try catch block to ignore that exception
				 */
				try {
					msController.addA(legSipSession);
				} catch (Throwable e) {
					logger.warn(origLegCallId
							+ " A already added in Orig MS Sip Session");
				}
				msController.activate(legSipSession);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Invoke dialout on MSSbb for " + legId
							+ " leg call id " + origLegCallId);
				}

				msController.dialOut(msCapabilities);
			} else if (vxmlFilePath == null || vxmlFilePath.isEmpty()) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Send connect request to msController[Connect]");
				}
				msController.connect(legInitialReq, msCapabilities);
			} else {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Send playVoiceXmlOnConnect request to msController");
				}

				vxmlFilePath = vxmlFilePath.replaceAll(
						PhConstants.FLOATING_IP_STR, SipProtocolUtil
								.getConfig(SipProtocolConfig.FLOATING_IP));
				vxmlFilePath = vxmlFilePath.replaceAll(
						PhConstants.HTTP_LISTENER_PORT_STR,
						SipProtocolUtil.getConfig(SipProtocolConfig.HTTP_PORT));

				legData.set(LegDataAttributes.NP_VXML_FILE_PATH, vxmlFilePath);
				msController.playVoiceXmlOnConnect(legInitialReq, new URL(
						vxmlFilePath));
			}

			legData.set(LegDataAttributes.P_LEG_SIP_STATE,
					State.MS_CONN_IN_PROGRESS);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: setInvalidateWhenReady(false)");
			}
			msController.getB().setInvalidateWhenReady(false);

			// /*
			// * Added for ivr call not getting cleaned in initial state for
			// alternate routing kind of scenarios.it may be nay scenarios
			// * actullay in which we are dropping ivr leg when we have not
			// received any provisional/success response from it yet
			// */
			msController.getB().setAttribute(PhConstants.IVR_LEG,
					PhConstants.TRUE);
			msController.getB().setAttribute(PhConstants.LEG_ID, legId);
			appSession.setAttribute(
					PhConstants.SESSION_ID_WITH_ORIG_SDP_EXCHANGED,
					msController.getB().getId());

			/*
			 * Set the connection type in appsession to identify the sip session
			 * in case FT happens before receiving 200 OK from IVR.
			 */
			callData.set(CallDataAttribute.P_LEG_ID_CONNECTED_TO_IVR, legId);

			legSipSession
					.removeAttribute(PhConstants.SESSION_READY_TO_INVALIDATE);
			legData.remove(LegDataAttributes.P_IVR_SESSION_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Replicate the session on peer node");
			}
			PhUtilityServices
					.getInstance(
							(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getAppDataReplicator().doReplicate(appSession);

			MeasurementCounter measurementCounter = PhMeasurementService
					.getInstance().getMeasurementCounter(Protocol.SIP);
			if (measurementCounter != null) {
				measurementCounter
						.incrementMediaServerCount(MSConnectionMode.SIP);
			}
		} catch (Exception ex) {
			/*
			 * Fix for bug#16008
			 */
			/*
			 * In this case, service might need to continue the call. So the
			 * state is set as SERVICE_LOGIC to re-initialize the orig leg state
			 * machine.
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Failed to connect " + legId
						+ " to IVR. ", ex);
			}
			logger.error(origLegCallId + ":: Failed to connect " + legId
					+ " to IVR. " + ex.getMessage());
			SipSession legInitialInv = SipProtocolUtil
					.getSipSessionFromSessionId(origLegCallId, appSession,
							(String) legData
									.get(LegDataAttributes.P_SESSION_ID));
			ServiceInterface serviceHandler = PhUtilityServices.getInstance(
					(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();
			legInitialInv.setHandler(serviceHandler.getServletName());
			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.INIT);

			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.IVR_CONNECT_FAILED);
			// Create Event and inform service
			Event event = new Event(EventType.EVENT_MS_FAILURE, Protocol.SIP,
					legId);
			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);
		}
	}

	/**
	 * <p>
	 * For response received for ORIG_CONNECTION_TYPE Start session refresh
	 * timer on orig leg and check the orig sip state as follows:
	 * <ul>
	 * <li>If OrigSipCallState is CONNECTED state, Send ACK for received success
	 * response,Send pending success response with received SDP to term leg and
	 * Start session refresh timer on term leg.
	 * <li>If OrigSipCallState is CONNECTION_IN_PROGRESS, Send ACK to term with
	 * received SDP and Inform service that resync call completed or term
	 * connection connectedbased on last action.
	 * </ul>
	 * <p>
	 * For response received for ORIG_CONNECTION_TYPE Start session refresh
	 * timer on term leg.Create ACK only if it is not the PBX Dial In case. In
	 * case of PBX Dial In, the ACK would be send with MS SDP by MSSbb. Check
	 * the term sip state as follows:
	 * <ul>
	 * <li>If TermSipCallState is CONNECTED Send ACK for received success
	 * response, Send pending success response to orig with received SDP, Start
	 * session refresh timer on orig leg.
	 * <li>If TermSipCallState is CONNECTION_IN_PROGRESS and Last action
	 * performed is CONNECT_TERM_IVR then Initiate term media server connection
	 * else Send hold update to party A.
	 * <li>If TermSipCallState is CONNECTION_IN_PROGRESS and Last action
	 * performed is CONNECT_TERM or RESYNC_CALL then check for orig sip state.
	 * For CONFIRMED orig sip state ack for this response is held back and
	 * reinvite is sent as offer to orig leg. For EARLY or INITIAL orig sip
	 * state ack for this response is held back and update is sent as offer to
	 * orig leg provided SDP is changed from last sent. Else hold the term ACK
	 * till answer received from orig.Also start the timers and notify the
	 * service of event.
	 * <li>In all other cases call is dropped with cv=41.
	 * </ul>
	 * For any success response received in invalid state, drop call with CV
	 * =41. This method handles the INVITE success response
	 * 
	 * @param sipResponse
	 *            represents the instance of SipServletResponse
	 * @throws Exception
	 */

	public static void handleInviteSuccess(SipServletResponse sipResponse,
			CallData callData, LegData legData) throws Exception {
		SipApplicationSession appSession = sipResponse.getApplicationSession();
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside handleInviteSuccessResponse");
		}

		/**
		 * SIP-T support
		 */
		if (legData.get(LegDataAttributes.P_SIGNALING_TYPE_KEY) == SignalingTypeEnum.SIGNALING_TYPE.SIP_T) {
			SipIsupHelper.setReceivedAnm(sipResponse, origLegCallId);
		}
		// Set negotiated session expiry time
		SipProtocolUtil.setSessionExpiryTime(sipResponse, legData,
				origLegCallId);

		// Fetch leg details
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: INVITE success response is for "
					+ connType + " at state " + state);
		}
		
		if(connType==ConnectionType.ORIG_CONNECTION&& state == State.PROXY_CONNECT){
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Return as it is proxy");
			}
			return;
		}

		if (connType == ConnectionType.TERM_CONNECTION
				&& !PhConstants.TRUE.equals(callData
						.get(CallDataAttribute.P_REINVITE_SENT_TO_TERM))
				&& state == State.CONNECTED) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: 200 ok received from term when it is in connected state and reinvite to term was not sent...");
				logger.debug("Looks like mutilple final response scenario so send BYE to this response and return");
			}
			SipProtocolMessageCreator.createByeRequest(origLegCallId,
					sipResponse.getSession(), 31, false).send();
			sipResponse.getSession().setAttribute(PhConstants.STALE,
					PhConstants.TRUE);
			return;
		}

		// Fetch peer leg data
		LegData peerLegData = SipProtocolUtil
				.getPeerLegData(callData, connType);

		State peerstate = (State) peerLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);

		// Set AppSessionTimeout
		int maxSessionTimeout = SipProtocolUtil.getMaxSessionExpireTime(
				appSession, origLegCallId, legData, peerLegData);
		if (maxSessionTimeout > 60) {
			maxSessionTimeout = (maxSessionTimeout / 60) + 1;
			SipProtocolUtil.setAppSessionTimeout(appSession, maxSessionTimeout,
					origLegCallId);
		}

		// Store received SDP for future reference
		SipProtocolUtil.setReceivedSdp(sipResponse, callData);

		// Start session refresh timer
		SipProtocolUtil.startSessionRefreshTimer(appSession, legData);

		/*
		 * This is 200 OK response received for initial INVITE that was sent by
		 * mPH to term leg. Inform service that term leg is connected now
		 */
		Action lastAction = (Action) callData
				.get(CallDataAttribute.P_CURRENT_ACTION);
		Action.CONNECTIONMODE mode = lastAction.getConnectionMode();

		Action lastLegAction = (Action) legData
				.get(LegDataAttributes.P_LEG_CURRENT_ACTION);

		SipServletRequest peerLegInitialInv = SipProtocolUtil.getInitialInvite(
				appSession, peerLegData);

		if (lastAction != null
				&& lastAction.getActionType() == Action.ActionType.ACTION_CONNECT_PARALLEL) {
			SipRingParallel.handleSuccessResponse(origLegCallId, sipResponse,
					peerLegInitialInv, callData, legData);
			return;
		}

		if (connType == ConnectionType.TERM_CONNECTION
				&& state == State.CONN_IN_PROGRESS
				&& (lastAction.getActionType() == ActionType.ACTION_CONNECT
						|| lastAction.getActionType() == ActionType.ACTION_TRANSFER_CONNECT
						|| lastAction.getActionType() == ActionType.ACTION_TRY_REDIRECT_CONTACTS
						|| lastAction.getActionType() == ActionType.ACTION_CONNECT_PARALLEL
						|| lastAction.getActionType() == ActionType.ACTION_CONNECT_SERIAL || (lastLegAction != null && (lastLegAction
						.getActionType() == ActionType.ACTION_TRY_REDIRECT_CONTACTS || lastLegAction
						.getActionType() == ActionType.ACTION_CONNECT)))) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: This success response is for INITIAL INVITE that was sent to term");
			}
			if (PhConstants.TRUE.equals(legData
					.get(LegDataAttributes.NP_IS_OFFER_SENT))) {
				/*
				 * Offer was sent to term in initial INVITE and answer has ben
				 * received for this. So there is no usage of storing ACK and
				 * SDPhas ben exchanged
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Offer was sent to term in initial INVITE");
				}
				legData.remove(LegDataAttributes.NP_IS_OFFER_SENT);

				appSession.setAttribute(PhConstants.INVITE_SUCCESS_RESPONSE,
						sipResponse);

				if ((peerstate == State.CONN_IN_PROGRESS || peerstate == State.INIT) // init
																						// state
																						// is
																						// added
																						// for
																						// case
																						// when
																						// 180
																						// is
																						// not
																						// received
																						// from
																						// term
																						// so
																						// orig
																						// state
																						// will
																						// be
																						// init
						&& Action.CONNECTIONMODE.B2BUA.equals(mode)) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Orig state is connection in progress so forward success response to orig as connection mode is B2BUA");
					}


					MultipartBody sdpContent=(MultipartBody)legData.get(
							LegDataAttributes.P_CURRENT_RECEIVED_SDP);

					//					SipServletResponse response = peerLegInitialInv
					//							.createResponse(SipServletResponse.SC_OK);
					SipServletResponse response =SipProtocolMessageCreator.createSuccessResponseInvite(peerLegInitialInv, sdpContent);//(origLegCallId, peerLegInitialInv, SipServletResponse.SC_OK, callData);
					//					response.setContent(sipResponse.getContent(),
					//							sipResponse.getContentType());

					// Store received SDP for future reference
					SipProtocolUtil.setSentSdp(response, peerLegData, origLegCallId);
					peerLegData.set(
							LegDataAttributes.P_LEG_SIP_STATE, State.CONNECTED);

					response.send();

				}

				/*
				 * Changes done for P-Charge-Vector
				 */
				SipProtocolMessageCreator.createAckRequest(origLegCallId,
						sipResponse, callData).send();

			} else {
				/*
				 * Offer was NOT sent to term in initial INVITE. And offer has
				 * been received in 200OK . So store ACK for term in LegData to
				 * send SDP to term after getting new SDP from orig
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Offer was NOT sent to term in initial INVITE");
				}
				legData.set(LegDataAttributes.NP_IS_OFFER_RECEIVED,
						PhConstants.TRUE);
				legData.set(LegDataAttributes.P_PENDING_ACK,
						SipProtocolMessageCreator.createAckRequest(
								origLegCallId, sipResponse, callData));
			}

			SipProtocolUtil.stopTimer(appSession, PhConstants.NO_ANSWER_TIMER);
			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.CONNECTED);

			// Set call connected time
			callData.set(CallDataAttribute.P_CALL_CONNECT_TIME, new Date());
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inform service that TERM connected");
			}

			ServiceInterface serviceHandler = PhUtilityServices.getInstance(
					(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();
			// .getAppChainManager()
			// .getServiceInterface(
			// (String) callData
			// .get(CallDataAttribute.SERVICE_ID));

			Event event = null;
			if (lastAction.getActionType() == ActionType.ACTION_TRANSFER_CONNECT) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Inform service for successfull call transfer connect");
				}
				legData.set(LegDataAttributes.P_LEG_SIP_STATE,
						State.CALL_TRANSFERED);
				peerLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
						State.CALL_TRANSFERED);

				event = new Event(EventType.EVENT_TRANSFER_SUCCESS,
						Protocol.SIP, (String) sipResponse.getSession()
								.getAttribute(PhConstants.LEG_ID));

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);

			} else {

				SipProtocolUtil.startMaxCallDurationTimer(origLegCallId,
						callData, appSession);
				event = new Event(EventType.EVENT_SUCCESS, Protocol.SIP,
						(String) sipResponse.getSession().getAttribute(
								PhConstants.LEG_ID));
				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}
			return;
		}

		/*
		 * This final response is not for initial INVITE because mPH sends
		 * initial INVITE only to term legs and that too when requested by
		 * service by sending ACTION CONNECT_TERM. And if service had sent
		 * CONNECT_TERM ,then on the above if condition mPH would have called
		 * service. So this code will not execute
		 */
		SipServletRequest ackReqForThisLeg = SipProtocolMessageCreator
				.createAckRequest(origLegCallId, sipResponse, callData);// .send();//sipResponse.createAck();

		if (PhConstants.TRUE.equals(legData
				.get(LegDataAttributes.NP_IS_OFFER_SENT))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Offer was sent on " + connType);
				logger.debug(origLegCallId
						+ ":: Offer-Answer completed. Send empty ACK now");
			}

			// reeta commenting it to not to again send reinvite on 200ok from
			// term when reinvite was already sent on 183 from term
			// legData.removeNonpersistableData(LegDataAttributes.NP_IS_OFFER_SENT);
			ackReqForThisLeg.send();
			if (peerLegData != null) {
				SipServletResponse peerPendingSipResponse = (SipServletResponse) peerLegData
						.get(LegDataAttributes.P_PENDING_SUCC_RESP);
				SipServletRequest peerPendingAck = (SipServletRequest) peerLegData
						.get(LegDataAttributes.P_PENDING_ACK);
				SipServletRequest peerPendingPrack = (SipServletRequest) peerLegData
						.get(LegDataAttributes.NP_PENDING_PRACK);

				if (peerPendingSipResponse != null) {
					/*
					 * This is normal session refresh or Re-INVITE for SDP
					 * change case
					 */
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: success response is pending on peer leg");
					}
					peerLegData.remove(LegDataAttributes.P_PENDING_SUCC_RESP);
					peerLegData.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
					// Set latest SDP content
					peerPendingSipResponse.setContent(sipResponse.getContent(),
							sipResponse.getContentType());
					if (PhConstants.INVITE_REQUEST
							.equals(peerPendingSipResponse.getMethod())) {
						peerLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.CONNECTED);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Pending success Response is for INVITE request. Send response and start session refresh timer on peer leg");
						}
						// Start session refresh timer
						SipProtocolUtil.startSessionRefreshTimer(appSession,
								peerLegData);
					}
					peerPendingSipResponse.send();
					SipProtocolUtil.setSentSdp(peerPendingSipResponse,
							peerLegData, origLegCallId);
				} else if (peerPendingAck != null
						|| (PhConstants.TRUE.equals(peerLegData
								.get(LegDataAttributes.NP_IS_OFFER_SENT)))) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: ACK is pending on peer leg or offer was sent in itniial invite to term so no ack would have been sent to term already");
					}

					if (peerPendingAck != null) {
						peerLegData.remove(LegDataAttributes.P_PENDING_ACK);
						peerLegData
								.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
						// Set latest SDP content
						peerPendingAck.setContent(sipResponse.getContent(),
								sipResponse.getContentType());
						peerPendingAck.send();
						SipProtocolUtil.setSentSdp(peerPendingAck, peerLegData,
								origLegCallId);
					} else if (peerPendingPrack != null) {
						/*
						 * This is normal session refresh or Re-INVITE for SDP
						 * change case
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Prack  is pending on peer leg");
						}
						peerLegData.remove(LegDataAttributes.NP_PENDING_PRACK);
						// Set latest SDP content

						if (PhConstants.FALSE.equals(peerLegData
								.get(LegDataAttributes.NP_IS_OFFER_SENT))) { // set
																				// sdp
																				// is
																				// offer
																				// was
																				// received
																				// in
																				// 18x

							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: offer was not sent so prack should contain answer for 18x");
							}
							peerPendingPrack.setContent(
									sipResponse.getContent(),
									sipResponse.getContentType());
							SipProtocolUtil.setSentSdp(peerPendingPrack,
									peerLegData, origLegCallId);
						} else {

							// PRACK should be sent empty as we can not send new
							// offer in PRACK as offer was sent in initial
							// invite
							// and answer received in 183 , so can not send new
							// offer in PRACK , but will send it in update when
							// 200ok of prack will be rceeived

							MultipartBody sdpContent = (MultipartBody) legData
									.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
							SipServletRequest updateReq = SipProtocolMessageCreator
									.createUpdate(
											peerPendingPrack.getSession(),
											sdpContent, callData);

							// will send it after we received 200 ok for this
							// PRACK

							// Update attribute for future reference
							SipProtocolUtil.setSentSdp(updateReq, peerLegData,
									origLegCallId);
							peerLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
									PhConstants.TRUE);

							peerLegData.set(LegDataAttributes.P_PENDING_UPDATE,
									updateReq);
						}

						peerPendingPrack.send();

					} else {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: ACK is not pending looks like");
						}

					}
					if (((Action) callData
							.get(CallDataAttribute.P_CURRENT_ACTION))
							.getActionType() == ActionType.ACTION_RESYNC_CALL) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Current action is RESYNC_CALL. Inform service that orig/term resync is successfully completed");
						}

						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.CONNECTED);
						peerLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.CONNECTED);

						ServiceInterface serviceHandler = PhUtilityServices
								.getInstance(
										(String) callData
												.get(CallDataAttribute.SERVICE_ID))
								.getServiceHandler();
						Event event = new Event(EventType.EVENT_RESYNC_SUCCESS,
								Protocol.SIP, CallDataAttribute.P_LEG1.name());
						ProtocolRouter.getInstance().execute(event, callData,
								serviceHandler);
						PhUtilityServices
								.getInstance(
										(String) callData
												.get(CallDataAttribute.SERVICE_ID))
								.getAppDataReplicator().doReplicate(appSession);
					}

					// Action
					// lastLegAction=(Action)legData.getPersistableData(LegDataAttributes.P_LEG_CURRENT_ACTION);
					if (((Action) callData
							.get(CallDataAttribute.P_CURRENT_ACTION))
							.getActionType() == ActionType.ACTION_TRY_REDIRECT_CONTACTS
							|| lastLegAction != null
							&& lastLegAction.getActionType() == ActionType.ACTION_TRY_REDIRECT_CONTACTS) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Current action is ACTION_TRY_REDIRECT_CONTACTS. Inform service that orig/term connected successfully completed");
						}

						// Set call connected time
						callData.set(CallDataAttribute.P_CALL_CONNECT_TIME,
								new Date());

						SipProtocolUtil.startMaxCallDurationTimer(
								origLegCallId, callData, appSession);

						ServiceInterface serviceHandler = PhUtilityServices
								.getInstance(
										(String) callData
												.get(CallDataAttribute.SERVICE_ID))
								.getServiceHandler();
						Event event = new Event(EventType.EVENT_SUCCESS,
								Protocol.SIP, CallDataAttribute.P_LEG2.name());
						ProtocolRouter.getInstance().execute(event, callData,
								serviceHandler);
					}

				} else {
					if (logger.isInfoEnabled()) {
						logger.info(origLegCallId
								+ ":: No pending message on peer leg. Do nothing");
					}

					boolean isDialOutCall = false;

					Object dialOut = callData.get(CallDataAttribute.P_DIALOUT);
					if (dialOut != null) {
						isDialOutCall = (Boolean) dialOut;
					}
					if (isDialOutCall) {
						if (logger.isDebugEnabled()) {
							logger.debug("dialOut call, so sending reInvite to term with orig sdp");
						}
						ConnectionType connType1 = (ConnectionType) legData
								.get(LegDataAttributes.P_CONNECTION_TYPE);

						if (ConnectionType.ORIG_CONNECTION.equals(connType1)) {
							LegData termLegData = SipProtocolUtil
									.getPeerLegData(
											callData,
											(ConnectionType) legData
													.get(LegDataAttributes.P_CONNECTION_TYPE));
							SipServletRequest termInitialInvite = SipProtocolUtil
									.getInitialInvite(appSession, termLegData);

							MultipartBody origRcvdSdp1 = (MultipartBody) legData
									.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

							SipServletRequest termReInvReq = SipProtocolMessageCreator
									.createReinviteRequest(termInitialInvite,
											origRcvdSdp1);

							termReInvReq.send();
						}

					}

					/*
					 * in race conditions on A side the leg action set on a will
					 * be resynch
					 */
					if ((lastLegAction != null && lastLegAction.getActionType() == ActionType.ACTION_RESYNC_CALL)) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Current action is RESYNC_CALL. Inform service that orig/term resync is successfully completed");
						}

						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.CONNECTED);
						peerLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.CONNECTED);

						ServiceInterface serviceHandler = PhUtilityServices
								.getInstance(
										(String) callData
												.get(CallDataAttribute.SERVICE_ID))
								.getServiceHandler();
						Event event = new Event(EventType.EVENT_RESYNC_SUCCESS,
								Protocol.SIP, CallDataAttribute.P_LEG1.name());
						ProtocolRouter.getInstance().execute(event, callData,
								serviceHandler);
						PhUtilityServices
								.getInstance(
										(String) callData
												.get(CallDataAttribute.SERVICE_ID))
								.getAppDataReplicator().doReplicate(appSession);
					}
				}

				if (lastAction.getActionType() == ActionType.ACTION_HOLD_CALL
						&& state == State.CONNECTED) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Inform service for successful call hold");
					}

					ServiceInterface serviceHandler = PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getServiceHandler();
					Event event = new Event(EventType.EVENT_HOLD_SUCCESS,
							Protocol.SIP, (String) sipResponse.getSession()
									.getAttribute(PhConstants.LEG_ID));

					ProtocolRouter.getInstance().execute(event, callData,
							serviceHandler);
					return;
				}
			}

		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Offer was not sent by mPH in INVITE to "
						+ connType);
			}

			if (lastAction.getActionType() == ActionType.ACTION_HOLD_CALL
					&& state == State.CONNECTED) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: sending ACK for hold response");
				}

				ackReqForThisLeg.send();
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Inform service for successful call hold");
				}

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(EventType.EVENT_HOLD_SUCCESS,
						Protocol.SIP, (String) sipResponse.getSession()
								.getAttribute(PhConstants.LEG_ID));

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
				return;
			}
			if (sipResponse.getContent() != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SDP received in 200 OK from " + connType);
				}
				MultipartBody lastReceivedSdp = (MultipartBody) legData
						.get(LegDataAttributes.P_LAST_RECEIVED_SDP);
				MultipartBody currentReceivedSdp = (MultipartBody) legData
						.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

				boolean isSdpChanged = SipProtocolUtil.isSdpChanged(
						lastReceivedSdp, currentReceivedSdp, origLegCallId);
				if (isSdpChanged) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: New Offer is received in 200 OK from "
								+ connType);
					}
					
					/**
					 * any bew condition for dialout for proximus
					 */
					if(connType==ConnectionType.ORIG_CONNECTION && state==State.CONN_IN_PROGRESS){
						
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: orig state is conn inprogress and 200ok recived so generating event success as it might be dialot to A case "
									+ connType);
						}
						
						legData.set(
								LegDataAttributes.P_PENDING_ACK,
								ackReqForThisLeg);
						
						ServiceInterface serviceHandler = PhUtilityServices
								.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
						Event event = new Event(EventType.EVENT_SUCCESS,
								Protocol.SIP, CallDataAttribute.P_LEG2.name());
						ProtocolRouter.getInstance().execute(event, callData,
								serviceHandler);
						
						return;

					}					/*
					 * This is the case of RESYNC Call. Because in case of
					 * RESYNC call only mPH sends INVITE without SDP.
					 */
					/*
					 * Send this new SDP to peerleg. If peer Leg is in
					 * INITIAL/EARLY state then send UPDATE otherwise send
					 * Re-INVITE
					 */
					// SipServletRequest peerLegInitialInv = SipProtocolUtil
					// .getInitialInvite(appSession, peerLegData);
					SipSession peerSipSession = peerLegInitialInv.getSession();

					switch (peerSipSession.getState()) {
					case INITIAL:
					case EARLY: {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Peer leg is in INITIAL/EARLY state. Send UPDATE on peer leg with new SDP received from "
									+ connType);
						}
						MultipartBody currentRcvdSdpFromLeg = (MultipartBody) legData
								.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
						SipServletRequest updateReq = SipProtocolMessageCreator
								.createUpdate(peerSipSession,
										currentRcvdSdpFromLeg, callData);

						updateReq.send();
						// Update attribute for future reference
						SipProtocolUtil.setSentSdp(updateReq, peerLegData,
								origLegCallId);
						peerLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
								PhConstants.TRUE);
						legData.set(LegDataAttributes.P_PENDING_ACK,
								ackReqForThisLeg);

						SipServletResponse peerPendingSuccRes = SipProtocolMessageCreator
								.createSuccessResponseInvite(peerLegInitialInv,
										currentRcvdSdpFromLeg);
						peerLegData.set(LegDataAttributes.P_PENDING_SUCC_RESP,
								peerPendingSuccRes);

						break;
					}
					case CONFIRMED: {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Peer leg is in CONFIRMED state. Send Re-INVITE on peer leg with new SDP received from "
									+ connType);
						}
						
						MultipartBody currentRcvdSdpFromLeg = (MultipartBody) legData
								.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
						SipServletRequest reInviteReq = SipProtocolMessageCreator
								.createReinviteRequest(peerLegInitialInv,
										currentRcvdSdpFromLeg);
						reInviteReq.send();
						// Update attribute for future reference
						peerLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
								PhConstants.TRUE);
						SipProtocolUtil.setSentSdp(reInviteReq, peerLegData,
								origLegCallId);

						legData.set(LegDataAttributes.P_PENDING_ACK,
								ackReqForThisLeg);
						}

						/**
						 * Reeta commented it donot know why this code is
						 * iimeplemented her eony reinvite should be sent to
						 * peer leg . what is this response for
						 * 
						 */
						// SipServletResponse peerPendingSuccRes =
						// SipProtocolMessageCreator
						// .createSuccessResponseInvite(peerLegInitialInv,
						// currentRcvdSdpFromLeg);
						// peerLegData.setPersistableData(LegDataAttributes.P_PENDING_SUCC_RESP,
						// peerPendingSuccRes);
						break;
					
					default: {
						logger.warn(origLegCallId
								+ ":: Peer leg is in TERMINATED state. Send ACK for received 200 OK on "
								+ connType);
						MultipartBody lastSentSdp = (MultipartBody) legData
								.get(LegDataAttributes.P_LAST_SENT_SDP);
						ackReqForThisLeg.setContent(lastSentSdp.getContent(),
								lastSentSdp.getContentType());
						ackReqForThisLeg.send();
						break;
					}
					}
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Already nagotiated SDP is received in 200 OK from "
								+ connType);
					}

					/**
					 * adding below for laternate routing:
					 */

					// Send pending 200 OK to orig leg
					// Send pending success response to Orig leg
					SipServletResponse pendingSuccResp = (SipServletResponse) peerLegData
							.get(LegDataAttributes.P_PENDING_SUCC_RESP);

					if (pendingSuccResp != null) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Already nagotiated SDP is received in 200 OK from "
									+ connType);
						}

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Send pending success response to peer");
							logger.debug(origLegCallId
									+ ":: Start session refresh timer");
						}

						// Start orig leg session refresh timer
						SipProtocolUtil.startSessionRefreshTimer(appSession,
								legData);

						// Set orig leg state to CONNECTED
						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.CONNECTED);

						// Send success response to orig leg and update sent sdp
						pendingSuccResp.setContent(sipResponse.getContent(),
								sipResponse.getContentType());
						pendingSuccResp.send();

						legData.remove(LegDataAttributes.P_PENDING_SUCC_RESP);

						// Update attribute for future reference
						SipProtocolUtil.setSentSdp(pendingSuccResp, legData,
								origLegCallId);

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Inform service that orig and term connected");
						}

						if (((Action) callData
								.get(CallDataAttribute.P_CURRENT_ACTION))
								.getActionType() == ActionType.ACTION_RESYNC_CALL) {

							legData.set(LegDataAttributes.P_LEG_SIP_STATE,
									State.CONNECTED);
							peerLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
									State.CONNECTED);

							Event event = new Event(
									EventType.EVENT_RESYNC_SUCCESS,
									Protocol.SIP,
									CallDataAttribute.P_LEG2.name());
							ProtocolRouter
									.getInstance()
									.execute(
											event,
											callData,
											PhUtilityServices
													.getInstance(
															(String) callData
																	.get(CallDataAttribute.SERVICE_ID))
													.getServiceHandler());
						}

					}
					// else{
					/**
					 * this code was written to take care of null sdp sent to
					 * term scenario then reinvite should go to A. commenting
					 * for time being as we have not tetsed this code yet.. tis
					 * is actually required code for null reinvite to term case,
					 * we will uncomment it when we will do tetsing of reinvite
					 * nulll to B
					 */
					//
					// SipServletRequest peerLegInitialInv = SipProtocolUtil
					// .getInitialInvite(appSession, peerLegData);
					// SipSession peerSipSession =
					// peerLegInitialInv.getSession();
					//
					// switch (peerSipSession.getState()) {
					// case INITIAL:
					// case EARLY:
					// break;
					// case CONFIRMED: {
					// if (logger.isDebugEnabled()) {
					// logger.debug(origLegCallId
					// +
					// ":: Peer leg is in CONFIRMED state. Send Re-INVITE on peer leg with new SDP received from "
					// + connType +" Need to resynch both parties again .");
					// }
					// MultipartBody currentRcvdSdpFromLeg = (MultipartBody)
					// legData
					// .getPersistableData(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
					// SipServletRequest reInviteReq = SipProtocolMessageCreator
					// .createReinviteRequest(peerLegInitialInv,
					// currentRcvdSdpFromLeg);
					// reInviteReq.send();
					// // Update attribute for future reference
					// peerLegData.setNonpersistableData(
					// LegDataAttributes.NP_IS_OFFER_SENT,
					// PhConstants.TRUE);
					// SipProtocolUtil.setSentSdp(reInviteReq, peerLegData,
					// origLegCallId);
					//
					// legData.setPersistableData(
					// LegDataAttributes.P_PENDING_ACK,
					// ackReqForThisLeg);
					// break;
					// }
					// default: {
					// logger.warn(origLegCallId
					// + ":: Peer leg is in TERMINATED state. ");
					// break;
					// }
					// }
					//
					// return;
					// }

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: send ACK to this party");
					}

					MultipartBody lastSentSdp = (MultipartBody) legData
							.get(LegDataAttributes.P_LAST_SENT_SDP);
					ackReqForThisLeg.setContent(lastSentSdp.getContent(),
							lastSentSdp.getContentType());
					ackReqForThisLeg.send();
				}

			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SDP is NOT received in 200 OK from "
							+ connType);
					logger.debug(origLegCallId + ":: Send ACK");
				}
				ackReqForThisLeg.send();
			}
		}
	}

	/*
	 * This method is used
	 */
	public static void resyncCall(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleResyncCall");
		}
		// Orig Leg Info
		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		SipServletRequest origInitialInv = SipProtocolUtil.getInitialInvite(
				appSession, origLegData);
		MultipartBody origRcvdSdp = (MultipartBody) origLegData
				.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
		State origState = (State) origLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);

		// Term Leg Info
		LegData termLegData = SipProtocolUtil.getPeerLegData(callData,
				(ConnectionType) origLegData
						.get(LegDataAttributes.P_CONNECTION_TYPE));

		SipServletRequest termInitialInv = SipProtocolUtil.getInitialInvite(
				appSession, termLegData);
		MultipartBody termLastSentSdp = (MultipartBody) termLegData
				.get(LegDataAttributes.P_LAST_SENT_SDP);
		MultipartBody termCurrentRcvdSdp = (MultipartBody) termLegData
				.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
		State termState = (State) termLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Orig state " + origState
					+ " Term State " + termState);
		}
		if (SipProtocolUtil.isSdpChanged(origRcvdSdp, termLastSentSdp,
				origLegCallId)) {
			/*
			 * This is the case when
			 * 
			 * 1. initial INVITE has been sent to term without SDP [Call
			 * Forwarding] (OR)
			 * 
			 * 2. TERM has been connected to announcement
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: SDP received from orig is NOT same as SDP sent to term leg");
				logger.debug(origLegCallId
						+ ":: So SDP nagotiation is required between orig and term to resync call");
			}


			if (PhConstants.TRUE
					.equals(termLegData
							.get(LegDataAttributes.NP_IS_OFFER_RECEIVED)) 
							&& (termInitialInv.getSession().getState()!=SipSession.State.CONFIRMED)) {

				/*
				 * -> SDP has been received from term but not answered yet. And
				 * there is pending ACK on term. Send this ACK on term with new
				 * SDP obtained from orig. So get new SDP from orig
				 */

				if (PhConstants.TRUE
						.equals(origLegData
								.get(LegDataAttributes.NP_IS_OFFER_RECEIVED)) 
								&& (origInitialInv.getSession().getState()!=SipSession.State.CONFIRMED)) {

					/*
					 * Orig OFFER has not been answered yet. So send this SDP to
					 * term in pending ACK. Although this case will never happen
					 */
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: SDP answer is pending on orig and term");
					}
					SipServletRequest termPendingAck = (SipServletRequest) termLegData
							.get(LegDataAttributes.P_PENDING_ACK);
					if (termPendingAck != null) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: ACK is pending on term so send it with orig SDP");
						}
						termPendingAck.setContent(origRcvdSdp.getContent(),
								origRcvdSdp.getContentType());
						termPendingAck.send();
						SipProtocolUtil.setSentSdp(termPendingAck, termLegData,
								origLegCallId);
						termLegData.remove(LegDataAttributes.P_PENDING_ACK);
						termLegData
								.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
					}

					SipServletResponse origInvSuccRes = SipProtocolMessageCreator
							.createSuccessResponseInvite(origInitialInv,
									termCurrentRcvdSdp);
					origInvSuccRes.send();

					SipProtocolUtil.setSentSdp(origInvSuccRes, origLegData,
							origLegCallId);
					origLegData.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
					// Start session refresh timer
					SipProtocolUtil.startSessionRefreshTimer(appSession,
							origLegData);
					origLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.CONNECTED);
					// Inform service that orig and term connected
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Inform service that orig and term connected");
					}
					Event event = new Event(EventType.EVENT_RESYNC_SUCCESS,
							Protocol.SIP, action.getLeg());
					ProtocolRouter.getInstance().execute(
							event,
							callData,
							PhUtilityServices.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
									.getServiceHandler());
				} else {
					/*
					 * Orig OFFER SDP has already been answered or used.So get
					 * new SDP from orig to send this new orig SDP to term in
					 * term_pending_ack
					 */
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: SDP answer is pending on term");
					}
					switch (origInitialInv.getSession().getState()) {
					case INITIAL:
					case EARLY: {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Send UPDATE to orig to obtain new SDP from orig");
							logger.debug(origLegCallId
									+ ":: Set term SDP in this UPDATE to orig");
						}
						SipServletRequest origUpdateReq = SipProtocolMessageCreator
								.createUpdate(origInitialInv.getSession(),
										termCurrentRcvdSdp, callData);
						origUpdateReq.send();
						SipProtocolUtil.setSentSdp(origUpdateReq, origLegData,
								origLegCallId);

						origLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
								PhConstants.TRUE);

						SipServletResponse origPendingSuccRes = SipProtocolMessageCreator
								.createSuccessResponseInvite(origInitialInv,
										termCurrentRcvdSdp);
						origLegData.set(LegDataAttributes.P_PENDING_SUCC_RESP,
								origPendingSuccRes);

						break;
					}
					case CONFIRMED: {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Send Re-INVITE to orig to obtain new SDP from orig");
							logger.debug(origLegCallId
									+ ":: Set term SDP in this Re-INVITE to orig");
						}
						SipServletRequest origReInvReq = SipProtocolMessageCreator
								.createReinviteRequest(origInitialInv,
										termCurrentRcvdSdp);

						origReInvReq.send();
						SipProtocolUtil.setSentSdp(origReInvReq, origLegData,
								origLegCallId);
						origLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
								PhConstants.TRUE);

						break;
					}
					default: {
						logger.error(origLegCallId
								+ ":: Unexpected orig SIP state"
								+ origInitialInv.getSession().getState()
								+ ". Drop call");
						callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
								SipProtocolRelReasonCode.UNEXPECTED_MPH_STATE);
						origLegData.set(LegDataAttributes.P_CAUSE_CODE,
								Integer.parseInt(SipProtocolUtil.getConfig(

								SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
						dropCall(appSession);
						break;
					}
					}

				}

			} else {
				/*
				 * TERM and orig offer/answer is already completed or orig and
				 * term SDP have already been used. SO get new SDP from term to
				 * nagotiate it with orig
				 */
				// Get new SDP from Term
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Need to re-negotiate SDP between Orig and Term");
					// logger.debug(origLegCallId
					// +
					// ":: Send Re-INVITE without SDP to term  also check if pending success response for orig required");
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SDP answer is pending on orig and term");
				}
				Object dialout = callData.get(CallDataAttribute.P_DIALOUT);
				
				
				if (dialout != null) { //for click to dial kind of dialout
				SipServletRequest origPendingAck = (SipServletRequest) origLegData
						.get(LegDataAttributes.P_PENDING_ACK);
				
				if (origPendingAck != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: ACK is pending on orig so send it with orig SDP");
					}
					origPendingAck.setContent(termCurrentRcvdSdp.getContent(),
							termCurrentRcvdSdp.getContentType());
					origPendingAck.send();
					SipProtocolUtil.setSentSdp(origPendingAck, origLegData,
							origLegCallId);
					origLegData
					.remove(LegDataAttributes.P_PENDING_ACK);
					origLegData
					.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
				}
				
				SipServletRequest termPendingAck = (SipServletRequest) termLegData
						.get(LegDataAttributes.P_PENDING_ACK);
				if (termPendingAck != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: ACK is pending on term so send it with orig SDP");
					}
					termPendingAck.setContent(origRcvdSdp.getContent(),
							origRcvdSdp.getContentType());
					termPendingAck.send();
					SipProtocolUtil.setSentSdp(termPendingAck, termLegData,
							origLegCallId);
					termLegData
					.remove(LegDataAttributes.P_PENDING_ACK);
					termLegData
					.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
				}
				}

				boolean isNoInviteWithoutSdp = PhConstants.TRUE.equals(SipProtocolUtil
						.getConfig(SipProtocolConfig.NO_INVITE_WITHOUT_SDP));

				switch (origInitialInv.getSession().getState()) {
				case INITIAL:
				case EARLY: {

					if (isNoInviteWithoutSdp) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Send success response to orig with term sdp !!!");
						}
						SipServletResponse successResponse = SipProtocolMessageCreator
								.createSuccessResponseInvite(origInitialInv,
										termCurrentRcvdSdp);
						successResponse.send();
						SipProtocolUtil.setSentSdp(successResponse,
								origLegData, origLegCallId);

						SipProtocolUtil.startSessionRefreshTimer(appSession,
								origLegData);
						origLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.CONNECTED);
						// Inform service that orig and term connected
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Inform service that orig and term connected");
						}
						Event event = new Event(EventType.EVENT_RESYNC_SUCCESS,
								Protocol.SIP, action.getLeg());
						ProtocolRouter
								.getInstance()
								.execute(
										event,
										callData,
										PhUtilityServices
												.getInstance(
														(String) callData
																.get(CallDataAttribute.SERVICE_ID))
												.getServiceHandler());
						return;
					} else {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Save pending success response for orig will send it after receieving 200 ok from term for null re-invite !!!");
						}
						SipServletResponse successResponse = SipProtocolMessageCreator
								.createSuccessResponseInvite(origInitialInv,
										null);
						origLegData.set(LegDataAttributes.P_PENDING_SUCC_RESP,
								successResponse);
					}
				}
					break;
				case CONFIRMED: {
					if (isNoInviteWithoutSdp) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Send Re-INVITE to orig with term sdp we will not send null sdp here");
						}
						SipServletRequest origReInvReq = SipProtocolMessageCreator
								.createReinviteRequest(origInitialInv,
										termCurrentRcvdSdp);
						origReInvReq.send();
						SipProtocolUtil.setSentSdp(origReInvReq, origLegData,
								origLegCallId);
						origLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
								PhConstants.TRUE);
						return;
					}
				}
				default:{
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: No need to create success response on orig side");
					}
				 }

				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: createReinviteRequest to peer leg");
				}
				SipServletRequest termReInvReq = SipProtocolMessageCreator
						.createReinviteRequest(termInitialInv, null);
				termReInvReq.send();
				SipProtocolUtil.setSentSdp(termReInvReq, termLegData,
						origLegCallId);

			}
		} else {
			/*
			 * This is the case when a new call is received from orig and then
			 * service connects orig and term.
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: SDP received from orig is same as SDP sent to term leg");
			}
			SipServletRequest termPendingAck = (SipServletRequest) termLegData
					.get(LegDataAttributes.P_PENDING_ACK);
			if (termPendingAck != null && origRcvdSdp != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Send pending ACK on term leg as initial sdp from A was not null so offer answer is complete now");
				}
				termPendingAck.send();
				termLegData.remove(LegDataAttributes.P_PENDING_ACK);
			}

			switch (origInitialInv.getSession().getState()) {
			case INITIAL:
			case EARLY: {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Set term SDP in this success to orig");
				}
				SipServletResponse origInvSuccRes = SipProtocolMessageCreator
						.createSuccessResponseInvite(origInitialInv,
								termCurrentRcvdSdp);
				if (appSession
						.getAttribute(PhConstants.INVITE_SUCCESS_RESPONSE) != null) {
					SipServletResponse inviteSuccessRes = (SipServletResponse) appSession
							.getAttribute(PhConstants.INVITE_SUCCESS_RESPONSE);
					SipProtocolUtil.copyHeaders(callData, inviteSuccessRes,
							origInvSuccRes);
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: copying headers from invite success to orig success...");
					}

					appSession
							.removeAttribute(PhConstants.INVITE_SUCCESS_RESPONSE);
				}
				origInvSuccRes.send();

				SipProtocolUtil.setSentSdp(origInvSuccRes, origLegData,
						origLegCallId);

				origLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
						PhConstants.TRUE);
				origLegData.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
				// Start session refresh timer
				SipProtocolUtil.startSessionRefreshTimer(appSession,
						origLegData);
				origLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
						State.CONNECTED);
				// Inform service that orig and term connected
				/*
				 * if (logger.isDebugEnabled()) { logger.debug(origLegCallId +
				 * ":: Inform service that orig and term connected"); } Event
				 * event = new Event(EventType.EVENT_RESYNC_SUCCESS,
				 * Protocol.SIP, action.getLeg());
				 * ProtocolRouter.getInstance().execute(event, callData,
				 * PhUtilityServices
				 * .getInstance((String)callData.get(CallDataAttribute
				 * .SERVICE_ID)).getServiceHandler());
				 */
				break;
			}
			case CONFIRMED: {

				MultipartBody lastSentSdpContent = (MultipartBody) origLegData
						.get(LegDataAttributes.P_LAST_SENT_SDP);
				// !PhConstants.TRUE
				// .equals(origLegData
				// .getNonpersistableData(LegDataAttributes.NP_IS_OFFER_SENT))||
				if (SipProtocolUtil.isSdpChanged(lastSentSdpContent,
						termCurrentRcvdSdp, origLegCallId)) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Send Re-INVITE to orig with B sdp");
					}
					SipServletRequest origReInitialInv = SipProtocolMessageCreator
							.createReinviteRequest(origInitialInv,
									termCurrentRcvdSdp);
					origReInitialInv.send();
					SipProtocolUtil.setSentSdp(origReInitialInv, origLegData,
							origLegCallId);
					origLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
							PhConstants.TRUE);
					/*
					 * set current action on term leg
					 */
					origLegData.set(LegDataAttributes.P_LEG_CURRENT_ACTION,
							action);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: No need to send Re-INVITE to orig , sdp already shared with orig");
					}
					origLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
							PhConstants.FALSE);
				}
				break;
			}
			default: {
				logger.error(origLegCallId + ":: Unexpected orig SIP state"
						+ origInitialInv.getSession().getState()
						+ ". Drop call");
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.UNEXPECTED_MPH_STATE);
				origLegData
						.set(LegDataAttributes.P_CAUSE_CODE,
								Integer.parseInt(SipProtocolUtil
										.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
				dropCall(appSession);
				break;
			}
			}

			// SipServletResponse origInvSuccRes = SipProtocolMessageCreator
			// .createSuccessResponseInvite(origInitialInv,
			// termCurrentRcvdSdp);
			// origInvSuccRes.send();
			//
			// SipProtocolUtil.setSentSdp(origInvSuccRes, origLegData,
			// origLegCallId);
			// origLegData
			// .removeNonpersistableData(LegDataAttributes.NP_IS_OFFER_RECEIVED);
			// // Start session refresh timer
			// SipProtocolUtil.startSessionRefreshTimer(appSession,
			// origLegData);
			// origLegData.setPersistableData(LegDataAttributes.P_LEG_SIP_STATE,
			// State.CONNECTED);
			// // Inform service that orig and term connected
			// if (logger.isDebugEnabled()) {
			// logger.debug(origLegCallId
			// + ":: Inform service that orig and term connected");
			// }
			// Event event = new Event(EventType.EVENT_RESYNC_SUCCESS,
			// Protocol.SIP, action.getLeg());
			// ProtocolRouter.getInstance().execute(event, callData,
			// PhUtilityServices.getInstance().getServiceHandler());
		}

	}

	/**
	 * mPH can receive UPDATE success response only from ORIG connection. And at
	 * this time, term connection will always be in connected state
	 * 
	 * @param sipResponse
	 * @param callData
	 * @param legData
	 * @throws Exception
	 */
	public static void handleUpdateSuccess(SipServletResponse sipResponse,
			CallData callData, LegData legData) throws Exception {
		SipApplicationSession appSession = sipResponse.getApplicationSession();
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleUpdateSuccess");
		}
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);

		// Connection type from where response received commenting below code
		// reeta for LEV-1308 bug
		// if (connType != ConnectionType.ORIG_CONNECTION) {
		// throw new Exception(
		// "Unexpected update success response as not from orig leg");
		// }

		// legData.remove(LegDataAttributes.NP_IS_OFFER_SENT);
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Update success received from "
					+ connType + " at state " + state);
		}
		
		if(State.PROXY_CONNECT.equals(state)){
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: return from state PROXY_CONNECT");
			}
			return;
			
		}
		SipProtocolUtil.setReceivedSdp(sipResponse, callData);

		// Get peer legData which is always TERM_CONNECTION type
		LegData peerLegData = SipProtocolUtil
				.getPeerLegData(callData, connType);
		State peerState = (State) peerLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);

		switch (state) {
		case CONN_IN_PROGRESS:
		case MS_DISCONNECTED: {
			switch (peerState) {
			case MS_DISCONNECTED:
			case CONNECTED: {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: TermSipCallState is CONNECTED");
					logger.debug(origLegCallId
							+ ":: Send pending message to orig/term leg");
				}

				// Send pending ACK to term leg
				SipServletRequest pendingTermAck = (SipServletRequest) peerLegData
						.get(LegDataAttributes.P_PENDING_ACK);
				String termLegId = null;
				if (pendingTermAck != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Send pending ACK to term with updated SDP");
					}
					pendingTermAck.setContent(sipResponse.getContent(),
							sipResponse.getContentType());
					pendingTermAck.send();
					peerLegData.remove(LegDataAttributes.P_PENDING_ACK);

					// Set last send sdp
					SipProtocolUtil.setSentSdp(pendingTermAck, peerLegData,
							origLegCallId);
					peerLegData.remove(LegDataAttributes.NP_IS_OFFER_RECEIVED);
					termLegId = (String) pendingTermAck.getSession()
							.getAttribute(PhConstants.LEG_ID);
				}

				// Send pending 200 OK to orig leg
				// Send pending provisional response to Orig leg
				SipServletResponse pendingSuccResp = (SipServletResponse) legData
						.get(LegDataAttributes.P_PENDING_SUCC_RESP);

				if (pendingSuccResp != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Send pending success response to orig");
						logger.debug(origLegCallId
								+ ":: Start session refresh timer");
					}

					// Start orig leg session refresh timer
					SipProtocolUtil.startSessionRefreshTimer(appSession,
							legData);

					// Set orig leg state to CONNECTED
					legData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.CONNECTED);

					// Send success response to orig leg and update sent sdp
					pendingSuccResp.send();

					legData.remove(LegDataAttributes.P_PENDING_SUCC_RESP);

					// Update attribute for future reference
					SipProtocolUtil.setSentSdp(pendingSuccResp, legData,
							origLegCallId);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Inform service that orig and term connected");
					}
					if (termLegId == null) {
						termLegId = (String) SipProtocolUtil
								.getInitialInvite(appSession, peerLegData)
								.getSession().getAttribute(PhConstants.LEG_ID);
					}

					legData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.CONNECTED);
					peerLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.CONNECTED);

					Event event = new Event(EventType.EVENT_RESYNC_SUCCESS,
							Protocol.SIP, termLegId);
					ProtocolRouter.getInstance().execute(
							event,
							callData,
							PhUtilityServices.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
									.getServiceHandler());

				}
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::  "
							+ " peer is in connected state check if update was received from peer and  need to send 200 ok");
				}
				SipServletRequest peerPendingUpdate = (SipServletRequest) peerLegData
						.get(LegDataAttributes.P_PENDING_UPDATE);

				if (peerPendingUpdate != null) {

					peerLegData.remove(LegDataAttributes.P_PENDING_UPDATE);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::  "
								+ " update was received from peer need to send 200 ok");
					}
					SipServletResponse updateResponse = SipProtocolMessageCreator
							.createSuccessResponseUpdate(peerPendingUpdate);

					if (sipResponse.getContentLength() > 0) {
						updateResponse.setContent(sipResponse.getContent(),
								sipResponse.getContentType());
					}
					updateResponse.send();
				}
				break;
			}
			case CONN_IN_PROGRESS: {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Update response received in CONN_IN_PROGRESS ");
				}

				SipServletRequest peerPendingUpdate = (SipServletRequest) peerLegData
						.get(LegDataAttributes.P_PENDING_UPDATE);

				if (peerPendingUpdate != null) {
					peerLegData.remove(LegDataAttributes.P_PENDING_UPDATE);
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::  "
								+ " update was received from peer need to send 200 ok");
					}
					SipServletResponse updateResponse = SipProtocolMessageCreator
							.createSuccessResponseUpdate(peerPendingUpdate);

					if (sipResponse.getContentLength() > 0) {
						updateResponse.setContent(sipResponse.getContent(),
								sipResponse.getContentType());
					}

					SipProtocolUtil.copyHeaders(callData, sipResponse,
							updateResponse);
					updateResponse.send();
				}

				SipServletResponse pendingProvRes = (SipServletResponse) legData
						.get(LegDataAttributes.NP_PENDING_PROV_RESP);
				if (pendingProvRes != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Send pending provisional response to orig");
					}
					pendingProvRes.sendReliably();
					legData.remove(LegDataAttributes.NP_PENDING_PROV_RESP);
				}

				SipServletRequest pendingTermAck = (SipServletRequest) peerLegData
						.get(LegDataAttributes.P_PENDING_ACK);
				if (pendingTermAck != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Send pending ACK to term with updated SDP");
					}
					pendingTermAck.setContent(sipResponse.getContent(),
							sipResponse.getContentType());
					pendingTermAck.send();
					peerLegData.remove(LegDataAttributes.P_PENDING_ACK);
				}

				// Send pending 200 OK to orig leg
				// Send pending provisional response to Orig leg
				SipServletResponse pendingSuccResp = (SipServletResponse) legData
						.get(LegDataAttributes.P_PENDING_SUCC_RESP);

				if (pendingSuccResp != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Send pending success response to orig");
						logger.debug(origLegCallId
								+ ":: Start session refresh timer");
					}

					// Start orig leg session refresh timer
					SipProtocolUtil.startSessionRefreshTimer(appSession,
							legData);

					// Set orig leg state to CONNECTED
					legData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.CONNECTED);

					// Send success response to orig leg and update sent sdp

					pendingSuccResp.setContent(sipResponse.getContent(),
							sipResponse.getContentType());
					pendingSuccResp.send();

					legData.remove(LegDataAttributes.P_PENDING_SUCC_RESP);

					// Update attribute for future reference
					SipProtocolUtil.setSentSdp(pendingSuccResp, legData,
							origLegCallId);
				}

				break;
			}
			default: {
				logger.error(origLegCallId
						+ ":: UPDATE success received from orig. But term connection is not in CONNECTED state");
				logger.error(origLegCallId + ":: ORIG state " + state
						+ " TERM state " + peerState);
				break;
			}
			}
			break;
		}

		case CONNECTED:

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::  " + connType
						+ " is in connected state");
			}
			switch (peerState) {
			case CONNECTED: {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::  "
							+ " peer is in connected state check if update was received from peer and  need to send 200 ok");
				}
				SipServletRequest peerPendingUpdate = (SipServletRequest) peerLegData
						.get(LegDataAttributes.P_PENDING_UPDATE);

				if (peerPendingUpdate != null) {

					peerLegData.remove(LegDataAttributes.P_PENDING_UPDATE);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::  "
								+ " update was received from peer need to send 200 ok");
					}
					SipServletResponse updateResponse = SipProtocolMessageCreator
							.createSuccessResponseUpdate(peerPendingUpdate);

					if (sipResponse.getContentLength() > 0) {
						updateResponse.setContent(sipResponse.getContent(),
								sipResponse.getContentType());
					}
					updateResponse.send();
				}
			}
				break;
			default: {
				logger.error(origLegCallId
						+ ":: UPDATE success . But peer connection is not in CONNECTED state");
				logger.error(origLegCallId + ":: ORIG state " + state
						+ " TERM state " + peerState);
				break;
			}
			}
			break;
		default: {
			logger.error(origLegCallId
					+ ":: UPDATE success received from orig. But orig connection is not in CONN_IN_PROGRESS state");
			logger.error(origLegCallId + ":: ORIG state " + state
					+ " TERM state " + peerState);
			break;
		}
		}
	}

	/**
	 * mPH can receive UPDATE success response only from ORIG connection. And at
	 * this time, term connection will always be in connected state
	 * 
	 * @param sipResponse
	 * @param callData
	 * @param legData
	 * @throws Exception
	 */
	public static void handlePrackSuccess(SipServletResponse sipResponse,
			CallData callData, LegData legData) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handlePrackSuccess");
		}
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);

		// legData.remove(LegDataAttributes.NP_IS_OFFER_SENT);
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Update success received from "
					+ connType + " at state " + state);
		}
		SipProtocolUtil.setReceivedSdp(sipResponse, callData);

		State peerState = (State) legData
				.get(LegDataAttributes.P_LEG_SIP_STATE);

		LegData peerLegData = SipProtocolUtil
				.getPeerLegData(callData, connType);

		switch (state) {

		case CONN_IN_PROGRESS: {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Update response received in CONN_IN_PROGRESS ");
			}

			SipServletRequest pendingUpdate = (SipServletRequest) legData
					.get(LegDataAttributes.P_PENDING_UPDATE);

			if (pendingUpdate != null) {
				legData.remove(LegDataAttributes.P_PENDING_UPDATE);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::  "
							+ " update need to be send to peer");
				}
				SipProtocolUtil.setSentSdp(pendingUpdate, legData,
						origLegCallId);
				pendingUpdate.send();
			}

			SipServletResponse pendingPrackSucc = (SipServletResponse) peerLegData
					.get(LegDataAttributes.NP_PENDING_PRACK_SUCCESS);

			if (pendingPrackSucc != null) {
				peerLegData.remove(LegDataAttributes.NP_PENDING_PRACK_SUCCESS);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::  "
							+ " 200 ok for prack need to be send to peer");
				}

				if (sipResponse.getContentLength() > 0) {
					pendingPrackSucc.setContent(sipResponse.getContent(),
							sipResponse.getContentType());
				}
				SipProtocolUtil.setSentSdp(pendingPrackSucc, peerLegData,
						origLegCallId);
				SipProtocolUtil.copyHeaders(callData, sipResponse,
						pendingPrackSucc);
				pendingPrackSucc.send();
			}
			// Update attribute for future reference

			break;
		}
		default: {
			logger.error(origLegCallId
					+ ":: PRACK success received from orig. ");
			logger.error(origLegCallId + ":: ORIG state " + state
					+ " TERM state " + peerState);
			break;
		}
		}

	}

	public static void handleByeSuccess(SipServletResponse sipResponse,
			CallData callData, LegData legData) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		SipSession legSipSession = SipProtocolUtil.getSipSessionFromSessionId(
				origLegCallId, sipResponse.getApplicationSession(),
				(String) legData.get(LegDataAttributes.P_SESSION_ID));

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleByeSuccess");
		}

		Action lastAction = (Action) callData
				.get(CallDataAttribute.P_CURRENT_ACTION);

		if (lastAction.getActionType() == Action.ActionType.ACTION_DISCONNECT) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: success response received for bye for disocnnecting  leg");
			}
			String legId = (String) sipResponse.getSession().getAttribute(
					PhConstants.LEG_ID);

			Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.SIP,
					legId);
			ProtocolRouter.getInstance().execute(
					event,
					callData,
					PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getServiceHandler());
			// Mark this session invalidate as all transaction has
			// been completed on it setting ready to invalidate attribute true
			// causes problem in alternate routing
			sipResponse.getSession().invalidate();
			return;
		}

		if (lastAction.getActionType() == Action.ActionType.ACTION_PICKUP_CALL) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: success response received for bye for disocnnecting  pickup call leg");
			}
			// Mark this session invalidate as all transaction has
			// been completed on it setting ready to invalidate attribute true
			// causes problem in alternate routing

			try {
				sipResponse.getSession().invalidate();
			} catch (Exception e) {
				logger.error("Exception while invalidtaing sipsession"
						+ sipResponse.getSession().getId());
			}

			SipProtocolHelper.resynchCallPickupLegs(callData);
			return;
		}

		/*
		 * Don't do anything if BYE has been sent for the session on noanswer
		 * timeout IMPORTANT: Don't put any logic before this check, otherwise
		 * behavior would become unpredictable
		 */
		if (PhConstants.TRUE.equals(legData
				.get(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Success response received for BYE sent on noanswer timeout");
			}

			// Mark sip session as ready to invalidate as all transaction are
			// completed now
			legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE,
					PhConstants.TRUE);
			return;
		}

		if (PhConstants.TRUE.equals(sipResponse.getSession().getAttribute(
				PhConstants.STALE))) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: returning from here received bye success for forked smultiple 200 ok response session");
			}
			sipResponse.getSession().setAttribute(
					PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
			return;
		}

		/*
		 * Check if bye success respons eis received for correlated orig cleanup
		 * BYE request
		 */
		if (PhConstants.TRUE.equals(legData
				.get(LegDataAttributes.P_CORRELATED_ORIG_LEG_CLEANUP_IND))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Success response received for BYE sent on correlated orig leg cleanup time");
			}

			// Mark sip session as ready to invalidate as all transaction are
			// completed now
			legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE,
					PhConstants.TRUE);
			legData.set(LegDataAttributes.P_CORRELATED_ORIG_LEG_CLEANUP_IND,
					PhConstants.FALSE);
			return;
		}

		State legState = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		ConnectionType legConnType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Success response received for BYE sent from "
					+ legConnType + " at state " + legState);
		}
		legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE,
				PhConstants.TRUE);
		legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.TERMINATED);

		// Invalidate the appsession if possible
		SipProtocolUtil.invalidateAppSession(sipResponse
				.getApplicationSession());
	}

	/**
	 * This method execute CONNECT_TERM action, requested from services. For
	 * orig sip state is MS_DISCONNECTION_IN_PROGRESS or CONNECTION_IN_PROGRESS
	 * or SERVICE_LOGIC
	 * <ul>
	 * <li>If connection mode is PORTROUTING or REDIRECTION, Send redirection
	 * response and set attempted indicator to 6.
	 * <li>If connection mode is B2BUA, send INVITE to term and session progress
	 * response to orig end.
	 * <li>In case of connect term invoked with invalid connection mode, drop
	 * call with CV=41.
	 * </ul>
	 * If Connect term is invoked in invalid state, drop call with CV=41.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action.
	 * @throws Exception
	 */
	public static void connectTerm(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ " :: Inside connectTerm with appSession and check the state and connection mode");
		}
		
		
		if (action.getConnectionMode().equals(
				Action.CONNECTIONMODE.PROXY_STATEFULL)
				|| action.getConnectionMode().equals(
						Action.CONNECTIONMODE.PROXY_STATELESS)) {
			
			proxyInitialRequest(appSession, callData, action);
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: Leaving  connectTerm by proxying request ");
			}
			
			return;

		}
		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		State origState = (State) origLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);
		switch (origState) {
		case MS_DISCONNECTED:
		case CONN_IN_PROGRESS:
		case MS_PLAY_COLLECT:
		case MS_PLAY:
		case MS_CONN_IN_PROGRESS:
		case MS_RECORD:
		case INIT: {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Send INVITE to term");
			}
			// B2BUA INVITE
			sendInitialInviteToTerm(appSession, true, callData, action);
			// 183 Response
			// sendSessionProgressResponse(appSession);
			break;
		}
		default:
			logger.error(origLegCallId
					+ ":: Connect term invoked in invalid orig state"
					+ origState + ", drop call");
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_PROV_HANDLING);
			origLegData
					.set(LegDataAttributes.P_CAUSE_CODE,
							Integer.parseInt(SipProtocolUtil
									.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);
			break;
		}
	}

	/**
	 * This method send the initial INVITE to terminating leg.
	 * <p>
	 * For PBXDialIn case reset isReliableSupported to FALSE. If initial INVITE
	 * was without SDP, then reset sendSdp flag to FALSE. For term type END
	 * POINT: create new request and populate the mandatory sip parameters apart
	 * from following notable points.
	 * <ul>
	 * <li>Set is100relSupported to true only when isReliableSupported is TRUE
	 * and party A also supports 100rel.
	 * <li>Removing existing X_ISC_SVC and X_ISC_ADDR headers.
	 * <li>Removing existing session expires header
	 * <li>Support session refresh on term in case of interservice communication
	 * unconditionally as service triggering the NGIN application does not
	 * support SE as UAC in outgoing INVITE and UAS is not support by NGIN PH.So
	 * it is decided that service triggering the NGIN application will not send
	 * the SE headers in the INVITE request.
	 * </ul>
	 * For term type VOICEMAIL: Create new request, link session and Populate
	 * the mandatory sip parameters apart from following notable points.
	 * <ul>
	 * The correction in voice mail triggering from ATF service:
	 * <li>Voice Mail Deposite-> The term URI should be created with toll-free
	 * number, voicemail IP and port. The from URI should be the calling number.
	 * <li>Voice Mail Retrival-> The term URI should be created with voicemail
	 * access number, IP and port. The access number attribute needs to be added
	 * to gateway.properties configuration.
	 * <li>No need to run no-answer timer for voice-mail
	 * </ul>
	 * Create ISUP MULTIPART if required.Start the session refresh and No answer
	 * timer. Perform replication after sending invite.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 * @param sendSdp
	 *            represents the instance of boolean
	 * @param isReliableSupported
	 *            represents the instance of boolean
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	protected static void sendInitialInviteToTerm(
			SipApplicationSession appSession, boolean sendSdp,
			CallData callData, Action action) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		boolean isReliableSupported = PhConstants.TRUE.equals(SipProtocolUtil
				.getConfig(SipProtocolConfig.REL_PROVISIONAL_SUPPORTED));
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside sendInitialInviteToTerm leg is "
					+ action.getLeg());
		}


		boolean isIMSCall=false;

		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: isOfferReceived from leg"
					+ origLegData.get(LegDataAttributes.NP_IS_OFFER_RECEIVED));
		}

		Object dialout = callData.get(CallDataAttribute.P_DIALOUT);
		
		
		if (dialout != null) {
			String dialoutParty = (String) callData
					.get(CallDataAttribute.NP_DIALOUT_PARTY);

			if (dialoutParty != null) {
				if (dialoutParty.equalsIgnoreCase("B")) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Inside set leg2 as action leg as dialout party is B");
					}
					action.setLeg(CallDataAttribute.P_LEG2.name());
				} else if (dialoutParty.equalsIgnoreCase("A")){
					
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Inside set leg1 as action leg as dialout party is A");
					}
					action.setLeg(CallDataAttribute.P_LEG1.name());
				}
			}
		}
		if(logger.isDebugEnabled()){
			logger.debug(origLegCallId + ":: isOfferReceived from leg" + origLegData.get(LegDataAttributes.NP_IS_OFFER_RECEIVED));

		}
		LegData termLegData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: sendInitialInviteToTerm termlegData  is "
					+ termLegData);
		}

		InviteAttributes inviteAttributes = (InviteAttributes) termLegData
				.get(LegDataAttributes.INVITE_ATTRIBUTES);

		// SipSession termLegSipSession =
		// SipProtocolUtil.getSipSessionFromSessionId(
		// origLegCallId, appSession, (String)
		// termLegData.getPersistableData(LegDataAttributes.P_SESSION_ID));

		/**
		 * Remove error response received from other party because in alternate
		 * routing kind of scenarios the error response of first failure will be
		 * saved in this attribute
		 */

		Action lastAction = (Action) callData
				.get(CallDataAttribute.P_CURRENT_ACTION);

		if (lastAction.getConnectionMode() != CONNECTIONMODE.EQSROUTING) {
			origLegData.remove(LegDataAttributes.P_OTHER_LEG_ERR_BYE_CAN_MSG);
			termLegData.remove(LegDataAttributes.P_CAUSE_CODE);
			termLegData.remove(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND);
		}

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId + ":: Orig Leg data is " + origLegData
					+ " and term leg data is " + termLegData);
		}

		boolean isSdpSentOnLeg1Earlier = PhConstants.TRUE
				.equals((String) origLegData
						.get(LegDataAttributes.NP_IS_SDP_SENT));

		boolean isNoInviteWithoutSdp = PhConstants.TRUE.equals(SipProtocolUtil
				.getConfig(SipProtocolConfig.NO_INVITE_WITHOUT_SDP));

		if (isNoInviteWithoutSdp) {

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: No invite should go without sdp is enabled  ");
			}
			sendSdp = true;
		} else if (sendSdp
				&& (!PhConstants.TRUE.equals(origLegData
						.get(LegDataAttributes.NP_IS_OFFER_RECEIVED)))
				|| isSdpSentOnLeg1Earlier) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Initial INVITE without SDP or sdp already sent on any leg , so reset sendSdp to FALSE and NP_IS_OFFER_RECEIVED to false on orig");
			}
			sendSdp = false;
			origLegData.set(LegDataAttributes.NP_IS_OFFER_RECEIVED,
					PhConstants.FALSE);
		}


		termLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
				State.INIT);
		
		if(action.getLeg()==CallDataAttribute.P_LEG1.name()){
			termLegData.set(LegDataAttributes.P_CONNECTION_TYPE,
					ConnectionType.ORIG_CONNECTION);
		}else{
			termLegData.set(LegDataAttributes.P_CONNECTION_TYPE,
					ConnectionType.TERM_CONNECTION);
		}

		termLegData.set(LegDataAttributes.P_LEG_CURRENT_ACTION, action);

		SipServletRequest origInvRequest = SipProtocolUtil.getInitialInvite(
				appSession, origLegData);
		if(dialout != null && origInvRequest==null){
			
		}else{

		String origSipSessionLeg = (String) origInvRequest.getSession()
				.getAttribute(PhConstants.LEG_ID);
		if (origSipSessionLeg != CallDataAttribute.P_LEG1.name()) {
			/**
			 * This is special case in which Leg1 or Party A is disconnected and
			 * we reconnect the A party from B party by swapping legs, in that
			 * case leg updation also required in SipSession
			 */
			if (logger.isDebugEnabled()) {
				logger.debug("Need to update leg in sipSession");
			}

			origInvRequest.getSession().setAttribute(PhConstants.LEG_ID,
					CallDataAttribute.P_LEG1.name());
		}
		}
		HashMap<String, List<String>> headerMap = new HashMap<String, List<String>>();

		SipServletRequest termSipRequest = null;

		URI termUri = null;
		String pAIFromMCContact = null;
		String modCallingNumber = null;
		URI fromUri = null;
		/**
		 * use the to from and request uri for outgoing request set by the
		 * application
		 */
		if (inviteAttributes != null) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Application has set invite attributes on outgoing leg ");
			}
			termUri = inviteAttributes.getRequestURI();
			fromUri = inviteAttributes.getFrom().getURI();

			LinkedList<String> termUriList = new LinkedList<String>();
			LinkedList<String> fromUriList = new LinkedList<String>();

			termUriList.add(inviteAttributes.getTo().toString());
			fromUriList.add(inviteAttributes.getFrom().toString());

			headerMap.put(PhConstants.FROM_HEADER, fromUriList);
			headerMap.put(PhConstants.TO_HEADER, termUriList);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":Iinvite attributes on outgoing leg are --> termURI "
						+ termUri + " FromUri " + fromUri + " toUri is "
						+ inviteAttributes.getTo());
			}
		} else {

			// To Header

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Term type is ENDPOINT, create new request");
			}

			if (action.getActionType() == Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS) {

				// termLegData.setPersistableData(LegDataAttributes.P_LEG_CURRENT_ACTION,action
				// );

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Create term uri for redirect contact received");
				}

				/**
				 * Try redirect contacts received in 300 MC or 302
				 */
				@SuppressWarnings("unchecked")
				List<MultiChoiceContact> mccList = (List<MultiChoiceContact>) callData
						.get(CallDataAttribute.P_MC_CONTACTS_LIST);

				@SuppressWarnings("unchecked")
				List<TermRedirectionContact> termRedirectList = (List<TermRedirectionContact>) callData
						.get(CallDataAttribute.NP_REDIRECT_ADDRESS_LIST);

				/*
				 * Try redirect contact received in 302
				 */
				if (termRedirectList != null && !termRedirectList.isEmpty()) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Create term uri from redirect address received from 302 redirection contact ");
					}
					TermRedirectionContact termRedirection = termRedirectList
							.get(0);

					termUri = termRedirection.getRedirectContactUri();
					termLegData.set(LegDataAttributes.NP_REDIRECT_ADDRESS,
							termUri);
					termRedirectList.remove(termRedirection);

				} else if (mccList != null && !mccList.isEmpty()) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Create term uri from redirect address received from 303 MC contact ");
					}

					MultiChoiceContact mcc1 = mccList.get(0);

					callData.set(CallDataAttribute.P_MC_CONTACT, mcc1);

					termUri = mcc1.getContactUri();

					if (mcc1.getTgrp() != null
							&& mcc1.getTrunkContext() != null) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::set tgrp and trunkcontact for this redirect contact in calldata ");
						}
						callData.set(CallDataAttribute.P_DEST_TGRP,
								mcc1.getTgrp());
						callData.set(CallDataAttribute.P_DEST_TRUNK_CONTEXT,
								mcc1.getTrunkContext());
					} else {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::set DTG for this redirect contact in call data ");
						}
						callData.set(CallDataAttribute.P_DEST_TRUNK_GROUP,
								mcc1.getDtg());
					}

					pAIFromMCContact = mcc1.getPAIHdr();

					// termLegData.setPersistableData(LegDataAttributes.P_RN,
					// mcc1.getRn());
					// termLegData.setPersistableData(LegDataAttributes.P_NPDI,
					// mcc1.getNpdi());
					/**
					 * Removing this multiple contact from list because in case
					 * t fails MPH will try on next contact
					 */
					mccList.remove(mcc1);
					/*
					 * contact uri will go as it is as term uri in case of
					 * multiple contacts will contain tgid as well for new
					 * enhancements so commenting below code
					 */
					// termUri.setParameter(PhConstants.TRUNK_GRP_ID,
					// mcc1.getTrunkGroup());
					// }
					/*
					 * Try redirect contact reecived in 302
					 */
					// else if (callData
					// .get(CallDataAttribute.NP_REDIRECT_ADDRESS) != null) {
					//
					// if (logger.isDebugEnabled()) {
					// logger.debug(origLegCallId
					// + "::Create term uri from redirect address");
					// }
					//
					// String npRedirectAddress = (String) callData
					// .get(CallDataAttribute.NP_REDIRECT_ADDRESS);
					// termUri = (SipURI) PhUtilityServices.getInstance()
					// .getSipFactory().createAddress(npRedirectAddress)
					// .getURI();
				} else {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: No redirect contact available for redirection can not connect now");
					}
				}
			} else {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Create term uri for connecting to term");
				}

				if ((action.getActionType() == Action.ActionType.ACTION_TRANSFER_CONNECT)
						|| (action.getActionType() == Action.ActionType.ACTION_DTMF_TRANSFER_CONNECT)) {

					String transferedUser = null;

					if ((action.getActionType() == Action.ActionType.ACTION_TRANSFER_CONNECT)) {

						URI uri = (URI) termLegData
								.get(LegDataAttributes.TRANSFER_MODIFIED_URI);

						transferedUser = ((SipURI) origInvRequest
								.getRequestURI()).getUser();
					} else {
						transferedUser = (String) callData
								.get(CallDataAttribute.P_COLLECTED_DIGITS);
						if (transferedUser != null
								&& transferedUser.endsWith("#")) {
							transferedUser = transferedUser.substring(0,
									(transferedUser.length() - 1));
						}

					}
					/**
					 * send the request from where it was received the default
					 * value would have been set as remote address i.e. last via
					 * in doInvite() on receiving the invite
					 */
					termUri = PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getSipFactory()
							.createSipURI(
									transferedUser,
									SipProtocolUtil.getTerminatingIp(callData,
											termLegData));
					((SipURI) termUri).setPort(SipProtocolUtil
							.getTerminatingPort(callData, termLegData));

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Term uri for call transfer is  " + termUri);
					}
					/*
					 * Save attributes for CDR writing purpose
					 */
					termLegData.set(LegDataAttributes.P_REMOTE_IP,
							((SipURI) termUri).getHost());
					termLegData.set(LegDataAttributes.P_REMOTE_PORT,
							((SipURI) termUri).getPort());

				} else {
//<<<<<<< HEAD
//=======
					
					String sendIncomingURI= SipProtocolConfig.getConfigData(SipProtocolConfig.SEND_INCOMING_RURI_TOTERM);
                    
                    String ip =null;
                    int port =5060;
               
                    PhoneNumber calledNum=(PhoneNumber)origLegData.get(LegDataAttributes.P_CALLED_PARTY);
                    PhoneNumber destPhoneNum = (PhoneNumber) termLegData
							.get(LegDataAttributes.P_DESTINATION_NUMBER);
                    
					boolean sameDest = true;
					if (calledNum != null
							&& destPhoneNum != null
							&& !calledNum.getAddress().equals(destPhoneNum.getAddress())) {
						sameDest = false;
						 if (logger.isDebugEnabled()) {

								logger.debug(origLegCallId+ ":: Destination number is not same ");
								
							}
					}
					
                    if (PhConstants.TRUE.equalsIgnoreCase(sendIncomingURI)) {
                    	
					if (origInvRequest.getHeader(PhConstants.ROUTE_HEADER) != null
							&& PhConstants.TRUE
									.equalsIgnoreCase(sendIncomingURI)) {

						isIMSCall = true;
						
						 termUri = origInvRequest.getRequestURI();
						
						 if(!sameDest){
							
							 if (logger.isDebugEnabled()) {

									logger.debug(origLegCallId+ ":: Destination number is not same so updating uri wit dest phone number");
									
								}
								if (termUri instanceof SipURI) {
									SipURI tempuri = (SipURI) termUri;
									String user = tempuri.getUser();
									String[] userparts = user.split(";", 2);
									if (userparts.length > 1) {
										tempuri.setUser(destPhoneNum
												.getAddress()
												+ ";"
												+ userparts[1]);
									} else {
										tempuri.setUser(destPhoneNum
												.getAddress());
									}
								}
								
								if (termUri instanceof TelURL) {
									TelURL tempuri = (TelURL) termUri;
									tempuri.setPhoneNumber(destPhoneNum.getAddress());
								}
						}
					
						if (logger.isDebugEnabled()) {

							logger.debug(origLegCallId+ ":: Reading toUri from URI , it may be IMS call " + termUri);
							
						}
						
						 if(termUri instanceof TelURL){
							
							TelURL tel=(TelURL)termUri;
						
							if(tel.getParameter(PhConstants.NPDI_PARAM)!=null 
									&& tel.getParameter(PhConstants.NPDI_PARAM).equals("")){
						
							((TelURL)termUri).setParameter(PhConstants.NPDI_PARAM, "");
							
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId+ ":: TermURI updated is   " + termUri);
								
							}
						}
						 }
						
						LinkedList<String> toUriList = new LinkedList<String>();
						LinkedList<String> fromUriList = new LinkedList<String>();

						toUriList.add(origInvRequest.getTo().toString());
						fromUriList.add(origInvRequest.getFrom().toString());
						
//						LinkedList<String> contactList = new LinkedList<String>();
//						contactList.add(origInvRequest.getAddressHeader(PhConstants.CONTACT_HEADER).toString());

						headerMap.put(PhConstants.FROM_HEADER, fromUriList);

						headerMap.put(PhConstants.TO_HEADER, toUriList);	
				//		headerMap.put(PhConstants.CONTACT_HEADER, contactList);
						
				
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: proxying IMS call to  " + termUri);

						}
//<<<<<<< HEAD

						// Address
						// sipAdd=PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipFactory()
						// .createAddress(origInvRequest.getHeader(PhConstants.ROUTE_HEADER));
						// origInvRequest.getProxy().proxyTo(sipAdd.getURI());
						// return;

					} 
                    }else {
//=======
//					} else{
//
//						 destPhoneNum = (PhoneNumber) termLegData
//								.get(LegDataAttributes.P_DESTINATION_NUMBER);
//>>>>>>> MPH10.1_DEV

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::Destination Phone number is  "
									+ destPhoneNum);
						}

						ip = SipProtocolUtil.getTerminatingIp(callData,
								termLegData);
						port = SipProtocolUtil.getTerminatingPort(callData,
								termLegData);

					}
                    

					/**
					 * check if External query service pool id available then
					 * obgw selector will be used to find the term ip and port
					 */
					if (action.getConnectionMode() == Action.CONNECTIONMODE.EQSROUTING) {

						Integer retry = (Integer) termLegData
								.get(LegDataAttributes.NP_EQS_RETRY_COUNT);

						if (retry != null && retry.intValue() == 2) {

							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ "::EQS routing retry count has reached max value "
										+ retry
										+ " Notify application of failure");
							}

							ServiceInterface serviceHandler = PhUtilityServices
									.getInstance(
											(String) callData
													.get(CallDataAttribute.SERVICE_ID))
									.getServiceHandler();
							Event event = new Event(EventType.EVENT_FAILURE,
									Protocol.SIP, action.getLeg());
							ProtocolRouter.getInstance().execute(event,
									callData, serviceHandler);
							return;

						}

						String eqspoolId = (String) termLegData
								.get(LegDataAttributes.NP_GW_POOL_ID);

						if (eqspoolId != null) {

							OutboundGatewaySelector obgwSelector = PhUtilityServices
									.getInstance(
											(String) callData
													.get(CallDataAttribute.SERVICE_ID))
									.getObgwSelector();

							String lastEqsId = (String) termLegData
									.get(LegDataAttributes.NP_LAST_GW_ID);

							OutboundGateway obgw = null;

							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ "::select obgw for pool id  "
										+ eqspoolId);
							}

							if (lastEqsId != null) {

								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ "::Last EQS pool id available select obgw for "
											+ eqspoolId + " execpt  "
											+ lastEqsId);
								}
								obgw = obgwSelector.selectFromGroupExcept(
										eqspoolId, lastEqsId);

							} else {
								obgw = obgwSelector.selectFromGroup(eqspoolId);
							}

							if (obgw == null) {

								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ "::Notify service that obgw not found for  "
											+ eqspoolId);
								}

								termLegData.set(
										LegDataAttributes.NP_LAST_GW_ID, null);
								termLegData.set(
										LegDataAttributes.P_CAUSE_CODE, SipProtocolRelReasonCode.OBGW_NOT_AVAILABLE);
								ServiceInterface serviceHandler = PhUtilityServices
										.getInstance(
												(String) callData
														.get(CallDataAttribute.SERVICE_ID))
										.getServiceHandler();
								Event event = new Event(
										EventType.EVENT_GW_NOT_FOUND,
										Protocol.SIP, action.getLeg());
								ProtocolRouter.getInstance().execute(event,
										callData, serviceHandler);
								return;

							} else {

								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ "::EQS host is  "
											+ obgw.getHost() + " port is  "
											+ obgw.getPort());
								}

								if (obgw.getHost() != null) {
									ip = obgw.getHost().getHostAddress();
								}
								if (obgw.getPort() != -1) {
									port = obgw.getPort();
								}

								termLegData.set(
										LegDataAttributes.NP_LAST_GW_ID,
										obgw.getId());

								if (retry == null) {
									termLegData
											.set(LegDataAttributes.NP_EQS_RETRY_COUNT,
													1);
								} else {

									if (logger.isDebugEnabled()) {
										logger.debug(origLegCallId
												+ "::Update retry count to  "
												+ retry.intValue() + 1);
									}
									termLegData
											.set(LegDataAttributes.NP_EQS_RETRY_COUNT,
													retry.intValue() + 1);
								}
								termUri = PhUtilityServices
										.getInstance(
												(String) callData
														.get(CallDataAttribute.SERVICE_ID))
										.getSipFactory()
										.createSipURI(
												destPhoneNum.getAddress(), ip);
								((SipURI) termUri).setPort(port);
							}
						} else {

							termUri = PhUtilityServices
									.getInstance(
											(String) callData
													.get(CallDataAttribute.SERVICE_ID))
									.getSipFactory()
									.createSipURI(destPhoneNum.getAddress(), ip);
							((SipURI) termUri).setPort(port);
						}
					} else {

						if (termUri == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ "::Term uri is not created yet so create it now "
										+ ip + " port is  " + port
										+ " DestinationNumber " + destPhoneNum);
							}
//<<<<<<< HEAD
//							termUri = PhUtilityServices
//									.getInstance(
//											(String) callData
//													.get(CallDataAttribute.SERVICE_ID))
//									.getSipFactory()
//									.createSipURI(destPhoneNum.getAddress(), ip);
//							((SipURI) termUri).setPort(port);
//=======
							
							if (origInvRequest == null) {
								termUri = PhUtilityServices
										.getInstance(
												(String) callData
														.get(CallDataAttribute.SERVICE_ID))
										.getSipFactory()
										.createSipURI(
												destPhoneNum.getAddress(), ip);
								((SipURI) termUri).setPort(port);
							} else if (origInvRequest.getRequestURI()
									.isSipURI()) {
								termUri = PhUtilityServices
										.getInstance(
												(String) callData
														.get(CallDataAttribute.SERVICE_ID))
										.getSipFactory()
										.createSipURI(
												destPhoneNum.getAddress(), ip);
								((SipURI) termUri).setPort(port);
							} else {
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ "::create Tel URI as incoming is tel");
								}
								termUri = PhUtilityServices
										.getInstance(
												(String) callData
														.get(CallDataAttribute.SERVICE_ID))
										.getSipFactory()
										.createURI(
												"tel:"
														+ destPhoneNum
																.getAddress());
							}
//>>>>>>> MPH10.1_DEV
						}
					}
				
			}

			/**
			 * the switch id and trunk group id will be needed in connect term
			 * if provided for transfered call we are doing this
			 */
			// if (action.getActionType() == Action.ActionType.ACTION_CONNECT) {
			if (termLegData.get(LegDataAttributes.P_SWITCH_ID) != null) {

				String swid = (String) termLegData
						.get(LegDataAttributes.P_SWITCH_ID);
				termUri.setParameter(PhConstants.SWITCH_ID, swid);
			}
			if (termLegData.get(LegDataAttributes.P_TRUNK_GRP_ID) != null) {

				String trunkId = (String) termLegData
						.get(LegDataAttributes.P_TRUNK_GRP_ID);
				termUri.setParameter(PhConstants.TRUNK_GRP_ID, trunkId);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " termLegData.get(LegDataAttributes.P_OUT_CALLED_ISUB) "
						+ termLegData.get(LegDataAttributes.P_OUT_CALLED_ISUB));
			}

			String outCalledIsub = (String) termLegData
					.get(LegDataAttributes.P_OUT_CALLED_ISUB);

			// for setting isub parameter in uri and to header
			if (outCalledIsub != null && !outCalledIsub.isEmpty()) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "Setting out isub in user part of URI  "
							+ outCalledIsub);
				}

				String termUser = ((SipURI) termUri).getUser() + ";"
						+ PhConstants.ISUB + "=" + outCalledIsub;
				((SipURI) termUri).setUser(termUser);
			}

			// }

			/**
			 * set the destination trunl group if found in call data. dtg may
			 * have been received either in incoming invite or in CRE flow may
			 * have come with redirect contacts. just to make sure it donot go
			 * to CRE but egres gateway we have put check for this action
			 */
			if (action.getActionType() == Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS) {

				if (callData.get(CallDataAttribute.P_DEST_TGRP) != null
						&& callData.get(CallDataAttribute.P_DEST_TRUNK_CONTEXT) != null) {

					String tgrp = (String) callData
							.get(CallDataAttribute.P_DEST_TGRP);
					String trunkCtxt = (String) callData
							.get(CallDataAttribute.P_DEST_TRUNK_CONTEXT);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Add destination tgrp and trunkcontext to termUri "
								+ tgrp + " , " + trunkCtxt);
					}

					termUri.setParameter(PhConstants.TGRP_PARAM, tgrp);
					termUri.setParameter(PhConstants.TRUNK_CONTEXT, trunkCtxt);

				} else {
					if (callData.get(CallDataAttribute.P_DEST_TRUNK_GROUP) != null) {
						String dtg = (String) callData
								.get(CallDataAttribute.P_DEST_TRUNK_GROUP);

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::Add destination trunk group ( DTG ) to termUri "
									+ dtg);
						}

						termUri.setParameter(PhConstants.DTG_PARAM, dtg);
					}
				}

				/**
				 * bug 1544
				 */

				String termUser = null;

				if (origLegData.get(LegDataAttributes.P_NPDI) != null
						&& !termUri.toString().contains(PhConstants.NPDI_PARAM)) {

					String npdiParam = (String) origLegData
							.get(LegDataAttributes.P_NPDI);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Set NPDI param  to termUri " + npdiParam);
					}

//<<<<<<< HEAD
//					termUser = ((SipURI) termUri).getUser() + ";"
//							+ PhConstants.NPDI_PARAM + "=" + npdiParam;
//					((SipURI) termUri).setUser(termUser);
//=======
					termUser=((SipURI)termUri).getUser()+";"+PhConstants.NPDI_PARAM+"="+npdiParam;
					
					if(termUri instanceof SipURI)
					((SipURI)termUri).setUser(termUser);
//>>>>>>> MPH10.1_DEV
				}
				if (origLegData.get(LegDataAttributes.P_RN) != null
						&& !termUri.toString().contains(PhConstants.RN_PARAM)) {

					String rnParam = (String) origLegData
							.get(LegDataAttributes.P_RN);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Set RN param  to termUri " + rnParam);
					}

//<<<<<<< HEAD
//					termUser = ((SipURI) termUri).getUser() + ";"
//							+ PhConstants.RN_PARAM + "=" + rnParam;
//					((SipURI) termUri).setUser(termUser);
//=======
					termUser=((SipURI)termUri).getUser()+";"+PhConstants.RN_PARAM+"="+rnParam;
					
					if(termUri instanceof SipURI)
					((SipURI)termUri).setUser(termUser);
//>>>>>>> MPH10.1_DEV
				}

			}

			// LinkedList<String> termUriList = new LinkedList<String>();
			// if (action.getActionType() !=
			// Action.ActionType.ACTION_TRANSFER_CONNECT) {
			// /**
			// * For transfered call the to header is not modified and call is
			// routed to proxy from whr it was received
			// */
			// termUriList.add(termUri.toString());
			// headerMap.put(PhConstants.TO_HEADER, termUriList);
			// }

			/**
			 * If from domain is available then create from uri from from domain
			 */
			/*
			 * Need to create from uri like this
			 * sip:+17005554141;cpc=payphone;otg=383;anything=222@domain>
			 */
			StringBuffer fromUser=null;
				if (origInvRequest != null) {
					if (origInvRequest.getFrom().getURI() instanceof SipURI) {
						fromUser = new StringBuffer(
								((SipURI) (origInvRequest.getFrom().getURI()))
										.getUser());
					} else if (origInvRequest.getFrom().getURI() instanceof TelURL) {
						fromUser = new StringBuffer(
								((TelURL) (origInvRequest.getFrom().getURI()))
										.getPhoneNumber());
					}
				}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::Creating from From user :>  "
						+ fromUser);
			}

			if (callData.get(CallDataAttribute.P_MODIFIED_CALLING_NUMBER) != null) {

				modCallingNumber = (String) callData
						.get(CallDataAttribute.P_MODIFIED_CALLING_NUMBER);

				int index = fromUser.indexOf(";");
				if (index != -1) {
					fromUser.replace(0, index, modCallingNumber);
				} else {
					fromUser = new StringBuffer(modCallingNumber);
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::override modified calling user in From user :>  "
							+ fromUser);
				}
			}
			
			if (fromUser == null) {
				PhoneNumber from = (PhoneNumber) origLegData
						.get(LegDataAttributes.P_CALLING_PARTY);
				if (from != null) {
					fromUser = new StringBuffer(from.getAddress());
				}
			}

			String SEMI_COLON = ";";
			String EQUALTO = "=";

			String otg = null;
			String oli = null;
			String cic = null;
			String cpc = null;
			String jip = null;
			if (callData.get(CallDataAttribute.P_ORIG_TRUNK_GROUP) != null
					&& fromUser.toString().indexOf(PhConstants.OTG_PARAM) == -1) {
				otg = (String) callData
						.get(CallDataAttribute.P_ORIG_TRUNK_GROUP);

				fromUser.append(SEMI_COLON).append(PhConstants.OTG_PARAM)
						.append(EQUALTO).append(otg);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Add orig trunk group ( OTG ) to fromuri "
							+ otg);
				}
			}

			// if (callData.get(CallDataAttribute.P_ORIG_LINE_INFO) != null&&
			// fromUser.toString().indexOf(PhConstants.ISUP_OLI_PARAM)==-1) {
			// oli = (String) callData
			// .get(CallDataAttribute.P_ORIG_LINE_INFO);
			//
			// fromUser.append(SEMI_COLON).append(PhConstants.ISUP_OLI_PARAM)
			// .append(EQUALTO).append(oli);
			//
			// if (logger.isDebugEnabled()) {
			// logger.debug(origLegCallId
			// + "::Add orig Line Info ( OTG ) to fromuri " + oli);
			// }
			// }

			if (origLegData.get(LegDataAttributes.P_CIC) != null) {
				cic = (String) origLegData.get(LegDataAttributes.P_CIC);

				fromUser.append(SEMI_COLON).append(PhConstants.CIC_PARAM)
						.append(EQUALTO).append(cic);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Add orig Line Info ( OTG ) to fromuri " + cic);
				}
			}

			/**
			 * Check if CPC needs to be sent out or not ,set is user part if
			 * incoming had it in uder part
			 */
			if (PhConstants.TRUE.equals(SipProtocolConfig
					.getConfigData(SipProtocolConfig.SEND_CPC_PARAM))) {

//<<<<<<< HEAD
//				if (((SipURI) origInvRequest.getFrom().getURI()).getUser()
//						.contains(PhConstants.CPC_PARAM)) {
//=======
				if(origInvRequest!=null &&((SipURI)origInvRequest.getFrom().getURI()).getUser().contains(PhConstants.CPC_PARAM)){
//>>>>>>> MPH10.1_DEV

					if (origLegData.get(LegDataAttributes.P_CPC_ORIGINAL) != null
							&& fromUser.toString().indexOf(
									PhConstants.CPC_PARAM) == -1) {
						cpc = (String) origLegData
								.get(LegDataAttributes.P_CPC_ORIGINAL);

						fromUser.append(SEMI_COLON)
								.append(PhConstants.CPC_PARAM).append(EQUALTO)
								.append(cpc);
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::Add cpc  to fromuri " + cpc);
						}
					}
				}
			}

			if (origLegData.get(LegDataAttributes.P_JIP) != null
					&& fromUser.toString().indexOf(PhConstants.JIP_PARAM) == -1) {
				jip = (String) origLegData.get(LegDataAttributes.P_JIP);
				fromUser.append(SEMI_COLON).append(PhConstants.JIP_PARAM)
						.append(EQUALTO).append(jip);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::Add jip  to fromuri " + jip);
				}
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + "::Created  From user :>  "
						+ fromUser);
			}

			if (termUri instanceof SipURI && termUri.getParameter(PhConstants.SWITCH_ID) != null
					&& termUri.getParameter(PhConstants.TRUNK_GRP_ID) != null
					&& action.getConnectionMode() != Action.CONNECTIONMODE.EQSROUTING) {

				/**
				 * if switch id and TGID available then we need to send CAS ip
				 * in from for dedicated if its terminating number call not eqs
				 */
				SipURI contactUri = ((SipURI) origInvRequest.getRequestURI());
				// .getAddressHeader(PhConstants.CONTACT_HEADER);

				fromUri = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getSipFactory()
						.createSipURI(fromUser.toString(),
								contactUri.getHost().toString());

				((SipURI) fromUri).setPort(contactUri.getPort());
			} else if (termLegData.get(LegDataAttributes.P_FROM_DOMAIN) != null) {

				String fromDomain = (String) termLegData
						.get(LegDataAttributes.P_FROM_DOMAIN);
				// for routing to terminating number
				fromUri = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getSipFactory()
						.createSipURI(fromUser.toString(), fromDomain);
			} else {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::get original from uri from incoming request");
				}
//<<<<<<< HEAD
//				fromUri = (SipURI) origInvRequest.getFrom().getURI().clone();
//				String fromUserStr = fromUser.toString();
//				try {
//					String sendIsubInFrom = SipProtocolConfig
//							.getConfigData(SipProtocolConfig.SEND_ISUB_IN_FROM);
//					if (sendIsubInFrom != null
//							&& PhConstants.FALSE.equals(sendIsubInFrom)) {
//
//						fromUserStr = fromUserStr.replaceAll(";isub=\\d+", "");
//						if (logger.isDebugEnabled()) {
//							logger.debug(origLegCallId
//									+ "::removing isub param from user in outgoing from "
//									+ fromUserStr);
//						}
//
//					}
//				} catch (Exception e) {
//					logger.error("Exception in removing isub param from user",
//							e);
//=======
				String fromUserStr =null;
				if (origInvRequest != null) {
					fromUri = (SipURI) origInvRequest.getFrom().getURI()
							.clone();
					fromUserStr = fromUser.toString();
					try {
						String sendIsubInFrom = SipProtocolConfig
								.getConfigData(SipProtocolConfig.SEND_ISUB_IN_FROM);
						if (sendIsubInFrom != null
								&& PhConstants.FALSE.equals(sendIsubInFrom)) {

							fromUserStr = fromUserStr.replaceAll(";isub=\\d+",
									"");
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ "::removing isub param from user in outgoing from "
										+ fromUserStr);
							}

						}
					} catch (Exception e) {
						logger.error(
								"Exception in removing isub param from user", e);
					}
				} else {

					fromUri = PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getSipFactory()
							.createSipURI(
									fromUser.toString(),
									SipProtocolConfig
									.getConfigData(SipProtocolConfig.FLOATING_IP)
											+ ":5060");
//>>>>>>> MPH10.1_DEV
				}

				if (modCallingNumber != null) {

					if (fromUri instanceof SipURI) {

						((SipURI) fromUri).setUser(fromUserStr);

					} else if (fromUri instanceof TelURL) {
						((TelURL) fromUri).setPhoneNumber(modCallingNumber);
					}
				}
			}

			/**
			 * FROM_DOMAIN shhould go to EQS always
			 */
			if (action.getConnectionMode() == Action.CONNECTIONMODE.EQSROUTING
					|| (action.getActionType() == Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS)) {
				if (termLegData.get(LegDataAttributes.P_FROM_DOMAIN) != null) {

					String fromDomain = (String) termLegData
							.get(LegDataAttributes.P_FROM_DOMAIN);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Adding from domain in from uri to EQS  "
								+ fromDomain);
					}

					// for routing to terminating number
					fromUri = PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getSipFactory()
							.createSipURI(fromUser.toString(), fromDomain);
				}
			}

			/**
			 * Check and apply calling party override on calling party
			 */
//<<<<<<< HEAD
//			if (fromUri instanceof SipURI) {
//				SipProtocolUtil.checkAndApplyCallingPartyOverride(
//						origInvRequest, origLegCallId, origLegData,
//						(SipURI) fromUri, termLegData);
//=======
			if(fromUri instanceof SipURI &&origInvRequest!=null){
			SipProtocolUtil.checkAndApplyCallingPartyOverride(origInvRequest,
					origLegCallId, origLegData, (SipURI)fromUri, termLegData);
//>>>>>>> MPH10.1_DEV
			}

			LinkedList<String> fromUriList = new LinkedList<String>();
			Address fromAddress = PhUtilityServices
					.getInstance(
							(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getSipFactory().createAddress(fromUri);

			/**
			 * Add isup-oli param in parameter instaed of user part
			 */

			String sendoli = SipProtocolConfig
					.getConfigData(SipProtocolConfig.SEND_OLI_PARAM);

			if (PhConstants.TRUE.equals(sendoli)) {
				if (callData.get(CallDataAttribute.P_ORIG_LINE_INFO) != null
						&& !fromUser.toString().contains(
								PhConstants.ISUP_OLI_PARAM)) {

					oli = (String) callData
							.get(CallDataAttribute.P_ORIG_LINE_INFO);

					fromUri.setParameter(PhConstants.ISUP_OLI_PARAM, oli);
					fromAddress = PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getSipFactory().createAddress(fromUri);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Add orig Line Info ( OTG ) to fromuri "
								+ oli);
					}
				}
			} else {
				fromUri.removeParameter(PhConstants.ISUP_OLI_PARAM);
				fromAddress.getURI()
						.removeParameter(PhConstants.ISUP_OLI_PARAM);
			}

			/**
			 * check if cpc is found in sip uri then set it in from uri
			 * otherwise if found in from address then set it in outgoign from
			 * address
			 */
			if (PhConstants.TRUE.equals(SipProtocolConfig
					.getConfigData(SipProtocolConfig.SEND_CPC_PARAM)) &&origInvRequest!=null) {

				String cpcParamUri = origInvRequest.getFrom().getURI()
						.getParameter(PhConstants.CPC_PARAM);
				String cpcFromAdd = origInvRequest.getFrom().getParameter(
						PhConstants.CPC_PARAM);

				if (cpcParamUri != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Add CPC param to fromuri " + cpcParamUri);
					}
					fromAddress.getURI().setParameter(PhConstants.CPC_PARAM,
							cpcParamUri);
				} else if (cpcFromAdd != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ "::Add CPC param to from Address "
								+ cpcParamUri);
					}
					fromAddress.setParameter(PhConstants.CPC_PARAM, cpcFromAdd);
				}
			} else {
				fromAddress.removeParameter(PhConstants.CPC_PARAM);
				fromAddress.getURI().removeParameter(PhConstants.CPC_PARAM);
			}

			// check if Calling number was modified and display name has to be
			// added
			if (modCallingNumber != null
					&& callData
							.get(CallDataAttribute.P_MODIFIED_CALLING_DISPLAY_NAME) != null) {
				String displayName = (String) callData
						.get(CallDataAttribute.P_MODIFIED_CALLING_DISPLAY_NAME);

				fromAddress.setDisplayName(displayName);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Display name added to address:" + displayName
							+ ", from address:" + fromAddress.toString());
				}
			}

			fromUriList.add(fromAddress.toString());

			if (headerMap.get(PhConstants.FROM_HEADER) == null) {
				headerMap.put(PhConstants.FROM_HEADER, fromUriList);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "::Create Term sip request for toUri " + termUri
						+ " FromUri : " + fromUri);
			}

			boolean earlyMedia = (!PhConstants.TRUE.equals(origLegData
					.get(LegDataAttributes.NP_IS_CHARGEABLE_ANN)));

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "::Creating unlinked request from b2bua helper !!!!!!! ");
			}

			/*
			 * dedicated terminating number will go in to header
			 */
			LinkedList<String> termUriList = new LinkedList<String>();

			if (termUri instanceof SipURI && termUri.getParameter(PhConstants.SWITCH_ID) != null
					&& termUri.getParameter(PhConstants.TRUNK_GRP_ID) != null
					&& PhConstants.TRUE.equals(termLegData
							.get(LegDataAttributes.NP_SEND_ORIG_TO_HDR))) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Update terminating number in to header as NP_SEND_ORIG_TO_HDR=TRUE !!!!!!! ");
				}
				/**
				 * update user part for incoming Request URI not the To hdr//
				 * header and ip from the CRE ip
				 */

				PhoneNumber ph = (PhoneNumber) origLegData
						.get(LegDataAttributes.P_DIALED_DIGITS);

				// Address termAddress=(Address)origInvRequest.getTo().clone();
				((SipURI) termUri).setUser(ph.getAddress());// ((SipURI)termAddress.getURI()).getUser());
				// termAddress.setURI(termUri);
				// termUriList.add(termAddress.toString());
				// headerMap.put(PhConstants.TO_HEADER, termUriList);

			} else {
				/*
				 * Switched to header from orig request will go in to header .
				 * switch id and trunk grp id will only go in term uri not the
				 * to header
				 */

				URI toUri = termUri.clone();
				toUri.removeParameter(PhConstants.SWITCH_ID);
				toUri.removeParameter(PhConstants.TRUNK_GRP_ID);
				toUri.removeParameter(PhConstants.TRUNK_CONTEXT);
				toUri.removeParameter(PhConstants.TGRP_PARAM);

				if (action.getActionType() != Action.ActionType.ACTION_TRANSFER_CONNECT) {
					/**
					 * For transfered call the to header is not modified and
					 * call is routed to proxy from whr it was received
					 */
					Address termAddress = PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getSipFactory().createAddress(toUri);
					termUriList.add(termAddress.toString());
					// headerMap.put(PhConstants.TO_HEADER, termUriList);
				}
			}

			if (headerMap.get(PhConstants.TO_HEADER) == null) {
				headerMap.put(PhConstants.TO_HEADER, termUriList);
			}

		}
		}

		/*
		 * Fix for looping issues because of web spidyr contact binding
		 */
		String ip = SipProtocolConfig
				.getConfigData(SipProtocolConfig.FLOATING_IP);
		int port = 5060;
		try {
			port = Integer.parseInt(SipProtocolConfig
					.getConfigData(SipProtocolConfig.SIP_PORT));
		} catch (NumberFormatException ex) {
			logger.error("[PH}SIP port not configured properly. Asssuming 5060.");
		}

		String routeIp = null;
		int routePort = 5060;

		if (termUri.isSipURI()) {
			routeIp = ((SipURI) termUri).getHost();
			routePort = ((SipURI) termUri).getPort();

		} else {
			routeIp = ((TelURL) termUri).getPhoneContext();

		}

		if (ip != null && ip.equals(routeIp) && port == routePort) {
			logger.error("[PH] A loop has been detected. The destination of INVITE request is back to the server. Sending 482.");
			origInvRequest.createResponse(482, "Loop Detected").send();
			return;
		}

//<<<<<<< HEAD
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::HeaderMap is " + headerMap);
		}

//=======
       if(origInvRequest!=null){
//>>>>>>> MPH10.1_DEV
		termSipRequest = origInvRequest.getB2buaHelper().createRequest(
				origInvRequest, false, headerMap);
       }else{
    	   
    		if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "::create term from sip factory");
			}
    	    appSession = SipProtocolUtil
   				.getAppSession((String) callData
   						.get(CallDataAttribute.P_APP_SESSION_ID), (String) callData
   						.get(CallDataAttribute.SERVICE_ID));
    	    termSipRequest= PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
			.getSipFactory().createRequest(appSession, "INVITE", fromUri, termUri);
       }

		if (pAIFromMCContact != null) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "::Add PAIheader from 300 M contact "
						+ pAIFromMCContact);
			}
			termSipRequest.setHeader(PhConstants.PAI_HEADER, pAIFromMCContact);
		}

		/**
		 * update modified calling number in PAI header as well if provided by
		 * application
		 */
		if (modCallingNumber != null) {

			Address paiHeader = termSipRequest
					.getAddressHeader(PhConstants.PAI_HEADER);

			if (paiHeader != null) {

				URI paiUri = paiHeader.getURI();

				StringBuffer paiUser = null;
				SipURI sippaiUri = null;
				TelURL telpaiurl = null;

				if (paiUri.isSipURI()) {
					sippaiUri = (SipURI) paiUri;
					paiUser = new StringBuffer(sippaiUri.getUser());
				} else {
					telpaiurl = (TelURL) paiUri;
					paiUser = new StringBuffer(telpaiurl.getPhoneNumber());
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Need to override modified calling user in existing PAI user :>  "
							+ paiUser);
				}

				int index = paiUser.indexOf(";");
				if (index != -1) {
					paiUser.replace(0, index, modCallingNumber);
				} else {
					paiUser = new StringBuffer(modCallingNumber);
				}

				String paiUserStr = paiUser.toString();
				try {
					String sendIsubInFrom = SipProtocolConfig
							.getConfigData(SipProtocolConfig.SEND_ISUB_IN_FROM);
					if (sendIsubInFrom != null
							&& PhConstants.FALSE.equals(sendIsubInFrom)) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ "::removing isub : pai user..");
						}
						paiUserStr = paiUserStr.replaceAll(";isub=\\d+", "");

					}
				} catch (Exception e) {
					logger.error("Exception in removing isub from pai ", e);
				}

				if (sippaiUri != null) {
					sippaiUri.setUser(paiUserStr);
					termSipRequest.setHeader(PhConstants.PAI_HEADER,
							sippaiUri.toString());
				}

				if (telpaiurl != null) {
					telpaiurl.setPhoneNumber(paiUserStr);
					termSipRequest.setHeader(PhConstants.PAI_HEADER,
							telpaiurl.toString());
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::overrided modified calling user in PAI user New PAI user is :>  "
							+ paiUser);
				}
			}
		}

		/**
		 * Remove refered-to and replaces headers from outgoing transfered
		 * request these headers shuld not be forwarded the outgoing invite
		 * otherwise outgoing gateway will think its transfered request not the
		 * final destination( trasfrreded target)
		 */
		if (action.getActionType() == Action.ActionType.ACTION_TRANSFER_CONNECT) {

			if (termSipRequest.getHeader(PhConstants.REFERRED_BY_HEADER) != null) {
				termSipRequest.removeHeader(PhConstants.REFERRED_BY_HEADER);
			}
			if (termSipRequest.getHeader(PhConstants.REPLACES_HEADER) != null) {
				termSipRequest.removeHeader(PhConstants.REPLACES_HEADER);
			}

		}

		/**
		 * Remove PAI header and p-charge-info header from EQS routing
		 */
		if (action.getConnectionMode() == Action.CONNECTIONMODE.EQSROUTING) {
			termSipRequest.removeHeader(PhConstants.PAI_HEADER);
			termSipRequest.removeHeader(PhConstants.P_CHARGE_INFO);
		} else {

			// update from domain in PAI and p-charge-info headers for all
			// outgoing requests

			String fromDomain = (String) termLegData
					.get(LegDataAttributes.P_FROM_DOMAIN);
			String updatedPaiPciAdddr = SipProtocolConfig
					.getConfigData(SipProtocolConfig.OVERRIDE_PAI_PCI_HDRS_ADDRESS);

			if (fromDomain != null && !fromDomain.isEmpty()
					&& PhConstants.DOMAIN.equals(updatedPaiPciAdddr)) {

				updatePAIAndPchargeInfoHeaders(origLegCallId, termSipRequest,
						fromDomain, -1);

			} else if (PhConstants.CASFIP.equals(updatedPaiPciAdddr)
					&& (origInvRequest.getRequestURI() instanceof SipURI)) {

				SipURI contactUri = ((SipURI) origInvRequest.getRequestURI());

				updatePAIAndPchargeInfoHeaders(origLegCallId, termSipRequest,
						contactUri.getHost(), contactUri.getPort());
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "::Update CAS IP/FIP in PAI and P-CHARGE-INFO headers "
							+ fromDomain);
				}

			}
		}

		/**
		 * Set sendSdp false so that reinvite is sent to orig on receiving
		 * success response from term
		 * 
		 */

		// if(origInvRequest.getSession().getState()
		// .equals(SipSession.State.CONFIRMED )){
		//
		//
		// if (logger.isDebugEnabled()) {
		// logger.debug(origLegCallId
		// +
		// "::orig is already in connected state so negotiate without sdp first we will send reinvite later on");
		// }
		// sendSdp = false;
		// }
		// }

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ": Term sip request is created "
					+ termSipRequest);
		}

		// Send OrigCalledPartyID in Diversion header if set by an application
		PhoneNumber origCalledPartyId = (PhoneNumber) termLegData
				.get(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER);

		String sendDivHeader = (String) termLegData
				.get(LegDataAttributes.NP_SEND_DIVERSION_HDR);

		if (origCalledPartyId != null && PhConstants.TRUE.equals(sendDivHeader)) {
			// Reason for diversion
			String reason = (String) termLegData
					.get(LegDataAttributes.NP_DIVERSION_REASON);

			if (reason == null) {
				reason = "number-translation";
			}

			// Diversion Header
			String diversionHdr = "sip:" + origCalledPartyId.getAddress() + "@"
					+ origInvRequest.getLocalAddr() + ":"
					+ origInvRequest.getLocalPort() + ";reason=" + reason;

			termSipRequest
			.addHeader(PhConstants.DIVERSION_HEADER, diversionHdr);

		}

		// Set all required attributes on term leg
		termSipRequest.getSession().setAttribute(PhConstants.LEG_ID,
				action.getLeg());
		termLegData.set(LegDataAttributes.P_SESSION_ID, termSipRequest
				.getSession().getId());
		termLegData.set(LegDataAttributes.APP_SESSION_ID, termSipRequest
				.getSession().getApplicationSession().getId());

		termLegData
				.set(LegDataAttributes.P_CALL_ID, termSipRequest.getCallId());

	

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Removing existing session expires header");
			}
			termSipRequest.removeHeader(PhConstants.MIN_SE_HEADER);
			termSipRequest.removeHeader(PhConstants.SESSION_EXPIRE_HEADER);

			/*
			 * NOTE:: sTermSessionRefresh - Flag is to indicate whether session
			 * refresh needs to be supported for term or not. Possible values: 0
			 * - Session Refresh is not required 1 - Session refresh required
			 * unconditionally 2 - Session refresh required when requested from
			 * originating side
			 * 
			 * sTermSessionSeValues - Flag is to indicate whether Min-SE and
			 * Session expires values of orig needs to be used or these should
			 * be used from gateway.properties 1 - Use parameters of originating
			 * request 2 - Use parameters from gateway.properties
			 */

			boolean isTimerSupported = false;
			int sTermSessionRefresh = Integer.parseInt(SipProtocolUtil
					.getConfig(SipProtocolConfig.TERM_SESSION_REF_SUPPORT));
			int sTermSessionSeValues = Integer.parseInt(SipProtocolUtil
					.getConfig(SipProtocolConfig.TERM_SESSION_REFRESH_HEADER));
			if (sTermSessionRefresh != 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Session Refresh has to be supported for term");
				}
				long origMinSE = 0;
				long origMaxSE = 0;
				boolean isOrigLegSEApplicable = false;
				if (sTermSessionSeValues == 1 || sTermSessionRefresh == 2) {
					String origMinSEValue = (String) origLegData
							.get(LegDataAttributes.P_SESSION_EXPIRE_MIN_SE);
					String origMaxSEValue = (String) origLegData
							.get(LegDataAttributes.P_SESSION_EXPIRE_TIME);

					if (origMinSEValue != null && !origMinSEValue.isEmpty()) {
						try {
							origMinSE = Long.parseLong(origMinSEValue);
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: Min-SE from orig is " + origMinSE);
							}
						} catch (Exception ex) {
							logger.error(origLegCallId
									+ ":: Min-SE from orig leg is not numeric");
						}
					}
					if (origMaxSEValue != null && !origMaxSEValue.isEmpty()) {
						try {
							origMaxSE = Long.parseLong(origMaxSEValue);
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: Session Expires from orig is "
										+ origMaxSE);
							}
							if (origMaxSE > 0) {
								if (logger.isDebugEnabled()) {
									logger.debug(origLegCallId
											+ ":: Session Refresh is requested from orig");
								}
								isOrigLegSEApplicable = true;
							}
						} catch (Exception ex) {
							logger.error(origLegCallId
									+ ":: SE duration from orig leg is not numeric");
						}
					}
				}

				long termMinSE = 0;
				long termMaxSE = 0;
				if (sTermSessionRefresh == 1 || isOrigLegSEApplicable) {
					if (sTermSessionSeValues == 1) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Use Session Expires value of orig");
						}
						termMinSE = origMinSE;
						termMaxSE = origMaxSE;
					}

					if (termMinSE <= 0) {
						termMinSE = Long.parseLong(SipProtocolUtil
								.getConfig(SipProtocolConfig.TERM_MIN_SE_VAL));
					}

					if (termMaxSE <= 0) {
						termMaxSE = Long.parseLong(SipProtocolUtil.getConfig(

						SipProtocolConfig.TERM_SESSION_EXPIRES_VAL));
					}

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Min-SE value for term is " + termMinSE);
						logger.debug(origLegCallId
								+ ":: Session Expire value for term is "
								+ termMaxSE);
					}

					if (termMaxSE > 0 && termMaxSE >= termMinSE) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Adding session expires header in term request");
						}

						termSipRequest.addHeader(PhConstants.MIN_SE_HEADER,
								Long.toString(termMinSE));

///<<<<<<< HEAD
						// Set Session-Expires
						termSipRequest.addHeader(
								PhConstants.SESSION_EXPIRE_HEADER, termMaxSE
										+ "; " + PhConstants.REFRESHER_TAG
										+ "=" + PhConstants.REFRESHER_UAS);
						isTimerSupported = true;
					}
				}
			}
//=======
		/*
		 * Set is100relSupported to true only when isReliableSupported is TRUE
		 * and party A also supports 100rel
		 */
		boolean is100relSupported = false;
		if (isReliableSupported && origInvRequest!=null) {
			ListIterator<String> supportedHeaderIter = origInvRequest
					.getHeaders(PhConstants.SUPPORTED_HEADER);
			if (supportedHeaderIter != null) {
				while (supportedHeaderIter.hasNext()) {
					String supportedHeader = supportedHeaderIter.next();
					if (supportedHeader.contains(PhConstants.SUPPORTED_100REL)) {
						is100relSupported = true;
						break;
//>>>>>>> MPH10.1_DEV
					}
				}
			}
		}
			
			if (!isIMSCall) {

			// Add supported headers
			if (termSipRequest.getHeader(PhConstants.SUPPORTED_HEADER) != null) {
				termSipRequest.removeHeader(PhConstants.SUPPORTED_HEADER);
			}

			/*
			 * Set is100relSupported to true only when isReliableSupported is
			 * TRUE and party A also supports 100rel
			 */
			//boolean is100relSupported = false;
			if (isReliableSupported) {
				ListIterator<String> supportedHeaderIter = origInvRequest
						.getHeaders(PhConstants.SUPPORTED_HEADER);
				if (supportedHeaderIter != null) {
					while (supportedHeaderIter.hasNext()) {
						String supportedHeader = supportedHeaderIter.next();
						if (supportedHeader
								.contains(PhConstants.SUPPORTED_100REL)) {
							is100relSupported = true;
							break;
						}
					}
				}
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: is100relSupported "
						+ is100relSupported + "isReliableSupported"
						+ isReliableSupported);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: timer and 100rel supported flags are isTimerSupported "
						+ isTimerSupported + " is100relSupported "
						+ is100relSupported);
			}

			if (isTimerSupported && is100relSupported) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Add timer and 100rel both are supported ");
				}
				termSipRequest.addHeader(PhConstants.SUPPORTED_HEADER,
						PhConstants.SUPPORTED_TIMER_100REL);
			} else if (isTimerSupported && !is100relSupported) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Add timer is supported ");
				}
				termSipRequest.setHeader(PhConstants.SUPPORTED_HEADER,
						PhConstants.SUPPORTED_TIMER);
			} else if (!isTimerSupported && is100relSupported) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Add 100 rel is supported ");
				}
				termSipRequest.setHeader(PhConstants.SUPPORTED_HEADER,
						PhConstants.SUPPORTED_100REL);
			}

		}else{
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Not updated supported and Session-Expires for IMS call");
			}
		}

		if (PhConstants.TRUE
				.equals(SipProtocolConfig
						.getConfigData(SipProtocolConfig.SEND_USER_PHONE_PARAM_TO_TERM))) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: set user=phone paramter in request uri");
			}

			termUri.setParameter("user", "phone");
		}

		/*
		 * final Request-URI is
		 */
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: setRequestURI in Term Invite "
					+ termUri);
		}

		// add trunk group related attributes if present. // was added and
		// commented again for telepacific ,keeping this code for some time
		// String trunkGroup = (String)
		// termLegData.get(LegDataAttributes.NP_TRUNK_GROUP);
		// String trunkGroupContext = (String)
		// termLegData.get(LegDataAttributes.NP_TRUNK_GROUP_CONTEXT);
		//
		// if (trunkGroup != null && trunkGroupContext != null) {
		//
		// PhoneNumber destPhoneNum = (PhoneNumber) termLegData
		// .get(LegDataAttributes.P_DESTINATION_NUMBER);
		//
		// String uri = "sip:" + destPhoneNum.getAddress()
		// + PhConstants.SEMI_COLON_STR + PhConstants.TGRP_PARAM
		// + PhConstants.EQUALS_STR + trunkGroup
		// + PhConstants.SEMI_COLON_STR + PhConstants.TRUNK_CONTEXT
		// + PhConstants.EQUALS_STR + trunkGroupContext;
		// URI termUriString = PhUtilityServices.getInstance().getSipFactory()
		// .createURI(uri);
		// // termUriString.setParameter(PhConstants.TGRP_PARAM, trunkGroup);
		// // termUriString.setParameter(PhConstants.TRUNK_CONTEXT,
		// trunkGroupContext);
		// if (logger.isDebugEnabled()) {
		// logger.debug(origLegCallId + ":: Terminating URI="
		// + termUriString);
		// }
		// termSipRequest.setRequestURI(termUriString);
		// } else {

//		String transport=SipProtocolUtil.getTransportFromVia(origLegCallId, origInvRequest);
//		
//		if (logger.isDebugEnabled()) {
//			logger.debug(origLegCallId + ":: set Transport param in RURI as  "+transport);
//		}
		//termUri.setParameter("transport", transport);
		
		termSipRequest.setRequestURI(termUri);

		String disableProxy = SipProtocolConfig
				.getConfigData(SipProtocolConfig.DISABLE_OUTBOUND_PROXY);

		if (PhConstants.TRUE.equalsIgnoreCase(disableProxy)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: DISABLE_OUTBOUND_PROXY ");
			}
			termSipRequest.getSession().setAttribute("DISABLE_OUTBOUND_PROXY",
					true);
		}

		/*
		 * SBTM-UAT-1401 Fix- Start this timer on first 18x reponse from party-B
		 * so commented here //Start no-answer timer if
		 * (action.getNoAnswerFlag()) { long noAnswerTime =
		 * action.getNoAnswerTimer() <= 0 ? PhConstant.DEF_NO_ANSWER_TIME :
		 * action .getNoAnswerTimer() * 1000; if (logger.isDebugEnabled()) {
		 * logger.debug(origLegCallId + " :: Start no answer timer for ms " +
		 * noAnswerTime); }
		 * 
		 * //This timer should be non-persistable to avoid data replication
		 * SipIsupHelper.startTimer(cTimerService, appSession, noAnswerTime,
		 * false, PhConstant.NO_ANSWER_TIMER); }
		 */

		/*
		 * If orig leg SDP is already used then do not send orig leg SDP to term
		 * leg
		 */
		// Content, Content Type, Content Length
		// reeta added for SIP-T support
		boolean isDialout = false;
//<<<<<<< HEAD
//		Object dialout = callData.get(CallDataAttribute.P_DIALOUT);
//		if (dialout != null) {
//=======
		dialout = callData.get(CallDataAttribute.P_DIALOUT);
		if(dialout != null){
//>>>>>>> MPH10.1_DEV
			isDialout = (Boolean) dialout;
			appSession.setAttribute("P_DIALOUT", dialout);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("isDialOut : " + isDialout);
		}

		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (leg1Data.get(LegDataAttributes.P_SIGNALING_TYPE_KEY) != null) {
			if (logger.isDebugEnabled()) {
				logger.debug("Setting P_SIGNALING_TYPE_KEY in leg2 from leg1 data");
			}

			termLegData.set(LegDataAttributes.P_SIGNALING_TYPE_KEY,
					leg1Data.get(LegDataAttributes.P_SIGNALING_TYPE_KEY));
		}

		if (termLegData.get(LegDataAttributes.P_SIGNALING_TYPE_KEY) == SignalingTypeEnum.SIGNALING_TYPE.SIP_T) {
			SipIsupHelper.setMultipartContentTerm(appSession, termSipRequest,
					sendSdp);
//<<<<<<< HEAD
//			termLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
//					PhConstants.TRUE);
//		} else if (origInvRequest.getContentType() != null
//=======
			termLegData.set(
					LegDataAttributes.NP_IS_OFFER_SENT, PhConstants.TRUE);
		} else if (origInvRequest!=null &&origInvRequest.getContentType() != null
//>>>>>>> MPH10.1_DEV
				&& origInvRequest.getContentType().startsWith(
						PhConstants.APPLICATION_SDP) && sendSdp && !isDialout) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Set request sdp content in Term Invite Content");
			}

			int sendSipItoTerm = 0;

			if (termLegData.get(LegDataAttributes.NP_SEND_DNIS_IN_SIPT) != null) {
				sendSipItoTerm = (Integer) termLegData
						.get(LegDataAttributes.NP_SEND_DNIS_IN_SIPT);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SEND_DNIS_IN_SIPT from service is "
							+ sendSipItoTerm);
				}
			} else {

				String sendDnis = SipProtocolConfig
						.getConfigData(SipProtocolConfig.SEND_DNIS_IN_SIPT);

				sendSipItoTerm = PhConstants.TRUE.equals(sendDnis) ? 1 : 0;

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SEND_DNIS_IN_SIPT from mph properties is "
							+ sendSipItoTerm);
				}
			}

			if (sendSipItoTerm > 0 && origCalledPartyId != null
					&& fromUri instanceof SipURI) {
				SipIsupHelper.createSipTBodyPart(
						callData,
						action,
						origInvRequest,
						((SipURI) fromUri).getUser().split(
								PhConstants.SEMI_COLON_STR)[0], termSipRequest);
			} else {
				MultipartBody origReceivedSDP=(MultipartBody) origLegData.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
				termSipRequest.setContent(origReceivedSDP.getContent(),//origInvRequest.getContent(),
						origInvRequest.getContentType());
			}

			termLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
					PhConstants.TRUE);

		} else if (origInvRequest!=null &&origInvRequest.getContentType() != null
				&& origInvRequest.getContentType().startsWith(
						PhConstants.MULTIPART_MIXED) && sendSdp) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Set request sdp content in Term Invite Content parsed from orig mulipart");
			}
			MultipartBody origReceivedSDP=(MultipartBody) origLegData.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
			termSipRequest.setContent(origReceivedSDP.getContent()
				//	SipProtocolUtil
						//	.getSdpContent(origInvRequest, origLegCallId).getContent()
							, PhConstants.APPLICATION_SDP);
			termLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
					PhConstants.TRUE);

//<<<<<<< HEAD
//		} else {
//=======
		}else if (origInvRequest!=null &&origInvRequest.getContentType() != null
				&& origInvRequest.getContentType().startsWith(
						PhConstants.APPLICATION_SDP) && sendSdp) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Set request sdp content in Term Invite ");
			}
			MultipartBody origReceivedSDP=(MultipartBody) origLegData.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);
			termSipRequest.setContent(origReceivedSDP.getContent() ,//origInvRequest.getContent(),
					PhConstants.APPLICATION_SDP);
			termLegData.set(
					LegDataAttributes.NP_IS_OFFER_SENT, PhConstants.TRUE);

		}else {
//>>>>>>> MPH10.1_DEV
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Invalid content or sendSdp flag is FALSE, do nothing");
			}
			termLegData.set(LegDataAttributes.NP_IS_OFFER_SENT,
					PhConstants.FALSE);

			if (!sendSdp && termSipRequest.getContentLength() > 0) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: sendSdp is false but content is there in termRequest--->"
							+ termSipRequest);
				}

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: sendSdp is false so forcefuly set Term Invite Content null---> by removing content headers");
				}
				termSipRequest.setContent("", PhConstants.APPLICATION_SDP);
				// termSipRequest.removeHeader("Content-Type");
				// termSipRequest.removeHeader("Content-Length");
			} else if (isDialout) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "sending invite without sdp..");
				}

				termSipRequest.setContent("", PhConstants.APPLICATION_SDP);
			}
		}

		/**
		 * P-sig-info header will come in 300 multiple choices so if service
		 * asks for trying on these multiple contacts the add this header in
		 * outgoing requests
		 */
		if (action.getActionType() == Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS
				&& callData.get(CallDataAttribute.NP_SIG_INFO) != null) {

			termSipRequest.addHeader(PhConstants.P_SIG_INFO_HEADER,
					(String) callData.get(CallDataAttribute.NP_SIG_INFO));
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Send INVITE request to terminating with new call id "
					+ termSipRequest.getCallId());
		}

		/**
		 * For adding custom headers in code provided in leg
		 */
	//	if (action.isAddCustomHeader()) {
			SipProtocolUtil.addCustomHeaders(callData, termLegData,
					termSipRequest);
	//	}

		/**
		 * For adding invite attributes custom header, this will be used for a
		 * particular leg e.g. in case of serial parallel ringing
		 */

		SipProtocolUtil.addInviteAttributesCustomHeaders(origLegCallId,
				inviteAttributes, termSipRequest);

		if (logger.isInfoEnabled()) {
			logger.info(origLegCallId + ":: setInvalidateWhenReady(false)");
		}
		termSipRequest.getSession().setInvalidateWhenReady(false);

		if (PhConstants.TRUE.equals(termLegData
				.get(LegDataAttributes.NP_REMOVE_PRIVACY_HEADER))) {
			/**
			 * by default PAI is not removed from privacy
			 */
			if (PhConstants.TRUE.equals(SipProtocolConfig
					.getConfigData(SipProtocolConfig.REMOVE_PAI_PRIVACY))) {
				termSipRequest.removeHeader(PhConstants.PAI_HEADER);
			}
			termSipRequest.removeHeader(PhConstants.PRIVACY_HEADER);
			termSipRequest.removeHeader(PhConstants.PROXY_REQUIRES_HEADER);
			termSipRequest.removeHeader(PhConstants.P_PREFFERED_IDENTITY);
		}

		String outPriorityHdr = (String) termLegData
				.get(LegDataAttributes.NP_OUT_RES_PRIORITY_HEADER);
		if (outPriorityHdr != null) {
			termSipRequest.setHeader(PhConstants.RES_PRIORITY_HEADER,
					outPriorityHdr);
		}

		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig
				.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
		if (callData.get(CallDataAttribute.P_CHARGE_VECTOR) != null
				&& needToAddPCHargeVector) {

			String pChargeVect = (String) callData
					.get(CallDataAttribute.P_CHARGE_VECTOR);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: set P_CHARGE_VECTOR header on outging request "
						+ pChargeVect);
			}
			termSipRequest.setHeader(PhConstants.P_CHARGE_VECTOR, pChargeVect);
		}

//<<<<<<< HEAD
//		if (origInvRequest.getHeader(PhConstants.REPLACES_HEADER) != null) {
//=======

		if (origInvRequest!=null &&origInvRequest.getHeader(PhConstants.REPLACES_HEADER) != null) {
//>>>>>>> MPH10.1_DEV

			String replaces = origInvRequest
					.getHeader(PhConstants.REPLACES_HEADER);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: set Replaces header on outging request "
						+ replaces);
			}
			termSipRequest.setHeader(PhConstants.REPLACES_HEADER, replaces);
		}

		/**
		 * added support for updating sdp for web rtc
		 */

		if (!isDialout) {
			SipProtocolUtil.updateWebRtcSdp(origInvRequest, termSipRequest);
//<<<<<<< HEAD
//=======
		}	
		
		SipProtocolUtil.updatePaniCellIdInOutgoing(termSipRequest,termLegData);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: term sip request is "
					+ termSipRequest);
//>>>>>>> MPH10.1_DEV
		}
		termSipRequest.send();

		callData.set(CallDataAttribute.P_TERM_LEG_CALL_ID,
				termSipRequest.getCallId());

		termLegData.set(
				LegDataAttributes.P_INVITE_PENDING_TRANS, PhConstants.TRUE);
		
		Boolean dialoutCheck = (Boolean) callData.get(CallDataAttribute.P_DIALOUT);
		Protocol protocol = (Protocol) callData.get(CallDataAttribute.P_PROTOCOL);
		
		if (lastAction.getConnectionMode() == CONNECTIONMODE.EQSROUTING
				&& dialoutCheck != null && dialoutCheck.booleanValue() == true) {
			if(protocol == Protocol.AIN_SCF){
				if(origLegData.get(LegDataAttributes.P_LIDB_QUERY_TYPE)==null){
				termLegData.set(LegDataAttributes.P_LEG_SS7_STATE,
						AinCallStates.PSX_ROUTING);
				}
			}else if(protocol == Protocol.ITUINAPCS1_SCF || protocol == Protocol.ITUINAPCS2_SCF){
				termLegData.set(LegDataAttributes.P_LEG_SS7_STATE,
						InapCallStates.PSX_ROUTING);
			}else if(protocol == Protocol.CAPV2_SCF){
				termLegData.set(LegDataAttributes.P_LEG_SS7_STATE,
						CapV2CallStates.PSX_ROUTING);
			}
		} else {
			termLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
					State.CONN_IN_PROGRESS);
		}
		SipProtocolUtil.setSentSdp(termSipRequest, termLegData, origLegCallId);

		// Store the session refresh timer value
		SipProtocolUtil.setSessionExpiryTime(termSipRequest, termLegData,
				origLegCallId);

		// Commenting the replication as discussed in Meeting on 24th Feb '16.
		// if (logger.isDebugEnabled()) {
		// logger.debug(origLegCallId
		// + ":: Replicate the session on peer node");
		// }
		// PhUtilityServices.getInstance().getAppDataReplicator()
		// .doReplicate(appSession);

	}

	/**
	 * This method is used t update host info in PAI and PCI headers if enabled
	 * in mph sip properties
	 * 
	 * @param origLegCallId
	 * @param termSipRequest
	 * @param host
	 * @param port
	 * @throws ServletParseException
	 */
	private static void updatePAIAndPchargeInfoHeaders(String origLegCallId,
			SipServletRequest termSipRequest, String host, int port)
			throws ServletParseException {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "::updatePAIAndPchargeInfoHeaders Host/Port in PAI and P-CHARGE-INFO headers "
					+ host + " port " + port);
		}
		ListIterator<Address> paiheaders = termSipRequest
				.getAddressHeaders(PhConstants.PAI_HEADER);
		termSipRequest.removeHeader(PhConstants.PAI_HEADER);

		while (paiheaders.hasNext()) {
			Address paiAddr = paiheaders.next();
			if (paiAddr.getURI() instanceof SipURI) {
				((SipURI) paiAddr.getURI()).setHost(host);
				((SipURI) paiAddr.getURI()).setPort(port);
				termSipRequest.addAddressHeader(PhConstants.PAI_HEADER,
						paiAddr, false);
			} else {
				termSipRequest.addAddressHeader(PhConstants.PAI_HEADER,
						paiAddr, false);
			}
		}

		ListIterator<Address> pciheaders = termSipRequest
				.getAddressHeaders(PhConstants.P_CHARGE_INFO);
		termSipRequest.removeHeader(PhConstants.P_CHARGE_INFO);

		while (pciheaders.hasNext()) {
			Address pciAddr = pciheaders.next();
			if (pciAddr.getURI() instanceof SipURI) {
				((SipURI) pciAddr.getURI()).setHost(host);
				((SipURI) pciAddr.getURI()).setPort(port);
				termSipRequest.addAddressHeader(PhConstants.P_CHARGE_INFO,
						pciAddr, false);
			} else {
				termSipRequest.addAddressHeader(PhConstants.P_CHARGE_INFO,
						pciAddr, false);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "::updatePAIAndPchargeInfoHeaders leaving");
		}
	}

	/**
	 * This method execute the PLAY_ANN action, requested by services
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	public static void playAnnouncement(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside playAnnouncement");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside playAnnouncement with appSession");
			logger.debug(origLegCallId + ":: Play ann on " + connType);
			logger.debug(origLegCallId + ":: State is " + state);

		}

		if ((state == State.MS_CONNECTED || state == State.MS_PLAY
				|| state == State.MS_PLAY_COLLECT
				|| state == State.MS_PLAY_RECORD || state == State.MS_RECORD)) {
			try {
				GroupedMsSessionController msController = SipProtocolUtil
						.getMsController(appSession, legData, action.getLeg());
				String baseUriStr = msController.getMediaServer()
						.getAnnouncementBaseURI().toString();

				MsPlaySpec msPlaySpec = new MsPlaySpec();
				SipProtocolUtil.formPlaySpec(callData, legData, baseUriStr,
						msPlaySpec, null);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Invoke play on msController");
				}

				msPlaySpec.setPlayExit(null, null, "play.end play.amt");

				msController.play(msPlaySpec);
				legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_PLAY);
			} catch (Exception ex) {
				logger.error(origLegCallId
						+ ":: Play announcement failed. Error is "
						+ ex.getMessage());

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Play announcement failed",
							ex);
					logger.info(origLegCallId
							+ ":: Notify service that play announcement failed");
				}
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.EXCEP_PLAY);
				legData.set(
						LegDataAttributes.P_CAUSE_CODE,
						Integer.parseInt(SipProtocolUtil
								.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(EventType.EVENT_PLAY_FAILURE,
						Protocol.SIP, action.getLeg());

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}
		} else if (state == State.MS_CONF_JOINED) {
			try {
				SBBFactory sbbFactory = (SBBFactory) appSession
						.getAttribute(PhConstants.ATTRIB_SBB_FACTORY);
				if (sbbFactory == null) {

					logger.error("[PH] ConferenceHandler:connectParticipant SBBFactory not available in application session.");
					return;
				}
				String confId = (String) appSession
						.getAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID);
				ConferenceController confController = (ConferenceController) sbbFactory
						.getSBB(ConferenceController.class.getName(),
								confId,
								appSession,
								PhUtilityServices
										.getInstance(
												(String) callData
														.get(CallDataAttribute.SERVICE_ID))
										.getServletContext());

				String baseUriStr = null;
				if (confController.getMediaServer() == null) {
					if (logger.isDebugEnabled()) {
						logger.debug("Media Server is null");
					}
					MediaServerSelector msSelector = (MediaServerSelector) PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getServletContext()
							.getAttribute(MediaServerSelector.class.getName());
					MediaServer ms = msSelector
							.selectByCapabilities(PhConstants.MS_CAPABILITIES);

					baseUriStr = ms.getAnnouncementBaseURI().toString();
				} else {
					baseUriStr = confController.getMediaServer()
							.getAnnouncementBaseURI().toString();
				}

				MsPlaySpec msPlaySpec = new MsPlaySpec();

				SipProtocolUtil.formPlaySpec(callData, legData, baseUriStr,
						msPlaySpec, null);
				msPlaySpec.setPlayExit(null, null, "play.end play.amt");

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: invoke playRecord on confController");
				}
				confController.play(msPlaySpec);
				legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_PLAY);
			} catch (Exception ex) {
				logger.error(origLegCallId
						+ ":: Play announcement failed. Error is "
						+ ex.getMessage());

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Play announcement failed",
							ex);
					logger.info(origLegCallId
							+ ":: Notify service that play announcement failed");
				}
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.EXCEP_PLAY);
				legData.set(
						LegDataAttributes.P_CAUSE_CODE,
						Integer.parseInt(SipProtocolUtil
								.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(EventType.EVENT_PLAY_FAILURE,
						Protocol.SIP, action.getLeg());

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}
		} else {
			logger.error(origLegCallId
					+ ":: Play announcement invoked in invalid state, drop call");
			logger.error(origLegCallId + ":: ConnType = " + connType
					+ ", state=" + state);
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.UNEXP_ACT_PLAY);
			legData.set(
					LegDataAttributes.P_CAUSE_CODE,
					Integer.parseInt(SipProtocolUtil
							.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			dropCall(appSession);
		}

		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
	}

	/**
	 * This method execute the ACTION_PLAY_COLLECT action, requested by services
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void playAndCollect(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside playAndCollect");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		boolean earlyMedia = (!PhConstants.TRUE.equals(legData
				.get(LegDataAttributes.NP_IS_CHARGEABLE_ANN)));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside playAndCollect with appSession");
			logger.debug(origLegCallId + ":: PlayAndCollect ann on " + connType);
			logger.debug(origLegCallId + ":: State is " + state);

		}
		if ((state == State.MS_CONNECTED) || (state == State.MS_PLAY)
				|| (state == State.MS_PLAY_COLLECT)) {
			try {
				GroupedMsSessionController msController = SipProtocolUtil
						.getMsController(appSession, legData, action.getLeg());
				String baseUriStr = msController.getMediaServer()
						.getAnnouncementBaseURI().toString();

				MsPlaySpec msPlaySpec = new MsPlaySpec();
				SipProtocolUtil.formPlaySpec(callData, legData, baseUriStr,
						msPlaySpec, null);

				MsCollectSpec msCollectSpec = new MsCollectSpec();
				SipProtocolUtil.formCollectSpec(callData, legData, baseUriStr,
						msCollectSpec, msController);

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: msController.getMediaServer().get "
							+ msController.getMediaServer()
									.getAdaptorClassName());
					logger.debug(origLegCallId
							+ ":: Invoke playCollect on msController");
				}
				legData.remove(LegDataAttributes.P_COLLECTED_DIGITS);

				if (!earlyMedia) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Set early media flag as FALSE on msController");
					}
					msController.setAttribute(SBB.EARLY_MEDIA, "false");
				}
				msController.playCollect(msPlaySpec, msCollectSpec);
				legData.set(LegDataAttributes.P_LEG_SIP_STATE,
						State.MS_PLAY_COLLECT);
			} catch (Exception ex) {
				logger.error(origLegCallId
						+ ":: Play collect failed. Error is " + ex.getMessage());

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Play and collect announcement failed", ex);
					logger.info(origLegCallId
							+ ":: Notify service that play collect announcement failed");
				}
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.EXCEP_PLAYCOLLECT);
				legData.set(
						LegDataAttributes.P_CAUSE_CODE,
						Integer.parseInt(SipProtocolUtil
								.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(EventType.EVENT_PNC_FAILURE,
						Protocol.SIP, action.getLeg());

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}
		} else {

			logger.error(origLegCallId
					+ ":: Play collect invoked in invalid state, drop call");
			logger.error(origLegCallId + ":: ConnType = " + connType
					+ ", state=" + state);
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.UNEXP_ACT_PLAYCOL);
			legData.set(
					LegDataAttributes.P_CAUSE_CODE,
					Integer.parseInt(SipProtocolUtil
							.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			dropCall(appSession);
		}

		SipProtocolUtil.incrementNetworkTransactions(callData, 1);

	}

	/**
	 * This method execute the DISCONNECT_IVR action, requested by services. In
	 * case of an exception in disconnecting ivr connection, call is dropped
	 * with CV=41.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	public static void disconnectIvr(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside disconnectIvr");
		}
		String legId = action.getLeg();
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(legId));
		ConnectionType connType = null;
		Protocol protocol = null;

		try {

			// fetch protocol, certail decision to be made based on protocol
			if (null != callData.get(CallDataAttribute.P_PROTOCOL)) {
				protocol = (Protocol) callData
						.get(CallDataAttribute.P_PROTOCOL);
			}

			State state = (State) legData
					.get(LegDataAttributes.P_LEG_SIP_STATE);
			connType = (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Ms disconnect called for Leg = " + legId
						+ "ConnType = " + connType + " CallState = " + state
						+ " Protocol:" + protocol);
				logger.debug(origLegCallId
						+ ":: Invoke disconnectMediaServer on msController");
			}

			GroupedMsSessionController msController = SipProtocolUtil
					.getMsController(appSession, legData, legId);

			boolean stopHandler = false;
			boolean isDialOutCall = false;
			if (callData.get(CallDataAttribute.P_DIALOUT) != null) {
				isDialOutCall = (Boolean) callData
						.get(CallDataAttribute.P_DIALOUT);
			}

			if (isDialOutCall) {
				if (logger.isDebugEnabled()) {
					logger.debug("dialout case, so updating the protocol to Sip, to avoid disconnecting both parties.");
				}
				callData.set(CallDataAttribute.P_PROTOCOL, Protocol.SIP);
				protocol = (Protocol) callData
						.get(CallDataAttribute.P_PROTOCOL);
			}
//<<<<<<< HEAD
//			// In case of AIN Protocol, we need to disconnect both A- and
//			// B-party.
//			// Switch on receiving BYE will get Resource Clear as input.
//			if (protocol != null && protocol == Protocol.AIN_SCF) {
//				msController.disconnect();
//=======
			// In case of AIN Protocol, we need to disconnect both A- and B-party.
			// Switch on receiving BYE will get Resource Clear as input. 
			if(protocol != null && protocol == Protocol.AIN_SCF){
			
//>>>>>>> MPH10.1_DEV

				// In case of AIN Call flow, we do not want to notify
				// application reagrding MS_DISCONNECTED
				// On SIP cleanup. We want it be sent in case resource clear is
				// received. This flag will stop
				// SIP in MediaEvent Listner to stop sending notification.
				// However in case of Handoff, let it notify application as in
				// Handoff response is sent in AIN:
				Object ainState = legData
						.get(LegDataAttributes.P_LEG_SS7_STATE);
				if (ainState != null) {
					
					msController.setAttribute(MsSessionController.PARTY_A_REASON_HDR, "Q.850 ;cause=16 ;text=\"Normal call clearing\"");
					if (ainState.equals(AinCallStates.ASSIST)) {
						msController.disconnect();
						action.setMsDiconnectedForAinCall(true);
					} else {
						// handoff - B2BUA mode
						msController.disconnectMediaServer();
						action.setMsDiconnectedForAinCall(false);
						stopHandler = true;
					}

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ " Handling for AIN Protocol, in state:"
								+ ainState + ", flag:" + stopHandler);
					}
				}

			} else {
				msController.disconnectMediaServer();
			}

			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_DISCONNECTED);
			legData.set(LegDataAttributes.P_LEG_CURRENT_ACTION, action);

			SipProtocolUtil.incrementNetworkTransactions(callData, 1);
			msController.getB().setAttribute(
					PhConstants.MS_BYE_TRXN_INCREMENTED, PhConstants.TRUE);

			SipSession legSipSession = SipProtocolUtil
					.getSipSessionFromSessionId(origLegCallId, appSession,
							(String) legData
									.get(LegDataAttributes.P_SESSION_ID));
			ServiceInterface serviceHandler = PhUtilityServices.getInstance(
					(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();

			if (!stopHandler) {
				legSipSession.setHandler(serviceHandler.getServletName());
			}

			/*
			 * Below code is added by reeta to take care of correlation
			 * scenarion in which on disconnectIVR operation the correlated si
			 * ivr leg is disconnected . and correlated orig sip leg is
			 * disconnected when BYE is sent by SS7 n/w when DFC is sent by Inap
			 * PH o SS7 network . but in case bye is not received from the n/w
			 * then we need to clean this correlated orig leg thats why we are
			 * started a timer below.
			 */
			Object ss7StateObj = legData.get(LegDataAttributes.P_LEG_SS7_STATE);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != null) {
				if (ss7StateObj.equals(InapCallStates.ASSIST)
						|| ss7StateObj.equals(AinCallStates.ASSIST)) {

					if (protocol != Protocol.AIN_SCF) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: INAP Call State is  ASSIST start timer to cleanup ETC invite leg .if bye is not received from ss7 network on dfc we will send bye on this timer expiry");
						}
						SipProtocolUtil
								.startTimer(
										appSession,
										SipProtocolUtil
												.getCorrelatedOrigCleanupTime(appSession),
										true,
										PhConstants.CORRELATED_ORIG_CLEANUP_TIMER);

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Sending DFC for INAP..");
						}
						InapCS1MediaServerHelper.sendAssistDfc(
								SipProtocolUtil.getTcapSession(appSession),
								action);
					}
				}
			}
		} catch (Exception ex) {
			logger.error(origLegCallId + ":: Failed to disconnect ivr "
					+ connType + ". Error is " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Error in disconnecting ivr.",
						ex);
			}
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_DISCON_IVR);
			legData.set(
					LegDataAttributes.P_CAUSE_CODE,
					Integer.parseInt(SipProtocolUtil
							.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			dropCall(appSession);
		}
	}

	/**
	 * Tis method is used to try the redirect contacts received in either 300 or
	 * 302
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void tryRecievedRedirectContacts(
			SipApplicationSession appSession, CallData callData, Action action)
			throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside tryRecievedRedirectContacts() ");

		}
		connectTerm(appSession, callData, action);
	}

	/**
	 * This method is used to perform transfer called by Application using
	 * Action ACTION_ALLOW_TRANSFER
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @param dialogManager
	 * @throws Exception
	 */
	public static void allowAndPerformCallTransfer(
			SipApplicationSession appSession, CallData callData, Action action,
			AseDialogManager dialogManager) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside allowAndPerformCallTransfer() ");
		}

		/**
		 * get new trasnfered request from the call data which has got trasnfred
		 * as in consultation case old calldata is passed in transfer initiated
		 * event to application
		 */
		SipSession newtransferedSession = (SipSession) callData
				.get(CallDataAttribute.NP_TRANSFERED_SESSION);

		if (PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.NP_IS_CONSULTED_TRANSFER))
				|| (newtransferedSession != null)) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: It is consulted transfer or invite received for blind/attended so need to handle pending transfered invite request ");
			}

			if (newtransferedSession != null) {

				// LegData termLegData = (LegData)
				// callData.get(CallDataAttribute.valueOf(action.getLeg()));

				// InviteAttributes inviteAttributes = (InviteAttributes)
				// termLegData
				// .get(LegDataAttributes.INVITE_ATTRIBUTES);

				/**
				 * get transferedSession and pending request from new calldata
				 * as in consultation case old calldata is passed in transfer
				 * initiated event to application
				 */

				CallData newtransferedCallData = SipProtocolUtil
						.getCallData(newtransferedSession
								.getApplicationSession());

				SipSession transferedSession = (SipSession) newtransferedCallData
						.get(CallDataAttribute.NP_TRANSFERED_SESSION);

				SipServletRequest newtransferdRequest = (SipServletRequest) newtransferedCallData
						.get(CallDataAttribute.NP_TRANSFERED_PENDING_INVITE_REQ);

				newtransferedCallData.set(
						CallDataAttribute.NP_IS_CONSULTED_TRANSFER,
						PhConstants.FALSE);

				callData.set(CallDataAttribute.NP_IS_CONSULTED_TRANSFER,
						PhConstants.FALSE);

				callData.remove(CallDataAttribute.NP_TRANSFERED_SESSION);
				newtransferedCallData
						.remove(CallDataAttribute.NP_TRANSFERED_SESSION);
				newtransferedCallData
						.remove(CallDataAttribute.NP_TRANSFERED_PENDING_INVITE_REQ);

				/**
				 * create call trasnfer leg data
				 */
				// termLegData = new LegData();
				// termLegData.set(LegDataAttributes.P_CONNECTION_TYPE,ConnectionType.TERM_CONNECTION);
				// termLegData.set(LegDataAttributes.INVITE_ATTRIBUTES,
				// inviteAttributes);

				// callData.set(CallDataAttribute.P_LEG2, termLegData);
				// newtransferedCallData.set(CallDataAttribute.P_LEG2,
				// termLegData);

				handleCallTransferInvite(newtransferdRequest,
						newtransferdRequest.getApplicationSession(),
						transferedSession, newtransferedCallData);
			} else {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: It is consulted transfer No new trasnfere session found in old trasnfrered call data ");
				}
			}

			return;

		} else {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: It is blind/attended transfer so need to  handle Refer pending request ");
			}

		}

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		SipServletRequest referReqReecived = (SipServletRequest) legData
				.get(LegDataAttributes.P_PENDING_REFER);

		// Connection type from where response received
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);

		/**
		 * Mark state of the user as transfered call
		 */
		legData.set(LegDataAttributes.P_LEG_SIP_STATE,
				State.CALL_TRANSFER_IN_PROGRESS);

		LegData peerLegData = SipProtocolUtil
				.getPeerLegData(callData, connType);

		peerLegData.set(LegDataAttributes.P_LEG_SIP_STATE,
				State.CALL_TRANSFER_IN_PROGRESS);

		SipSession peerLegSipSession = SipProtocolUtil
				.getSipSessionFromSessionId(origLegCallId, appSession,
						(String) peerLegData
								.get(LegDataAttributes.P_SESSION_ID));

		// To fix "Dialog Terminated" exception observed in production
		if (peerLegSipSession == null
				|| peerLegSipSession.getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Peer SIP session state is terminated, can not send REFER");
			}
			return;
		}

		if (referReqReecived != null) {

			SipServletRequest peerReferRequest = SipProtocolMessageCreator
					.createRequest(origLegCallId, peerLegSipSession,
							PhConstants.REFER_REQUEST, callData);

			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: Add refer-to header in peer refer request");
			}

			Address referToHdr = referReqReecived
					.getAddressHeader(PhConstants.REFER_TO_HEADER);
			// String
			// referToHdr=referReqReecived.getHeader(PhConstants.REFER_TO_HEADER);

			// referToHdr = referToHdr.replaceAll(";", "%3B").replaceAll("=",
			// "%3D").replaceAll("@", "%40");
			String incomingReferUri = ((SipURI) referToHdr.getURI()).toString();

			if (incomingReferUri.indexOf(PhConstants.REPLACES
					+ PhConstants.EQUALS_STR) != -1) {

				String dialogId = dialogManager
						.getDialogId((AseSipSession) peerReferRequest
								.getSession());

				String peerReferToHdr = SipProtocolUtil
						.getPeerReplacesAttribute(origLegCallId,
								incomingReferUri, dialogId);

				peerReferRequest.addHeader(PhConstants.REFER_TO_HEADER,
						peerReferToHdr);
			} else {
				peerReferRequest.addAddressHeader(PhConstants.REFER_TO_HEADER,
						referToHdr, true);
			}

			if (referReqReecived.getHeader(PhConstants.REFERRED_BY_HEADER) != null) {

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ ":: Add Referred-By header in peer refer request");
				}
				peerReferRequest.addHeader(PhConstants.REFERRED_BY_HEADER,
						referReqReecived
								.getHeader(PhConstants.REFERRED_BY_HEADER));
			} else {

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId
							+ "::  Referred-By header not found");
				}
			}

			String confid = referReqReecived
					.getHeader(PhConstants.X_CONF_HEADER);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] X-Conference-Id header: " + confid);
			}

			if (confid != null) {
				peerReferRequest.addHeader(PhConstants.X_CONF_HEADER, confid);
			}

			peerReferRequest.send();
		} else {
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId
						+ ":: No Peer Refer request available");
			}
		}

	}

	/**
	 * This method is used to decline transfer
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void rejectCallTransfer(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside rejectCallTransfer() ");
		}

		if (PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.NP_IS_CONSULTED_TRANSFER))) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: It is consulted transfer so need to reject pending transfered invite request with cause code");
			}

			SipSession transferedSession = (SipSession) callData
					.get(CallDataAttribute.NP_TRANSFERED_SESSION);

			if (transferedSession != null) {

				CallData transferedCallData = SipProtocolUtil
						.getCallData(transferedSession.getApplicationSession());
				LegData origLegData = (LegData) transferedCallData
						.get(CallDataAttribute.P_LEG1);
				origLegData.set(LegDataAttributes.P_CAUSE_CODE, 603);

				dropCall(transferedSession.getApplicationSession());
			}
			return;

		}
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		// Connection type from where response received
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);

		SipServletRequest pendingRefer = (SipServletRequest) legData
				.get(LegDataAttributes.P_PENDING_REFER);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Reject the REFER  received with call id = "
					+ pendingRefer.getCallId() + " for " + connType
					+ " at state " + state);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Send 603 Declined for REFER received");
		}
		// SipServletResponse sipResponse = pendingRefer
		// .createResponse(SipServletResponse.SC_DECLINE);

		SipServletResponse sipResponse = SipProtocolMessageCreator
				.createResponse(origLegCallId, pendingRefer,
						SipServletResponse.SC_DECLINE, callData);
		sipResponse.send();

		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();
		Event event = new Event(EventType.EVENT_TRANSFER_REJECTED,
				Protocol.SIP, action.getLeg());

		ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

	}

	/**
	 * This method is used to send refer success response from transfree to
	 * transferor
	 * 
	 * @param sipResponse
	 * @param callData
	 * @param legData
	 * @throws IOException
	 */
	public static void handleReferAccepted(SipServletResponse sipResponse,
			CallData callData, LegData legData) throws IOException {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleReferAccepted() ");
		}
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		// Connection type from where response received
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		LegData peerLegData = SipProtocolUtil
				.getPeerLegData(callData, connType);

		// String
		// referTokey=(String)peerLegData.getNonpersistableData(LegDataAttributes.NP_TRANSFER_KEY);
		//
		// if(referTokey!=null){
		//
		// if (logger.isDebugEnabled()) {
		// logger.debug(origLegCallId
		// + ":: Remove Transfer key from attributes "+referTokey);
		// }
		// peerLegData.removeNonpersistableData(LegDataAttributes.NP_TRANSFER_KEY);
		// }else{
		//
		// if (logger.isDebugEnabled()) {
		// logger.debug(origLegCallId
		// +
		// ":: No Transfer key found in peer legdata can not remove from attributes");
		// }
		// }
		SipServletRequest pendingRefer = (SipServletRequest) peerLegData
				.get(LegDataAttributes.P_PENDING_REFER);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: REFER was received with call id = "
					+ pendingRefer.getCallId() + " from " + connType
					+ " at state " + state);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Send 202 Accepted for REFER received");
		}
		// SipServletResponse peersipResponse = pendingRefer
		// .createResponse(SipServletResponse.SC_ACCEPTED);

		SipServletResponse peersipResponse = SipProtocolMessageCreator
				.createResponse(origLegCallId, pendingRefer,
						SipServletResponse.SC_ACCEPTED, callData);
		peersipResponse.send();

		SipProtocolUtil.startTimer(sipResponse.getApplicationSession(), 60000,
				true, PhConstants.CALL_TRANSFER_TIMER);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Forward it transparently to other leg");
		}

	}

	/**
	 * This method is used to handle error response of refer to transfree and
	 * forwarding it to transferor
	 * 
	 * @param sipResponse
	 * @param callData
	 * @param legData
	 * @throws IOException
	 */
	public static void handleReferRejected(SipServletResponse sipResponse,
			CallData callData, LegData legData, Map m_ReferDialogMap)
			throws IOException {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleReferRejected() ");
		}
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		// Connection type from where response received
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);
		LegData peerLegData = SipProtocolUtil
				.getPeerLegData(callData, connType);

		String referTokey = (String) peerLegData
				.get(LegDataAttributes.NP_TRANSFER_KEY);

		if (referTokey != null) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Remove Transfer key from dialog map "
						+ referTokey);
			}
			peerLegData.remove(LegDataAttributes.NP_TRANSFER_KEY);
			m_ReferDialogMap.remove(referTokey);
		} else {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: No Transfer key found in peer legdata ");
			}
		}

		SipServletRequest pendingRefer = (SipServletRequest) peerLegData
				.get(LegDataAttributes.P_PENDING_REFER);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: REFER was received with call id = "
					+ pendingRefer.getCallId() + " from " + connType
					+ " at state " + state);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Send " + sipResponse.getStatus()
					+ "for REFER received");
		}
		// SipServletResponse peersipResponse = pendingRefer
		// .createResponse(sipResponse.getStatus());

		SipServletResponse peersipResponse = SipProtocolMessageCreator
				.createResponse(origLegCallId, pendingRefer,
						sipResponse.getStatus(), callData);
		peersipResponse.send();

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Forward it transparently to other leg");
		}

	}

	/**
	 * This method is used to handle transfered call invite this invite will
	 * come in diffrent app session so no call data will be here
	 * 
	 * @param transferdRequest
	 * @param appSession
	 * @param callData
	 * @throws Exception
	 */
	public static void handleCallTransferInvite(
			SipServletRequest newtransferdRequest,
			SipApplicationSession appSession, SipSession transferedSession,
			CallData callData) throws Exception {

		/*
		 * This is transfered request service will not have any data for it
		 * because it will come in new application session
		 */
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside handleCallTransferInvite() ");
		}
		if (transferedSession != null) {

			CallData transferedCallData = SipProtocolUtil
					.getCallData(transferedSession.getApplicationSession());

			String oldOrigLegCallId = (String) transferedCallData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			/**
			 * Mark the status of the call as transferedCall for
			 * transferdSession
			 */
			// transferedCallData.setPersistableData(CallDataAttribute.P_TRANSFERED_CALL_IND,
			// PhConstants.TRUE);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Transfered Orig Call id found from transfered call data is "
						+ oldOrigLegCallId);
			}
			callData.set(CallDataAttribute.P_TRANSFERED_CALL_ID,
					oldOrigLegCallId);

			callData.set(CallDataAttribute.P_TRANSFERED_CALL_IND,
					PhConstants.TRUE);

		} else {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Transfered call session and callData is not found, can not process it . dropping call .Application notification is not needed send 603");
			}

			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			if (legData != null) {
				legData.set(LegDataAttributes.P_CAUSE_CODE, 603);
			}

			dropCall(appSession);
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: send transfered call to transfer target  ");
		}

		// Action action = new
		// Action(Action.ActionType.ACTION_TRANSFER_CONNECT);
		// action.setConnectionMode(Action.CONNECTIONMODE.B2BUA);
		// action.setProtocol(Protocol.SIP);
		// action.setLeg(CallDataAttribute.P_LEG2.name());
		//
		// callData.set(CallDataAttribute.P_CURRENT_ACTION, action);

		// LegData termLegData = new LegData();
		// termLegData.set(LegDataAttributes.P_CONNECTION_TYPE,
		// ConnectionType.TERM_CONNECTION);
		// callData.set(CallDataAttribute.P_LEG2, termLegData);

		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();
		Event event = new Event(EventType.EVENT_TRANSFER_INITIATED,
				Protocol.SIP, CallDataAttribute.P_LEG2.name());
		ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

		// will raise trasnfer initiated from here so that applictaion can
		// uupdate binding
		// sendInitialInviteToTerm(appSession, true, callData, action);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Leaving handleCallTransferInvite() ");
		}

		return;
	}

	/**
	 * This method is used to start a timer requested by application
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void startApplicationTimer(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
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
		SipProtocolUtil.startTimer(appSession, action.getTimerTimeInMills(),
				false, timerName);

		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();
		Event appEvent = new Event(EventType.EVENT_APP_TIMER_STARTED,
				Protocol.SIP, action.getLeg());

		ProtocolRouter.getInstance()
				.execute(appEvent, callData, serviceHandler);
	}

	/**
	 * This method is used to stop a timer requested by application
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void stopApplicationTimer(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
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
		SipProtocolUtil.stopTimer(appSession, timerName);

		try {
			ServiceInterface serviceHandler = PhUtilityServices.getInstance(
					(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();
			Event appEvent = new Event(EventType.EVENT_APP_TIMER_STOPPED,
					Protocol.SIP, action.getLeg());

			ProtocolRouter.getInstance().execute(appEvent, callData,
					serviceHandler);
		} catch (Exception ex) {
			logger.error("Error occured while stopping application timer : "
					+ action.getTimerName());
			logger.error("Error occured : " + ex);
			throw ex;
		}

	}

	/**
	 * This method is used to stop currently performed media operation
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws IllegalStateException
	 * @throws MediaServerException
	 */
	public static void stopMediaOperation(SipApplicationSession appSession,
			CallData callData, Action action) throws IllegalStateException,
			MediaServerException {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside stopMediaOperation() ");
		}

		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		State state = (State) origLegData
				.get(LegDataAttributes.P_LEG_SIP_STATE);
		MsSessionController msController = null;
		if (state.equals(State.MS_PLAY_RECORD)) {
			SBBFactory sbbFactory = (SBBFactory) appSession
					.getAttribute(PhConstants.ATTRIB_SBB_FACTORY);
			if (sbbFactory == null) {

				logger.error("[PH] ConferenceHandler:connectParticipant SBBFactory not available in application session.");
				return;
			}
			SipServletRequest request = SipProtocolUtil.getInitialInvite(
					appSession, origLegData);
			msController = (ConferenceParticipant) sbbFactory.getSBB(
					ConferenceParticipant.class.getName(),
					request.getCallId(),
					appSession,
					PhUtilityServices
							.getInstance(
									(String) callData
											.get(CallDataAttribute.SERVICE_ID))
							.getServletContext());
		} else {
			msController = SipProtocolUtil.getMsController(appSession,
					origLegData, CallDataAttribute.P_LEG1.name());

			callData.set(CallDataAttribute.NP_MS_OP_STOP_INPROG,
					PhConstants.TRUE);

			msController.setAttribute(MsSessionController.STOP_ANNOUNCEMENT,
					Boolean.TRUE);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Invoking Stop Media Operation");
		}
		msController.stopMediaOperations();

	}

	/**
	 * This method is used to process next calls queued by application
	 * 
	 * @param appSessionIds
	 * @param callId
	 */
	static void processNextCalls(ArrayList<String> appSessionIds,
			String callId, String serviceId) {
		if (logger.isDebugEnabled()) {
			logger.debug(callId + ":: Inside processNextCalls. AppSessionIds "
					+ appSessionIds);
		}
		for (String appSessionId : appSessionIds) {
			if (appSessionId != null && !appSessionId.isEmpty()) {
				try {
					SipApplicationSession appSession = SipProtocolUtil
							.getAppSession(appSessionId, serviceId);
					try {
						CallData callData = SipProtocolUtil
								.getCallData(appSession);
						if (callData == null) {
							logger.warn(callId
									+ ":: CallData is null for appSessionId "
									+ appSessionId);
							continue;
						}
						processNextCall(callData);
					} catch (Exception ex) {
						logger.error(
								callId
										+ ":: Error in processing dequeued call with appSessionId "
										+ appSessionId, ex);
						SipProtocolHelper.dropCall(appSession);
					}
				} catch (Exception ex) {
					logger.error(callId
							+ ":: Error in getting application session for id "
							+ appSessionId, ex);
				}
			}
		}
	}

	/**
	 * This method process the next call retrieved from queue by service.
	 * 
	 * @param callData
	 *            represents the instance of CallData
	 * @throws IllegalStateException
	 * @throws MediaServerException
	 */
	private static void processNextCall(CallData callData) {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside processNextCall()");
			logger.debug(origLegCallId
					+ ":: Start 10ms second timer to put this call in worker thread");
		}

		SipApplicationSession appSession = SipProtocolUtil.getAppSession(
				(String) callData.get(CallDataAttribute.P_APP_SESSION_ID),
				(String) callData.get(CallDataAttribute.SERVICE_ID));

		// This timer should be non-persistable to avoid data replication
		SipProtocolUtil.startTimer(appSession, 10, false,
				PhConstants.NEXT_CALL_TIMER);
	}

	/**
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @param status
	 * @param legData
	 * @param lastAction
	 * @param appSession
	 * @return
	 * @throws Exception
	 */
	protected static Event tryNextContactFromList(String origLegCallId,
			CallData callData, int status, LegData legData, Action lastAction,
			SipApplicationSession appSession, SipSession termSession,
			boolean invalidate) throws Exception {

		Event event = null;

		Action lastLegAction = (Action) legData
				.get(LegDataAttributes.P_LEG_CURRENT_ACTION);

		if (lastLegAction != null
				&& lastLegAction.getActionType() == Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: last term leg action was  ACTION_TRY_REDIRECT_CONTACTS ");
			}
			lastAction = lastLegAction;
		}

		if (lastAction.getActionType() == Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: last action was  ACTION_TRY_REDIRECT_CONTACTS . Lets check if another redirect contact available for trying ");
			}

			Action.EQS_TYPE routeType = null;

			if (legData.get(LegDataAttributes.NP_EQS_ERR_RESPONSE_ROUING_MAP) != null) {

				@SuppressWarnings("unchecked")
				Map<Integer, Action.EQS_TYPE> errorCodeMap = (Map<Integer, EQS_TYPE>) legData
						.get(LegDataAttributes.NP_EQS_ERR_RESPONSE_ROUING_MAP);

				routeType = errorCodeMap.get(Integer.valueOf(status));

				if (routeType == null) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: The route type not found for the error code "
								+ status
								+ " Notifying application of EVENT_ROUTE_TERMINATE ");
					}
					event = new Event(Event.EventType.EVENT_ROUTE_TERMINATE,
							Protocol.SIP, lastAction.getLeg());
					return event;
				}

				switch (routeType) {

				case TRY_NEXT_CONTACT:

					@SuppressWarnings("unchecked")
					List<MultiChoiceContact> mccList = (List<MultiChoiceContact>) callData
							.get(CallDataAttribute.P_MC_CONTACTS_LIST);

					@SuppressWarnings("unchecked")
					List<TermRedirectionContact> redirectContactList = (List<TermRedirectionContact>) callData
							.get(CallDataAttribute.NP_REDIRECT_ADDRESS_LIST);

					if ((redirectContactList != null && redirectContactList
							.size() > 0)
							|| (mccList != null && mccList.size() > 0)) {

						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: try next available contact from redirect contacts list !!! TRY_NEXT_CONTACT");
						}

						/**
						 * invalidate this failure session so that it do'not
						 * exists at cleanup time this is done when called from
						 * doErrorResponse not from timer timeot
						 */

						if (invalidate) {
							termSession.invalidate();
						}

						if (!invalidate) {
							legData.remove(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND);
						}

						SipProtocolHelper.connectTerm(appSession, callData,
								lastAction);

					} else {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: No other redirect contact available so not trying further");
						}
						event = new Event(Event.EventType.EVENT_FAILURE,
								Protocol.SIP, lastAction.getLeg());
					}
					break;
				case TRY_NEXT_ROUTE:
					// termSession.invalidate();
					event = new Event(Event.EventType.EVENT_ROUTE_NEXT,
							Protocol.SIP, lastAction.getLeg());
					break;
				case TERMINATE:
					event = new Event(Event.EventType.EVENT_ROUTE_TERMINATE,
							Protocol.SIP, lastAction.getLeg());
					break;
				}
			} else {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Routing error map not available, check if next contact is available or not otherwise raise event failure!!!");
				}

				@SuppressWarnings("unchecked")
				List<MultiChoiceContact> mccList = (List<MultiChoiceContact>) callData
						.get(CallDataAttribute.P_MC_CONTACTS_LIST);

				@SuppressWarnings("unchecked")
				List<TermRedirectionContact> redirectContactList = (List<TermRedirectionContact>) callData
						.get(CallDataAttribute.NP_REDIRECT_ADDRESS_LIST);

				if (mccList != null && mccList.size() > 0
						|| redirectContactList != null
						&& redirectContactList.size() > 0) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: try next available contact from redirect contacts list  routing error map not available!!!");
					}

					/**
					 * invalidate this failure session so that it do'not exists
					 * at cleanup time this is done when called from
					 * doErrorResponse not from timer timeot
					 */
					if (invalidate) {
						termSession.invalidate();
					}

					if (!invalidate) {
						legData.remove(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND);
					}

					SipProtocolHelper.connectTerm(appSession, callData,
							lastAction);

				} else {

					/**
					 * code will reach here if 302 was received instead of 300MC
					 */
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: No other redirect contact  available so not trying further");
					}

					event = new Event(Event.EventType.EVENT_FAILURE,
							Protocol.SIP, lastAction.getLeg());
				}
			}
		}

		if (event != null
				&& (event.getEventType() == Event.EventType.EVENT_FAILURE || event
						.getEventType() == Event.EventType.EVENT_ROUTE_NEXT)) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Remove Available MCC list and redirection address list from calldata");
			}
			callData.remove(CallDataAttribute.P_MC_CONTACTS_LIST);

			callData.remove(CallDataAttribute.NP_REDIRECT_ADDRESS_LIST);
		}
		return event;
	}

	/**
	 * This method is used to end service here
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void serviceComplete(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug(" Call serviceComplete on app chain manager ");
		}

		String serviceId = action.getApplicationName();

		/*
		 * if service id not provided by applictaion then use current service id
		 * ,application should set this because if applictaion calls service
		 * comeplete after invokeservice chaining then current service id in
		 * calldata will change to next service id , so applictaion should
		 * specify service id on calling service complete
		 */
		if (serviceId == null) {
			serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		}
		PhUtilityServices
				.getInstance(serviceId)
				.getAppChainManager()
				.serviceComplete(serviceId, action.isNotifyPrevService(),
						action.getEvent(), appSession, null);

	}

	/**
	 * This method is used to invoke service chanining
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static boolean invokeServiceChaining(
			SipApplicationSession appSession, CallData callData, Action action)
			throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " invokeServiceChaining Entering ...");
		}

		if (!action.isInvokeServiceChaining()) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "No need to invoke chaining as flag is not set by application..so connecting to term");
			}
			return false;
		}

		String currentSvcId = (String) callData
				.get(CallDataAttribute.SERVICE_ID);

		String prevSvcId = (String) callData
				.get(CallDataAttribute.PREV_SERVICE_ID);

		String origInfo = callData.get(CallDataAttribute.P_ORIGIN_IP) + "|"
				+ callData.get(CallDataAttribute.P_ORIGIN_PORT);

		Event event = new Event(EventType.EVENT_INITIAL, Protocol.SIP,
				CallDataAttribute.P_LEG1.name());

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " current service id is  "
					+ currentSvcId + " Prev svc id " + prevSvcId);
		}


		AseAppChainManager acm=PhUtilityServices.getInstance(currentSvcId).getAppChainManager();

		if(leg2Data==null) {
			logger.debug("creating new leg2Data");
			leg2Data =new LegData();
		}
		
		if(leg1Data == null) {
			logger.debug("creating new leg1Data");
			leg1Data =new LegData();
		}


		Map<String, Map<CallChainedAttributes, Object>> chainingMap = (Map<String, Map<CallChainedAttributes, Object>>) callData
				.get(CallDataAttribute.P_SVC_CHAINING_MAP);

		Object modCallingNum = null;
		Object modCalledNum = null;

		Object modOrigInfo = null;

		if (chainingMap != null && chainingMap.get(currentSvcId) != null) {

			Map<CallChainedAttributes, Object> chainedAttributes = (Map<CallChainedAttributes, Object>) chainingMap
					.get(currentSvcId);
			modCallingNum = chainedAttributes
					.get(CallChainedAttributes.MODIFIED_CALLING_NUM);
			modCalledNum = chainedAttributes
					.get(CallChainedAttributes.MODIFIED_CALLED_NUM);

			if (chainedAttributes
					.get(CallChainedAttributes.MODIFIED_ORIGIN_INFO) != null) {
				origInfo = (String) chainedAttributes
						.get(CallChainedAttributes.MODIFIED_ORIGIN_INFO);
			}

			if (chainedAttributes.get(CallChainedAttributes.CHAIN_START_TIME) == null) {
				chainedAttributes.put(CallChainedAttributes.CHAIN_START_TIME,
						callData.get(CallDataAttribute.P_CALL_START_TIME));
			}
			chainedAttributes.put(CallChainedAttributes.END_TIME, new Date());

		} else {
			modCallingNum = leg1Data.get(LegDataAttributes.P_CALLING_PARTY);
			modCalledNum = leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);
		}
		
		if(modCalledNum==null){
			modCalledNum=(PhoneNumber)leg1Data.get(LegDataAttributes.P_CALLED_PARTY);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId +" create AddressesMap with calling   "+ modCallingNum +" and called as "+ modCalledNum);
		}

		@SuppressWarnings("unchecked")
		Map<String, Object> addressesMap = (Map<String, Object>) callData
				.get(CallDataAttribute.ADDRESS_MAP);

		if (addressesMap == null) {
			addressesMap = new HashMap<String, Object>();
		}

		addressesMap.put(AseAppChainManager.CALLING_NUM, modCallingNum);
		addressesMap.put(AseAppChainManager.MODIFIED_DIALLED_NUMBER,
				modCalledNum);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " AddressesMap created is  "
					+ addressesMap);
		}
		Protocol protocol = (Protocol) callData
				.get(CallDataAttribute.P_PROTOCOL);
		String nextServiceId = acm.getNextInterestedService(origLegCallId,
				currentSvcId, prevSvcId, addressesMap, event, origInfo,
				protocol);

		if (nextServiceId != null) {

			/**
			 * Creating chaining data for next service and updating it in
			 * chaining map
			 */

			/**
			 * Reset N/W transactions
			 */

			SipProtocolUtil.resetNetworkTransactions(callData);

			Map<CallChainedAttributes, Object> nexChainedMap = new HashMap<CallChainedAttributes, Object>();

			nexChainedMap.put(CallChainedAttributes.MODIFIED_CALLING_NUM,
					modCallingNum);
			nexChainedMap.put(CallChainedAttributes.MODIFIED_CALLED_NUM,
					modCalledNum);
			nexChainedMap.put(CallChainedAttributes.CHAIN_START_TIME,
					new Date());
			nexChainedMap.put(CallChainedAttributes.MODIFIED_ORIGIN_INFO,
					origInfo);

			if (chainingMap == null) {
				chainingMap = new HashMap<String, Map<CallChainedAttributes, Object>>();
				callData.set(CallDataAttribute.P_SVC_CHAINING_MAP, chainingMap);
			}

			chainingMap.put(nextServiceId, nexChainedMap);

			// leg1Data.set(LegDataAttributes.P_CALLED_PARTY,leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER));

			if (logger.isDebugEnabled()) {
				logger.debug(" next service found is " + nextServiceId
						+ " update serviceid in call data and changed map "
						+ chainingMap);
			}

			callData.set(CallDataAttribute.SERVICE_ID, nextServiceId);
			callData.set(CallDataAttribute.PREV_SERVICE_ID, currentSvcId);

			// reset Final CDR flag so that next service can write CDR
			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN,
					PhConstants.FALSE);

			appSession.setAttribute(PhConstants.SERVICE_ID, nextServiceId);

			if (logger.isDebugEnabled()) {
				logger.debug(" call invokeServiceChaining on app chain manager  !!! ");
			}

			acm.invokeServiceChaining(currentSvcId, nextServiceId,
					addressesMap, event, null, appSession,
					action.isRemainInPath());// action.getEvent()

			if (logger.isDebugEnabled()) {
				logger.debug(" return true that service chaining is invoked next service is !!! "
						+ nextServiceId);
			}

			// Event event = new Event(EventType.EVENT_SERVICE_CHAIN_INVOKED,
			// Protocol.SIP, null);
			// ProtocolRouter.getInstance().execute(event, callData,
			// serviceHandler);

			MeasurementCounter measurementCounter = PhMeasurementService
					.getInstance().getMeasurementCounter(Protocol.SIP);
			if (measurementCounter != null) {
				measurementCounter.incrementServiceTriggeredCount(
						nextServiceId, true);
			}

			return true;

		} else {

			if (logger.isDebugEnabled()) {
				logger.debug("No next service is available for current service  "
						+ currentSvcId);
			}

			if (action.isDropCallOnNoNextSvc()) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ "Drop call as service want to drop call here");
				}

				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.REJECT_NO_NEXT_SERVICE);
				/*
				 * Do not mark sip session as ready to invalidate so that
				 * cleanupSipLeg() can give an attempt to cleanup this leg.
				 */
				SipProtocolHelper.dropCall(appSession);
				return true;
			}

			// Event event = new Event(EventType.EVENT_NO_NEXT_SERVICE,
			// Protocol.SIP, null);
			// ProtocolRouter.getInstance().execute(event, callData,
			// acm.getServiceInterface(currentSvcId));
		}

		return false;
	}

	public static void initiateDtmfCallTransfer(
			SipApplicationSession appSession, CallData callData, Action action) {
		// TODO Auto-generated method stub

	}

	/**
	 * This method is used to drop a particular leg
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void disconnectLeg(SipApplicationSession appSession,
			CallData callData, Action action) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: disconnectLeg..."
					+ action.getLeg());
		}
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		SipSession sipSession = SipProtocolUtil.getSipSessionFromSessionId(
				origLegCallId, appSession,
				(String) legData.get(LegDataAttributes.P_SESSION_ID));

		disconnectSession(origLegCallId, sipSession, legData, action, callData);
	}

	/**
	 * This method is used to disconnect linked session will currently used for
	 * call pickup kind of scenario
	 * 
	 * @param sipSession
	 * @param action
	 */
	private static void disconnectLinkedSession(SipSession sipSession,
			Action action) {

		if (logger.isDebugEnabled()) {
			logger.debug(":: disconnectLinkedSession..." + action.getLeg());
		}

		CallData callData = SipProtocolUtil.getCallData(sipSession
				.getApplicationSession());
		LegData legData = SipProtocolUtil.getLegDataForSipSession(
				sipSession.getApplicationSession(), sipSession, callData);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(":: Going to disconnect linked session..."
					+ sipSession.getId());
		}
		disconnectSession(origLegCallId, sipSession, legData, action, callData);
	}

	/**
	 * This method is used to disconnect a particular sip session leg
	 * 
	 * @param origLegCallId
	 * @param sipSession
	 * @param legData
	 * @param action
	 * @param callData
	 */
	private static void disconnectSession(String origLegCallId,
			SipSession sipSession, LegData legData, Action action,
			CallData callData) {

		if (logger.isDebugEnabled()) {
			logger.debug(":: Disconnect sip session..." + sipSession.getId());
		}
		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();

		SipServletRequest initInviteRequest = (SipServletRequest) sipSession
				.getAttribute(PhConstants.ORIG_REQUEST);

		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);

		try {

			switch (connType) {
			case ORIG_CONNECTION: {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Disconnect orig sip leg");
				}
				// Set the handler on sip session
				sipSession.setHandler(serviceHandler.getServletName());
				/*
				 * sendPendingRequests() is added to fix for the error scanrios
				 * like - if flexible charging is applied, then while sending
				 * 183+CHG to orig PH creates 200 OK for orig. But if PRACK is
				 * not received from orig and PRACK timeout happens, then PH is
				 * not able to send error response to orig as final response has
				 * already been created on this sip session. - If SDP change
				 * Re-INVITE is received from Leg A then PH creates 200 OK for
				 * leg A and stores that in Leg A sip session. After this PH
				 * sends Re-INVITE to B leg. B leg does not answer this INVITE.
				 * Then 408 is generated for leg B and it goes into terminated
				 * state. But because 200 OK has already been created and stored
				 * for leg A , so PH is direcly sending BYE to leg A, without
				 * any final response to leg A.
				 */
				if (!PhConstants.TRUE.equals(legData
						.get(LegDataAttributes.P_IS_CANCEL_RECEIVED))) {
					sendPendingRequests(callData, sipSession, legData);
				}
				switch (sipSession.getState()) {
				case INITIAL:
				case EARLY: {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Session state is EARLY or INITIAL");
						logger.debug(origLegCallId
								+ ":: Send Error response to orig leg");
					}
					legData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.TERMINATED);
					SipServletResponse errorResponse = null;
					/*
					 * SBTM-UAT-555: If IS_CANCEL_RECEIVED attribute is set on
					 * orig SIP Session, then
					 * SipInapIsupHandlerServlet.cleanupSipSession() will send
					 * 487 for INVITE on orig leg
					 */
					if (PhConstants.TRUE.equals(legData
							.get(LegDataAttributes.P_IS_CANCEL_RECEIVED))) {
						// errorResponse = initInviteRequest
						// .createResponse(487);
						errorResponse = SipProtocolMessageCreator
								.createResponse(
										origLegCallId,
										initInviteRequest,
										SipServletResponse.SC_REQUEST_TERMINATED,
										callData);
					} else {
						errorResponse = SipProtocolMessageCreator
								.createErrorResponse(initInviteRequest, action);
					}
					errorResponse.send();

					sipSession.setAttribute(
							PhConstants.SESSION_READY_TO_INVALIDATE,
							PhConstants.TRUE); // LEV-3764
					break;
				}
				case CONFIRMED: {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Session state is CONFIRMED");
						logger.debug(origLegCallId + ":: Send BYE request");
					}

					SipServletRequest pendingAck = (SipServletRequest) legData
							.get(LegDataAttributes.P_PENDING_ACK);
					if (pendingAck != null) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: ACK is pending so send it first");
							
						}
						legData.remove(LegDataAttributes.P_PENDING_ACK);
						pendingAck.send();
					}

					legData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.TERMINATED);

					/*
					 * The 31 should be used as cause value in REL if ANM has
					 * already been sent [SBTM-UAT-192]. The only exception to
					 * this case is that REL received from party B would be sent
					 * transparently to party A.
					 */
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Change cause value to 31 as ANM has already been sent");
					}
					SipProtocolMessageCreator.createByeRequest(origLegCallId,
							sipSession, PhConstants.BYE_CV_ANM_ALREADY_SENT,
							action.isAddCustomHeader()).send();
					break;
				}
				case TERMINATED: {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Session state is TERMINATED, do nothing");
					}
					legData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.TERMINATED);
					break;
				}
				}
				break;
			}
			case TERM_CONNECTION: {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Disconnect term sip leg");
				}
				// Set the handler on sip session
				sipSession.setHandler(serviceHandler.getServletName());
				/*
				 * sendPendingRequests() is added to fix for the error scanrios
				 * like - If SDP change Re-INVITE is received from Leg A then PH
				 * creates 200 OK for leg A and stores that in Leg A sip
				 * session. After this PH sends Re-INVITE to B leg. B leg does
				 * not answer this INVITE. Then 408 is generated for leg B and
				 * it goes into terminated state. But because 200 OK has already
				 * been created and stored for leg A , so PH is direcly sending
				 * BYE to leg A, without any final response to leg A.
				 */
				sendPendingRequests(callData, sipSession, legData);

				/*
				 * Send Cancel/Bye on term leg only if Cancel has already not
				 * been sent on term leg. This is to take care of the following
				 * use case: When No-Answer timer times-out, PH sends Cancel/Bye
				 * to B-Party and informs service. If service returns drop_call,
				 * then PH iterates through all sip sessions. But for term sip
				 * session, 487 for INVITE has not received till now. So that
				 * sip session is in INITIAL state. So this method again tries
				 * to send cancel
				 */
				String cancelOrByeSent = (String) legData
						.get(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND);
				switch (sipSession.getState()) {
				case INITIAL:
				case EARLY: {
					/*
					 * Check if the no answer timeout was not received for term
					 * and also no error response was received from term (cuase
					 * code will be null)
					 */
					if (!PhConstants.TRUE.equals(cancelOrByeSent)) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Session state is EARLY or INITIAL");
							logger.debug(origLegCallId
									+ ":: Send Cancel request to term leg");
						}
						// Old PH was setting TERMINATION_IN_PROGRESS state
						// here
						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.TERMINATED);
						// legData.setPersistableData(LegDataAttributes.P_NO_ANSWER_TIMEOUT_IND,PhConstants.TRUE);
						SipProtocolMessageCreator.createCancelRequest(
								origLegCallId, initInviteRequest,
								PhConstants.NO_ANS_TIMEOUT_CV_TERM_LEG).send();
					}
					break;
				}
				case CONFIRMED: {
					if (!PhConstants.TRUE.equals(cancelOrByeSent)) {
						if (logger.isDebugEnabled()) {
							logger.debug(origLegCallId
									+ ":: Session state is CONFIRMED");
							logger.debug(origLegCallId + ":: Send BYE request");
						}
						SipServletRequest pendingAck = (SipServletRequest) legData
								.get(LegDataAttributes.P_PENDING_ACK);
						if (pendingAck != null) {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegCallId
										+ ":: ACK is pending so send it first");
							}
							pendingAck.send();
						}

						// Old PH was setting TERMINATION_IN_PROGRESS state
						// here
						legData.set(LegDataAttributes.P_LEG_SIP_STATE,
								State.TERMINATED);
						SipProtocolMessageCreator.createByeRequest(
								origLegCallId, sipSession,
								PhConstants.BYE_CV_ANM_ALREADY_SENT,
								action.isAddCustomHeader()).send();
					}
					break;
				}
				case TERMINATED: {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Session state is TERMINATED, do nothing");
					}
					legData.set(LegDataAttributes.P_LEG_SIP_STATE,
							State.TERMINATED);
					break;
				}
				}
				break;
			}
			default: {
				logger.warn(origLegCallId + ":: Unrecognized connection type "
						+ connType + ", do nothing");
			}

			}
		} catch (Exception ex) {
			logger.warn(origLegCallId + ":: Error in releasing sip leg is "
					+ ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(origLegCallId + ":: Error in releasing sip leg", ex);
			}
			/*
			 * SBTM-UAT-1583 Set ready to invalidate for this leg if exception
			 * comes while cleaning it so that appSession can be invalidated.
			 */
			sipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE,
					PhConstants.TRUE);
		}

	}

	/**
	 * This method is used to resynch call
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws IOException
	 */
	public static void resynchLegs(CallData callData, Action action)
			throws IOException {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: resynchLegs ...");
		}

		String resynchLegId1 = action.getLeg();
		String resynchLegId2 = (String) callData
				.get(CallDataAttribute.RESYNCH_LEG_ID);

		LegData legData1 = (LegData) callData.get(CallDataAttribute
				.valueOf(resynchLegId1));
		String appSessionId1 = (String) legData1
				.get(LegDataAttributes.APP_SESSION_ID);

		SipApplicationSession appSessionLeg1 = SipProtocolUtil.getAppSession(
				appSessionId1,
				(String) callData.get(CallDataAttribute.SERVICE_ID));

		LegData legData2 = (LegData) callData.get(CallDataAttribute
				.valueOf(resynchLegId2));
		String appSessionId2 = (String) legData2
				.get(LegDataAttributes.APP_SESSION_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: AppSession1 and AppSession1 ids are  ..."
					+ appSessionId1 + " " + appSessionId2);
		}

		SipApplicationSession appSessionLeg2 = SipProtocolUtil.getAppSession(
				appSessionId2,
				(String) callData.get(CallDataAttribute.SERVICE_ID));

		// appSessionLeg1.setAttribute(CallData.CALL_DATA, callData);
		// appSessionLeg2.setAttribute(CallData.CALL_DATA, callData);

		// SipSession sipSession1 = SipProtocolUtil
		// .getSipSessionFromSessionId(
		// origLegCallId,
		// appSessionLeg1,
		// (String) legData1
		// .get(LegDataAttributes.P_SESSION_ID));
		//
		// SipSession sipSession2 = SipProtocolUtil
		// .getSipSessionFromSessionId(
		// origLegCallId,
		// appSessionLeg2,
		// (String) legData2
		// .get(LegDataAttributes.P_SESSION_ID));

		State termLegState = (State) legData2
				.get(LegDataAttributes.P_LEG_SIP_STATE);

		State origLegState = (State) legData1
				.get(LegDataAttributes.P_LEG_SIP_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Leg1 state is  ..." + origLegState
					+ " leg2 state is " + termLegState);
		}

		if (termLegState == State.INIT) {

			if (origLegState == State.CONN_IN_PROGRESS) {

				legData1.set(LegDataAttributes.P_LEG_CURRENT_ACTION, action);

				legData2.set(LegDataAttributes.P_LEG_CURRENT_ACTION, action);

				MultipartBody mpb = (MultipartBody) legData2
						.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

				SipServletRequest ssr1 = SipProtocolUtil.getInitialInvite(
						appSessionLeg1, legData1);

				SipServletResponse ssrs1 = ssr1
						.createResponse(SipServletResponse.SC_OK);
				ssrs1.setContent(mpb.getContent(), mpb.getContentType());
				ssrs1.send();

				legData1.set(LegDataAttributes.P_LEG_SIP_STATE, State.CONNECTED);

				MultipartBody mpb1 = (MultipartBody) legData1
						.get(LegDataAttributes.P_CURRENT_RECEIVED_SDP);

				SipServletRequest ssr2 = SipProtocolUtil.getInitialInvite(
						appSessionLeg2, legData2);
				SipServletResponse ssrs2 = ssr2
						.createResponse(SipServletResponse.SC_OK);
				ssrs2.setContent(mpb1.getContent(), mpb1.getContentType());
				ssrs2.send();

				legData2.set(LegDataAttributes.P_LEG_SIP_STATE, State.CONNECTED);

				/**
				 * link sip sessions so that they can be disconnected on call
				 * drop from any of parties
				 */
				ssr1.getSession().setAttribute(
						PhConstants.CALLPICKUP_LINKED_REQUEST, ssr2);

				ssr2.getSession().setAttribute(
						PhConstants.CALLPICKUP_LINKED_REQUEST, ssr1);

			}

		} else {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: No valid state for  resycnh currently  ...");
			}

		}
		// try {
		// SipServletRequest sipRequest=sipSession2.createRequest("INVITE");
		// //
		// request1.getApplicationSession().setAttribute(Constants.ATTRIB_RESYNCH_PARTY_2,sipRequest);
		// sipRequest.send();
		// } catch (IOException e) {
		// logger.error(" could not craete invite request on second resycnh session");
		// e.printStackTrace();
		// }

	}

	/**
	 * This method is used to connectMultipleDestinations either serially or
	 * paralley
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws ServletParseException
	 * @throws TooManyHopsException
	 */
	public static void connectMultipleDestinations(
			SipApplicationSession appSession, CallData callData, Action action,
			boolean isParallel) throws ServletParseException,
			TooManyHopsException {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: connectMultipleDestinations, in parallel ? "
					+ isParallel);
		}

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		List<Destination> destinations = (List<Destination>) legData
				.get(LegDataAttributes.DESTINATION_LIST);

		SipFactory sipFactory = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getSipFactory();

		ArrayList<URI> uris = new ArrayList<URI>();
		for (Destination a : destinations) {

			String uri = "sip:" + a.getUsername() + "@" + a.getHost() + ":"
					+ a.getPort();

			uris.add(sipFactory.createURI(uri));
		}
		SipServletRequest sipRequest = SipProtocolUtil.getOrigInitialInvite(
				appSession, callData);
		Proxy proxy = sipRequest.getProxy();

		proxy.setSequentialSearchTimeout(30);
		proxy.setProxyTimeout(30);
		proxy.setRecordRoute(false);
		proxy.setParallel(isParallel);
		proxy.setRecurse(true);
		proxy.setStateful(true);
		proxy.setSupervised(false);
		proxy.proxyTo(uris);
		if (logger.isDebugEnabled()) {
			logger.debug("connectMultipleDestinations Called proxyTo with:"
					+ uris);
		}

	}

	/**
	 * This method is used to resend invite request
	 * 
	 * @param responseCode
	 * @param callData
	 * @param response
	 * @return
	 */
	public static boolean isResendInvite(CallData callData,
			SipServletResponse response) {

		if (logger.isDebugEnabled()) {
			logger.debug(" Entering isResendInvite() ");
		}

		int responseCode = response.getStatus();

		if (responseCode == SipServletResponse.SC_UNAUTHORIZED
				|| responseCode == SipServletResponse.SC_PROXY_AUTHENTICATION_REQUIRED) {
			if (logger.isDebugEnabled()) {
				logger.debug(responseCode
						+ " Response received try invite again");
			}
			AuthInfo authInfo = PhUtilityServices
					.getInstance(
							(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getSipFactory().createAuthInfo();
			SipServletRequest previousRequest = response.getRequest();
			// SipServletRequest origInvite =
			// SIPHelper.getPeerRequest(previousRequest);
			// if (logger.isDebugEnabled()) {
			// logger.debug("[PH] Request from " + origInvite.getFrom());
			// }
			String pwd = (String) callData.get(CallDataAttribute.AUTH_PASSWORD); // DIDPassword);
			SipURI from = (SipURI) previousRequest.getFrom().getURI();
			String userName = from.getUser();
			if (pwd != null && userName != null) {
				authInfo.addAuthInfo(responseCode, "asterisk", userName, pwd);
			} else {
				authInfo.addAuthInfo(responseCode, "asterisk", "dummyuser",
						"dummypass");
			}
			SipProtocolHelper.resendPeerInvite(callData, response, authInfo);
			return true;

		}
		if (responseCode == SipServletResponse.SC_SESSION_INTERVAL_TOO_SMALL) {
			SipProtocolHelper.resendPeerInvite(callData, response, null);
			return true;
		}
		if (logger.isDebugEnabled()) {
			logger.debug(" leaving  isResendInvite() with false ");
		}
		return false;
	}

	/**
	 * resendPeerInvite is used to resend the peer invite
	 * 
	 * @param callData
	 * @param response
	 * @param authInfo
	 */
	private static void resendPeerInvite(CallData callData,
			SipServletResponse response, AuthInfo authInfo) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] reSendPeerInvite(): enter");
		}
		if (response != null) {
			SipServletRequest previousRequest = response.getRequest();
			SipApplicationSession appSession = response.getApplicationSession();
			SipServletRequest origInvite = SipProtocolUtil
					.getOrigInitialInvite(appSession, callData);
			SipServletRequest peerRequest = null;
			try {
				if (authInfo != null) {
					peerRequest = response.getSession().createRequest(
							response.getMethod());
					peerRequest.addAuthHeader(response, authInfo);
				} else {
					peerRequest = response.getSession().createRequest(
							PhConstants.INVITE_REQUEST);
				}
				SipProtocolUtil.copyHeaders(callData, previousRequest,
						peerRequest);
				// if
				// (peerRequest.getSession().getAttribute(PhConstants.WEB_USER)
				// != null) {
				// if (logger.isDebugEnabled()) {
				// logger.debug("[PH] Adding X-App header for passing to Spidyr");
				// }
				// peerRequest.addHeader(PhConstants.X_APP_HEADER, "Spidyr");
				// }
				String deltaSec = response.getHeader(PhConstants.MIN_SE_HEADER);
				if (deltaSec != null) {
					peerRequest.setHeader(PhConstants.MIN_SE_HEADER, deltaSec);
					peerRequest.setHeader(PhConstants.SESSION_EXPIRE_HEADER,
							deltaSec + ";refresher=uas");
				}
				if (previousRequest.getContent() != null) {
					peerRequest.setContent(previousRequest.getContent(),
							previousRequest.getContentType());
				}
				// Ring Parallel handling
				// if (appSession.getAttribute(Constants.PEER_REQUEST_LIST) !=
				// null) {
				// List<SipServletRequest> peerReqList =
				// (List<SipServletRequest>)
				// appSession.getAttribute(Constants.PEER_REQUEST_LIST);
				// boolean isRemoved = peerReqList.remove(previousRequest);
				// if (isRemoved) {
				// peerReqList.add(peerRequest);
				// SIPHelper.associateRequest(origInvite, peerRequest);
				// peerRequest.send();
				// }
				// } else {
				// SIPHelper.associateRequest(origInvite, peerRequest);
				peerRequest.send();
				callData.set(CallDataAttribute.P_TERM_LEG_CALL_ID,
						peerRequest.getCallId());
				// }

			} catch (Exception e) {
				logger.error("[PH] Exception occured in performing Action", e);
				try {
					logger.error("[PH] Exception occured in performing Action so sending 500 Internal Server Error");
					origInvite.createResponse(
							SipServletResponse.SC_SERVER_INTERNAL_ERROR).send();
				} catch (IOException ex) {
					logger.error(
							"[PH] Exception occured in sending 500 error response for Invite:",
							ex);
				}
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] reSendPeerInvite(): exit");
		}

	}

	/**
	 * This method is used to initiate transfer connect
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void initiateTransferConnect(
			SipApplicationSession appSession, CallData callData, Action action)
			throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: initiateTransferConnect");
		}
		sendInitialInviteToTerm(appSession, true, callData, action);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] initiateTransferConnect(): exit");
		}
	}

	/**
	 * initiateCallPickup
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void initiateCallPickup(SipApplicationSession appSession,
			CallData callData, Action action) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: initiateCallPickup...");
		}
		
		
		CallPickupData cpData = (CallPickupData) callData
				.get(CallDataAttribute.CALL_PICKUP_DATA);

		CallData ringingLegData = cpData.getRingingLegCallData();

		action.setLeg(CallDataAttribute.P_LEG2.name());

		String appSessionId = (String) ringingLegData
				.get(CallDataAttribute.APP_SESSION_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Ringing leg app sesison id is..."
					+ appSessionId);
		}

		SipApplicationSession appSessionringingLeg = SipProtocolUtil
				.getAppSession(appSessionId,
						(String) callData.get(CallDataAttribute.SERVICE_ID));

		ringingLegData.set(CallDataAttribute.P_CURRENT_ACTION, action);

		ringingLegData.set(CallDataAttribute.CALL_PICKUP_DATA, cpData);
		SipProtocolUtil.stopTimer(appSessionringingLeg,
				PhConstants.NO_ANSWER_TIMER);

		disconnectLeg(appSessionringingLeg, ringingLegData, action);
	}

	/**
	 * 
	 * @param callData
	 * @throws IOException
	 */
	public static void resynchCallPickupLegs(CallData callData)
			throws IOException {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: resynchCallPickupLegs");
		}

		Action action = new Action(Action.ActionType.ACTION_RESYNCH_LEGS);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		callData.set(CallDataAttribute.RESYNCH_LEG_ID,
				CallDataAttribute.P_LEG2.name());

		CallPickupData cpData = (CallPickupData) callData
				.get(CallDataAttribute.CALL_PICKUP_DATA);

		CallData ringingLegData = cpData.getRingingLegCallData();
		CallData pickupData = cpData.getPickupLegCallData();

		if (ringingLegData != null && pickupData != null) {

			LegData ringingLeg1 = (LegData) ringingLegData
					.get(CallDataAttribute.P_LEG1);
			LegData pickupLeg1 = (LegData) pickupData
					.get(CallDataAttribute.P_LEG1);
			pickupLeg1.set(LegDataAttributes.P_CONNECTION_TYPE,
					ConnectionType.TERM_CONNECTION);

			callData.set(CallDataAttribute.P_LEG1, ringingLeg1);
			callData.set(CallDataAttribute.P_LEG2, pickupLeg1);

			/**
			 * update call legs in call data
			 */

			ringingLegData.set(CallDataAttribute.P_LEG1, ringingLeg1);
			ringingLegData.set(CallDataAttribute.P_LEG2, pickupLeg1);

			pickupData.set(CallDataAttribute.P_LEG1, ringingLeg1);
			pickupData.set(CallDataAttribute.P_LEG2, pickupLeg1);

			SipApplicationSession appSession = SipProtocolUtil
					.getAppSessionForLegId(origLegCallId, pickupLeg1, callData);
			SipSession sipSessionPickupLeg = SipProtocolUtil
					.getSipSessionFromSessionId(origLegCallId, appSession,
							(String) pickupLeg1
									.get(LegDataAttributes.P_SESSION_ID));

			sipSessionPickupLeg.setAttribute(PhConstants.LEG_ID,
					CallDataAttribute.P_LEG2.name());

			resynchLegs(callData, action);
		} else {

			logger.error("Could not resynch clal pick up legs either of call datas are null");
		}

	}

	/**
	 * This method will put the call on hold. Leg which required to be hold must
	 * be defined in action. An event EVENT_HOLD_SUCCESS will be raised on
	 * successful call hold.
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void holdCall(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside holdCall");
		}
		String legId = action.getLeg();
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(legId));

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "putting leg " + legId + " on hold");
		}

		SipServletRequest initialInviteRequest = SipProtocolUtil
				.getInitialInvite(appSession, legData);
		MultipartBody lastSentSdp = (MultipartBody) legData
				.get(LegDataAttributes.P_LAST_SENT_SDP);
		lastSentSdp = SipProtocolUtil.getSdpContentForHold(origLegCallId,
				lastSentSdp, false);
		SipServletRequest reInviteRequest = SipProtocolMessageCreator
				.createReinviteRequest(initialInviteRequest, lastSentSdp);

		SipProtocolUtil.setSentSdp(reInviteRequest, legData, origLegCallId);

		reInviteRequest.send();
	}

	/**
	 * This method will create conference dialog to have communications between
	 * multiple parties.
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void createConference(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside createConference");
		}
		try {
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			SipServletRequest initialRequest = SipProtocolUtil
					.getInitialInvite(appSession, legData);

			MsConferenceSpec conferenceSpec = (MsConferenceSpec) legData
					.get(LegDataAttributes.NP_CONF_SPEC);

			appSession.setAttribute(PhConstants.ATTRIB_HOST_SESSION, true);
			appSession.setAttribute(PhConstants.CONF_B2BUA_MODE, true);
			initialRequest.getApplicationSession().setAttribute(
					PhConstants.ATTRIB_PARTICIPANT_INVITE, initialRequest);
			ConferenceHandler.getInstance().createConf(appSession,
					initialRequest, conferenceSpec.getConferenceId());

			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_CONF_JOINED);
		} catch (Exception ex) {
			logger.error("Error while createConference :: " + ex);
		}

	}

	/**
	 * This method will record voice call.
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void recordCall(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside recordCall");
			logger.debug(origLegCallId + ":: State is : " + state);
		}

		if (state == State.MS_CONNECTED || state == State.MS_PLAY
				|| state == State.MS_PLAY_COLLECT
				|| state == State.MS_PLAY_RECORD || state == State.CONNECTED) {

			try {
				SBBFactory sbbFactory = (SBBFactory) appSession
						.getAttribute(PhConstants.ATTRIB_SBB_FACTORY);
				if (sbbFactory == null) {

					logger.error("[PH] ConferenceHandler:connectParticipant SBBFactory not available in application session.");
					return;
				}
				MsRecordSpec msRecordSpec = (MsRecordSpec) legData
						.get(LegDataAttributes.NP_REC_SPEC);
				String confId = (String) appSession
						.getAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID);
				if (confId != null) {
					ConferenceController confController = (ConferenceController) sbbFactory
							.getSBB(ConferenceController.class.getName(),
									confId,
									appSession,
									PhUtilityServices
											.getInstance(
													(String) callData
															.get(CallDataAttribute.SERVICE_ID))
											.getServletContext());

					String baseUriStr = confController.getMediaServer()
							.getRecordingBaseURI().toString();

					java.net.URI uri = msRecordSpec.getRecordingDestination();

					String filePath = baseUriStr + File.separator
							+ uri.getPath();
					if (logger.isDebugEnabled()) {
						logger.debug("recording destinition : " + filePath);
					}
					msRecordSpec.setRecordingDestination(new java.net.URI(
							filePath));
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: invoke recoedCall on confController");
					}

					callData.set(CallDataAttribute.P_RECORD_START_TIME,
							new Date());
					confController.record(msRecordSpec);
				} else {
					GroupedMsSessionController msController = SipProtocolUtil
							.getMsController(appSession, legData,
									action.getLeg());

					String baseUriStr = msController.getMediaServer()
							.getRecordingBaseURI().toString();

					java.net.URI uri = msRecordSpec.getRecordingDestination();

					String filePath = baseUriStr + File.separator
							+ uri.getPath();
					if (logger.isDebugEnabled()) {
						logger.debug("recording destinition : " + filePath);
					}
					msRecordSpec.setRecordingDestination(new java.net.URI(
							filePath));
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: invoke recordCall on msController");
					}

					callData.set(CallDataAttribute.P_RECORD_START_TIME,
							new Date());
					msController.record(msRecordSpec);
				}

				legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_RECORD);
			} catch (Exception ex) {
				logger.error(origLegCallId
						+ ":: Call Record failed. Error is : "
						+ ex.getMessage());
				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: Call Record failed : " + ex);
					logger.info(origLegCallId
							+ ":: Notify service that Call Record failed");
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						SipProtocolRelReasonCode.EXCEP_RECORD);
				legData.set(
						LegDataAttributes.P_CAUSE_CODE,
						Integer.parseInt(SipProtocolUtil
								.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(EventType.EVENT_RECORD_FAILURE,
						Protocol.SIP, action.getLeg());

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}

		} else {
			logger.error(origLegCallId
					+ ":: Call record invoked in invalid state : " + state
					+ ", dropCall");

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
					SipProtocolRelReasonCode.EXCEP_RECORD);
			legData.set(
					LegDataAttributes.P_CAUSE_CODE,
					Integer.parseInt(SipProtocolUtil
							.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));

			dropCall(appSession);
		}

	}

	/**
	 * This method will play annoucement and record voice call.
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void playAndRecord(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		ConnectionType connType = (ConnectionType) legData
				.get(LegDataAttributes.P_CONNECTION_TYPE);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside playAndRecord");
			logger.debug(origLegCallId + ":: State is : " + state);
		}

		if (State.MS_CONF_JOINED.equals(state)
				|| State.MS_PLAY_COLLECT.equals(state)
				|| State.MS_CONNECTED.equals(state)
				|| State.MS_PLAY.equals(state)) {
			try {
				SBBFactory sbbFactory = (SBBFactory) appSession
						.getAttribute(PhConstants.ATTRIB_SBB_FACTORY);
				if (sbbFactory == null) {

					logger.error("[PH] ConferenceHandler:connectParticipant SBBFactory not available in application session.");
					return;
				}
				String confId = (String) appSession
						.getAttribute(PhConstants.ATTRIB_PARTICIPANT_CONFID);
				MsRecordSpec msRecordSpec = (MsRecordSpec) legData
						.get(LegDataAttributes.NP_REC_SPEC);

				if (logger.isDebugEnabled()) {
					logger.debug("msRecordSpec max duration is : " + msRecordSpec.getMaxRecordingTime());
				}
				
				if (confId != null) {
					ConferenceController confController = (ConferenceController) sbbFactory
							.getSBB(ConferenceController.class.getName(),
									confId,
									appSession,
									PhUtilityServices
											.getInstance(
													(String) callData
															.get(CallDataAttribute.SERVICE_ID))
											.getServletContext());

					String annBaseUriStr = confController.getMediaServer()
							.getAnnouncementBaseURI().toString();

					String recordingBaseUriStr = confController
							.getMediaServer().getRecordingBaseURI().toString();
					java.net.URI uri = msRecordSpec.getRecordingDestination();

					String filePath = recordingBaseUriStr + File.separator
							+ uri.getPath();
					if (logger.isDebugEnabled()) {
						logger.debug("recording destinition : " + filePath);
					}
					msRecordSpec.setRecordingDestination(new java.net.URI(
							filePath));

					MsPlaySpec msPlaySpec = new MsPlaySpec();

					SipProtocolUtil.formPlaySpec(callData, legData,
							annBaseUriStr, msPlaySpec, null);
					msPlaySpec.setPlayExit(null, null, "play.end play.amt");

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: invoke playAndRecord on confController");
					}

					callData.set(CallDataAttribute.P_RECORD_START_TIME,
							new Date());

					((MsSessionController) confController).playRecord(
							msPlaySpec, msRecordSpec);
				} else {
					GroupedMsSessionController msController = SipProtocolUtil
							.getMsController(appSession, legData,
									action.getLeg());

					String annBaseUriStr = msController.getMediaServer()
							.getRecordingBaseURI().toString();

					String recordingBaseUriStr = msController.getMediaServer()
							.getRecordingBaseURI().toString();
					java.net.URI uri = msRecordSpec.getRecordingDestination();

					String filePath = recordingBaseUriStr + File.separator
							+ uri.getPath();
					if (logger.isDebugEnabled()) {
						logger.debug("recording destinition : " + filePath);
					}
					msRecordSpec.setRecordingDestination(new java.net.URI(
							filePath));
					MsPlaySpec msPlaySpec = new MsPlaySpec();

					SipProtocolUtil.formPlaySpec(callData, legData,
							annBaseUriStr, msPlaySpec, null);
					msPlaySpec.setPlayExit(null, null, "play.end play.amt");
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: invoke playAndRecord on msController");
					}

					callData.set(CallDataAttribute.P_RECORD_START_TIME,
							new Date());
					msController.playRecord(msPlaySpec, msRecordSpec);
				}
			} catch (Exception ex) {
				logger.error(origLegCallId
						+ ":: PlayAndRecord failed. Error is "
						+ ex.getMessage());

				if (logger.isInfoEnabled()) {
					logger.info(origLegCallId + ":: PlayAndRecord failed", ex);
					logger.info(origLegCallId
							+ ":: Notify service that PlayAndRecord failed");
				}
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.EXCEP_PLAYREC);
				legData.set(
						LegDataAttributes.P_CAUSE_CODE,
						Integer.parseInt(SipProtocolUtil
								.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance(
								(String) callData
										.get(CallDataAttribute.SERVICE_ID))
						.getServiceHandler();
				Event event = new Event(EventType.EVENT_PNR_FAILURE,
						Protocol.SIP, action.getLeg());

				ProtocolRouter.getInstance().execute(event, callData,
						serviceHandler);
			}
		} else {
			logger.error(origLegCallId
					+ ":: PlayAndRecord invoked in invalid state, drop call");
			logger.error(origLegCallId + ":: ConnType = " + connType
					+ ", state=" + state);
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.UNEXP_ACT_PLAYREC);
			legData.set(
					LegDataAttributes.P_CAUSE_CODE,
					Integer.parseInt(SipProtocolUtil
							.getConfig(SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			dropCall(appSession);
		}

		SipProtocolUtil.incrementNetworkTransactions(callData, 1);
	}
	
	
	/**
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	protected static void proxyInitialRequest(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {
		
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside proxyInitialRequest for Leg "
					+ action.getLeg());
		}

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		SipServletRequest initialInvRequest = SipProtocolUtil.getInitialInvite(
				appSession, legData);

		boolean stateful = false;

		if (action.getConnectionMode().equals(
				Action.CONNECTIONMODE.PROXY_STATEFULL)) {
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside proxyInitialRequest connection mode is do PROXY_STATEFULL");
			}
			stateful = true;
		} else if (action.getConnectionMode().equals(
				Action.CONNECTIONMODE.PROXY_STATELESS)) {
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside proxyInitialRequest connection mode is do PROXY_STATELESS");
			}
			stateful = false;
		}
		
	
		Proxy proxy = initialInvRequest.getProxy();
		
		if (stateful) {
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: setRecordRoute(true) for  PROXY_STATEFULL");
			}
			proxy.setRecordRoute(true);
		}
		
		Boolean needToAddPCHargeVector = Boolean.valueOf(SipProtocolConfig
				.getConfigData(SipProtocolConfig.ADD_P_CHARGE_VECTOR));
		if (callData.get(CallDataAttribute.P_CHARGE_VECTOR) != null
				&& needToAddPCHargeVector) {

			String pChargeVect = (String) callData
					.get(CallDataAttribute.P_CHARGE_VECTOR);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: set P_CHARGE_VECTOR header on outging proxy request "
						+ pChargeVect);
			}
			initialInvRequest.setHeader(PhConstants.P_CHARGE_VECTOR, pChargeVect);
		}
		
		updatePServedUserHeader(callData ,initialInvRequest);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside proxyInitialRequest proxy to initialInvRequest.getRequestURI()");
		}
		
		legData.set(LegDataAttributes.P_LEG_SIP_STATE,
				State.PROXY_CONNECT);
		
		SipFactory sipFactory = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getSipFactory();

		String sendIncomingURI = SipProtocolConfig
				.getConfigData(SipProtocolConfig.SEND_INCOMING_RURI_TOTERM);
		
		String destURI = null;
		String rURI=null;
		
			
			destURI = "sip:" + SipProtocolUtil.getTerminatingIp(callData, legData)+ ":"
					+ SipProtocolUtil.getTerminatingPort(callData, legData);
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Terminating IP and PORT  " +destURI);
			}
				
		
		if (PhConstants.TRUE.equals(sendIncomingURI)) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Send same Request URI as received ");
			}

			if (initialInvRequest.getHeader(PhConstants.ROUTE_HEADER) == null) {
				if (destURI != null) {
					//
					// if (logger.isDebugEnabled()) {
					// logger.debug(origLegCallId
					// + ":: add route header as "+destURI
					// +" To route call from where it is received ");
					// }
					// initialInvRequest.addHeader(PhConstants.ROUTE_HEADER,
					// destURI);
					//
					// }
					
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Push route " +destURI);
					}
					initialInvRequest.pushRoute((SipURI) sipFactory
							.createURI(destURI));
				}
			}

			rURI = initialInvRequest.getRequestURI().toString();
		} else {
			rURI = destURI;
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ "::SEND_INCOMING_RURI_TOTERM is false so proxy to   " +rURI);
			}
			
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: proxy  call to  " +rURI);
		}
	    URI routeURI=	sipFactory.createURI(rURI);

		proxy.proxyTo(routeURI);
	//	proxy.setSupervised(false);
		
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Leaving  proxyInitialRequest proxied to  "
					+ initialInvRequest.getRequestURI());
		}

	}

	private static void updatePServedUserHeader(CallData callData,
			SipServletRequest initialInvRequest) {
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside updatePServedUserHeader as per information available");
		}

		if (leg1Data.get(LegDataAttributes.P_SERVD_USER_PRIORITY) != null) {
			String priority = (String) leg1Data
					.get(LegDataAttributes.P_SERVD_USER_PRIORITY);
			initialInvRequest.setHeader(PhConstants.HDR_PRIORITY, priority);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ ":: Inside updatePServedUserHeader leaving");
		}
	}
}

