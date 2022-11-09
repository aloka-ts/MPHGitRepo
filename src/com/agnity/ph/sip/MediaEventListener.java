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

import java.util.Date;

import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipServletMessage;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipServletResponse;
import javax.servlet.sip.SipSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.AnnSpec.ANN_TYPE;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.MultipartBody;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.State;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.isup.SipIsupHelper;
import com.baypackets.ase.sbb.GroupedMsSessionController;
import com.baypackets.ase.sbb.MediaServer;
import com.baypackets.ase.sbb.MsOperationResult;
import com.baypackets.ase.sbb.MsSessionController;
import com.baypackets.ase.sbb.SBB;
import com.baypackets.ase.sbb.SBBEvent;
import com.baypackets.ase.sbb.SBBEventListener;
import com.baypackets.ase.sbb.impl.SBBOperationContext;
import com.baypackets.ase.sbb.util.Constants;

/**
 * This class is used to listen Media SBB events and does their processing.
 *
 */
public class MediaEventListener implements SBBEventListener {

	private static final long	serialVersionUID	= -490338805492170199L;
	private transient Logger	logger				= Logger.getLogger(MediaEventListener.class);

	@Override
	public void activate(SBB sbb) {

	}

	@Override
	public int handleEvent(SBB sbb, SBBEvent sbbEvent) {
		SipApplicationSession appSession = null;
		CallData callData = null;
		String origLegCallId = "0";

		String sbbEventId = sbbEvent.getEventId();

		try {
			appSession = sbb.getApplicationSession();

			// To fix "Object already invalided" exception observed in
			// production
			if (appSession == null || !appSession.isValid()) {
				if (logger.isDebugEnabled()) {
					logger.debug("Do nothing as appsession is null or invalidated. Event "
							+ sbbEventId);
				}
				return SBBEventListener.CONTINUE;
			}

			callData = SipProtocolUtil.getCallData(appSession);

			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Handle Event " + sbbEventId);
			}

			if (sbbEventId.equals(SBBEvent.EVENT_EARLY_MEDIA_CONNECT_PROGRESS)) {

				return handleEarlyMediaConnectProgress(sbb, sbbEvent);
			} else if (sbbEventId.equals(SBBEvent.EVENT_EARLY_MEDIA)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 2);
				return handleEarlyMediaConnected(sbb, sbbEvent);
			} else if (sbbEventId.equals(SBBEvent.EVENT_CONNECTED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 4);
				return handleConnected(sbb, sbbEvent);
			} else if (sbbEventId.equals(MsSessionController.EVENT_PLAY_COMPLETED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 1);

				return handlePlayCompleted(sbb, sbbEvent);
			} else if (sbbEventId.equals(MsSessionController.EVENT_PLAY_COLLECT_COMPLETED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 2);

				return handlePlayCollectCompleted(sbb, sbbEvent);
			} else if (sbbEventId.equals(SBBEvent.EVENT_DISCONNECTED)) {

				if (sbb.getB() != null
						&& !PhConstants.TRUE.equals(sbb.getB().getAttribute(
								PhConstants.MS_BYE_TRXN_INCREMENTED))) {
					SipProtocolUtil.incrementNetworkTransactions(callData, 1);
				}

				return handleDisconnected(sbb, sbbEvent);
			} else if (sbbEventId.equals(SBBEvent.EVENT_SIG_IN_PROGRESS)) {

				if (sbbEvent.getMessage() != null
						&& sbbEvent.getMessage().getMethod().equals("INVITE")) {

					SipProtocolUtil.incrementNetworkTransactions(callData, 2);
				} else {
					SipProtocolUtil.incrementNetworkTransactions(callData, 1);
				}
				return handleSigInProgress(sbb, sbbEvent);
			} else if (sbbEventId.equals(SBBEvent.EVENT_CONNECT_FAILED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 1);

				return handleConnectFailed(sbb, sbbEvent);
			} else if (sbbEventId.equals(SBBEvent.EVENT_DISCONNECT_FAILED)) {
				return handleDisconnectFailed(sbb, sbbEvent);
			} else if (sbbEventId.equals(MsSessionController.EVENT_PLAY_COLLECT_FAILED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 2);

				return handlePlayCollectFailed(sbb, sbbEvent);
			} else if (sbbEventId.equals(MsSessionController.EVENT_PLAY_FAILED)) {
				SipProtocolUtil.incrementNetworkTransactions(callData, 2);

				return handlePlayFailed(sbb, sbbEvent);
			} else if (sbbEventId.equals(SBBEvent.EVENT_CONNECT_PROGRESS)) {
				return handleConnectProgress(sbb, sbbEvent);
			} else if (sbbEventId.equals(MsSessionController.EVENT_PLAY_RECORD_COMPLETED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 3);
				if(logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Play Record Completed recieved");
				}
			} else if (sbbEventId.equals(MsSessionController.EVENT_PLAY_RECORD_FAILED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 2);

				return handlePlayRecordFailed(sbb, sbbEvent);
			} else if (sbbEventId.equals(MsSessionController.EVENT_STOP_RECORD_COMPLETED)){
				//|| sbbEventId.equals(MsSessionController.EVENT_STOP_RECORD_FAILED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 3);

				String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

				LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
				State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

				if (state == State.MS_PLAY_COLLECT) {

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Stop operation done for play collect !!! ");
					}
					return handlePlayCollectCompleted(sbb, sbbEvent);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Stop operation done for play  !!!");
					}

					return handlePlayCompleted(sbb, sbbEvent);
				}
			} else if (sbbEventId
					.equals(MsSessionController.EVENT_STOP_RECORD_FAILED)) {

				SipProtocolUtil.incrementNetworkTransactions(callData, 2);

				String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

				LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
				State state = (State) legData
						.get(LegDataAttributes.P_LEG_SIP_STATE);

				if (state == State.MS_PLAY_COLLECT) {

					logger.error(origLegCallId
							+ ":: Stop operation failed for play collect !!! ");

				} else {

					logger.error(origLegCallId
							+ ":: Stop operation failed for play !!! ");
				}
			} else if (sbbEventId.equals(SBBEvent.EVENT_MS_SESSION_EXPIRED)) {
				return handleOrigIvrSessionExpired(sbb, sbbEvent);
			} else if (sbbEventId.equals(SBBEvent.EVENT_SIG_COMPLETED)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: EVENT_SIG_COMPLETED received, do nothing");
				}
			} else if(sbbEventId.equals("PLAY_COLLECT_DONE")||sbbEventId.equals("PLAY_COLLECT_APP.DONE")){
				handlePlayCollectDone(sbb, sbbEvent);
			}else if(sbbEventId.equals("PLAY_DONE")){
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: PLAY_DONE received, do nothing");
				}
			}else if(sbbEventId.equals("RECORD_DONE")){
				if(logger.isDebugEnabled()){
					logger.debug(origLegCallId + ":: RECORD_DONE received");
				}
				handleRecordCompleted(sbb, sbbEvent);
			}else if(sbbEventId.equals(MsSessionController.EVENT_PLAY_IN_PROGRESS)){
				if(logger.isDebugEnabled()){
					logger.debug(origLegCallId + ":: EVENT_PLAY_IN_PROGRESS received");
				}
				String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
				ServiceInterface serviceInterface = PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_PLAY_IN_PROGRESS, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
			}else if(sbbEventId.equals(MsSessionController.EVENT_PLAY_RECORD_DONE)||sbbEventId.equals("PLAY_RECORD_APP.DONE")){
				if(logger.isDebugEnabled()){
					logger.debug(origLegCallId + ":: PLAY_RECORD_DONE received");
				}
				SipProtocolUtil.incrementNetworkTransactions(callData, 3);
				handlePlayRecordCompleted(sbb, sbbEvent);
			}else {
				logger.error(origLegCallId + ":: Unexpected orig MS SBB Event with id "
						+ sbbEventId + ", do nothing");
				// dont set release reason code as call is not getting
				// disconnected here
				// callData.setReasonForRelease(ReleaseReasonCode.UNEXPECTED_MSSBB_EVENT_ORIG);

				// TODO: Confirm if call needs to be dropped here
			}
		} catch (Exception ex) {
			logger.error(origLegCallId + ":: Orig MS SBB event processing failed", ex);
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.EXCEP_IN_HANDLE_MSEVENT_ORIG);
			LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
			leg1.set(LegDataAttributes.P_CAUSE_CODE, Integer
					.parseInt(SipProtocolUtil.getConfig(
							SipProtocolConfig.SIP_RES_CODE_IN_EXCEPTION)));
			SipProtocolHelper.dropCall(appSession);
		}
		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the play failed event from MSSbb. This event notify
	 * service about the play request failure
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handlePlayFailed(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		MsOperationResult msOperationResult = ((GroupedMsSessionController)sbb).getResult();

		String reason = getReason(origLegCallId,msOperationResult);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handlePlayFailed for " + legId + "at state "
					+ state+ "Reason is "+reason);
		}

		switch (state) {
		case MS_PLAY: {
			logger.error(origLegCallId + ":: Play ann failed on " + legId);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Notify service that play ann failed");
			}

			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					reason);
			// Parse the message and fill leg data in call data
			Event event = new Event(EventType.EVENT_PLAY_FAILURE, Protocol.SIP, legId);
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			break;
		}
		default: {
			logger.error(origLegCallId + ":: PLAY_FAILED received in invalid state " + state
					+ " for leg " + legId + ".Do nothing.");
			// TODO: Confirm if call needs to be dropped here.
		}

		}

		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handled the play record failed event from MSSbb. This event
	 * notify service about the failure of the play record request
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handlePlayRecordFailed(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		MsOperationResult msOperationResult = ((GroupedMsSessionController)sbb).getResult();
		String reason = getReason(origLegCallId,msOperationResult);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handlePlayRecordFailed for " + legId
					+ "at state " + state + " Reason is "+reason);

		}
		switch (state) {
		case MS_PLAY_RECORD: {
			logger.error(origLegCallId + ":: Play record failed on " + legId);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Notify service that recording failed");
			}
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					reason);
			// Parse the message and fill leg data in call data
			Event event = new Event(EventType.EVENT_PNR_FAILURE, Protocol.SIP, legId);
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			break;
		}
		default: {
			logger.error(origLegCallId + ":: PLAY_RECORD_FAILED received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
			// TODO: Confirm if call needs to be dropped here.
			break;
		}
		}
		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the play collect failed event from MSSbb. This event
	 * notify service about play collect failure
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handlePlayCollectFailed(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);

		MsOperationResult msOperationResult = ((GroupedMsSessionController)sbb).getResult();
		String reason = getReason(origLegCallId,msOperationResult);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handlePlayCollectFailed for " + legId
					+ "at state " + state +"  Reason is "+reason);

		}
		switch (state) {
		case MS_PLAY_RECORD: {
			logger.error(origLegCallId + ":: Play colect failed on " + legId);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Notify service that play and collect failed");
			}
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					reason);
			// Create Event and inform service
			Event event = new Event(EventType.EVENT_PNC_FAILURE, Protocol.SIP, legId);
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			break;
		}
		case MS_PLAY_COLLECT: {
			logger.error(origLegCallId + ":: Play colect failed on " + legId);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Notify service that play and collect failed");
			}
			//				callData.setNonpersistableData(CallDataAttribute.NP_REASON_FOR_RELEASE,
			//								SipProtocolRelReasonCode.IVR_PLAYCOL_FAILED);
			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					reason);
			// Create Event and inform service
			Event event = new Event(EventType.EVENT_PNC_FAILURE, Protocol.SIP, legId);
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			break;
		}
		default: {
			logger.error(origLegCallId + ":: PLAY_COLLECT_FAILED received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
			// TODO: Confirm if call needs to be dropped here.
			break;
		}
		}
		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the disconnect failed event from MSSbb. This event
	 * notify service about failure of disconnect request. For
	 * TERMINATION_IN_PROGRESS and TERMINATED orig sip call state Invalidate
	 * appsession if possible and for MS_DISCONNECTION_IN_PROGRESS orig sip call
	 * state notify the service with connectionDisconnected event.
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handleDisconnectFailed(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		SipSession ivrSession = sbb.getB();
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleDisconnectFailed for " + legId
					+ "at state " + state);

		}
		switch (state) {
		case TERMINATED: {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set ready to invalidate flag on " + legId
						+ " ivr");
			}
			if(null != ivrSession) {
				ivrSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
			}
			// Invalidate appsession if possible
			SipProtocolUtil.invalidateAppSession(appSession);
			break;
		}
		case MS_DISCONNECTED: {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Set ready to invalidate flag on " + legId
						+ " ivr");
			}
			/*
			 * In this case, service might need to continue the call. So the
			 * state is set as INIT to re-initialize current leg state machine.
			 */
			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_DISCONNECTED);
			if(null != ivrSession) {
				ivrSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
			}
			if (ActionType.ACTION_DISCONNECT_MS == (ActionType) callData
					.get(CallDataAttribute.P_CURRENT_ACTION)) {
				// Create Event and inform service
				Event event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.SIP, legId);
				ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			}
			break;
		}
		default: {
			logger.error(origLegCallId + ":: DISCONNECT_FAILED received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
			// TODO: Confirm if call needs to be dropped here.
			break;
		}
		}
		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the connect failed event from MSSbb. This event
	 * notify service about connect request failure. For TERMINATED orig sip
	 * state Notify service with connectionFailed event and for other cases set
	 * release cause value to 41 as it is SBTM requirement that on IVR connect
	 * failure, if call gets dropped; it should be dropped with cause value 41.
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handleConnectFailed(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		SipSession legSipSession = SipProtocolUtil.getSipSessionFromSessionId(
				origLegCallId, appSession, (String) legData.get(LegDataAttributes.P_SESSION_ID));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		SipSession ivrSession = sbb.getB();
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleConnectFailed  for " + legId
					+ "at state " + state);
		}

		ivrSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
		switch (state) {
		case MS_CONN_IN_PROGRESS: {

			/*
			 * Added below 2 attributes for BUG 1603
			 */
			ivrSession.setAttribute(PhConstants.IVR_LEG,PhConstants.TRUE);
			legData.set(LegDataAttributes.P_IVR_SESSION_ID,
					sbb.getB().getId());
			/*
			 * ends here
			 */

			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_DISCONNECTED);
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			SipServletRequest legInitialInv = SipProtocolUtil.getInitialInvite(appSession,
					legData);
			legInitialInv.getSession().setHandler(serviceHandler.getServletName());

			MediaServer mediaServer = ((GroupedMsSessionController)sbb).getMediaServer();
			if(null != mediaServer){
				callData.set(CallDataAttribute.P_MS_ID, mediaServer.getId());
			}
			if (legInitialInv.getSession().getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
				logger.warn(origLegCallId + "Connect failed rcvd from MS SBB for " + legId);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Notify service that " + legId
							+ " connection failed");
				}
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.IVR_CONNECT_FAILED);

				legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
				// Create Event and inform service
				Event event = new Event(EventType.EVENT_FAILURE, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			} else {
				logger.warn(origLegCallId + "Failed to connect " + legId + " to MS");
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Notify service that ivr connection failed for leg "
							+ legId);
				}

				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.IVR_CONNECT_FAILED);
				// Create Event and inform service
				Event event = new Event(EventType.EVENT_MS_FAILURE, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			}
			/*
			 * Return NOOP as next action would have been performed based on
			 * ACTION returned. In case of DROP_CALL action, all the remaing sip
			 * legs would be cleaned-up
			 */
			return SBBEventListener.NOOP;

		}
		default: {
			logger.error(origLegCallId + ":: CONNECT_FAILED received in invalid state " + state
					+ " for leg " + legId + ".Do nothing.");
			// TODO: Confirm if call needs to be dropped here
		}
		}

		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the signal in progress event from MSSbb. This event
	 * notifies service about any event received from orig leg
	 * <ul>
	 * <li>For an event received for Re-INVITE, do Nothing.
	 * <li>For an event received for INFO request during MS interaction MSSbb will send 200 OK for
	 * it and do nothing.
	 * <li>For a Bye event received for BYE request from Orig, Inform service that Orig
	 * disconnected.
	 * <li>For a Bye received from Orig Ivr service might need to continue the call. So the state is
	 * set as SERVICE_LOGIC to re-initialize the term leg state machine.Notify service with
	 * connectionFailed.
	 * </ul>
	 * Rest include following cases:
	 * <ul>
	 * <li>CANCEL from party A while listening the ANN or in process of MS connect
	 * <li>PRACK timeout for the 18x sent
	 * <li>ACK timeout for the 200 OK sent Call can not be recovered from here. So just drop the
	 * call with cause value 41 to cleanup any remaing sip leg.
	 * </ul>
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handleSigInProgress(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		SipServletMessage sipMsg = sbbEvent.getMessage();
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleSigInProgress for " + legId + "at state "
					+ state);
			logger.debug(origLegCallId + ":: Message = "
					+ (sipMsg == null ? "NULL" : sipMsg.getMethod()));

		}

		if (sipMsg != null && sipMsg.getMethod().equalsIgnoreCase(PhConstants.INVITE_REQUEST)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Event received for Re-INVITE, do Nothing");
			}
			SipServletRequest legInitialInv = SipProtocolUtil.getInitialInvite(appSession, legData);
			
			SipProtocolUtil.setReceivedSdp(sipMsg, callData);
			if (legInitialInv.getSession().getState() == javax.servlet.sip.SipSession.State.INITIAL
					|| legInitialInv.getSession().getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
				logger.warn(origLegCallId + ":: Stuck MS Session case, cleaning it");
				GroupedMsSessionController msController = SipProtocolUtil.getMsController(
						appSession, legData, legId);
				try {
					if (PhConstants.TRUE.equals(callData
							.get(CallDataAttribute.NP_FT_CALL))
							|| msController.getB() == null) {
						msController
						.addB(SipProtocolUtil.getSipSessionFromSessionId(
								origLegCallId,
								appSession,
								(String) legData
								.get(LegDataAttributes.P_IVR_SESSION_ID)));
					}
				} catch (Throwable e) {
					logger.warn(origLegCallId + " B already added in Orig MS Sip Session");
				}
				msController.setApplicationSession(appSession);
				msController.setEventListener(this);
				msController.disconnectMediaServer();
				SipProtocolUtil.setAppSessionTimeout(appSession, 1, origLegCallId);
				return SBBEventListener.NOOP;
			}
		} else if (sipMsg != null && sipMsg.getMethod().equalsIgnoreCase(PhConstants.INFO_REQUEST)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: INFO recieved during MS interaction");
			}
			/*
			 * SBTM-UAT-928: SUS/RES INFO should be forwarded to peer leg if
			 * orig and term leg are in connected state
			 */
			LegData peerLegData = SipProtocolUtil.getPeerLegData(callData, (ConnectionType) legData
					.get(LegDataAttributes.P_CONNECTION_TYPE));
			SipServletRequest peerLegInvReq = SipProtocolUtil.getInitialInvite(appSession,
					peerLegData);

			if (peerLegInvReq != null
					&& peerLegInvReq.getSession().getState() == javax.servlet.sip.SipSession.State.CONFIRMED
					&& sipMsg.getSession().getState() == javax.servlet.sip.SipSession.State.CONFIRMED) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: INFO Received in connected state");
					logger.debug(origLegCallId + ":: Forward it transparently to other leg");
				}
				SipServletRequest peerInfoRequest = peerLegInvReq.getSession().createRequest(
						PhConstants.INFO_REQUEST);
				peerInfoRequest.setContent(sipMsg.getContent(), sipMsg.getContentType());
				peerInfoRequest.send();
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: MSSbb will send 200 OK for it and do nothing");
				}
			}
			// Return NOOP as no need to do any thing
			return SBBEventListener.NOOP;
		} else if (sipMsg != null && sipMsg.getMethod().equalsIgnoreCase(PhConstants.BYE_REQUEST)) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Recieved sig in progress for BYE");
			}

			//Set this sip message to other leg to copy content, reason header etc
			SipIsupHelper.setErrByeCanMsgOnOtherLeg(sipMsg);

			SipSession legSipSession = SipProtocolUtil.getSipSessionFromSessionId(origLegCallId,
					appSession,
					(String) legData.get(LegDataAttributes.P_SESSION_ID));
			if (legSipSession.getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Bye received from leg " + legId);
				}
				// Set handler back
				ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				legSipSession.setHandler(srviceHandler.getServletName());
				// Set this session to READY_TO_INVALIDATE. FIx for
				// bug#13800
				legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
				legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.TERMINATED);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Inform service that " + legId
							+ " disconnected");
				}
				/**
				 * NP_ORIG_BYE_ON_IVR is used so that app sesion is not invalidated when app is dropping call and sbb is able to send  200ok for
				 * bye form orig and we only set timeout as 1 min in dropcall so that sbb sends 200 ok and then later 1 min app sesison gets invalidated
				 */
				callData.set(CallDataAttribute.NP_ORIG_BYE_ON_IVR, PhConstants.TRUE);
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.BYE_RCVD_FROM_ORIG);
				// Create Event and inform service
				int assistVal=-1;
				if(logger.isDebugEnabled()){
					logger.debug("ASSIST INDIACTION Value::"+assistVal);
				}
				
				if(callData.get(CallDataAttribute.P_ASSIST_IND)!=null){
					assistVal=(int)callData.get(CallDataAttribute.P_ASSIST_IND);
				}
				if(assistVal==1){
					Event event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.SIP, legId);
					ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
					return SBBEventListener.NOOP;
				}else{
					Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.SIP, legId);
					ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
					return SBBEventListener.NOOP;
				}
				
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Bye received from IVR of leg " + legId);
				}
				/*
				 * In this case, service might need to continue the call. So the
				 * state is set as SERVICE_LOGIC to re-initialize the term leg
				 * state machine.
				 */
				legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_DISCONNECTED);
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.BYE_RCVD_FROM_IVR);
				/*
				 * Here connectionFailed should be called on service instead of
				 * connectionDisconnected because here the MS connection is
				 * disconnected by MS and not by PH/Service. So from service
				 * perspective IVR connection is failed.
				 */
				// Create Event and inform service
				ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
				return SBBEventListener.NOOP;
			}
		} else if (sipMsg != null && sipMsg.getMethod().equalsIgnoreCase(PhConstants.UPDATE_REQUEST)) {

			return SBBEventListener.CONTINUE;
		}else{
			/*
			 * This section covers the following cases: 1. CANCEL from party A
			 * while listening the ANN or in process of MS connect 2. PRACK
			 * timeout for the 18x sent 3. ACK timeout for the 200 OK sent
			 * 
			 * Call can not be recovered from here. So just drop the call with
			 * cause value 41 to cleanup any remaing sip leg.
			 */

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Error on originating session");
				logger.debug(origLegCallId + ":: Call can not be recovered from here");
				logger.debug(origLegCallId + ":: Drop call just to cleanup remaining sip session");
			}

			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.TERMINATED);

			if (SBBEvent.REASON_CODE_CANCELLED_BY_ENDPOINT.equals(sbbEvent.getReasonCode())) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: CANCEL received from " + legId);
				}

				/*
				 * SBTM-UAT-555: Do not set PhConstant.READY_TO_INVALIDATE.
				 * Otherwise SipInapIsupHandlerServlet.cleanupSipSession() will
				 * NOT send 487 for INVITE
				 */
				/*
				 * origSipRequest.getSession().setAttribute(PhConstant.
				 * READY_TO_INVALIDATE, PhConstant.TRUE);
				 */
				/*
				 * SBTM-UAT-555: If following attribute is set on orig SIP
				 * Session, then SipInapIsupHandlerServlet.cleanupSipSession()
				 * will send 487 for INVITE on orig leg
				 */
				legData
				.set(LegDataAttributes.P_IS_CANCEL_RECEIVED, PhConstants.TRUE);
				legData.set(LegDataAttributes.P_CAUSE_CODE, SipServletResponse.SC_REQUEST_TERMINATED);
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.CANCEL_RCVD_FROM_ORIG);
			} else {
				logger.error(origLegCallId + ":: ACK/PRACK/200 OK (UPDATE) timedout, drop call");
				legData.set(LegDataAttributes.P_CAUSE_CODE, Integer
						.parseInt(SipProtocolUtil.getConfig(
								SipProtocolConfig.ACK_PRACK_TIMEOUT)));
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.ACK_PRACK_TIMEOUT);
			}

			SipProtocolHelper.dropCall(appSession);

			// Return CONTINUE from here so that MSSbb can perform remaing
			// task if any
			return SBBEventListener.CONTINUE;
		}

		// Default return statement
		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the ivr session expired event from MSSbb. This event
	 * notify service about the disconnection of IVR leg due to session expire
	 * Call should be disconnected with CV=41, if ORIG session refresh timer
	 * expires
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handleOrigIvrSessionExpired(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleOrigIvrSessionExpired for " + legId
					+ "at state " + state);

		}

		// Set handler back
		ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		SipSession legSipSession = SipProtocolUtil.getSipSessionFromSessionId(origLegCallId, appSession,
				(String) legData.get(LegDataAttributes.P_SESSION_ID));
		legSipSession.setHandler(srviceHandler.getServletName());

		// Set this session to READY_TO_INVALIDATE. FIx for bug#13800
		legSipSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
		/*
		 * The state is set to the CONNECTION_IN_PROGRESS as it is require to
		 * continue the call or for proper call cleanup
		 */
		legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.CONN_IN_PROGRESS);

		logger.error(origLegCallId + ":: Session expired for MS connection of " + legId);

		callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
				SipProtocolRelReasonCode.SESSION_EXPIRED_IVR);

		Event event = new Event(EventType.EVENT_MS_FAILURE, Protocol.SIP, legId);
		ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
		return SBBEventListener.NOOP;
	}

	/**
	 * This method handles the disconnected event from MSSbb. This event notify
	 * the service that MS has been disconnected.
	 * <ul>
	 * <li>For Terminated Calls just invalidate the app session.
	 * <li>For MS_DISCONNECTION_IN_PROGRESS, service might need to continue the call. So the state
	 * is set as SERVICE_LOGIC to re-initialize the term leg state machine. Also notify the service.
	 * <li>For all other cases Call should be disconnected with CV=41, if ORIG session refresh timer
	 * expires.
	 * </ul>
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handleDisconnected(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
		SipSession ivrSession = sbb.getB();
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleDisconnected for " + legId + " at state "
					+ state);

		}
		if(null != ivrSession) {
			ivrSession.setAttribute(PhConstants.SESSION_READY_TO_INVALIDATE, PhConstants.TRUE);
		}
		switch (state) {
		case TERMINATED: {
			// Set this session to READY_TO_INVALIDATE. FIx for bug#13800
			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.CONN_IN_PROGRESS);

			// Invalidate appsession if possible
			SipProtocolUtil.invalidateAppSession(appSession);
			break;
		}
		case MS_DISCONNECTED: {
			/*
			 * In this case, service might need to continue the call. So the
			 * state is set as SERVICE_LOGIC to re-initialize the term leg state
			 * machine.
			 */
			//legData.setPersistableData(LegDataAttributes.STATE, State.MS_DISCONNECTED);

			/*
			 * This check ensures that service will be informed only when
			 * DISCONNECT_IVR is invoked alone. In case DISCONNECT_IVR is
			 * invoked along with another action, the latest action would have
			 * been changed by the time MS SBB disconnect the MS, and in this
			 * case PH will not inform service about MS connection disconnection
			 */
			Action action = (Action) callData
					.get(CallDataAttribute.P_CURRENT_ACTION);
			
			Action legAction = (Action) legData
					.get(LegDataAttributes.P_LEG_CURRENT_ACTION);
			
			if (action.getActionType() == Action.ActionType.ACTION_DISCONNECT_MS
					|| (legAction != null && legAction.getActionType() == Action.ActionType.ACTION_DISCONNECT_MS)) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: Notify service that ivr connection disconnected for "
							+ legId);
				}

				// DO not send MS_DISCONNECTED in case if it is for AIN Call
				if(!action.isMsDiconnectedForAinCall()){
					// Create Event and inform service
					ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
					Event event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.SIP, legId);
					ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
				} else {
					if(logger.isDebugEnabled()){
						logger.debug(origLegCallId + "Not sending EVENT_MS_DISCONNECT for AIN CALL");
					}
				}
			}
			break;
		}
		case MS_PLAY:
		case MS_PLAY_COLLECT:
		case MS_PLAY_RECORD: {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: DISCONNETED received as Orig Session Expired");
			}

			// Set handler back
			ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			SipSession legSipSession = SipProtocolUtil
					.getSipSessionFromSessionId(origLegCallId, appSession, (String) legData
							.get(LegDataAttributes.P_SESSION_ID));
			legSipSession.setHandler(srviceHandler.getServletName());

			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.TERMINATED);

			logger.error(origLegCallId + ":: Session expired for originating connection");

			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
					SipProtocolRelReasonCode.SESSION_EXPIRED_ORIG);

			Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.SIP, legId);
			ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
			break;
		}
		default: {
			// TODO: Confirm if call needs to be dropped here
			/*
			 * READY_TO_INVALIDATE on orig IVR. Because for IVR Routing +
			 * Connect Term flow, when disconnected event is received from
			 * MS-Sbb then origSipCallState is connection_in_progress so
			 * defgault case is invoked.
			 */
			logger.error(origLegCallId + ":: DISCONNECTED received in invalid state " + state
					+ " for leg " + legId + ".Do nothing.");

		}
		}

		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the play record completed event from MSSbb. This
	 * event notify service about completion of play record request.
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */

	private int handlePlayRecordCompleted(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handlePlayRecordCompleted for " + legId
					+ "at state " + state);

		}

		switch (state) {
		case MS_PLAY_RECORD:
		case MS_PLAY:
		case MS_PLAY_COLLECT:{
			MsOperationResult result = ((MsSessionController) sbb).getResult();
			String recordingLength = (String) result
					.getAttribute(MsOperationResult.RECORDING_LENGTH);
			
			if(recordingLength !=null &&recordingLength.endsWith("ms")){
				recordingLength= recordingLength.substring(0, recordingLength.indexOf("ms"));
			}
			/*
			 * SBTM-UAT-1258: Trim 0.5 seconds from recorded ann if reason to
			 * stop recording is # key
			 */
			GroupedMsSessionController groupedMsSessionController = SipProtocolUtil
					.getMsController(appSession, legData, legId);
			MsOperationResult msOperationResult = groupedMsSessionController.getResult();
			String reason = (String) msOperationResult.getAttribute("reason");
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Notify service that " + recordingLength
						+ " long announcement recorded. Reason to stop recording: "
						+ reason);
			}
			legData.set(LegDataAttributes.NP_STOP_RECORDING_REASON, reason);
			legData.set(LegDataAttributes.NP_RECORDING_LENGTH,
					recordingLength);

			ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event event = new Event(EventType.EVENT_PNR_SUCCESS, Protocol.SIP, legId);
			ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
			break;
		}
		default: {
			logger.error(origLegCallId + ":: PLAY_RECORD_COMPLETED received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
			// TODO: Confirm if call needs to be dropped here
		}

		}

		return SBBEventListener.CONTINUE;
	}
	//Added this method to support XMS
	private int handlePlayCollectDone(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handlePlayCollectDone for " + legId
					+ "at state " + state);

		}
		switch (state) {
		case MS_PLAY_COLLECT: {
			/*
			 * In case of MSML based MS like XMS, the collected Digits will be the part of
			 * MSMLResult for PLAY_COLLECT_DONE event and not PLAY_COLLECT_COMPLETED
			 * event.
			 */
			MsOperationResult result = ((MsSessionController) sbb).getResult();
			String digits = (String) result.getAttribute(MsOperationResult.COLLECTED_DIGITS);
			String playDuration = (String) result.getAttribute(MsOperationResult.PLAY_DURATION);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Collected digits are [" + digits + "]");
				logger.debug(origLegCallId + ":: Play duration is " + playDuration);
			}
			/*
			 * In case of MSML, we get the #[termination key] also in return,
			 * even if the return key is marked as #.
			 */
			AnnSpec annSpec = (AnnSpec) legData
					.get(LegDataAttributes.NP_ANN_SPEC);
			String termKey = annSpec.getTerminationKey();
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Termination Key is " + termKey);
			}
			if (digits == null) {
				digits = "";
			} else if (termKey != null) {
				if (digits.equals(termKey)) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: Noinput provided by user");
					}
					int sipMsPassRkt=Integer.parseInt(SipProtocolConfig.getConfigData(SipProtocolConfig.SIP_MS_PASS_RTK));
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: sipMsPassRkt val :"+sipMsPassRkt);
					}
					
					if(sipMsPassRkt != 1){
						digits = "";
					}
					
				} else if (digits.endsWith(termKey)) {
					digits = digits.substring(0, digits.indexOf(termKey));
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId + ":: User did not press termination key "
								+ termKey + " after providing input " + digits
								//+ ". So assuming no input");
								+ ". So keeping digits as it is.");
					}
					// Bug 24634 - Do not discard the digits if without term key
					//digits = "";
				}
			}
			legData.set(LegDataAttributes.P_COLLECTED_DIGITS, digits);
			break;
		}
		default: {
			logger.error(origLegCallId + ":: PLAY_COLLECT_DONE received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
		}
		}

		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the play collect completed event from MSSbb. This
	 * event notify service about successful play collect completion.
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handlePlayCollectCompleted(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handlePlayCollectCompleted for " + legId
					+ "at state " + state);

		}

		switch (state) {
		case MS_PLAY_COLLECT: {

			SipSession legSipSession = SipProtocolUtil
					.getSipSessionFromSessionId(origLegCallId, appSession, (String) legData
							.get(LegDataAttributes.P_SESSION_ID));
			ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

			if (legSipSession.getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
				logger.error(origLegCallId + ":: Session expired for " + legId);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Notify service that " + legId
							+ " disconnected");
				}
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.SESSION_EXPIRED);

				// Create event and inform services
				Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, srviceHandler);

			} else {
				if (legData.get(LegDataAttributes.P_COLLECTED_DIGITS) == null) {
					MsOperationResult result = ((MsSessionController) sbb).getResult();
					String digits = (String) result
							.getAttribute(MsOperationResult.COLLECTED_DIGITS);
					String playDuration = (String) result
							.getAttribute(MsOperationResult.PLAY_DURATION);

					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Notify service that collected digits are ["
								+ digits + "]");
						logger.debug(origLegCallId + ":: Play duration is " + playDuration);
					}
					legData
					.set(LegDataAttributes.P_COLLECTED_DIGITS, digits);
				}

				// Create event and inform services
				Event event = new Event(EventType.EVENT_PNC_SUCCESS, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
			}
			break;
		}
		default: {
			logger.error(origLegCallId + ":: PLAY_COLLECT_COMPLETED received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
			// TODO: Confirm if call needs to be dropped here
		}
		}
		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the play completed event from MSSbb. This event
	 * confirm the completion of play request
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handlePlayCompleted(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handlePlayCompleted for " + legId + "at state "
					+ state);

		}
		/*
		 * bug#16453 When PH calls, stopMediaOperations() on msController,
		 * MS-Sbb returns two events to PH. PLAY_COMPLETED and
		 * STOP_RECORD_COMPLETED. PH will inform service on receiving
		 * STOP_RECORD_COMPLETED and not on PLAY_COMPLETED. Also the following
		 * if block is not added in handlePlayTermCompleted() as PH is calling
		 * stopMediaOperations() for orig MS only.
		 */

		/*
		 * GroupedMsSessionController groupedMsSessionController =
		 * (GroupedMsSessionController) sbb;
		 */
		GroupedMsSessionController groupedMsSessionController = SipProtocolUtil.getMsController(
				appSession, legData, legId);
		MsOperationResult msOperationResult = groupedMsSessionController.getResult();
		String reason = (String) msOperationResult.getAttribute("reason");
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: reason : " + reason);
			logger.debug(origLegCallId + ":: msOperationResult.getResponseCode : "
					+ msOperationResult.getResponseCode());
		}

		if (PhConstants.STOP_ANN_INFO_REASON.equalsIgnoreCase(reason)) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Announcement Stopped. Inform service for stopped ms operation");
			}
			ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			
			Event event = new Event(EventType.EVENT_MS_OPERATION_STOPPED, Protocol.SIP, legId);
			ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
			return SBBEventListener.CONTINUE;
		}
		switch (state) {
		case MS_PLAY: {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Notify service that ann played successfully");
			}
			ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			SipSession legSipSession = SipProtocolUtil
					.getSipSessionFromSessionId(origLegCallId, appSession, (String) legData
							.get(LegDataAttributes.P_SESSION_ID));

			if (legSipSession.getState() == javax.servlet.sip.SipSession.State.TERMINATED) {
				logger.error(origLegCallId + ":: Session expired on " + legId);
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: Notify service that " + legId
							+ " conn disconnected");
				}
				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.SESSION_EXPIRED);

				// Create event and inform services
				Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
			} else {
				// Create event and inform services
				Event event = new Event(EventType.EVENT_PLAY_SUCCESS, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, srviceHandler);
			}
			break;
		}
		default: {
			logger.error(origLegCallId + ":: PLAY_COMPLETED received in invalid state " + state
					+ " for leg " + legId + ".Do nothing.");
			// TODO: Confirm if call needs to be dropped here
		}
		}

		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the connect in progress event from MSSbb. This event
	 * provides an opportunity to service to set ISUP content if any or some
	 * other flag on MSSbb before it sends provisional response.
	 * <ul>
	 * <li>Stores the MS SDP for handling the boundary scenarios where re-invite for session refresh
	 * come from orig leg just after IVR is disconnected. Though this case would not handle them
	 * re-invite for SDP change.
	 * <li>In case ACM/CPG of party B has already been sent to party A,send CPG with PROGRESS
	 * indicator only. So that CIT and CA does not get changed from last sent and billing records
	 * get generated with party B charging area information.
	 * <li>Sets 100ms delay between success and provisional response.
	 * <li>Sets P-CDR-INFO in final response if not the assist case and Set attempted indicator to 0
	 * as sending 200 OK to caller.
	 * </ul>
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws Exception
	 */
	private int handleConnectProgress(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleConnectProgress for " + legId
					+ "at state " + state);

		}

		switch (state) {
		case MS_CONN_IN_PROGRESS: {
			/*
			 * Store the MS SDP for handling the boundary scenarios where
			 * re-invite for session refresh come from orig leg just after IVR
			 * is disconnected. Though this case would not handle the re-invite
			 * for SDP change.
			 */
			if (sbbEvent.getMessage() != null) {
				SipSession ivrSipSession = sbbEvent.getMessage().getSession();
				if (logger.isDebugEnabled()) {
					logger
					.debug(origLegCallId + ":: MS Session ID is " + ivrSipSession.getId());
				}
				// Set connection type as ivr connection
				legData.set(LegDataAttributes.P_IVR_SESSION_ID,
						ivrSipSession.getId());
				/*
				 * IVR_LEG attribute is set on all IVR sipSessions to
				 * distinguish IVR sipSessions from orig and term sip sessions
				 */
				ivrSipSession.setAttribute(PhConstants.IVR_LEG, PhConstants.TRUE);
				ivrSipSession.setAttribute(PhConstants.LEG_ID, legId);
				callData.remove(CallDataAttribute.P_LEG_ID_CONNECTED_TO_IVR);

				Object msSdp = ivrSipSession.getAttribute(SBBOperationContext.ATTRIBUTE_SDP);
				if (msSdp != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Store MS SDP sent to orig for reference");
					}
					MultipartBody msSdpContent = new MultipartBody((byte[]) msSdp,
							PhConstants.APPLICATION_SDP);

					legData.set(LegDataAttributes.P_LAST_SENT_SDP, msSdpContent);
				}

				if (logger.isDebugEnabled()) {
					logger
					.debug(origLegCallId
							+ ":: SDP CURRENT RCVD SDP "
							+ ivrSipSession
							.getAttribute(SBBOperationContext.ATTRIBUTE_SDP_PARTY_B));
				}

			}

			GroupedMsSessionController msController = SipProtocolUtil.getMsController(
					appSession, legData, legId);

			// Set UPDATE_NEEDED flag on MS if SDP has already been sent on
			// orig leg
			boolean isSdpSentOnLeg1Earlier = PhConstants.TRUE.equals((String) legData
					.get(LegDataAttributes.NP_IS_SDP_SENT));
			if (isSdpSentOnLeg1Earlier) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SDP already sent so set UPDATE_NEEDED flag on MS");
				}
				msController.setAttribute(Constants.UPDATE_NEEDED, PhConstants.TRUE);
			}

			// Set 100ms delay between success and provisional response
			msController.setAttribute(Constants.TIMEOUT_REQUIRED, "100");
			break;
		}
		default: {
			logger.error(origLegCallId + ":: EVENT_CONNECT_PROGRESS received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
			// TODO: Discuss if call needs to be dropped here
		}
		}
		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the connected event from MSSbb. This event provided
	 * by MSSbb after sending 200 OK to origination.
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws IllegalArgumentException
	 * @throws IllegalStateException
	 * @throws Exception
	 */
	private int handleConnected(SBB sbb, SBBEvent sbbEvent) throws Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleConnected for " + legId + "at state "
					+ state);

		}

		if (callData
				.get(CallDataAttribute.P_CALL_MS_CONNECT_TIME) == null) {
			callData.set(
					CallDataAttribute.P_CALL_MS_CONNECT_TIME, new Date());
		}

		switch (state) {
		case MS_CONNECTED:
		case MS_CONN_IN_PROGRESS:
			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_CONNECTED);

			if (sbbEvent.getMessage() != null) {
				SipSession ivrLegSession = sbbEvent.getMessage().getSession();
				if (logger.isDebugEnabled()) {
					logger
					.debug(origLegCallId + ":: MS Session ID is " + ivrLegSession.getId());
				}

				// Set connection type as ivr connection
				legData.set(LegDataAttributes.P_IVR_SESSION_ID,
						ivrLegSession.getId());

				/*
				 * IVR_LEG attribute is set on all IVR sipSessions to
				 * distinguish IVR sipSessions from orig and term sip sessions
				 */
				ivrLegSession.setAttribute(PhConstants.IVR_LEG, PhConstants.TRUE);
				ivrLegSession.setAttribute(PhConstants.LEG_ID, legId);

				callData.remove(CallDataAttribute.P_LEG_ID_CONNECTED_TO_IVR);
				Object msSdp = ivrLegSession.getAttribute(SBBOperationContext.ATTRIBUTE_SDP);
				if (msSdp != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Store MS SDP sent to term for reference");
					}
					MultipartBody msSdpContent = new MultipartBody((byte[]) msSdp,
							PhConstants.APPLICATION_SDP);
					legData.set(LegDataAttributes.P_LAST_SENT_SDP, msSdpContent);
				}

				if (logger.isDebugEnabled()) {
					logger
					.debug(origLegCallId
							+ ":: SDP CURRENT RCVD SDP "
							+ ivrLegSession
							.getAttribute(SBBOperationContext.ATTRIBUTE_SDP_PARTY_B));
				}

			}
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Notify service that ivr connected");
				logger.debug(origLegCallId + ":: Set IS_SDP_SENT flag to true in leg " + legId);
			}
			// Set IS_SDP_SENT flag to TRUE for future reference
			legData.set(LegDataAttributes.NP_IS_SDP_SENT, PhConstants.TRUE);

			// Create event and inform services
			callData.set(CallDataAttribute.P_MS_ID, ((GroupedMsSessionController)sbb).getMediaServer().getId());

			String fileName=SipProtocolConfig.getConfigData(SipProtocolConfig.ANN_FILE_MUSIC_ON_HOLD);

			if (PhConstants.TRUE.equals(legData
					.get(LegDataAttributes.HOLD_ON_MUSIC_PLAY))) {

				AnnSpec annspec = new AnnSpec();
				annspec.setAnnIteration(10000);
				annspec.setBarge(false);
				annspec.setAnnLanguage(AnnSpec.ANN_LANG_EN_US);
				annspec.addMessage(fileName, ANN_TYPE.ANN);
				legData.set(LegDataAttributes.NP_ANN_SPEC, annspec);
				Action action = new Action(ActionType.ACTION_PLAY);
				action.setLeg(legId);
				SipProtocolHelper
				.playAnnouncement(appSession, callData, action);
			} else {
				ServiceInterface srviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_MS_SUCCESS,
						Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData,
						srviceHandler);
			}
			break;
		default:
			logger.error(origLegCallId + ":: CONNECTED received in invalid state " + state
					+ " for leg " + legId + ".Do nothing.");
			// TODO: Discuss if call needs to be dropped here
		}

		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handle the early media connect progress event from MSSbb.
	 * This event provides an opportunity to service to set ISUP content, or
	 * some other attribute on MSSbb before it sends provisional response to
	 * origination end.
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws IllegalArgumentException
	 * @throws IllegalStateException
	 * @throws Exception
	 */
	private int handleEarlyMediaConnectProgress(SBB sbb, SBBEvent sbbEvent)
			throws IllegalArgumentException, IllegalStateException, Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleEarlyMediaConnectProgress for " + legId
					+ "at state " + state);

		}

		switch (state) {
		case MS_CONN_IN_PROGRESS: {
			/*
			 * Store the MS SDP for handling the boundary scenarios where
			 * re-invite for session refresh come from orig leg just after IVR
			 * is disconnected. Though this case would not handle the re-invite
			 * for SDP change.
			 */
			if (sbbEvent.getMessage() != null) {
				SipSession ivrSipSession = sbbEvent.getMessage().getSession();
				if (logger.isDebugEnabled()) {
					logger
					.debug(origLegCallId + ":: MS Session ID is " + ivrSipSession.getId());
				}
				// Set connection type as ivr connection
				legData.set(LegDataAttributes.P_IVR_SESSION_ID,
						ivrSipSession.getId());

				/*
				 * IVR_LEG attribute is set on all IVR sipSessions to
				 * distinguish IVR sipSessions from orig and term sip sessions
				 */
				ivrSipSession.setAttribute(PhConstants.IVR_LEG, PhConstants.TRUE);
				ivrSipSession.setAttribute(PhConstants.LEG_ID, legId);

				callData.remove(CallDataAttribute.P_LEG_ID_CONNECTED_TO_IVR);

				Object msSdp = ivrSipSession.getAttribute(SBBOperationContext.ATTRIBUTE_SDP);
				if (msSdp != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Store MS SDP sent to orig for reference");
					}
					MultipartBody msSdpContent = new MultipartBody((byte[]) msSdp,
							PhConstants.APPLICATION_SDP);
					legData.set(LegDataAttributes.P_LAST_SENT_SDP, msSdpContent);
				}

			}

			GroupedMsSessionController msController = SipProtocolUtil.getMsController(
					appSession, legData, legId);

			// Set UPDATE_NEEDED flag on MS if SDP has already been sent on
			// orig leg
			boolean isSdpSentOnLeg1Earlier = PhConstants.TRUE.equals((String) legData
					.get(LegDataAttributes.NP_IS_SDP_SENT));
			if (isSdpSentOnLeg1Earlier) {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId
							+ ":: SDP already sent so set UPDATE_NEEDED flag on MS");
				}
				msController.setAttribute(Constants.UPDATE_NEEDED, PhConstants.TRUE);
			}
			break;
		}
		default: {
			logger.error(origLegCallId
					+ ":: EARLY_MEDIA_CONNECT_PROGRESS received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
			/*
			 * TODO:: Once MSSBB fix to send SDP in ACK if INVITE is sent w/o
			 * SDP Disconnect MS here if party-A is terminated or termination in
			 * progress
			 */
		}
		}

		return SBBEventListener.CONTINUE;
	}

	/**
	 * This method handles the early media connected event from MSSbb. This
	 * event given by MSSbb after sending provisional response to origination
	 * end.
	 * 
	 * @param sbb
	 *            represents an instance of SBB
	 * @param sbbEvent
	 *            represents an instance of SBBEvent
	 * @return integer equivalent of SBB event CONTINUE or NOOP
	 * @throws IllegalArgumentException
	 * @throws IllegalStateException
	 * @throws Exception
	 */
	private int handleEarlyMediaConnected(SBB sbb, SBBEvent sbbEvent)
			throws IllegalArgumentException, IllegalStateException, Exception {
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + ":: Inside handleEarlyMediaConnected for " + legId
					+ "at state " + state);
		}


		if (callData
				.get(CallDataAttribute.P_CALL_MS_CONNECT_TIME) == null) {
			callData.set(
					CallDataAttribute.P_CALL_MS_CONNECT_TIME, new Date());
		}
		switch (state) {
		case MS_CONN_IN_PROGRESS: {
			if (sbbEvent.getMessage() != null) {
				SipSession ivrLegSession = sbbEvent.getMessage().getSession();
				Object msSdp = ivrLegSession.getAttribute(SBBOperationContext.ATTRIBUTE_SDP);
				if (msSdp != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(origLegCallId
								+ ":: Store MS SDP sent to "+legId+" for reference");
					}
					MultipartBody msSdpContent = new MultipartBody((byte[]) msSdp,
							PhConstants.APPLICATION_SDP);
					legData.set(LegDataAttributes.P_LAST_SENT_SDP, msSdpContent);
				}

				if (logger.isDebugEnabled()) {
					logger
					.debug(origLegCallId
							+ ":: SDP CURRENT RCVD SDP "
							+ ivrLegSession
							.getAttribute(SBBOperationContext.ATTRIBUTE_SDP_PARTY_B));
				}

			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Set IS_SDP_SENT flag to true in orig leg session");
			}
			// Set IS_SDP_SENT flag to TRUE for future reference
			legData.set(LegDataAttributes.NP_IS_SDP_SENT, PhConstants.TRUE);
			legData.set(LegDataAttributes.P_LEG_SIP_STATE, State.MS_CONNECTED);

			callData.set(CallDataAttribute.P_MS_ID, ((GroupedMsSessionController)sbb).getMediaServer().getId());

			// Early Media Ann. Notify service of IVR Connection Connected
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Notify service that ivr connected");
			}

			// Create event and inform services
			ServiceInterface srviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event event = new Event(EventType.EVENT_MS_SUCCESS, Protocol.SIP, legId);
			ProtocolRouter.getInstance().execute(event, callData, srviceHandler);

			break;
		}
		default: {
			logger.error(origLegCallId + ":: EARLY_MEDIA_CONNECTED received in invalid state "
					+ state + " for leg " + legId + ".Do nothing.");
			// TODO: Discuss if call needs to be dropped here
		}
		}
		return SBBEventListener.CONTINUE;
	}


	/**
	 * This method is used to get the reason of failure of a ms operation
	 * @param msOperationResult
	 * @return
	 */
	private String getReason(String origLegCallId,
			MsOperationResult msOperationResult) {

		String reason=null;
		if (msOperationResult != null) {
			reason = (String) msOperationResult.getAttribute("reason");

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: getReason() Entering  reason from msoperation result is "
						+ reason);
			}
			String dialogEvent = (String) msOperationResult
					.getAttribute(MsOperationResult.MSML_EVENT_NAME);

			if (dialogEvent != null && dialogEvent.equals("msml.dialog.exit")) {
				reason = (String) msOperationResult
						.getAttribute("dialog.exit.status");
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: getReason() returning reason as " + reason);
			}
		}
		return reason;
	}

	private int handleRecordCompleted(SBB sbb, SBBEvent sbbEvent) throws Exception{
		
		SipApplicationSession appSession = sbb.getApplicationSession();
		CallData callData = SipProtocolUtil.getCallData(appSession);
		String origLegCallId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		String legId = (String) sbb.getAttribute(PhConstants.LEG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(legId));
		State state = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
		
		if(logger.isDebugEnabled()){
			logger.debug(origLegCallId + ":: inside handleRecordCompleted for leg : " + legId + ", at state : " + state);
		}
		
		switch(state){
			
			case MS_RECORD:
			case MS_PLAY:
			case MS_PLAY_COLLECT:{
				MsOperationResult result = ((MsSessionController) sbb).getResult();
				String recordingLength = (String) result.getAttribute(MsOperationResult.RECORDING_LENGTH);
				
				if(recordingLength !=null &&recordingLength.endsWith("ms")){
					recordingLength= recordingLength.substring(0, recordingLength.indexOf("ms"));
				}
						
				
				GroupedMsSessionController msSessionController = SipProtocolUtil.getMsController(appSession, legData, legId);
				MsOperationResult operationResult = msSessionController.getResult();
				String reason = (String) operationResult.getAttribute("reason");
				if(logger.isDebugEnabled()){
					logger.debug(origLegCallId + ":: Notify service that " + recordingLength
							+ " long announcement recoreded. Reason to stop recording : " + reason);
				}
				
				legData.set(LegDataAttributes.NP_STOP_RECORDING_REASON, reason);
				legData.set(LegDataAttributes.NP_RECORDING_LENGTH, recordingLength);
				
				ServiceInterface serviceInterface = PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_RECORD_SUCCESS, Protocol.SIP, legId);
				ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
				
				break; 
			}
			default:{
				logger.error(origLegCallId + ":: RECORD_DONE received in invalid state : " + state + ", for leg : " + legId + ".Do Nothing.");
			}
		}
		return SBBEventListener.CONTINUE;
	}
}
