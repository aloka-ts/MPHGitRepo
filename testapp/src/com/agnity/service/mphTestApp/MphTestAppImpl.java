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

package com.agnity.service.mphTestApp;

import jain.protocol.ss7.SccpUserAddress;

import java.util.HashSet;
import java.util.Set;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.CONNECTIONMODE;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.AnnSpec.ANN_TYPE;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolConfig;
import com.baypackets.ase.util.AseAlarmUtil;

public class MphTestAppImpl implements ServiceInterface {

	/*
	 * lnpServiceImplObj : LnpServiceImpl instance to make this class singleton
	 */
	private static final MphTestAppImpl mphTestAppImplObj = new MphTestAppImpl();

	private static Logger logger = Logger.getLogger(MphTestAppImpl.class);

	/**
	 * @return LnpServiceImpl instance
	 */
	public static MphTestAppImpl getInstance() {
		return mphTestAppImplObj;
	}

	public static MphTestAppImpl getInstance(AseAlarmUtil aseAlarmUtilRef) {
		return mphTestAppImplObj;
	}

	public static boolean initialize() {

		return true;
	}

	/**
	 * <p>
	 * To send 302 to orig:- 1. Create ACTION with ACTION_REDIRECT 2. Set LEG in
	 * ACTIOn to LEG1 which is orig leg 3. Set Destination phone number
	 * PhoneNumber in orig LegData 4. Set Protocol type in Action Refer method
	 * sendRedirect
	 * <p>
	 * To connect orig and term :- 1. Create Action ACTION_CONNECT 2. Create
	 * LegData object for destination and set in callData 3. Set destination
	 * LegData key in Action->Leg 4. Set destination Phone Number in destination
	 * LegData Refer connectTerm mPH will connect term leg and will send EVENT
	 * EVENT_SUCCESS with term leg in EVENT object. Then to connect orig and
	 * term 1. Create Action ACTION_RESYNC_CALL 2. Set Protocol in Action 3. Set
	 * destination/term leg name in Action mPH will connect orig and term and
	 * will return EVENT EVENT_RESYNC_SUCCESS with Leg->TERM leg
	 * <p>
	 * Disconnect/END the call 1.Create ACTION ACTION_END_CALL 2. Set Protocol
	 * in Action 3. Set SIP error code for all legs 4. No need to set Leg in
	 * action Refer dropCall() method
	 * <p>
	 * Play and disconnect. Play chargeable ann 1. Create action
	 * ACTION_CONNECT_MS 2. Set Action->Protocol = SIP, Action->Leg=LEG1 and
	 * return this action to mPH 3. Set LEG1
	 * LegData(PersistableData)->IS_CHARGEABLE_ANN attribute to TRUE 4. mPH will
	 * return EVENT_MS_SUCCESS event after connecting orig to MS 5. Create
	 * action ACTION_PLAY 6. mPH will return EVENT_PLAY_SUCCESS 7. Now disconect
	 * IVR connected to orig. Cretae action ACTION_DISCONNECT_MS 8. mPH will
	 * return EVENT_MS_DISCONNECT 9. Now drop the call. Refer
	 * connectOrigToIvrInConfirmed() and playAnnToOrig(), disconnectOrigIvr(),
	 * dropCall()
	 */
	@Override
	public Action[] processEvent(Event event, CallData callData) {
		Action[] actionArr = null;
		if (event != null && callData != null) {
			Protocol protocol = event.getProtocol();
			switch (protocol) {
			case SIP:
				actionArr = processSipEvent(event, callData);
				break;
			case ITUINAPCS1_SCF:
				actionArr = processInapCS1Event(event, callData);
				break;
			default:
				logger.error("Unknown unsupported protocol event received returning NULL ... ");
				break;
			}
		}
		return actionArr;
	}

	private Action[] processInapCS1Event(Event event, CallData callData) {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		Action[] actionArr = null;
		logger.debug(origLegCallId + ":: Inside processEvent for INAP");
		logger.debug(origLegCallId + ":: Event is " + event);
		switch (event.getEventType()) {
		case EVENT_INITIAL: {
			logger.debug(origLegCallId + " Rceeived a new call for protocol "
					+ event.getProtocol());
		//	actionArr = connectInapTerm(origLegCallId, callData,
			//		Action.SEND_MODE.END); // it is for connect flow
			 actionArr=connectInapOrigIVR(origLegCallId, callData);// its is
			// for ivr flow
			break;
		}
		case EVENT_MS_SUCCESS: {
			logger.debug(origLegCallId + " Received MS success event  "
					+ event.getProtocol());
			// actionArr = connectInapTerm(origLegCallId, callData);
			actionArr = playAnnToOrig(origLegCallId, callData);
			break;
		}

		case EVENT_SUCCESS: {
			logger.debug(origLegCallId + " Rceeived Success event "
					+ event.getProtocol());
			break;
		}
		case EVENT_DISCONNECT: {
			logger.debug(origLegCallId + " Rceeived disconnect event "
					+ event.getProtocol());

			actionArr = dropCall(origLegCallId, callData, null,
					Protocol.ITUINAPCS1_SCF);
			break;
		}
		default:
			break;
		}
		return actionArr;

	}

	private Action[] processSipEvent(Event event, CallData callData) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		Action[] actionArr = null;
		logger.debug(origLegCallId + ":: Inside processEvent for SIP");
		logger.debug(origLegCallId + ":: Event is " + event);
		switch (event.getEventType()) {
		case EVENT_INITIAL: {
			logger.debug(origLegCallId + " Rceeived a new call for protocol "
					+ event.getProtocol());
			// actionArr = sendRedirect(origLegCallId, callData);
			actionArr = connectTerm(origLegCallId, callData);
			// actionArr = connectOrigToIvrInConfirmed(origLegCallId, callData);
			// actionArr = connectOrigToIvrInEarlyMedia(origLegCallId,
			// callData);
			break;
		}
		case EVENT_SUCCESS: {
			if (!CallDataAttribute.P_LEG1.equals(event.getLeg())) {
				LegData termLegData = (LegData) callData
						.get(CallDataAttribute.P_LEG2);
				logger.debug(origLegCallId + " 200 OK received from term "
						+ termLegData);
				// logger.debug(origLegCallId
				// + " Creating action to connect orig and term "
				// + termLegData);
				// return connectOrigAndTerm(origLegCallId, callData);
				logger.debug(origLegCallId
						+ " Creating action to connect term to MS "
						+ termLegData);
				return connectTermToIvr(origLegCallId, callData);
			}
			break;
		}
		case EVENT_RESYNC_SUCCESS: {
			logger.debug(origLegCallId + " orig and term connected");
			// return dropCall(origLegCallId, callData, "503");
			return null;

		}
		case EVENT_DISCONNECT: {

			if (CallDataAttribute.P_LEG1.equals(event.getLeg())) {
				logger.debug(origLegCallId + " Call disconnected by Orig");
			} else {
				logger.debug(origLegCallId + " Call disconnected by Term");
			}
			return dropCall(origLegCallId, callData, "503", Protocol.SIP);
		}
		case EVENT_MS_SUCCESS: {

			if (CallDataAttribute.P_LEG1.equals(event.getLeg())) {
				logger.debug(origLegCallId
						+ "Orig connected to IVR. Now play ann to orig");
				return playAnnToOrig(origLegCallId, callData);
			} else {
				logger.debug(origLegCallId
						+ "Term connected to IVR. Now play ann to term");
				return playAnnToTerm(origLegCallId, callData);
			}

		}
		case EVENT_PLAY_SUCCESS: {

			if (CallDataAttribute.P_LEG1.equals(event.getLeg())) {
				logger.debug(origLegCallId
						+ "Ann played to orig. Now disconnect IVR connected to orig");

				if (callData
						.get(CallDataAttribute.P_CORRELATION_ID) != null) {

					logger.debug(origLegCallId
							+ "Ann played to orig. Now disconnect INAP IVR connection ");

					return disconnectOrigIvr(origLegCallId, callData,
							Protocol.ITUINAPCS1_SCF);
				} else {

					logger.debug(origLegCallId
							+ "Ann played to orig. Now disconnect SIP IVR connection ");
					return disconnectOrigIvr(origLegCallId, callData,
							Protocol.SIP);
				}
			} else {
				logger.debug(origLegCallId
						+ "Ann played to term. Now disconnect IVR connected to term");
				return disconnectTermIvr(origLegCallId, callData);
			}
		}
		case EVENT_MS_DISCONNECT: {

			logger.debug(origLegCallId
					+ "Orig IVR connection disconnected do nothing need to check cleanup timer for ETC invite");
			//
			// if (origLegCallId.equals(event.getLeg())) {
			// logger
			// .debug(origLegCallId +
			// "Orig IVR connection disconnected. Now drop call.");
			// return dropCall(origLegCallId, callData, "487");
			// } else {
			// logger
			// .debug(origLegCallId
			// +
			// "Term IVR connection disconnected. Now connect orig and term.");
			// return connectOrigAndTerm(origLegCallId, callData);
			// }

		}
		default:
			break;
		}
		return actionArr;

	}

	/*
	 * This method creates and returns ACTION for mPH to send SIP 302 response
	 */
	public Action[] sendRedirect(String origLegCallId, CallData callData) {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendRedirect() Send 302 to orig");
		Action[] actionArr = null;
		actionArr = new Action[1];
		String destinationNumber = "123412";
		PhoneNumber destPhNumber = new PhoneNumber();
		destPhNumber.setAddress(destinationNumber);
		Action action = new Action(Action.ActionType.ACTION_REDIRECT);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		leg1.set(LegDataAttributes.P_DESTINATION_NUMBER,
				destPhNumber);
		actionArr[0] = action;

		/*
		 * Set Terminating IP and port. These will be set in 302 response
		 * contact header
		 */
		leg1.set(LegDataAttributes.P_REMOTE_IP, "127.0.0.1");
		leg1.set(LegDataAttributes.P_REMOTE_PORT, "5040");
		return actionArr;
	}

	/*
	 * When service want to connect orig and term for first time then -- 1.
	 * After receiving initial INVITE mPH will send EVENT_INITIAL to service 2.
	 * Service will return CONNECT_TERM 3. mPH will send initial INVITE to term
	 * leg. After receiving 200 OK from term, mPH will send EVENT_CONNECTED to
	 * service with legId of TERM Leg 4. Then service will return SYNC_WITH_ORIG
	 * ACTION to connect orig and term term leg
	 */
	public Action[] connectTerm(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId
				+ " connectTerm() Send initial INVITE to term and connect mPH to term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		/*
		 * Create LegData for term leg and set destination details on term
		 * legData
		 */
		String destinationNumber = "123412";
		PhoneNumber destPhNumber = new PhoneNumber();
		destPhNumber.setAddress(destinationNumber);
		Action action = new Action(Action.ActionType.ACTION_CONNECT);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		LegData termLegData = new LegData();
		termLegData.set(LegDataAttributes.P_DESTINATION_NUMBER,
				destPhNumber);
		termLegData.set(LegDataAttributes.P_CONNECTION_TYPE,
				ConnectionType.TERM_CONNECTION);
		callData.set(CallDataAttribute.P_LEG2, termLegData);

		actionArr[0] = action;

		/*
		 * Set Terminating IP and port. These will be set in 302 response
		 * contact header
		 */

		termLegData.set(LegDataAttributes.P_REMOTE_IP,
				"127.0.0.1");
		termLegData.set(LegDataAttributes.P_REMOTE_PORT, "5090");
		return actionArr;
	}

	/**
	 * This method creates action to connect orig to media-server. Orig is
	 * connected in confirmed state or chargeable mode
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	public Action[] connectOrigToIvrInConfirmed(String origLegCallId,
			CallData callData) {
		logger.debug(origLegCallId
				+ " connectIvrInConfirmed() Connect orig to IVR in confirmed mode");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_CONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		origLegData.set(LegDataAttributes.NP_IS_CHARGEABLE_ANN,
				"TRUE");
		actionArr[0] = action;
		return actionArr;
	}

	/**
	 * This method creates action to connect term to media-server.
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	public Action[] connectTermToIvr(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId + " connectTermToIvr() Connect term to IVR");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_CONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());

		actionArr[0] = action;
		return actionArr;
	}

	/**
	 * This method creates action to connect orig to media-server. Orig is
	 * connected in early media state
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	public Action[] connectOrigToIvrInEarlyMedia(String origLegCallId,
			CallData callData) {
		logger.debug(origLegCallId
				+ " connectOrigToIvrInEarlyMedia() Connect orig to IVR in EarlyMedia mode");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_CONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		/*
		 * If service does not set IS_CHARGEABLE_ANN then default is early media
		 * TRUE
		 */
		origLegData.set(LegDataAttributes.NP_IS_CHARGEABLE_ANN,
				"FALSE");
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] playAnnToOrig(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId + " playAnnToOrig() play ann to orig");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_PLAY);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		// Create and set annSpec in LegData
		AnnSpec annSpec = new AnnSpec();
		annSpec.setTerminationKey("Z");
		annSpec.setEsacpeKey("Z");
		annSpec.setAnnIteration(5);
		annSpec.setAnnLength(3);
		annSpec.setAnnLanguage(AnnSpec.ANN_LANG_EN_US);
		annSpec.addMessage("ann/1101011013.wav", ANN_TYPE.ANN);
		origLegData.set(LegDataAttributes.NP_ANN_SPEC, annSpec);
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] playAnnToTerm(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId + " playAnnToTerm() play ann to term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_PLAY);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		LegData termLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG2);
		// Create and set annSpec in LegData
		AnnSpec annSpec = new AnnSpec();
		annSpec.setTerminationKey("Z");
		annSpec.setEsacpeKey("Z");
		annSpec.setAnnIteration(5);
		annSpec.setAnnLength(3);
		annSpec.setAnnLanguage(AnnSpec.ANN_LANG_EN_US);
		annSpec.addMessage("MyAnnFolder/myAnn.wav", ANN_TYPE.ANN);

		if (termLegData != null) {
			termLegData.set(LegDataAttributes.NP_ANN_SPEC,
					annSpec);
		}else{
			logger.debug(origLegCallId + " could not playAnnToTerm() play ann to term termleg data is null");
		}
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] connectOrigAndTerm(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId
				+ " connectTerm() 200 OK received from term. Now connect orig and term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_RESYNC_CALL);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] disconnectOrigIvr(String origLegCallId, CallData callData,
			Protocol protocol) {
		logger.debug(origLegCallId + " disconnectOrigIvr()");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_DISCONNECT_MS);
		action.setProtocol(protocol);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] disconnectTermIvr(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId + " disconnectTermIvr()");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_DISCONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] dropCall(String origLegCallId, CallData callData,
			String causeCode, Protocol protocol) {
		logger.debug(origLegCallId + " dropCall()");

		Action[] actionArr = null;
		actionArr = new Action[1];

		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		origLegData.set(LegDataAttributes.P_CAUSE_CODE,
				causeCode);
		Action action = new Action(Action.ActionType.ACTION_END_CALL);
		action.setDropCallMode(Action.DROP_CALL_MODE.RELEASE_CALL);
		action.setProtocol(protocol);
		action.setLeg(null);// No need to set leg in action
		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * When service want to send connect for IDP request. After receiving
	 * initial INVITE mPH will send EVENT_INITIAL to service 2. Service will
	 * return CONNECT_TERM 3. mPH will send CONNECT request to term
	 */
	public Action[] connectInapTerm(String origLegCallId, CallData callData,
			Action.SEND_MODE sendMode) {
		logger.debug(origLegCallId
				+ " connectTerm() Send initial INVITE to term and connect mPH to term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		/*
		 * Create LegData for term leg and set destination details on term
		 * legData
		 */
		String destinationNumber = "123412";
		PhoneNumber destPhNumber = new PhoneNumber();
		destPhNumber.setAddress(destinationNumber);
		destPhNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		callData.set(CallDataAttribute.P_DESTINATION_NUMBER,
				destPhNumber);
		Action action = new Action(Action.ActionType.ACTION_CONNECT);
		action.setProtocol(Protocol.ITUINAPCS1_SCF);
		action.setConnectionMode(CONNECTIONMODE.REROUTING);
		action.setSendMode(sendMode);

		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData OriglegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		OriglegData.set(LegDataAttributes.P_DESTINATION_NUMBER,
				destPhNumber);
		OriglegData.set(LegDataAttributes.P_CONNECTION_TYPE,
				ConnectionType.TERM_CONNECTION);

		/*
		 * Set Terminating IP and port. These will be set in 302 response
		 * contact header
		 */
		if (action.getSendMode() != Action.SEND_MODE.END) {
			Set<Action.ERB_TYPE> erbSet = new HashSet<Action.ERB_TYPE>();
			erbSet.add(Action.ERB_TYPE.ERB_BUSY);
			erbSet.add(Action.ERB_TYPE.ERB_NO_ANSWER);
			erbSet.add(Action.ERB_TYPE.ERB_DISCONNECT);
			erbSet.add(Action.ERB_TYPE.ERB_ANSWER);
			OriglegData.set(LegDataAttributes.P_ERB_SET, erbSet);
		}

		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * When service want to send connect for IDP request. After receiving
	 * initial INVITE mPH will send EVENT_INITIAL to service 2. Service will
	 * return CONNECT_TERM 3. mPH will send CONNECT request to term
	 */
	public Action[] connectInapOrigIVR(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId
				+ " connectTerm() Send initial INVITE to term and connect mPH to term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		/*
		 * Create LegData for term leg and set destination details on term
		 * legData
		 */

		Action action = new Action(Action.ActionType.ACTION_CONNECT_MS);
		action.setProtocol(Protocol.ITUINAPCS1_SCF);
		action.setConnectionMode(CONNECTIONMODE.ASSIST);
		action.setLeg(CallDataAttribute.P_LEG1.name());

		SccpUserAddress localAddr = InapCS1ScfProtocolConfig.getInstance()
				.getSccpLocalAddressList().get(0);
		callData.set(
				CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS, localAddr);
		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		origLegData.set(LegDataAttributes.NP_IS_CHARGEABLE_ANN,
				PhConstants.TRUE);
		setCorrelationId(callData);

		origLegData.set(LegDataAttributes.NP_IS_SDP_SENT,
				PhConstants.FALSE);

		actionArr[0] = action;
		return actionArr;
	}

	private void setCorrelationId(CallData callData) {
		// TODO Auto-generated method stub

		String corrId = "12345678";
		callData.set(CallDataAttribute.P_CORRELATION_ID, corrId);

	}

	@Override
	public String getServletName() {
		logger.debug("Inside getServletName");
		return "mphTestAppSipServlet";
	}

	@Override
	public String getApplicationName() {
		logger.debug("Inside getApplicationName");
		return "1";
	}

	@Override
	public String[] getServiceCdr(CallData callData) {
		logger.debug("Inside getServiceCdr");
		String[] cdr = new String[1];
		cdr[0] = "MphTestAppCDR";
		return cdr;
	}
};
