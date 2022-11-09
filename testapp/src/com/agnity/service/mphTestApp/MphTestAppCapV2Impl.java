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
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.apache.log4j.Logger;

import com.agnity.map.datatypes.AnyTimeInterrogationArgMap;
import com.agnity.map.datatypes.AnyTimeInterrogationResMap;
import com.agnity.map.datatypes.AnyTimeModificationArgMap;
import com.agnity.map.datatypes.AnyTimeModificationResMap;
import com.agnity.map.datatypes.AnyTimeSubscriptionInterrogationArgMap;
import com.agnity.map.datatypes.AnyTimeSubscriptionInterrogationResMap;
import com.agnity.map.datatypes.BasicServiceCodeMap;
import com.agnity.map.datatypes.ISDNAddressStringMap;
import com.agnity.map.datatypes.ImsiDataType;
import com.agnity.map.datatypes.RequestedInfoMap;
import com.agnity.map.datatypes.RequestedNodesMap;
import com.agnity.map.datatypes.RequestedSubscriptionInfoMap;
import com.agnity.map.datatypes.SendRoutingInfoArgMap;
import com.agnity.map.datatypes.SendRoutingInfoResMap;
import com.agnity.map.datatypes.SsCodeMap;
import com.agnity.map.datatypes.SsForBSCodeMap;
import com.agnity.map.datatypes.SubscriberIdentityMap;
import com.agnity.map.enumdata.AddlRequestedCAMELSubscriptionInfoMapEnum;
import com.agnity.map.enumdata.BearerServiceCodeMapEnum;
import com.agnity.map.enumdata.DomainTypeMapEnum;
import com.agnity.map.enumdata.ExtentionMapEnum;
import com.agnity.map.enumdata.InterrogationTypeEnumMap;
import com.agnity.map.enumdata.NatureOfAddressMapEnum;
import com.agnity.map.enumdata.NumberPlanMapEnum;
import com.agnity.map.enumdata.RequestedCAMELSubscriptionInfoMapEnum;
import com.agnity.map.enumdata.RequestedNodesMapEnum;
import com.agnity.map.enumdata.SupplementaryServicesMapEnum;
import com.agnity.map.exceptions.InvalidInputException;
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
import com.agnity.ph.capv2scf.Capv2ScfProtocolConfig;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.baypackets.ase.util.AseAlarmUtil;

/**
 * 
 * This Class will handle the CAP Events received from PH and calls backs appropraite action
 * based upon the call data attributes received
 *
 */

public class MphTestAppCapV2Impl implements ServiceInterface {
	private static final String HLR_SSN = "HLR_SSN";
	private static final String HLR_SPC = "HLR_SPC";
	private static SccpUserAddress hlrSccpAddr;
	private static final MphTestAppCapV2Impl mphTestAppImplObj = new MphTestAppCapV2Impl();
	private static Logger logger = Logger.getLogger(MphTestAppCapV2Impl.class);
	private static boolean isHlrInteractionReqd = false;

	/**
	 * @return MphTestAppCapV2Impl instance
	 */
	public static MphTestAppCapV2Impl getInstance() {
		return mphTestAppImplObj;
	}

	/**
	 * @param aseAlarmUtilRef
	 * @return
	 */
	public static MphTestAppCapV2Impl getInstance(AseAlarmUtil aseAlarmUtilRef) {
		return mphTestAppImplObj;
	}

	/**
	 * @return
	 */
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
	 * action ACTION_PLAY 6. mPH will return EVENT_PLAY_SUCCESS 7. Now disconnect
	 * IVR connected to orig. Create action ACTION_DISCONNECT_MS 8. mPH will
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
				case CAPV2_SCF:
					actionArr = processCapCS2Event(event, callData);
					break;
				case MAP_SCF:
					actionArr = processMapEvent(event, callData);
					break;
				default:
					logger.error("Unknown unsupported protocol event received returning NULL");
					break;
			}
		}
		return actionArr;
	}

	/**
	 * This Method processes CAP CS 2 events
	 * @param event
	 * 			It represents an instance of Event
	 * @param callData
	 * 			it represents an instance of CallData
	 * @return
	 * @throws InvalidInputException 
	 */
	private Action[] processMapEvent(Event event, CallData callData) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		Action[] actionArr = null;
		
		if (logger.isDebugEnabled()){
			logger.debug(origLegCallId + ":: Inside processEvent for Cap");
			logger.debug(origLegCallId + ":: Event is " + event);
		}
		
		if (logger.isDebugEnabled()){
			logger.debug(origLegCallId + ":: Inside processEvent for Cap");
			logger.debug(origLegCallId + ":: Event is " + event);
		}
		
		LegData legData = null;
				
		if (callData.get(CallDataAttribute.P_LAST_CALL_ACTION) != null) {

			Action lastAction = (Action) callData
					.get(CallDataAttribute.P_LAST_CALL_ACTION);
			legData = (LegData) callData
					.get(CallDataAttribute.valueOf(lastAction.getLeg()));
		} else {
			legData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
		}
		
		switch (event.getEventType()) {
			case EVENT_SUCCESS: {
				logger.debug(origLegCallId + " Received Success event "
						+ event.getProtocol());
			
				if(legData.get(LegDataAttributes.NP_ATI_RES) != null) {
					AnyTimeInterrogationResMap atires = 
							(AnyTimeInterrogationResMap) legData.get(LegDataAttributes.NP_ATI_RES);
					logger.info("===================================");
					logger.info("Obtained ATI response = "+atires);
					logger.info("===================================");
				} else 	if(legData.get(LegDataAttributes.NP_ATSI_RES) != null) {
					AnyTimeSubscriptionInterrogationResMap atsires = 
							(AnyTimeSubscriptionInterrogationResMap) legData.get(LegDataAttributes.NP_ATSI_RES);		
					logger.info("===================================");
					logger.info("Obtained ATSI response = "+atsires);
					logger.info("===================================");
				} else 	if(legData.get(LegDataAttributes.NP_ATM_RES) != null) {
					AnyTimeModificationResMap atmres = 
							(AnyTimeModificationResMap) legData.get(LegDataAttributes.NP_ATM_RES);	
					logger.info("===================================");
					logger.info("Obtained ATM response = "+atmres);
					logger.info("===================================");
				} else if(legData.get(LegDataAttributes.NP_SRI_RES) != null){
					SendRoutingInfoResMap srires = 
							(SendRoutingInfoResMap) legData.get(LegDataAttributes.NP_SRI_RES);
					logger.info("===================================");
					logger.info("Obtained SRI response = "+srires);
					logger.info("===================================");
				}
				break;
			}
				
			default: { 
				logger.info("Event received :" + event.getEventType()+ " Doing no action");
				break;
			}
		}
		
		return actionArr;
	}

	/**
	 * This Method processes CAP CS 2 events
	 * @param event
	 * 			It represents an instance of Event
	 * @param callData
	 * 			it represents an instance of CallData
	 * @return
	 */
	private Action[] processCapCS2Event(Event event, CallData callData) {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		Action[] actionArr = null;
		
		if (logger.isDebugEnabled()){
			logger.debug(origLegCallId + ":: Inside processEvent for Cap");
			logger.debug(origLegCallId + ":: Event is " + event);
		}

		switch (event.getEventType()) {
			case EVENT_INITIAL: {
				logger.debug(origLegCallId + " Received a new call for protocol "
					+ event.getProtocol());
		
				LegData leg1Data=(LegData)callData.get(CallDataAttribute.P_LEG1);
				PhoneNumber calledParty1 =(PhoneNumber)leg1Data.get(LegDataAttributes.P_CALLED_PARTY);
				String calledParty=calledParty1.getAddress();
				
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + " Received a new call for protocol "
						+ event.getProtocol()+ " with calledParty "+calledParty);
					logger.debug("Checking if HLR configuration is available, to be put in legData");
				}

				if (!isHlrInteractionReqd && hlrSccpAddr == null){
					logger.info("Reading the HLR configuration");
					readHlrConfiguration();
				}
				
				if (isHlrInteractionReqd == true && hlrSccpAddr != null) {
					leg1Data.set(LegDataAttributes.P_HLR_SUA, hlrSccpAddr);
					logger.info("setting hlr in leg data to "+(SccpUserAddress)leg1Data.get(LegDataAttributes.P_HLR_SUA));
				}else{
					logger.info("HLR Not configured, interaction not required"); 
				}
		
				if (calledParty != null && calledParty.equals("123")) {
				    actionArr = connectCapTerm(origLegCallId, callData,Action.SEND_MODE.END); 
				} else if (calledParty != null && calledParty.equals("124")){
					actionArr = connectCapTerm(origLegCallId, callData,Action.SEND_MODE.CONTINUE); 
				} else if (calledParty != null && calledParty.equals("125")){
					actionArr = dropCall(origLegCallId, callData, null,
							Protocol.CAPV2_SCF);
				} else if (calledParty != null && calledParty.equals("126")){
					actionArr = connectCapOrigIVR(origLegCallId, callData);
				} else if (calledParty != null && calledParty.equals("127")) {
					if (logger.isDebugEnabled()) {
						logger.debug("Triggering ATI request");
					}
					actionArr = anyTimeInterrogationReq(origLegCallId, callData);
				} else if (calledParty != null && calledParty.equals("128")) {
					if (logger.isDebugEnabled()) {
						logger.debug("Triggering ATSI request");
					}
					actionArr = anyTimeSubsInterrogationReq(origLegCallId, callData);
				} else if (calledParty != null && calledParty.equals("129")) {
					if (logger.isDebugEnabled()) {
						logger.debug("Triggering ATM request");
					}
					actionArr = anyTimeModificationReq(origLegCallId, callData);
				} else if (calledParty != null && calledParty.equals("130")) {
					if (logger.isDebugEnabled()) {
						logger.debug("Triggering SRI request");
					}
					actionArr = sendRoutingInfoReq(origLegCallId, callData);
				} else 	{
					  actionArr = connectCapTerm(origLegCallId, callData,Action.SEND_MODE.END); // it is for connect flow
				}
			break;
		}
		case EVENT_MS_SUCCESS: {
			logger.debug(origLegCallId + " Received MS success event  "
					+ event.getProtocol());
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
					Protocol.CAPV2_SCF);
			break;
		}
		default:
			break;
		}
		return actionArr;

	}

	private Action[] interrogateSubscriberData(String origLegCallId,
			CallData callData) {
		logger.debug(origLegCallId
				+ " connectCapTerm() Send initial INVITE to term and connect mPH to term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		/*
		 * Create LegData for term leg and set destination details on term
		 * legData
		 */
		Action action = new Action(Action.ActionType.ACTION_INTERROGATE);
		action.setProtocol(Protocol.MAP_SCF);

		action.setLeg(CallDataAttribute.P_LEG1.name());

		actionArr[0] = action;
		return actionArr;
	}
	
	
	
	
	

	/**
	 * This Method processes SIP events
	 * @param event
	 * 			It represents an instance of Event
	 * @param callData
	 * 			it represents an instance of CallData
	 */
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
							+ "Ann played to orig. Now disconnect Cap IVR connection ");

					return disconnectOrigIvr(origLegCallId, callData,
							Protocol.CAPV2_SCF);
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
	/**
	 * @param origLegCallId
	 * @param callData
	 * @return
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

	/**
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
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

	/**
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
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

	/**
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
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

	/**
	 * @param origLegCallId
	 * @param callData
	 * @param protocol
	 * @return
	 */
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

	/**
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
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

	/**
	 * @param origLegCallId
	 * @param callData
	 * @param causeCode
	 * @param protocol
	 * @return
	 */
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
	public Action[] connectCapTerm(String origLegCallId, CallData callData,
			Action.SEND_MODE sendMode) {
		logger.debug(origLegCallId
				+ " connectCapTerm() Send initial INVITE to term and connect mPH to term");
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
		action.setProtocol(Protocol.CAPV2_SCF);
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
		
		if (action.getSendMode() == Action.SEND_MODE.END) {
			
		//	byte[] bytes= hexStringToByteArray(MphTestAppSipServlet.fci);
			OriglegData.set(LegDataAttributes.P_FCI_BILLING_CHARACTERISTICS,MphTestAppSipServlet.fci);
					
		}

		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * When service want to send connect for IDP request. After receiving
	 * initial INVITE mPH will send EVENT_INITIAL to service 2. Service will
	 * return CONNECT_TERM 3. mPH will send CONNECT request to term
	 */
	public Action[] connectCapOrigIVR(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId
				+ " connectCapOrigIVR() ::Inside");
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
		action.setProtocol(Protocol.CAPV2_SCF);
		action.setConnectionMode(CONNECTIONMODE.ASSIST);
		action.setLeg(CallDataAttribute.P_LEG1.name());

		SccpUserAddress localAddr = Capv2ScfProtocolConfig.getInstance()
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
	
	public Action[] anyTimeInterrogationReq(String origLegCallId, CallData callData) {
		logger.info("anyTimeInterrogationReq: Entr");

		if(hlrSccpAddr == null) {
			logger.error("Destination HLR Configuration not available, not sending ATI");
			return null;
		}
		
		logger.debug("Orig Call-Leg id = "+origLegCallId);
		
		Action[] actionArr = null;
		actionArr = new Action[1];
		
		Action action = new Action(Action.ActionType.ACTION_INTERROGATE);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		
		LegData origLegData = (LegData)callData.get(CallDataAttribute.valueOf(action.getLeg()));

		try {
			logger.info("Setting the ATI Argument");
			ImsiDataType imsi = new ImsiDataType();
			imsi.setMobileCountryCode("460");
			imsi.setMobileNetworkCode("00");
			imsi.setMsin("13511078690");
			SubscriberIdentityMap subId = new SubscriberIdentityMap(imsi);
			
			RequestedInfoMap reqInfo = new RequestedInfoMap();
			
			RequestedNodesMap reqNodes = new RequestedNodesMap();
			reqNodes.enableRequestedNodeAtIndex(RequestedNodesMapEnum.MME);
			
			reqInfo.setDomainType(DomainTypeMapEnum.PS_DOMAIN);
			reqInfo.setRequestedNodes(reqNodes);
			
			ISDNAddressStringMap gsmScfAddr = new ISDNAddressStringMap();
			gsmScfAddr.setAddressDigits("9230001559");
			gsmScfAddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmScfAddr.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmScfAddr.setNumberPlan(NumberPlanMapEnum.DATA_NUMBERING_PLAN);
			
			AnyTimeInterrogationArgMap atiArg = new AnyTimeInterrogationArgMap(
					subId, reqInfo, gsmScfAddr);
			
			
			origLegData.set(LegDataAttributes.NP_ATI_REQ, atiArg);

			actionArr[0] = action;
		}
		catch(Exception ex) {
			logger.error("Error encoding ATI request: "+ex);
		}
		
		logger.info("anyTimeInterrogationReq: Exit");
		
		return actionArr;
	}
	
	public Action[] anyTimeSubsInterrogationReq(String origLevCallId, CallData callData) {
		logger.info("anyTimeSubsInterrogationReq: Entr");

		if(hlrSccpAddr == null) {
			logger.error("Destination HLR Configuration not available, not sending ATI");
			return null;
		}

		Action[] actionArr = null;
		actionArr = new Action[1];
		
		Action action = new Action(Action.ActionType.ACTION_INTERROGATE_SUBS);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		
		LegData origLegData = (LegData)callData.get(CallDataAttribute.valueOf(action.getLeg()));

		try {
			// Subscriber Identity User object
			ImsiDataType imsi = new ImsiDataType();
			imsi.setMobileCountryCode("1227");
			imsi.setMobileNetworkCode("28");
			imsi.setMsin("0982321332");
			System.out.println("imsi  = "+imsi);
			SubscriberIdentityMap subId = new SubscriberIdentityMap(imsi);

			// Subscription Info User object
			RequestedNodesMap nodes = new RequestedNodesMap();
			nodes.enableRequestedNodeAtIndex(RequestedNodesMapEnum.SGSN);
			
			RequestedSubscriptionInfoMap reqInfo = new RequestedSubscriptionInfoMap();
			
			SsForBSCodeMap reqSsInfo = new SsForBSCodeMap(
					new SsCodeMap(SupplementaryServicesMapEnum.CALL_SESSION_RELATED_SS));
			
			reqSsInfo.setBasicServiceCode(new BasicServiceCodeMap(BearerServiceCodeMapEnum.ALLDATACIRCUITSYNCHRONOUS));
		
			reqInfo.setSsforBSCode(reqSsInfo);
			
			reqInfo.setReqCAMELSubsInfo(RequestedCAMELSubscriptionInfoMapEnum.D_CSI);
			reqInfo.setAddlReqCAMELSubsInfo(AddlRequestedCAMELSubscriptionInfoMapEnum.MT_SMS_CSI);

			// GSM ScfAddr User object
			ISDNAddressStringMap gsmScfAddr = new ISDNAddressStringMap();
			gsmScfAddr.setAddressDigits("3214793122");
			gsmScfAddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmScfAddr.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmScfAddr.setNumberPlan(NumberPlanMapEnum.ISDN_TELEPHONY_NUMBERING);
			
			AnyTimeSubscriptionInterrogationArgMap atsi = 
					new AnyTimeSubscriptionInterrogationArgMap(
					subId, reqInfo, gsmScfAddr);
			
			
			origLegData.set(LegDataAttributes.NP_ATSI_REQ, atsi);
			callData.set(CallDataAttribute.P_LEG1, origLegData);
			actionArr[0] = action;
		}
		catch(Exception ex) {
			logger.error("Error encoding ATSI request: "+ex);
		}

		logger.info("anyTimeSubsInterrogationReq: Exit");
		return actionArr;		
	}

	public Action[] anyTimeModificationReq(String origLevCallId, CallData callData) {
		logger.info("anyTimeModificationReq: Entr");
		if(hlrSccpAddr == null) {
			logger.error("Destination HLR Configuration not available, not sending ATI");
			return null;
		}
		
		Action[] actionArr = null;
		actionArr = new Action[1];
		
		Action action = new Action(Action.ActionType.ACTION_MODIFY_INFO);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		actionArr[0] = action;
		
		LegData origLegData = (LegData)callData.get(CallDataAttribute.valueOf(action.getLeg()));

		try{
			ImsiDataType imsi = new ImsiDataType();
			imsi.setMobileCountryCode("460");
			imsi.setMobileNetworkCode("00");
			imsi.setMsin("13511078690");
			SubscriberIdentityMap subId = new SubscriberIdentityMap(imsi);
	
			
			ISDNAddressStringMap gsmScfAddr = new ISDNAddressStringMap();
			gsmScfAddr.setAddressDigits("9230001559"); // check for odd digits
			gsmScfAddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmScfAddr.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmScfAddr.setNumberPlan(NumberPlanMapEnum.DATA_NUMBERING_PLAN);
			
			AnyTimeModificationArgMap atmArg = 
					new AnyTimeModificationArgMap(subId, gsmScfAddr);
			
			
			origLegData.set(LegDataAttributes.NP_ATM_REQ, atmArg);
			callData.set(CallDataAttribute.P_LEG1, origLegData);
			actionArr[0] = action;
		}
		catch(Exception ex) {
			logger.error("Error encoding ATM Request: "+ex);
		}

		logger.info("anyTimeModificationReq: Exit");
		return actionArr;		
	}

	public Action[] sendRoutingInfoReq(String origLevCallId, CallData callData) {
		logger.info("sendRoutingInfoReq: Entr");

		if(hlrSccpAddr == null) {
			logger.error("Destination HLR Configuration not available, not sending ATI");
			return null;
		}
		
		Action[] actionArr = null;
		actionArr = new Action[1];
		
		Action action = new Action(Action.ActionType.ACTION_SEND_ROUTING_INFO);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());

		LegData origLegData = (LegData)callData.get(CallDataAttribute.valueOf(action.getLeg()));

		try {
			ISDNAddressStringMap msisdn = new ISDNAddressStringMap();
			msisdn.setAddressDigits("9230001559");  // check for odd digits
			msisdn.setExtention(ExtentionMapEnum.NO_EXTENTION);
			msisdn.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			msisdn.setNumberPlan(NumberPlanMapEnum.DATA_NUMBERING_PLAN);
			
			
			// gsmorgsmscf address
			ISDNAddressStringMap gsmscfaddr = new ISDNAddressStringMap();
			gsmscfaddr.setAddressDigits("5234371559");  // check for odd digits
			gsmscfaddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmscfaddr.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmscfaddr.setNumberPlan(NumberPlanMapEnum.DATA_NUMBERING_PLAN);
			
			
			SendRoutingInfoArgMap sriArg = 
					new SendRoutingInfoArgMap(msisdn, InterrogationTypeEnumMap.BASIC_CALL, gsmscfaddr);
			
			
			origLegData.set(LegDataAttributes.NP_SRI_REQ, sriArg);
			callData.set(CallDataAttribute.P_LEG1, origLegData);
			actionArr[0] = action;

		}
		catch(Exception ex) {
			logger.error("Error encoding IMSI: "+ex);
		}
		logger.info("sendRoutingInfoReq: Exit");
		return actionArr;		
	}
	
	private void setCorrelationId(CallData callData) {
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
	
	private static void readHlrConfiguration(){
		String testAppProFile = System.getProperty("ase.home")
				+ "/conf/mphtestapp.properties";

		Properties p = new Properties();
		try {
			p.load(new FileInputStream(new File(testAppProFile)));
		} catch (FileNotFoundException e) {
			logger.error(testAppProFile + " Not found");
		} catch (IOException e) {
			logger.error(testAppProFile + " IOException");
		}
		
		String hlrSsn = "";
		String hlrSpc = "";

		if (p.getProperty(HLR_SPC) != null) {
			hlrSpc = p.getProperty(HLR_SPC);
		}
		
		if(p.getProperty(HLR_SSN) != null) {
			hlrSsn = p.getProperty(HLR_SSN);
		}
		
		
		if(!hlrSpc.isEmpty()) {
			String[] tmp = hlrSpc.split("-");
			SignalingPointCode spc = null;
			
			if (tmp.length == 3) {
				spc = new SignalingPointCode(Integer.parseInt(tmp[2]), Integer
					.parseInt(tmp[1]), Integer.parseInt(tmp[0]));
			}
			
			if( spc != null && !hlrSsn.isEmpty()) {
				hlrSccpAddr = new SccpUserAddress(new 
						SubSystemAddress(spc, (short)Integer.parseInt(hlrSsn)));
				hlrSccpAddr.setProtocolVariant(1);
				logger.info("HLR Configuration = "+hlrSccpAddr);
				isHlrInteractionReqd = true;
			}
		}
		
	}
};
