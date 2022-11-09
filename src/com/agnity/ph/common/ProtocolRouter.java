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
package com.agnity.ph.common;

import static com.agnity.mphdata.common.Action.ActionType.ACTION_CONNECT;
import static com.agnity.mphdata.common.Action.ActionType.ACTION_DISCONNECT;
import static com.agnity.mphdata.common.Action.ActionType.ACTION_END_CALL;
import static com.agnity.mphdata.common.Action.ActionType.ACTION_PROCESS_NEXT;
import static com.agnity.mphdata.common.Action.ActionType.ACTION_REDIRECT;
import static com.agnity.mphdata.common.InapCallStates.NULL;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.InapCallStates;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.ph.ainscf.acg.ACGOveroadManager;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;

/**
 * This class is used by all the protocol handlers to send a protocol specific event to the service.
 * it also invokes a handler for a specific protocol for executing a action returned by service.
 */
public class ProtocolRouter {

	private static final ProtocolRouter INSTANCE = new ProtocolRouter();
	private static Logger logger = Logger.getLogger(ProtocolRouter.class);

	private ProtocolRouter() {

	}

	public static ProtocolRouter getInstance() {
		return INSTANCE;
	}

	/**
	 * This method process a event and calls executeAction of a specific protocol handler
	 * for the protocol provided by service
	 *
	 * @param event
	 * @param callData
	 * @param serviceHandler
	 * @throws Exception
	 */
	public void execute(Event event, CallData callData, ServiceInterface serviceHandler)
			throws Exception {

		String origLegId = "";

		if (callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID) != null) {
			origLegId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(origLegId+":: Execute event:" + event);
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		if (event.getEventType() == Event.EventType.EVENT_INITIAL && event.getProtocol() == Protocol.AIN_SCF) {
			
			if (logger.isDebugEnabled()) {
				logger.debug(origLegId+":: check overload condition for AIN application:");
			}
			
			//String calledParty = null;
			//PhoneNumber calledPartyPhoneNum = (PhoneNumber)legData.get(LegDataAttributes.P_CALLED_PARTY);	
			//if(calledPartyPhoneNum != null){
			//	calledParty = calledPartyPhoneNum.getAddress();
			//}

			//boolean isoverload = ACGOveroadManager
			//		.getInstance()
			//		.updateAndCheckOverLoadLevel(
			//				(String) callData.get(CallDataAttribute.SERVICE_ID),
			//				calledParty);
			//if (isoverload) {
			//		legData.set(LegDataAttributes.SEND_ACG, PhConstants.TRUE);
			//}
		}

		Action[] actions = serviceHandler.processEvent(event, callData);
		//Setting up the whole action array in call data
		callData.set(CallDataAttribute.SERVICE_REQUESTED_ACTION_ARRAY, actions);

		if (logger.isDebugEnabled()) {
			logger.debug(origLegId + ":: Returned actions are " + ArrayUtils.toString(actions));
		}

		if (null != actions) {

			for (Action action : actions) {

				if (action.getProtocol() != Protocol.HTTP && event.getProtocol() != Protocol.HTTP) {
					transformProtocol(callData, action, event);
				}else{
					
					if(event.getProtocol() == Protocol.HTTP){
						if(action.getActionType()==Action.ActionType.ACTION_CONNECT){
							
							if (logger.isDebugEnabled()) {
								logger.debug(origLegId + ":: set protocol to SIP: ");
							}
							action.setProtocol(Protocol.SIP);
						}else if(action.getProtocol() ==null){
							if (logger.isDebugEnabled()) {
								logger.debug(origLegId + ":: Setting protocol http as protocol is null: ");
							}
							action.setProtocol(Protocol.HTTP);
						}else {
							if (logger.isDebugEnabled()) {
								logger.debug(origLegId + ":: donot tranform http protocol: ");
							}
						}
					}
					
				}
				ProtocolHandler stateHandler = ProtocolHandlerFactory.getProtocolHandler(action.getProtocol());
				if (logger.isDebugEnabled()) {
					logger.debug(origLegId + ":: Executing action: " + action);
				}
				stateHandler.executeAction(callData, action);
			}
		}
	}

	private static void transformProtocol(CallData callData, Action action, Event event) {
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		String origLegId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		//Session protocol is the SS7 protocol for the call, set on first Dialog-Ind event.
		Protocol sessionProtocol = (Protocol) callData.get(CallDataAttribute.P_PROTOCOL);
		InapCallStates ss7CallState = NULL;
		AinCallStates ainCallStates = AinCallStates.NULL;
		logger.info(origLegId + ":: Input protocol action: " + action);

		if (null != legData) {

			if(event.getProtocol() == Protocol.ITUINAPCS1_SCF ||
					event.getProtocol() == Protocol.ITUINAPCS2_SCF || sessionProtocol == Protocol.ITUINAPCS1_SCF || sessionProtocol == Protocol.ITUINAPCS2_SCF){
				ss7CallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
				action.setProtocol(getUpdatedProtocol(origLegId, ss7CallState, action, event, sessionProtocol));

			}else if(event.getProtocol() == Protocol.AIN_SCF || sessionProtocol == Protocol.AIN_SCF){
				ainCallStates = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
				action.setProtocol(getUpdatedProtocol(origLegId, ainCallStates, action, event, sessionProtocol));
			}else{
				action.setProtocol(getUpdatedProtocol(origLegId, ss7CallState, action, event, sessionProtocol));
			}

		}


		logger.info(origLegId + ":: Returning with transformed action: " + action);
	}

	/**
	 * This changes the protocol of action to appropriate protocol. Mainly in case of Assist flows,
	 * protocol of actions needs to be changed on action requested by service.
	 *
	 * @param inapCallState   Current INAP call State
	 * @param action          Action whose protocol needs to be updated
	 * @param event
	 * @param sessionProtocol SS7 protocol set
	 * @return Updated protocol
	 */
	private static Protocol getUpdatedProtocol(String origLegId, InapCallStates inapCallState, Action action, Event event, Protocol sessionProtocol) {
		logger.debug(String.format(origLegId + ":: Current :" +
				"INAP SS7 call state: [%s], " +
				"ActionType: [%s], " +
				"Event Protocol: [%s], " +
				"Session Protocol: [%s]",
				inapCallState,
				action.getActionType(),
				event.getProtocol(),
				sessionProtocol));

		Protocol p = event.getProtocol();
		if(action.getActionType()==ActionType.ACTION_SEND_ENUM_QUERY || 
				action.getActionType()==ActionType.ACTION_SEND_ENUM_RESPONSE){

			logger.debug(origLegId + ":: Enum Setting protocol as " + action.getProtocol());
			return action.getProtocol();
		}
		
		if(action.getActionType() == ActionType.ACTION_SH_SEND_USER_DATA_REQUEST) {
			logger.debug(origLegId + ":: Diameter SH protocol as " + action.getProtocol());
			return action.getProtocol();
		}
		
		

		if (inapCallState == InapCallStates.MS_DISCONNECTED) {
			/*
            In case of Assist scenarios, if the MS has already been disconnected, then regardless of any action
            set the protocol as SS7 Protocol
			 */
			p = sessionProtocol;
			logger.debug(origLegId + ":: Setting the action protocol to SS7 since INAPCallState is MS_DISCONNECTED: " + p);
		} else if (inapCallState == InapCallStates.ASSIST || inapCallState == inapCallState.SERVICE_LOGIC) {
			//In case of Assist scenarios, below actions must happen on SS7 session
			if (action.getActionType() == ACTION_CONNECT
					|| action.getActionType() == ACTION_END_CALL
					|| action.getActionType() == ACTION_REDIRECT
					|| action.getActionType() == ACTION_DISCONNECT
					|| action.getActionType() == ACTION_PROCESS_NEXT
					|| action.getActionType() == ActionType.ACTION_DISCONNECT_MS 
					|| action.getActionType() == Action.ActionType.ACTION_STOP_TIMER) {
				p = sessionProtocol;

				if(p == Protocol.ENUM){
					p = event.getProtocol();
				}
				logger.debug(origLegId + ":: Call in Assist flow.. Setting the protocol to : " + p + 
						", sessionProtocol:"+ sessionProtocol);
			}
		} else if(action.getProtocol()==null){
			p = sessionProtocol;
			logger.debug(origLegId + ":: Falling back the protocol to : " + p);
		} else {
			p = action.getProtocol();
			logger.debug(origLegId + ":: Falling back the protocol to : " + p);
		}

		/*
         In case the session protocol was null, or all of the above checks failed, then set the protocol
         to one received in Event.
		 */
		if (null == p) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegId + ":: Could not find suitable protocol Setting the protocol as in event: " + event.getProtocol());
			}
			p = event.getProtocol();
		}

		logger.info(String.format(origLegId + ":: Current protocol set in action: [%s], Updating the protocol to: [%s]",
				action.getProtocol(), p));

		return p;
	}

	/**
	 * This changes the protocol of action to appropriate protocol. Mainly in case of Assist flows,
	 * protocol of actions needs to be changed on action requested by service.
	 *
	 * @param ainCallStates   Current AIN call State
	 * @param action          Action whose protocol needs to be updated
	 * @param event
	 * @param sessionProtocol SS7 protocol set
	 * @return Updated protocol
	 */
	private static Protocol getUpdatedProtocol(String origLegId, AinCallStates ainCallStates, Action action, Event event, Protocol sessionProtocol) {
		logger.debug(String.format(origLegId + ":: Current :" +
				"ANSI SS7 call state: [%s], " +
				"ActionType: [%s], " +
				"Event Protocol: [%s], " +
				"Session Protocol: [%s]",
				ainCallStates,
				action.getActionType(),
				event.getProtocol(),
				sessionProtocol));

		Protocol p = event.getProtocol();

		if(action.getActionType()==ActionType.ACTION_SEND_ENUM_QUERY || 
				action.getActionType()==ActionType.ACTION_SEND_ENUM_RESPONSE){

			logger.debug(origLegId + ":: Setting protocol as " + action.getProtocol());
			return action.getProtocol();
		}
		
		
		if(action.getActionType()==ActionType.ACTION_INVOKE_SVC_CHAINING) {
			logger.debug(origLegId + "::ACTION_INVOKE_SVC_CHAINING got  Setting protocol as " + action.getProtocol());
			return action.getProtocol();
		}

		if (ainCallStates == AinCallStates.ASSIST || ainCallStates == AinCallStates.SERVICE_LOGIC || ainCallStates==AinCallStates.GN_QUERY){ //||
				//ainCallStates == AinCallStates.HANDOFF ) {
			//In case of Assist scenarios, below actions must happen on SS7 session
			if (action.getActionType() == ACTION_CONNECT
					|| action.getActionType() == ACTION_END_CALL
					|| action.getActionType() == ACTION_REDIRECT
					|| action.getActionType() == ACTION_DISCONNECT
					|| action.getActionType() == ACTION_PROCESS_NEXT
					|| action.getActionType() == ActionType.ACTION_HOLD_CALL
					|| action.getActionType() == Action.ActionType.ACTION_STOP_TIMER) {
				p = sessionProtocol;

				// check if the protocol is invalid like ENUM then move to event protocol 
				if(p ==  Protocol.ENUM){
					p = event.getProtocol();
				}
				logger.debug(origLegId + "::  Setting the protocol to session protocol for this action: " + p +
						", SessionProtocol:" + sessionProtocol);
			}
		} else {
			if(p ==  Protocol.ENUM){
				p = sessionProtocol;
			}else{
				p = event.getProtocol();
			}
			logger.debug(origLegId + ":: Falling back the protocol to : " + p);
		}

		/*
         In case the session protocol was null, or all of the above checks failed, then set the protocol
         to one received in Event.
		 */
		if (null == p) {
			if (logger.isInfoEnabled()) {
				logger.info(origLegId + ":: Could not find suitable protocol Setting the protocol as in event: " + event.getProtocol());
			}
			p = event.getProtocol();
		}

		logger.info(String.format(origLegId + ":: Current protocol set in action: [%s], Updating the protocol to: [%s]",
				action.getProtocol(), p));

		return p;
	}
}
