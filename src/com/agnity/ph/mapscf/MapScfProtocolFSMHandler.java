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
import com.agnity.mphdata.common.MapCallStates;
import jain.protocol.ss7.tcap.TcapConstants;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.genband.tcap.provider.TcapSession;

/**
 * This class is used to define the Functional state Machine of MAP  protocol.it
 * maintains the mapping of various MAP incoming messages with the possible
 * MAP protocol states.so when a MAP message is received from network it
 * validates it against current state of call. if the event is not valid for the
 * current state ,the validation fails and the error response is sent by protocol handler.
 * 
 */
public class MapScfProtocolFSMHandler {

	private static Logger logger = Logger.getLogger(MapScfProtocolFSMHandler.class);

	/**
	 * This map will store mapping of call state to possible events in state
	 */
	private static Map<MapCallStates, Set<MapScfProtocolEvent>> mapValidEventsMap = new HashMap<MapCallStates, Set<MapScfProtocolEvent>>();

	public static enum AbortInfoEnum {

		NO_REASON_GIVEN(1), APPLICATION_TIMER_EXPIRED(2), PROTOCOL_PROHIBITED_SIGNAL_RECIEVED(
				3), ABNORMAL_PROCESSING(4), CONGESTION(5), AC_NEGOTIATION_FAILED(
						6), UNRECOGNIZED_EXTENSION_PARAMETER(7);

		private AbortInfoEnum(int i) {
			this.code = i;
		}

		private int code;

		public int getCode() {
			return code;
		}

		public static AbortInfoEnum fromInt(int num) {
			AbortInfoEnum abortInfo = NO_REASON_GIVEN;
			switch (num) {
			case 1: {
				abortInfo = NO_REASON_GIVEN;
				break;
			}
			case 2: {
				abortInfo = APPLICATION_TIMER_EXPIRED;
				break;
			}
			case 3: {
				abortInfo = PROTOCOL_PROHIBITED_SIGNAL_RECIEVED;
				break;
			}
			case 4: {
				abortInfo = ABNORMAL_PROCESSING;
				break;
			}
			case 5: {
				abortInfo = CONGESTION;
				break;
			}
			case 6: {
				abortInfo = AC_NEGOTIATION_FAILED;
				break;
			}
			case 7: {
				abortInfo = UNRECOGNIZED_EXTENSION_PARAMETER;
				break;
			}
			default: {
				abortInfo = NO_REASON_GIVEN;
				break;
			}
			}// @End Switch
			return abortInfo;
		}
	}

	/**
	 * Create a Map which specifies what all events are valid in which state. 
	 */
	/**
	 * Create a Map which specifies what all events are valid in which state. 
	 * INIT state - BEGIN and IDP are expected
	 * CONN_IN_PROGRESS - 
	 * CONNECTED - TERMINATED -
	 */
	static {
		LinkedHashSet<MapScfProtocolEvent> mapEvent = new LinkedHashSet<MapScfProtocolEvent>();
		mapEvent.add(MapScfProtocolEvent.BEGIN);
		mapEvent.add(MapScfProtocolEvent.NSDM);
		mapEvent.add(MapScfProtocolEvent.SSIN);
		mapValidEventsMap.put(MapCallStates.INIT, mapEvent);

		LinkedHashSet<MapScfProtocolEvent> opInProgMapEvent = new LinkedHashSet<MapScfProtocolEvent>();
		
		opInProgMapEvent.add(MapScfProtocolEvent.NOTICE);
		
		opInProgMapEvent.add(MapScfProtocolEvent.RETURNERROR);
		opInProgMapEvent.add(MapScfProtocolEvent.RETURNRESULT);
		opInProgMapEvent.add(MapScfProtocolEvent.UREJECT);
		opInProgMapEvent.add(MapScfProtocolEvent.END);	
		opInProgMapEvent.add(MapScfProtocolEvent.NSDM);
		opInProgMapEvent.add(MapScfProtocolEvent.SSIN);
		mapValidEventsMap.put(MapCallStates.OPERATION_IN_PROGRESS, opInProgMapEvent);

	}

	/**
	 * This method validates the MAP event against current state in call data.
	 * 
	 * @param mapProtocolEvent
	 *            represents the instance of MapScfProtocolEvent
	 * @param callData
	 *            represents the instance of TcapSession
	 * @return true if valid event. false in case of error or invalid event
	 */
	static boolean validateFSMState(MapScfProtocolEvent mapProtocolEvent, TcapSession tcapSession) {
		
			if (logger.isDebugEnabled()){
				logger.debug("[PH] validateFSMState Enter");
			}
			CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
			int dialogueId    = tcapSession.getDialogueId();

			if (callData == null) {
				
				if (logger.isDebugEnabled()){
					logger.debug(dialogueId + " [PH]::Call data is null return false ");
				}

				/*
				 *  create dummy call data for error handling
				 */
				callData = new CallData();
				
				callData.set(CallDataAttribute.P_PROTOCOL, Protocol.MAP_SCF);
				callData.set(CallDataAttribute.P_DIALOG_ID, dialogueId);
				callData.set(CallDataAttribute.NP_FT_CALL, true);
				callData.set(CallDataAttribute.P_ATTEMPTED_IND, 4);

				tcapSession.setAttribute(CallData.CALL_DATA, callData);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId	+ " [PH]:: New call data created and set in TCAP session");
				}

				return false;
			}

			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			if (legData == null) {
				logger.error("[PH] ::validateFSMState - LegData is NULL for Dialogue Id :" +  tcapSession.getDialogueId());
				return false;
			}

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "::[PH] Inside validateFSMState for event["+ mapProtocolEvent + "]");
			}

			/*
			 *  fetch current call state
			 */
			MapCallStates currCallState = (MapCallStates) legData.get(LegDataAttributes.P_LEG_MAP_STATE);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "::[PH] curr call state is ["+ currCallState + "]");
			}


			/*
			 * fetch valid events for current call state
			 */
			Set<MapScfProtocolEvent> validEvents = mapValidEventsMap.get(currCallState);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId	+ "::[PH] Possible events for curr call state are ["+ validEvents + "]");
			}

			/*
			 *  fetch dialogue primitive
			 */
			int rxDialogType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);

			/*
			 *  match current event to valid event list
			 */
			if (validEvents.contains(mapProtocolEvent)) {
				if (logger.isDebugEnabled()){
					logger.debug(dialogueId+ "::[PH] current event is allowed for given call state; checking event specific preconditions::");
				}	
				boolean status = true;
				
				/*
				 * event specific checks.. like IDP in begin erb as per armed events.Currently ERB is not coming as individual events so individual ERBs validation
				 * code will not be invoked
				 * 
				 */
				switch (mapProtocolEvent) {
				case NSDM: {
					if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId+ "::[PH] NSDM not part of begin dialouge. Last dialog recived::["	+ rxDialogType + "]");
						}
						status = false;
					}
					break;
				}
				case END: {

					if (rxDialogType != TcapConstants.PRIMITIVE_END) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId+ "::[PH] END is not part of END dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
					}
					break;
				}

				default:
					/*
					 *  status should remain as true;
					 */
					status = true;
					break;

				}
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId+ "::[PH] Return with validation status::[" + status + "]");
				}

				return status;
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId+ "::[PH] event is NOT allowed for cuee call state return false");
				}
				return false;
			}

		}
	}

