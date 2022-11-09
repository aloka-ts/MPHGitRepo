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

package com.agnity.ph.capv2scf;

import com.agnity.mphdata.common.*;
import jain.protocol.ss7.tcap.TcapConstants;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;

import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.PhConstants;
import com.genband.tcap.provider.TcapSession;

/**
 * This class is used to define the Functional state Machine of CAP protocol.it
 * maintains the mapping of various CAP incoming messages with the possible
 * CAP protocol states.so when an CAP message is received from network it
 * validates it against current state of call. if the event is not valid for the
 * current state ,the validation fails and the error response is sent by protocol handler.
 *
 */
public class Capv2ScfProtocolFSMHandler {

	private static Logger logger = Logger.getLogger(Capv2ScfProtocolFSMHandler.class); 

	/**
	 * This map will store mapping of call state to possible events in state 
	 */
	private static Map<CapV2CallStates, Set<Capv2ScfProtocolEvent>> capValidEventsMap = new HashMap<CapV2CallStates, Set<Capv2ScfProtocolEvent>>();

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

		/**
		 * @param num
		 * @return
		 */
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

	/*
	 * Create a Map which specifies what all events are valid in which state. 
	 * INIT state - BEGIN and IDP are expected
	 * CONN_IN_PROGRESS - 
	 * CONNECTED - TERMINATED -
	 */
	static {
		LinkedHashSet<Capv2ScfProtocolEvent> initCAPEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		initCAPEvent.add(Capv2ScfProtocolEvent.BEGIN);
		initCAPEvent.add(Capv2ScfProtocolEvent.IDP);
		capValidEventsMap.put(CapV2CallStates.INIT, initCAPEvent);

		LinkedHashSet<Capv2ScfProtocolEvent> connInProgCapEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		connInProgCapEvent.add(Capv2ScfProtocolEvent.CONTINUE);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.AT_TIMEOUT);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.END);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.UABORT);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.PABORT);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.NOTICE);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.ERB);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.ERB_ANS);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.ERB_BUSY);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.ERB_NOANS);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.ERB_ABANDON);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.ERB_DISCONNECT);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.ERB_ROUTESELECTFAILURE);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.RETURNERROR);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.RETURNRESULT);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.UREJECT);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.SRR);
		connInProgCapEvent.add(Capv2ScfProtocolEvent.RRBCSM_TIMEOUT);
		capValidEventsMap.put(CapV2CallStates.TERM_CONNECT_IN_PROGRESS, connInProgCapEvent);
		capValidEventsMap.put(CapV2CallStates.USER_INTREACTION_IN_PROGRESS, connInProgCapEvent);

		LinkedHashSet<Capv2ScfProtocolEvent> connectedCapEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		connectedCapEvent.add(Capv2ScfProtocolEvent.CONTINUE);
		connectedCapEvent.add(Capv2ScfProtocolEvent.AT_TIMEOUT);
		connectedCapEvent.add(Capv2ScfProtocolEvent.END);
		connectedCapEvent.add(Capv2ScfProtocolEvent.UABORT);
		connectedCapEvent.add(Capv2ScfProtocolEvent.PABORT);
		connectedCapEvent.add(Capv2ScfProtocolEvent.NOTICE);
		connectedCapEvent.add(Capv2ScfProtocolEvent.CDR_TIMEOUT);
		connectedCapEvent.add(Capv2ScfProtocolEvent.ERB);
		connectedCapEvent.add(Capv2ScfProtocolEvent.ERB_DISCONNECT);
		connectedCapEvent.add(Capv2ScfProtocolEvent.RETURNERROR);
		connectedCapEvent.add(Capv2ScfProtocolEvent.RETURNRESULT);
		connectedCapEvent.add(Capv2ScfProtocolEvent.UREJECT);
		connectedCapEvent.add(Capv2ScfProtocolEvent.ACR);
		connectedCapEvent.add(Capv2ScfProtocolEvent.ENC);
		capValidEventsMap.put(CapV2CallStates.TERM_CONNECTED, connectedCapEvent);
		capValidEventsMap.put(CapV2CallStates.AC_SENT, connectedCapEvent);


		LinkedHashSet<Capv2ScfProtocolEvent> assistInapEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		assistInapEvent.add(Capv2ScfProtocolEvent.CONTINUE);
		assistInapEvent.add(Capv2ScfProtocolEvent.AT_TIMEOUT);
		assistInapEvent.add(Capv2ScfProtocolEvent.END);
		assistInapEvent.add(Capv2ScfProtocolEvent.ERB);
		assistInapEvent.add(Capv2ScfProtocolEvent.UABORT);
		assistInapEvent.add(Capv2ScfProtocolEvent.PABORT);
		assistInapEvent.add(Capv2ScfProtocolEvent.NOTICE);
		assistInapEvent.add(Capv2ScfProtocolEvent.RETURNERROR);
		assistInapEvent.add(Capv2ScfProtocolEvent.RETURNRESULT);
		assistInapEvent.add(Capv2ScfProtocolEvent.CORRELATION_TIMEOUT);
		assistInapEvent.add(Capv2ScfProtocolEvent.UREJECT);
		capValidEventsMap.put(CapV2CallStates.ASSIST, assistInapEvent);

		LinkedHashSet<Capv2ScfProtocolEvent> terminatedCapEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		terminatedCapEvent.add(Capv2ScfProtocolEvent.ACR);
		terminatedCapEvent.add(Capv2ScfProtocolEvent.END);
		terminatedCapEvent.add(Capv2ScfProtocolEvent.ENC);
		capValidEventsMap
				.put(CapV2CallStates.TERMINATED, terminatedCapEvent);

		LinkedHashSet<Capv2ScfProtocolEvent> serviceCapEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		serviceCapEvent.add(Capv2ScfProtocolEvent.CONTINUE);
		serviceCapEvent.add(Capv2ScfProtocolEvent.AT_TIMEOUT);
		serviceCapEvent.add(Capv2ScfProtocolEvent.END);
		serviceCapEvent.add(Capv2ScfProtocolEvent.ERB);
		serviceCapEvent.add(Capv2ScfProtocolEvent.UABORT);
		serviceCapEvent.add(Capv2ScfProtocolEvent.PABORT);
		serviceCapEvent.add(Capv2ScfProtocolEvent.NOTICE);
		capValidEventsMap
				.put(CapV2CallStates.SERVICE_LOGIC, serviceCapEvent);

		LinkedHashSet<Capv2ScfProtocolEvent> terminationInProgresCapEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		terminationInProgresCapEvent.add(Capv2ScfProtocolEvent.CONTINUE);
		terminationInProgresCapEvent.add(Capv2ScfProtocolEvent.END);
		terminationInProgresCapEvent.add(Capv2ScfProtocolEvent.AT_TIMEOUT);
		terminationInProgresCapEvent.add(Capv2ScfProtocolEvent.ACR);
		terminationInProgresCapEvent.add(Capv2ScfProtocolEvent.ENC);
		capValidEventsMap.put(CapV2CallStates.TERMINATION_IN_PROGRESS,
				terminationInProgresCapEvent);
		
		
		LinkedHashSet<Capv2ScfProtocolEvent> callHBInProgresCapEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		callHBInProgresCapEvent.add(Capv2ScfProtocolEvent.PABORT);
		callHBInProgresCapEvent.add(Capv2ScfProtocolEvent.CONTINUE);
		callHBInProgresCapEvent.add(Capv2ScfProtocolEvent.END);
		callHBInProgresCapEvent.add(Capv2ScfProtocolEvent.AT_TIMEOUT);
		capValidEventsMap.put(CapV2CallStates.CALL_HB_IN_PROGRESS,
				callHBInProgresCapEvent);
		
		LinkedHashSet<Capv2ScfProtocolEvent> termConnectedCapCapEvent = new LinkedHashSet<Capv2ScfProtocolEvent>();
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.PABORT);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.UABORT);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.CONTINUE);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.END);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.NOTICE);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.UREJECT);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.RETURNERROR);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.RETURNRESULT);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.AT_TIMEOUT);
		termConnectedCapCapEvent.add(Capv2ScfProtocolEvent.ERB);
		capValidEventsMap.put(CapV2CallStates.TERM_CONNECTED_ACR,
				termConnectedCapCapEvent);
	}

	/**
	 * This method validates the CAP event against current state in call data.
	 * 
	 * @param capProtocolEvent
	 *            represents the instance of CapEvent
	 * @param callData
	 *            represents the instance of TcapSession
	 * @return true if valid event. false in case of error or invalid event
	 */

	static boolean validateFSMState(Capv2ScfProtocolEvent capProtocolEvent, TcapSession tcapSession) {
		if (logger.isDebugEnabled()){
			logger.debug("[PH] validateFSMState Enter");
		}
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogueId    = tcapSession.getDialogueId();

		if (callData == null) {
			
			if (logger.isDebugEnabled()){
				logger.debug(dialogueId + " [PH]::Call data is null return false ");
			}

			/*
			 *  create dummy call data for error handling
			 */
			callData = new CallData();
			
			callData.set(CallDataAttribute.P_PROTOCOL, Protocol.CAPV2_SCF);
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
			logger.debug(dialogueId + "::[PH] Inside validateFSMState for event["+ capProtocolEvent + "]");
		}

		/*
		 *  fetch current call state
		 */
		CapV2CallStates currCallState = (CapV2CallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] curr call state is ["+ currCallState + "]");
		}

		/*
		 *  check if valid message
		 */
		/*
		 *  For FT call and Non timeout events mark event as invalid
		 */
		if ((PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.NP_FT_CALL))
				&& (currCallState != CapV2CallStates.TERM_CONNECTED)
				&& (capProtocolEvent != Capv2ScfProtocolEvent.AT_TIMEOUT
				&& capProtocolEvent != Capv2ScfProtocolEvent.CORRELATION_TIMEOUT && capProtocolEvent != Capv2ScfProtocolEvent.CDR_TIMEOUT))) {
			if (logger.isInfoEnabled()) {
				logger.info(dialogueId + "::[PH]  Event received after CAS failover");
				logger.info(dialogueId + "::[PH]  Call is not in connected state invalid state after FT; we should drop a transient call");
			}
			
			/*
			 * for this scenario set failed call set attempted call ind for this
			 * scenario
			 */
			callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
			return false;
		}

		/*
		 * fetch valid events for current call state
		 */
		Set<Capv2ScfProtocolEvent> validEvents = capValidEventsMap.get(currCallState);

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
		if (validEvents.contains(capProtocolEvent)) {
			if (logger.isDebugEnabled()){
				logger.debug(dialogueId+ "::[PH] current event is allowed for given call state; checking event specific preconditions::");
			}	
			boolean status = true;
			
			/*
			 * event specific checks.. like IDP in begin erb as per armed events.Currently ERB is not coming as individual events so individual ERBs validation
			 * code will not be invoked
			 * 
			 */
			switch (capProtocolEvent) {
			case IDP: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] IDP not part of begin dialouge. Last dialog recived::["	+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case ERB_ROUTESELECTFAILURE: {

	             /*
	              * Here null should be replaced with legid for which ERB is received
	              */
				Set<Action.ERB_TYPE> erbSet = Capv2CS1ScfProtocolUtil.getErbSetByApplication(callData,null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ROUTESELECTFAILURE))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set doesn't contain ERB_ROUTESELECTFAILURE");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE,PhConstants.TRUE);
				}
				break;

			}
			case ERB_BUSY: {

				Set<Action.ERB_TYPE> erbSet = Capv2CS1ScfProtocolUtil.getErbSetByApplication(callData,null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_BUSY))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set doesn't contain ERB_BUSY");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE,PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
					case TcapConstants.PRIMITIVE_END:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ "::[PH] ERB_BUSY not part of Continue/END dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
						break;
					}// end default
					}// end switch
				}// end else
				break;
			}
			case ERB_NOANS: {

				Set<Action.ERB_TYPE> erbSet = Capv2CS1ScfProtocolUtil.getErbSetByApplication(callData,null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_NO_ANSWER))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set doesn't contain ERB_NOANS");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE,PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
					case TcapConstants.PRIMITIVE_END:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ "::[PH] ERB_NOANS not part of Continue/END dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
						break;
					}// end default
					}// end switch
				}// end else status
				break;

			}
			case ERB_ANS: {
				Set<Action.ERB_TYPE> erbSet = Capv2CS1ScfProtocolUtil.getErbSetByApplication(callData,null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ANSWER))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set doesn't contain ERB_ANS");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE,PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ "::[PH] ERB_ANS not part of Continue dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
						break;
					}// end default
					}// end switch
				}// end else status
				break;

			}
			case ERB_DISCONNECT: {
				Set<Action.ERB_TYPE> erbSet = Capv2CS1ScfProtocolUtil.getErbSetByApplication(callData,null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_DISCONNECT))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set doesn't contain ERB_DISCONNECT");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE,PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
					case TcapConstants.PRIMITIVE_END:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId+ "::[PH] ERB_DISCONNECT not part of Continue/END dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
						break;
					}// end default
					}// end switch
				}// end else status
				break;

			}
			case ERB_ABANDON: {
				Set<Action.ERB_TYPE> erbSet = Capv2CS1ScfProtocolUtil.getErbSetByApplication(callData,null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ABANDON))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] Erb set doesn't contain ERB_OABANDON");
					}
					status = false;
				}
				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE,PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
					case TcapConstants.PRIMITIVE_END:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId+ "::[PH] ERB_ABANDON not part of Continue/END dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
						break;
					}// end default
					}// end switch
				}// end else status
				break;
			}
			case ACR: {

				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] ACR not part of Continue dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case SRR: {

				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] SRR not part of Continue dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			
			case ENC: {

				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId+ "::[PH] ENC not part of Continue dialouge. Last dialog recived::["
								+ rxDialogType + "]");
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
