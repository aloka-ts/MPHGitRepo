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

package com.agnity.ph.ainscf;

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
 * This file contains Finite state machine for AIN protocol. It contains map for
 * possible states and expected events (that are received from network) and
 * actions (those which are received from an application logic).
 * 
 * @author reeta
 *
 */
public class AinScfProtocolFSMHandler {

	private static Logger logger = Logger.getLogger(AinScfProtocolFSMHandler.class);

	/**
	 * This map will store mapping of call state to possible events in state
	 */
	private static Map<AinCallStates, Set<AinScfProtocolEvent>> ainValidEventsMap = new HashMap<AinCallStates, Set<AinScfProtocolEvent>>();

	public static enum AbortInfoEnum {

		NO_REASON_GIVEN(1), APPLICATION_TIMER_EXPIRED(2), PROTOCOL_PROHIBITED_SIGNAL_RECIEVED(3),
		ABNORMAL_PROCESSING(4), CONGESTION(5), AC_NEGOTIATION_FAILED(6), UNRECOGNIZED_EXTENSION_PARAMETER(7);

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
	 * Create a Map which specifies what all events are valid in which state. Map
	 * conatins State (INIT, TERM_CONNECT_IN_PROGRESS, TERM_CONNECTED,
	 * SERVICE_LOGIC, ASSIST, TERMINATION_IN_PROGRESS, MS_CONNECT_INPROGRESS,
	 * CALL_HB_IN_PROGRESS, USER_INTREACTION_IN_PROGRESS), Expected Events and
	 * Actions with in each state.
	 */
	static {
		LinkedHashSet<AinScfProtocolEvent> initainEvent = new LinkedHashSet<AinScfProtocolEvent>();

		// INIT State- Event allowed in initial state from network (InfoAnalyze,
		// InfoCollected)
		initainEvent.add(AinScfProtocolEvent.BEGIN);
		initainEvent.add(AinScfProtocolEvent.INFO_ANALYZE); // Info ANalyze
		initainEvent.add(AinScfProtocolEvent.INFO_COLLECT); // Info Collected
		initainEvent.add(AinScfProtocolEvent.LIDB_QUERY); // LIDB_QUERY
		initainEvent.add(AinScfProtocolEvent.AC_QUERY); // AC QUERY
		initainEvent.add(AinScfProtocolEvent.ISVM_QUERY); //ISVM QUERY
		initainEvent.add(AinScfProtocolEvent.PROVIDE_INSTRUCTION); // PROVIDER_INSTRUCTION
		initainEvent.add(AinScfProtocolEvent.TERMINATION_ATTEMPT); // Termination Attemot
		initainEvent.add(AinScfProtocolEvent.NTWK_BUSY); // Network Busy
		initainEvent.add(AinScfProtocolEvent.ORIGINATION_ATTEMPT); // Origination Attempt
		ainValidEventsMap.put(AinCallStates.INIT, initainEvent);
		ainValidEventsMap.put(AinCallStates.PSX_ROUTING, initainEvent);

		// TERM_CONNECT_IN_PROGRESS State - Events allowed in connect in progress
		// states.
		LinkedHashSet<AinScfProtocolEvent> connInProgainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		connInProgainEvent.add(AinScfProtocolEvent.CONTINUE);
		connInProgainEvent.add(AinScfProtocolEvent.AT_TIMEOUT);
		connInProgainEvent.add(AinScfProtocolEvent.END);
		connInProgainEvent.add(AinScfProtocolEvent.UABORT);
		connInProgainEvent.add(AinScfProtocolEvent.PABORT);
		connInProgainEvent.add(AinScfProtocolEvent.NOTICE);
		connInProgainEvent.add(AinScfProtocolEvent.ERB);
		connInProgainEvent.add(AinScfProtocolEvent.ERB_ANS);
		connInProgainEvent.add(AinScfProtocolEvent.ERB_BUSY);
		connInProgainEvent.add(AinScfProtocolEvent.ERB_NOANS);
		connInProgainEvent.add(AinScfProtocolEvent.ERB_ABANDON);
		connInProgainEvent.add(AinScfProtocolEvent.ERB_DISCONNECT);
		connInProgainEvent.add(AinScfProtocolEvent.ERB_TERM_SEIZED);
		connInProgainEvent.add(AinScfProtocolEvent.ERB_ROUTESELECTFAILURE);
		connInProgainEvent.add(AinScfProtocolEvent.RETURNERROR);
		connInProgainEvent.add(AinScfProtocolEvent.NTWK_BUSY);
		connInProgainEvent.add(AinScfProtocolEvent.RETURNRESULT);
		connInProgainEvent.add(AinScfProtocolEvent.UREJECT);
		connInProgainEvent.add(AinScfProtocolEvent.RRBCSM_TIMEOUT);
		connInProgainEvent.add(AinScfProtocolEvent.TERMINATION_NOTIFICATION);
		ainValidEventsMap.put(AinCallStates.TERM_CONNECT_IN_PROGRESS, connInProgainEvent);

		/*
		 * TERM_CONNECTED - Possible event which can be received in connected state are
		 * mentioned below.
		 */
		LinkedHashSet<AinScfProtocolEvent> connectedainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		connectedainEvent.add(AinScfProtocolEvent.CONTINUE);
		connectedainEvent.add(AinScfProtocolEvent.AT_TIMEOUT);
		connectedainEvent.add(AinScfProtocolEvent.END);
		connectedainEvent.add(AinScfProtocolEvent.UABORT);
		connectedainEvent.add(AinScfProtocolEvent.PABORT);
		connectedainEvent.add(AinScfProtocolEvent.NOTICE);
		connectedainEvent.add(AinScfProtocolEvent.CDR_TIMEOUT);
		connectedainEvent.add(AinScfProtocolEvent.ERB);
		connectedainEvent.add(AinScfProtocolEvent.ERB_DISCONNECT);
		connectedainEvent.add(AinScfProtocolEvent.RETURNERROR);
		connectedainEvent.add(AinScfProtocolEvent.RETURNRESULT);
		connectedainEvent.add(AinScfProtocolEvent.UREJECT);
		connectedainEvent.add(AinScfProtocolEvent.CLOSE);
		connInProgainEvent.add(AinScfProtocolEvent.TERMINATION_NOTIFICATION);
		ainValidEventsMap.put(AinCallStates.TERM_CONNECTED, connectedainEvent);

		/*
		 * TERMINATED - Possible events allowed in Terminated state.
		 */
		LinkedHashSet<AinScfProtocolEvent> terminatedainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		terminatedainEvent.add(AinScfProtocolEvent.CLOSE);
		terminatedainEvent.add(AinScfProtocolEvent.END);
		ainValidEventsMap.put(AinCallStates.TERMINATED, terminatedainEvent);

		/*
		 * SERVICE_LOGIC - Following events may come due to service logic execution.
		 */
		LinkedHashSet<AinScfProtocolEvent> serviceainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		serviceainEvent.add(AinScfProtocolEvent.CONTINUE);
		serviceainEvent.add(AinScfProtocolEvent.AT_TIMEOUT);
		serviceainEvent.add(AinScfProtocolEvent.END);
		serviceainEvent.add(AinScfProtocolEvent.ERB);
		serviceainEvent.add(AinScfProtocolEvent.UABORT);
		serviceainEvent.add(AinScfProtocolEvent.CLOSE);
		serviceainEvent.add(AinScfProtocolEvent.PABORT);
		serviceainEvent.add(AinScfProtocolEvent.NOTICE);
		ainValidEventsMap.put(AinCallStates.SERVICE_LOGIC, serviceainEvent);

		/*
		 * ASSIST state - This state is where one of the AIN call leg is connected to
		 * SIP leg. Possible events in this state are mentioned below.
		 */

		LinkedHashSet<AinScfProtocolEvent> assistainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		assistainEvent.add(AinScfProtocolEvent.CONTINUE);
		assistainEvent.add(AinScfProtocolEvent.AT_TIMEOUT);
		assistainEvent.add(AinScfProtocolEvent.END);
		assistainEvent.add(AinScfProtocolEvent.ERB);
		assistainEvent.add(AinScfProtocolEvent.UABORT);
		assistainEvent.add(AinScfProtocolEvent.PABORT);
		assistainEvent.add(AinScfProtocolEvent.NOTICE);
		assistainEvent.add(AinScfProtocolEvent.RETURNERROR);
		assistainEvent.add(AinScfProtocolEvent.RETURNRESULT);
		assistainEvent.add(AinScfProtocolEvent.NTWK_BUSY);
		assistainEvent.add(AinScfProtocolEvent.RES_CLR);
		assistainEvent.add(AinScfProtocolEvent.CORRELATION_TIMEOUT);
		assistainEvent.add(AinScfProtocolEvent.RES_CLR);
		assistainEvent.add(AinScfProtocolEvent.UREJECT);
		ainValidEventsMap.put(AinCallStates.ASSIST, assistainEvent);
		ainValidEventsMap.put(AinCallStates.HANDOFF, assistainEvent);

		/*
		 * TERMINATION_IN_PROGRESS - Events which may result transition to terminate
		 * state.
		 */
		LinkedHashSet<AinScfProtocolEvent> terminationInProgresainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		terminationInProgresainEvent.add(AinScfProtocolEvent.CONTINUE);
		terminationInProgresainEvent.add(AinScfProtocolEvent.END);
		terminationInProgresainEvent.add(AinScfProtocolEvent.AT_TIMEOUT);
		terminationInProgresainEvent.add(AinScfProtocolEvent.CLOSE);
		ainValidEventsMap.put(AinCallStates.TERMINATION_IN_PROGRESS, terminationInProgresainEvent);

		/*
		 * MS_CONNECT_INPROGRESS - Possible state in case where media server
		 * connectivity is in progress. This protocol helper class make use of SIP IVR
		 * interaction required for media server connectivity.
		 */
		LinkedHashSet<AinScfProtocolEvent> msConnectInProgresainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		msConnectInProgresainEvent.add(AinScfProtocolEvent.BEGIN);
		msConnectInProgresainEvent.add(AinScfProtocolEvent.CONTINUE);
		msConnectInProgresainEvent.add(AinScfProtocolEvent.END);
		msConnectInProgresainEvent.add(AinScfProtocolEvent.AT_TIMEOUT);
		ainValidEventsMap.put(AinCallStates.MS_CONNECT_INPROGRESS, msConnectInProgresainEvent);

		/*
		 * CALL_HB_IN_PROGRESS - State allowed during Activity test handling. it is
		 * currently not used
		 */
		LinkedHashSet<AinScfProtocolEvent> callHBInProgresainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		callHBInProgresainEvent.add(AinScfProtocolEvent.PABORT);
		callHBInProgresainEvent.add(AinScfProtocolEvent.CONTINUE);
		callHBInProgresainEvent.add(AinScfProtocolEvent.END);
		callHBInProgresainEvent.add(AinScfProtocolEvent.AT_TIMEOUT);
		callHBInProgresainEvent.add(AinScfProtocolEvent.CLOSE);
		callHBInProgresainEvent.add(AinScfProtocolEvent.NTWK_BUSY);
		ainValidEventsMap.put(AinCallStates.CALL_HB_IN_PROGRESS, callHBInProgresainEvent);

		/*
		 * USER_INTREACTION_IN_PROGRESS - state where IVR interaction is going on.
		 * Following events are handled in this case.
		 */
		LinkedHashSet<AinScfProtocolEvent> userInterInProgainEvent = new LinkedHashSet<AinScfProtocolEvent>();
		userInterInProgainEvent.add(AinScfProtocolEvent.PABORT);
		userInterInProgainEvent.add(AinScfProtocolEvent.UABORT);
		userInterInProgainEvent.add(AinScfProtocolEvent.CONTINUE);
		userInterInProgainEvent.add(AinScfProtocolEvent.END);
		userInterInProgainEvent.add(AinScfProtocolEvent.NOTICE);
		userInterInProgainEvent.add(AinScfProtocolEvent.UREJECT);
		userInterInProgainEvent.add(AinScfProtocolEvent.RETURNERROR);
		userInterInProgainEvent.add(AinScfProtocolEvent.RETURNRESULT);
		userInterInProgainEvent.add(AinScfProtocolEvent.AT_TIMEOUT);
		userInterInProgainEvent.add(AinScfProtocolEvent.RES_CLR);
		userInterInProgainEvent.add(AinScfProtocolEvent.CALL_INFO_FRM_RESRC);
		assistainEvent.add(AinScfProtocolEvent.RES_CLR);
		ainValidEventsMap.put(AinCallStates.USER_INTREACTION_IN_PROGRESS, userInterInProgainEvent);
		ainValidEventsMap.put(AinCallStates.MS_PLAY, userInterInProgainEvent);
		ainValidEventsMap.put(AinCallStates.MS_PLAYCOLLECT, userInterInProgainEvent);
		ainValidEventsMap.put(AinCallStates.MS_PLAY_APP_REQUEST, userInterInProgainEvent);
		ainValidEventsMap.put(AinCallStates.MS_DISCONNECT_INPROGRESS, userInterInProgainEvent);
		ainValidEventsMap.put(AinCallStates.MS_PLAY_COLLECT_CIFR, userInterInProgainEvent);
		ainValidEventsMap.put(AinCallStates.MS_PLAY_CIFR, userInterInProgainEvent);

	}

	/**
	 * /** This method validates the AIN event against current state in call data.
	 * 
	 * @param ainProtocolEvent represent the instance of ainEvent
	 * @param tcapSession      TCAP session for which event needs to be validated.
	 *                         Call Data, which is fetched from TcapSession,
	 *                         contains details of call state and other call
	 *                         specific data.
	 * @return true if validation is successful else false.
	 */
	static boolean validateFSMState(AinScfProtocolEvent ainProtocolEvent, TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] validateFSMState Enter");
		}
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogueId = tcapSession.getDialogueId();

		if (callData == null) {

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + " [PH]::Call data is null create dummy call data for error handling ");
			}

			/*
			 * create dummy call data for error handling
			 */
			callData = new CallData();

			callData.set(CallDataAttribute.P_PROTOCOL, Protocol.AIN_SCF);
			callData.set(CallDataAttribute.P_DIALOG_ID, dialogueId);
			callData.set(CallDataAttribute.NP_FT_CALL, true);
			callData.set(CallDataAttribute.P_ATTEMPTED_IND, 4);

			tcapSession.setAttribute(CallData.CALL_DATA, callData);
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + " [PH]:: New call data created and set in TCAP session");
			}

			return false;
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (legData == null) {
			logger.error("[PH] ::validateFSMState - LegData is NULL for Dialogue Id :" + tcapSession.getDialogueId());
			return false;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside validateFSMState for event[" + ainProtocolEvent + "]");
		}

		/*
		 * fetch current call state
		 */
		AinCallStates currCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] curr call state is [" + currCallState + "]");
		}

		/*
		 * check if valid message For FT call and Non timeout events mark event as
		 * invalid
		 */
		if ((PhConstants.TRUE.equals(callData.get(CallDataAttribute.NP_FT_CALL))
				&& (currCallState != AinCallStates.TERM_CONNECTED)
				&& (ainProtocolEvent != AinScfProtocolEvent.AT_TIMEOUT
						&& ainProtocolEvent != AinScfProtocolEvent.CORRELATION_TIMEOUT
						&& ainProtocolEvent != AinScfProtocolEvent.CDR_TIMEOUT))) {
			if (logger.isInfoEnabled()) {
				logger.info(dialogueId + "::[PH]  Event received after SAS failover");
				logger.info(dialogueId
						+ "::[PH]  Call is not in connected state invalid state after FT; we should drop a transient call");
			}

			/*
			 * for this scenario set failed call set attempted call ind for this scenario
			 */
			callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
			return false;
		}

		/*
		 * fetch valid events for current call state
		 */
		Set<AinScfProtocolEvent> validEvents = ainValidEventsMap.get(currCallState);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Possible events for curr call state are [" + validEvents + "]");
		}

		/*
		 * fetch dialogue primitive
		 */
		int rxDialogType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);

		/*
		 * match current event to valid event list
		 */
		if (validEvents.contains(ainProtocolEvent)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId
						+ "::[PH] current event is allowed for given call state; checking event specific preconditions::");
			}
			boolean status = true;

			/*
			 * event specific checks.. like IDP in begin erb as per armed events.Currently
			 * ERB is not coming as individual events so individual ERBs validation code
			 * will not be invoked
			 * 
			 */
			switch (ainProtocolEvent) {
			case INFO_ANALYZE: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(
								dialogueId + "::[PH] INFO Analyze not part of begin dialouge. Last dialog recived::["
										+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case LIDB_QUERY: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(
								dialogueId + "::[PH] LIDB_QUERY  not part of begin dialouge. Last dialog recived::["
										+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case PROVIDE_INSTRUCTION: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ "::[PH] PROVIDER_INSTRUCTION  not part of begin dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case AC_QUERY: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] AC_QUERY  not part of begin dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case ISVM_QUERY: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] ISVM_QUERY  not part of begin dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case INFO_COLLECT: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(
								dialogueId + "::[PH] INFO Collect not part of begin dialouge. Last dialog recived::["
										+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}

			case TERMINATION_ATTEMPT: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ "::[PH]TERMINATION_ATTEMPT  not part of begin dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}

			case ERB_BUSY: {

				Set<Action.ERB_TYPE> erbSet = AinScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_BUSY))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set doesn't contain ERB_BUSY");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
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
					} // end default
					}// end switch
				} // end else
				break;
			}
			case ERB_TERM_SEIZED: {

				Set<Action.ERB_TYPE> erbSet = AinScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_TERMSEIZED))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set doesn't contain ERB_BUSY");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
					case TcapConstants.PRIMITIVE_END:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ "::[PH] ERB_TERM_SEIZED not part of Continue/END dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
						break;
					} // end default
					}// end switch
				} // end else
				break;
			}
			case ERB_NOANS: {

				Set<Action.ERB_TYPE> erbSet = AinScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_NO_ANSWER))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set doesn't contain ERB_NOANS");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
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
					} // end default
					}// end switch
				} // end else status
				break;

			}
			case ERB_ANS: {
				Set<Action.ERB_TYPE> erbSet = AinScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ANSWER))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set doesn't contain ERB_ANS");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(
									dialogueId + "::[PH] ERB_ANS not part of Continue dialouge. Last dialog recived::["
											+ rxDialogType + "]");
						}
						status = false;
						break;
					} // end default
					}// end switch
				} // end else status
				break;

			}
			case ERB_DISCONNECT: {
				Set<Action.ERB_TYPE> erbSet = AinScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_DISCONNECT))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set doesn't contain ERB_DISCONNECT");
					}
					status = false;
				}

				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
					case TcapConstants.PRIMITIVE_END:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ "::[PH] ERB_DISCONNECT not part of Continue/END dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
						break;
					} // end default
					}// end switch
				} // end else status
				break;

			}
			case ERB_ABANDON: {
				Set<Action.ERB_TYPE> erbSet = AinScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ABANDON))) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Erb set doesn't contain ERB_OABANDON");
					}
					status = false;
				}
				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
				} else {

					switch (rxDialogType) {
					case TcapConstants.PRIMITIVE_CONTINUE:
					case TcapConstants.PRIMITIVE_END:
						break;
					default: {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ "::[PH] ERB_ABANDON not part of Continue/END dialouge. Last dialog recived::["
									+ rxDialogType + "]");
						}
						status = false;
						break;
					} // end default
					}// end switch
				} // end else status
				break;
			}
			case CALL_INFO_FRM_RESRC:
			case RES_CLR: {

				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] ACR not part of Continue dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case NTWK_BUSY: {

				// Network Busy may come as QWP as well as CWP. so removing check
				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE && rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug(
								dialogueId + "::[PH] Network Busy not part of Continue dialouge. Last dialog recived::["
										+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}

			case CLOSE: {

				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] ENC not part of Continue dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case END: {

				if (rxDialogType != TcapConstants.PRIMITIVE_END) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] END is not part of END dialouge. Last dialog recived::["
								+ rxDialogType + "]");
					}
					status = false;
				}
				break;
			}

			default:
				/*
				 * status should remain as true;
				 */
				status = true;
				break;

			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "::[PH] Return with validation status::[" + status + "]");
			}

			return status;
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "::[PH] event is NOT allowed for cuee call state return false");
			}
			return false;
		}

	}
}
