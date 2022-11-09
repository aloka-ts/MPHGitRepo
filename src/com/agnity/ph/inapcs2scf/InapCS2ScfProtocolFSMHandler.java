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
package com.agnity.ph.inapcs2scf;

import com.agnity.mphdata.common.*;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.tcap.TcapConstants;

import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * This class is used to define the Functional state Machine of INAP protocol.it
 * maintains the mapping of various INAP incoming messages with the possible
 * INAP protocol states.so when an INAP message is received from network it
 * validates it against current state of call. if the event is not valid for the
 * current state ,the validation fails and the error response is sent by protocol handler.
 */
public class InapCS2ScfProtocolFSMHandler {

	private static Logger logger = Logger.getLogger(InapCS2ScfProtocolFSMHandler.class);

	/**
	 * This map will store mapping of call state to possible events in state
	 */
	private static Map<InapCallStates, Set<InapCS2ScfProtocolEvent>> inapValidEventsMap = new HashMap<InapCallStates, Set<InapCS2ScfProtocolEvent>>();

	/**
	 * Create a Map which specifies what all events are valid in which state.
	 * INIT state - BEGIN and IDP are expected
	 * CONN_IN_PROGRESS -
	 * CONNECTED - TERMINATED -
	 */
	static {
		LinkedHashSet<InapCS2ScfProtocolEvent> initInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		initInapEvent.add(InapCS2ScfProtocolEvent.BEGIN);
		initInapEvent.add(InapCS2ScfProtocolEvent.IDP);
		inapValidEventsMap.put(InapCallStates.INIT, initInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> connInProgInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.AT_TIMEOUT);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.END);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.UABORT);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.PABORT);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.NOTICE);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.ERB);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.ERB_ANS);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.ERB_BUSY);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.ERB_NOANS);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.ERB_ABANDON);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.ERB_DISCONNECT);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.ERB_ROUTESELECTFAILURE);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.RETURNERROR);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.RETURNRESULT);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.UREJECT);
		connInProgInapEvent.add(InapCS2ScfProtocolEvent.RRBCSM_TIMEOUT);
		inapValidEventsMap.put(InapCallStates.TERM_CONNECT_IN_PROGRESS, connInProgInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> connectedInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		connectedInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.AT_TIMEOUT);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.END);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.UABORT);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.PABORT);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.NOTICE);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.CDR_TIMEOUT);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.ERB);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.ERB_DISCONNECT);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.RETURNERROR);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.RETURNRESULT);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.UREJECT);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.ACR);
		connectedInapEvent.add(InapCS2ScfProtocolEvent.ENC);
		inapValidEventsMap.put(InapCallStates.TERM_CONNECTED, connectedInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> terminatedInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		terminatedInapEvent.add(InapCS2ScfProtocolEvent.ACR);
		terminatedInapEvent.add(InapCS2ScfProtocolEvent.END);
		terminatedInapEvent.add(InapCS2ScfProtocolEvent.ENC);
		inapValidEventsMap.put(InapCallStates.TERMINATED, terminatedInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> serviceInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		serviceInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		serviceInapEvent.add(InapCS2ScfProtocolEvent.AT_TIMEOUT);
		serviceInapEvent.add(InapCS2ScfProtocolEvent.END);
		serviceInapEvent.add(InapCS2ScfProtocolEvent.ERB);
		serviceInapEvent.add(InapCS2ScfProtocolEvent.UABORT);
		serviceInapEvent.add(InapCS2ScfProtocolEvent.PABORT);
		serviceInapEvent.add(InapCS2ScfProtocolEvent.NOTICE);
		inapValidEventsMap.put(InapCallStates.SERVICE_LOGIC, serviceInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> handOffInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		handOffInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.END);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.ERB);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.UABORT);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.PABORT);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.NOTICE);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.RETURNERROR);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.RETURNRESULT);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.UREJECT);
		handOffInapEvent.add(InapCS2ScfProtocolEvent.CORRELATION_TIMEOUT);
		inapValidEventsMap.put(InapCallStates.HANDOFF, handOffInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> assistInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		assistInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		assistInapEvent.add(InapCS2ScfProtocolEvent.AT_TIMEOUT);
		assistInapEvent.add(InapCS2ScfProtocolEvent.END);
		assistInapEvent.add(InapCS2ScfProtocolEvent.ERB);
		assistInapEvent.add(InapCS2ScfProtocolEvent.UABORT);
		assistInapEvent.add(InapCS2ScfProtocolEvent.PABORT);
		assistInapEvent.add(InapCS2ScfProtocolEvent.NOTICE);
		assistInapEvent.add(InapCS2ScfProtocolEvent.RETURNERROR);
		assistInapEvent.add(InapCS2ScfProtocolEvent.RETURNRESULT);
		assistInapEvent.add(InapCS2ScfProtocolEvent.CORRELATION_TIMEOUT);
		assistInapEvent.add(InapCS2ScfProtocolEvent.UREJECT);
		inapValidEventsMap.put(InapCallStates.ASSIST, assistInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> terminationInProgresInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		terminationInProgresInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		terminationInProgresInapEvent.add(InapCS2ScfProtocolEvent.END);
		terminationInProgresInapEvent.add(InapCS2ScfProtocolEvent.AT_TIMEOUT);
		terminationInProgresInapEvent.add(InapCS2ScfProtocolEvent.ACR);
		terminationInProgresInapEvent.add(InapCS2ScfProtocolEvent.ENC);
		inapValidEventsMap.put(InapCallStates.TERMINATION_IN_PROGRESS, terminationInProgresInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> msConnectInProgresInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		msConnectInProgresInapEvent.add(InapCS2ScfProtocolEvent.BEGIN);
		msConnectInProgresInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		msConnectInProgresInapEvent.add(InapCS2ScfProtocolEvent.END);
		msConnectInProgresInapEvent.add(InapCS2ScfProtocolEvent.ARI);
		msConnectInProgresInapEvent.add(InapCS2ScfProtocolEvent.AT_TIMEOUT);
		inapValidEventsMap.put(InapCallStates.MS_CONNECT_INPROGRESS, msConnectInProgresInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> termConnectedACRInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.PABORT);
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.UABORT);
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.END);
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.NOTICE);
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.UREJECT);
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.RETURNERROR);
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.RETURNRESULT);
		termConnectedACRInapEvent.add(InapCS2ScfProtocolEvent.AT_TIMEOUT);
		inapValidEventsMap.put(InapCallStates.TERM_CONNECTED_ACR, termConnectedACRInapEvent);

		LinkedHashSet<InapCS2ScfProtocolEvent> userInterInProgInapEvent = new LinkedHashSet<InapCS2ScfProtocolEvent>();
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.PABORT);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.UABORT);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.CONTINUE);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.END);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.NOTICE);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.UREJECT);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.RETURNERROR);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.RETURNRESULT);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.AT_TIMEOUT);
		userInterInProgInapEvent.add(InapCS2ScfProtocolEvent.SRR);
		inapValidEventsMap.put(InapCallStates.USER_INTREACTION_IN_PROGRESS, userInterInProgInapEvent);
	}

	/**
	 * This method validates the INAP event against current state in call data
	 *
	 * @return true if valid event. false in case of error or invalid event
	 */
	public static boolean validateFSMState(InapCS2ScfProtocolEvent inapProtocolEvent, TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: validateFSMState Enter");
		}
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		int dialogueId = tcapSession.getDialogueId();

		if (callData == null) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Call data is null return false ");
			}

			//create dummy call data for error handling
			callData = new CallData();

			callData.set(CallDataAttribute.P_PROTOCOL, Protocol.ITUINAPCS2_SCF);
			callData.set(CallDataAttribute.P_DIALOG_ID, dialogueId);
			callData.set(CallDataAttribute.NP_FT_CALL, true);
			callData.set(CallDataAttribute.P_ATTEMPTED_IND, 4);

			tcapSession.setAttribute(CallData.CALL_DATA, callData);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: New call data created and set in TCAP session");
			}
			return false;
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (legData == null) {
			logger.error("[PH]:: validateFSMState - LegData is NULL for Dialogue Id :" + tcapSession.getDialogueId());
			return false;
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside validateFSMState for event[" + inapProtocolEvent + "]");
		}

		// fetch current call state
		InapCallStates currCallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: curr call state is [" + currCallState + "]");
		}

		//  check if valid message
		//  For FT call and Non timeout events mark event as invalid
		if ((PhConstants.TRUE.equals(callData.get(CallDataAttribute.NP_FT_CALL)) &&
				(currCallState != InapCallStates.TERM_CONNECTED) &&
				(inapProtocolEvent != InapCS2ScfProtocolEvent.AT_TIMEOUT &&
				inapProtocolEvent != InapCS2ScfProtocolEvent.CORRELATION_TIMEOUT &&
				inapProtocolEvent != InapCS2ScfProtocolEvent.CDR_TIMEOUT))) {
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Event received after SAS failover");
				logger.info("[PH]:: Call is not in connected state invalid state after FT; we should drop a transient call");
			}

			// for this scenario set failed call set attempted call ind for this scenario
			callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
			return false;
		}

		//fetch valid events for current call state
		Set<InapCS2ScfProtocolEvent> validEvents = inapValidEventsMap.get(currCallState);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Possible events for curr call state are [" + validEvents + "]");
		}

		// fetch dialogue primitive
		int rxDialogType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);

		// match current event to valid event list
		if (validEvents.contains(inapProtocolEvent)) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: current event is allowed for given call state; checking event specific preconditions::");
			}
			boolean status = true;

			/*
			 * event specific checks.. like IDP in begin erb as per armed events.Currently ERB is
			 * not coming as individual events so individual ERBs validation
			 * code will not be invoked
			 */
			switch (inapProtocolEvent) {
			case IDP: {
				if (rxDialogType != TcapConstants.PRIMITIVE_BEGIN) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: IDP not part of begin dialouge. Last dialog recived::[" + rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case ERB_ROUTESELECTFAILURE: {
				// Here null should be replaced with legid for which ERB is received
				Set<Action.ERB_TYPE> erbSet = InapCS2ScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ROUTESELECTFAILURE))) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set doesn't contain ERB_ROUTESELECTFAILURE");
					}
					status = false;
				}
				if (!status) {
					tcapSession.setAttribute(PhConstants.UNARMED_ERROR_TYPE, PhConstants.TRUE);
				}
				break;
			}
			case ERB_BUSY: {
				Set<Action.ERB_TYPE> erbSet = InapCS2ScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_BUSY))) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set doesn't contain ERB_BUSY");
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
							logger.debug("[PH]:: ERB_BUSY not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
						}
						status = false;
						break;
					}
					}
				}
				break;
			}
			case ERB_NOANS: {
				Set<Action.ERB_TYPE> erbSet = InapCS2ScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_NO_ANSWER))) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set doesn't contain ERB_NOANS");
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
							logger.debug("[PH]:: ERB_NOANS not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
						}
						status = false;
						break;
					}
					}
				}
				break;
			}
			case ERB_ANS: {
				Set<Action.ERB_TYPE> erbSet = InapCS2ScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ANSWER))) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set doesn't contain ERB_ANS");
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
							logger.debug("[PH]:: ERB_ANS not part of Continue dialouge. Last dialog recived::[" + rxDialogType + "]");
						}
						status = false;
						break;
					}
					}
				}
				break;
			}
			case ERB_DISCONNECT: {
				Set<Action.ERB_TYPE> erbSet = InapCS2ScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_DISCONNECT))) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set doesn't contain ERB_DISCONNECT");
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
							logger.debug("[PH]:: ERB_DISCONNECT not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
						}
						status = false;
						break;
					}
					}
				}
				break;
			}
			case ERB_ABANDON: {
				Set<Action.ERB_TYPE> erbSet = InapCS2ScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_ABANDON))) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set doesn't contain ERB_OABANDON");
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
							logger.debug("[PH]:: ERB_ABANDON not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
						}
						status = false;
						break;
					}
					}
				}
				break;
			}
			case ERB_MIDCALL: {
				Set<Action.ERB_TYPE> erbSet = InapCS2ScfProtocolUtil.getErbSetByApplication(callData, null);
				if (erbSet == null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set is null in call data Got invalid erbtype");
					}
					status = false;
				} else if (!(erbSet.contains(Action.ERB_TYPE.ERB_MIDCALL))) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Erb set doesn't contain ERB_MIDCALL");
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
							logger.debug("[PH]:: ERB_MIDCALL not part of Continue/END dialouge. Last dialog recived::[" + rxDialogType + "]");
						}
						status = false;
						break;
					}
					}
				}
				break;
			}
			case ACR: {
				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: ACR not part of Continue dialouge. Last dialog recived::[" + rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case SRR: {
				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: SRR not part of Continue dialouge. Last dialog recived::[" + rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case ENC: {
				if (rxDialogType != TcapConstants.PRIMITIVE_CONTINUE) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: ENC not part of Continue dialouge. Last dialog recived::[" + rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			case END: {
				if (rxDialogType != TcapConstants.PRIMITIVE_END) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: END is not part of END dialouge. Last dialog recived::[" + rxDialogType + "]");
					}
					status = false;
				}
				break;
			}
			default: {
				// status should remain as true;
				status = true;
				break;
			}
			}
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Return with validation status::[" + status + "]");
			}
			return status;
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: event is NOT allowed for cuee call state return false");
			}
			return false;
		}
	}

	public static enum AbortInfoEnum {

		NO_REASON_GIVEN(1),
		APPLICATION_TIMER_EXPIRED(2),
		PROTOCOL_PROHIBITED_SIGNAL_RECIEVED(3),
		ABNORMAL_PROCESSING(4),
		CONGESTION(5),
		AC_NEGOTIATION_FAILED(6),
		UNRECOGNIZED_EXTENSION_PARAMETER(7);

		private int code;

		private AbortInfoEnum(int i) {
			this.code = i;
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
			}
			return abortInfo;
		}

		public int getCode() {
			return code;
		}
	}
}
