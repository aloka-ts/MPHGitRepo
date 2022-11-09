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

import static com.agnity.mphdata.common.AnnSpec.ANN_TYPE_DAT;
import static com.agnity.mphdata.common.AnnSpec.ANN_TYPE_MNY;
import static com.agnity.mphdata.common.AnnSpec.ANN_TYPE_NUM;
import static com.agnity.mphdata.common.AnnSpec.ANN_TYPE_TIME;
import static com.google.common.collect.Collections2.filter;

import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.EventObject;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.sip.ServletParseException;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipFactory;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.SipSessionsUtil;
import javax.servlet.sip.URI;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.camelv2.asngenerated.CollectedDigits;
import com.agnity.camelv2.asngenerated.CollectedInfo;
import com.agnity.camelv2.asngenerated.InbandInfo;
import com.agnity.camelv2.asngenerated.InformationToSend;
import com.agnity.camelv2.asngenerated.Integer4;
import com.agnity.camelv2.asngenerated.MessageID;
import com.agnity.camelv2.asngenerated.PromptAndCollectUserInformationArg;
import com.agnity.camelv2.asngenerated.VariablePart;
import com.agnity.camelv2.enumdata.CauseValEnum;
import com.agnity.camelv2.operations.CapV2OpCodes;
import com.agnity.camelv2.operations.CapV2OperationsCoding;
import com.agnity.camelv2.util.Util;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.Action.CONNECTIONMODE;
import com.agnity.mphdata.common.Action.CONTINUE_MODE;
import com.agnity.mphdata.common.Action.DROP_CALL_MODE;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CapV2CallStates;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.capv2scf.Capv2ScfProtocolFSMHandler.AbortInfoEnum;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolHandler;
import com.agnity.ph.common.ProtocolHandlerFactory;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ASNParsingException.FAILTYPE;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
import com.agnity.ph.common.exception.CriticalityTypeException;
import com.agnity.ph.common.exception.CriticalityTypeException.CRITICALITY;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.common.measurement.MeasurementCounter;
import com.agnity.ph.common.measurement.PhMeasurementService;
import com.agnity.ph.common.measurement.enums.SS7Message;
import com.agnity.ph.inapcs1scf.messagehelper.BCDEncoderHelper;
import com.agnity.ph.sip.SipProtocolConfig;
import com.baypackets.ase.sbb.OutboundGateway;
import com.baypackets.ase.sbb.OutboundGatewaySelector;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;
import com.google.common.base.Predicate;

import jain.MandatoryParameterNotSetException;
import jain.ParameterNotSetException;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.ComponentReqEvent;
import jain.protocol.ss7.tcap.DialogueIndEvent;
import jain.protocol.ss7.tcap.DialogueReqEvent;
import jain.protocol.ss7.tcap.TcapConstants;
import jain.protocol.ss7.tcap.component.ComponentConstants;
import jain.protocol.ss7.tcap.component.ErrorIndEvent;
import jain.protocol.ss7.tcap.component.ErrorReqEvent;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.InvokeReqEvent;
import jain.protocol.ss7.tcap.component.Operation;
import jain.protocol.ss7.tcap.component.Parameters;
import jain.protocol.ss7.tcap.component.RejectIndEvent;
import jain.protocol.ss7.tcap.component.RejectReqEvent;
import jain.protocol.ss7.tcap.component.ResultIndEvent;
import jain.protocol.ss7.tcap.component.ResultReqEvent;
import jain.protocol.ss7.tcap.dialogue.BeginIndEvent;
import jain.protocol.ss7.tcap.dialogue.ContinueReqEvent;
import jain.protocol.ss7.tcap.dialogue.DialogueConstants;
import jain.protocol.ss7.tcap.dialogue.DialoguePortion;
import jain.protocol.ss7.tcap.dialogue.EndReqEvent;
import jain.protocol.ss7.tcap.dialogue.ProviderAbortIndEvent;
import jain.protocol.ss7.tcap.dialogue.UserAbortIndEvent;
import jain.protocol.ss7.tcap.dialogue.UserAbortReqEvent;

/**
 * 
 * This class is a helper class which is used by CAP protocol handler to delegate
 * the message processing of incoming messages and and creation of outgoing CAP messages
 *
 */
public class Capv2ScfProtocolHelper {

	private static Logger logger = Logger
			.getLogger(Capv2ScfProtocolHelper.class);
	private static Object src = "source".intern();

	/**
	 * It defines the different connect types possible in CAP call.
	 */
	private static enum CONNECT_TYPE {
		TERMINATING, PORTED, CORRELATION
	}

	/**
	 * This method will trace incoming CAP Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error;
	 * 
	 * @param die
	 *            represents the instance of DialogueIndEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueIndEvent die, CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside traceDialog");
		}
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();
		try {
			String traceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);

			if (traceFlag.equals(PhConstants.TRUE)) {
				int primitive = die.getPrimitiveType();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				StringBuilder traceMsg = new StringBuilder();

				traceMsg.append("<<<<Recieved<<<<");
				traceMsg.append("\n");

				traceMsg.append("DIALOGUE::");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_BEGIN: {
					traceMsg.append("BEGIN");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_CONTINUE: {
					traceMsg.append("CONTINUE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_END: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					traceMsg.append("false");
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_END_PRE_ARRANGED: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					traceMsg.append("true");
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_PROVIDER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("P-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(((ProviderAbortIndEvent) die).getPAbort());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_USER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("U-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(Util.formatBytes(((UserAbortIndEvent) die)
							.getUserAbortInformation()));
					traceMsg.append("\n");

					traceMsg.append("Generic Tcap Reason::");
					traceMsg.append(((UserAbortIndEvent) die).getAbortReason());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_NOTICE: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("NOTICE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
					traceMsg.append("UNIDIRECTIONAL");
					traceMsg.append("\n");
					break;
				}
				default: {
					traceMsg.append("CODE:" + primitive);
					traceMsg.append("\n");
					break;
				}
				}

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					@SuppressWarnings("unchecked")
					List<Integer> constraintList = (List<Integer>) callData
					.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								Capv2CS1ScfProtocolUtil.getUniqueCallIDForTracing(callData),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing Dialog ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing dialogue ind event", t);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit traceDialog");
		}
	}

	/**
	 * This method performs the processing of Begin dialogue indication event.
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param callConfigParams
	 *            represents the instance of Properties
	 * @return an array of Action objects.
	 * @throws MandatoryParameterNotSetException
	 */
	static void processBegin(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) throws MandatoryParameterNotSetException {

		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] processBegin Enter ");
		}

		/*
		 * call data can't be null if it has passed validation method so moving
		 * ahead without null check store dialog
		 */

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ ":: [PH] Set Dialogue Portion in call data");
			}

			try {
				/*
				 * Store Dialogue Portion in Leg Data.
				 */
				legData.set(LegDataAttributes.P_DIALOG_PORTION,
						dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId
						+ ":: [PH] Error getting dialogue portion "
						+ e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId
							+ ":: [PH] Error getting dialog portion.", e);
					logger.info(dialogId
							+ "::[PH] IGNORE ParameterNotSetException fetching "
							+ "dialoguePortion from BEGIN dialogue event");
				}
			}
		}

		/*
		 * perform ACN check
		 */
		BeginIndEvent beginIndEvent = (BeginIndEvent) dialogueIndEvent;
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " ::[PH] isAppContextNamePresent = "
					+ beginIndEvent.isAppContextNamePresent());
		}
		if (beginIndEvent.isAppContextNamePresent()) {
			try {
				byte[] appContextName = beginIndEvent.getAppContextName();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " ::[PH] Application Context version from IDP = "
							+ Util.formatBytes(appContextName));
				}

				/*
				 * As isAppContextNamePresent always returns true even if
				 * appCOntextName is null making null check
				 */
				if (appContextName != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ " :: [PH] Application Context not null");
					}
					legData.set(
							LegDataAttributes.P_ACTIVITY_CONTEXT_VERSION,
							appContextName[appContextName.length - 1]);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ " :: [PH] Application Context is null");
					}

					/*
					 * valid acn handling is odne at service in test call only
					 * return handleInavlidAcn(tcapSession);
					 */
				}
			} catch (Exception e) {
				logger.warn(dialogId + ":: [PH] Error fetching AC version "
						+ e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId
							+ " :: [PH] Exception fetching AC Version", e);
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: [PH] Set Application Context version "
						+ legData
						.get(LegDataAttributes.P_ACTIVITY_CONTEXT_VERSION)
						+ " in call data");
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: isAppContextNamePresent is false");
			}
		}

		try {
			SccpUserAddress sccpUserAddress = beginIndEvent
					.getOriginatingAddress();
			SignalingPointCode signalingPointCode = sccpUserAddress
					.getSubSystemAddress().getSignalingPointCode();
			int zone = signalingPointCode.getZone();
			int cluster = signalingPointCode.getCluster();
			int member = signalingPointCode.getMember();
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] Origin zone= " + zone
						+ " cluster=" + cluster + " member=" + member);
			}
			String pcBitStr = lPad(Integer.toBinaryString(zone), 3)
					+ lPad(Integer.toBinaryString(cluster), 8)
					+ lPad(Integer.toBinaryString(member), 3);
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] pcBitStr =" + pcBitStr);
			}
			int pc = Integer.parseInt(pcBitStr, 2);
			legData.set(LegDataAttributes.P_SPC, signalingPointCode);
			legData.set(LegDataAttributes.P_OPC, pc);
			callData.set(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS,beginIndEvent.getDestinationAddress() );
			callData.set(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS, beginIndEvent.getOriginatingAddress());
			if (logger.isInfoEnabled()) {
				logger.info(dialogId
						+ " :: [PH] Calculated Origin Point Code form IDP-Begin ="
						+ legData.get(LegDataAttributes.P_OPC));
			}
		} catch (ParameterNotSetException e1) {
			logger.error(dialogId
					+ " :: [PH] Failed to get origin point code from Dialog Indication event. "
					+ e1.getMessage());
		}

		/*
		 * Use the sccp address received in IDP to support multiple pc-ssn
		 */
		legData.set(LegDataAttributes.P_SUA,
				beginIndEvent.getDestinationAddress());
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: processBegin Exit ");
		}
	}

	/*
	 * 
	 */
	/**
	 * This Method i used to pad string
	 * @param input
	 * @param resultSize
	 * @return
	 */
	private static String lPad(String input, int resultSize) {
		if (input == null) {
			return input;
		}
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < (resultSize - input.length()); i++) {
			result.append("0");
		}
		result.append(input);
		return result.toString();
	}

	/**
	 * This method performs the processing of continue dialogue indication
	 * event.
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processContinue(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: processContinue Enter ");
		}

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Set Dialogue Portion in call data");
			}

			try {
				legData.set(LegDataAttributes.P_DIALOG_PORTION,
						dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId + ":: Error getting dialogue portion. "
						+ e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + ":: Error getting dialog portion.",
							e);
					logger.info(dialogId
							+ "::IGNORE ParameterNotSetException fetching "
							+ "dialoguePortion from CONTINUE dialogue event");
				}
			}
		}
		return null;

	}

	/**
	 * This method performs the processing of TC_END dialogue indication event.
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processEnd(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processEnd Enter ");

		}

		return null;
	}

	/**
	 * This method performs the processing of Abort dialogue indication event.
	 * Marks the cause value as 41 for failed calls.
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processAbort(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processAbort Enter ");
		}

		return null;
	}

	/**
	 * This method performs the processing of UAbort dialogue indication event.
	 * Marks the cause value as 31 because user hung up the call and related
	 * events are not armed..
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processUAbort(DialogueIndEvent dialogueIndEvent,
			TcapSession tcapSession) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processUAbort Enter ");
		}

		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever an IDP is
	 * received.
	 * 
	 * @param invokeIndEvent
	 *            represents the instance of InvokeIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param cts
	 *            represents the instance of CallTraceService
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] processIdp(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession, CallTraceService cts) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: CAP processIdp Enter");
		}

		/*
		 * change state to service logic
		 */

		legData.set(LegDataAttributes.P_LEG_SS7_STATE,
				CapV2CallStates.SERVICE_LOGIC);

		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke ID::" + invokeId);
		}

		/*
		 * parse IDP
		 */
		try {
			Capv2ScfProtocolParser.parseIdp(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in IDP.", ape);

			switch (ape.getParseFailType()) {
			case CPC_MISSING: {
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.IDP_MISSING_CPC);
				break;
			}
			case TMR_MISSING: {
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.IDP_MISSING_TMR);
				break;
			}
			case ACPC_MISSING: {
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.IDP_MISSING_ACPC);
				break;
			}
			case FCI_MISSING: {
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.IDP_MISSING_FCI);
				break;
			}
			default: {
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.IDP_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in IDP.",
					cte);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.IDP_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession,
					cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId
					+ ":: [PH] ParameterOutOfRangeException in IDP.", pore);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.IDP_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.IDP);
		}

		/*
		 * Unable to take TraceCall by OriginatingAddress in case CAP IDP
		 * includes Contract Number.
		 */
		/*
		 * fetch calling party address first check contractor; if contractor not
		 * present get callingNum
		 */
		String contractorAddress = legData
				.get(LegDataAttributes.P_CONTRACTOR_NUMBER) == null ? null
						: ((PhoneNumber) legData
								.get(LegDataAttributes.P_CONTRACTOR_NUMBER))
						.getAddress();

		String callingPartyAddress = legData
				.get(LegDataAttributes.P_CALLING_PARTY) == null ? null
						: ((PhoneNumber) legData
								.get(LegDataAttributes.P_CALLING_PARTY))
						.getAddress();

		String calledPartyAddress = legData
				.get(LegDataAttributes.P_CALLED_PARTY) == null ? null
						: ((PhoneNumber) legData
								.get(LegDataAttributes.P_CALLED_PARTY))
						.getAddress();

		String idpCalledPartyAddress = legData
				.get(LegDataAttributes.P_IDP_CALLED_PARTY) == null ? null
						: ((PhoneNumber) legData
								.get(LegDataAttributes.P_IDP_CALLED_PARTY))
						.getAddress();

		/*
		 * checking if calling party is mising
		 */
		if ((contractorAddress == null || contractorAddress.isEmpty())
				&& (callingPartyAddress == null || callingPartyAddress
				.isEmpty())) {
			logger.error(dialogId + ":: [PH] Calling party missing");
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.IDP_MISSING_CALLING_PARTY);
			return getCallingPartyMissingAction(tcapSession);
		}

		/*
		 * match call tracing constraints
		 */
		List constraintIdList = cts.matchesCriteria(callingPartyAddress,
				idpCalledPartyAddress, calledPartyAddress);

		if (constraintIdList != null && !constraintIdList.isEmpty()) {
			callData.set(CallDataAttribute.P_TRACE_CONSTRAINT_ID, constraintIdList);
			callData.set(CallDataAttribute.P_TRACE_FLAG, PhConstants.TRUE);
			callData.set(CallDataAttribute.P_TRACE_MESSAGE, new StringBuilder());
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ " :: [PH] constraintId="
					+ constraintIdList
					+ ", TraceFlag="
					+ Boolean.valueOf((String) callData
							.get(CallDataAttribute.P_TRACE_FLAG)));
			logger.debug(dialogId
					+ " [PH]::processIdp return and start processing");
		}

		return null;
	}



	/**
	 * This method is called by the Protocol handler whenever an ENC is
	 * received.
	 * 
	 * @param invokeIndEvent
	 *            represents the instance of InvokeIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] processErb(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processErb");
		}

		/*
		 * parse ERB
		 */
		try {
			Capv2ScfProtocolParser.parseErbcsm(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in ERB.", ape);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ERB_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in ERB.",
					pore);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ERB_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ERB);

		}

		if (logger.isDebugEnabled()) {
			logger.debug("Exiting processErb() .....");
		}
		return null;
	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of PRIMITIVE_RESULT type is received.
	 * 
	 * @param resultIndEvent
	 *            represents an instance of ResultIndEvent
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action Object
	 * @throws Exception 
	 * @throws ParameterOutOfRangeException 
	 * @throws CriticalityTypeException 
	 * @throws ASNParsingException 
	 */
	static Action[] processResult(ResultIndEvent resultIndEvent,
			TcapSession tcapSession) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processResult");
		}

		byte[] operCode = null;
		Action[] action = null;
		if (resultIndEvent.isLastResultEvent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: It is last result event");
			}
			/*
			 * check if opcode present in result if yes get opercode directly
			 */
			if (resultIndEvent.isOperationPresent()) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Operation Present in event");
				}
				try {
					operCode = resultIndEvent.getOperation().getOperationCode();
				} catch (MandatoryParameterNotSetException e) {
					logger.error(dialogId + ":: Error getting operation code "
							+ e.getMessage());
					throw e;
				} catch (ParameterNotSetException e) {
					logger.warn(dialogId + ":: Error getting operation code "
							+ e.getMessage());
					if (logger.isInfoEnabled()) {
						logger.warn(dialogId
								+ ":: Error getting operation code", e);
						logger.info(dialogId
								+ ":: IGNORE ParameterNotSetException in getOperation.");
					}
				}
			}
			else if (resultIndEvent.isInvokeIdPresent()) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Invoke Id present in event");
				}

				try {
					int resultInvokeId = resultIndEvent.getInvokeId();
					Integer atInvokeId = (Integer) callData.get(CallDataAttribute.P_AT_INVOKE_ID);
					Integer pacInvokeId = (Integer) callData.get(CallDataAttribute.PROMPT_COLLECT_INVOKE_ID);

					if (atInvokeId != null && atInvokeId == resultInvokeId) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: It is Activity Test Event");
						}

						operCode = new byte[1];
						operCode[0] = CapV2OpCodes.ACTIVITY_TEST_BYTE;
					} else if (pacInvokeId != null && pacInvokeId == resultInvokeId) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: It is Prompt & Collect Event");
						}
						operCode = new byte[1];
						operCode[0] = CapV2OpCodes.PROMPT_COLLECT_USER_INFO_BYTE;
					} else {
						logger.warn("[PH]:: Unknown invoke id received: " + resultInvokeId +
								". Valid invokeIds in current context: [" + atInvokeId + "," + pacInvokeId + "]");
					}
				} catch (ParameterNotSetException e) {
					logger.warn(dialogId + ":: Error getting invoke id "
							+ e.getMessage());
					if (logger.isInfoEnabled()) {
						logger.warn(dialogId + ":: Error getting invoke id", e);
						logger.debug(
								dialogId
								+ ":: IGNORE ParameterNotSetException in getInvokeId.",
								e);
					}
				}
			}
			/*
			 * if opcode is still null
			 */
			if (operCode != null) {
				String operCodeStr = CommonUtils.formatBytes(operCode);
				byte operCodeByte = operCode[0];
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Operation Code is "
							+ operCodeStr);
				}

				switch (operCodeByte) {
				case CapV2OpCodes.ACTIVITY_TEST_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Cancel the Activity Test Acknowledgement Timer");
					}
					cancelATTimer(tcapSession);
					break;
				}
				case CapV2OpCodes.PROMPT_COLLECT_USER_INFO_BYTE: {
					Capv2ScfProtocolParser.parsePACResult(resultIndEvent, callData);
					Event event = new Event(EventType.EVENT_PNC_SUCCESS, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG1.name());
					try {
						ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
					} catch (Exception e) {
						logger.error("[PH]:: Exception thrown while parsing PAC result");
					}
				}
				break; 
				default: {
					logger.warn(dialogId
							+ ":: Received Result for unknown Component Indication Event "
							+ operCodeStr);
					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_CODE,
							Capv2ScfRelReasonCode.UNKNOWN_RESULT);
					action = getUnknownResultAction(tcapSession);
					break;
				}// end default
				}// end switch
			} else {
				logger.warn(dialogId + "::Result Operation code is unknown.");
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.UNKNOWN_RESULT);
				action = getUnknownResultAction(tcapSession);
			}
		}
		logger.error("Not supported processResult() .....");
		return null;
	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of PRIMITIVE_ERROR type is received.
	 * 
	 * @param errorIndEvent
	 *            represents an instance of ErrorIndEvent
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action Objects.
	 */
	static Action[] processError(ErrorIndEvent errorIndEvent,
			TcapSession tcapSession) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processError");
		}

		int invokeId = -1;
		byte[] errorCode = null;

		/*
		 * setting DFC to false as CAP call is failed
		 */
		callData.set(CallDataAttribute.P_DFC_REQUIRED_FLAG,
				PhConstants.FALSE);

		try {
			invokeId = errorIndEvent.getInvokeId();
			errorCode = errorIndEvent.getErrorCode();
		} catch (MandatoryParameterNotSetException mpne) {
			logger.error(dialogId
					+ ":: MandatoryParameterNotSetException in RE", mpne);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ERR_MSG_MAND_PARAM_MISSING);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UERROR,
					FAILTYPE.DEFAULT);
		} catch (ParameterNotSetException pne) {
			logger.error(dialogId + ":: ParameterNotSetException in RE", pne);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ERR_MSG_PARAM_NOT_SET);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UERROR,
					FAILTYPE.DEFAULT);
		}

		int lastInvokeIdStart = Capv2CS1ScfProtocolUtil
				.getLastInvokeIdStartRange(callData);
		int lastInvokeIdEnd = Capv2CS1ScfProtocolUtil
				.getLastInvokeIdEndRange(callData);

		// PlayandCollect - we may recieve Error Code 
		Integer pacInvokeId = (Integer) callData.get(CallDataAttribute.PROMPT_COLLECT_INVOKE_ID);

		/*
		 * validateInvokeId
		 */
		boolean found=false;
		if(pacInvokeId != null && pacInvokeId ==  invokeId){
			found=true;
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);		
			CapV2CallStates callstate = (CapV2CallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
			if(logger.isDebugEnabled()){
				logger.debug(dialogId + ":: error received for PAC as last operation" +
						": current state:" + callstate);
			}

			Event event = new Event(EventType.EVENT_PNC_FAILURE, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG1.name());
			try {
				ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
			} catch (Exception e) {
				logger.error("[PH]:: Exception thrown while parsing PAC result");
			}

		}else if ((invokeId < lastInvokeIdStart) || (invokeId > lastInvokeIdEnd)) {
			logger.warn(dialogId + "::Invoke id invalid in U error; recived:"
					+ invokeId + " valid range for current message:: "
					+ lastInvokeIdStart + " to " + lastInvokeIdEnd);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ERR_MSG_INVALID_INVOK_ID);
			return getInvalidInvokeIdAction(tcapSession, MESSAGE.UERROR);
		}

		Action action = null;
		if(!found){
			if (errorCode != null && logger.isDebugEnabled()) {
				String errorCodeStr = CommonUtils.formatBytes(errorCode);
				logger.debug(dialogId + ":: Error code is " + errorCodeStr);

			}

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Return action to Drop the call");
			}
			action = new Action(Action.ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			/*
		 	* set cause as temporary failure
		 	*/
			action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
		}

		if(action != null){
			return (new Action[] { action });
		}else{
			return null;
		}
	}

	/**
	 * This method is used to Cancel the Activity Test Acknowledgment Timer.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 */
	private static void cancelATTimer(TcapSession tcapSession) {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside cancelATTimer");
		}
		Capv2CS1ScfProtocolUtil.stopTimer(tcapSession, PhConstants.AT_ACK_TIMER);

	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of type PRIMITIVE_REJECT is received.
	 * 
	 * @param rejectIndEvent
	 *            represents the instance of RejectIndEvent
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	public static Action[] processReject(RejectIndEvent rejectIndEvent,
			TcapSession tcapSession) {

		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processReject");
		}
		int rejectType = -1;
		int problemType = -1;
		int problem = -1;

		/*
		 * setting DFC to false as CAP call is failed
		 */
		callData.set(CallDataAttribute.P_DFC_REQUIRED_FLAG,
				PhConstants.FALSE);

		/*
		 * get reject type
		 */
		if (rejectIndEvent.isRejectTypePresent()) {
			try {
				rejectType = rejectIndEvent.getRejectType();
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId + ":: Error in getting reject type "
						+ e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + ":: Error getting reject type", e);
					logger.info(dialogId
							+ ":: IGNORE ParameterNotSetException in getting reject type.");
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Reject Type is [" + rejectType
						+ "]");
			}
		}

		int invokeId = -1;
		try {
			invokeId = rejectIndEvent.getInvokeId();
			/*
			 * get problem type and problem
			 */
			problemType = rejectIndEvent.getProblemType();
			problem = rejectIndEvent.getProblem();

		} catch (MandatoryParameterNotSetException mpne) {
			logger.error(dialogId
					+ ":: MandatoryParameterNotSetException in UREJECT", mpne);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.REJECT_MSG_MAND_PARAM_MISSING);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UREJECT,
					FAILTYPE.DEFAULT);
		} catch (ParameterNotSetException pne) {
			logger.error(dialogId + ":: ParameterNotSetException in UREJECT",
					pne);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.REJECT_MSG_PARAM_NOT_SET);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UREJECT,
					FAILTYPE.DEFAULT);
		}

		/*
		 * validateInvokeId
		 */

		int lastInvokeIdStart = Capv2CS1ScfProtocolUtil
				.getLastInvokeIdStartRange(callData);
		int lastInvokeIdEnd = Capv2CS1ScfProtocolUtil
				.getLastInvokeIdEndRange(callData);
		if ((invokeId < lastInvokeIdStart) || (invokeId > lastInvokeIdEnd)) {
			logger.error(dialogId + "::Invoke id invalid in U Reject; recived:"
					+ invokeId + " valid range for current message:: "
					+ lastInvokeIdStart + " to " + lastInvokeIdEnd);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.REJECT_INVALID_INVOK_ID);
			return getInvalidInvokeIdAction(tcapSession, MESSAGE.UREJECT);
		}

		/*
		 * validate problem type
		 */
		if (problemType != ComponentConstants.PROBLEM_TYPE_INVOKE) {
			logger.error(dialogId
					+ "::Problem type invalid in U reject; recived:" + invokeId
					+ " problemType:" + problemType + " Expected problem type:"
					+ ComponentConstants.PROBLEM_TYPE_INVOKE);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.REJECT_INVALID_PROBLEM_TYPE);
			return getInvalidProblemAction(tcapSession, MESSAGE.UREJECT);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Problem Type is [" + problemType
					+ "]  problem is  [" + problem + "]");
		}

		Action action = new Action(Action.ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.NULL_END);
		action.setReleaseCauseValue(41);

		return (new Action[] { action });
	}

	/**
	 * This method is for handling any unknown dialogue events. It just ignores
	 * unknown dialog events returning null.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action objects.
	 */
	static Action[] getUnknownDialogAction(TcapSession tcapSession) {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] Inside getUnknownDialogAction:: "
					+ "ignore unknown dialog events");
		}
		/*
		 * ignore unknown dialog events returning null.
		 */
		return null;

	}

	/**
	 * This method is for handling any unknown result events. It just ignores
	 * unknown Result opcode.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action objects.
	 */
	static Action[] getUnknownResultAction(TcapSession tcapSession) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getUnknownResultAction:: "
					+ "Drop call with U-Reject");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
		action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_RETURN_RESULT_UNEXPECTED);

		return (new Action[] { action });

	}

	/**
	 * This method returns action to be taken when an out of sequence dialogue
	 * indication event is received. In case TC_END is not received just release
	 * the call with CV=41.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param dialogType
	 *            represents an integer representation of dialogue type.
	 * @return an array of Action Objects
	 */
	static Action[] getOutOfSequenceDialogAction(TcapSession tcapSession,
			int dialogType) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getOutOfSequenceDialogAction:: Drop Call");
		}
		Action action = null;

		CapV2CallStates callState = (CapV2CallStates) legData
				.get(LegDataAttributes.P_LEG_SS7_STATE);
		if (callState == CapV2CallStates.INIT) {
			/*
			 * send UAbort for terminated calls
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ ":: [PH]Last Rx dlg not END do release calls:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			/*
			 * set reason as no reason given
			 */
			action.setReleaseCauseValue(AbortInfoEnum.NO_REASON_GIVEN.getCode());
		} else {
			switch (dialogType) {
			case TcapConstants.PRIMITIVE_BEGIN:
			case TcapConstants.PRIMITIVE_CONTINUE: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: [PH] Inside handleOutOfSequenceDialog:: unknown begin/continue");
				}
				action = new Action(ActionType.ACTION_END_CALL);
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Temporary_failure
						.getCode());
				break;
			}
			default: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: [PH] Inside handleOutOfSequenceDialog:: unknown dialog "
							+ dialogType);
				}
				action = new Action(ActionType.ACTION_END_CALL);
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Temporary_failure
						.getCode());
				break;
			}
			}
		}
		return (new Action[] { action });
	}

	/**
	 * This method returns action to be taken when an out of sequence invoke
	 * indication event is received.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param message
	 *            represents an instance of MESSAGE
	 * @return an array of Action Objects.
	 */
	static Action[] getInvalidInvokeIdAction(TcapSession tcapSession,
			MESSAGE message) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getInvalidInvokeIdAction with message::"
					+ message);
		}

		Action[] actionArr = null;

		switch (message) {
		case UERROR: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_UNRECOGNIZED_INVOKE_ID);
			actionArr = new Action[] { action };
			break;
		}
		case UREJECT: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_COMPONENT);
			actionArr = (new Action[] { action });
			break;
		}

		}// @end switch

		return actionArr;
	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of type PRIMITIVE_REJECT is received and problem received is
	 * invalid.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param message
	 *            represents an instance of MESSAGE
	 * @return an array of Action Objects
	 */
	static Action[] getInvalidProblemAction(TcapSession tcapSession,
			MESSAGE message) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]:: Inside getInvalidProblemAction with message::"
					+ message);
		}
		Action[] actionArr = null;

		switch (message) {
		case UREJECT: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_COMPONENT);
			actionArr = (new Action[] { action });
			break;
		}

		}// @end switch

		return actionArr;
	}

	/**
	 * Method to retrieve actions to be performed on encountering ASNParsing
	 * Exception.
	 * 
	 * @param invokeIndEvent
	 *            represents an instance of InvokeIndEvent
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param ape
	 *            represents an instance of ASNParsingException
	 * @return an array of Action objects.
	 */
	static Action[] getASNParsingFailureAction(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession, ASNParsingException ape) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: Inside handleASNParsingFailure with exception::"
					+ ape);
		}
		MESSAGE message = ape.getInapMessage();
		return getASNParsingFailureAction(tcapSession, message,
				ape.getParseFailType());

	}

	/**
	 * Method to retrieve actions to be performed on encountering ASNParsing
	 * Exception.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param message
	 *            represents an instance of MESSAGE
	 * @param failtype
	 *            represents an instance of FAILTYPE
	 * @return an array of Action objects.
	 */
	static Action[] getASNParsingFailureAction(TcapSession tcapSession,
			MESSAGE message, FAILTYPE failtype) {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH] :: Inside getASNParsingFailureAction with message::"
					+ message + "   failtype::" + failtype);
		}
		Action[] actionArr = null;

		switch (message) {
		case IDP: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			switch (failtype) {
			case CPC_MISSING: {
				action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
				action.setReleaseCauseValue(AbortInfoEnum.ABNORMAL_PROCESSING
						.getCode());
				break;
			}
			case TMR_MISSING:
			case ACPC_MISSING: {
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Temporary_failure
						.getCode());
				break;
			}
			case FCI_MISSING: {
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Service_not_available
						.getCode());
				break;
			}
			case DEFAULT:
			default: {
				action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
				action.setReleaseCauseValue(AbortInfoEnum.NO_REASON_GIVEN
						.getCode());
				break;
			}
			}
			actionArr = (new Action[] { action });
			break;
		}
		case UERROR: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_PARAMETER);
			actionArr = (new Action[] { action });
			break;
		}
		case UREJECT: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_COMPONENT);
			actionArr = (new Action[] { action });
			break;
		}
		default: {
			/*
			 * reject as continue
			 */
			Action action1 = new Action(ActionType.ACTION_CONTINUE);
			action1.setContinueMode(CONTINUE_MODE.USER_REJECT);
			action1.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_PARAMETER);

			/*
			 * set Release cause as tc end
			 */
			Action action2 = new Action(ActionType.ACTION_END_CALL);
			action2.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);

			actionArr = (new Action[] { action1, action2 });
			break;
		}
		}// @end switch

		return actionArr;
	}

	/**
	 * This method is called by the Protocol handler whenever
	 * CriticalityTypeException is thrown while parsing IDP received.
	 * 
	 * @param invokeIndEvent
	 *            represents an instance of InvokeIndEvent
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param cte
	 *            represents an instance of CriticalityTypeException
	 * @return an array of Action objects.
	 * @throws Exception
	 */
	static Action[] getInvalidExtensionTypeAction(
			InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CriticalityTypeException cte) throws Exception {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getInvalidExtensionTypeAction:: Drop call");
		}
		Action action = new Action(ActionType.ACTION_END_CALL);
		logger.warn(cte.getMessage());
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] cte error", cte);
		}
		if (cte.getCriticality().equals(CRITICALITY.IGNORE)) {
			/*
			 * this case is never reached as we continue on ignore
			 */
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			action.setReleaseCauseValue(CauseValEnum.Mandatory_information_element_missing
					.getCode());
		} else if (cte.getCriticality().equals(CRITICALITY.ABORT)) {
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(AbortInfoEnum.UNRECOGNIZED_EXTENSION_PARAMETER
					.getCode());
		}

		return (new Action[] { action });
	}

	/**
	 * This method is called by the Protocol handler whenever
	 * ParameterOutOfRangeException is thrown while parsing CAP signal
	 * received.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param message
	 *            represents an instance of MESSAGE
	 * @return an array of Action objects.
	 */
	static Action[] getOutOfRangeParamterAction(TcapSession tcapSession,
			MESSAGE message) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getOutOfRangeParamterAction::" + message);
		}

		/*
		 * handle as ASN parsing
		 */
		return getASNParsingFailureAction(tcapSession, message,
				FAILTYPE.DEFAULT);

	}

	/**
	 * This method return actions to be performed to Protocol handler when
	 * calling party is missing from IDP.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] getCallingPartyMissingAction(TcapSession tcapSession)
			throws Exception {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getCallingPartyMissingAction:: Drop call");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
		return (new Action[] { action });

	}

	/**
	 * This method return actions to be performed to Protocol handler when
	 * unknown message is received.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param event
	 *            represents an instance of CAPEvent
	 * @return an array of Action Objects
	 */
	static Action[] getUnknownMessageAction(TcapSession tcapSession,
			Capv2ScfProtocolEvent event) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getUnknownMessageAction:: same as handleOutOfSequenceMsg");
		}
		return getOutOfSequenceMsgAction(tcapSession, event);

	}

	/**
	 * This method return actions to be performed to Protocol handler when
	 * unarmed ERB event is received.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param event
	 *            represents an instance of CAPEvent
	 * @return an array of Action Objects
	 */
	static Action[] getUnArmedErbEventAction(TcapSession tcapSession,
			Capv2ScfProtocolEvent event) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getUnArmedErbEventAction send reject and RC");
		}

		/*
		 * reject as continue
		 */
		Action action1 = new Action(ActionType.ACTION_CONTINUE);
		action1.setContinueMode(CONTINUE_MODE.USER_REJECT);
		action1.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_PARAMETER);

		/*
		 * release cause as tc end
		 */
		Action action2 = new Action(ActionType.ACTION_END_CALL);
		action2.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		action2.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());

		Action[] actionArr = (new Action[] { action1, action2 });

		return actionArr;

	}

	/**
	 * This method return actions to be performed to Protocol handler when out
	 * of sequence message is received.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param event
	 *            represents an instance of CAPEvent
	 * @return an array of Action Objects
	 */
	static Action[] getOutOfSequenceMsgAction(TcapSession tcapSession,
			Capv2ScfProtocolEvent event) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getOutOfSequenceMsgAction:: Drop call");
		}

		/*
		 * release call if end dialog not exchanged yet.
		 */
		Action action = null;
		CapV2CallStates callState = (CapV2CallStates) legData
				.get(LegDataAttributes.P_LEG_SS7_STATE);
		if (callState == CapV2CallStates.INIT) {
			/*
			 * send UAbort for terminated calls
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ "[PH] ::Last Rx dlg not END do release calls:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			/*
			 * set reason as no reason given
			 */
			action.setReleaseCauseValue(AbortInfoEnum.NO_REASON_GIVEN.getCode());
		} else if (callState != CapV2CallStates.TERMINATED) {
			/*
			 * check if it was unarmed event case or event in invalid call state
			 */
			Object unarmedEventError = tcapSession
					.getAttribute(PhConstants.UNARMED_ERROR_TYPE);
			if (unarmedEventError != null) {
				/*
				 * case of unArmed ERB event
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + "::Unarmed ERB event:: ");
				}

				return getUnArmedErbEventAction(tcapSession, event);
			}

			/*
			 * send release call for in progress calls
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ ":: [PH] Last call state is not terminated create action:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			action.setReleaseCauseValue(CauseValEnum.Temporary_failure
					.getCode());

		} else {
			/*
			 * send UAbort for terminated calls
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ ":: [PH] Last Rx dlg not END do release calls:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			/*
			 * set reason as no reason given
			 */
			action.setReleaseCauseValue(1);
		}

		return (new Action[] { action });
	}

	/**
	 * method returns ER response action
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 */
	/**
	 * This method returns ER response action whenever an ER event is received.
	 * 
	 * @param invokeIndEvent
	 *            represents an instance of InvokeIndEvent
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @return an array of Action objects
	 */
	static Action[] getEntityReleasedAction(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession) {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]::Inside getEntityReleasedAction:: Drop call");
		}
		Action action = null;

		/*
		 * setting DFC to false as CAP call si cleaned
		 */
		callData.set(CallDataAttribute.P_DFC_REQUIRED_FLAG,
				PhConstants.FALSE);
		/*
		 * parses and store cv from ER
		 */
		try {
			Capv2ScfProtocolParser.parseEntityRelease(invokeIndEvent, callData);
		} catch (Exception e) {
			logger.error(dialogId
					+ "::  [PH] cause value pasring from ER failed", e);
			/*
			 * using default as message type as ER is not supported and rare
			 * message
			 */
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ER_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(tcapSession, MESSAGE.DEFAULT,
					FAILTYPE.DEFAULT);
		}

		action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());

		return (new Action[] { action });

	}

	/**
	 * In case of drop call event, this method sends appropriate CAP message to
	 * the switch.
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param action
	 *            represents an instance of Action
	 * @param cCallTraceService
	 *            represents an instance of CallTraceService
	 * @throws Exception
	 */
	public static void sendDropMessage(TcapSession tcapSession, Action action,
			CallTraceService cCallTraceService) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside sendDropMessage");
		}
		DROP_CALL_MODE dropCallMode = action.getDropCallMode();

		/*
		 * set cause value for CDRS
		 */
		setCallDataParamsForCDR(callData, action);

		switch (dropCallMode) {
		case INITIAL_ERROR: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " ::[PH] Send RETURN_ERROR with TC_END");
			}

			sendErrorReqEvent(tcapSession, action, cCallTraceService);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		}
		case RELEASE_CALL: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH]  Send FCI+REL with TC_END");
			}
			//	sendFci(tcapSession, cCallTraceService);
			sendReleaseCall(tcapSession, action, cCallTraceService);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		}
		case USER_ABORT: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH]  Send USER_ABORT");
			}
			sendUAbortRequestEvent(tcapSession, action, cCallTraceService);
			break;
		}
		case USER_REJECT: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH]  Send U_REJECT with TC_CON");
			}
			sendRejectReqEvent(tcapSession, action, cCallTraceService);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		}
		case NULL_END: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH]  Send NULL_END");
			}
			sendEndRequestEvent(tcapSession, false, cCallTraceService);

			break;
		}
		case NULL_END_PREARRANGED: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH]  Send NULL_END_PREARRANGED");
			}
			sendEndRequestEvent(tcapSession, true, cCallTraceService);

			break;
		}
		case CANCEL_NULL_END_PREARRANGED: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH]  Send CANCEL_NULL_END_PREARRANGED, send cancel+Continue");
			}

			// send Cancel and Continue followed by pre-arranged end
			sendCancel(tcapSession, callData, action);
			sendContinueComponent(tcapSession, callData, action);
			DialogueReqEvent dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
			sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);

			// send preaggarnged end
			sendEndRequestEvent(tcapSession, true, cCallTraceService);

			break;
		}
		case NONE: {
			if (logger.isInfoEnabled()) {
				logger.info(dialogId
						+ " :: [PH]  Do nothing for drop call mode NONE");
			}
			break;
		}
		}// @end switch

	}

	/**
	 * This method is called from sendDropMessage, and is responsible for
	 * setting relevant information regarding CDR into CallData object being
	 * passed.
	 * 
	 * @param callData
	 *            represents an instance of CallData
	 * @param action
	 *            represents an instance of Action
	 */
	public static void setCallDataParamsForCDR(CallData callData, Action action) {
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);

		DROP_CALL_MODE dropCallMode = action.getDropCallMode();
		int releaseCauseValue = action.getReleaseCauseValue();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ ":: [PH]  setCallDataParamsForCDR:: Drop call mode is "
					+ dropCallMode);
		}

		/*
		 * setting cause value
		 */
		switch (dropCallMode) {
		case RELEASE_CALL: {
			String valueFromMsg = (String) callData
					.get(CallDataAttribute.P_CAUSE_VALUE_FROM_MSG);

			if (!(Boolean.valueOf(valueFromMsg))) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: [PH]  Set cause value from action "
							+ action.getReleaseCauseValue());
				}
				// cause value will be set by an application
				//				callData.set(
				//						CallDataAttribute.NP_RELEASE_REASON_VALUE,
				//						action.getReleaseCauseValue());
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: [PH]  Cause value in callData is "
							+ releaseCauseValue);
				}
			}
			break;
		}
		case NULL_END: {
			if (releaseCauseValue <= 0 && action.getReleaseCauseValue() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: [PH]  Set release cause value from action to "
							+ action.getReleaseCauseValue());
				}
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE,
						action.getReleaseCauseValue());
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: [PH]  Release cause value in callData is "
							+ releaseCauseValue);
				}
			}
			break;
		}
		case INITIAL_ERROR: {
			if (action.getReleaseCauseValue() == 6) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " :: [PH]  Set attempted indicator to 5 as action cause value is 6");
				}
			}
			/*
			 * don't break as further processing is required
			 */
		}
		case USER_ABORT:
		case USER_REJECT:
		case NULL_END_PREARRANGED:
		case NONE:
		default: {
			if (releaseCauseValue <= 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Set release cause value to 41");
				}
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Release cause value in callData is "
							+ releaseCauseValue);
				}
			}
			break;
		}// end initial error,uabort,default, u reject,nullend prearranged,none

		}// @end switch

	}

	/**
	 * This method is called by protocol handler for sending continue message.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	public static void sendContinueMessage(TcapSession tcapSession,
			Action action, CallTraceService cCallTraceService) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside sendContinueMessage::");
		}
		CONTINUE_MODE continueMode = action.getContinueMode();

		switch (continueMode) {
		case INITIAL_ERROR: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: Send RETURN_ERROR with TC_CON");
			}
			sendErrorReqEvent(tcapSession, action, cCallTraceService);
			sendContinueRequestEvent(tcapSession, cCallTraceService);
			break;
		}
		case USER_REJECT: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: Send U_REJECT with TC_CON");
			}
			sendRejectReqEvent(tcapSession, action, cCallTraceService);
			sendContinueRequestEvent(tcapSession, cCallTraceService);
			break;
		}

		}
	}

	/**
	 * This method is called by protocol handler for sending error request
	 * event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	static void sendErrorReqEvent(TcapSession tcapSession, Action action,
			CallTraceService cCallTraceService) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendErrorReqEvent");
		}

		byte[] reason = new byte[] { 0x07 };
		if (action.getReleaseCauseValue() > 0) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " [PH]:: Got custom reason for u error::"
						+ action.getReleaseCauseValue());
			}
			reason = new byte[] { (byte) action.getReleaseCauseValue() };
		}

		ErrorReqEvent errorReqEvent = new ErrorReqEvent(src,
				tcapSession.getDialogueId(), ComponentConstants.ERROR_LOCAL,
				reason);

		errorReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getLastRxInvokeId(callData));

		sendComponentReq(errorReqEvent, callData, cCallTraceService);
	}

	/**
	 * This method is called by protocol handler for sending Reject request
	 * event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	@SuppressWarnings("deprecation")
	private static void sendRejectReqEvent(TcapSession tcapSession,
			Action action, CallTraceService cCallTraceService) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendRejectReqEvent");
		}

		RejectReqEvent rejectReqEvent = new RejectReqEvent(src);
		rejectReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getLastRxInvokeId(callData));
		rejectReqEvent.setDialogueId(tcapSession.getDialogueId());

		/*
		 * reject type from service will always be user
		 */
		rejectReqEvent.setRejectType(ComponentConstants.REJECT_TYPE_USER);

		int problem = action.getReleaseCauseValue();

		rejectReqEvent.setProblem(problem);

		sendComponentReq(rejectReqEvent, callData, cCallTraceService);

	}

	/**
	 * This method is called by protocol handler for sending UAbort request
	 * event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	static void sendUAbortRequestEvent(TcapSession tcapSession, Action action,
			CallTraceService cCallTraceService) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: Inside sendUAbortRequestEvent");
		}

		UserAbortReqEvent uAbortReqEvent = new UserAbortReqEvent(src,
				tcapSession.getDialogueId());

		int reason = action.getReleaseCauseValue();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: [PH] Set U-Abort Reason=" + reason);
		}
		byte[] infoBytes = null;

		/*
		 * set reason and prepapre info bytes
		 */
		switch (AbortInfoEnum.fromInt(reason)) {
		case NO_REASON_GIVEN:
		case APPLICATION_TIMER_EXPIRED:
		case PROTOCOL_PROHIBITED_SIGNAL_RECIEVED:
		case ABNORMAL_PROCESSING:
		case CONGESTION:
		case UNRECOGNIZED_EXTENSION_PARAMETER: {
			infoBytes = getInformationBytes(reason);
			uAbortReqEvent
			.setAbortReason(DialogueConstants.ABORT_REASON_USER_SPECIFIC);
			break;
		}
		case AC_NEGOTIATION_FAILED: {
			infoBytes = getInformationBytes(reason);
			uAbortReqEvent
			.setAbortReason(DialogueConstants.ABORT_REASON_ACN_NOT_SUPPORTED);
			break;
		}
		default: {
			infoBytes = getInformationBytes(AbortInfoEnum.NO_REASON_GIVEN
					.getCode());
			uAbortReqEvent
			.setAbortReason(DialogueConstants.ABORT_REASON_USER_SPECIFIC);
			break;
		}

		}

		uAbortReqEvent.setUserAbortInformation(infoBytes);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		DialoguePortion dialoguePortion = (DialoguePortion) legData
				.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: set dialogue portion");
			}
			uAbortReqEvent.setDialoguePortion(dialoguePortion);
		}
		sendDialogueReq(uAbortReqEvent, callData, cCallTraceService);
	}

	/**
	 * Prepare Uabort INfo with hardocded object identifier
	 * 
	 * @param code
	 *            represents integer representation of UAbort Info
	 * @return an instance of byte[]
	 */
	private static byte[] getInformationBytes(int code) {
		byte[] uAbortInfo = new byte[] { (byte) 0x28, (byte) 0x0F, (byte) 0x06,
				(byte) 0x08, (byte) 0x02, (byte) 0x83, (byte) 0x38,
				(byte) 0x66, (byte) 0x03, (byte) 0x02, (byte) 0x06,
				(byte) 0x00, (byte) 0xA0, (byte) 0x03, (byte) 0x0A,
				(byte) 0x01, (byte) 0x01 };

		int pos = uAbortInfo.length - 1;
		uAbortInfo[pos] = (byte) code;
		return uAbortInfo;

	}

	/**
	 * This method is for creating FCI
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param connectType
	 *            represents CONNECT_TYPE
	 * @throws Exception
	 */
	public static void sendFci(TcapSession tcapSession,
			CallTraceService cCallTraceService) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendFci");
		}

		byte[] fci = Capv2ScfProtocolParser.createFci(callData);

		byte[] fciOpCode = { CapV2OpCodes.FURNISH_CHARGING_INFORMATION_BYTE };

		Operation fciOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				fciOpCode);

		InvokeReqEvent fciInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), fciOperation);
		fciInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getNextInvokeId(callData));
		fciInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, fci));
		fciInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(fciInvokeReqEvent, callData, cCallTraceService);

	}

	/**
	 * This method is called by protocol handler for sending Release Call.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	static void sendReleaseCall(TcapSession tcapSession, Action action,
			CallTraceService cCallTraceService) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendReleaseCall");
		}

		byte[] releaseCall = Capv2ScfProtocolParser.createReleaseCall(callData, action);

		byte[] rcOpCode = { CapV2OpCodes.RELEASE_CALL_BYTE };

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				rcOpCode);

		InvokeReqEvent rcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rcOperation);
		rcInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		rcInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, releaseCall));
		rcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(rcInvokeReqEvent, callData, cCallTraceService);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Exit sendReleaseCall");
		}
	}



	/**
	 * This method is called by protocol handler for sending END request event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param preArrangedEnd
	 *            represents the instance of boolean
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendEndRequestEvent(TcapSession tcapSession,
			boolean preArrangedEnd, CallTraceService cCallTraceService)
					throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendEndRequestEvent");
		}

		EndReqEvent endReqEvent = new EndReqEvent(src,
				tcapSession.getDialogueId());

		if (preArrangedEnd) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Send Pre-arranged END");
			}

			endReqEvent.setTermination(DialogueConstants.TC_PRE_ARRANGED_END);
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Send Basic END");
			}

			endReqEvent.setTermination(DialogueConstants.TC_BASIC_END);
		}

		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		DialoguePortion dialoguePortion = (DialoguePortion) legData
				.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Set Dialogue portion");
			}
			endReqEvent.setDialoguePortion(dialoguePortion);
		}

		sendDialogueReq(endReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendEndRequestEvent");
		}

		Capv2CS1ScfProtocolUtil.setLastInvokeIdStartRange(
				Capv2CS1ScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1,
				callData);
		Capv2CS1ScfProtocolUtil.setLastInvokeIdEndRange(
				Capv2CS1ScfProtocolUtil.getLastInvokeId(callData), callData);
	}

	/**
	 * This method is called by protocol handler for sending continue request
	 * event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendContinueRequestEvent(TcapSession tcapSession,
			CallTraceService cCallTraceService) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendContinueRequestEvent");
		}

		ContinueReqEvent continueReqEvent = new ContinueReqEvent(src,
				tcapSession.getDialogueId());
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		SccpUserAddress sua = (SccpUserAddress) legData
				.get(LegDataAttributes.P_SUA);
		continueReqEvent.setOriginatingAddress(sua);
		DialoguePortion dialoguePortion = (DialoguePortion) legData
				.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			continueReqEvent.setDialoguePortion(dialoguePortion);
		}

		sendDialogueReq(continueReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendContinueRequestEvent");
		}

		Capv2CS1ScfProtocolUtil.setLastInvokeIdStartRange(
				Capv2CS1ScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1,
				callData);
		Capv2CS1ScfProtocolUtil.setLastInvokeIdEndRange(
				Capv2CS1ScfProtocolUtil.getLastInvokeId(callData), callData);
	}

	/**
	 * This method is called by protocol handler for sending CAP component
	 * indication event.
	 * 
	 * @param cre
	 *            represents the instance of ComponentReqEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendComponentReq(ComponentReqEvent cre, CallData callData,
			CallTraceService cCallTraceService) throws Exception {

		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendComponentReq");
		}

		PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider()
		.sendComponentReqEvent(cre);

		traceComponent(cre, callData);

	}

	/**
	 * This method will trace outgoing CAP component; We catch inside try catch
	 * throwable to avoid any impact on call in case of error;
	 * 
	 * @param cre
	 *            represents the instance of ComponentReqEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */

	static void traceComponent(ComponentReqEvent cre, CallData callData) {
		try {
			CallTraceService cCallTraceService = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			String isTraceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);

			if (PhConstants.TRUE.equals(isTraceFlag)) {
				int primitive = cre.getPrimitiveType();

				StringBuilder traceMsg = new StringBuilder();

				traceMsg.append(">>>>Sending>>>>");
				traceMsg.append("\n");

				traceMsg.append("INVOKE ID::");
				traceMsg.append(cre.getInvokeId());
				traceMsg.append("\n");

				traceMsg.append("PRIMITIVE::");

				int callState = CallTraceService.CALL_IN_PROGRESS;

				switch (primitive) {
				case TcapConstants.PRIMITIVE_INVOKE: {
					traceMsg.append("INVOKE");
					traceMsg.append("\n");

					byte[] operCode = ((InvokeReqEvent) cre).getOperation()
							.getOperationCode();
					byte operCodeByte = operCode[0];
					traceMsg.append("MESSAGE::");
					switch (operCodeByte) {
					case CapV2OpCodes.IDP_BYTE: {
						traceMsg.append("IDP");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.EVENT_REPORT_BCSM_BYTE: {
						traceMsg.append("EVENT_REPORT_BCSM");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.SCI_BYTE: {
						traceMsg.append("SCI");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.RRBE_BYTE: {
						traceMsg.append("RRBE");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.CONNECT_BYTE: {
						traceMsg.append("CONNECT");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.ESTABLISH_TEMP_CONNECTION_BYTE : {
						traceMsg.append("ETC");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.DISCONNECT_FORWARD_CONNECTION_WITH_ARGS_BYTE  : {
						traceMsg.append("DFC");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.RELEASE_CALL_BYTE: {
						traceMsg.append("RELEASE_CALL");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.ACTIVITY_TEST_BYTE: {
						traceMsg.append("AT");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.DISCONNECT_FORWARD_CONNECTION_WITHOUT_ARGS_BYTE: {
						traceMsg.append("DFC_NO_ARGS");
						traceMsg.append("\n");
						break;
					}
					default: {
						traceMsg.append("OPCODE BYTE:" + operCodeByte);
						traceMsg.append("\n");
						break;
					}
					}

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((InvokeReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((InvokeReqEvent) cre)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				}// @end switch message type
				case TcapConstants.PRIMITIVE_RESULT: {
					traceMsg.append("RESULT");
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ResultReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((ResultReqEvent) cre)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
					break;
				}
				case TcapConstants.PRIMITIVE_ERROR: {
					traceMsg.append("ERROR");
					traceMsg.append("\n");

					traceMsg.append("Type::");
					traceMsg.append(((ErrorReqEvent) cre).getErrorType());
					traceMsg.append("\n");

					traceMsg.append("Code::");
					traceMsg.append(Util.formatBytes(((ErrorReqEvent) cre)
							.getErrorCode()));
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ErrorReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((ErrorReqEvent) cre)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				}
				case TcapConstants.PRIMITIVE_REJECT: {
					traceMsg.append("REJECT");
					traceMsg.append("\n");

					traceMsg.append("Type::");
					traceMsg.append(((RejectReqEvent) cre).getProblemType());
					traceMsg.append("\n");

					traceMsg.append("Problem::");
					traceMsg.append(((RejectReqEvent) cre).getProblem());
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((RejectReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((RejectReqEvent) cre)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
				}
				}// @switch primitive

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								Capv2CS1ScfProtocolUtil.getUniqueCallIDForTracing(callData),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing component req event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error in traceComponent", t);
			}
		}
	}

	/**
	 * This method will trace incoming CAP component; inside try catch
	 * throwable to avoid any impact on call in case of error;
	 * 
	 * @param cie
	 *            represents the instance of ComponentIndEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */
	static void traceComponent(ComponentIndEvent cie, CallData callData) {
		try {
			CallTraceService cCallTraceService = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			String isTraceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);

			if (PhConstants.TRUE.equals(isTraceFlag)) {
				int primitive = cie.getPrimitiveType();

				StringBuilder traceMsg = new StringBuilder();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				traceMsg.append("<<<<Recieved<<<<");
				traceMsg.append("\n");

				traceMsg.append("INVOKE ID::");
				traceMsg.append(cie.getInvokeId());
				traceMsg.append("\n");

				traceMsg.append("PRIMITIVE::");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_INVOKE: {
					traceMsg.append("INVOKE");
					traceMsg.append("\n");

					byte[] operCode = ((InvokeIndEvent) cie).getOperation()
							.getOperationCode();
					byte operCodeByte = operCode[0];
					traceMsg.append("MESSAGE::");
					switch (operCodeByte) {
					case CapV2OpCodes.IDP_BYTE: {
						/*
						 * special case where we need to trace begin also
						 */
						traceBegin(callData, cCallTraceService);
						traceMsg.append("IDP");
						traceMsg.append("\n");
						break;
					}

					case CapV2OpCodes.SCI_BYTE: {
						traceMsg.append("SCI");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.RRBE_BYTE: {
						traceMsg.append("RRBE");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.CONNECT_BYTE: {
						traceMsg.append("CONNECT");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.ESTABLISH_TEMP_CONNECTION_BYTE: {
						traceMsg.append("ETC");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.DISCONNECT_FORWARD_CONNECTION_WITH_ARGS_BYTE: {
						traceMsg.append("DFC");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.RELEASE_CALL_BYTE: {
						traceMsg.append("RELEASE_CALL");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.ACTIVITY_TEST_BYTE: {
						traceMsg.append("AT");
						traceMsg.append("\n");
						break;
					}
					case CapV2OpCodes.DISCONNECT_FORWARD_CONNECTION_WITHOUT_ARGS_BYTE: {
						traceMsg.append("DFC_NO_ARGS");
						traceMsg.append("\n");
						break;
					}
					default: {
						traceMsg.append("OPCODE BYTE:" + operCodeByte);
						traceMsg.append("\n");
						break;
					}
					}

					Object traceCall=callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {

						if (((InvokeIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((InvokeIndEvent) cie)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				}// @end switch message type
				case TcapConstants.PRIMITIVE_RESULT: {
					traceMsg.append("RESULT");
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ResultIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((ResultIndEvent) cie)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
					break;
				}
				case TcapConstants.PRIMITIVE_ERROR: {
					traceMsg.append("ERROR");
					traceMsg.append("\n");

					traceMsg.append("Type::");
					traceMsg.append(((ErrorIndEvent) cie).getErrorType());
					traceMsg.append("\n");

					traceMsg.append("Code::");
					traceMsg.append(Util.formatBytes(((ErrorIndEvent) cie)
							.getErrorCode()));
					traceMsg.append("\n");

					Object traceCall=callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ErrorIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((ErrorIndEvent) cie)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				}
				case TcapConstants.PRIMITIVE_REJECT: {
					traceMsg.append("REJECT");
					traceMsg.append("\n");

					traceMsg.append("Type::");
					traceMsg.append(((RejectIndEvent) cie).getProblemType());
					traceMsg.append("\n");

					traceMsg.append("Problem::");
					traceMsg.append(((RejectIndEvent) cie).getProblem());
					traceMsg.append("\n");

					Object traceCall = callData
							.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((RejectIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((RejectIndEvent) cie)
									.getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(Util.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
				}
				}// @switch primitive

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {

					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								Capv2CS1ScfProtocolUtil.getUniqueCallIDForTracing(callData),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing component ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error in tracing component ind event", t);
			}
		}
	}

	/**
	 * This method is used by protocol handler for sending dialogue request.
	 * 
	 * @param dre
	 *            represents the instance of DialogueReqEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendDialogueReq(DialogueReqEvent dre, CallData callData,
			CallTraceService cCallTraceService) throws Exception {

		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: Inside sendComponentReq");
		}

		incrementTcapCounters(dre);
		PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider()
		.sendDialogueReqEvent(dre);

		traceDialog(dre, callData, cCallTraceService);

	}

	/**
	 * This method will trace outgoing CAP Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error;
	 * 
	 * @param dre
	 *            represents the instance of DialogueReqEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueReqEvent dre, CallData callData,
			CallTraceService cCallTraceService) {
		try {
			String traceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);
			if (PhConstants.TRUE.equals(traceFlag)) {
				int primitive = dre.getPrimitiveType();

				StringBuilder traceMsg = new StringBuilder();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				traceMsg.append(">>>>Sending>>>>");
				traceMsg.append("\n");

				traceMsg.append("DIALOGUE::");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_BEGIN: {
					traceMsg.append("BEGIN");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_CONTINUE: {
					traceMsg.append("CONTINUE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_END: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					if (((EndReqEvent) dre).getTermination() == DialogueConstants.TC_PRE_ARRANGED_END) {
						traceMsg.append("true");
					} else {
						traceMsg.append("false");
					}
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_PROVIDER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("P-ABORT");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_USER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("U-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(Util.formatBytes(((UserAbortReqEvent) dre)
							.getUserAbortInformation()));
					traceMsg.append("\n");

					traceMsg.append("Generic Tcap Reason::");
					traceMsg.append(((UserAbortReqEvent) dre).getAbortReason());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_NOTICE: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("NOTICE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
					traceMsg.append("UNIDIRECTIONAL");
					traceMsg.append("\n");
					break;
				}
				default: {
					traceMsg.append("CODE:" + primitive);
					traceMsg.append("\n");
					break;
				}
				}// @switch primitive

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								Capv2CS1ScfProtocolUtil.getUniqueCallIDForTracing(callData),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing Dialog req event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing dialogue event", t);
			}
		}

	}

	/**
	 * This method will trace incoming CAP Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error;
	 * 
	 * @param die
	 *            represents the instance of DialogueIndEvent
	 * @param callData
	 *            represents the instance of CallData
	 * @param cCallTraceService
	 *            represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueIndEvent die, CallData callData,
			CallTraceService cCallTraceService) {

		try {

			String traceFlag = (String) callData
					.get(CallDataAttribute.P_TRACE_FLAG);
			if (traceFlag.equals(PhConstants.TRUE)) {
				int primitive = die.getPrimitiveType();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				StringBuilder traceMsg = new StringBuilder();

				traceMsg.append("<<<<Recieved<<<<");
				traceMsg.append("\n");

				traceMsg.append("DIALOGUE::");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_BEGIN: {
					traceMsg.append("BEGIN");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_CONTINUE: {
					traceMsg.append("CONTINUE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_END: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					traceMsg.append("false");
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_END_PRE_ARRANGED: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END");
					traceMsg.append("\n");

					traceMsg.append("Pre-Arranged::");
					traceMsg.append("true");
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_PROVIDER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("P-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(((ProviderAbortIndEvent) die).getPAbort());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_USER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("U-ABORT");
					traceMsg.append("\n");

					traceMsg.append("Information::");
					traceMsg.append(Util.formatBytes(((UserAbortIndEvent) die)
							.getUserAbortInformation()));
					traceMsg.append("\n");

					traceMsg.append("Generic Tcap Reason::");
					traceMsg.append(((UserAbortIndEvent) die).getAbortReason());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_NOTICE: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("NOTICE");
					traceMsg.append("\n");
					break;
				}
				case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
					traceMsg.append("UNIDIRECTIONAL");
					traceMsg.append("\n");
					break;
				}
				default: {
					traceMsg.append("CODE:" + primitive);
					traceMsg.append("\n");
					break;
				}
				}// @switch primitive

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList
							.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService
						.trace(constraintIterator.next(),
								Capv2CS1ScfProtocolUtil.getUniqueCallIDForTracing(callData),
								traceMsg.toString(), callState);
					}
				}

			}// end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing Dialog ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing dialogue ind event", t);
			}
		}

	}

	/**
	 * This method will trace incoming BEGIN Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error; invoked
	 * from trace component ind for IDP
	 * 
	 * @param callData
	 *            represents an instance of CallData
	 * @param cCallTraceService
	 *            represents an instance of CallData CallTraceService
	 */
	private static void traceBegin(CallData callData,
			CallTraceService cCallTraceService) {
		try {
			StringBuilder traceMsg = new StringBuilder();
			int callState = CallTraceService.CALL_IN_PROGRESS;
			traceMsg.append("<<<<Recieved<<<<");
			traceMsg.append("\n");

			traceMsg.append("DIALOGUE::");

			traceMsg.append("BEGIN");
			traceMsg.append("\n");

			if (callData
					.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
				List<Integer> constraintList = (List<Integer>) callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
				Iterator<Integer> constraintIterator = constraintList
						.iterator();
				while (constraintIterator.hasNext()) {
					cCallTraceService
					.trace(constraintIterator.next(),
							Capv2CS1ScfProtocolUtil.getUniqueCallIDForTracing(callData),
							traceMsg.toString(), callState);
				}
			}

		} catch (Throwable t) {
			logger.warn("Error tracing Dialog ind event BEGINS"
					+ t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing begin event", t);
			}
		}

	}



	/**
	 * This Method is used to send connect to Term party 
	 * @param tcapSession
	 * 			represents the instance of TcapSession
	 * @param callData
	 * 			represents the instance of call Data
	 * @param action
	 * @throws Exception
	 */
	public static void connectTerm(TcapSession tcapSession, CallData callData,
			Action action) throws Exception {

		Integer dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		Boolean dialoutCheck = (Boolean) callData.get(CallDataAttribute.P_DIALOUT);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if(dialoutCheck == null){
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: [PH] Setting dialout Check as False for Value null");
			}
			dialoutCheck = false;
		}

		CapV2CallStates capCallState = (CapV2CallStates) legData
				.get(LegDataAttributes.P_LEG_SS7_STATE);

		if (action.getConnectionMode() == Action.CONNECTIONMODE.EQSROUTING && dialoutCheck) {
			if(logger.isDebugEnabled()){
				logger.debug(dialogId+":: Capv2 Condition match for PSX Route");
			}
			sendSipInviteFromCap(tcapSession, callData,action);
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside connectTerm with tcapSession");
			logger.debug(dialogId + " :: CapCallState is "
					+ legData.get(LegDataAttributes.P_LEG_SS7_STATE));
		}

		switch (capCallState) {
		case SERVICE_LOGIC:
		case MS_DISCONNECTED:
		case PSX_ROUTING:
		case ASSIST:
			/*
			 *  case ASSIST:
			 */
			if (action.getConnectionMode() == CONNECTIONMODE.REROUTING) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Initiating Terminating Connection in REROUTING mode");
				}

				String capv2Flow = Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW);

				if (capv2Flow != null && StringUtils.equalsIgnoreCase(capv2Flow, "OCS")) {
					if(logger.isDebugEnabled()){
						logger.debug("SERVICE_LOGIC state: Call flow enabled for: "+ capv2Flow);
					}
					sendConnectTermOCS(tcapSession, action);
				} else {
					sendConnectTerm(tcapSession, action);
				}
				//legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERM_CONNECT_IN_PROGRESS);

				/*
				 * Set assist appsession timeout to 2 miniutes
				 */
				SipApplicationSession assistAppSession = getAssistAppSession(
						tcapSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getSipSessionsUtil());
				if (assistAppSession != null) {
					CommonUtils.setAppSessionTimeout(assistAppSession,
							PhConstants.CLEANUP_APPSESSION_TIMEOUT, dialogId);
				}
			} else if (action.getConnectionMode() == CONNECTIONMODE.PORTROUTING) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Initiating Terminating Connection in PORTROUTING mode");
				}

				sendConnectPort(tcapSession, action);
			} else if ((action.getConnectionMode() == CONNECTIONMODE.REDIRECTION)
					|| (action.getConnectionMode() == CONNECTIONMODE.B2BUA)) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Initiating Terminating Connection with handoff");
				}

				sendConnectHandoff(tcapSession, action);
				legData.set(LegDataAttributes.P_LEG_SS7_STATE,
						CapV2CallStates.HANDOFF);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Start correlation timer");
				}

				Capv2CS1ScfProtocolUtil.startTimer(tcapSession,
						Capv2CS1ScfProtocolUtil.getCorrelationTime(callData),
						true, PhConstants.CORRELATION_TIMER);

				/*
				 * setting tcapsession id in corr map; not setting entire object
				 * to avoid ft issues
				 */
				String correlationId = (String) callData
						.get(CallDataAttribute.P_CORRELATION_ID);
				PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap()
				.put(correlationId, tcapSession.getDialogueId());
				tcapSession.setAttribute(PhConstants.CORRELATION_ID,
						correlationId);
			}else if (action.getConnectionMode() == CONNECTIONMODE.CONTINUE){ 

				// check if we need to send RRBCSM, sending TAbandon - controlled through config
				// this is requirement from Telus RAIN application. Need to send RRBCSM (Dlg) 
				// followed by continue. 
				if((StringUtils.equalsIgnoreCase(Capv2ScfProtocolConfig
						.getConfigData(Capv2ScfProtocolConfig.SEND_TERMINATING_RRBCSM), "TRUE")) &&
						capCallState != capCallState.MS_DISCONNECTED) {
					sendRRBCSMForArming(tcapSession, action);
					// Send RRBCAM in continue dialogue
					sendContinueRequestEvent(tcapSession, cCallTraceService);
				}

				// send continue
				sendContinueComponent(tcapSession, callData, action);

				if (action.getSendMode() == Action.SEND_MODE.END){
					preProcessDroppedCall(tcapSession);
					sendEndRequestEvent(tcapSession, false, cCallTraceService);
					postProcessDroppedCall(tcapSession, true);
				}else{
					sendContinueRequestEvent(tcapSession, cCallTraceService);
				}				

			} else {
				logger.error(dialogId
						+ ":: Connect term invoked with invalid connection mode, drop call");

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.INVALID_CONNMODE_TERM);

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
			}
			break;
		default:
			logger.error(dialogId
					+ ":: Connect term invoked in invalid state, drop call");
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.UNEXP_ACT_CONNECT_TERM);

			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			dropCall(tcapSession, callData);
			break;
		}
	}

	/**
	 * Default drop call method to create default action on drop call based on
	 * call state.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 */
	public static void dropCall(TcapSession tcapSession, CallData callData) {
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside dropCall with tcapSession");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		CapV2CallStates capCallState = (CapV2CallStates) legData
				.get(LegDataAttributes.P_LEG_SS7_STATE);
		/*
		 * Constructing default action. For RC use cause value set by service.
		 * For other cases use default.
		 */
		switch (capCallState) {
		case INIT:
		case SERVICE_LOGIC:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: For service logic state default drop Mode is USER_ABORT");
			}
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(Capv2ScfProtocolFSMHandler.AbortInfoEnum.NO_REASON_GIVEN
					.getCode());
			break;
		case HANDOFF:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: For HANDOFF state default drop Mode is NONE");
			}
			action.setDropCallMode(DROP_CALL_MODE.NONE);
			break;
		case TERMINATION_IN_PROGRESS:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: For TERMINATION_IN_PROGRESS state default drop Mode is NULL_END");
			}
			action.setDropCallMode(DROP_CALL_MODE.NULL_END);
			break;
		case TERMINATED:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: For TERMINATED state default drop Mode is USER_ABORT");
			}
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			/*
			 * set reason as no reason given
			 */
			action.setReleaseCauseValue(1);
			break;
		case TERM_CONNECT_IN_PROGRESS:
		case ASSIST:
		case TERM_CONNECTED:
		default:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ " :: For CONNECT_INPROGRESS/CONNECTED/ASSIST/default states "
						+ "use default drop Mode is RELEASE_CALL");
			}
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			action.setReleaseCauseValue(CauseValEnum.Temporary_failure
					.getCode());

		}// end switch
		dropCall(tcapSession, callData, action);
	}

	/**
	 * Executes drop call action passed to method based on CAP call state.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param callData
	 *          represents the instance of callData
	 * @param action
	 *            represents the instance of Action
	 */
	public static void dropCall(TcapSession tcapSession, CallData callData,
			Action action) {

		Integer dialogId = null;
		boolean stateAlreadyTerminated = false;
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		try {

			dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ "[PH]:: Inside dropCall, CapCallState:"+ legData.get(LegDataAttributes.P_LEG_SS7_STATE)
						+ ", DropCallMode: "+ action.getDropCallMode().name());
			}

			/*
			 * if last dialog is END; No need to term message 
			 */
			int rxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
			if (rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END) {
				logger.warn(dialogId
						+ ":: Last Rx dialog primitive is END; Clean the call locally.");
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.END_RCVD_WITH_COMP);

				/*
				 * before return pre process dropped call as in finally block
				 * CDRs will be written
				 */

				preProcessDroppedCall(tcapSession);
				return;
			}
			if (rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_USER_ABORT
					|| rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_PROVIDER_ABORT
					|| rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_NOTICE) {
				logger.error(dialogId
						+ ":: Last dialogue received, so clean the call locally");
				/*
				 * before return pre process dropped call as in finally block
				 * CDRs will be written
				 */
				preProcessDroppedCall(tcapSession);
				return;
			}


			CapV2CallStates capCallState = (CapV2CallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

			if(capCallState == CapV2CallStates.TERMINATED){
				//preProcessDroppedCall(tcapSession);
				if(logger.isDebugEnabled()){
					logger.debug(dialogId + ":: call state is terminated do not do anything"); 
				}
				stateAlreadyTerminated = true;
				return;
			}

			switch (capCallState) {
			case INIT:
			case SERVICE_LOGIC:
			case TERM_CONNECT_IN_PROGRESS:
			case TERM_CONNECTED:
			case TERMINATED:
			case TERM_CONNECTED_ACR:
			case TERMINATED_ACR:
			case MS_DISCONNECTED:
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " ::dropCallMode: " + action.getDropCallMode().name());
				}

				// incase last state is TERMINATED_ACR then set drop call mode as CANCEL_
				if(capCallState == CapV2CallStates.TERMINATED_ACR){
					action.setDropCallMode(DROP_CALL_MODE.CANCEL_NULL_END_PREARRANGED);

					if(logger.isDebugEnabled()){
						logger.debug("OCS Flow: Service state: TERMINATED_ACR, setting DropCallMode:" +
								"CANCEL_NULL_END_PREARRANGED");
					}
				}
				break;
			case TERMINATION_IN_PROGRESS:

				/*
				 * This is internal stage after this stage is set.. No messages
				 * will be received by ph before state is changed to Terminated.
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " ::dropCallMode: NULL_END");
				}
				/*
				 * force drop call mode to null end as state happens on
				 * odisconnect and oAbndon only
				 */
				action.setDropCallMode(DROP_CALL_MODE.NULL_END);
				break;
			case ASSIST:
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " :: dropCall(session,action), send DFC and remove entry from correlation map");
					logger.debug(dialogId + " :: clean sip leg if any");
				}

				String dfcFlag = (String) callData
						.get(CallDataAttribute.P_DFC_REQUIRED_FLAG);

				if (dfcFlag!=null &&!PhConstants.FALSE.equals(dfcFlag)) {

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ " :: isDfcRequired is true; Orig sip call state: origSipCallState");
					}
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + " :: sending DFC");
					}
					sendDfc(tcapSession);
				}

				/*
				 * clean sip leg if presentCleanup all sip leg according to
				 * their sip session state
				 */

				break;

			case HANDOFF:
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " :: dropCall(session,action) HANDOFF, resource cleanup on common action");
				}

				/*
				 * for handoff no message is required to be sent out
				 */
				action.setDropCallMode(DROP_CALL_MODE.NONE);
				break;

			default:
				logger.warn(dialogId
						+ "::dropCall(session,action) Unhandled CAP call state");
				action.setDropCallMode(DROP_CALL_MODE.NONE);
				break;
			}

			/*
			 * marking call for clean up and cleaning timers/correlation
			 * resources before sending
			 */
			preProcessDroppedCall(tcapSession);

			/*
			 * dropping the call as per action
			 */
			sendDropMessage(tcapSession, action, PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());

		} catch (Exception ex) {
			logger.error(dialogId + ":: Failed to drop the call.", ex);
		} finally {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ ":: Inform service that call is dropped and write CDR");
			}

			if(!stateAlreadyTerminated){
				postProcessDroppedCall(tcapSession, true);
			}

		}// end finally
	}

	/**
	 * This Method is used to send Disconnect forward connection 
	 * @param tcapSession
	 * 		represents an instance of TcapSession
	 * @throws Exception
	 */
	private static void sendDfc(TcapSession tcapSession) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);

		Integer dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: Inside sendDfc");
		}

		if (tcapSession.getAttribute(PhConstants.DFC_SENT) == null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Send DFC as CAPCallState is ASSIST");
			}
			byte[] dfcOpCode = { CapV2OpCodes.DISCONNECT_FORWARD_CONNECTION_WITHOUT_ARGS_BYTE  };

			Operation dfcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, dfcOpCode);

			InvokeReqEvent dfcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), dfcOperation);
			dfcInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
			dfcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

			sendComponentReq(dfcInvokeReqEvent, callData, PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());
			tcapSession.setAttribute(PhConstants.DFC_SENT, PhConstants.TRUE);

			/*
			 * ASSIST INVITE leg should be cleaned if DFC has not been sent
			 */
			SipApplicationSession sipApplicationSession = getAssistAppSession(
					tcapSession, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getSipSessionsUtil());
			if (sipApplicationSession != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Setting DFC_SENT flag in appSessionId: "
							+ sipApplicationSession.getId());
				}
				sipApplicationSession.setAttribute(PhConstants.DFC_SENT, PhConstants.TRUE);
			}
		}
	}

	/**
	 * This method returns the sip application session of the correlated INVITE
	 * received for outgoing ASSIST
	 * 
	 * @param tcapSession
	 *            represents an instance of TcapSession
	 * @param sipSessionUtilrepresents
	 *            an instance of SipSessionsUtil
	 * @return an instance of SipApplicationSession
	 */
	public static SipApplicationSession getAssistAppSession(
			TcapSession tcapSession, SipSessionsUtil sipSessionUtil) {
		SipApplicationSession sipApplicationSession = null;
		String appSessionId = (String) tcapSession
				.getAttribute(PhConstants.ASSIST_APP_SESSION_ID);
		if (appSessionId != null) {
			sipApplicationSession = sipSessionUtil
					.getApplicationSessionById(appSessionId);
		}
		return sipApplicationSession;
	}

	/**
	 * This Method is used to initiate a Media server connection 
	 * @param tcapSession
	 * 				represents an instance of TcapSession
	 * @param callData
	 * 				represents the instance of callData
	 * @param action
	 * 			represents the instance of Action
	 * @throws Exception
	 */
	public static void initiateMediaServerConnection(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		CapV2CallStates capCallState = (CapV2CallStates) legData
				.get(LegDataAttributes.P_LEG_SS7_STATE);
		
		CallTraceService cCallTraceService = PhUtilityServices.getInstance
				((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		String capv2Flow = Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW);

		boolean useSS7IP = false;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ ":: Inside initiateMediaServerConnection with tcapSession");
			logger.debug(dialogId + ":: CapCallState is " + capCallState);
			logger.debug(dialogId + ":: ConnectionMode is " + action.getConnectionMode().name());
		}

		switch (capCallState) {
		case SERVICE_LOGIC:
		case ASSIST:
			if (action.getConnectionMode() == CONNECTIONMODE.ASSIST) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Connection Mode is ASSIST. Send ETC");
				}

				legData.set(LegDataAttributes.P_LEG_SS7_STATE,
						CapV2CallStates.ASSIST);

				Object correlationIdObj = callData
						.get(CallDataAttribute.P_CORRELATION_ID);

				/*
				 * When correlation id is set by application then we will send
				 * ETC to MS which may be SIP or SS7 IPMS but in case
				 * correlation id is not set by application it means the
				 * deployment is pure SS7 and deployment will directly use
				 * IP(Intellegent Peripheral) of SS7 network. so we will send
				 * ConnectToResource in this case.
				 */

				if (correlationIdObj != null) {

					/*
					 * Setting dialouge-id for the correlation-id so that SAS
					 * can give INVITE of ETC to same thread where IDP were
					 * delivered. This has been done to avoid deadlock due to
					 * parallel processing of CAP and SIP in different thread.
					 */

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Save correlationId in correlationMap .."
								+ correlationIdObj);
					}

					PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getCorrelationMap()
					.put((String) correlationIdObj,
							Integer.toString(tcapSession
									.getDialogueId()));

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Updated correlation map is  .."
								+ PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
								.getCorrelationMap());
					}
					sendAssistEtc(tcapSession, action);
				} else {

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Correlation id is not set ..seems like deployment is pure SS7. sending CTR");
					}
					useSS7IP = true;
					sendConnectToResource(tcapSession, callData, action);
				}

			} else if (action.getConnectionMode() == CONNECTIONMODE.B2BUA) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Connection Mode is B2BUA. Send connect for handoff");
				}
				sendConnectHandoff(tcapSession, action);
				legData.set(LegDataAttributes.P_LEG_SS7_STATE,
						CapV2CallStates.HANDOFF);
			}else if (action.getConnectionMode() == CONNECTIONMODE.DIRECT_MS){
				useSS7IP = true;

				// remove any correlation key if stored.
				if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("DirectMS scenario, correlation Id already set, removing it:"
								+ (String) callData.get(CallDataAttribute.P_CORRELATION_ID));
					}
					callData.remove(CallDataAttribute.P_CORRELATION_ID);
				}

				/*
				 * This call flow is executed for GR-1129. STR is sent to switch which
				 * encapsulated Announcement information. Since this is the request from an
				 * application for IVR connection (MS_CONNECT), and STR needs to be sent on next
				 * action MS_PLAY or MS_PLAYCOLECT therefore it returns MS_SUCCESS from here.
				 */
				// Issue with switch. Its expecting some delay between RRBCM and CTR. Therefore 
				// need to send RRBCSM during CTR.
				if(StringUtils.equalsIgnoreCase(Capv2ScfProtocolConfig
						.getConfigData(Capv2ScfProtocolConfig.SEND_TERMINATING_RRBCSM), "TRUE")){
					// Play Announcement coudl be called multiple times. only first time 
					// need to send RRBCSM/CTR/PlayAnns
					byte[] mtRRBCSMArg = Capv2ScfProtocolParser.generateRRBCSMTerminatingMO();

					byte[] rrbcmOpCode = { CapV2OpCodes.REQUEST_REPORT_BYTE };

					Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rrbcmOpCode);

					// Sending Leg 1 RRBCSM Event
					InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
					connectInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
					connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, mtRRBCSMArg));
					connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

					sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
					DialogueReqEvent dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
					sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);
				}

				legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.MS_CONNECTED);
				Event event = new Event(Event.EventType.EVENT_MS_SUCCESS, Protocol.CAPV2_SCF,
						CallDataAttribute.P_LEG1.name());

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Notify application MS Connected loopback .");
				}
				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Returning after MS_CONNECT execution");
				}
			} else {
				logger.error(dialogId
						+ ":: Invalid connection mode for connect ivr, drop call");

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.INVALID_CONNMODE_ORIG_IVR);
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
				return;
			}

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Start Correlation Timer");
			}

			if (!useSS7IP) {

				Capv2CS1ScfProtocolUtil.startTimer(tcapSession,
						Capv2CS1ScfProtocolUtil.getCorrelationTime(callData),
						true, PhConstants.CORRELATION_TIMER);

				/*
				 * setting tcapsession id in corr map; not setting entire object
				 * to avoid ft issues
				 */
				String correlationId = (String) callData
						.get(CallDataAttribute.P_CORRELATION_ID);
				PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap()
				.put(correlationId, tcapSession.getDialogueId());
				tcapSession.setAttribute(PhConstants.CORRELATION_ID,
						correlationId);
			}
			break;

		case DIRECT_MS:
			useSS7IP = true;

			// remove any correlation key if stored.
			if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
				if (logger.isDebugEnabled()) {
					logger.debug("DirectMS scenario, correlation Id already set, removing it:"
							+ (String) callData.get(CallDataAttribute.P_CORRELATION_ID));
				}
				callData.remove(CallDataAttribute.P_CORRELATION_ID);
			}

			/*
			 * This call flow is executed for GR-1129. STR is sent to switch which
			 * encapsulated Announcement information. Since this is the request from an
			 * application for IVR connection (MS_CONNECT), and STR needs to be sent on next
			 * action MS_PLAY or MS_PLAYCOLECT therefore it returns MS_SUCCESS from here.
			 */
			// Issue with switch. Its expecting some delay between RRBCM and CTR. Therefore 
			// need to send RRBCSM during CTR.
			if(StringUtils.equalsIgnoreCase(Capv2ScfProtocolConfig
					.getConfigData(Capv2ScfProtocolConfig.SEND_TERMINATING_RRBCSM), "TRUE")){
				// Play Announcement coudl be called multiple times. only first time 
				// need to send RRBCSM/CTR/PlayAnns
				byte[] mtRRBCSMArg = Capv2ScfProtocolParser.generateRRBCSMTerminatingMO();

				byte[] rrbcmOpCode = { CapV2OpCodes.REQUEST_REPORT_BYTE };

				Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rrbcmOpCode);

				// Sending Leg 1 RRBCSM Event
				InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
				connectInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
				connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, mtRRBCSMArg));
				connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

				sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
				DialogueReqEvent dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
				sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);
			}

			legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.MS_CONNECTED);
			Event event = new Event(Event.EventType.EVENT_MS_SUCCESS, Protocol.CAPV2_SCF,
					CallDataAttribute.P_LEG1.name());

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Notify application MS Connected loopback .");
			}
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Returning after MS_CONNECT execution");
			}

			break;
		default:
			logger.error(dialogId
					+ ":: Connect ivr invoked in invalid state, drop call");
			logger.error(capCallState + " ");
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.UNEXP_ACT_CONIVR_ORIG);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			dropCall(tcapSession, callData);
		}
	}

	/**
	 * Method to send ETC with continue in Assist mode.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	private static void sendAssistEtc(TcapSession tcapSession, Action action)
			throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendAssistEtc");
			logger.debug(dialogId + ":: Send ETC with CONTINUE");
		}

		sendEtc(tcapSession);

		sendContinueRequestEvent(tcapSession, cCallTraceService);
	}

	/**
	 * Method to send ETC component indication event.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @throws Exception
	 */
	private static void sendEtc(TcapSession tcapSession) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendEtc");
		}

		byte[] etc = Capv2ScfProtocolParser
				.createEtc(
						callData,
						(SccpUserAddress) callData
						.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS));

		byte[] etcOpCode = { CapV2OpCodes.ESTABLISH_TEMP_CONNECTION_BYTE  };

		Operation etcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				etcOpCode);

		InvokeReqEvent etcInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), etcOperation);
		etcInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getNextInvokeId(callData));
		etcInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, etc));
		etcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(etcInvokeReqEvent, callData, cCallTraceService);

		/*
		 * done to handle scenarios of multiple assist -- for future use
		 */
		tcapSession.removeAttribute(PhConstants.DFC_SENT);
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
	public static void disconnectIvr(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {

		Object dialogId = "0";
		Event event = null;

		try {

			dialogId = callData.get(CallDataAttribute.P_DIALOG_ID);
			LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getCallTraceService();

			CapV2CallStates capState = (CapV2CallStates) origLegData
					.get(LegDataAttributes.P_LEG_SS7_STATE);

			if(logger.isDebugEnabled()){
				logger.debug("disconenctIvr: currentState:" + capState);
			}

			/*
			 * SIP IVR ASSIST case it will be IP media server connection when we
			 * send ETC to use SIP IVR. A sip invite will be received . so CAP
			 * state will be ASSIST and SIP state will be MS_CONNETED
			 */

			if (capState == CapV2CallStates.ASSIST) {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " [PH]::  ASK SIPProtocolHandler to cleanup SIP IVR leg as CAP Call state is "
							+ CapV2CallStates.ASSIST);
				}

				ProtocolHandler stateHandler = ProtocolHandlerFactory
						.getProtocolHandler(Protocol.SIP);
				stateHandler.executeAction(callData, action);

			} else {

				// check if OCS then loopback MS_DISCONNECTED
				String capv2Flow = Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW);

				if (capv2Flow != null && StringUtils.equalsIgnoreCase(capv2Flow, "OCS")){
					//	||(StringUtils.equalsIgnoreCase(Capv2ScfProtocolConfig
					//			.getConfigData(Capv2ScfProtocolConfig.SEND_TERMINATING_RRBCSM), "TRUE"))) {


					// Check is we have received SRR last then we need to 
					// send DFC + ReleaseCall
					// In case last state is PLAY_SUCCESS_SRR
					if(capState == CapV2CallStates.PLAY_SUCCESS_SRR){
						if(logger.isDebugEnabled()){
							logger.debug("Drop MS: last state PLAY_SUCCESS_SRR  & OCS, sending DFC and RC");
						}

						// sendDfc
						sendDfc(tcapSession);
						sendContinueRequestEvent(tcapSession, cCallTraceService);

						// ReleaseCall
						sendReleaseCall(tcapSession, action, cCallTraceService);
						sendContinueRequestEvent(tcapSession, cCallTraceService);

					}

					event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG1.name());
					origLegData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.MS_DISCONNECTED);
				}else{
					// send DFC followed by event MS_DISCONNECTED

					sendDfc(tcapSession);
					sendContinueRequestEvent(tcapSession, cCallTraceService);

					event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG1.name());
					origLegData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.MS_DISCONNECTED);
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " [PH]:: Not an ASSIST flow " + capState);
				}
			}
			/*
			 * 1. when we send ETC to use IP media server then .ARI(ASSIST
			 * request Insrtuctions ) is receieved . on this we sends
			 * ConnectToResource . so CAP state will be MS_CONNECTED not ASSIST
			 * 2. when we sends ConnectToResource to connect directly SS7 IP
			 * media server . so CAP state will be MS_CONNECTED not ASSIST
			 */
			if(event == null){
				sendDfc(tcapSession);
				sendContinueRequestEvent(tcapSession, cCallTraceService);
			}else{
				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			}

		} catch (Exception ex) {
			logger.error(dialogId + ":: Failed to disconnect ivr. Error is "
					+ ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + ":: Error in disconnecting orig ivr.",
						ex);
			}
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.EXCEP_DISCON_ORIG_IVR);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			dropCall(tcapSession, callData);
		}
	}

	public static void playAnnouncementWrapper(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if(logger.isDebugEnabled()){
			logger.debug(dialogId + ": Inside playAnnouncementWrapper");
		}
		String capv2Flow = Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW);

		if (capv2Flow != null && StringUtils.equalsIgnoreCase(capv2Flow, "OCS")) {
			playAnnouncementOCS(tcapSession, callData, action);
		}else{
			playAnnouncement(tcapSession, callData, action);
		}
	}
	/**
	 * Method used to send RRBCSM for originating leg followed by CTR + Play ANn
	 * @param tcapSession
	 * @param callData
	 * @param action
	 */
	public static void playAnnouncement(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance
				((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		CapV2CallStates callstate = (CapV2CallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		DialogueReqEvent dialougeReqEvent  = null;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside playAnnoucement");
		}

		if(callstate != CapV2CallStates.USER_INTREACTION_IN_PROGRESS){
			// create and send CTR
			byte[] ctrBytes = Capv2ScfProtocolParser.createConnectToResource(callData);
			byte[] ctrOpCode = { CapV2OpCodes.CONNECT_TO_RESOURCE_BYTE };

			Operation ctrOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, ctrOpCode);

			// Sending Leg 1 RRBCSM Event
			InvokeReqEvent ctrInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), ctrOperation);
			ctrInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
			ctrInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, ctrBytes));
			ctrInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

			sendComponentReq(ctrInvokeReqEvent, callData, cCallTraceService);
		}

		// create and send Play
		byte[] playBytes = Capv2ScfProtocolParser.createPlay(callData);
		byte[] playOpCode = { CapV2OpCodes.PLAY_ANNOUNCEMENT_BYTE };

		Operation playOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, playOpCode);

		// Sending PlayAnn
		InvokeReqEvent playInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), playOperation);
		playInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		playInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, playBytes));
		playInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(playInvokeReqEvent, callData, cCallTraceService);

		// Send dialogue
		dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
		sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);

		// Set call state as user interaction 
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.USER_INTREACTION_IN_PROGRESS);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Exit playAnnoucement");
		}
	}
	/**
	 * Method used to send RRBCSM for originating leg followed by CTR + Play ANn
	 * @param tcapSession
	 * @param callData
	 * @param action
	 */
	public static void playAnnouncementOCS(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance
				((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside playAnnouncementOCS");
		}

		// send RRBCSM for Leg 1
		byte[] leg1RRBCSMArgs = Capv2ScfProtocolParser.generateRRBCSMLeg1Parameters();

		byte[] rrbcmOpCode = { CapV2OpCodes.REQUEST_REPORT_BYTE };

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rrbcmOpCode);

		// Sending Leg 1 RRBCSM Event
		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, leg1RRBCSMArgs));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
		DialogueReqEvent dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
		sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);

		// create and send CTR
		byte[] ctrBytes = Capv2ScfProtocolParser.createConnectToResource(callData);
		byte[] ctrOpCode = { CapV2OpCodes.CONNECT_TO_RESOURCE_BYTE };

		Operation ctrOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, ctrOpCode);

		// Sending Leg 1 RRBCSM Event
		InvokeReqEvent ctrInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), ctrOperation);
		ctrInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		ctrInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, ctrBytes));
		ctrInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(ctrInvokeReqEvent, callData, cCallTraceService);

		// create and send Play
		byte[] playBytes = Capv2ScfProtocolParser.createPlay(callData);
		byte[] playOpCode = { CapV2OpCodes.PLAY_ANNOUNCEMENT_BYTE };

		Operation playOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, playOpCode);

		// Sending Leg 1 RRBCSM Event
		InvokeReqEvent playInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), playOperation);
		playInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		playInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, playBytes));
		playInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(playInvokeReqEvent, callData, cCallTraceService);


		// Send dialogue
		dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
		sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);

		// Set call state as user interaction 
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.USER_INTREACTION_IN_PROGRESS);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Exit playAnnoucement");
		}
	}

	/**
	 * @param tcapSession
	 * @param callData
	 * @param action
	 */
	public static void playAndCollect(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance
				((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		CapV2CallStates callstate = (CapV2CallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		DialogueReqEvent dialougeReqEvent  = null;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside playAndCollect in state: " + callstate);
		}

		// if Play and Collected called for first time then we need to send RRBCSM 
		// CTR and PlayAndCollect else only PlayAndCollect

		if(callstate != CapV2CallStates.USER_INTREACTION_IN_PROGRESS){

			// create and send CTR
			byte[] ctrBytes = Capv2ScfProtocolParser.createConnectToResource(callData);
			byte[] ctrOpCode = { CapV2OpCodes.CONNECT_TO_RESOURCE_BYTE };

			Operation ctrOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, ctrOpCode);

			// Sending Leg 1 RRBCSM Event
			InvokeReqEvent ctrInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), ctrOperation);
			ctrInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
			ctrInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, ctrBytes));
			ctrInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

			sendComponentReq(ctrInvokeReqEvent, callData, cCallTraceService);
		}
		
		// hack 
		if(StringUtils.isNoneBlank(Capv2ScfProtocolConfig
				.getConfigData(Capv2ScfProtocolConfig.CAP_DELAY_BW_TWO_DIALOGUE))){
			try{
				int value = Integer.parseInt(Capv2ScfProtocolConfig
				.getConfigData(Capv2ScfProtocolConfig.CAP_DELAY_BW_TWO_DIALOGUE));
				
				if(logger.isDebugEnabled()){
					logger.debug(dialogId + "; adding delay of " + value + " between 2 dialogues");
				}
				Thread.sleep(value);
			}catch(Exception ex){}
		}
		
		// send PlayAndCollect
		Capv2ScfProtocolParser.sendPromptAndCollectAnnouncement(tcapSession, callData, action);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Exit playAndCollect");
		}
	}

	/**
	 * This method is called by PH to Send SCI, RNC, RRBCSM for Arming, CONNECT
	 * with CONTINUE. It internally calls other methods like
	 * sendChargingInformation, sendRequestNotificationChargingEvent
	 * sendRRBCSMForArming, sendConnect.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action.
	 * @throws Exception
	 */
	private static void sendConnectTerm(TcapSession tcapSession, Action action)
			throws Exception {
		logger.info("Inside sendConnectTerm");
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legdata1 = (LegData) callData.get(CallDataAttribute.P_LEG1);


		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendConnectTerm");
		}

		/*
		 * Request Report BCSM Event
		 */
		if (action.getSendMode() != Action.SEND_MODE.END){
			if (Capv2CS1ScfProtocolUtil.getErbSetByApplication(callData,
					action.getLeg()) != null) {

				sendRRBCSMForArming(tcapSession, action);
			}
		}
		/*
		 * Connect
		 */
		sendConnect(tcapSession, CONNECT_TYPE.TERMINATING);

		if (action.getSendMode() == Action.SEND_MODE.END) {

			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.CAP_CALL_CONNECTED);
			preProcessDroppedCall(tcapSession);

			if(action.isForcedFciFlag()){
				sendFci(tcapSession, cCallTraceService);
			}
			sendEndRequestEvent(tcapSession, false, cCallTraceService);

			postProcessDroppedCall(tcapSession, true);
		} else {
			sendContinueRequestEvent(tcapSession, cCallTraceService);
			legdata1.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERM_CONNECT_IN_PROGRESS);
		}
	}

	/**
	 * This method is for creating CAP component indication event CONNECT
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param connectType
	 *            represents CONNECT_TYPE
	 * @throws Exception
	 */
	private static void sendConnect(TcapSession tcapSession,
			CONNECT_TYPE connectType) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendConnect");
			logger.debug(dialogId + " :: connectType is " + connectType.name());
		}

		byte[] connect = null;
		if (connectType == CONNECT_TYPE.TERMINATING) {
			connect = Capv2ScfProtocolParser.createConnectForTerm(callData);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exit createConnectForTerm");
			}
		} else if (connectType == CONNECT_TYPE.PORTED) {
			/*
			 * Set the call data in appsession as tcap session would be
			 * invalidated in this case on CONNECT + TC-END.
			 */
			SipApplicationSession appSession = Capv2CS1ScfProtocolUtil
					.getAppSession(tcapSession);
			appSession.setAttribute(CallData.CALL_DATA, callData);
			connect = Capv2ScfProtocolParser.createConnectForPort(callData);
		} else if (connectType == CONNECT_TYPE.CORRELATION) {
			tcapSession.setAttribute(PhConstants.FOR_HANDOFF, PhConstants.TRUE);
			SccpUserAddress sua = (SccpUserAddress) legData
					.get(LegDataAttributes.P_SUA);
			connect = Capv2ScfProtocolParser.createConnectForHandoff(callData, sua);
		} else {
			logger.error(dialogId + ":: Invalid connection type");
			connect = Capv2ScfProtocolParser.createConnectForTerm(callData);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exit createConnectForTerm");
			}
		}

		byte[] connectOpCode = { CapV2OpCodes.CONNECT_BYTE };

		Operation connectOperation = new Operation(
				Operation.OPERATIONTYPE_LOCAL, connectOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, connect));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
		// callData.incrementReconnectionCount();
	}

	/**
	 * This method is used for sending component indication event CONNECT with
	 * dialogue indication event TC_END.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action.
	 * @throws Exception
	 */
	private static void sendConnectPort(TcapSession tcapSession, Action action)
			throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside sendConnectPort");
			logger.debug(dialogId + " :: Send CONNECT with TC_END");
		}

		/*
		 * Connect
		 */
		sendConnect(tcapSession, CONNECT_TYPE.PORTED);
		/*
		 * End Request Event (TC_END)
		 */
		sendEndRequestEvent(tcapSession, false, cCallTraceService);
		/*
		 * Set disconnect time etc as TC-END has been sent
		 */
		preProcessDroppedCall(tcapSession);
		/*
		 * notify service and write CDRs
		 */
		postProcessDroppedCall(tcapSession, true);

	}

	/**
	 * This method is for sending component indication event CONNECT for
	 * handoff. So it sends RRBCSM for disarming followed by CONNECT with
	 * TC_END.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action.
	 * @throws Exception
	 */
	private static void sendConnectHandoff(TcapSession tcapSession,
			Action action) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		String dialogId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: Inside sendConnectHandoff");
		}

		/*
		 * Request Report BCSM Event
		 */
		String leg1TriggersArmed = (String) tcapSession
				.getAttribute(PhConstants.LEG1_TRIGGERS_ARMED);
		if (leg1TriggersArmed != null
				&& leg1TriggersArmed.equals(PhConstants.TRUE)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: Send RRBCSM for disarming");
			}
			sendRRBCSMForDisarming(tcapSession, action);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Send CONNECT with TC_END");
		}
		/*
		 * Connect
		 */
		sendConnect(tcapSession, CONNECT_TYPE.CORRELATION);

		/*
		 * End Request Event (TC_END)
		 */
		sendEndRequestEvent(tcapSession, false, cCallTraceService);
	}

	/**
	 * This methos is for creating CAP component indication event for disarming
	 * the events.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	private static void sendRRBCSMForDisarming(TcapSession tcapSession,
			Action action) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		String dialogId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendRRBCSMForDisarming");
		}

		byte[] rrbe = Capv2ScfProtocolParser.createRRBCSMForDisarming(callData, action);
		byte[] rrbeOpCode = { CapV2OpCodes.RRBE_BYTE };

		Operation rrbeOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				rrbeOpCode);

		InvokeReqEvent rrbeInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), rrbeOperation);
		rrbeInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getNextInvokeId(callData));
		rrbeInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, rrbe));
		rrbeInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(rrbeInvokeReqEvent, callData, cCallTraceService);
		tcapSession.setAttribute(PhConstants.LEG1_TRIGGERS_ARMED,
				PhConstants.FALSE);
	}

	/**
	 * This method is for creating CAP component indication event for arming
	 * the events.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	private static void sendRRBCSMForArming(TcapSession tcapSession,
			Action action) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		String dialogId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendRRBCSMForArming");
		}

		byte[] rrbe = null;
		if(StringUtils.equalsIgnoreCase(Capv2ScfProtocolConfig
				.getConfigData(Capv2ScfProtocolConfig.SEND_TERMINATING_RRBCSM), "TRUE")){
			rrbe = Capv2ScfProtocolParser.generateRRBCSMTerminatingMO();

			if(logger.isDebugEnabled()){
				logger.debug(dialogId + ": Creaing Terminating MO RRBCSM");
			}
		}else{		
			rrbe = Capv2ScfProtocolParser.createRRBCSMForArming(callData, action);
		}

		byte[] rrbeOpCode = { CapV2OpCodes.REQUEST_REPORT_BYTE };

		Operation rrbeOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				rrbeOpCode);

		InvokeReqEvent rrbeInvokeReqEvent = new InvokeReqEvent(src,tcapSession.getDialogueId(), rrbeOperation);
		rrbeInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		rrbeInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, rrbe));
		rrbeInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(rrbeInvokeReqEvent, callData, cCallTraceService);
		tcapSession.setAttribute(PhConstants.LEG1_TRIGGERS_ARMED,
				PhConstants.TRUE);

	}

	public static void playAndRecord(TcapSession tcapSession,
			CallData callData, Action action) {

	}

	/**
	 * This Method is used to send Activity Test 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action
	 * @param callData
	 * 				represents the instance of callData
	 * @throws Exception
	 */
	public static void callHeartBeat(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {

		sendActivityTestForHB(tcapSession, action);

		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		/*
		 * Set the call state to HB in progress
		 */
		legData.set(LegDataAttributes.P_LEG_SS7_STATE,
				CapV2CallStates.CALL_HB_IN_PROGRESS);

		/*
		 * Start activity test timer . call will be dropped if timer gets
		 * timeout befor getting response from SCCP
		 */
		Capv2CS1ScfProtocolUtil.startTimer(tcapSession,
				Capv2CS1ScfProtocolUtil.getActivityTestTime(callData), true,
				PhConstants.AT_ACK_TIMER);

	}

	/**
	 * This Method is used to send apply charging 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param callData
	 * 				represents the instance of callData
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	public static void performCharging(TcapSession tcapSession,
			CallData callData, Action action) throws Exception{

		String dialogId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside performCharging");
		}

		byte[] ac = Capv2ScfProtocolParser.createApplyCharging(callData);
		byte[] acOpCode = { CapV2OpCodes.APPLY_CHARGING_BYTE };

		Operation chrgingOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				acOpCode);

		InvokeReqEvent acInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), chrgingOperation);
		acInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		acInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, ac));
		acInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: before  sendComponentReq ");
		}
		sendComponentReq(acInvokeReqEvent, callData, cCallTraceService);

		// check state if TERM_CONNECTED then we need to send Continue only once. 
		// After sendign ApplyCharging, satte shall move to APPLY_CHARGING_SENT
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
		CapV2CallStates capCallState = (CapV2CallStates) leg1Data.get(LegDataAttributes.P_LEG_SS7_STATE);

		if(capCallState == CapV2CallStates.TERM_CONNECTED){
			// need to send Continue. 
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: before  sendContinueComponent ");
			}
			sendContinueComponent(tcapSession, callData, action);

			if(logger.isDebugEnabled()){
				logger.debug("Send APply Charging with Continue as last state is TERM_CONNECTED");
			}
		}

		// Set Call state 
		leg1Data.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.AC_SENT);

		DialogueReqEvent dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
		sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);

		if(logger.isDebugEnabled()){
			logger.debug("Send ApplyCharging Exit");
		}
	}

	/**
	 * @param tcapSession
	 * @param callData
	 * @param action
	 */
	public static void sendHttpRequest(TcapSession tcapSession,
			CallData callData, Action action) {

	}

	/**
	 * @param tcapSession
	 * @param callData
	 * @param action
	 */
	public static void sendLsRequest(TcapSession tcapSession,
			CallData callData, Action action) {

	}

	/**
	 * This Methos is used to disconnect Term
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param callData
	 * 				represents the instance of callData
	 * @param action
	 *            represents the instance of Action
	 */
	public static void disconnctTerm(TcapSession tcapSession,
			CallData callData, Action action) {

		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside disconnectTerm() ");
		}
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();
		try {
			sendUAbortRequestEvent(tcapSession, action, cCallTraceService);
		} catch (Exception e) {
			logger.error("Exception thrown while disconnecting term", e);
			e.printStackTrace();
		}

	}

	public static void redirect(TcapSession tcapSession, CallData callData,
			Action action) {

	}

	/**
	 * This method marks call state, set disconnection time. Also cleans
	 * correlation timers. Invoke this method before sending termination message
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 */
	public static void preProcessDroppedCall(TcapSession tcapSession) {
		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside preProcessDroppedCall");
		}
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);

		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
			Capv2CS1ScfProtocolUtil.cleanupCorrelationResources(tcapSession);
		}

		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LEG_SS7_STATE,
				CapV2CallStates.TERMINATED);
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME,
				new Date());
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit preProcessDroppedCall");
		}
	}

	/**
	 * This method notifies service that call is dropped and executres actions
	 * reurned from service Also writes CDR after service notifictaion. invoke
	 * thsi method afters ending termination mesage
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param executeServiceAction
	 *            represents the instance of boolean
	 */
	public static void postProcessDroppedCall(TcapSession tcapSession,
			boolean executeServiceAction) {
		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside postProcessDroppedCall");
		}
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		/*
		 * Set the call state again just to handle the case where
		 * preProcessDroppedCall() is not called due to some exception during
		 * execution.
		 */
		legData.set(LegDataAttributes.P_LEG_SS7_STATE,
				CapV2CallStates.TERMINATED);

		/*
		 * Notify service that call is dropped
		 */
		notifyCallDropped(tcapSession, executeServiceAction);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Write CDR");
		}

		/*
		 * Set the call disconnect time again just to make sure that CDR is
		 * written properly using call disconnect time. This is to handle the
		 * case where preProcessDroppedCall() is not called due to some
		 * exception during execution.
		 */
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME,
				new Date());

		/*
		 * write CDR, CDR should be written after notifying service
		 */
		Capv2CS1ScfProtocolUtil.writeServiceCdr(tcapSession);

		/*
		 * Assist appsession cleanup moved after writing CDRs. otherwise
		 * P-CDR-INFO will be sent instead of DSI CDR so moved after writing
		 * CDR; This will also mean if ABORT/ER/REJ/RE/timeout is recived after
		 * assist SIP leg is cleaned
		 */
		SipApplicationSession sipApplicationSession = Capv2ScfProtocolHelper
				.getAssistAppSession(tcapSession, PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());

		/*
		 * Added null check as for assist timeout there wont be assist sip
		 * appsession
		 */
		if (sipApplicationSession != null) {
			/*
			 * In case of ASSIST DFC+RC+TC-END is sent to Orig. And TcapSession
			 * is invalidated. So TcapSession will not be available. And due to
			 * this SN would not be able to obtain callData from TcapSession. So
			 * set callData at AppSession of Assist INVITE
			 */
			sipApplicationSession.setAttribute(CallData.CALL_DATA, callData);

		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit postProcessDroppedCall");
		}
	}

	/**
	 * This method notify service that call is dropped.
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param executeServiceAction
	 *            represents the instance of boolean
	 */
	public static void notifyCallDropped(TcapSession tcapSession,
			boolean executeServiceAction) {
		int dialogueId = 0;
		try {
			dialogueId = tcapSession.getDialogueId();
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "[PH]:: Inside notifyCallDropped");
			}
			CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId
						+ " :: [PH] Notify service that call is dropped");
			}
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();
			Event event = new Event(EventType.EVENT_CALL_DROPPED,
					Protocol.CAPV2_SCF, null);

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);

		} catch (Exception ex) {
			logger.error(dialogueId + "::[PH] Error in notifyCallDropped "
					+ ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(dialogueId
						+ "::[PH] Error in processing notifyCallDropped", ex);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit notifyCallDropped");
		}
	}

	public static Action[] processAcr(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession) throws Exception {

		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processAcr");
		}

		/*
		 * parse ACR
		 */
		try {
			Capv2ScfProtocolParser.parseAcr(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in ACR.", ape);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ACR_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in ACR.",
					pore);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ACR_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ACR);

		}
		if (logger.isDebugEnabled()) {
			logger.debug("Exiting processACR() .....");
		}
		return null;
	}

	/**
	 * Handles Specialized Resource Report
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws Exception
	 */
	public static Action[] processSRR(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession) throws Exception {

		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processSRR");
		}

		try {
			Capv2ScfProtocolParser.parseSRR(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in ACR.", ape);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ACR_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in ACR.",
					pore);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ACR_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ACR);

		}

		return null;
	}

	/**
	 * This method is used to connect SS7 IP media server.
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendConnectToResource(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {

		/*	String dialogId = (String) callData
				.getPersistableData(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance()
				.getCallTraceService();
		LegData legData = (LegData) callData
				.getPersistableData(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendConnectToResource");
		}

		byte[] connect = Capv2ScfProtocolParser.createConnectForIP(callData);

		byte[] connectOpCode = { CapV2OpCodes.CONNECT_TO_RESOURCE_BYTE };

		Operation connectOperation = new Operation(
				Operation.OPERATIONTYPE_LOCAL, connectOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, connect));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);

		/*
		 * Currently setting state to MS connected as no success response comes
		 * for CTR PH need to notify application that MS has got connected now
		 * it can proceed with media operations later on PH will wait for error
		 * response of CTR if it comes. so PH will create a invoke operation
		 * timer of (10 secs ) timeout time of CTR . only then we will send
		 * connected event to app.
		 */
		/*legData.setPersistableData(LegDataAttributes.P_LEG_SS7_STATE,
				CAPv2_CALL_STATES.MS_CONNECTED);

		Event event = new Event(EventType.EVENT_MS_SUCCESS,
				Protocol.ITUINAPCS1_SCF, null);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ " [PH]::Notify application that MS has got connected .");
		}
		ServiceInterface serviceHandler = PhUtilityServices.getInstance()
				.getServiceHandler();

		ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

		if (logger.isDebugEnabled()) {
			logger.debug("Exiting sendConnectToResource() .....");
		}*/

	}

	/**
	 * This method is used for sending conitnue message
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param connectType
	 *            represents CONNECT_TYPE
	 * @throws Exception
	 */
	public static void sendContinueComponent(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {

		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		String dialogId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendContinueComponent");
		}

		byte[] cont = Capv2ScfProtocolParser.createContinue(callData);

		byte[] contOpCode = { CapV2OpCodes.CONTINUE_WITHOUT_ARG_BYTE };
		logger.debug("in sendContinueComponent cont > "+  cont  + " contOpCode:"+ contOpCode);
		Operation contOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				contOpCode);

		InvokeReqEvent contInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), contOperation);
		contInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		//contInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, cont));
		contInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		
		String capv2Flow = Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW);

		if (capv2Flow != null && StringUtils.equalsIgnoreCase(capv2Flow, "OCS")) {
			logger.debug("sendContinueComponent: not sending COntinue Parameter");
		}else{
			contInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, cont));
		}

		sendComponentReq(contInvokeReqEvent, callData, cCallTraceService);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Exit sendContinueComponent");
		}
	}

	/**
	 * This method is used for activity Test
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param action
	 *            represents the instance of Action
	 * @throws Exception
	 */
	private static void sendActivityTestForHB(TcapSession tcapSession,
			Action action) throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		String dialogId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendActivityTestForHB");
		}

		byte[] atOpCode = { CapV2OpCodes.ACTIVITY_TEST_BYTE };

		Operation atOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				atOpCode);

		InvokeReqEvent atInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), atOperation);
		atInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getNextInvokeId(callData));
		callData.set(CallDataAttribute.P_AT_INVOKE_ID,
				atInvokeReqEvent.getInvokeId());
		atInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(atInvokeReqEvent, callData, cCallTraceService);

	}

	/**
	 * This method is called to start the CDR timer for the intermediate CDRs
	 * 
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param callData
	 *            represents the instance of CallData
	 */
	private void startCdrTimer(TcapSession tcapSession, CallData callData) {

		SipApplicationSession appSession = Capv2CS1ScfProtocolUtil
				.getAppSession(tcapSession);
		String dialogId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		long initialDelay = 0L;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Start Intermediate CDR Timer");
		}

		/*
		 * moved before timer creation to replicate updated value
		 */
		CommonUtils.setAppSessionTimeout(appSession,
				(int) (initialDelay * 60000) + 5, dialogId);

		Capv2CS1ScfProtocolUtil.startTimer(tcapSession, initialDelay, false,
				PhConstants.CDR_TIMER);

	}

	/**
	 * This method is used to process the Assist Request Instructions
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws Exception
	 */
	public static Action[] processARI(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession) throws Exception {
		/*	CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		LegData legData = (LegData) callData
				.getPersistableData(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processARI");
		}

		/*
		 * parse ARI
		 */
		/*	try {
			Capv2ScfProtocolParser.parseAri(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in ARI.", ape);
			callData.setNonpersistableData(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ARI_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in ARI.",
					pore);
			callData.setNonpersistableData(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					Capv2ScfRelReasonCode.ACR_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ACR);

		}
		if (logger.isDebugEnabled()) {
			logger.debug("Exiting processARI() .....");
		}

		/*
		 * When ARI is received we need to cleanup the correlation id and send
		 * the connect to resource to IP media server
		 */
		/*	Capv2CS1ScfProtocolUtil.cleanupCorrelationResources(tcapSession);

		sendConnectToResource(tcapSession, callData, null);*/

		return null;
	}


	/**
	 * @param tcapSession
	 *            represents the instance of TcapSession
	 * @param callData
	 *            represents the instance of CallData
	 * @param action
	 *			 represents the instance of Action
	 * @throws Exception
	 */
	public static void sendResetTimer(TcapSession tcapSession,
			CallData callData, Action action)  throws Exception {
		String dialogId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendResetTimer");
		}

		byte[] ac = Capv2ScfProtocolParser.createResetTimer(callData);
		byte[] acOpCode = { CapV2OpCodes.RESET_TIMER_BYTE };

		Operation resetOperation = new Operation(Operation.OPERATIONTYPE_LOCAL,
				acOpCode);

		InvokeReqEvent resetInvokeReqEvent = new InvokeReqEvent(src,
				tcapSession.getDialogueId(), resetOperation);
		resetInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil
				.getNextInvokeId(callData));
		resetInvokeReqEvent.setParameters(new Parameters(
				Parameters.PARAMETERTYPE_SEQUENCE, ac));
		resetInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		sendComponentReq(resetInvokeReqEvent, callData, cCallTraceService);
		sendContinueRequestEvent(tcapSession,  cCallTraceService);

	}

	private static void incrementTcapCounters(EventObject eventObject) {
		MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.CAPV2_SCF);
		if(measurementCounter == null) {
			return;
		}

		int primitiveType = -1;
		if(eventObject instanceof DialogueReqEvent) {
			primitiveType = ((DialogueReqEvent) eventObject).getPrimitiveType();
		}else if(eventObject instanceof ComponentReqEvent) {
			primitiveType = ((ComponentReqEvent) eventObject).getPrimitiveType();
		}

		SS7Message ss7Message = SS7Message.valueOf(primitiveType);

		switch(ss7Message) {

		case PRIMITIVE_BEGIN : {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_BEGIN, false);
			break;
		}

		case PRIMITIVE_END : {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_END, false);
			break;
		}

		case PRIMITIVE_USER_ABORT : {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_USER_ABORT, false);
			break;
		}
		}
	}


	private static void sendConnectTermOCS(TcapSession tcapSession, Action action) throws Exception {
		logger.info("Inside sendConnectTermOCS");
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);


		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		byte[] leg1RRBCSMArgs = Capv2ScfProtocolParser.generateRRBCSMLeg1Parameters();

		byte[] rrbcmOpCode = { CapV2OpCodes.REQUEST_REPORT_BYTE };

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rrbcmOpCode);

		// Sending Leg 1 RRBCSM Event
		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, leg1RRBCSMArgs));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
		DialogueReqEvent dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
		sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);

		// Sending Leg 2 RRBCSM Event
		byte[] leg2RRBCSMArgs = Capv2ScfProtocolParser.generateRRBCSMLeg2Parameters();
		connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, leg2RRBCSMArgs));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);

		sendContinueComponent(tcapSession, callData, action);

		dialougeReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
		sendDialogueReq(dialougeReqEvent, callData, cCallTraceService);
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERM_CONNECT_IN_PROGRESS);
		logger.info("sendConnectTermOCS finished");
	}

	/**
	 * Method to convert Hex buffer to Hex String. 
	 * like 0x30 0xfe 0x40 to 30fe40 string
	 * @param byteArray
	 * @return
	 */
	public static String encodeHexString(byte[] byteArray) {
		StringBuffer hexStringBuffer = new StringBuffer();
		for (int i = 0; i < byteArray.length; i++) {
			hexStringBuffer.append(byteToHex(byteArray[i]));
		}
		return hexStringBuffer.toString();
	}

	/**
	 * Helper to encodeHexString
	 * @param num
	 * @return
	 */
	public static String byteToHex(byte num) {
		char[] hexDigits = new char[2];
		hexDigits[0] = Character.forDigit((num >> 4) & 0xF, 16);
		hexDigits[1] = Character.forDigit((num & 0xF), 16);
		return new String(hexDigits);
	}

	/**
	 * Method is used to send CANCEL and COntinue to cleanup call leg on switch. 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendCancel(TcapSession tcapSession,
			CallData callData, Action action) throws Exception{

		String dialogId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendCancel");
		}

		byte[] cancelBytes = Capv2ScfProtocolParser.createCancel(callData);
		byte[] cancelOpCode = { CapV2OpCodes.CANCEL_BYTE };

		Operation cancelOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, cancelOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), cancelOperation);
		connectInvokeReqEvent.setInvokeId(Capv2CS1ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, cancelBytes));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
	}

	/**
	 * 
	 * @param tacpSession
	 * @param callData
	 * @throws Exception 
	 */

	public static void sendSipInviteFromCap(TcapSession tacpSession, CallData callData,Action action) throws Exception {
		if(logger.isDebugEnabled()){
			logger.debug("Enter sendSipInvite for PSX from Camel");
		}
		boolean outboundGwNotFound = true;

		try {
			createSipInviteFromCap(tacpSession, callData);
			outboundGwNotFound = false;
		} catch (ServletParseException e) {
			logger.error("ServletParseException inside sendInvite to PSX from Cap:: " + e);
		} catch (IOException e) {
			logger.error("IOException inside sendInvite to PSX from Cap:: " + e.getMessage());
		} catch (ServletException e) {
			logger.error("ServletException inside sendInvite to PSX from Cap:: " + e);
		}

		if(outboundGwNotFound) {
			Action lastAction = (Action) callData.get(CallDataAttribute.P_LAST_CALL_ACTION);
			String legId = lastAction.getLeg();

			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
			legData.set(LegDataAttributes.P_CAUSE_CODE, Capv2ScfRelReasonCode.OBGW_NOT_AVAILABLE);

			logger.error(tacpSession.getDialogueId() + ": OutboundGateway not found");
			Event event = new Event(EventType.EVENT_GW_NOT_FOUND, Protocol.CAPV2_SCF, legId);
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
		}
	}

	/**
	 * when InitialDP come to cas then mph will create a Invite request and send
	 * PSX . this method perform to send a INVITE request
	 * 
	 * @param tcapsesion
	 * @param calldata
	 * @throws IOException
	 * @throws ServletException
	 */
	@SuppressWarnings("deprecation")
	private static void createSipInviteFromCap(TcapSession tcapsesion, CallData calldata)
			throws IOException, ServletException {
		if(logger.isDebugEnabled()){
			logger.debug("Inside createSipInviteFromCap::enter ");
		}
		String origLegCallId = (String) calldata
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legdata1 = (LegData) calldata.get(CallDataAttribute.P_LEG1);
		legdata1.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.PSX_ROUTING);
		if(logger.isDebugEnabled()){
			logger.debug("LegData1 Inside createSipInviteFromCap is:: " + legdata1);
		}
		LegData legdata2 = (LegData) calldata.get(CallDataAttribute.P_LEG2);

		if(logger.isDebugEnabled()){
			logger.debug("LegData2 Inside createSipInviteFromCap is:: " + legdata2);
		}
		String ip =null;
		int port =5060;
		String eqspoolId = (String)legdata2.get(LegDataAttributes.NP_GW_POOL_ID);
		if(logger.isDebugEnabled()){
			logger.debug("NP_GW_POOL_ID in createSipInviteFromCap::"+eqspoolId);
		}

		if (eqspoolId != null){
			OutboundGatewaySelector obgwSelector = PhUtilityServices
					.getInstance((String)calldata.get(CallDataAttribute.SERVICE_ID)).getObgwSelector();

			OutboundGateway obgw = null;

			obgw = obgwSelector.selectFromGroup(eqspoolId);

			if (obgw == null) {

				if (logger.isDebugEnabled()) {
					logger.debug(":Notify service that obgw not found for  "
							+ eqspoolId);
				}

				legdata2.set(
						LegDataAttributes.NP_LAST_GW_ID, null);
			}else {
				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + "::EQS host is  "
							+ obgw.getHost() + " port is  "
							+ obgw.getPort());
				}

				if (obgw.getHost() != null) {
					ip = obgw.getHost().getHostAddress();
				}
				if (obgw.getPort() != -1) {
					port = obgw.getPort();
				}
			}
		}


		/**
		 * Getting SipFactory object corresponding Service ID.
		 */
		SipFactory factory = PhUtilityServices.getInstance((String) calldata.get(CallDataAttribute.SERVICE_ID))
				.getSipFactory();
		PhoneNumber caller = (PhoneNumber) legdata1.get(LegDataAttributes.P_CALLING_PARTY);
		PhoneNumber called = (PhoneNumber) legdata2.get(LegDataAttributes.P_DESTINATION_NUMBER);


		/**
		 * creating new sip app session
		 */
		SipApplicationSession sipApplicationSession = factory.createApplicationSession();

		/**
		 * creating called and caller URI for PSX proxy server.
		 */
		URI callerURI = null;
		URI calledUri = null;

		if (SipProtocolConfig.getConfigData(SipProtocolConfig.FLOATING_IP) != null
				&& SipProtocolConfig.getConfigData(SipProtocolConfig.SIP_PORT) != null) {
			if(logger.isDebugEnabled()){
				logger.debug("Creating Caller URI using ::");
			}
			callerURI = factory.createURI(
					"sip:" + ((caller != null)?caller.getAddress():"Anonymous") + "@" + SipProtocolConfig.getConfigData(SipProtocolConfig.FLOATING_IP)
					+ ":" + SipProtocolConfig.getConfigData(SipProtocolConfig.SIP_PORT));
		} else {
			logger.error("Exeception in creating caller URI::");
			throw new ServletException("Exeception in creating caller URI");
		}

		if(ip !=null){

			if(logger.isDebugEnabled()){
				logger.debug("Creating Called URI using NP_GW_POOL_ID::");
			}

			if(legdata2.get(LegDataAttributes.P_PSX_QTYPE) != null){
				calledUri = factory.createURI("sip:" + called.getAddress() + "@"
						+ ip + ":" + port
						+ ";" + "QTYPE=" + legdata2.get(LegDataAttributes.P_PSX_QTYPE));
			} else {
				calledUri = factory.createURI("sip:" + called.getAddress() + "@"
						+ ip + ":" + port);
			}
		}
		else if (legdata2.get(LegDataAttributes.P_REMOTE_IP) != null
				&& legdata2.get(LegDataAttributes.P_REMOTE_PORT) != null) {
			if(logger.isDebugEnabled()){
				logger.debug("Creating Called URI using outbound gateway address ::");
			}

			if(legdata2.get(LegDataAttributes.P_PSX_QTYPE) != null){
				calledUri = factory.createURI("sip:" + called.getAddress() + "@"
						+ legdata2.get(LegDataAttributes.P_REMOTE_IP) + ":" + legdata2.get(LegDataAttributes.P_REMOTE_PORT)
						+ ";" + "QTYPE=" + legdata2.get(LegDataAttributes.P_PSX_QTYPE));
			} else {
				calledUri = factory.createURI("sip:" + called.getAddress() + "@"
						+ legdata2.get(LegDataAttributes.P_REMOTE_IP) + ":" + legdata2.get(LegDataAttributes.P_REMOTE_PORT));
			}
		} else {
			logger.error("Exeception in creating called URI::");
			throw new ServletException("Exeception in creating called URI");
		}
		if(logger.isDebugEnabled()){
			logger.debug("Created callerURI as: " + callerURI + " and CalledUri as " + calledUri);
		}
		calldata.set(CallDataAttribute.TO_URI, calledUri);

		SipApplicationSession tcapApplicationSession = Capv2CS1ScfProtocolUtil.getAppSession(tcapsesion);

		sipApplicationSession.setAttribute(CallData.CALL_DATA, calldata);
		sipApplicationSession.setAttribute(PhConstants.TCAP_SESSION_ID,
				tcapApplicationSession.getAttribute(PhConstants.TCAP_SESSION_ID));

		if(logger.isDebugEnabled()){
			logger.debug("Created new sip app session as:: " + sipApplicationSession);
		}

		tcapsesion.setAttribute(CallData.CALL_DATA, calldata);

		SipServletRequest sipRequest = factory.createRequest(sipApplicationSession, "INVITE", callerURI, calledUri);
		ServiceInterface srviceHandler = PhUtilityServices
				.getInstance((String) calldata.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

		if(logger.isDebugEnabled()){
			logger.debug("srviceHandler: " + srviceHandler);
		}

		SipSession legSipSession = sipRequest.getSession();
		// legSipSession.setAttribute("TCAP_SESSION", tcapsesion);

		legdata1.set(LegDataAttributes.P_SESSION_ID, legSipSession.getId());
		if(logger.isDebugEnabled()){
			logger.debug("tcap-session at the time of creating Invite msg: " + tcapsesion);
		}

		if(logger.isDebugEnabled()){
			logger.debug("legSipSession: " + legSipSession);
		}


		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_PSX_ROUTING);
		action.setProtocol(Protocol.CAPV2_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		actionArr[0] = action;
		calldata.set(CallDataAttribute.P_CURRENT_ACTION, action);

		legSipSession.setHandler(srviceHandler.getServletName());
		if(logger.isDebugEnabled()){
			logger.debug("setting servlet as: " + srviceHandler.getServletName());
		}
		if(logger.isDebugEnabled()){
			logger.debug("Sending INVITE Reques, Request: " + sipRequest);
		}

		sipRequest.send();
		if(logger.isDebugEnabled()){
			logger.debug("Inside createSipInviteFromCap::exit");
		}
	}
	/**
	 * This method is used to start a timer requested by application
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void startApplicationTimer(TcapSession appSession,
			CallData callData, Action action) throws Exception{
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

		Capv2CS1ScfProtocolUtil.startTimer(appSession, action.getTimerTimeInMills(), false, timerName);
		ServiceInterface serviceHandler = PhUtilityServices
				.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		Event appEvent = new Event(EventType.EVENT_APP_TIMER_STARTED, Protocol.CAPV2_SCF, action.getLeg());

		ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
	}

	/**
	 * This method is used to stop a timer requested by application
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 */
	public static void stopApplicationTimer(TcapSession appSession,
			CallData callData, Action action) throws Exception{
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
		Capv2CS1ScfProtocolUtil.stopTimer(appSession, timerName);

		try{
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event appEvent = new Event(EventType.EVENT_APP_TIMER_STOPPED, Protocol.CAPV2_SCF, action.getLeg());

			ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
		}catch(Exception ex){
			logger.error("Error occured while stopping application timer : " + action.getTimerName() );
			logger.error("Error occured : " + ex);
			ex.printStackTrace();
			throw ex;
		}
	}

	/**
	 * @param annSpec
	 * @return
	 */
	public static byte[] createPromptAndCollectUserInformation(AnnSpec annSpec) throws Exception {
		logger.debug("Input AnnSpec: " + annSpec);
		PromptAndCollectUserInformationArg promptAndCollectUserInformationArg = getPromptAndCollectAnnouncementArgFromAnnSpec(annSpec);
		return createMessageByteArray(CapV2OpCodes.PROMPT_COLLECT, promptAndCollectUserInformationArg);
	}

	/**
	 * @param opCode
	 * @param operationObject
	 * @return
	 * @throws Exception
	 */
	private static byte[] createMessageByteArray(String opCode, Object operationObject) throws Exception {
		LinkedList<String> opCodeList = new LinkedList<String>();
		opCodeList.add(opCode);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(operationObject);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCodeList, true);
		return encodeList.getFirst();
	}

	/**
	 * @param annSpec
	 * @return
	 * @throws ASNParsingException
	 */
	private static PromptAndCollectUserInformationArg getPromptAndCollectAnnouncementArgFromAnnSpec(AnnSpec annSpec) throws ASNParsingException {
		PromptAndCollectUserInformationArg promptAndCollectUserInformationArg = new PromptAndCollectUserInformationArg();

		//promptAndCollectUserInformationArg.setDisconnectFromIPForbidden(true);
		try {
			if (null != annSpec) {
				promptAndCollectUserInformationArg.setInformationToSend(getInformationToSend(annSpec));
				promptAndCollectUserInformationArg.setCollectedInfo(getCollectedInfo(annSpec));
			} else {
				throw new IllegalArgumentException("AnnSpec cannot be null!");
			}
		} catch (ASNParsingException asne) {
			throw new ASNParsingException(asne.getMessage(), asne.getCause(), ASNParsingException.MESSAGE.PA);
		}
		return promptAndCollectUserInformationArg;
	}

	/**
	 * @param annSpec
	 * @return
	 * @throws ASNParsingException
	 */
	private static InformationToSend getInformationToSend(AnnSpec annSpec) throws ASNParsingException {
		InformationToSend informationToSend = new InformationToSend();

		InbandInfo inbandInfo = new InbandInfo();
		inbandInfo.setMessageID(getMessageId(annSpec));

		if(annSpec.getAnnIteration() != 0){
			inbandInfo.setNumberOfRepetitions(annSpec.getAnnIteration());
		}

		if(annSpec.getAnnLength() != 0){
			inbandInfo.setDuration(annSpec.getAnnLength());
		}

		if(logger.isDebugEnabled()){
			logger.debug("getInformationToSend: PhConstants.TRUE.equals(sendOptionalParam):" 
					+ ", AnnIteraction:" + annSpec.getAnnIteration() + ", annlength:" + annSpec.getAnnLength() );
		}

		informationToSend.selectInbandInfo(inbandInfo);

		return informationToSend;
	}

	/**
	 * get message id
	 * @param annSpec
	 * @return
	 * @throws ASNParsingException
	 */
	private static MessageID getMessageId(AnnSpec annSpec) throws ASNParsingException {
		MessageID messageID = new MessageID();
		Collection<AnnSpec.PlayMessage> fixedAnnouncements = getListOfAnnouncementsByType(annSpec, AnnSpec.ANN_TYPE.ANN);
		Collection<AnnSpec.PlayMessage> variableAnnouncements = getListOfAnnouncementsByType(annSpec, AnnSpec.ANN_TYPE.VAR);

		if (CollectionUtils.isNotEmpty(fixedAnnouncements)) {
			logger.debug("Found non-empty fixed announcements list.");
			if (fixedAnnouncements.size() == 1) {
				messageID.selectElementaryMessageID(getElementaryMessageIdsCollection(fixedAnnouncements).get(0));
			} else {
				messageID.selectElementaryMessageIDs(getElementaryMessageIdsCollection(fixedAnnouncements));
			}
		} else if (CollectionUtils.isNotEmpty(variableAnnouncements)) {
			logger.debug("Found non-empty variable announcements list.");
			//INAP-CS1 supports only 1 variable announcement. So getting the first argument
			setVariableMessage(messageID, variableAnnouncements.iterator().next());
		} else {
			logger.error("Neither of fix or variable announcments are set..");
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, Neither of fix or variable announcments are set.", ASNParsingException.MESSAGE.PA);
		}
		return messageID;
	}

	private static LinkedList<Integer4> getElementaryMessageIdsCollection(Collection<AnnSpec.PlayMessage> playMessages) throws ASNParsingException {
		LinkedList<Integer4> messageIds = new LinkedList<Integer4>();
		try {
			for (AnnSpec.PlayMessage playMessage : playMessages) {
				messageIds.add(getElementaryMessageId(playMessage));
			}
		} catch (Exception e) {
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, PA/PAC parsing failure occured.", e, ASNParsingException.MESSAGE.PA);
		}
		return messageIds;
	}

	private static Integer4 getElementaryMessageId(AnnSpec.PlayMessage playMessage) throws ASNParsingException {
		Integer4 messageId = new Integer4();
		try {
			//FIXME : Currently hardcoding the announcemnets as integers. THIS Needs to be fixed.

			logger.debug("getElementaryMessageId: " + playMessage.getMessageId());
			messageId.setValue(Integer.parseInt(playMessage.getMessageId()));
			//messageId.setValue(1234234);
		} catch (Exception e) {
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, PA/PAC parsing failure occured.", e, ASNParsingException.MESSAGE.PA);
		}
		return messageId;
	}

	/**
	 * @param annSpec
	 * @return
	 * @throws ASNParsingException
	 */
	private static CollectedInfo getCollectedInfo(AnnSpec annSpec) throws ASNParsingException {
		CollectedInfo collectedInfo = new CollectedInfo();

		CollectedDigits collectedDigits = new CollectedDigits();
		int maxDigits = annSpec.getMaxDigits();
		maxDigits = maxDigits > 0 ? maxDigits : PhConstants.DEFAULT_PNC_MAX_DIGIT;
		collectedDigits.setMaximumNbOfDigits(maxDigits);

		int minDigits = annSpec.getMinDigits();
		minDigits = minDigits > 0 ? minDigits : PhConstants.DEFAULT_PNC_MIN_DIGIT;
		collectedDigits.setMinimumNbOfDigits(minDigits);

		if(logger.isDebugEnabled()){
			logger.debug("setting maxDigits as :" + maxDigits + "and minDigits as :" + minDigits + "in annSpec");
		}

		if (StringUtils.isNotEmpty(annSpec.getTerminationKey())) {
			collectedDigits.setEndOfReplyDigit(annSpec.getTerminationKey().getBytes());
		}
		if (StringUtils.isNotEmpty(annSpec.getEsacpeKey())) {
			collectedDigits.setCancelDigit(annSpec.getEsacpeKey().getBytes());
		}
		collectedDigits.setFirstDigitTimeOut(annSpec.getFirstDigitTimer());
		//collectedDigits.setInterDigitTimeOut(annSpec.getInterDigitTimer());
		//collectedDigits.setInterruptableAnnInd(annSpec.isBarge());

		collectedInfo.selectCollectedDigits(collectedDigits);
		return collectedInfo;
	}

	/**
	 * @param messageID
	 * @param playMessage
	 * @throws ASNParsingException
	 */
	private static void setVariableMessage(MessageID messageID, AnnSpec.PlayMessage playMessage) throws ASNParsingException {
		MessageID.VariableMessageSequenceType variableMessageSequenceType = new MessageID.VariableMessageSequenceType();
		variableMessageSequenceType.setElementaryMessageID(getElementaryMessageId(playMessage));
		VariablePart variablePart = new VariablePart();
		String variableAnnType = playMessage.getVarAnnType();

		//TODO: Currently AnnSpec is not supporting variable announcements properly. So adding dummy data.
		if (variableAnnType.equals(ANN_TYPE_DAT)) {
			//1993 September 30th
			String testDate = "930930";
			byte[] encodedDate = BCDEncoderHelper.getBCDEncodedArray(testDate, 3);
			variablePart.selectDate(encodedDate);
		} else if (variableAnnType.equals(ANN_TYPE_MNY)) {
			//$249.50
			String sampleMoney = "24950";
			byte[] encodedMoney = BCDEncoderHelper.getBCDEncodedArray(sampleMoney, 4);
			variablePart.selectPrice(encodedMoney);
		} else if (variableAnnType.equals(ANN_TYPE_NUM)) {
			logger.debug("ANN_TYPE_NUM");
		} else if (variableAnnType.equals(ANN_TYPE_TIME)) {
			//12:15
			String sampleTime = "1215";
			byte[] encodedTime = BCDEncoderHelper.getBCDEncodedArray(sampleTime, 2);
			variablePart.selectTime(encodedTime);
		} else {
			throw new IllegalArgumentException("Unsupported variable ann type! : " + variableAnnType);
		}

		//Creating a single-value collection
		List<VariablePart> variablePartList = new LinkedList<VariablePart>();
		variablePartList.add(variablePart);
		variableMessageSequenceType.setVariableParts(variablePartList);
		messageID.selectVariableMessage(variableMessageSequenceType);
	}

	private static Collection<AnnSpec.PlayMessage> getListOfAnnouncementsByType(AnnSpec annSpec, final AnnSpec.ANN_TYPE annType) {
		return filter(annSpec.getPlayMsgList(), new Predicate<AnnSpec.PlayMessage>() {
			@Override
			public boolean apply(AnnSpec.PlayMessage playMessage) {
				return (playMessage.getAnnType() == annType);
			}
		});
	}

	/**
	 * @param callData
	 * @param tcapSession
	 */
	public static void traceMessage(CallData callData, TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside CAP traceMessage() for service...,tcapSession:" + tcapSession);
		}

		if(tcapSession ==null){
			return;
		}

		String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);
		int dialogueId = tcapSession.getDialogueId();
		int callState = CallTraceService.CALL_IN_PROGRESS;

		if (PhConstants.TRUE.equals(traceFlag)) {

			StringBuilder traceMsg = (StringBuilder) callData.get(CallDataAttribute.P_TRACE_MESSAGE);

			if (traceMsg.length() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Trace message is " + traceMsg);
				}

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					for (int constraint : constraintList) {
						PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService()
						.trace(constraint, Capv2CS1ScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(), callState);
					}
				}
				traceMsg.delete(0, traceMsg.length());
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exitting AIN traceMessage()...");
		}
	}		
}
