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

import jain.MandatoryParameterNotSetException;
import jain.ParameterNotSetException;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;
import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.ComponentReqEvent;
import jain.protocol.ss7.tcap.DialogueIndEvent;
import jain.protocol.ss7.tcap.DialogueReqEvent;
import jain.protocol.ss7.tcap.GTIndicator0001;
import jain.protocol.ss7.tcap.GTIndicator0010;
import jain.protocol.ss7.tcap.GTIndicator0011;
import jain.protocol.ss7.tcap.GTIndicator0100;
import jain.protocol.ss7.tcap.GlobalTitle;
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
import jain.protocol.ss7.tcap.dialogue.BeginReqEvent;
import jain.protocol.ss7.tcap.dialogue.ContinueReqEvent;
import jain.protocol.ss7.tcap.dialogue.DialogueConstants;
import jain.protocol.ss7.tcap.dialogue.DialoguePortion;
import jain.protocol.ss7.tcap.dialogue.EndReqEvent;
import jain.protocol.ss7.tcap.dialogue.ProviderAbortIndEvent;
import jain.protocol.ss7.tcap.dialogue.UnidirectionalIndEvent;
import jain.protocol.ss7.tcap.dialogue.UserAbortIndEvent;
import jain.protocol.ss7.tcap.dialogue.UserAbortReqEvent;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Date;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.sip.ServletParseException;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipFactory;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.SipSessionsUtil;
import javax.servlet.sip.URI;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.bn.exceptions.EnumParamOutOfRangeException;

import com.agnity.ain.asngenerated.AcgArg;
import com.agnity.ain.asngenerated.ApplicationErrorString;
import com.agnity.ain.asngenerated.ApplicationErrorString.ApplicationErrorStringSequenceType;
import com.agnity.ain.asngenerated.ControlCauseIndicator;
import com.agnity.ain.asngenerated.ErrorCause;
import com.agnity.ain.asngenerated.ErrorCause.ErrorCauseEnumType;
import com.agnity.ain.asngenerated.ErrorCause.ErrorCauseEnumType.EnumType;
import com.agnity.ain.asngenerated.GapDuration;
import com.agnity.ain.asngenerated.GapDuration.GapDurationEnumType;
import com.agnity.ain.asngenerated.GapInterval;
import com.agnity.ain.asngenerated.GlobalTitleAddress;
import com.agnity.ain.asngenerated.NationalGapInterval;
import com.agnity.ain.asngenerated.NationalGapInterval.NationalGapIntervalEnumType;
import com.agnity.ain.asngenerated.PrivateGapInterval;
import com.agnity.ain.asngenerated.PrivateGapInterval.PrivateGapIntervalEnumType;
import com.agnity.ain.datatypes.AddressSignal;
import com.agnity.ain.datatypes.T1Digits;
import com.agnity.ain.asngenerated.TranslationType;
import com.agnity.ain.enumdata.CauseValEnum;
import com.agnity.ain.enumdata.NatureOfNumEnum;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.operations.AinOpCodes;
import com.agnity.ain.operations.AinOperationsCoding;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.Action.CONNECTIONMODE;
import com.agnity.mphdata.common.Action.CONTINUE_MODE;
import com.agnity.mphdata.common.Action.DROP_CALL_MODE;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CarrierInfo;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.State;
import com.agnity.ph.ainscf.gr533.Gr533MessageHandler;
import com.agnity.ph.ainscf.gr533.Gr533ProblemCodes;
import com.agnity.ph.ainscf.lidb.AccountCode;
import com.agnity.ph.ainscf.lidb.BNSQuery;
import com.agnity.ph.ainscf.lidb.CC1Query;
import com.agnity.ph.ainscf.lidb.CC2Query;
import com.agnity.ph.ainscf.lidb.GenericNameQuery;
import com.agnity.ph.ainscf.lidb.GetDataQuery;
import com.agnity.ph.ainscf.lidb.ICDCQuery;
import com.agnity.ph.ainscf.lidb.InterceptQuery;
import com.agnity.ph.ainscf.lidb.OLNSQuery;
import com.agnity.ph.ainscf.lidb.TLNSQuery;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.ainscf.mrs.MRSMessageHandler;
import com.agnity.ph.ainscf.tr533.TR533MessageHandler;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolHandler;
import com.agnity.ph.common.ProtocolHandlerFactory;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallChainedAttributes;
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
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolFSMHandler.AbortInfoEnum;
import com.agnity.ph.inapcs1scf.InapCS1ScfRelReasonCode;
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolUtil;
import com.agnity.ph.sip.SipProtocolConfig;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.router.acm.AseAppChainManager;
import com.baypackets.ase.sbb.OutboundGateway;
import com.baypackets.ase.sbb.OutboundGatewaySelector;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;

/**
 * This class implement helper functions to handle incoming TCAP dialogues
 * (Begin, Continue and END) and AIN messages. Please note that JAIN TCAP API
 * doesn't implement ANSI TCAP dialogues and its mapped to ITU-T TCAP dialogue
 * portion.
 * 
 * @author reeta
 *
 */
public class AinScfProtocolHelper {

	private static Logger logger = Logger.getLogger(AinScfProtocolHelper.class);
	private static Object src = "source".intern();

	/**
	 * It defines the different connect types possible in AIN call.
	 */
	private static enum CONNECT_TYPE {
		TERMINATING, PORTED, CORRELATION
	}

	@SuppressWarnings("unchecked")
	public static void traceMessage(CallData callData, TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside AIN traceMessage() for service...");
		}
		String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);
		int dialogueId = tcapSession.getDialogueId();
		int callState = CallTraceService.CALL_IN_PROGRESS;

		 LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			
			String caller = null;
			String called = null;
			if (legData != null) {
				PhoneNumber callingP = (PhoneNumber) legData
						.get(LegDataAttributes.P_CALLING_PARTY);
				if (callingP != null) {
					caller = callingP.getAddress();
				}
				PhoneNumber calledP = (PhoneNumber) legData
						.get(LegDataAttributes.P_CALLED_PARTY);
				if (calledP != null) {
					called = calledP.getAddress();
				}
			}
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
						.trace(constraint, AinScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(), callState,caller,called);
					}
				}
				traceMsg.delete(0, traceMsg.length());
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exitting AIN traceMessage()...");
		}
	}

	/**
	 * This method will trace incoming AIN Dialogue; From try block we are catching
	 * throwable to avoid any impact on call in case of error;
	 * 
	 * @param die               represents the instance of DialogueIndEvent
	 * @param callData          represents the instance of CallData
	 * @param cCallTraceService represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueIndEvent die, CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside traceDialog");
		}
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		try {
			String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);

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
					traceMsg.append(CommonUtils.formatBytes(((UserAbortIndEvent) die).getUserAbortInformation()));
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

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					@SuppressWarnings("unchecked")
					List<Integer> constraintList = (List<Integer>) callData
					.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(),
								AinScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(),
								callState);
					}
				}

			} // end if tracing required
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
	 * This method performs the processing of Begin dialogue indication event. In
	 * case ANSI this method will be called in case of Query With Permission or
	 * Query without permission.
	 * 
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @throws MandatoryParameterNotSetException
	 */
	static void processBegin(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession)
			throws MandatoryParameterNotSetException {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] processBegin Enter ");
		}

		/*
		 * call data can't be null if it has passed validation method so moving ahead
		 * without null check store dialog
		 */

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: [PH] Set Dialogue Portion in call data");
			}

			try {
				/*
				 * Store Dialogue Portion in Leg Data.
				 */
				legData.set(LegDataAttributes.P_DIALOG_PORTION, dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId + ":: [PH] Error getting dialogue portion " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + ":: [PH] Error getting dialog portion.", e);
					logger.info(dialogId + "::[PH] IGNORE ParameterNotSetException fetching "
							+ "dialoguePortion from BEGIN dialogue event");
				}
			}
		}

		if(dialogueIndEvent.isReturnOptionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH] Set Return Option value" + dialogueIndEvent.getM_returnOption());
			}
			legData.set(LegDataAttributes.P_RETURN_OPTION, dialogueIndEvent.getM_returnOption());
		}
		
		if(dialogueIndEvent.isSequenceControlPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH] Set Sequence Control value" + dialogueIndEvent.getM_sequenceControl());
			}
			legData.set(LegDataAttributes.P_SEQUENCE_CONTROL, dialogueIndEvent.getM_sequenceControl());
		}
		
		if(dialogueIndEvent.isMessagePriorityPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH] Set Message Priority value" + dialogueIndEvent.getM_messagePriority());
			}
			legData.set(LegDataAttributes.P_MESSAGE_PRIORITY, dialogueIndEvent.getM_messagePriority());
		}
		
		/*
		 * perform ACN check
		 */
		BeginIndEvent beginIndEvent = (BeginIndEvent) dialogueIndEvent;
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " ::[PH] isAppContextNamePresent = " + beginIndEvent.isAppContextNamePresent());
		}
		if (beginIndEvent.isAppContextNamePresent()) {
			try {
				byte[] appContextName = beginIndEvent.getAppContextName();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " ::[PH] Application Context version from IDP = "
							+ CommonUtils.formatBytes(appContextName));
				}

				/*
				 * As isAppContextNamePresent always returns true even if appCOntextName is null
				 * making null check
				 */
				if (appContextName != null) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + " :: [PH] Application Context not null");
					}
					legData.set(LegDataAttributes.P_ACTIVITY_CONTEXT_VERSION,
							appContextName[appContextName.length - 1]);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + " :: [PH] Application Context is null");
					}

					/*
					 * valid acn handling is odne at service in test call only return
					 * handleInavlidAcn(tcapSession);
					 */
				}
			} catch (Exception e) {
				logger.warn(dialogId + ":: [PH] Error fetching AC version " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + " :: [PH] Exception fetching AC Version", e);
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH] Set Application Context version "
						+ legData.get(LegDataAttributes.P_ACTIVITY_CONTEXT_VERSION) + " in call data");
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: isAppContextNamePresent is false");
			}
		}

		try {

			legData.set(LegDataAttributes.P_BEGIN_QOS, beginIndEvent.getQualityOfService());
			SccpUserAddress sccpUserAddress = beginIndEvent.getOriginatingAddress();
			SignalingPointCode signalingPointCode = sccpUserAddress.getSubSystemAddress().getSignalingPointCode();
			int zone = signalingPointCode.getZone();
			int cluster = signalingPointCode.getCluster();
			int member = signalingPointCode.getMember();
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] Origin zone= " + zone + " cluster=" + cluster + " member=" + member);
			}
			
			if(zone==0 && cluster==0 && member==0) {
				logger.debug("0 found for zone , cluster and member replacing with mtp3Opc");
				SignalingPointCode spc =beginIndEvent.getMtp3Opc();
				zone= spc.getZone();
				cluster=spc.getCluster();
				member=spc.getMember();
			}

			logger.debug("Zone:- "+ zone+ " cluster:- "+ cluster + " member:- "+ member);
			String zcmFormat = zone + "-" + cluster + "-" + member;

			String pcBitStr = lPad(Integer.toBinaryString(zone), 8) + lPad(Integer.toBinaryString(cluster), 8)
			+ lPad(Integer.toBinaryString(member), 8);
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] pcBitStr =" + pcBitStr);
			}
			int pc = Integer.parseInt(pcBitStr, 2);
			legData.set(LegDataAttributes.P_OPC, pc);
			legData.set(LegDataAttributes.P_SPC, zcmFormat);
			callData.set(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS, beginIndEvent.getDestinationAddress());
			callData.set(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS, beginIndEvent.getOriginatingAddress());
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] Calculated Origin Point Code form IDP-Begin ="
						+ legData.get(LegDataAttributes.P_OPC));
			}

		} catch (ParameterNotSetException e1) {
			logger.error(dialogId + " :: [PH] Failed to get origin point code from Dialog Indication event. "
					+ e1.getMessage());
		}

		/*
		 * Use the sccp address received in IDP to support multiple pc-ssn
		 */
		legData.set(LegDataAttributes.P_SUA, beginIndEvent.getDestinationAddress());
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: processBegin Exit ");
		}
	}

	/**
	 * This method appends 0 as leading digits to input.
	 * 
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
	 * This method performs the processing of continue dialogue indication event. In
	 * ANSI, TCAP dialogue being received as CwP or CwoP shall be mapped to Continue
	 * 
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action Objects
	 * @throws MandatoryParameterNotSetException 
	 */
	static Action[] processContinue(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) throws MandatoryParameterNotSetException {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: processContinue Enter ");
		}

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Set Dialogue Portion in call data");
			}

			try {
				legData.set(LegDataAttributes.P_DIALOG_PORTION, dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId + ":: Error getting dialogue portion. " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + ":: Error getting dialog portion.", e);
					logger.info(dialogId + "::IGNORE ParameterNotSetException fetching "
							+ "dialoguePortion from CONTINUE dialogue event");
				}
			}
		}
		
		if(dialogueIndEvent.isReturnOptionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH] Set Return Option value" + dialogueIndEvent.getM_returnOption());
			}
			legData.set(LegDataAttributes.P_RETURN_OPTION, dialogueIndEvent.getM_returnOption());
		}
		
		if(dialogueIndEvent.isSequenceControlPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH] Set Sequence Control value" + dialogueIndEvent.getM_sequenceControl());
			}
			legData.set(LegDataAttributes.P_SEQUENCE_CONTROL, dialogueIndEvent.getM_sequenceControl());
		}
		
		if(dialogueIndEvent.isMessagePriorityPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH] Set Message Priority value" + dialogueIndEvent.getM_messagePriority());
			}
			legData.set(LegDataAttributes.P_MESSAGE_PRIORITY, dialogueIndEvent.getM_messagePriority());
		}
		
		return null;

	}

	/**
	 * This method performs the processing of TC_END dialogue indication event. In
	 * ANSI, Response is received which is mapped to END.
	 * 
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processEnd(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processEnd Enter ");

		}

		return null;
	}

	/**
	 * This method performs the processing of Abort dialogue indication event. Marks
	 * the cause value as 41 for failed calls.
	 * 
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processAbort(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processAbort Enter ");
		}

		return null;
	}

	/**
	 * This method performs the processing of UAbort dialogue indication event.
	 * Marks the cause value as 31 because user hung up the call and related events
	 * are not armed..
	 * 
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processUAbort(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processUAbort Enter ");
		}

		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever an InfoAnalyze is
	 * received.
	 * 
	 * @param invokeIndEvent represents the instance of InvokeIndEvent
	 * @param tcapSession    represents the instance of TcapSession
	 * @param cts            represents the instance of CallTraceService
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] processInfoAnalyzed(InvokeIndEvent invokeIndEvent, TcapSession tcapSession, 
			CallTraceService callTraceService) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processInfoAnalyzed Enter ");
		}

		/*
		 * change state to service logic
		 */
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);

		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke INFO_ANALYZE::" + invokeId);
		}

		/*
		 * parse IDP
		 */
		try {
			AinScfProtocolParser.parseInfoAnalyzed(invokeIndEvent, callData);

			// based on last message received, response shall be framed.
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.INFO_ANALYZE);

		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in INFO_ANALYZE.", ape);

			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.INFO_ANALYZ_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.INFO_ANALYZ_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in INFO_ANALYZE.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INFO_ANALYZ_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in INFO_ANALYZE.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INFO_ANALYZ_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.INFO_ANALYZE);
		}

		checkCallTracingEnabled(dialogId, callData, callTraceService);

		//		/*
		//		 * Fields for call tracing
		//		 */
		//		String callingPartyAddress = legData.get(LegDataAttributes.P_CALLING_PARTY) == null ? null
		//				: ((PhoneNumber) legData.get(LegDataAttributes.P_CALLING_PARTY)).getAddress();
		//		String calledPartyAddress = legData.get(LegDataAttributes.P_CALLED_PARTY) == null ? null
		//				: ((PhoneNumber) legData.get(LegDataAttributes.P_CALLED_PARTY)).getAddress();
		//		String idpCalledPartyAddress = legData.get(LegDataAttributes.P_IDP_CALLED_PARTY) == null ? null
		//				: ((PhoneNumber) legData.get(LegDataAttributes.P_IDP_CALLED_PARTY)).getAddress();
		//		String signalingPointCode = legData.get(LegDataAttributes.P_SPC) == null ? null
		//				: legData.get(LegDataAttributes.P_SPC).toString();
		//
		//		/*
		//		 * match call tracing constraints
		//		 */
		//		List constraintIdList = cts.matchesCriteria(callingPartyAddress, idpCalledPartyAddress, calledPartyAddress);
		//
		//		if (constraintIdList != null && !constraintIdList.isEmpty()) {
		//			callData.set(CallDataAttribute.P_TRACE_CONSTRAINT_ID, constraintIdList);
		//			callData.set(CallDataAttribute.P_TRACE_FLAG, PhConstants.TRUE);
		//			callData.set(CallDataAttribute.P_TRACE_MESSAGE, new StringBuilder());
		//		}
		//
		//		if (logger.isDebugEnabled()) {
		//			logger.debug("[PH]:: constraintId=" + constraintIdList + ", TraceFlag="
		//					+ Boolean.valueOf((String) callData.get(CallDataAttribute.P_TRACE_FLAG)));
		//			logger.debug("[PH]:: processIdp return and start processing");
		//		}
		//
		//		if (logger.isDebugEnabled()) {
		//			logger.debug(dialogId + " :: [PH] constraintId=" + constraintIdList + ", TraceFlag="
		//					+ Boolean.valueOf((String) callData.get(CallDataAttribute.P_TRACE_FLAG)));
		//			logger.debug(dialogId + " [PH]::processInfoAnalyzed return and start processing");
		//		}

		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever ProviderInstruction
	 * Operation is received.
	 * 
	 * @param invokeIndEvent represents the instance of InvokeIndEvent
	 * @param tcapSession    represents the instance of TcapSession
	 * @param cts            represents the instance of CallTraceService
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] processProvideInstruction(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService cts) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processProviderInstruction Enter ");
		}

		/*
		 * change state to PROVIDE_INSTRUCTION
		 */
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.PROVIDE_INSTRUCTION);
		callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.PROVIDE_INSTRUCTION);

		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke PROVIDER_INSTRUCTION::" + invokeId);
		}
		try {
			AinScfProtocolParser.parseProvideInstruction(invokeIndEvent, callData);

		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in parseProviderInstruction.", ape);

			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.PROVIDE_INSTRUCTION_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.PROVIDE_INSTRUCTION_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in PROVIDER_INSTRUCTION.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
					AinScfRelReasonCode.PROVIDE_INSTRUCTION_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in PROVIDER_INSTRUCTION.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
					AinScfRelReasonCode.PROVIDE_INSTRUCTION_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.PROVIDE_INSTRUCTION);
		}
		return null;

	}

	public static Action[] processAcQuery(ComponentIndEvent componentIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processAcQuery Enter ");
		}
		callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.AC_QUERY);
		/*
		 * store invoke id
		 */
		int invokeId = ((InvokeIndEvent) componentIndEvent).getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke AC Query::" + invokeId);
		}
		//setting service type to MRS		
		legData.set(LegDataAttributes.P_SERVICE_TYPE, PhConstants.MRS);
		legData.set(LegDataAttributes.P_MRS_QTYPE, PhConstants.CLASS);

		if(legData.get(LegDataAttributes.P_MRS_COMP_LIST) == null) {
			ArrayList<InvokeIndEvent> invokeIndEventList = new ArrayList<InvokeIndEvent>();
			legData.set(LegDataAttributes.P_MRS_COMP_LIST, invokeIndEventList);
		}
		if (!componentIndEvent.isLastComponent()) {
			//check need to decode or not
			if(MRSMessageHandler.isSixDigitsSCCP(callData)) {
				MRSMessageHandler.parseISVMtcapInitialQuery((InvokeIndEvent) componentIndEvent, callData);
			}
			//NOT LAST COMPONENT
			@SuppressWarnings("unchecked")
			ArrayList<InvokeIndEvent> list = (ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST) == null?
					new ArrayList<InvokeIndEvent>():(ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST);
					list.add((InvokeIndEvent) componentIndEvent);
					legData.set(LegDataAttributes.P_MRS_COMP_LIST, list);
					if (logger.isInfoEnabled()) {
						logger.info("AC Query component list (NOT LAST):"+list);
					}

		} else {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.AC_QUERY);
			@SuppressWarnings("unchecked")
			ArrayList<InvokeIndEvent> list = (ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST);
			list.add((InvokeIndEvent) componentIndEvent);
			legData.set(LegDataAttributes.P_MRS_COMP_LIST, list);
			if (logger.isInfoEnabled()) {
				logger.info("AC Query component list (LAST):"+list);
			}
		}
		return null;

	}

	/**
	 * This method is called by the Protocol handler whenever an InfoAnalyze is
	 * received.
	 * 
	 * @param invokeIndEvent represents the instance of InvokeIndEvent
	 * @param tcapSession    represents the instance of TcapSession
	 * @param cts            represents the instance of CallTraceService
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] processTerminationAttempt(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService cts) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processTerminationAttempt Enter ");
		}

		/*
		 * change state to service logic
		 */
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);

		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke TERMINATION ATTEMPT::" + invokeId);
		}

		/*
		 * parse Termination Attempt
		 */
		try {
			AinScfProtocolParser.parseTerminationAttempt(invokeIndEvent, callData);

			// based on last message received, response shall be framed.
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.TERM_ATTEMPT);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in TERMINATION ATTEMPT.", ape);

			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.TERMINATION_ATTEMPT_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.TERMINATION_ATTEMPT_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in TERMINATION ATTEMPT.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
					AinScfRelReasonCode.TERMINATION_ATTEMPT_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.TERM_ATTEMPT);
		}

		/*
		 * Fields for call tracing
		 */
		String callingPartyAddress = legData.get(LegDataAttributes.P_CALLING_PARTY) == null ? null
				: ((PhoneNumber) legData.get(LegDataAttributes.P_CALLING_PARTY)).getAddress();
		String calledPartyAddress = legData.get(LegDataAttributes.P_CALLED_PARTY) == null ? null
				: ((PhoneNumber) legData.get(LegDataAttributes.P_CALLED_PARTY)).getAddress();
		String idpCalledPartyAddress = legData.get(LegDataAttributes.P_IDP_CALLED_PARTY) == null ? null
				: ((PhoneNumber) legData.get(LegDataAttributes.P_IDP_CALLED_PARTY)).getAddress();
		String signalingPointCode = legData.get(LegDataAttributes.P_SPC) == null ? null
				: legData.get(LegDataAttributes.P_SPC).toString();

		/*
		 * match call tracing constraints
		 */
		List constraintIdList = cts.matchesCriteria(callingPartyAddress, idpCalledPartyAddress, calledPartyAddress);

		if (constraintIdList != null && !constraintIdList.isEmpty()) {
			callData.set(CallDataAttribute.P_TRACE_CONSTRAINT_ID, constraintIdList);
			callData.set(CallDataAttribute.P_TRACE_FLAG, PhConstants.TRUE);
			callData.set(CallDataAttribute.P_TRACE_MESSAGE, new StringBuilder());
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: constraintId=" + constraintIdList + ", TraceFlag="
					+ Boolean.valueOf((String) callData.get(CallDataAttribute.P_TRACE_FLAG)));
			logger.debug("[PH]:: processTerminationAttempt return and start processing");
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: [PH] constraintId=" + constraintIdList + ", TraceFlag="
					+ Boolean.valueOf((String) callData.get(CallDataAttribute.P_TRACE_FLAG)));

			logger.debug(dialogId + " [PH]::processTerminationAttempt return and start processing");
		}

		checkCallTracingEnabled(dialogId, callData,  cts);

		return null;
	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of PRIMITIVE_RESULT type is received.
	 * 
	 * @param resultIndEvent represents an instance of ResultIndEvent
	 * @param tcapSession    represents an instance of TcapSession
	 * @return an array of Action Object
	 * @throws MandatoryParameterNotSetException
	 */
	static Action[] processResult(ResultIndEvent resultIndEvent, TcapSession tcapSession)
			throws MandatoryParameterNotSetException {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
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
					logger.error(dialogId + ":: Error getting operation code " + e.getMessage());
					throw e;
				} catch (ParameterNotSetException e) {
					logger.warn(dialogId + ":: Error getting operation code " + e.getMessage());
					if (logger.isInfoEnabled()) {
						logger.warn(dialogId + ":: Error getting operation code", e);
						logger.info(dialogId + ":: IGNORE ParameterNotSetException in getOperation.");
					}
				}
			}
			/*
			 * opcode not present get invoke id and fetch opcode form there
			 */
			else if (resultIndEvent.isInvokeIdPresent()) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Invoke Id present in event");
				}

				try {
					Object atInvokeId = callData.get(CallDataAttribute.P_AT_INVOKE_ID);
					if (atInvokeId != null && ((Integer) atInvokeId == resultIndEvent.getInvokeId())) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogId + ":: It is Activity Test Event");
						}

						operCode = new byte[1];
						operCode[0] = AinOpCodes.ACTIVITY_TEST_BYTE;
					}
				} catch (ParameterNotSetException e) {
					logger.warn(dialogId + ":: Error getting invoke id " + e.getMessage());
					if (logger.isInfoEnabled()) {
						logger.warn(dialogId + ":: Error getting invoke id", e);
						logger.debug(dialogId + ":: IGNORE ParameterNotSetException in getInvokeId.", e);
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
					logger.debug(dialogId + ":: Operation Code is " + operCodeStr);
				}

				switch (operCodeByte) {
				case AinOpCodes.ACTIVITY_TEST_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Cancel the Activity Test Acknowledgement Timer");
					}
					cancelATTimer(tcapSession);
					break;
				}
				default: {
					logger.warn(dialogId + ":: Received Result for unknown Component Indication Event " + operCodeStr);
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNKNOWN_RESULT);
					action = getUnknownResultAction(tcapSession);
					break;
				} // end default
				}// end switch
			} else {
				logger.warn(dialogId + "::Result Operation code is unknown.");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNKNOWN_RESULT);
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
	 * @param errorIndEvent represents an instance of ErrorIndEvent
	 * @param tcapSession   represents an instance of TcapSession
	 * @return an array of Action Objects.
	 */
	static Action[] processError(ErrorIndEvent errorIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processError");
		}

		int invokeId = -1;
		byte[] errorCode = null;

		try {
			invokeId = errorIndEvent.getInvokeId();
			errorCode = errorIndEvent.getErrorCode();
		} catch (MandatoryParameterNotSetException mpne) {
			logger.error(dialogId + ":: MandatoryParameterNotSetException in RE", mpne);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.ERR_MSG_MAND_PARAM_MISSING);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UERROR, FAILTYPE.DEFAULT);
		} catch (ParameterNotSetException pne) {
			logger.error(dialogId + ":: ParameterNotSetException in RE", pne);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.ERR_MSG_PARAM_NOT_SET);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UERROR, FAILTYPE.DEFAULT);
		}

		int lastInvokeIdStart = AinScfProtocolUtil.getLastInvokeIdStartRange(callData);
		int lastInvokeIdEnd = AinScfProtocolUtil.getLastInvokeIdEndRange(callData);
		/*
		 * validateInvokeId
		 */
		if ((invokeId < lastInvokeIdStart) || (invokeId > lastInvokeIdEnd)) {
			logger.warn(dialogId + "::Invoke id invalid in U error; recived:" + invokeId
					+ " valid range for current message:: " + lastInvokeIdStart + " to " + lastInvokeIdEnd);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.ERR_MSG_INVALID_INVOK_ID);
			return getInvalidInvokeIdAction(tcapSession, MESSAGE.UERROR);
		}

		if (errorCode != null && logger.isDebugEnabled()) {
			String errorCodeStr = CommonUtils.formatBytes(errorCode);
			logger.debug(dialogId + ":: Error code is " + errorCodeStr);

		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Return action to Drop the call");
		}
		Action action = new Action(Action.ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		/*
		 * set cause as temporary failure
		 */
		action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());

		return (new Action[] { action });

	}

	/**
	 * This method is used to Cancel the Activity Test Acknowledgment Timer.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 */
	private static void cancelATTimer(TcapSession tcapSession) {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside cancelATTimer");
		}
		AinScfProtocolUtil.stopTimer(tcapSession, PhConstants.AT_ACK_TIMER);

	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of type PRIMITIVE_REJECT is received.
	 * 
	 * @param rejectIndEvent represents the instance of RejectIndEvent
	 * @param tcapSession    represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	public static Action[] processReject(RejectIndEvent rejectIndEvent, TcapSession tcapSession) {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processReject");
		}
		int rejectType = -1;
		int problemType = -1;
		int problem = -1;

		/*
		 * get reject type
		 */
		if (rejectIndEvent.isRejectTypePresent()) {
			try {
				rejectType = rejectIndEvent.getRejectType();
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId + ":: Error in getting reject type " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + ":: Error getting reject type", e);
					logger.info(dialogId + ":: IGNORE ParameterNotSetException in getting reject type.");
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Reject Type is [" + rejectType + "]");
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
			logger.error(dialogId + ":: MandatoryParameterNotSetException in UREJECT", mpne);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.REJECT_MSG_MAND_PARAM_MISSING);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UREJECT, FAILTYPE.DEFAULT);
		} catch (ParameterNotSetException pne) {
			logger.error(dialogId + ":: ParameterNotSetException in UREJECT", pne);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.REJECT_MSG_PARAM_NOT_SET);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UREJECT, FAILTYPE.DEFAULT);
		}

		/*
		 * validateInvokeId
		 */

		int lastInvokeIdStart = AinScfProtocolUtil.getLastInvokeIdStartRange(callData);
		int lastInvokeIdEnd = AinScfProtocolUtil.getLastInvokeIdEndRange(callData);
		if ((invokeId < lastInvokeIdStart) || (invokeId > lastInvokeIdEnd)) {
			logger.error(dialogId + "::Invoke id invalid in U Reject; recived:" + invokeId
					+ " valid range for current message:: " + lastInvokeIdStart + " to " + lastInvokeIdEnd);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.REJECT_INVALID_INVOK_ID);
			return getInvalidInvokeIdAction(tcapSession, MESSAGE.UREJECT);
		}

		/*
		 * validate problem type
		 */
		if (problemType != ComponentConstants.PROBLEM_TYPE_INVOKE) {
			logger.error(dialogId + "::Problem type invalid in U reject; recived:" + invokeId + " problemType:"
					+ problemType + " Expected problem type:" + ComponentConstants.PROBLEM_TYPE_INVOKE);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.REJECT_INVALID_PROBLEM_TYPE);
			return getInvalidProblemAction(tcapSession, MESSAGE.UREJECT);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Problem Type is [" + problemType + "]  problem is  [" + problem + "]");
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
	 * @param tcapSession represents an instance of TcapSession
	 * @return an array of Action objects.
	 */
	static Action[] getUnknownDialogAction(TcapSession tcapSession) {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] Inside getUnknownDialogAction:: " + "ignore unknown dialog events");
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
	 * @param tcapSession represents an instance of TcapSession
	 * @return an array of Action objects.
	 */
	static Action[] getUnknownResultAction(TcapSession tcapSession) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getUnknownResultAction:: " + "Drop call with U-Reject");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
		action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_RETURN_RESULT_UNEXPECTED);

		return (new Action[] { action });

	}

	/**
	 * This method returns action to be taken when an out of sequence dialogue
	 * indication event is received. In case TC_END is not received just release the
	 * call with CV=41.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @param dialogType  represents an integer representation of dialogue type.
	 * @return an array of Action Objects
	 */
	static Action[] getOutOfSequenceDialogAction(TcapSession tcapSession, int dialogType) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getOutOfSequenceDialogAction:: Drop Call");
		}
		Action action = null;

		AinCallStates callState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		if (callState == AinCallStates.INIT) {
			/*
			 * send UAbort for terminated calls
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: [PH]Last Rx dlg not END do release calls:: ");
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
					logger.debug(dialogId + ":: [PH] Inside handleOutOfSequenceDialog:: unknown begin/continue");
				}
				action = new Action(ActionType.ACTION_END_CALL);
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
				break;
			}
			default: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: [PH] Inside handleOutOfSequenceDialog:: unknown dialog " + dialogType);
				}
				action = new Action(ActionType.ACTION_END_CALL);
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
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
	 * @param tcapSession represents an instance of TcapSession
	 * @param message     represents an instance of MESSAGE
	 * @return an array of Action Objects.
	 */
	static Action[] getInvalidInvokeIdAction(TcapSession tcapSession, MESSAGE message) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getInvalidInvokeIdAction with message::" + message);
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
	 * event of type PRIMITIVE_REJECT is received and problem received is invalid.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @param message     represents an instance of MESSAGE
	 * @return an array of Action Objects
	 */
	static Action[] getInvalidProblemAction(TcapSession tcapSession, MESSAGE message) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: Inside getInvalidProblemAction with message::" + message);
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
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param tcapSession    represents an instance of TcapSession
	 * @param ape            represents an instance of ASNParsingException
	 * @return an array of Action objects.
	 */
	public static Action[] getASNParsingFailureAction(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			ASNParsingException ape) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: Inside handleASNParsingFailure with exception::" + ape);
		}
		MESSAGE message = ape.getInapMessage();
		return getASNParsingFailureAction(tcapSession, message, ape.getParseFailType());

	}

	/**
	 * Method to retrieve actions to be performed on encountering ASNParsing
	 * Exception.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @param message     represents an instance of MESSAGE
	 * @param failtype    represents an instance of FAILTYPE
	 * @return an array of Action objects.
	 */
	static Action[] getASNParsingFailureAction(TcapSession tcapSession, MESSAGE message, FAILTYPE failtype) {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: Inside getASNParsingFailureAction with message::" + message
					+ "   failtype::" + failtype);
		}
		Action[] actionArr = null;

		switch (message) {
		case INFO_ANALYZE: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			switch (failtype) {
			case CPC_MISSING: {
				action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
				action.setReleaseCauseValue(AbortInfoEnum.ABNORMAL_PROCESSING.getCode());
				break;
			}
			case TMR_MISSING:
			case ACPC_MISSING: {
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
				break;
			}
			case FCI_MISSING: {
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Service_not_available.getCode());
				break;
			}
			case DEFAULT:
			default: {
				action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
				action.setReleaseCauseValue(AbortInfoEnum.NO_REASON_GIVEN.getCode());
				break;
			}
			}
			actionArr = (new Action[] { action });
			break;
		}
		case LIDB_PROTOCOL_ERR: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			switch (failtype) {
			case PROTOCOL_ERROR:
				action.setDropCallMode(DROP_CALL_MODE.LIDB_PROTOCOL_ERROR);
				action.setReleaseCauseValue(AbortInfoEnum.LIDB_PROTOCOL_ERROR.getCode());
				break;

			default: {
				action.setDropCallMode(DROP_CALL_MODE.LIDB_PROTOCOL_ERROR);
				action.setReleaseCauseValue(AbortInfoEnum.LIDB_PROTOCOL_ERROR.getCode());
				break;
			}
			}
			actionArr = (new Action[] { action });
			break;
		}
		case PROVIDE_INSTRUCTION: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			switch (failtype) {
			case DEFAULT:
				action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
				action.setReleaseCauseValue(AbortInfoEnum.PROVIDE_INSTRUCTION_PARSE_ERROR.getCode());
				break;
			default: {
				action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
				action.setReleaseCauseValue(AbortInfoEnum.PROVIDE_INSTRUCTION_PARSE_ERROR.getCode());
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
	 * CriticalityTypeException is thrown while parsing InfoAnalyze received.
	 * 
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param tcapSession    represents an instance of TcapSession
	 * @param cte            represents an instance of CriticalityTypeException
	 * @return an array of Action objects.
	 * @throws Exception
	 */
	public static Action[] getInvalidExtensionTypeAction(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CriticalityTypeException cte) throws Exception {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getInvalidExtensionTypeAction:: Drop call");
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
			action.setReleaseCauseValue(CauseValEnum.Invalid_information_element.getCode());
		} else if (cte.getCriticality().equals(CRITICALITY.ABORT)) {
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(AbortInfoEnum.UNRECOGNIZED_EXTENSION_PARAMETER.getCode());
		}

		return (new Action[] { action });
	}

	/**
	 * This method is called by the Protocol handler whenever
	 * ParameterOutOfRangeException is thrown while parsing AIN signal received.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @param message     represents an instance of MESSAGE
	 * @return an array of Action objects.
	 */
	public static Action[] getOutOfRangeParamterAction(TcapSession tcapSession, MESSAGE message) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getOutOfRangeParamterAction::" + message);
		}

		/*
		 * handle as ASN parsing
		 */
		return getASNParsingFailureAction(tcapSession, message, FAILTYPE.DEFAULT);

	}

	/**
	 * This method return actions to be performed to Protocol handler when calling
	 * party is missing from IDP.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] getCallingPartyMissingAction(TcapSession tcapSession) throws Exception {

		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getCallingPartyMissingAction:: Drop call");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
		return (new Action[] { action });

	}

	/**
	 * This method return actions to be performed to Protocol handler when unknown
	 * message is received.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @param event       represents an instance of AinEvent
	 * @return an array of Action Objects
	 */
	static Action[] getUnknownMessageAction(TcapSession tcapSession, AinScfProtocolEvent event) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getUnknownMessageAction:: same as handleOutOfSequenceMsg");
		}
		return getOutOfSequenceMsgAction(tcapSession, event);

	}

	/**
	 * This method return actions to be performed to Protocol handler when unarmed
	 * ERB event is received.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @param event       represents an instance of AinEvent
	 * @return an array of Action Objects
	 */
	static Action[] getUnArmedErbEventAction(TcapSession tcapSession, AinScfProtocolEvent event) {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getUnArmedErbEventAction send reject and RC");
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
	 * This method return actions to be performed to Protocol handler when out of
	 * sequence message is received.
	 * 
	 * @param tcapSession represents an instance of TcapSession
	 * @param event       represents an instance of AinEvent
	 * @return an array of Action Objects
	 */
	static Action[] getOutOfSequenceMsgAction(TcapSession tcapSession, AinScfProtocolEvent event) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside getOutOfSequenceMsgAction:: Drop call");
		}

		/*
		 * release call if end dialog not exchanged yet.
		 */
		Action action = null;
		AinCallStates callState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		if (callState == AinCallStates.INIT) {
			/*
			 * send UAbort for terminated calls
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + "[PH] ::Last Rx dlg not END do release calls:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			/*
			 * set reason as no reason given
			 */
			action.setReleaseCauseValue(AbortInfoEnum.NO_REASON_GIVEN.getCode());
			// } else if (callState == AinCallStates.BNS_QUERY) {
			// action = new Action(ActionType.ACTION_END_CALL);
			// action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			// action.setReleaseCauseValue(AbortInfoEnum.NO_REASON_GIVEN.getCode());
			// } else if (callState == AinCallStates.GN_QUERY) {
			// action = new Action(ActionType.ACTION_END_CALL);
			// action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			// action.setReleaseCauseValue(AbortInfoEnum.NO_REASON_GIVEN.getCode());
			// }
		} else if (callState != AinCallStates.TERMINATED) {
			/*
			 * check if it was unarmed event case or event in invalid call state
			 */
			Object unarmedEventError = tcapSession.getAttribute(PhConstants.UNARMED_ERROR_TYPE);
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
				logger.debug(dialogId + ":: [PH] Last call state is not terminated create action:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());

		} else {
			/*
			 * send UAbort for terminated calls
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: [PH] Last Rx dlg not END do release calls:: ");
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
	 * In case of drop call event, this method sends appropriate AIN message to the
	 * switch.
	 * 
	 * @param tcapSession       represents an instance of TcapSession
	 * @param action            represents an instance of Action
	 * @param cCallTraceService represents an instance of CallTraceService
	 * @throws Exception
	 */
	public static void sendDropMessage(TcapSession tcapSession, Action action, CallTraceService cCallTraceService)
			throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]::Inside sendDropMessage");
		}
		DROP_CALL_MODE dropCallMode = action.getDropCallMode();

		// Check if for this call we need not send prearranged end
		if (callData.get(CallDataAttribute.P_SEND_PREARRANGED_END) != null) {
			dropCallMode = DROP_CALL_MODE.NULL_END_PREARRANGED;
			if (logger.isDebugEnabled()) {
				logger.debug("Call needs to be terminated with pre-arranged end, setting DropCallMode:" 
						+ dropCallMode);
			}
		}

		/*
		 * set cause value for CDRS
		 */
		setCallDataParamsForCDR(callData, action);

		switch (dropCallMode) {

		// case GN_RESPONSE_ERROR:{
		// sendGnResponseError(tcapSession, action, cCallTraceService);
		// break;
		// }

		case GN_APPLICATION_ERROR: {
			sendGnAppError(tcapSession, action);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		}
		case GN_PROTOCOL_ERROR: {
			sendBnsProtocolError(tcapSession, action, cCallTraceService);

			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		}
		case OLNS_APPLICATION_ERROR: {
			sendBnsAppError(tcapSession, action);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		}
		case OLNS_PROTOCOL_ERROR: {
			sendBnsProtocolError(tcapSession, action, cCallTraceService);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		}
		case INITIAL_ERROR: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " ::[PH] Send RETURN_ERROR with TC_END");
			}

			sendErrorReqEvent(tcapSession, action, cCallTraceService);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		}
		case RELEASE_CALL:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH]  Send Disconnect  with TC_END");
			}

			sendDisconnect(tcapSession, action, cCallTraceService);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			break;
		case RELEASE_CALL_WITH_STR: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: [PH]  Send SendToResource  with TC_END");
			}

			sendSTR(tcapSession, action.getLeg(), true);
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
				logger.debug(dialogId + " :: [PH]  Send U_REJECT with TC_END");
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
		case NONE: {
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH]  Do nothing for drop call mode NONE");
			}
			break;
		}
		}// @end switch

	}

	/**
	 * This method is called from sendDropMessage, and is responsible for setting
	 * relevant information regarding CDR into CallData object being passed.
	 * 
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 */
	public static void setCallDataParamsForCDR(CallData callData, Action action) {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		DROP_CALL_MODE dropCallMode = action.getDropCallMode();
		int releaseCauseValue = action.getReleaseCauseValue();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH]  setCallDataParamsForCDR:: Drop call mode is " + dropCallMode);
		}

		/*
		 * setting cause value
		 */
		switch (dropCallMode) {
		case RELEASE_CALL: {
			String valueFromMsg = (String) callData.get(CallDataAttribute.P_CAUSE_VALUE_FROM_MSG);

			if (!(Boolean.valueOf(valueFromMsg))) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: [PH]  Set cause value from action " + action.getReleaseCauseValue());
				}
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, action.getReleaseCauseValue());
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: [PH]  Cause value in callData is " + releaseCauseValue);
				}
			}
			break;
		}
		case NULL_END: {
			if (releaseCauseValue <= 0 && action.getReleaseCauseValue() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: [PH]  Set release cause value from action to "
							+ action.getReleaseCauseValue());
				}
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, action.getReleaseCauseValue());
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: [PH]  Release cause value in callData is " + releaseCauseValue);
				}
			}
			break;
		}
		case INITIAL_ERROR: {
			if (action.getReleaseCauseValue() == 6) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " :: [PH]  Set attempted indicator to 5 as action cause value is 6");
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
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Release cause value in callData is " + releaseCauseValue);
				}
			}
			break;
		} // end initial error,uabort,default, u reject,nullend prearranged,none

		}// @end switch

	}

	/**
	 * This method is called by protocol handler for sending continue message.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @param action            represents the instance of Action
	 * @throws Exception
	 */
	public static void sendContinueMessage(TcapSession tcapSession, Action action, CallTraceService cCallTraceService)
			throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
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
		case ENTITY_RELEASE: {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: Send ER with TC_CON");
			}
			// sendEntityRelease(tcapSession, action, cCallTraceService);
			sendContinueRequestEvent(tcapSession, cCallTraceService);
			break;
		}

		}
	}

	/**
	 * This method is called by protocol handler for sending error request event.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @param action            represents the instance of Action
	 * @throws Exception
	 */
	static void sendErrorReqEvent(TcapSession tcapSession, Action action, CallTraceService cCallTraceService)
			throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendErrorReqEvent:"+action.getReleaseCauseValue());
		}

		// As per GR1299 ApplicationErrorString is sent as Error code 
		try {
			ApplicationErrorString appErrStr = new ApplicationErrorString();
			ApplicationErrorStringSequenceType appErrStrSeqType = new ApplicationErrorStringSequenceType();

			ErrorCause errCause = new ErrorCause();
			ErrorCauseEnumType errCauseEnumType = new ErrorCauseEnumType();

			switch(action.getReleaseCauseValue()){
			case 0:errCauseEnumType.setValue(EnumType.erroneousDataValue);break;
			case 1:errCauseEnumType.setValue(EnumType.missingConditionalParameter);break;
			case 2:errCauseEnumType.setValue(EnumType.responseMessageTimerExpired);break;
			case 3:errCauseEnumType.setValue(EnumType.unexpectedCommunication);break;
			case 4:errCauseEnumType.setValue(EnumType.unexpectedMessage);break;
			case 5:errCauseEnumType.setValue(EnumType.unexpectedMessageSequence);break;
			case 6:errCauseEnumType.setValue(EnumType.unexpectedParameterSequence);break;
			default:errCauseEnumType.setValue(EnumType.erroneousDataValue);break;
			}

			errCause.setValue(errCauseEnumType);
			appErrStrSeqType.setErrorCause(errCause);

			appErrStr.setValue(appErrStrSeqType);

			byte[] buf = (byte[]) AinOperationsCoding.encodeFieldsValue(appErrStr, "ApplicationErrorString");

			if(logger.isDebugEnabled()){
				logger.debug("Application Error String:"+CommonUtils.formatBytes(buf));
			}

			int i=0;

			byte [] reason = new byte[buf.length + 2];
			reason[i++] = 0x30;
			reason[i++] = (byte) buf.length;
			for(int j=0; j<buf.length; j++){
				reason[i++] = buf[j];
			}

			if(logger.isDebugEnabled()){
				logger.debug("Reson :"+CommonUtils.formatBytes(reason));
			}

			// This API contains Error code as Application Error or other error 
			byte [] appErr = {0x01};
			ErrorReqEvent errorReqEvent = new ErrorReqEvent(src, tcapSession.getDialogueId(),
					ComponentConstants.ERROR_LOCAL, appErr);

			errorReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, reason));
			errorReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

			sendComponentReq(errorReqEvent, callData, cCallTraceService);
		}catch(Exception ex){
			System.out.println("Exception in APplicationErroString: " + ex);
		}
	}

	/**
	 * This method is called by protocol handler for sending Reject request event.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param action            represents the instance of Action
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @throws Exception
	 */
	@SuppressWarnings("deprecation")
	private static void sendRejectReqEvent(TcapSession tcapSession, Action action, CallTraceService cCallTraceService)
			throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendRejectReqEvent");
		}

		RejectReqEvent rejectReqEvent = new RejectReqEvent(src);
		rejectReqEvent.setInvokeId(AinScfProtocolUtil.getLastRxInvokeId(callData));
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
	 * This method is called by protocol handler for sending UAbort request event.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @param action            represents the instance of Action
	 * @throws Exception
	 */
	static void sendUAbortRequestEvent(TcapSession tcapSession, Action action, CallTraceService cCallTraceService)
			throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: Inside sendUAbortRequestEvent");
		}

		UserAbortReqEvent uAbortReqEvent = new UserAbortReqEvent(src, tcapSession.getDialogueId());

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
			uAbortReqEvent.setAbortReason(DialogueConstants.ABORT_REASON_USER_SPECIFIC);
			break;
		}
		case AC_NEGOTIATION_FAILED: {
			infoBytes = getInformationBytes(reason);
			uAbortReqEvent.setAbortReason(DialogueConstants.ABORT_REASON_ACN_NOT_SUPPORTED);
			break;
		}
		default: {
			infoBytes = getInformationBytes(AbortInfoEnum.NO_REASON_GIVEN.getCode());
			uAbortReqEvent.setAbortReason(DialogueConstants.ABORT_REASON_USER_SPECIFIC);
			break;
		}

		}

		uAbortReqEvent.setUserAbortInformation(infoBytes);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		DialoguePortion dialoguePortion = (DialoguePortion) legData.get(LegDataAttributes.P_DIALOG_PORTION);
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
	 * @param code represents integer representation of UAbort Info
	 * @return an instance of byte[]
	 */
	private static byte[] getInformationBytes(int code) {
		byte[] uAbortInfo = new byte[] { (byte) 0x28, (byte) 0x0F, (byte) 0x06, (byte) 0x08, (byte) 0x02, (byte) 0x83,
				(byte) 0x38, (byte) 0x66, (byte) 0x03, (byte) 0x02, (byte) 0x06, (byte) 0x00, (byte) 0xA0, (byte) 0x03,
				(byte) 0x0A, (byte) 0x01, (byte) 0x01 };

		int pos = uAbortInfo.length - 1;
		uAbortInfo[pos] = (byte) code;
		return uAbortInfo;

	}

	/**
	 * This method is called by protocol handler for sending Release Call.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @param action            represents the instance of Action
	 * @throws Exception
	 */
	static void sendDisconnect(TcapSession tcapSession, Action action, CallTraceService cCallTraceService)
			throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendDisconnect");
		}

		byte[] disconnectCall = AinScfProtocolParser.createDisconnect(callData, action);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Exit createDisconnect");
		}
		byte[] rcOpCode = CommonUtils.formatIntToByte(AinOpCodes.DISCONNECT_BYTE);

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rcOpCode);

		InvokeReqEvent disconnectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rcOperation);
		disconnectInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		disconnectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, disconnectCall));
		disconnectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		disconnectInvokeReqEvent.setLastInvokeEvent(true);
		disconnectInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(disconnectInvokeReqEvent, callData, cCallTraceService);
	}

	public static void sendBnsAppError(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		String sendAcgFlag = null;
		boolean isAcgParamPresent = false;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendBnsAppError");
		}

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		int errorType = 0; // 0 - private, 1 - national 

		if(leg2Data.get(LegDataAttributes.SEND_ACG) != null){
			sendAcgFlag = (String) leg2Data.get(LegDataAttributes.SEND_ACG);
			if (sendAcgFlag != null && !sendAcgFlag.isEmpty()) {
				isAcgParamPresent = true;
				if (logger.isDebugEnabled()) {
					logger.debug("ACG send flag is set, Acg gap duration " + LegDataAttributes.ACG_GAP_DURATION + ", Acg interval " + leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
				}
			}
		}
		
		if (leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			String errCodeIden = (String) leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
			switch (Integer.parseInt(errCodeIden)) {
			case 1:
				errorType = 1;
				break;
			case 2:
				errorType = 0;
				break;
			default:
				break;
			}
		}

		byte[] encodedErrorCode = { 0x06 };
		if (leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			String errCode = (String) leg2Data.get(LegDataAttributes.P_APP_ERR_CODE);
			int errCodeInt = 6;
			try {
				errCodeInt = Integer.parseInt(errCode);
				if(errCodeInt <= 255){
					encodedErrorCode[0] = (byte) errCodeInt;
				}
			}catch(Exception ex){}
		}

		// as per standard if 7th bit is set to 1 then it Error code Type should be Private 
		// else national. Refer GR-1149, section 9.1.17
		if((encodedErrorCode[0] & 0x40) != 0x40){
			errorType = 1;
		}

		if(logger.isDebugEnabled()){
			logger.debug("sendBnsAppError: errortype:" + errorType);
		}

		ErrorReqEvent errorReqEvent = new ErrorReqEvent(src, tcapSession.getDialogueId(),
				(errorType == 0)?ComponentConstants.ERROR_LOCAL:ComponentConstants.ERROR_GLOBAL, encodedErrorCode);

		byte[] appError ={0x00};
		errorReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, appError));

		//errorReqEvent.setInvokeId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		errorReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(errorReqEvent, callData, cCallTraceService);
		if (isAcgParamPresent) {
			logger.info("sendLidbReturnError : ACG parameters are present. sending component request");
			sendAcgParameterComponentReqEvent(tcapSession, action);
		}else{
			logger.info("sendLidbReturnError : ACG parameters not present");
		}
		
		sendEndRequestEvent(tcapSession, false, cCallTraceService);

		action.setDropCallMode(DROP_CALL_MODE.BNS_APPLICATION_ERROR);

		preProcessDroppedCall(tcapSession);

		postProcessDroppedCall(tcapSession, true);
		if (logger.isDebugEnabled()) {
			logger.debug("Exit BNS Application Error");
		}
	}

	public static void sendGnAppError(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		String sendAcgFlag = null;
		boolean isAcgParamPresent = false;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendGnsAppError");
		}

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		int errorType = 0; // 0 - private, 1 - national 

		if(leg2Data.get(LegDataAttributes.SEND_ACG) != null){
			sendAcgFlag = (String) leg2Data.get(LegDataAttributes.SEND_ACG);
			if (sendAcgFlag != null && !sendAcgFlag.isEmpty()) {
				isAcgParamPresent = true;
				if (logger.isDebugEnabled()) {
					logger.debug("ACG send flag is set, Acg gap duration " + LegDataAttributes.ACG_GAP_DURATION + ", Acg interval " + leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
				}
			}
		}

		if (leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			String errCodeIden = (String) leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
			switch (Integer.parseInt(errCodeIden)) {
			case 1:
				errorType = 1;
				break;
			case 2:
				errorType = 0;
				break;
			default:
				break;
			}
		}

		byte[] encodedErrorCode = { 0x06 };
		if (leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			String errCode = (String) leg2Data.get(LegDataAttributes.P_APP_ERR_CODE);
			int errCodeInt = 6;
			try {
				errCodeInt = Integer.parseInt(errCode);
				if(errCodeInt <= 255){
					encodedErrorCode[0] = (byte) errCodeInt;
				}
			}catch(Exception ex){}
		}

		// as per standard if 7th bit is set to 1 then it Error code Type should be Private 
		// else national. Refer GR-1149, section 9.1.17
		if((encodedErrorCode[0] & 0x40) != 0x40){
			errorType = 1;
		}

		ErrorReqEvent errorReqEvent = new ErrorReqEvent(src, tcapSession.getDialogueId(),
				(errorType == 0)?ComponentConstants.ERROR_LOCAL:ComponentConstants.ERROR_GLOBAL, encodedErrorCode);

		byte[] appError ={0x00};
		errorReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, appError));

		//errorReqEvent.setInvokeId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		errorReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		action.setDropCallMode(DROP_CALL_MODE.GN_APPLICATION_ERROR);

		preProcessDroppedCall(tcapSession);

		sendComponentReq(errorReqEvent, callData, cCallTraceService);

		if (isAcgParamPresent) {
			logger.info("sendGnsAppError : ACG parameters are present. sending component request");
			sendAcgParameterComponentReqEvent(tcapSession, action);
		}else{
			logger.info("sendGnsAppError : ACG parameters not present");
		}
		
		sendEndRequestEvent(tcapSession, false, cCallTraceService);
		postProcessDroppedCall(tcapSession, true);
		if (logger.isDebugEnabled()) {
			logger.debug("Exit GN Application Error");
		}
	}

	static void sendBnsProtocolError(TcapSession tcapSession, Action action, CallTraceService cCallTraceService)
			throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendBnsProtocolError");
		}
		byte[] opCode = CommonUtils.formatIntToByte(AinOpCodes.LIDB_QUERY_BYTE);
		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, opCode);
		RejectReqEvent rejectReqEvent = new RejectReqEvent(src, tcapSession.getDialogueId(), 1, 1);
		//rejectReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		rejectReqEvent.setRejectType(1);
		rejectReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, opCode));
		rejectReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		preProcessDroppedCall(tcapSession);
		sendComponentReq(rejectReqEvent, callData, PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());
		sendEndRequestEvent(tcapSession, false, cCallTraceService);
		postProcessDroppedCall(tcapSession, true);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Exit sendBnsProtocolError");
		}
	}

	// static void sendGnProtocolError(TcapSession tcapSession, Action action,
	// CallTraceService cCallTraceService)
	// throws Exception {
	// CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
	// int dialogId = tcapSession.getDialogueId();
	// if (logger.isDebugEnabled()) {
	// logger.debug(dialogId + " [PH]:: Inside sendGnProtocolError");
	// }
	// byte[] opCode = CommonUtils.formatIntToByte(AinOpCodes.LIDB_QUERY_BYTE);
	//
	// Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, opCode);
	// RejectReqEvent rejectReqEvent = new RejectReqEvent(src,
	// tcapSession.getDialogueId(), 1, 1);
	// rejectReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
	// rejectReqEvent.setRejectType(1);
	// rejectReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET,
	// opCode));
	// rejectReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));
	// preProcessDroppedCall(tcapSession);
	// sendComponentReq(rejectReqEvent, callData, PhUtilityServices
	// .getInstance((String)
	// callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());
	// sendEndRequestEvent(tcapSession, false, cCallTraceService);
	// postProcessDroppedCall(tcapSession, true);
	// if (logger.isDebugEnabled()) {
	// logger.debug(dialogId + " [PH]:: Exit sendGnProtocolError");
	// }
	// }

	static void sendOlnsAppError(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendOlnsAppError");
		}

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		byte[] appError = OLNSQuery.getApplicationErrorForOlnsQuery(callData);
		preProcessDroppedCall(tcapSession);
		action.setDropCallMode(DROP_CALL_MODE.OLNS_APPLICATION_ERROR);

		preProcessDroppedCall(tcapSession);
		ErrorReqEvent errorReqEvent = new ErrorReqEvent(src, tcapSession.getDialogueId(),
				ComponentConstants.ERROR_LOCAL, appError);

		errorReqEvent.setInvokeId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(errorReqEvent, callData, cCallTraceService);
		sendEndRequestEvent(tcapSession, false, cCallTraceService);
		postProcessDroppedCall(tcapSession, true);
		if (logger.isDebugEnabled()) {
			logger.debug("Exit:sendOlnsAppError");
		}
	}

	public static void sendCc1AppError(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		String sendAcgFlag = null;
		boolean isAcgParamPresent = false;
		
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendCc1AppError");
		}

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		int errorType = 0; // 0 - private, 1 - national 

		if(leg2Data.get(LegDataAttributes.SEND_ACG) != null){
			sendAcgFlag = (String) leg2Data.get(LegDataAttributes.SEND_ACG);
			if (sendAcgFlag != null && !sendAcgFlag.isEmpty()) {
				isAcgParamPresent = true;
				if (logger.isDebugEnabled()) {
					logger.debug("ACG send flag is set, Acg gap duration " + LegDataAttributes.ACG_GAP_DURATION + ", Acg interval " + leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
				}
			}
		}

		if (leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			String errCodeIden = (String) leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
			switch (Integer.parseInt(errCodeIden)) {
			case 1:
				errorType = 1;
				break;
			case 2:
				errorType = 0;
				break;
			default:
				break;
			}
		}

		byte[] encodedErrorCode = { 0x06 };
		if (leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			String errCode = (String) leg2Data.get(LegDataAttributes.P_APP_ERR_CODE);
			int errCodeInt = 6;
			try {
				errCodeInt = Integer.parseInt(errCode);
				if(errCodeInt <= 255){
					encodedErrorCode[0] = (byte) errCodeInt;
				}
			}catch(Exception ex){}
		}

		// as per standard if 7th bit is set to 1 then it Error code Type should be Private 
		// else national. Refer GR-1149, section 9.1.17
		if((encodedErrorCode[0] & 0x40) != 0x40){
			errorType = 1;
		}

		if(logger.isDebugEnabled()){
			logger.debug("sendCc1AppError: errortype:" + errorType);
		}

		ErrorReqEvent errorReqEvent = new ErrorReqEvent(src, tcapSession.getDialogueId(),
				(errorType == 0)?ComponentConstants.ERROR_LOCAL:ComponentConstants.ERROR_GLOBAL, encodedErrorCode);

		
		//PIN will go with RETURN ERROR if pin mismatch occurs
		byte[] pin = null;
		if (leg2Data.get(LegDataAttributes.P_CC_PIN) != null) {
			logger.info("sendCc1AppError:parameter pin is present");
			String pinNumber = (String) leg2Data.get(LegDataAttributes.P_CC_PIN);
			pin = AinScfProtocolFieldCodec.encodeDigitsInBcdFormat(pinNumber);
			if (pin.length != 2) {
				logger.info("sendCc1AppError : pin length should be 2");
				try {
					throw new AINCodecException("pin length should be 2");
				} catch (AINCodecException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		
		byte[] appError = {0x00};
		if(pin != null) {
			int index = 0;
			appError = new byte[7];
			appError[index++] = (byte) 0x00;
			appError[index++] = (byte) 0x00;
			appError[index++] = (byte) 0xdf;
			appError[index++] = (byte) 0x60;
			appError[index++] = (byte) 0x02;
			for (byte encodedVal : pin) {
				appError[index++] = encodedVal;
			}
		} else {
		appError[0] = (byte) 0x00;
		}
		errorReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, appError));

		//errorReqEvent.setInvokeId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		errorReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(errorReqEvent, callData, cCallTraceService);

		if (isAcgParamPresent) {
			logger.info("sendCc1AppError : ACG parameters are present. sending component request");
			sendAcgParameterComponentReqEvent(tcapSession, action);
		}else{
			logger.info("sendCc1AppError : ACG parameters not present");
		}		
		sendEndRequestEvent(tcapSession, false, cCallTraceService);

		action.setDropCallMode(DROP_CALL_MODE.BNS_APPLICATION_ERROR);

		preProcessDroppedCall(tcapSession);

		postProcessDroppedCall(tcapSession, true);
		if (logger.isDebugEnabled()) {
			logger.debug("Exit CC1 Application Error");
		}
	}
	
	
    public static void sendApplicationErrorWithProblemData(TcapSession tcapSession, Action action) throws Exception {

        CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
        int dialogId = tcapSession.getDialogueId();
        LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

        if (logger.isDebugEnabled()) {
                logger.debug(dialogId + " [PH]:: Inside sendApplicationErrorWithProblemData");
        }

        CallTraceService cCallTraceService = PhUtilityServices
                        .getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

        int errorType = 0; // 0 - private, 1 - national

        if (legData.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
                String errCodeIden = (String) legData.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
                switch (Integer.parseInt(errCodeIden)) {
                case 1:
                        errorType = 1;
                        break;
                case 2:
                        errorType = 0;
                        break;
                default:
                        break;
                }
        }

        byte[] encodedErrorCode = { 0x06 };
        if (legData.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
                String errCode = (String) legData.get(LegDataAttributes.P_APP_ERR_CODE);
                int errCodeInt = 6;
                try {
                        errCodeInt = Integer.parseInt(errCode);
                        if(errCodeInt <= 255){
                                encodedErrorCode[0] = (byte) errCodeInt;
                        }
                }catch(Exception ex){}
        }

        // as per standard if 7th bit is set to 1 then it Error code Type should be Private
        // else national. Refer GR-1149, section 9.1.17
        if((encodedErrorCode[0] & 0x40) != 0x40){
                errorType = 1;
        }

        if(logger.isDebugEnabled()){
                logger.debug("sendApplicationErrorWithProblemData: errortype:" + errorType);
        }

        ErrorReqEvent errorReqEvent = new ErrorReqEvent(src, tcapSession.getDialogueId(),
                        (errorType == 0)?ComponentConstants.ERROR_LOCAL:ComponentConstants.ERROR_GLOBAL, encodedErrorCode);

        //Problem data will go with RETURN ERROR if problem data is not null
        //byte[] problemData = TlnsConstants.problemDataBytes;
        byte[] problemData = null;

        if (legData.get(LegDataAttributes.P_APP_ERR_PROBLEM_DATA) != null) {
                logger.info("sendApplicationErrorWithProblemData:problem data is present");
                byte[] prblmData = (byte[]) legData.get(LegDataAttributes.P_APP_ERR_PROBLEM_DATA);
                int index = 0;
                logger.info("sendApplicationErrorWithProblemData: problem Data length " + prblmData.length);
                problemData = new byte[prblmData.length + 2];
                problemData[index++] = (byte) 0x00;
                problemData[index++] = (byte) 0x00;
                for (byte encodedVal : prblmData) {
                        logger.info("sendApplicationErrorWithProblemData:Setting problem data index " + index + "- " + encodedVal);
                        problemData[index++] = encodedVal;
                }
        }
        else {
                problemData[0] = (byte) 0x00;
        }
        if(logger.isDebugEnabled()){
                logger.debug("sendApplicationErrorWithProblemData: problemData:" + problemData);
        }

        errorReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, problemData));

        //errorReqEvent.setInvokeId(AinScfProtocolUtil.getLastRxInvokeId(callData));
        errorReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

        sendComponentReq(errorReqEvent, callData, cCallTraceService);
        sendEndRequestEvent(tcapSession, false, cCallTraceService);

        action.setDropCallMode(DROP_CALL_MODE.BNS_APPLICATION_ERROR);

        preProcessDroppedCall(tcapSession);

        postProcessDroppedCall(tcapSession, true);
        if (logger.isDebugEnabled()) {
                logger.debug("Exit Application Error with Problem data");
        }
}

	// static void sendOlnsProtocolError(TcapSession tcapSession, Action action,
	// CallTraceService cCallTraceService)
	// throws Exception {
	// CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
	// int dialogId = tcapSession.getDialogueId();
	// if (logger.isDebugEnabled()) {
	// logger.debug(dialogId + " [PH]:: Inside sendOlnsProtocolError");
	// }
	// byte[] opCode = CommonUtils.formatIntToByte(AinOpCodes.LIDB_QUERY_BYTE);
	//
	// Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, opCode);
	// RejectReqEvent rejectReqEvent = new RejectReqEvent(src,
	// tcapSession.getDialogueId(), 1, 1);
	// rejectReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
	// rejectReqEvent.setRejectType(1);
	// rejectReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET,
	// opCode));
	// rejectReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));
	// preProcessDroppedCall(tcapSession);
	// sendComponentReq(rejectReqEvent, callData, PhUtilityServices
	// .getInstance((String)
	// callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());
	// sendEndRequestEvent(tcapSession, false, cCallTraceService);
	// postProcessDroppedCall(tcapSession, true);
	// if (logger.isDebugEnabled()) {
	// logger.debug(dialogId + " [PH]:: Exit sendOlnsProtocolError");
	// }
	// }

	/**
	 * This method is called by protocol handler for sending Entity release event.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @param action            represents the instance of Action
	 * @throws Exception
	 */

	/**
	 * This method is called by protocol handler for sending END request event.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param preArrangedEnd    represents the instance of boolean
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @throws Exception
	 */
	public static void sendEndRequestEvent(TcapSession tcapSession, boolean preArrangedEnd, CallTraceService cCallTraceService)
			throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendEndRequestEvent");
		}

		EndReqEvent endReqEvent = new EndReqEvent(src, tcapSession.getDialogueId());

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

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		DialoguePortion dialoguePortion = (DialoguePortion) legData.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Set Dialogue portion");
			}
			endReqEvent.setDialoguePortion(dialoguePortion);
		}
	

        LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
       
    	if (leg2Data != null) {
    		
        String mtp3=(String)leg2Data.get(LegDataAttributes.MTP3_DPC);
		
		SignalingPointCode mtp3dpc=AinScfProtocolUtil.createSignallingPointCode(mtp3);
		if(mtp3dpc!=null) {
			logger.debug("Setting Mtp3 DPC with value :- "+ mtp3dpc);
			endReqEvent.setMtp3Dpc(mtp3dpc);
		}
		
		String mtp3op=(String)leg2Data.get(LegDataAttributes.MTP3_OPC);
		SignalingPointCode mtp3opc =AinScfProtocolUtil.createSignallingPointCode(mtp3op);
		if(mtp3opc!=null) {
			logger.debug("Setting Mtp3 OPC with value :- "+ mtp3opc);

			endReqEvent.setMtp3Opc(mtp3opc);
		}
		
		String CD_PTY_NO_PC =(String) leg2Data.get(LegDataAttributes.CD_PTY_NO_PC);
		String CG_PTY_NO_PC = (String) leg2Data.get(LegDataAttributes.CG_PTY_NO_PC);
	
		
		SccpUserAddress origAddr=AinScfProtocolUtil.updateCallingAddress(callData, leg2Data);
		endReqEvent.setOriginatingAddress(origAddr);
		
		SccpUserAddress destAddr=AinScfProtocolUtil.updateCalledAddress(callData, leg2Data);
		if(StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"1")) {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to true in in orig addr endReqEvent");
			}
			destAddr.setPtyNoPC(true);
		}else {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to false in orig addr endReqEvent");
			}
			destAddr.setPtyNoPC(false);
		}
		if(StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"1")) {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to true in dest addr endReqEvent");
			}
			origAddr.setPtyNoPC(true);
		}else {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to false in dest addr endReqEvent");
			}
			origAddr.setPtyNoPC(false);
		}
		endReqEvent.setDestinationAddress(destAddr);
		
			String returnOption= (String) leg2Data.get(LegDataAttributes.P_RETURN_OPTION);
		    
		    if(returnOption != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: return Option is "+ returnOption);
		    	}
		    	endReqEvent.setReturnOption(returnOption);
		    }
		    
		    String sequenceControl= (String) leg2Data.get(LegDataAttributes.P_SEQUENCE_CONTROL);
		    
		    if(sequenceControl != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: sequenceControl is "+ sequenceControl);
		    	}
		    	endReqEvent.setSequenceControl(sequenceControl);
		    }
		    
		    String messagePriority= (String) leg2Data.get(LegDataAttributes.P_MESSAGE_PRIORITY);
		    
		    if(messagePriority != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: messagePriority is "+ messagePriority);
		    	}
		    	endReqEvent.setMessagePriority(messagePriority);
		    }
		}
		
		sendDialogueReq(endReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendEndRequestEvent");
		}

		AinScfProtocolUtil.setLastInvokeIdStartRange(AinScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1,
				callData);
		AinScfProtocolUtil.setLastInvokeIdEndRange(AinScfProtocolUtil.getLastInvokeId(callData), callData);
	}
	
	/**
	 * This method is called by protocol handler for sending END request event.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param preArrangedEnd    represents the instance of boolean
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @throws Exception
	 */
	public static void sendEndRequestEvent(TcapSession tcapSession, boolean preArrangedEnd, CallTraceService cCallTraceService,SccpUserAddress origAddr,SccpUserAddress destAddr)
			throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendEndRequestEvent with same orig and dest");
		}

		EndReqEvent endReqEvent = new EndReqEvent(src, tcapSession.getDialogueId(),destAddr,origAddr);

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

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		DialoguePortion dialoguePortion = (DialoguePortion) legData.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Set Dialogue portion");
			}
			endReqEvent.setDialoguePortion(dialoguePortion);
		}
		
		String mtp3 = (String) leg2Data.get(LegDataAttributes.MTP3_DPC);

		SignalingPointCode mtp3dpc = AinScfProtocolUtil
				.createSignallingPointCode(mtp3);
		if (mtp3dpc != null) {
			logger.debug("Setting Mtp3 DPC with value :- " + mtp3dpc);
			endReqEvent.setMtp3Dpc(mtp3dpc);
		}
		
		String mtp3op=(String)leg2Data.get(LegDataAttributes.MTP3_OPC);
		SignalingPointCode mtp3opc =AinScfProtocolUtil.createSignallingPointCode(mtp3op);
		if(mtp3opc!=null) {
			logger.debug("Setting Mtp3 OPC with value :- "+ mtp3opc);

			endReqEvent.setMtp3Opc(mtp3opc);
		}

		if (leg2Data != null) {
			String returnOption= (String) leg2Data.get(LegDataAttributes.P_RETURN_OPTION);
		    
		    if(returnOption != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: return Option is "+ returnOption);
		    	}
		    	endReqEvent.setReturnOption(returnOption);
		    }
		    
		    String sequenceControl= (String) leg2Data.get(LegDataAttributes.P_SEQUENCE_CONTROL);
		    
		    if(sequenceControl != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: sequenceControl is "+ sequenceControl);
		    	}
		    	endReqEvent.setSequenceControl(sequenceControl);
		    }
		    
		    String messagePriority= (String) leg2Data.get(LegDataAttributes.P_MESSAGE_PRIORITY);
		    
		    if(messagePriority != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: messagePriority is "+ messagePriority);
		    	}
		    	endReqEvent.setMessagePriority(messagePriority);
		    }
		}

		sendDialogueReq(endReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendEndRequestEvent");
		}

		AinScfProtocolUtil.setLastInvokeIdStartRange(AinScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1,
				callData);
		AinScfProtocolUtil.setLastInvokeIdEndRange(AinScfProtocolUtil.getLastInvokeId(callData), callData);
	}

	/**
	 * This method is called by protocol handler for sending continue request event.
	 * 
	 * @param tcapSession       represents the instance of TcapSession
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendContinueRequestEvent(TcapSession tcapSession, CallTraceService cCallTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendContinueRequestEvent");
		}

		ContinueReqEvent continueReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
//<<<<<<< HEAD
////		SccpUserAddress sua = (SccpUserAddress) legData.get(LegDataAttributes.P_SUA);
////
////		continueReqEvent.setOriginatingAddress(sua);
//		
//		SccpUserAddress mtp3Dpc= (SccpUserAddress)leg2Data.get(LegDataAttributes.MTP3_DPC);
//		
//		SccpUserAddress origAddr=AinScfProtocolUtil.updateCallingAddress(callData, leg2Data);
//		continueReqEvent.setOriginatingAddress(origAddr);

		
		SccpUserAddress origAddr=AinScfProtocolUtil.updateCallingAddress(callData, leg2Data);
		continueReqEvent.setOriginatingAddress(origAddr);
		
	if (leg2Data != null) {
			
			String CD_PTY_NO_PC =(String) leg2Data.get(LegDataAttributes.CD_PTY_NO_PC);
			String CG_PTY_NO_PC = (String) leg2Data.get(LegDataAttributes.CG_PTY_NO_PC);
		
		SccpUserAddress destAddr=AinScfProtocolUtil.updateCalledAddress(callData, leg2Data);
		if(StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"1")) {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to true in in orig addr continueReq");
			}
			destAddr.setPtyNoPC(true);
		}else {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to false in orig addr continueReq");
			}
			destAddr.setPtyNoPC(false);
		}
		if(StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"1")) {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to true in dest addr continueReq");
			}
			origAddr.setPtyNoPC(true);
		}else {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to false in dest addr continueReq");
			}
			origAddr.setPtyNoPC(false);
		}
		continueReqEvent.setDestinationAddress(destAddr);
	}
		DialoguePortion dialoguePortion = (DialoguePortion) legData.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			continueReqEvent.setDialoguePortion(dialoguePortion);
		}
		     String mtp3=(String)leg2Data.get(LegDataAttributes.MTP3_DPC);
			
			SignalingPointCode mtp3dpc=AinScfProtocolUtil.createSignallingPointCode(mtp3);
			if(mtp3dpc!=null) {
				logger.debug("Setting Mtp3 DPC with value :- "+ mtp3dpc);
				continueReqEvent.setMtp3Dpc(mtp3dpc);
			}
			
			String mtp3op=(String)leg2Data.get(LegDataAttributes.MTP3_OPC);
			SignalingPointCode mtp3opc =AinScfProtocolUtil.createSignallingPointCode(mtp3op);
			if(mtp3opc!=null) {
				logger.debug("Setting Mtp3 OPC with value :- "+ mtp3opc);

				continueReqEvent.setMtp3Opc(mtp3opc);
			}


		// whether to send Converation with permission or without permission in ANSI
		// is decided based on flag. True for converation with permission.
		continueReqEvent.setAllowedPermission(true);

		if (leg2Data != null) {
			String returnOption= (String) leg2Data.get(LegDataAttributes.P_RETURN_OPTION);
		    
		    if(returnOption != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: return Option is "+ returnOption);
		    	}
		    	continueReqEvent.setReturnOption(returnOption);
		    }
		    
		    String sequenceControl= (String) leg2Data.get(LegDataAttributes.P_SEQUENCE_CONTROL);
		    
		    if(sequenceControl != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: sequenceControl is "+ sequenceControl);
		    	}
		    	continueReqEvent.setSequenceControl(sequenceControl);
		    }
		    
		    String messagePriority= (String) leg2Data.get(LegDataAttributes.P_MESSAGE_PRIORITY);
		    
		    if(messagePriority != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug(dialogId + "[PH]:: messagePriority is "+ messagePriority);
		    	}
		    	continueReqEvent.setMessagePriority(messagePriority);
		    }
		}
		sendDialogueReq(continueReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: leave sendContinueRequestEvent");
		}

		AinScfProtocolUtil.setLastInvokeIdStartRange(AinScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1,
				callData);
		AinScfProtocolUtil.setLastInvokeIdEndRange(AinScfProtocolUtil.getLastInvokeId(callData), callData);
	}

	/**
	 * This method is called by protocol handler for sending AIN component
	 * indication event.
	 * 
	 * @param cre               represents the instance of ComponentReqEvent
	 * @param callData          represents the instance of CallData
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @throws Exception
	 */
	public static void sendComponentReq(ComponentReqEvent cre, CallData callData, CallTraceService cCallTraceService)
			throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendComponentReq");
		}

		PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider()
		.sendComponentReqEvent(cre);

		traceComponent(cre, callData);
	}

	/**
	 * This method will trace outgoing AIN component; We catch inside try catch
	 * throwable to avoid any impact on call in case of error;
	 * 
	 * @param cre               represents the instance of ComponentReqEvent
	 * @param callData          represents the instance of CallData
	 * @param cCallTraceService represents the instance of CallTraceService
	 */

	static void traceComponent(ComponentReqEvent cre, CallData callData) {
		try {
			CallTraceService cCallTraceService = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			String isTraceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);
			
        LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			
			String caller = null;
			String called = null;
			if (legData != null) {
				PhoneNumber callingP = (PhoneNumber) legData
						.get(LegDataAttributes.P_CALLING_PARTY);
				if (callingP != null) {
					caller = callingP.getAddress();
				}
				PhoneNumber calledP = (PhoneNumber) legData
						.get(LegDataAttributes.P_CALLED_PARTY);
				if (calledP != null) {
					called = calledP.getAddress();
				}
			}

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

					byte[] operCode = ((InvokeReqEvent) cre).getOperation().getOperationCode();

					int operCodeByte = CommonUtils.formatBytesToInt(operCode);
					traceMsg.append("MESSAGE::");
					switch (operCodeByte) {
					case AinOpCodes.INFO_ANALYZED_BYTE: {
						traceMsg.append("INFO_ANLYZE");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.INFO_COLLECTED_BYTE: {
						traceMsg.append("INFO COLLECT");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.REQUEST_REPORT_BCSM_EVENT_BYTE: {
						traceMsg.append("REQUEST_REPORT_BCSM_EVENT");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.NETWORK_BUSY_BYTE: {
						traceMsg.append("NTWRK BUSY");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.SEND_TO_RESOURCE_BYTE: {
						traceMsg.append("STR");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.RESOURCE_CLEAR_BYTE: {
						traceMsg.append("RES CLEAR");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.DISCONNECT_BYTE: {
						traceMsg.append("DISCONNECT BYTE");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.ACTIVITY_TEST_BYTE: {
						traceMsg.append("AT");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_ANSWER_BYTE: {
						traceMsg.append("O_ANS");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_DISCONNECT_BYTE: {
						traceMsg.append("O_DISCONNECT");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_CALLED_PARTY_BUSY_BYTE: {
						traceMsg.append("O_CALLED_PARTY_BUSY");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_NO_ANSWER_BYTE: {
						traceMsg.append("O_NO_ANSWER");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_TERM_SIZED_BYTE: {
						traceMsg.append("O_TERM_SIZED");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.CLOSE_BYTE: {
						traceMsg.append("CLOSE");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.ANALYZE_ROUTE_BYTE: {
						traceMsg.append("ANALYZE_ROUTE");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.TERMINATION_ATTEMPT_BYTE: {
						traceMsg.append("TERMINATION_ATTEMPT");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.AUTHORIZED_TERMINATION_BYTE: {
						traceMsg.append("AUTHORIZED_TERMINATION");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.FORWARD_CALL_BYTE: {
						traceMsg.append("FORWARD_CALL");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.ACG_BYTE: {
						traceMsg.append("ACG");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.CONNECTION_CONTROL_BYTE: {
						traceMsg.append("CONNECTION_CONTROL");
						traceMsg.append("\n");
						break;
					}
					default: {
						traceMsg.append("OPCODE BYTE:" + operCodeByte);
						traceMsg.append("\n");
						break;
					}
					}

					Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((InvokeReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((InvokeReqEvent) cre).getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(CommonUtils.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				} // @end switch message type
				case TcapConstants.PRIMITIVE_RESULT: {
					traceMsg.append("RESULT");
					traceMsg.append("\n");

					Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ResultReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((ResultReqEvent) cre).getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(CommonUtils.formatBytes(parms));
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
					traceMsg.append(CommonUtils.formatBytes(((ErrorReqEvent) cre).getErrorCode()));
					traceMsg.append("\n");

					Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ErrorReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((ErrorReqEvent) cre).getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(CommonUtils.formatBytes(parms));
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

					Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((RejectReqEvent) cre).isParametersPresent()) {
							byte[] parms = ((RejectReqEvent) cre).getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(CommonUtils.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
				}
				}// @switch primitive

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(),
								AinScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(),
								callState,caller,called);
					}
				}

			} // end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing component req event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error in traceComponent", t);
			}
		}
	}

	/**
	 * This method will trace incoming AIN component; inside try catch throwable to
	 * avoid any impact on call in case of error;
	 * 
	 * @param cie               represents the instance of ComponentIndEvent
	 * @param callData          represents the instance of CallData
	 * @param cCallTraceService represents the instance of CallTraceService
	 */
	static void traceComponent(ComponentIndEvent cie, CallData callData, boolean isBegin) {
		try {
			CallTraceService cCallTraceService = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			String isTraceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);

			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			
			String caller = null;
			String called = null;
			if (legData != null) {
				PhoneNumber callingP = (PhoneNumber) legData
						.get(LegDataAttributes.P_CALLING_PARTY);
				if (callingP != null) {
					caller = callingP.getAddress();
				}
				PhoneNumber calledP = (PhoneNumber) legData
						.get(LegDataAttributes.P_CALLED_PARTY);
				if (calledP != null) {
					called = calledP.getAddress();
				}
			}
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

					byte[] operCode = ((InvokeIndEvent) cie).getOperation().getOperationCode();
					int operCodeByte = CommonUtils.formatBytesToInt(operCode);

					traceMsg.append("MESSAGE::");
					switch (operCodeByte) {
					case AinOpCodes.INFO_ANALYZED_BYTE: {
						traceMsg.append("INFO_ANLYZE");
						traceMsg.append("\n");
						traceMsg.append(dumpTraceMsgForInitialMsg(callData)).append("\n\n");
						break;
					}
					case AinOpCodes.INFO_COLLECTED_BYTE: {
						traceMsg.append("INFO COLLECT");
						traceMsg.append("\n");
						traceMsg.append(dumpTraceMsgForInitialMsg(callData)).append("\n\n");
						break;
					}
					case AinOpCodes.RRBE_BYTE: {
						traceMsg.append("RRBE");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.NETWORK_BUSY_BYTE: {
						traceMsg.append("NTWRK BUSY");
						traceMsg.append("\n");
						if(isBegin){
							traceMsg.append(dumpTraceMsgForInitialMsg(callData)).append("\n\n");
						}
						break;
					}
					case AinOpCodes.SEND_TO_RESOURCE_BYTE: {
						traceMsg.append("STR");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.RESOURCE_CLEAR_BYTE: {
						traceMsg.append("RES CLEAR");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.DISCONNECT_BYTE: {
						traceMsg.append("DISCONNECT BYTE");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.ACTIVITY_TEST_BYTE: {
						traceMsg.append("AT");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_ANSWER_BYTE: {
						traceMsg.append("O_ANS");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_DISCONNECT_BYTE: {
						traceMsg.append("O_DISCONNECT");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_CALLED_PARTY_BUSY_BYTE: {
						traceMsg.append("O_CALLED_PARTY_BUSY");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_NO_ANSWER_BYTE: {
						traceMsg.append("O_NO_ANSWER");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.O_TERM_SIZED_BYTE: {
						traceMsg.append("O_TERM_SIZED");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.CLOSE_BYTE: {
						traceMsg.append("CLOSE");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.ANALYZE_ROUTE_BYTE: {
						traceMsg.append("ANALYZE_ROUTE");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.TERMINATION_ATTEMPT_BYTE: {
						traceMsg.append("TERMINATION_ATTEMPT");
						traceMsg.append("\n");
						traceMsg.append(dumpTraceMsgForInitialMsg(callData)).append("\n\n");
						break;
					}
					case AinOpCodes.TERMINATION_NOTIFICATION_BYTE: {
						traceMsg.append("TERMINATION_NOTIFICATION");
						traceMsg.append("\n");
						break;
					}
					case AinOpCodes.ORIGINATION_ATTEMPT_BYTE: {
						traceMsg.append("ORIGINATION_ATTEMPT");
						traceMsg.append("\n");
						traceMsg.append(dumpTraceMsgForInitialMsg(callData)).append("\n\n");
						break;
					}
					case AinOpCodes.LIDB_QUERY_BYTE: {
						if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null) {
							if (MESSAGE.BNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
								traceMsg.append("LIDB_QUERY: BNS");
							} else if (MESSAGE.GN_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
								traceMsg.append("LIDB_QUERY: GN");
							} else {
								traceMsg.append("LIDB_QUERY");
							}
						}
						traceMsg.append("\n");
						break;
					}
					default: {
						traceMsg.append("OPCODE BYTE:" + operCodeByte);
						traceMsg.append("\n");
						break;
					}
					}

					Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {

						if (((InvokeIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((InvokeIndEvent) cie).getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(CommonUtils.formatBytes(parms));
							traceMsg.append("\n");
						}
					}

					break;
				} // @end switch message type
				case TcapConstants.PRIMITIVE_RESULT: {
					traceMsg.append("RESULT");
					traceMsg.append("\n");

					Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ResultIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((ResultIndEvent) cie).getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(CommonUtils.formatBytes(parms));
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
					traceMsg.append(CommonUtils.formatBytes(((ErrorIndEvent) cie).getErrorCode()));
					traceMsg.append("\n");

					Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((ErrorIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((ErrorIndEvent) cie).getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(CommonUtils.formatBytes(parms));
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

					Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);

					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
						if (((RejectIndEvent) cie).isParametersPresent()) {
							byte[] parms = ((RejectIndEvent) cie).getParameters().getParameter();
							traceMsg.append("BUFFER::");
							traceMsg.append(CommonUtils.formatBytes(parms));
							traceMsg.append("\n");
						}
					}
				}
				}// @switch primitive

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {

					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(),
								AinScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(),
								callState,caller,called);
					}
				}

			} // end if tracing required
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
	 * @param dre               represents the instance of DialogueReqEvent
	 * @param callData          represents the instance of CallData
	 * @param cCallTraceService represents the instance of CallTraceService
	 * @throws Exception
	 */
	static void sendDialogueReq(DialogueReqEvent dre, CallData callData, CallTraceService cCallTraceService)
			throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: Inside sendComponentReq");
		}

		incrementTcapCounters(dre);
		PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider()
		.sendDialogueReqEvent(dre);

		traceDialog(dre, callData, cCallTraceService);

	}

	/**
	 * This method will trace outgoing AIN Dialogue; From try block we are catching
	 * throwable to avoid any impact on call in case of error;
	 * 
	 * @param dre               represents the instance of DialogueReqEvent
	 * @param callData          represents the instance of CallData
	 * @param cCallTraceService represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueReqEvent dre, CallData callData, CallTraceService cCallTraceService) {
		try {
			String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);
			if (PhConstants.TRUE.equals(traceFlag)) {
				int primitive = dre.getPrimitiveType();

				StringBuilder traceMsg = new StringBuilder();

				int callState = CallTraceService.CALL_IN_PROGRESS;
				
				String caller = null;
				String called = null;
				
				 LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
				if (legData != null) {
					PhoneNumber callingP = (PhoneNumber) legData
							.get(LegDataAttributes.P_CALLING_PARTY);
					if (callingP != null) {
						caller = callingP.getAddress();
					}
					PhoneNumber calledP = (PhoneNumber) legData
							.get(LegDataAttributes.P_CALLED_PARTY);
					if (calledP != null) {
						called = calledP.getAddress();
					}
				}

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
					traceMsg.append(CommonUtils.formatBytes(((UserAbortReqEvent) dre).getUserAbortInformation()));
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

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(),
								AinScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(),
								callState,caller,called);
					}
				}

			} // end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing Dialog req event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing dialogue event", t);
			}
		}

	}

	/**
	 * This method will trace incoming AIN Dialogue; From try block we are catching
	 * throwable to avoid any impact on call in case of error;
	 * 
	 * @param die               represents the instance of DialogueIndEvent
	 * @param callData          represents the instance of CallData
	 * @param cCallTraceService represents the instance of CallTraceService
	 */
	static void traceDialog(DialogueIndEvent die, CallData callData, CallTraceService cCallTraceService) {

		try {

			String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);
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
					traceMsg.append(CommonUtils.formatBytes(((UserAbortIndEvent) die).getUserAbortInformation()));
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

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(),
								AinScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(),
								callState);
					}
				}

			} // end if tracing required
		} catch (Throwable t) {
			logger.warn("Error tracing Dialog ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing dialogue ind event", t);
			}
		}

	}

	/**
	 * This method will trace incoming BEGIN Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error; invoked from
	 * trace component ind for IDP
	 * 
	 * @param callData          represents an instance of CallData
	 * @param cCallTraceService represents an instance of CallData CallTraceService
	 */
	private static void traceBegin(CallData callData, CallTraceService cCallTraceService) {
		try {
			StringBuilder traceMsg = new StringBuilder();
			int callState = CallTraceService.CALL_IN_PROGRESS;
			traceMsg.append("<<<<Recieved<<<<");
			traceMsg.append("\n");

			traceMsg.append("DIALOGUE::");

			traceMsg.append("BEGIN");
			traceMsg.append("\n");

			if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
				List<Integer> constraintList = (List<Integer>) callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
				Iterator<Integer> constraintIterator = constraintList.iterator();
				while (constraintIterator.hasNext()) {
					cCallTraceService.trace(constraintIterator.next(),
							AinScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(),
							callState);
				}
			}

		} catch (Throwable t) {
			logger.warn("Error tracing Dialog ind event BEGINS" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("Error tracing begin event", t);
			}
		}

	}

	/**
	 * This method is called on receiving ACTION_CONNECT from applicaiton
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void connectTerm(TcapSession tcapSession, CallData callData, Action action) throws Exception {

		Integer dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		Boolean dialoutCheck = (Boolean) callData.get(CallDataAttribute.P_DIALOUT);
		if(dialoutCheck == null){
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: [PH] Setting dialout Check as False for Value null");
			}
			dialoutCheck = false;
		}

		AinCallStates ainCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		if (action.getConnectionMode() == Action.CONNECTIONMODE.EQSROUTING && dialoutCheck) {
			if(logger.isDebugEnabled()){
				logger.debug(dialogId+":: Condition match for PSX Route");
			}
			sendSipInviteFrmAIN(tcapSession, callData,action);
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside connectTerm with tcapSession");
			logger.debug(dialogId + " :: AinCallState is " + legData.get(LegDataAttributes.P_LEG_SS7_STATE));
		}

		switch (ainCallState) {
		case MS_DISCONNECTED:
		case SERVICE_LOGIC:
		case PSX_ROUTING:
		case ASSIST: {
			logger.debug(dialogId + ":: ConnectioNmode: " + action.getConnectionMode());

			if (action.getConnectionMode() == CONNECTIONMODE.REROUTING
					|| action.getConnectionMode() == CONNECTIONMODE.CONTINUE) {

				boolean sendContinue = false;

				// Send Continue. It is allowed to send continue only if InfoAnalyed is
				// received.
				// In case termination attempt is received and user has set it by mistake then
				// send AuthorizationAttempt.
				if (action.getConnectionMode() == CONNECTIONMODE.CONTINUE) {
					sendContinue = true;
				}

				// Check the last operation and based on it decide the response
				if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null) {
					if (MESSAGE.INFO_ANALYZE == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
							|| MESSAGE.INFO_COLLECT == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
							|| MESSAGE.NTWK_BUSY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
						if (sendContinue) {
							sendContinue(tcapSession, action);
						} else {
							sendAnalyzeRoute(tcapSession, action);
						}
//					} else if (MESSAGE.PROVIDE_INSTRUCTION == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
//						sendConnectionControl(tcapSession, action);
//					} 
					}else if (MESSAGE.PROVIDE_INSTRUCTION == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
							String isTR533Enabled = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.TR533_ENABLED);
							if (PhConstants.TRUE.equals(isTR533Enabled)) {
								TR533MessageHandler.sendConnectionControlWrapper(tcapSession, action);
							}else{
								Gr533MessageHandler.sendConnectionControl(tcapSession, action);
							}
						}else {
						if (leg2Data.get(LegDataAttributes.NP_SEND_FORWARD_CALL_FOR_TERM_ATTEMPT) != null) {
							sendForwardCallForTerm(tcapSession, action);
						} else {
							sendAuthorizeTermination(tcapSession, action);
						}
					}
				} else {
					if (sendContinue) {
						sendContinue(tcapSession, action);
					} else {
						sendAnalyzeRoute(tcapSession, action);
					}
				}

				legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERM_CONNECT_IN_PROGRESS);

				/*
				 * Set assist appsession timeout to 2 miniutes
				 */
				SipApplicationSession assistAppSession = getAssistAppSession(tcapSession, PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());

				if (assistAppSession != null) {
					CommonUtils.setAppSessionTimeout(assistAppSession, PhConstants.CLEANUP_APPSESSION_TIMEOUT,
							dialogId);
				}
			}else if (action.getConnectionMode() == CONNECTIONMODE.MRS_RELAY 
					|| PhConstants.CLASS.equals(leg2Data.get(LegDataAttributes.P_PSX_QTYPE))
					|| PhConstants.ISVM.equals(leg2Data.get(LegDataAttributes.P_PSX_QTYPE))) {

				if(logger.isDebugEnabled()){
					logger.debug(dialogId + ":: ConnectioNmode: MRS_RELAY or performing ms relay as per QTYPE " +leg2Data.get(LegDataAttributes.P_PSX_QTYPE) );
				}
				// Check the last operation and based on it decide the response
				if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null) {

					AinScfProtocolHelper.sendMRSRelay(tcapSession, action);
				}

				//legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MRS_RELAY_IN_PROGRESS);

				/*
				 * Set assist appsession timeout to 2 miniutes
				 */
				SipApplicationSession assistAppSession = getAssistAppSession(tcapSession, PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());

				if (assistAppSession != null) {
					CommonUtils.setAppSessionTimeout(assistAppSession, PhConstants.CLEANUP_APPSESSION_TIMEOUT,
							dialogId);
				}
			}
			else {
				logger.error(dialogId + ":: Connect term invoked with invalid connection mode, drop call");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INVALID_CONNMODE_TERM);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
			}
			break;
		}
		case PROVIDE_INSTRUCTION: {
			if (logger.isDebugEnabled()) {
				logger.debug(
						dialogId + "AinCallState:" + ainCallState + "::ConnectioNmode: " + action.getConnectionMode());
			}
			// Check the last operation and based on it decide the response
			if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null) {
				if (MESSAGE.PROVIDE_INSTRUCTION == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
					String isTR533Enabled = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.TR533_ENABLED);
					if (PhConstants.TRUE.equals(isTR533Enabled)) {
						TR533MessageHandler.sendConnectionControlWrapper(tcapSession, action);
					}else{
						Gr533MessageHandler.sendConnectionControl(tcapSession, action);
					}
				}
			} else {
				logger.error(dialogId + ":: Last Operation not found for AinCallState:" + ainCallState);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INVALID_CONNMODE_TERM);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
			}
			break;
		}
		case AC_QUERY: {
			if (logger.isDebugEnabled()) {
				logger.debug(
						dialogId + "AinCallState:" + ainCallState + "::ConnectioNmode: " + action.getConnectionMode());
			}
			// Check the last operation and based on it decide the response
			if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null) {
				//	if (action.getConnectionMode() == CONNECTIONMODE.MRS_RELAY) {

				if(logger.isDebugEnabled()){
					logger.debug(dialogId + ":: ConnectioNmode: MRS_RELAY" );
				}

				AinScfProtocolHelper.sendMRSRelay(tcapSession, action);

				//legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MRS_RELAY_IN_PROGRESS);

				/*
				 * Set assist appsession timeout to 2 miniutes
				 */
				SipApplicationSession assistAppSession = getAssistAppSession(tcapSession, PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());

				if (assistAppSession != null) {
					CommonUtils.setAppSessionTimeout(assistAppSession, PhConstants.CLEANUP_APPSESSION_TIMEOUT,
							dialogId);
				}
				//				}else if (MESSAGE.AC_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
				//					sendAcQueryResponse(tcapSession, action);
				//				
				//				}
			} else {
				logger.error(dialogId + ":: Last Operation not found for AinCallState:" + ainCallState);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INVALID_CONNMODE_TERM);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
			}
			break;
		}
		case ISVM_QUERY: {
			if (logger.isDebugEnabled()) {
				logger.debug(
						dialogId + "AinCallState:" + ainCallState + "::ConnectioNmode: " + action.getConnectionMode());
			}
			// Check the last operation and based on it decide the response
			if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null) {
				//	if (action.getConnectionMode() == CONNECTIONMODE.MRS_RELAY) {

				if(logger.isDebugEnabled()){
					logger.debug(dialogId + ":: perform MRS_RELAY" );
				}

				AinScfProtocolHelper.sendMRSRelay(tcapSession, action);

				//legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MRS_RELAY_IN_PROGRESS);

				/*
				 * Set assist appsession timeout to 2 miniutes
				 */
				SipApplicationSession assistAppSession = getAssistAppSession(tcapSession, PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());

				if (assistAppSession != null) {
					CommonUtils.setAppSessionTimeout(assistAppSession, PhConstants.CLEANUP_APPSESSION_TIMEOUT,
							dialogId);
				}
				//	}
			} else {
				logger.error(dialogId + ":: Last Operation not found for AinCallState:" + ainCallState);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INVALID_CONNMODE_TERM);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
			}
			break;
		}
		case OLNS_QUERY:
		case GN_QUERY:
		case BNS_QUERY:
		case CC1_QUERY:
		case CC2_QUERY:
		case TLNS_QUERY:
		case GET_DATA_QUERY:
		case ICDC_QUERY:
		case INTERCEPT_QUERY:
		case ACCOUNT_CODE_QUERY: {

			// LIDB is always Query and Response.
			if (logger.isDebugEnabled()) {
				logger.debug(
						dialogId + "AinCallState:" + ainCallState + "::ConnectioNmode: " + action.getConnectionMode());
			}

			// Check the last operation and based on it decide the response
			if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null) {

				if (action.getConnectionMode() == CONNECTIONMODE.MRS_RELAY) {

					if(logger.isDebugEnabled()){
						logger.debug(dialogId + ":: ConnectioNmode: MRS_RELAY" );
					}

					AinScfProtocolHelper.sendMRSRelay(tcapSession, action);

					//legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MRS_RELAY_IN_PROGRESS);

					/*
					 * Set assist appsession timeout to 2 miniutes
					 */
					SipApplicationSession assistAppSession = getAssistAppSession(tcapSession, PhUtilityServices
							.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());

					if (assistAppSession != null) {
						CommonUtils.setAppSessionTimeout(assistAppSession, PhConstants.CLEANUP_APPSESSION_TIMEOUT,
								dialogId);
					}
				}else if (MESSAGE.BNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
						|| MESSAGE.GN_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
						|| MESSAGE.CC1_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
						|| MESSAGE.CC2_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
						|| MESSAGE.TLNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
						|| MESSAGE.GET_DATA_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)						
						|| MESSAGE.OLNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
						|| MESSAGE.ICDC_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
						|| MESSAGE.INTERCEPT_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)
						|| MESSAGE.ACCOUNT_CODE_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
					sendLidbResponse(tcapSession, action);
				}
			} else {
				logger.error(dialogId + ":: Last Operation not found for AinCallState:" + ainCallState);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INVALID_CONNMODE_TERM);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
			}
			break;

		}
		default:
			logger.error(dialogId + ":: Connect term invoked in invalid state, drop call");
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXP_ACT_CONNECT_TERM);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			dropCall(tcapSession, callData);
			break;
		}
	}

	/**
	 * Default drop call method to create default action on drop call based on call
	 * state.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void dropCall(TcapSession tcapSession, CallData callData) {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside dropCall with tcapSession");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		AinCallStates ainCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		/*
		 * Constructing default action. For RC use cause value set by service. For other
		 * cases use default.
		 */
		switch (ainCallState) {
		case INIT:
		case BNS_QUERY:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: For Bns Query default drop Mode is USER_ABORT");
			}
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(AinScfProtocolFSMHandler.AbortInfoEnum.NO_REASON_GIVEN.getCode());
			break;
		case GN_QUERY:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: For GN Query default drop Mode is USER_ABORT");
			}
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(AinScfProtocolFSMHandler.AbortInfoEnum.NO_REASON_GIVEN.getCode());
			break;
		case SERVICE_LOGIC:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: For service logic state default drop Mode is USER_ABORT");
			}
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(AinScfProtocolFSMHandler.AbortInfoEnum.NO_REASON_GIVEN.getCode());
			break;
		case HANDOFF:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: For HANDOFF state default drop Mode is NONE");
			}
			action.setDropCallMode(DROP_CALL_MODE.NONE);
			break;
		case TERMINATION_IN_PROGRESS:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: For TERMINATION_IN_PROGRESS state default drop Mode is NULL_END");
			}
			action.setDropCallMode(DROP_CALL_MODE.NULL_END);
			break;
		case TERMINATED:
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " :: For TERMINATED state default drop Mode is USER_ABORT");
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
				logger.debug(dialogId + " :: For CONNECT_INPROGRESS/CONNECTED/ASSIST/default states "
						+ "use default drop Mode is RELEASE_CALL");
			}
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());

		}// end switch
		dropCall(tcapSession, callData, action);
	}

	/**
	 * Executes drop call action passed to method based on AIN call state.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 */
	public static void dropCall(TcapSession tcapSession, CallData callData, Action action) {

		Integer dialogId = null;
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		try {

			dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + "[PH]:: Inside dropCall with tcapSession");
				logger.debug(dialogId + ":: AinCallState is " + legData.get(LegDataAttributes.P_LEG_SS7_STATE));
				logger.debug(dialogId + ":: DropCallMode is " + action.getDropCallMode().name());
			}

			/*
			 * if last dialog is END; No need to term message
			 */
			int rxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
			if (rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END) {
				logger.warn(dialogId + ":: Last Rx dialog primitive is END; Clean the call locally.");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.END_RCVD_WITH_COMP);

				/*
				 * before return pre process dropped call as in finally block CDRs will be
				 * written
				 */

				preProcessDroppedCall(tcapSession);
				return;
			}

			if (rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_USER_ABORT
					|| rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_PROVIDER_ABORT
					|| rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_NOTICE) {

				logger.error(dialogId + ":: Last dialogue received, so clean the call locally");
				/*
				 * before return pre process dropped call as in finally block CDRs will be
				 * written
				 */
				preProcessDroppedCall(tcapSession);
				return;
			}

			AinCallStates ainCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

			logger.debug("AinCallStates in dropCall: " + ainCallState);

			switch (ainCallState) {
			case GET_DATA_QUERY:			
			case CC2_QUERY:
			case OLNS_QUERY:
			case GN_QUERY:
			case TLNS_QUERY:
			case BNS_QUERY:
			case INTERCEPT_QUERY:
			case ICDC_QUERY:
			case ACCOUNT_CODE_QUERY: {
				if (logger.isDebugEnabled()) {
					logger.debug("Recieved Call drop for BNS/CC/TLNS/GETDATA/AccountCode APPLICATION ERROR");
				}
				sendBnsAppError(tcapSession, action);
				return;
			}
			case CC1_QUERY: {
				if (logger.isDebugEnabled()) {
					logger.debug("Recieved Call drop for CC1 APPLICATION ERROR");
				}
				sendCc1AppError(tcapSession, action);
				return;				
			}
			case LIDB_PROTOCOL_ERR: {
				if (logger.isDebugEnabled()) {
					logger.debug("Recieved Call drop for BNS PROTOCOL ERROR");
				}
				sendBnsProtocolError(tcapSession, action, PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());
				return;
			}
			case LIDB_APP_ERR_PROBLEM_DATA: {
		        if (logger.isDebugEnabled()) {
		                logger.debug("Received Call drop for APPLICATION ERROR with Problem Data");
		        }
		        sendApplicationErrorWithProblemData(tcapSession, action);
		        return;
			}
//			case OLNS_QUERY: {
//				if (logger.isDebugEnabled()) {
//					logger.debug("Recieved Call drop for OLNS APPLICATION ERROR");
//				}
//			}
//			sendOlnsAppError(tcapSession, action);
//			return;
//			case GN_QUERY: {
//				if (logger.isDebugEnabled()) {
//					logger.debug("Recieved Call drop for GN APPLICATION ERROR");
//				}
//			}
//			sendGnAppError(tcapSession, action);
//			return;
			case INIT:
			case SERVICE_LOGIC:
			case MS_PLAY:
			//case PROVIDE_INSTRUCTION:
			case TERM_CONNECT_IN_PROGRESS:
			case TERM_CONNECTED:
			case PSX_ROUTING:
			case TERMINATED:
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " ::dropCall(session,action)");
				}
				break;
			case TERMINATION_IN_PROGRESS:

				/*
				 * This is internal stage after this stage is set.. No messages will be received
				 * by ph before state is changed to Terminated.
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " ::dropCall(session,action)");
				}
				/*
				 * force drop call mode to null end as state happens on odisconnect and oAbndon
				 * only
				 */
				action.setDropCallMode(DROP_CALL_MODE.NULL_END);
				break;
			case ASSIST:
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " :: dropCall(session,action), send ResourceClear and remove entry from correlation map");
					logger.debug(dialogId + " :: clean sip leg if any");
				}

				State sipState = (State) legData.get(LegDataAttributes.P_LEG_SIP_STATE);
				if (sipState != null && sipState.equals(State.MS_CONNECTED)) {

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + " :: Sip state is ms connected so disconnect the MS:");
					}
					disconnectIvr(tcapSession, callData, new Action(ActionType.ACTION_DISCONNECT_MS));
				}

				/*
				 * clean sip leg if presentCleanup all sip leg according to their sip session
				 * state
				 */

				break;

			case HANDOFF:
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " :: dropCall(session,action) HANDOFF, resource cleanup on common action");
				}

				/*
				 * for handoff no message is required to be sent out
				 */
				action.setDropCallMode(DROP_CALL_MODE.NONE);
				break;

			/**
			 * return error message for GR533
			 */
			case PROVIDE_INSTRUCTION:
				if (logger.isDebugEnabled()) {
					logger.debug("Recieved Call drop for PROVIDE INSTRUCTION Error");
				}
				Gr533MessageHandler.sendGR533ReturnError(tcapSession, action);
				break;
			default:
				logger.warn(dialogId + "::dropCall(session,action) Unhandled AIN call state");
				action.setDropCallMode(DROP_CALL_MODE.NONE);
				break;
			}

			/*
			 * marking call for clean up and cleaning timers/correlation resources before
			 * sending
			 */
			preProcessDroppedCall(tcapSession);

			/*
			 * dropping the call as per action
			 */
			sendDropMessage(tcapSession, action, PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());

		} catch (Exception ex) {
			logger.error(dialogId + ":: Failed to drop the call.", ex);
		} finally {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Inform service that call is dropped and write CDR");
			}

			postProcessDroppedCall(tcapSession, true);

		} // end finally
	}

	/**
	 * This method returns the sip application session of the correlated INVITE
	 * received for outgoing ASSIST
	 * 
	 * @param tcapSession              represents an instance of TcapSession
	 * @param sipSessionUtilrepresents an instance of SipSessionsUtil
	 * @return an instance of SipApplicationSession
	 */
	public static SipApplicationSession getAssistAppSession(TcapSession tcapSession, SipSessionsUtil sipSessionUtil) {
		SipApplicationSession sipApplicationSession = null;
		String appSessionId = (String) tcapSession.getAttribute(PhConstants.ASSIST_APP_SESSION_ID);
		if (appSessionId != null) {
			sipApplicationSession = sipSessionUtil.getApplicationSessionById(appSessionId);
		}
		return sipApplicationSession;
	}

	/**
	 * This method is called on receiving action ACTION_CONNECT_MS
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void initiateMediaServerConnection(TcapSession tcapSession, CallData callData, Action action)
			throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		AinCallStates ainCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		boolean useSS7IP = false;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside initiateMediaServerConnection with tcapSession");
			logger.debug(dialogId + ":: AinCallState is " + ainCallState);
			logger.debug(dialogId + ":: ConnectionMode is " + action.getConnectionMode().name());
		}

		switch (ainCallState) {
		case SERVICE_LOGIC:
		case ASSIST:
		case MS_DISCONNECTED:
		case PROVIDE_INSTRUCTION:{
			switch (action.getConnectionMode()) {
			case ASSIST:
			case B2BUA: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Connection Mode is " + action.getConnectionMode().name()
							+ ". Send SendToResource");
				}

				if (action.getConnectionMode() == CONNECTIONMODE.B2BUA) {
					legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.HANDOFF);

					if (logger.isDebugEnabled()) {
						logger.debug("Setting AIN state as HANDOFF");
					}
				} else {
					legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.ASSIST);
				}

				String correlationId = (String) callData.get(CallDataAttribute.P_CORRELATION_ID);

				// check if sharedPool need to be used.
				String sharedPoolEnabled = AinScfProtocolConfig
						.getConfigData(AinScfProtocolConfig.SHARED_TOKEN_POOL_ENABLED);

				if (PhConstants.TRUE.equals(sharedPoolEnabled)) {

					correlationId = AinScfProtocolUtil.getTokenFromSharedPool(callData);

					if (correlationId == null) {
						logger.error(
								"PRI number exhausted. Cleaning up call for DialogueID:" + tcapSession.getDialogueId());
					}
				}

				/*
				 * When correlation id is set by application then we will send SendToResource to
				 * MS which may be SIP or SS7 IPMS but in case correlation id is not set by
				 * application it means the deployment is pure SS7 and deployment will directly
				 * use IP(Intellegent Peripheral) of SS7 network. so we will send
				 * ConnectToResource in this case.
				 */

				if (correlationId != null) {

					/*
					 * Setting dialouge-id for the correlation-id so that SAS can give INVITE of ETC
					 * to same thread where IDP were delivered. This has been done to avoid deadlock
					 * due to parallel processing of AIN and SIP in different thread.
					 */

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Save correlationId in correlationMap .." + correlationId);
					}

					PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
					.getCorrelationMap()
					.put((String) correlationId, Integer.toString(tcapSession.getDialogueId()));

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Updated correlation map is  .." + PhUtilityServices
								.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap());
					}

					if (action.getConnectionMode() == CONNECTIONMODE.B2BUA) {

						// Forward call can be sent only if TerminationAttempt is recevied as
						// last operation. In case InfoAnalyze is received then need to send
						// analyzeRoute with correlation Number
						if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null) {
							if (MESSAGE.INFO_ANALYZE == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
								sendHandOffAnalyzeRoute(tcapSession, action);
							} else if (MESSAGE.TERM_ATTEMPT == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
								sendHandOffForwardCall(tcapSession, action);
							} else {
								sendHandOffForwardCall(tcapSession, action);
							}
						} else {
							sendHandOffForwardCall(tcapSession, action);
						}
					} else {
						sendAssistSTR(tcapSession, action);
					}
				} else {

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Correlation id is not set ..seems like deployment is pure SS7. If deployemet is pure SS7 then use DIRCT_MS mode");
					}

					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
					callData.set(CallDataAttribute.NP_ETC_SEND_FAILURE, 1);
					dropCall(tcapSession, callData);
				}
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
				legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_CONNECTED);
				Event event = new Event(Event.EventType.EVENT_MS_SUCCESS, Protocol.AIN_SCF,
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
				useSS7IP = true;
				logger.error(dialogId + ":: Connect ivr invoked in invalid state, drop call");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXP_ACT_CONIVR_ORIG);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
			}

			if (!useSS7IP) {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Start Correlation Timer");
				}

				AinScfProtocolUtil.startTimer(tcapSession, AinScfProtocolUtil.getCorrelationTime(callData), true,
						PhConstants.CORRELATION_TIMER);

				/*
				 * setting tcapsession id in corr map; not setting entire object to avoid ft
				 * issues
				 */
				String correlationId = (String) callData.get(CallDataAttribute.P_CORRELATION_ID);
				PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCorrelationMap()
				.put(correlationId, tcapSession.getDialogueId());
				tcapSession.setAttribute(PhConstants.CORRELATION_ID, correlationId);
			}
		}
		break;
		default: {
			logger.error("[PH]:: Connect ivr invoked in invalid state, drop call");
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.UNEXP_ACT_CONIVR_ORIG);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			dropCall(tcapSession, callData);
		}
		}
	}

	/**
	 * Method to send STR with continue in Assist mode.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */
	private static void sendAssistSTR(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendAssistSTR");
		}

		// ToDo - whether to send STR or Forward call while connecting to media server
		// should be configurable through parameter.
		sendSTR(tcapSession, action.getLeg(), false);

		if (action.getConnectionMode() == CONNECTIONMODE.B2BUA) {
			// In case of Hand off we want to stop the session from getting invalidating
			// Therefore setting it true. JainTcapProviderImpl handles it.
			tcapSession.setAttribute(PhConstants.FOR_HANDOFF, PhConstants.TRUE);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
		} else {
			sendContinueRequestEvent(tcapSession, cCallTraceService);
		}
	}

	/**
	 * Method sends Forward Call in case of connecting Media server in Handoff mode
	 * 
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	private static void sendHandOffForwardCall(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendHandOffForwardCall");
		}

		sendForwardCall(tcapSession, action.getLeg());

		if (action.getConnectionMode() == CONNECTIONMODE.B2BUA) {
			// In case of Hand off we want to stop the session from getting invalidating
			// Therefore setting it true. JainTcapProviderImpl handles it.
			tcapSession.setAttribute(PhConstants.FOR_HANDOFF, PhConstants.TRUE);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
		} else {
			sendContinueRequestEvent(tcapSession, cCallTraceService);
		}
	}

	/**
	 * Method for sending ANalyze Route for hand off case in order to connect Media
	 * server.
	 * 
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	private static void sendHandOffAnalyzeRoute(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendHandOffAnalyzeRoute");
		}

		sendAnalyzeRouteForMsConnect(tcapSession, action.getLeg());

		tcapSession.setAttribute(PhConstants.FOR_HANDOFF, PhConstants.TRUE);
		sendEndRequestEvent(tcapSession, false, cCallTraceService);
	}

	/**
	 * Method to send Sent to Resource to connect to media server. STR can either be
	 * sent in Assist mode where based on destination number CAS will receive INVITE
	 * (SIP based IVR) or in case where Switch will play the announcement. The flag
	 * isStrForCallCompletion decides whether STR to be sent as TC_END with switch
	 * specific parameters so that Switch can play an announcement. This is
	 * generally used to provide proper error treatment so that switch can play
	 * announcment in case of error.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 * @throws Exception
	 */
	private static void sendSTR(TcapSession tcapSession, String leg, boolean isStrForCallCompletion) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendSTR");
		}

		byte[] str = null;

		if (!isStrForCallCompletion) {
			str = AinScfProtocolParser.createSTR(callData,
					(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS), leg);
		} else {
			// STR for call completion
			str = AinScfProtocolParser.createSTRForCallCompletion(callData,
					(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS), leg);

			if (logger.isDebugEnabled()) {
				logger.debug("SendSTR for call completion");
			}
		}

		byte[] etcOpCode = CommonUtils.formatIntToByte(AinOpCodes.SEND_TO_RESOURCE_BYTE);

		Operation etcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, etcOpCode);

		InvokeReqEvent strInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), etcOperation);
		strInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		strInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, str));
		strInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		strInvokeReqEvent.setLastInvokeEvent(true);
		strInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(strInvokeReqEvent, callData, cCallTraceService);
	}

	/**
	 * Send Forward Call.
	 * 
	 * @param tcapSession
	 * @param leg
	 * @throws Exception
	 */
	private static void sendForwardCall(TcapSession tcapSession, String leg) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendForwardCall");
		}

		byte[] fwdCallBuff = AinScfProtocolParser.createForwardCall(callData, leg);

		byte[] fwOpCode = CommonUtils.formatIntToByte(AinOpCodes.FORWARD_CALL_BYTE);

		Operation fwcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, fwOpCode);

		InvokeReqEvent fwcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), fwcOperation);
		fwcInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		fwcInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, fwdCallBuff));
		fwcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		fwcInvokeReqEvent.setLastInvokeEvent(true);
		fwcInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(fwcInvokeReqEvent, callData, cCallTraceService);
	}

	private static void sendForwardCallForQuery(TcapSession tcapSession, String leg) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendForwardCall");
		}
		byte[] fwdCallBuff = null;
		if (MESSAGE.BNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			fwdCallBuff = BNSQuery.encodeBnsQuery(callData);
		} else if (MESSAGE.GN_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			// fwdCallBuff = GenericNameQuery.encodeGenericNameQuery(callData);
		}
		byte[] fwOpCode = CommonUtils.formatIntToByte(AinOpCodes.FORWARD_CALL_BYTE);

		Operation fwcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, fwOpCode);

		InvokeReqEvent fwcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), fwcOperation);
		fwcInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		fwcInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, fwdCallBuff));
		fwcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		fwcInvokeReqEvent.setLastInvokeEvent(true);
		fwcInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(fwcInvokeReqEvent, callData, cCallTraceService);
	}

	/**
	 * Method is used to create AnalyzeROute for Handoff (to connect for media
	 * server interaction)
	 * 
	 * @param tcapSession
	 * @param leg
	 * @throws Exception
	 */
	private static void sendAnalyzeRouteForMsConnect(TcapSession tcapSession, String leg) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendAnalyzeRouteForMsConnect");
		}

		byte[] arBuff = AinScfProtocolParser.createAnalyzeRouteForMs(callData);

		byte[] arOpCode = CommonUtils.formatIntToByte(AinOpCodes.ANALYZE_ROUTE_BYTE);

		Operation arOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, arOpCode);

		InvokeReqEvent arInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), arOperation);
		arInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		arInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, arBuff));
		arInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		arInvokeReqEvent.setLastInvokeEvent(true);
		arInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(arInvokeReqEvent, callData, cCallTraceService);
	}

	/**
	 * This method execute the DISCONNECT_IVR action, requested by services. In case
	 * of an exception in disconnecting ivr connection, call is dropped with CV=41.
	 * 
	 * @param appSession represents the instance of SipApplicationSession
	 * @param action     represents the instance of Action
	 * @throws Exception
	 */
	public static void disconnectIvr(TcapSession tcapSession, CallData callData, Action action) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		try {

			LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			AinCallStates ainState = (AinCallStates) origLegData.get(LegDataAttributes.P_LEG_SS7_STATE);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: inside disconnectIvr in currentState:" + ainState);
			}
			/*
			 * SIP IVR ASSIST case it will be IP media server connection when we send
			 * SendToResource to use SIP IVR. A sip invite will be received . so AIN state
			 * will be ASSIST and SIP state will be MS_CONNETED.
			 * 
			 * In AIN ResourceClear is expected from switch instead of SCP sending it.
			 * Therefore BYE will be sent to Media Gateway and expect ResourceClear from
			 * Switch.
			 */

			if (ainState == AinCallStates.ASSIST) {

				if (logger.isDebugEnabled()) {
					logger.debug(
							dialogId + " [PH]::  ASK SIPProtocolHandler to cleanup SIP IVR leg as AIN Call state is "
									+ AinCallStates.ASSIST);
				}

				// In case AIN, ASSIST call flow, SIP IVR is used. When application request to
				// disconnect
				// media server then SIP sends BYE to MS and Originating Leg. At that moment SIP
				// mediaEventListener
				// received EVENT_DISCONNECTED is received and it send MS_DISCONNECTED to
				// application. Ideally
				// MS_Disconnected should be send on receiving Resource Clear. In order to make
				// sure that
				// MS_Disconnected is not raised by SIP. following flag is set.
				action.setMsDiconnectedForAinCall(true);

				ProtocolHandler stateHandler = ProtocolHandlerFactory.getProtocolHandler(Protocol.SIP);
				stateHandler.executeAction(callData, action);

			} else {

				// check if last state was for DIRECT_MS then we need to send
				// MS_Disconencted

				if (ainState == AinCallStates.MS_PLAY || ainState == AinCallStates.MS_PLAYCOLLECT
						|| ainState == AinCallStates.MS_PLAY_APP_REQUEST) {

					// remove correlation id 
					AinScfProtocolUtil.removeCorrelationId(callData);
					
					// set call state to MS_DISCONNECT
					origLegData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);

					ServiceInterface serviceHandler = PhUtilityServices
							.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
					Event event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.AIN_SCF,
							CallDataAttribute.P_LEG1.toString());

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: disconnectIvr, notify app MS_DISCONNECTED");
					}
					ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
				} else if (ainState == AinCallStates.MS_PLAY_CIFR || ainState == AinCallStates.MS_PLAY_COLLECT_CIFR) {
					// this is the case where CIFR is received from network in response to STR
					// If application need to send Disconnect then we need to send CITR as blank
					// return result. After which SSP will send ResourceClear
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: disconnectIvr, recieved for CIFR as state::" + ainState);
					}
					//					Going to send null CITR.
					sendDisconnectCITR(tcapSession, callData, action);
				} else {
					logger.error(dialogId + " Invalid State: for disconnectIvr(): ainState:" + ainState);
				}
			}

		} catch (Exception ex) {
			logger.error(dialogId + ":: Failed to disconnect ivr. Error is " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + ":: Error in disconnecting orig ivr.", ex);
			}
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.EXCEP_DISCON_ORIG_IVR);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			dropCall(tcapSession, callData);
		}
	}

	/**
	 * This method is used to send STR with - AnnouncementBlock Or -
	 * FlexParameterBlock - StrParameterBlock - AnnouncementBlock
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendPlayAnnouncement(TcapSession tcapSession, CallData callData, Action action)
			throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendPlayAnnouncement");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC);

		if (annSpec == null) {
			logger.error(dialogId + ":: No announmcement spec specified by Service");
			throw new Exception(dialogId + ":: :: No announmcement spec specified by Service");
		}

		byte[] str = null;
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		/**
		 * create STR ...Depending on condition if flex flag true then we will create
		 * STR for Flex Play Announcement. else we will create Normal STR for Play.
		 */
		try {
			if (annSpec.isFlex()) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + "::Going to create STR for FlexParameterBlock choice");
				}
				str = AinScfProtocolParser.createSTRForFlexPlay(callData, action);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + "::Going to create STR for AnnouncementBlock Choice");
				}
				str = AinScfProtocolParser.createSTRForPlay(callData, action);
			}

		} catch (Exception e) {
			logger.error(dialogId + "::Exception Inside sendPlayAnnouncement:" + e);
		}

		// set state
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_PLAY);

		byte[] strOpCode = CommonUtils.formatIntToByte(AinOpCodes.SEND_TO_RESOURCE_BYTE);
		Operation etcoprn = new Operation(Operation.OPERATIONTYPE_LOCAL, strOpCode);

		InvokeReqEvent strInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), etcoprn);
		strInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		strInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, str));
		strInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		strInvokeReqEvent.setLastInvokeEvent(true);
		strInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		sendComponentReq(strInvokeReqEvent, callData, cCallTraceService);

		// check the mode - END or CONTINUE - depending on mode send Response or
		// Continue
		if (action.getSendMode() == Action.SEND_MODE.END) {

			// set the release code as
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_DISCONNECT_AFTER_PLAY);
			preProcessDroppedCall(tcapSession);

			sendEndRequestEvent(tcapSession, false, cCallTraceService);

			postProcessDroppedCall(tcapSession, true);
		} else {
			sendContinueRequestEvent(tcapSession, cCallTraceService);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendPlayAnnouncement(), sent as:" + action.getSendMode());
		}
	}

	/**
	 * This method is used to send STR with - AnnouncementDigitBlock Or -
	 * FlexParameterBlock - StrParameterBlock - AnnouncementDigitBLock
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendPlayAndCollect(TcapSession tcapSession, CallData callData, Action action) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendPlayAndCollect");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC);

		if (annSpec == null) {
			logger.error(dialogId + ":: No announmcement spec specified by Service");
			throw new Exception(dialogId + ":: :: No announmcement spec specified by Service");
		}
		byte[] str = null;
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		/**
		 * create STR for Play And Collect. if flex flag is true then we will create STR
		 * for PNC Using FlexParameterBlock else create STR for PNC Using
		 * AnnouncementDigitBlock.
		 */
		try {
			if (annSpec.isFlex()) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + "::Going to create STR for FlexParameterBlock choice");
				}
				str = AinScfProtocolParser.createSTRForFlexPNC(callData);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + "::Going to create STR for AnnouncementBlock Choice");
				}
				str = AinScfProtocolParser.createSTRForPNC(callData,
						((action.getSendMode() == Action.SEND_MODE.END)?true:false));
			}
		} catch (Exception e) {
			logger.error(dialogId + "::Exception Inside sendPlayAndCollect:" + e);
		}

		// set state
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_PLAYCOLLECT);

		byte[] strOpCode = CommonUtils.formatIntToByte(AinOpCodes.SEND_TO_RESOURCE_BYTE);
		Operation etcoprn = new Operation(Operation.OPERATIONTYPE_LOCAL, strOpCode);

		InvokeReqEvent pcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), etcoprn);
		pcInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		pcInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, str));
		pcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		pcInvokeReqEvent.setLastInvokeEvent(true);
		pcInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		sendComponentReq(pcInvokeReqEvent, callData, cCallTraceService);

		if (action.getSendMode() == Action.SEND_MODE.END) {
			preProcessDroppedCall(tcapSession);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
			postProcessDroppedCall(tcapSession, true);
		} else {
			sendContinueRequestEvent(tcapSession, cCallTraceService);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendPlayAndCollect() with mode:" + action.getSendMode());
		}
	}

	/**
	 * This method is called by PH to send AnalyzeRoute. Depending on connection
	 * mode call shall be sent either in CWP or Repose. In case of CWP, application
	 * shall send AnalyzeRoyte Followed by RRBCSM. Also depending on flag,
	 * application shall send SendNotification message.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action.
	 * @throws Exception
	 */
	private static void sendAnalyzeRoute(TcapSession tcapSession, Action action) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendAnalyseRoute");
		}

		boolean isLastInvoke = (action.getSendMode() == Action.SEND_MODE.END) ? true : false;

		// overriding as true as one customer expects all parameters as INVOKE LAST
		// INVOKE LAST maps to E9 in component type
		isLastInvoke = true;

		// Send AnalyzeRoute - depending on if its END or not, InvokeLast will
		// be set in outgoing component
		sendAnalyzeRoute(tcapSession, CONNECT_TYPE.TERMINATING, isLastInvoke);

		// In case Send Mode is not END then send RRBCSM
		if (action.getSendMode() != Action.SEND_MODE.END) {

			// check if send Notification is also to be sent. If not then send
			// RRBCSM as INVOKE-LAST
			if (leg2Data.get(LegDataAttributes.NP_AIN_SEND_NOTIFICATION) == null) {
				isLastInvoke = true;
			}

			// override invoke last
			isLastInvoke = true;

			if (AinScfProtocolUtil.getErbSetByApplication(callData, action.getLeg()) != null) {
				sendRRBCSMForArming(tcapSession, action, isLastInvoke);
			}
		}

		// Check if SendNotification to be sent. It shall be sent based flag 
		// it can be sent in both cQP and Response scenarios
		if (leg2Data.get(LegDataAttributes.NP_AIN_SEND_NOTIFICATION) != null){
			//&& (action.getSendMode() != Action.SEND_MODE.END)) {
			sendSendNotification(tcapSession, action, true);
		}

		// check if ACG need to be sent
		if(leg2Data.get(LegDataAttributes.SEND_ACG) != null){
			sendAcg(tcapSession, action);
		}

		if (action.getSendMode() == Action.SEND_MODE.END) {

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
			preProcessDroppedCall(tcapSession);

			// sendAnalyzeRoute(tcapSession, CONNECT_TYPE.TERMINATING);
			sendEndRequestEvent(tcapSession, false, cCallTraceService);

			postProcessDroppedCall(tcapSession, true);
		} else {
			// sendAnalyzeRoute(tcapSession, CONNECT_TYPE.TERMINATING);
			sendContinueRequestEvent(tcapSession, cCallTraceService);
		}
	}

//	/**
//	 * This method is called by PH to send connectionControll.
//	 * 
//	 * @param tcapSession represents the instance of TcapSession
//	 * @param action      represents the instance of Action.
//	 * @throws Exception
//	 */
//	private static void sendConnectionControl(TcapSession tcapSession, Action action) throws Exception {
//		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
//		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
//		CallTraceService cCallTraceService = PhUtilityServices
//				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
//
//		if (logger.isDebugEnabled()) {
//			logger.debug(dialogId + " [PH]:: Inside sendConnectionControl");
//		}
//
//		byte[] ar = null;
//		ar = AinScfProtocolParser.createConnectionControlTerm(callData);
//
//		if (logger.isInfoEnabled()) {
//			logger.info("CONNECTION CONTROL ::" + CommonUtils.formatBytes(ar));
//		}
//		byte[] connectionControlOpcode = CommonUtils.formatIntToByte(AinOpCodes.CONNECTION_CONTROL_BYTE);// 1025
//		if(logger.isInfoEnabled()) {
//			logger.info("connectionControlOpcode"+CommonUtils.formatBytes(connectionControlOpcode));
//		}
//		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_GLOBAL, connectionControlOpcode);
//		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
//		connectInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
//		connectInvokeReqEvent.setLastInvokeEvent(true);
//		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, ar));
//		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
//		connectInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
//		sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
//		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
//		preProcessDroppedCall(tcapSession);
//		sendEndRequestEvent(tcapSession, false, cCallTraceService);
//		postProcessDroppedCall(tcapSession, true);
//	}

	private static void sendAcQueryResponse(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		LegData legdata = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendAcQueryResponse");
		}

		byte[] acQueryOpcode = CommonUtils.formatIntToByte(AinOpCodes.AC_QUERY_BYTE);
		if(logger.isInfoEnabled()) {
			logger.info("acQueryOpcode"+CommonUtils.formatBytes(acQueryOpcode));
		}
		Operation acOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, acQueryOpcode);

		ArrayList<InvokeIndEvent> list = (ArrayList<InvokeIndEvent>) legdata.get(LegDataAttributes.P_MRS_COMP_LIST);
		for(InvokeIndEvent Indevent: list) {
			InvokeReqEvent invokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), acOperation);
			invokeReqEvent.setInvokeId(Indevent.getInvokeId());
			invokeReqEvent.setLastInvokeEvent(true);
			invokeReqEvent.setParameters(Indevent.getParameters());
			invokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
			invokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
			sendComponentReq(invokeReqEvent, callData, cCallTraceService);
		}
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
		preProcessDroppedCall(tcapSession);
		sendEndRequestEvent(tcapSession, false, cCallTraceService);
		postProcessDroppedCall(tcapSession, true);
	}

	/**
	 * This method is used to send Forward call for Termination Attempt Request.
	 * This is controlled through variable NP_SEND_FORWARD_CALL_FOR_TERM_ATTEMPT,
	 * set in Leg2Data by application. If value is set then Forward call is sent
	 * 
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	private static void sendForwardCallForTerm(TcapSession tcapSession, Action action) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendForwardCallForTerm");
		}

		// Forward call will always be send as TC_END/

		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
		preProcessDroppedCall(tcapSession);

		sendForwardCall(tcapSession, action.getLeg());
		// Check if SendNotification to be sent. It shall be sent based flag 
				// it can be sent in both cQP and Response scenarios
				if (leg2Data.get(LegDataAttributes.NP_AIN_SEND_NOTIFICATION) != null){
					sendSendNotification(tcapSession, action, true);
				}
		sendEndRequestEvent(tcapSession, false, cCallTraceService);

		postProcessDroppedCall(tcapSession, true);

	}

	private static void sendForwardCallForLidbQuery(TcapSession tcapSession, Action action) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendForwardCallForLidbQuery");
		}

		// Forward call will always be send as TC_END/

		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
		preProcessDroppedCall(tcapSession);

		sendForwardCallForQuery(tcapSession, action.getLeg());

		// Check if SendNotification to be sent. It shall be sent based flag 
		// it can be sent in both cQP and Response scenarios
		if (leg2Data.get(LegDataAttributes.NP_AIN_SEND_NOTIFICATION) != null){
			sendSendNotification(tcapSession, action, true);
		}

		sendEndRequestEvent(tcapSession, false, cCallTraceService);

		postProcessDroppedCall(tcapSession, true);

	}

	/**
	 * This method is for creating AIN component indication event AnalyzeRoute
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 * @param connectType represents CONNECT_TYPE
	 * @throws Exception
	 */
	private static void sendAnalyzeRoute(TcapSession tcapSession, CONNECT_TYPE connectType, boolean isLastInvoke)
			throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside AnalyzeRoute, connectType is [" + connectType.name() + "]"
					+ ". isLastInvoke:" + isLastInvoke);
		}

		byte[] ar = null;
		if (connectType == CONNECT_TYPE.TERMINATING) {
			ar = AinScfProtocolParser.createAnalyzeRouteForTerm(callData);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exit createAnalyzeRouteForTerm, byte:" + CommonUtils.formatBytes(ar));
			}
		} else {
			logger.error(dialogId + ":: Invalid connection type");
			ar = AinScfProtocolParser.createAnalyzeRouteForTerm(callData);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exit createAnalyzeRouteForTerm, bytes:" + CommonUtils.formatBytes(ar));
			}
		}

		byte[] AnalyseOpCode = CommonUtils.formatIntToByte(AinOpCodes.ANALYZE_ROUTE_BYTE);

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, AnalyseOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, ar));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		if (isLastInvoke) {
			connectInvokeReqEvent.setLastInvokeEvent(true);
		} else {
			connectInvokeReqEvent.setLastInvokeEvent(false);
		}
		connectInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
	}

	/**
	 * This method is for creating AIN component RRBCSM event for disarming the
	 * events.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */
	private static void sendRRBCSMForDisarming(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		String dialogId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendRRBCSMForDisarming");
		}

		byte[] rrbe = AinScfProtocolParser.createRRBCSMForDisarming(callData, action);
		byte[] rrbeOpCode = { AinOpCodes.RRBE_BYTE };

		Operation rrbeOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rrbeOpCode);

		InvokeReqEvent rrbeInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rrbeOperation);
		rrbeInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		rrbeInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, rrbe));
		rrbeInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		rrbeInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(rrbeInvokeReqEvent, callData, cCallTraceService);
		tcapSession.setAttribute(PhConstants.LEG1_TRIGGERS_ARMED, PhConstants.FALSE);
	}

	/**
	 * This method is for creating AIN component indication event for arming the
	 * events.
	 * 
	 * @param tcapSession  represents the instance of TcapSession
	 * @param action       represents the instance of Action
	 * @param isLastInvoke
	 * @throws Exception
	 */
	private static void sendRRBCSMForArming(TcapSession tcapSession, Action action, boolean isLastInvoke)
			throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		String dialogId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendRRBCSMForArming");
		}

		byte[] rrbe = null;
		
		String isCainEnabled = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.CAIN_ENABLED);
		if (PhConstants.TRUE.equals(isCainEnabled)) {
			rrbe = AinScfProtocolParser.createRRBCSMForArmingForCain(callData, action);
		}else{
			rrbe = AinScfProtocolParser.createRRBCSMForArming(callData, action);
		}

		byte[] rrbeOpCode = CommonUtils.formatIntToByte(AinOpCodes.REQUEST_REPORT_BCSM_EVENT_BYTE);// { (byte)
		// AinOpCodes.REQUEST_REPORT_BCSM_EVENT_BYTE
		// };

		Operation rrbeOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rrbeOpCode);

		InvokeReqEvent rrbeInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rrbeOperation);
		rrbeInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		rrbeInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, rrbe));
		rrbeInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		rrbeInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		if (isLastInvoke) {
			rrbeInvokeReqEvent.setLastInvokeEvent(true);
		} else {
			rrbeInvokeReqEvent.setLastInvokeEvent(false);
		}

		sendComponentReq(rrbeInvokeReqEvent, callData, cCallTraceService);
		tcapSession.setAttribute(PhConstants.LEG1_TRIGGERS_ARMED, PhConstants.TRUE);

	}

	public static void playAndRecord(TcapSession tcapSession, CallData callData, Action action) {

	}

	public static void performCharging(TcapSession tcapSession, CallData callData, Action action) {

	}

	public static void sendHttpRequest(TcapSession tcapSession, CallData callData, Action action) {

	}

	/**
	 * This method is used to disconnect terminating party,
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 */
	public static void disconnctTerm(TcapSession tcapSession, CallData callData, Action action) {

		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside disconnectTerm() ");
		}
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		try {
			sendUAbortRequestEvent(tcapSession, action, cCallTraceService);
		} catch (Exception e) {
			logger.error("Exception thrown while disconnecting term", e);
			e.printStackTrace();
		}

	}

	public static void redirect(TcapSession tcapSession, CallData callData, Action action) {

	}

	/**
	 * This method marks call state, set disconnection time. Also cleans correlation
	 * timers. Invoke this method before sending termination message
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void preProcessDroppedCall(TcapSession tcapSession) {
		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside preProcessDroppedCall");
		}
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);

		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
			AinScfProtocolUtil.cleanupCorrelationResources(tcapSession);
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERMINATED);
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit preProcessDroppedCall");
		}
	}

	/**
	 * This method notifies service that call is dropped and executers actions
	 * reurned from service Also writes CDR after service notification. invoke thsi
	 * method afters ending termination mesage
	 * 
	 * @param tcapSession          represents the instance of TcapSession
	 * @param executeServiceAction represents the instance of boolean
	 */
	public static void postProcessDroppedCall(TcapSession tcapSession, boolean executeServiceAction) {

		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside postProcessDroppedCall");
		}
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		/*
		 * Set the call state again just to handle the case where
		 * preProcessDroppedCall() is not called due to some exception during execution.
		 */
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERMINATED);

		/*
		 * Notify service that call is dropped
		 */
		notifyCallDropped(tcapSession, executeServiceAction);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Write CDR");
		}

		/*
		 * Set the call disconnect time again just to make sure that CDR is written
		 * properly using call disconnect time. This is to handle the case where
		 * preProcessDroppedCall() is not called due to some exception during execution.
		 */
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());

		/*
		 * write CDR, CDR should be written after notifying service
		 */
		AinScfProtocolUtil.writeServiceCdr(tcapSession);

		/*
		 * Assist appsession cleanup moved after writing CDRs. otherwise P-CDR-INFO will
		 * be sent instead of DSI CDR so moved after writing CDR; This will also mean if
		 * ABORT/ER/REJ/RE/timeout is recived after assist SIP leg is cleaned
		 */
		SipApplicationSession sipApplicationSession = AinScfProtocolHelper.getAssistAppSession(tcapSession,
				PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
				.getSipSessionsUtil());

		/*
		 * Added null check as for assist timeout there wont be assist sip appsession
		 */
		if (sipApplicationSession != null) {
			/*
			 * In case of ASSIST DFC+RC+TC-END is sent to Orig. And TcapSession is
			 * invalidated. So TcapSession will not be available. And due to this SN would
			 * not be able to obtain callData from TcapSession. So set callData at
			 * AppSession of Assist INVITE
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
	 * @param tcapSession          represents the instance of TcapSession
	 * @param executeServiceAction represents the instance of boolean
	 */
	public static void notifyCallDropped(TcapSession tcapSession, boolean executeServiceAction) {
		int dialogueId = 0;
		try {
			dialogueId = tcapSession.getDialogueId();
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "[PH]:: Inside notifyCallDropped");
			}
			CallData callData = AinScfProtocolUtil.getCallData(tcapSession);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + " :: [PH] Notify service that call is dropped");
			}
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event event = new Event(EventType.EVENT_CALL_DROPPED, Protocol.AIN_SCF, null);

			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

		} catch (Exception ex) {
			logger.error(dialogueId + "::[PH] Error in notifyCallDropped " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info(dialogueId + "::[PH] Error in processing notifyCallDropped", ex);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit notifyCallDropped");
		}
	}

	/**
	 * This method is called to start the CDR timer for the intermediate CDRs
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 */
	private void startCdrTimer(TcapSession tcapSession, CallData callData) {

		SipApplicationSession appSession = AinScfProtocolUtil.getAppSession(tcapSession);
		String dialogId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

		long initialDelay = 0L;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Start Intermediate CDR Timer");
		}

		/*
		 * moved before timer creation to replicate updated value
		 */
		CommonUtils.setAppSessionTimeout(appSession, (int) (initialDelay * 60000) + 5, dialogId);

		AinScfProtocolUtil.startTimer(tcapSession, initialDelay, false, PhConstants.CDR_TIMER);

	}

	public static Action[] processNetworkBusy(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService, boolean componentInBegin) {

		// TODO Auto-generated method stub

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(
					dialogId + " :: Inside processNetworkBusy as " + ((componentInBegin == true) ? "BEGIN" : "CWP"));
		}

		// Process Network Busy
		try {
			AinScfProtocolParser.parseNetworkBusy(invokeIndEvent, callData, componentInBegin);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.NTWK_BUSY);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in NTWK_BUSY.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.NB_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in NTWK_BUSY.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.NB_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.NTWK_BUSY);
		}

		Action action = null;
		if (componentInBegin) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);

			// Fields for call tracing
			String callingPartyAddress = legData.get(LegDataAttributes.P_CALLING_PARTY) == null ? null
					: ((PhoneNumber) legData.get(LegDataAttributes.P_CALLING_PARTY)).getAddress();
			String calledPartyAddress = legData.get(LegDataAttributes.P_CALLED_PARTY) == null ? null
					: ((PhoneNumber) legData.get(LegDataAttributes.P_CALLED_PARTY)).getAddress();
			String nbCalledPartyAddress = legData.get(LegDataAttributes.P_IDP_CALLED_PARTY) == null ? null
					: ((PhoneNumber) legData.get(LegDataAttributes.P_IDP_CALLED_PARTY)).getAddress();
			String signalingPointCode = legData.get(LegDataAttributes.P_SPC) == null ? null
					: legData.get(LegDataAttributes.P_SPC).toString();

			/*
			 * match call tracing constraints
			 */
			checkCallTracingEnabled(dialogId, callData, callTraceService);

		} else {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.NETWORK_BUSY_RCVD);
		}
		// else{
		// action = new Action(Action.ActionType.ACTION_END_CALL);
		// action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		// /*
		// * set cause as temporary failure
		// */
		// action.setReleaseCauseValue(CauseValEnum.Network_failure.getCode());
		// }
		// return (new Action[] { action });
		return null;
	}

	public static Action[] processResourceClear(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AinCallStates curCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		Event event = null;
		Action lastAction = (Action) callData.get(CallDataAttribute.P_LAST_CALL_ACTION);
		String legId = lastAction.getLeg();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processResourceClear in state:" + curCallState);
		}
		// Deposit back PRI Number to pool. This shall be used only in case of
		// SIP based media server. However not required in case of Direct MS case.
		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
			AinScfProtocolUtil.returnTokenToSharedPool(callData);
		}
		try {
			AinScfProtocolParser.parseResourceClear(invokeIndEvent, callData);
			// if current state is ASSIST or Handoff then send MS_DISCONNECT
			// releaseCause = normal && current state is MS_PLAY then event as
			// MS_PLAY_SUCCESS
			// releaseCause != normal && current state is Play then event MS_PLAY_FAILURE
			// releaseCause = normal && current state is MS_PLAYCOLLECT then event
			// MS_PLAYCOLLECT_SUCCESS
			// releaseCause != normal && current state is MS_PLAYCOLLECT then even
			// MS_PLAYCOLELCT_FAILURE
			if (curCallState == AinCallStates.HANDOFF || curCallState == AinCallStates.ASSIST) {
				event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.AIN_SCF, null);
			} else if (curCallState == AinCallStates.MS_DISCONNECT_INPROGRESS) {
				event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.AIN_SCF, legId);
				legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_DISCONNECTED);
			} else {
				Integer causeCode = (Integer) legData.get(LegDataAttributes.P_RC_CAUSE);
				switch (causeCode) {
				// normal - GR1299 sec 6.47
				case 0: {
					if (curCallState == AinCallStates.MS_PLAY) {
						event = new Event(EventType.EVENT_PLAY_SUCCESS, Protocol.AIN_SCF, null);
					} else if (curCallState == AinCallStates.MS_PLAYCOLLECT) {
						event = new Event(EventType.EVENT_PNC_SUCCESS, Protocol.AIN_SCF, legId);
					}
					break;
				}
				// timeout
				case 2:
				default:
					if (curCallState == AinCallStates.MS_PLAY) {
						event = new Event(EventType.EVENT_PLAY_FAILURE, Protocol.AIN_SCF, null);
					} else if (curCallState == AinCallStates.MS_PLAYCOLLECT) {
						event = new Event(EventType.EVENT_PNC_FAILURE, Protocol.AIN_SCF, null);
					}
				}
			}
			if (event == null) {
				event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.AIN_SCF, legId);
				legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_DISCONNECTED);
			}
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in Resource clear.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in Resource clear.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.RES_CLR);
		} catch (Exception ex) {
			logger.error(dialogId + " Exception in ResourceClear" + ex);
		}
		return null;
	}

	public static Action[] processClose(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processClose");
		}

		try {
			AinScfProtocolParser.parseClose(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in processClose.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in processClose.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.CLOSE);
		}
		return null;

	}

	public static Action[] processInfoCollected(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processInfoCollected Enter ");
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: [PH] Inside processInfoCollected setting service state");
		}
		/*
		 * change state to service logic
		 */

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);

		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke INFO_COLLECT::" + invokeId);
		}

		/*
		 * parse IDP
		 */
		try {
			AinScfProtocolParser.parseInfoCollected(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.INFO_COLLECT);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in INFO_COLLECT.", ape);

			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.INFO_COLLECT_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.INFO_COLLECT_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in INFO_COLLECT.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INFO_COLLECT_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in INFO_COLLECT.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INFO_COLLECT_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.INFO_COLLECT);
		}
		return null;
	}

	public static Action[] processBNSQuery(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processBnsQuery Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke BNS_Query::" + invokeId);
		}

		/*
		 * parse BNS QUery
		 */
		try {
			AinScfProtocolParser.parseBnsQuery(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.BNS_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.BNS_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);

		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in BNS_QUERY.");
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.BNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.BNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in BNS_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.BNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in BNS Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.BNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.BNS_QUERY);
		}
		return null;
	}

	public static Action[] processGNQuery(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processGNQuery Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke GN_Query::" + invokeId);
		}

		/*
		 * parse GN Query
		 */
		try {
			AinScfProtocolParser.parseGnQuery(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.GN_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.GN_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in GN_QUERY.", ape);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.GN_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.GN_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in GN_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.GN_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in GN Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.GN_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.GN_QUERY);
		}
		return null;
	}

	public static Action[] processOLNSQuery(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processOLNSQuery Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke OLNS_Query::" + invokeId);
		}

		/*
		 * parse OLNS Query
		 */
		try {
			AinScfProtocolParser.parseOlnsQuery(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.OLNS_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.OLNS_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in OLNS_QUERY.", ape);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in OLNS_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in GN Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.OLNS_QUERY);
		}
		return null;
	}

	/**
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 */
	public static Action[] processOAnswer(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processOAnswer");
		}

		try {
			AinScfProtocolParser.parseAns(callData, invokeIndEvent, tcapSession);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in processOAnswer.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in processOAnswer.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ERB_ANS);
		}
		return null;
	}

	public static Action[] processODisconnect(InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws InvalidInputException, EnumParamOutOfRangeException {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processODisconnect");
		}

		try {
			AinScfProtocolParser.parseDisconnect(callData, invokeIndEvent, tcapSession);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in processODisconnect.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in processODisconnect.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ERB_DISCONNECT);
		}
		return null;
	}

	/**
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws EnumParamOutOfRangeException
	 * @throws InvalidInputException
	 */
	public static Action[] processAbandon(InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws InvalidInputException, EnumParamOutOfRangeException {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processAbandon");
		}

		try {
			AinScfProtocolParser.parseAbandon(callData, invokeIndEvent, tcapSession);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in processAbandon.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in processAbandon.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ERB_ABANDON);
		}
		return null;
	}

	/**
	 * This method is used to process busy ERB event
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 */
	public static Action[] processOCalledBusy(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processOCalledBusy");
		}

		try {
			AinScfProtocolParser.parseBusy(callData, invokeIndEvent, tcapSession);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in processOCalledBusy.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in processOCalledBusy.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ERB_DISCONNECT);
		}
		return null;
	}

	/**
	 * This method is used to process No Answer ERB event
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 */
	public static Action[] processONoAnswer(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processONoAnswer");
		}

		try {
			AinScfProtocolParser.parseNoAns(callData, invokeIndEvent, tcapSession);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in processONoAnswer.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in processONoAnswer.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ERB_NOANS);
		}
		return null;
	}

	/**
	 * This method is used to process Term Seized ERB event
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 */
	public static Action[] processOTermSeized(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processOTermSeized");
		}

		try {
			AinScfProtocolParser.parseTermSiezed(callData, invokeIndEvent, tcapSession);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in processOTermSeized.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in processOTermSeized.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ERB_TERMSIEZED);
		}

		return null;
	}

	/**
	 * This method is called to Authorize Attempt.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action.
	 * @throws Exception
	 */
	private static void sendAuthorizeTermination(TcapSession tcapSession, Action action) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendAuthorizeTermination");
		}

		byte[] authTermCall = AinScfProtocolParser.createAuthorizeTermination(callData, action);

		byte[] authTermOpCode = CommonUtils.formatIntToByte(AinOpCodes.AUTHORIZED_TERMINATION_BYTE);

		Operation authTermOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, authTermOpCode);

		InvokeReqEvent authTermInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(),
				authTermOperation);
		authTermInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		authTermInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, authTermCall));
		authTermInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		authTermInvokeReqEvent.setLastInvokeEvent(true);
		authTermInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(authTermInvokeReqEvent, callData, cCallTraceService);

		// Check if SendNotification to be sent. It shall be sent based flag 
		// it can be sent in both cQP and Response scenarios
		if (leg2Data.get(LegDataAttributes.NP_AIN_SEND_NOTIFICATION) != null){
			sendSendNotification(tcapSession, action, true);
		}

		if (action.getSendMode() == Action.SEND_MODE.END) {

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
			preProcessDroppedCall(tcapSession);

			sendEndRequestEvent(tcapSession, false, cCallTraceService);

			postProcessDroppedCall(tcapSession, true);
		} else {
			sendContinueRequestEvent(tcapSession, cCallTraceService);
		}
	}

	/**
	 * Method to send Continue message to switch
	 * 
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	private static void sendContinue(TcapSession tcapSession, Action action) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendContinue");
		}

		byte[] continueBuffer = AinScfProtocolParser.createContinue(callData, action);

		byte[] continueOpCode = CommonUtils.formatIntToByte(AinOpCodes.CONTINUE_AIN_BYTE);

		Operation authTermOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, continueOpCode);

		InvokeReqEvent continueInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(),
				authTermOperation);
		continueInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		continueInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, continueBuffer));
		continueInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		continueInvokeReqEvent.setLastInvokeEvent(true);
		continueInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(continueInvokeReqEvent, callData, cCallTraceService);

		// Check if SendNotification to be sent. It shall be sent based flag 
		// it can be sent in both cQP and Response scenarios
		if (leg2Data.get(LegDataAttributes.NP_AIN_SEND_NOTIFICATION) != null){
			sendSendNotification(tcapSession, action, true);
		}

		if (action.getSendMode() == Action.SEND_MODE.END) {

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
			preProcessDroppedCall(tcapSession);

			sendEndRequestEvent(tcapSession, false, cCallTraceService);

			postProcessDroppedCall(tcapSession, true);
		} else {
			sendContinueRequestEvent(tcapSession, cCallTraceService);
		}
	}

	/**
	 * Method to send LIDB Query
	 * 
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	private static void sendLidbResponse(TcapSession tcapSession, Action action) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendLidbResponse");
		}
		byte[] continueBuffer = null;
		String messageType = "";
		boolean isAcgParamPresent = false;
		String sendAcgFlag = null;

		if (MESSAGE.BNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			continueBuffer = BNSQuery.encodeBnsQuery(callData);
			messageType = "BNS";
		} else if (MESSAGE.GN_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
//			if (legData.get(LegDataAttributes.P_GN_IS_ACG_PARAM_PRESENT) != null) {
//				//isAcgParamPresent = (Boolean) legData.get(LegDataAttributes.P_GN_IS_ACG_PARAM_PRESENT);
//				isAcgParamPresent = true;
//			}
			if(leg2Data.get(LegDataAttributes.SEND_ACG) != null){
				sendAcgFlag = (String) leg2Data.get(LegDataAttributes.SEND_ACG);
				if (sendAcgFlag != null && !sendAcgFlag.isEmpty()) {
					isAcgParamPresent = true;
					if (logger.isDebugEnabled()) {
						logger.debug("ACG send flag is set, Acg gap duration " + LegDataAttributes.ACG_GAP_DURATION + ", Acg interval " + leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
					}
				}
			}
			
			continueBuffer = GenericNameQuery.encodeGenericName(callData);
			messageType = "GN";
			// ACG should be set by application if we need to send
			//isAcgParamPresent = true;
		} else if (MESSAGE.OLNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			continueBuffer = OLNSQuery.encodeOLNSQuery(callData);
			messageType = "OLNS";
		} else if (MESSAGE.GET_DATA_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			continueBuffer = GetDataQuery.encodeGetDataQuery(callData);
			messageType = "GET_DATA";
		} else if (MESSAGE.CC1_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			continueBuffer = CC1Query.encodeCc1Query(callData);
			messageType = "CC1";
		}else if (MESSAGE.CC2_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			continueBuffer = CC2Query.encodeCc2Query(callData);
			messageType = "CC2";
		}else if (MESSAGE.TLNS_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			continueBuffer = TLNSQuery.encodeTlnssQuery(callData);
			messageType = "TLNS";
		}else if (MESSAGE.ICDC_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			continueBuffer = ICDCQuery.encodeIcdcsQuery(callData);
			messageType = "ICDC";
		}else if (MESSAGE.INTERCEPT_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			continueBuffer = InterceptQuery.encodeInterceptsQuery(callData);
			messageType = "INTERCEPT";
		}else if (MESSAGE.ACCOUNT_CODE_QUERY == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
				continueBuffer = AccountCode.encodeAccountCodeResponse(callData);
				messageType = "AccountCode";
		} else {
			logger.error(dialogId + "Invalid LIDB Last Operation State");
		}

		if (logger.isDebugEnabled()) {
			logger.debug("LIDB Response Encoded for " + messageType + "::" + CommonUtils.formatBytes(continueBuffer));
		}

		byte[] continueOpCode = CommonUtils.formatIntToByte(AinOpCodes.DISCONNECT_BYTE);

		Operation authTermOperation = new Operation(Operation.OPERATIONTYPE_GLOBAL, continueOpCode);
		String CD_PTY_NO_PC =(String) leg2Data.get(LegDataAttributes.CD_PTY_NO_PC);
		String CG_PTY_NO_PC = (String) leg2Data.get(LegDataAttributes.CG_PTY_NO_PC);
		
		SccpUserAddress suaorig=(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS);

		SccpUserAddress suaDest=(SccpUserAddress) legData.get(LegDataAttributes.P_SUA);
		if(StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"1")) {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to true in in orig addr endReqEvent");
			}
			suaDest.setPtyNoPC(true);
			
		}else {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to false in orig addr endReqEvent");
			}
			suaDest.setPtyNoPC(false);
		}
		if(StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"1")) {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to true in dest addr endReqEvent");
			}
			suaorig.setPtyNoPC(true);
		}else {
			if(logger.isDebugEnabled()) {
				logger.debug("Setting PtyNoPc to false in dest addr endReqEvent");
			}
			suaorig.setPtyNoPC(false);
		}
		
//		String mrDigits=(String) leg2Data.get(LegDataAttributes.P_MR_GT_DIGITS);
//		  final String destTT = (String)leg2Data.get(LegDataAttributes.P_MR_TT);
//	        byte destbyte = -1;
//
//		if(PhConstants.TRUE.equals(leg2Data.get(LegDataAttributes.P_SEND_SUA_AS_RECEIVED))&&destTT != null) {
//			final Integer destt = Integer.parseInt(destTT);
//			destbyte = destt.byteValue();
//			GlobalTitle gt = new GlobalTitle();
//
//			if (suaorig.isGlobalTitlePresent()) {
//				gt = updateGlobalTitle(suaorig, destbyte);
//			} else if (logger.isDebugEnabled()) {
//				logger.debug((dialogId + " [PH]:: Globle Title not Present in P_SCCP_REMOTE_USER_ADDRESS "));
//			}
//			suaorig.setGlobalTitle(gt);
//			if (gt != null && mrDigits != null && !"-1".equals(mrDigits)) {
//
//				if (logger.isDebugEnabled()) {
//					logger.debug(dialogId
//							+ " [PH]:: set GT digits in address information "
//							+ mrDigits);
//				}
//
//				byte[] encodedGtd = null;
//				if (StringUtils.isNotBlank(mrDigits)) {
//					encodedGtd = AinScfProtocolFieldCodec
//							.encodeDigitsInBcdFormat(mrDigits);
//
//					if (logger.isDebugEnabled()) {
//						logger.debug(dialogId + " [PH]:: set GT digits byte[] "
//								+ encodedGtd);
//					}
//				}
//				gt.setAddressInformation(encodedGtd);
//			} else {
//				if (logger.isDebugEnabled()) {
//					logger.debug(dialogId
//							+ " [PH]:: not setting GT digits as GT not set in orig SUA");
//				}
//			}

     
		ResultReqEvent resultRequestEvent = new ResultReqEvent(src, dialogId, true);// new BeginReqEvent(src, tcapSession.getDialogueId(),suaDest , suaorig);
		resultRequestEvent.setOperation(authTermOperation);
		resultRequestEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, continueBuffer));
		logger.info("lastReceivedInvokeId ::" + AinScfProtocolUtil.getLastRxInvokeId(callData));
		resultRequestEvent.setInvokeId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		resultRequestEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(resultRequestEvent, callData, cCallTraceService);

		if (isAcgParamPresent) {
			logger.info("sendLidbResponse : ACG parameters are present. sending component request");
			sendAcgParameterComponentReqEvent(tcapSession, action);
		}else{
			logger.info("sendLidbResponse : ACG parameters not present");
		}

		// LIDB Response shall always go as RESPONSE irrespective of inputs from
		// application
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
		preProcessDroppedCall(tcapSession);

		if(PhConstants.TRUE.equals(leg2Data.get(LegDataAttributes.P_SEND_SUA_AS_RECEIVED))){
		    if (logger.isDebugEnabled()) {
				logger.debug((dialogId + " [PH]:: Sending same SUA "));
			}
		    sendEndRequestEvent(tcapSession, false, cCallTraceService,suaorig,suaDest);
		}else{
			sendEndRequestEvent(tcapSession, false, cCallTraceService);
		}

		postProcessDroppedCall(tcapSession, true);
	   
	}

	/**
	 * Method to send ACG for LIDB Query
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	private static void sendAcgParameterComponentReqEvent(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendAcgParameterComponentReqEvent");
		}

		byte[] acgBuffer = null;
		acgBuffer = GenericNameQuery.encodeAcgParameters(callData);
		byte[] acgOpCode = CommonUtils.formatIntToByte(AinOpCodes.GN_ACG_BYTE);

		Operation authTermOperation = new Operation(Operation.OPERATIONTYPE_GLOBAL, acgOpCode);

		InvokeReqEvent acgInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), authTermOperation);
		logger.info("getNextInvokeId ::" + AinScfProtocolUtil.getNextInvokeId(callData));
		acgInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		acgInvokeReqEvent.setLastInvokeEvent(true);
		acgInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, acgBuffer));
		acgInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		sendComponentReq(acgInvokeReqEvent, callData, cCallTraceService);
	}

	private static void incrementTcapCounters(EventObject eventObject) {
		MeasurementCounter measurementCounter = PhMeasurementService.getInstance()
				.getMeasurementCounter(Protocol.AIN_SCF);
		if (measurementCounter == null) {
			return;
		}

		int primitiveType = -1;
		if (eventObject instanceof DialogueReqEvent) {
			primitiveType = ((DialogueReqEvent) eventObject).getPrimitiveType();
		} else if (eventObject instanceof ComponentReqEvent) {
			primitiveType = ((ComponentReqEvent) eventObject).getPrimitiveType();
		}

		SS7Message ss7Message = SS7Message.valueOf(primitiveType);

		switch (ss7Message) {

		case PRIMITIVE_BEGIN: {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_BEGIN, false);
			break;
		}

		case PRIMITIVE_END: {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_END, false);
			break;
		}

		case PRIMITIVE_USER_ABORT: {
			measurementCounter.incrementSS7MessageCount(SS7Message.PRIMITIVE_USER_ABORT, false);
			break;
		}
		}
	}

	/**
	 * This method is for sending SEND NOTIFICATION component
	 * 
	 * @param tcapSession  represents the instance of TcapSession
	 * @param action       represents the instance of Action
	 * @param isInvokeLast
	 * @throws Exception
	 */
	private static void sendSendNotification(TcapSession tcapSession, Action action, boolean isLastInvoke) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		String dialogId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendSendNotification");
		}

		byte[] sendNotif = AinScfProtocolParser.createSendNotification(callData, action);
		byte[] sendNotifOpCode = CommonUtils.formatIntToByte(AinOpCodes.SEND_NOTIFICATION_BYTE);

		Operation rrbeOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, sendNotifOpCode);

		InvokeReqEvent sendNotifInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rrbeOperation);
		sendNotifInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		sendNotifInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, sendNotif));
		sendNotifInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		sendNotifInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		if(isLastInvoke){
			sendNotifInvokeReqEvent.setLastInvokeEvent(true);
		}else{
			sendNotifInvokeReqEvent.setLastInvokeEvent(false);
		}

		sendComponentReq(sendNotifInvokeReqEvent, callData, cCallTraceService);
	}

	/**
	 * This method parse termination Notification
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param origCallDataForCall
	 * @return
	 */
	public static CallData processTerminationNotification(InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws Exception {

		CallData origCallData = null;
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(" [PH]:: Inside processTerminationNotification");
		}

		try {
			origCallData = AinScfProtocolParser.parseTerminationNotification(invokeIndEvent, callData);

			if (logger.isDebugEnabled()) {
				logger.debug("processTerminationNotification: Call Data specific to echoData is "
						+ ((origCallData == null) ? " null" : " not null"));
			}

		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in TerminationNotification.", ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in TerminationNotification.", cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in TerminationNotification.", pore);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return origCallData;
	}

	/**
	 * This method performs the processing of Unidirectional dialogue indication
	 * event. No response is sent to network for Unidirectional messages
	 * 
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @throws MandatoryParameterNotSetException
	 */
	static void processUnidirectional(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession)
			throws MandatoryParameterNotSetException {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] processUnidirectional Enter ");
		}

		/*
		 * call data can't be null if it has passed validation method so moving ahead
		 * without null check store dialog
		 */

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: [PH] Set Dialogue Portion in call data");
			}

			try {
				/*
				 * Store Dialogue Portion in Leg Data.
				 */
				legData.set(LegDataAttributes.P_DIALOG_PORTION, dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn(dialogId + ":: [PH] Error getting dialogue portion " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn(dialogId + ":: [PH] Error getting dialog portion.", e);
					logger.info(dialogId + "::[PH] IGNORE ParameterNotSetException fetching "
							+ "dialoguePortion from BEGIN dialogue event");
				}
			}
		}
		UnidirectionalIndEvent uniIndEvent = (UnidirectionalIndEvent) dialogueIndEvent;

		try {
			SccpUserAddress sccpUserAddress = uniIndEvent.getOriginatingAddress();
			SignalingPointCode signalingPointCode = sccpUserAddress.getSubSystemAddress().getSignalingPointCode();
			int zone = signalingPointCode.getZone();
			int cluster = signalingPointCode.getCluster();
			int member = signalingPointCode.getMember();
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] Origin zone= " + zone + " cluster=" + cluster + " member=" + member);
			}

			String zcmFormat = zone + "-" + cluster + "-" + member;

			String pcBitStr = lPad(Integer.toBinaryString(zone), 8) + lPad(Integer.toBinaryString(cluster), 8)
			+ lPad(Integer.toBinaryString(member), 8);
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] pcBitStr =" + pcBitStr);
			}
			int pc = Integer.parseInt(pcBitStr, 2);
			legData.set(LegDataAttributes.P_OPC, pc);
			legData.set(LegDataAttributes.P_SPC, zcmFormat);
			callData.set(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS, uniIndEvent.getDestinationAddress());
			callData.set(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS, uniIndEvent.getOriginatingAddress());
			if (logger.isInfoEnabled()) {
				logger.info(dialogId + " :: [PH] Calculated Origin Point Code form IDP-Begin ="
						+ legData.get(LegDataAttributes.P_OPC));
			}

		} catch (ParameterNotSetException e1) {
			logger.error(dialogId + " :: [PH] Failed to get origin point code from Dialog Indication event. "
					+ e1.getMessage());
		}

		/*
		 * Use the sccp address received in IDP to support multiple pc-ssn
		 */
		legData.set(LegDataAttributes.P_SUA, uniIndEvent.getDestinationAddress());
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: processUnidirectional Exit ");
		}
	}

	/**
	 * 
	 * @param tacpSession
	 * @param callData
	 * @throws Exception 
	 */

	public static void sendSipInviteFrmAIN(TcapSession tacpSession, CallData callData,Action action) throws Exception {
		if(logger.isDebugEnabled()){
			logger.debug("Enter sendSipInviteFrmAIN");
		}
		boolean outboundGwNotFound = true;

		try {
			createSipInviteFrmAIN(tacpSession, callData);
			outboundGwNotFound = false;
			//	callData.set(CallDataAttribute.P_DIALOUT, false);
		} catch (ServletParseException e) {
			logger.error("ServletParseException inside sendSipInviteFrmAIN:: " + e);
		} catch (IOException e) {
			logger.error("IOException inside sendSipInviteFrmAIN:: " + e.getMessage());
		} catch (ServletException e) {
			logger.error("ServletException inside sendSipInviteFrmAIN:: " + e);
		}

		if(outboundGwNotFound) {
			Action lastAction = (Action) callData.get(CallDataAttribute.P_LAST_CALL_ACTION);
			String legId = lastAction.getLeg();

			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
			legData.set(LegDataAttributes.P_CAUSE_CODE, AinScfRelReasonCode.OBGW_NOT_AVAILABLE);

			logger.error(tacpSession.getDialogueId() + ": OutboundGateway not found");
			Event event = new Event(EventType.EVENT_GW_NOT_FOUND, Protocol.AIN_SCF, legId);
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
		}
	}
	/**
	 * when Info Analyzed come to cas then mph will create a Invite request and send
	 * PSX . this method perform to send a INVITE request
	 * 
	 * @param tcapsesion
	 * @param calldata
	 * @throws IOException
	 * @throws ServletException
	 */
	@SuppressWarnings("deprecation")
	private static void createSipInviteFrmAIN(TcapSession tcapsesion, CallData calldata)
			throws IOException, ServletException {
		if(logger.isDebugEnabled()){
			logger.debug("Inside createSipInviteFrmAIN::enter ");
		}
		String origLegCallId = (String) calldata
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legdata1 = (LegData) calldata.get(CallDataAttribute.P_LEG1);
		
		if(legdata1.get(LegDataAttributes.P_LIDB_QUERY_TYPE)==null){
		  legdata1.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.PSX_ROUTING);
		}
		
		legdata1.set(LegDataAttributes.P_LEG_SIP_STATE ,State.CONN_IN_PROGRESS);
		if(logger.isDebugEnabled()){
			logger.debug("LegData1 Inside createSipInviteFrmAIN is:: " + legdata1);
		}
		LegData legdata2 = (LegData) calldata.get(CallDataAttribute.P_LEG2);

		if(logger.isDebugEnabled()){
			logger.debug("LegData2 Inside createSipInviteFrmAIN is:: " + legdata2);
		}
		String ip =null;
		int port =5060;
		String eqspoolId = (String)legdata2.get(LegDataAttributes.NP_GW_POOL_ID);
		if(logger.isDebugEnabled()){
			logger.debug("NP_GW_POOL_ID in createSipInviteFrmAIN::"+eqspoolId);
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

		String calleruser=null;

		if(caller!=null){
			calleruser=caller.getAddress();
		}

		if (SipProtocolConfig.getConfigData(SipProtocolConfig.FLOATING_IP) != null
				&& SipProtocolConfig.getConfigData(SipProtocolConfig.SIP_PORT) != null) {
			if(logger.isDebugEnabled()){
				logger.debug("Creating Caller URI using ::");
			}
			if(calleruser!=null){
				callerURI = factory.createURI(

						"sip:" + ((caller != null)?caller.getAddress():"Anonymous") + "@" + SipProtocolConfig.getConfigData(SipProtocolConfig.FLOATING_IP)
						+ ":" + SipProtocolConfig.getConfigData(SipProtocolConfig.SIP_PORT));
			}else{
				callerURI = factory.createURI(
						"sip:anonymous@"+SipProtocolConfig.getConfigData(SipProtocolConfig.FLOATING_IP)
						+ ":" + SipProtocolConfig.getConfigData(SipProtocolConfig.SIP_PORT));
			}
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

		SipApplicationSession tcapApplicationSession = AinScfProtocolUtil.getAppSession(tcapsesion);

		sipApplicationSession.setAttribute(CallData.CALL_DATA, calldata);
		sipApplicationSession.setAttribute(PhConstants.TCAP_SESSION_ID,
				tcapApplicationSession.getAttribute(PhConstants.TCAP_SESSION_ID));

		if(logger.isDebugEnabled()){
			logger.debug("Created new sip app session as:: " + sipApplicationSession);
		}

		// sipApplicationSession.setAttribute("TCAP_SESSION", tcapsesion);
		// sipApplicationSession.setAttribute("CALL_STATE", "CALL_BEING_SETUP");
		// sipApplicationSession.setAttribute("CALLER_URI", callerURI);
		// sipApplicationSession.setAttribute("CALLEE_URI", calledUri);

		//		String appname1 = sipApplicationSession.getApplicationName();

		tcapsesion.setAttribute(CallData.CALL_DATA, calldata);

		SipServletRequest sipRequest = factory.createRequest(sipApplicationSession, "INVITE", callerURI, calledUri);
		ServiceInterface srviceHandler = PhUtilityServices
				.getInstance((String) calldata.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		// String appName =
		// PhUtilityServices.getInstance((String)calldata.get(CallDataAttribute.SERVICE_ID)).getServiceHandler().getApplicationName();
		// logger.info("the sip request created for appliaction "+appName);
		if(logger.isDebugEnabled()){
			logger.debug("srviceHandler: " + srviceHandler);
		}

		SipSession legSipSession = sipRequest.getSession();
		// legSipSession.setAttribute("TCAP_SESSION", tcapsesion);

		legdata1.set(LegDataAttributes.P_SESSION_ID, legSipSession.getId());
		legSipSession.setAttribute(PhConstants.LEG_ID, CallDataAttribute.P_LEG1.name());
		
		SipProtocolUtil.addCustomHeaders(calldata, legdata2,
				sipRequest);
		if(logger.isDebugEnabled()){
			logger.debug("tcap-session at the time of creating Invite msg: " + tcapsesion);
		}

		// SipApplicationSession appses = sipRequest.getApplicationSession();
		// logger.info("app-session at the created msg: "+appses);
		if(logger.isDebugEnabled()){
			logger.debug("legSipSession: " + legSipSession);
		}

		String disableProxy=SipProtocolConfig.getConfigData(SipProtocolConfig.DISABLE_OUTBOUND_PROXY);
		
		
		  if(PhConstants.TRUE.equalsIgnoreCase(disableProxy)){
			  if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + ":: DISABLE_OUTBOUND_PROXY ");
				}
			  sipRequest.getSession().setAttribute("DISABLE_OUTBOUND_PROXY", true);
	      }

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_PSX_ROUTING);
		action.setProtocol(Protocol.AIN_SCF);
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
			logger.debug("Inside createSipInviteFrmAIN::exit");
		}
	}

	/**
	 * method use for process Call Info from Resource
	 * 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 */
	public static Action[] processCallInfoFrmResrc(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AinCallStates curCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		Event event = null;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Inside processCallInfoFrmResrc in state:" + curCallState);
		}
		try {
			AinScfProtocolParser.parseCallInfoFrmResource(invokeIndEvent, callData);

			// releaseCause = normal && current state is MS_PLAY then event as
			// MS_PLAY_SUCCESS
			// releaseCause != normal && current state is Play then event MS_PLAY_FAILURE
			// releaseCause = normal && current state is MS_PLAYCOLLECT then event
			// MS_PLAYCOLLECT_SUCCESS
			// releaseCause != normal && current state is MS_PLAYCOLLECT then even
			// MS_PLAYCOLELCT_FAILURE

			//			Integer causeCode = (Integer) legData.get(LegDataAttributes.P_RC_CAUSE);
			Integer causeCode = 0;
			Action lastAction = (Action) callData.get(CallDataAttribute.P_LAST_CALL_ACTION);
			String legId = lastAction.getLeg();
			switch (causeCode) {
			// normal - GR1299 sec 6.47
			case 0: {
				if (curCallState == AinCallStates.MS_PLAY || curCallState == AinCallStates.MS_PLAY_CIFR) {
					event = new Event(EventType.EVENT_PLAY_SUCCESS, Protocol.AIN_SCF, null);
				} else if (curCallState == AinCallStates.MS_PLAYCOLLECT
						|| curCallState == AinCallStates.MS_PLAY_COLLECT_CIFR) {

					event = new Event(EventType.EVENT_PNC_SUCCESS, Protocol.AIN_SCF, legId);
				}
				break;
			}
			// timeout
			case 2:
			default:
				if (curCallState == AinCallStates.MS_PLAY || curCallState == AinCallStates.MS_PLAY_CIFR) {
					event = new Event(EventType.EVENT_PLAY_FAILURE, Protocol.AIN_SCF, null);
				} else if (curCallState == AinCallStates.MS_PLAYCOLLECT
						|| curCallState == AinCallStates.MS_PLAY_COLLECT_CIFR) {
					event = new Event(EventType.EVENT_PNC_FAILURE, Protocol.AIN_SCF, null);
				}
			}

			if (event != null) {

				// state change based on CIFR
				if (curCallState == AinCallStates.MS_PLAY) {
					legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_PLAY_CIFR);
				} else if (curCallState == AinCallStates.MS_PLAYCOLLECT) {
					legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_PLAY_COLLECT_CIFR);
				}
				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			} else {
				// this condition should not happen. Application needs
				// to be notified
				logger.error(dialogId + " Call Info From Resource - not sending any event to app");
			}
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in Call Info From Resource.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: ParameterOutOfRangeException in Call Info From Resource.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.RC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.CALL_INFO_FRM_RESRC);
		} catch (Exception ex) {
			logger.error(dialogId + " Exception in Call Info From Resource" + ex);
		}
		return null;

	}

	/**
	 * method use to handle play message as a return result of CIFR or normal STR.
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void handlePlayAnnouncement(TcapSession tcapSession, CallData callData, Action action)
			throws Exception {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Enter handlePlayAnnouncement()");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AinCallStates curCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		Event event = null;
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " ::[ph] handlePlayAnnouncement() with AinCallStates:: " + curCallState);
		}

		// state if MS_PLAY_CIFR or MS_PLAYCOLLECT_CIFR
		if (curCallState == AinCallStates.MS_PLAY_CIFR || curCallState == AinCallStates.MS_PLAY_COLLECT_CIFR) {
			sendPlayAnnouncementCITR(tcapSession, callData, action);
		} else if (callData.get(CallDataAttribute.P_LAST_OPERATION_RX) != null && MESSAGE.PROVIDE_INSTRUCTION == callData.get(CallDataAttribute.P_LAST_OPERATION_RX)) {
			String isTR533Enabled = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.TR533_ENABLED);
			if (PhConstants.TRUE.equals(isTR533Enabled)) {
				TR533MessageHandler.sendTR533PlayAnnouncement(tcapSession, callData, action);;
			}else{
				Gr533MessageHandler.sendGr533STR(tcapSession, callData,action);
			}
		}
		else {
			sendPlayAnnouncement(tcapSession, callData, action);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Exit handlePlayAnnouncement()");
		}
	}

	/**
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void handlePlayAndCollect(TcapSession tcapSession, CallData callData, Action action)
			throws Exception {
		int dialogId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Enter handlePlayAndCollect()");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AinCallStates curCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		Event event = null;
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " ::[ph] handlePlayAndCollect() with AinCallStates:: " + curCallState);
		}

		// state if MS_PLAY_CIFR or MS_PLAYCOLLECT_CIFR
		if (curCallState == AinCallStates.MS_PLAY_CIFR || curCallState == AinCallStates.MS_PLAY_COLLECT_CIFR) {
			sendPlayAndCollectCITR(tcapSession, callData, action);
		} else {
			sendPlayAndCollect(tcapSession, callData, action);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: Exit handlePlayAndCollect()");
		}
	}

	/**
	 * --
	 * 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendPlayAnnouncementCITR(TcapSession tcapSession, CallData callData, Action action)
			throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendPlayAnnouncementCITR");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC);

		if (annSpec == null) {
			logger.error(dialogId + ":: No announmcement spec specified by Service");
			throw new Exception(dialogId + ":: :: No announmcement spec specified by Service");
		}

		byte[] citr = null;
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		/**
		 * create CITR ...Depending on condition if flex flag true then we will create
		 * CITR for Flex Play Announcement. else we will create Normal CITR for Play.
		 */
		try {
			if (annSpec.isFlex()) {
				if (logger.isDebugEnabled()) {
					logger.debug(
							dialogId + "::Going to create CITR[call info to resource] for FlexParameterBlock choice");
				}
				citr = AinScfProtocolParser.createCITRForPlayFlex(callData);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(
							dialogId + "::Going to createCITR[call info to resource] for AnnouncementBlock Choice");
				}
				citr = AinScfProtocolParser.createCITRForPlay(callData);
			}

			if (logger.isDebugEnabled()) {
				logger.info("Buffer for call Info To Resource created successfully ");
			}

			// set state
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_PLAY_CIFR);

			byte[] citrOpCode = CommonUtils.formatIntToByte(AinOpCodes.CALL_INFO_FROM_RESOURCE_BYTE);
			Operation citroprn = new Operation(Operation.OPERATIONTYPE_LOCAL, citrOpCode);

			ResultReqEvent citrResultReqEvent = new ResultReqEvent(src, dialogId, false);
			citrResultReqEvent.setOperation(citroprn);
			citrResultReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, citr));
			citrResultReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
			citrResultReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

			sendComponentReq(citrResultReqEvent, callData, cCallTraceService);

			if (action.getSendMode() == Action.SEND_MODE.END) {

				// set the release code as
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_DISCONNECT_AFTER_PLAY);
				preProcessDroppedCall(tcapSession);

				sendEndRequestEvent(tcapSession, false, cCallTraceService);

				postProcessDroppedCall(tcapSession, true);
			} else {
				sendContinueRequestEvent(tcapSession, cCallTraceService);
			}
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exiting sendPlayAnnouncementCITR(), sent as:" + action.getSendMode());
			}

		} catch (Exception e) {
			logger.error(dialogId + "::Exception Inside sendPlayAnnouncementCITR:" + e);
		}

	}

	public static void sendPlayAndCollectCITR(TcapSession tcapSession, CallData callData, Action action)
			throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendPlayAndCollectCITR");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC);

		if (annSpec == null) {
			logger.error(dialogId + ":: No announmcement spec specified by Service");
			throw new Exception(dialogId + ":: :: No announmcement spec specified by Service");
		}

		byte[] citr = null;
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		/**
		 * create CITR ...Depending on condition if flex flag true then we will create
		 * CITR for Flex Play Announcement. else we will create Normal CITR for Play.
		 */
		try {
			if (annSpec.isFlex()) {
				if (logger.isDebugEnabled()) {
					logger.debug(
							dialogId + "::Going to create CITR[call info to resource] for FlexParameterBlock choice");
				}
				citr = AinScfProtocolParser.createCITRForPlayFlex(callData);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(
							dialogId + "::Going to createCITR[call info to resource] for AnnouncementBlock Choice");
				}
				citr = AinScfProtocolParser.createCITRForPlay(callData);
			}

			if (logger.isDebugEnabled()) {
				logger.info("Buffer for call Info To Resource created successfully ");
			}

			// set state
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_PLAY_COLLECT_CIFR);
			byte[] citrOpCode = CommonUtils.formatIntToByte(AinOpCodes.CALL_INFO_FROM_RESOURCE_BYTE);
			Operation citroprn = new Operation(Operation.OPERATIONTYPE_LOCAL, citrOpCode);

			ResultReqEvent citrResultReqEvent = new ResultReqEvent(src, dialogId, false);
			citrResultReqEvent.setOperation(citroprn);
			citrResultReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, citr));
			citrResultReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
			citrResultReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

			sendComponentReq(citrResultReqEvent, callData, cCallTraceService);

			if (action.getSendMode() == Action.SEND_MODE.END) {

				// set the release code as
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_DISCONNECT_AFTER_PLAY);
				preProcessDroppedCall(tcapSession);

				sendEndRequestEvent(tcapSession, false, cCallTraceService);

				postProcessDroppedCall(tcapSession, true);
			} else {
				sendContinueRequestEvent(tcapSession, cCallTraceService);
			}
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exiting sendPlayAndCollectCITR(), sent as:" + action.getSendMode());
			}

		} catch (Exception e) {
			logger.error(dialogId + "::Exception Inside sendPlayAnnouncementCITR:" + e);
		}
	}

	/**
	 * Helper method to send empty CITR.
	 * 
	 * @param callData
	 * @throws Exception
	 */
	public static void sendDisconnectCITR(TcapSession tcapSession, CallData callData, Action action) throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside sendDisconnectCITR");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		// creating empty CITR
		byte[] citrBuffer = AinScfProtocolParser.createDisconnectCITR(callData);

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.MS_DISCONNECT_INPROGRESS);

		byte[] citrOpCode = CommonUtils.formatIntToByte(AinOpCodes.CALL_INFO_FROM_RESOURCE_BYTE);
		Operation citroprn = new Operation(Operation.OPERATIONTYPE_LOCAL, citrOpCode);

		ResultReqEvent citrResultReqEvent = new ResultReqEvent(src, dialogId, true);
		citrResultReqEvent.setOperation(citroprn);
		citrResultReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, citrBuffer));
		citrResultReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		citrResultReqEvent.setLinkId(AinScfProtocolUtil.getLastRxInvokeId(callData));

		sendComponentReq(citrResultReqEvent, callData, cCallTraceService);

		// check the mode - END or CONTINUE - depending on mode send Response or
		// Continue
		if (action.getSendMode() == Action.SEND_MODE.END) {

			// set the release code as
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_DISCONNECT_AFTER_PLAY);
			preProcessDroppedCall(tcapSession);

			sendEndRequestEvent(tcapSession, false, cCallTraceService);

			postProcessDroppedCall(tcapSession, true);
		} else {
			sendContinueRequestEvent(tcapSession, cCallTraceService);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendDisconnectCITR(), sent as:" + action.getSendMode());
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
		AinScfProtocolUtil.startTimer(appSession, action.getTimerTimeInMills(),
				false, timerName);

		ServiceInterface serviceHandler = PhUtilityServices
				.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		Event appEvent = new Event(EventType.EVENT_APP_TIMER_STARTED, Protocol.AIN_SCF, action.getLeg());

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
		AinScfProtocolUtil.stopTimer(appSession, timerName);

		try{
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event appEvent = new Event(EventType.EVENT_APP_TIMER_STOPPED, Protocol.AIN_SCF, action.getLeg());

			ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
		}catch(Exception ex){
			logger.error("Error occured while stopping application timer : " + action.getTimerName() );
			logger.error("Error occured : " + ex);
			throw ex;
		}
	}

	/**
	 * Method to send ACG to SSP
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	private static void sendAcg(TcapSession tcapSession, Action action)
			throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendAcg");
		}

		AcgArg acgArg = new AcgArg();

		// create ControlCauseIndicator 
		// SCP Overload Control Ind - 0 No SCP Overload controls
		//                          - 1 SCP Overload controls 
		// SMS Init Contro Ind      - 0 No SMS initiated control 
		//                          - 1 SMS initiated controls 
		// Number of Digits to which control applied  - 1 t0 10 
		int scpOverloadControlInd = 0;
		int smsInitCtrlInd        = 1;
		int numOfDigits           = 10;
		if(leg2Data.get(LegDataAttributes.ACG_SCP_OVERLOAD_CTRL_IND)!= null){
			scpOverloadControlInd = Integer.parseInt((String)leg2Data.get(LegDataAttributes.ACG_SCP_OVERLOAD_CTRL_IND));
		}

		if(scpOverloadControlInd != 0 || scpOverloadControlInd != 1){
			scpOverloadControlInd = 0;
		}

		if(leg2Data.get(LegDataAttributes.ACG_SMS_INIT_CTRL_IND)!= null){
			smsInitCtrlInd = Integer.parseInt((String)leg2Data.get(LegDataAttributes.ACG_SMS_INIT_CTRL_IND));
		}

		if(smsInitCtrlInd != 0 || smsInitCtrlInd != 1){
			smsInitCtrlInd = 1;
		}

		if(leg2Data.get(LegDataAttributes.ACG_NUM_OF_DIGITS)!= null){
			numOfDigits = (Integer) leg2Data.get(LegDataAttributes.ACG_NUM_OF_DIGITS);
		}

		if(numOfDigits < 1 || numOfDigits > 10){
			numOfDigits = 10;
		}

		byte[] controlCauseIndicator = new byte[1];
		controlCauseIndicator[0]  = (byte) ((scpOverloadControlInd<<7) & 0x80);
		controlCauseIndicator[0] |= (byte)((smsInitCtrlInd<<6) & 0x40);
		controlCauseIndicator[0] |= (numOfDigits & 0x3F);

		ControlCauseIndicator cci = new ControlCauseIndicator();
		cci.setValue(controlCauseIndicator);

		// Set ControlCauseIndicator
		acgArg.setControlCauseIndicator(cci);

		GapDuration asnGapDur = new GapDuration();
		int gapDur = 10; // default value
		if(leg2Data.get(LegDataAttributes.ACG_GAP_DURATION)!= null){
			gapDur = Integer.parseInt((String)leg2Data.get(LegDataAttributes.ACG_GAP_DURATION));
		}

		GapDurationEnumType asnGapDurEnumType = new GapDurationEnumType();

		switch(gapDur){
		case 1:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no1Second);break;
		case 2:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no2Seconds);break;
		case 3:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no4Seconds);break;
		case 4:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no8Seconds);break;
		case 5:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no16Seconds);break;
		case 6:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no32Seconds);break;
		case 7:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no64Seconds);break;
		case 8:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no128Seconds);break;
		case 9:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no256Seconds);break;
		case 10:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no512Seconds);break;
		case 11:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no1024Seconds);break;
		case 12:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no2048Seconds);break;
		case 13:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.infinity);break;
		default:asnGapDurEnumType.setValue(GapDurationEnumType.EnumType.no512Seconds);break;
		}

		asnGapDur.setValue(asnGapDurEnumType);

		// Setting Gap Duration
		acgArg.setGapDuration(asnGapDur);

		// Gap Interval could be of either National or Private. 
		// Default is National
		GapInterval asnGapIntvl = new GapInterval();
		int gapIntervalType = 0; // 0 - National, 1 - Private

		if(leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_TYPE) != null){
			gapIntervalType = Integer.parseInt((String) leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_TYPE));
		}

		if(gapIntervalType !=0 && gapIntervalType != 1){
			gapIntervalType = 0;
		}

		int gapIntervalValue = 7;
		if(leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE) != null){
			gapIntervalValue = Integer.parseInt((String) leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
		}

		if(gapIntervalType == 0){
			// Handling for national
			NationalGapInterval asnNatGapIntvl = new NationalGapInterval();
			NationalGapIntervalEnumType asnNatGapIntvEnumType = new NationalGapIntervalEnumType();

			switch(gapIntervalValue){
			case 0:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.removeGapControl);break;
			case 1:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no0Seconds);break;
			case 2:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no010Seconds);break;
			case 3:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no025Seconds);break;
			case 4:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no050Seconds);break;
			case 5:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no1Second);break;
			case 6:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no2Seconds);break;
			case 7:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no5Seconds);break;
			case 8:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no10Seconds);break;
			case 9:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no15Seconds);break;
			case 10:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no30Seconds);break;
			case 11:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no60Seconds);break;
			case 12:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no120Seconds);break;
			case 13:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no300Seconds);break;
			case 14:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no600Seconds);break;
			case 15:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.stopAllCalls);break;
			default:asnNatGapIntvEnumType.setValue(NationalGapIntervalEnumType.EnumType.no5Seconds);break;
			}
			asnNatGapIntvl.setValue(asnNatGapIntvEnumType);
			asnGapIntvl.selectNationalGapInterval(asnNatGapIntvl);
		}else{
			// Handling for national
			PrivateGapInterval asnPvtGapIntvl = new PrivateGapInterval();
			PrivateGapIntervalEnumType asnPvtGapIntvEnumType = new PrivateGapIntervalEnumType();

			switch(gapIntervalValue){
			case 0:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no0Seconds);break;
			case 1:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no3Seconds);break;
			case 2:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no4Seconds);break;
			case 3:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no6Seconds);break;
			case 4:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no8Seconds);break;
			case 5:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no11Seconds);break;
			case 6:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no16Seconds);break;
			case 7:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no22Seconds);break;
			case 8:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no30Seconds);break;
			case 9:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no42Seconds);break;
			case 10:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no58Seconds);break;
			case 11:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no81Seconds);break;
			case 12:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no112Seconds);break;
			case 13:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no156Seconds);break;
			case 14:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no217Seconds);break;
			case 15:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no300Seconds);break;
			case 16:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.removeGapControl);break;
			case 17:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no010Seconds);break;
			case 18:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no025Seconds);break;
			case 19:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no050Seconds);break;
			case 20:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no1Second);break;
			case 21:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no2Seconds);break;
			default:asnPvtGapIntvEnumType.setValue(PrivateGapIntervalEnumType.EnumType.no22Seconds);break;
			}

			asnPvtGapIntvl.setValue(asnPvtGapIntvEnumType);
			asnGapIntvl.selectPrivateGapInterval(asnPvtGapIntvl);
		}
		// Set Gap Interval. 
		acgArg.setGapInterval(asnGapIntvl);

		// Translation Type
		int transType= 244;
		if(leg2Data.get(LegDataAttributes.ACG_TRANSLATION_TYPE) != null){
			transType = Integer.parseInt((String) leg2Data.get(LegDataAttributes.ACG_TRANSLATION_TYPE));
		}
		TranslationType asnTransType = new TranslationType();
		asnTransType.setValue(transType);

		// set TranslationType
		acgArg.setTranslationType(asnTransType);

		String gta = null;
		if(leg2Data.get(LegDataAttributes.ACG_GTA) != null){
			gta = (String) leg2Data.get(LegDataAttributes.ACG_GTA);
		}

		byte[] encodedGta = null;
		if(StringUtils.isNotBlank(gta)){
			encodedGta = AinScfProtocolFieldCodec.encodeDigitsInBcdFormat(gta);
		}else{
			logger.error("sendACG: GlobalTitleAddress is not available");
			return;
		}

		GlobalTitleAddress asnGta = new GlobalTitleAddress();
		asnGta.setValue(encodedGta);

		// Set Global Ttile Address
		acgArg.setGlobalTitleAddress(asnGta);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: ACG - controlCauseIndicator:" + controlCauseIndicator[0] +
					", gapDur:" +gapDur+ ", gapIntervalType:"+ gapIntervalType + ", gapInterval:" + gapIntervalValue+
					", Translation Type:"+transType + ", GTA:" +gta);
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.ACG);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(acgArg);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
		byte[] acgArgByte = encodeList.getFirst();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: ACG - encodedBuffer:" +CommonUtils.formatBytes(acgArgByte));
		}

		byte[] acgOpCode = CommonUtils.formatIntToByte(AinOpCodes.ACG_BYTE);

		Operation operation = new Operation(Operation.OPERATIONTYPE_LOCAL, acgOpCode);

		InvokeReqEvent invokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), operation);
		invokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		invokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, acgArgByte));
		invokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		invokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		// Will always be last component. 
		invokeReqEvent.setLastInvokeEvent(true);

		sendComponentReq(invokeReqEvent, callData, cCallTraceService);
	}

	/**
	 * This method checks for call tracing based calling and called party
	 * It should be called from messages being received in BEGIN
	 * @param callData
	 */
	public static void checkCallTracingEnabled(int dialogId, CallData callData, CallTraceService cts){

		if(logger.isDebugEnabled()){
			logger.debug("checkCallTracingEnabled: Inside");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		/*
		 * Fields for call tracing
		 */
		String callingPartyAddress = legData.get(LegDataAttributes.P_CALLING_PARTY) == null ? null
				: ((PhoneNumber) legData.get(LegDataAttributes.P_CALLING_PARTY)).getAddress();
		String calledPartyAddress = legData.get(LegDataAttributes.P_CALLED_PARTY) == null ? null
				: ((PhoneNumber) legData.get(LegDataAttributes.P_CALLED_PARTY)).getAddress();
		String idpCalledPartyAddress = legData.get(LegDataAttributes.P_IDP_CALLED_PARTY) == null ? null
				: ((PhoneNumber) legData.get(LegDataAttributes.P_IDP_CALLED_PARTY)).getAddress();
		String signalingPointCode = legData.get(LegDataAttributes.P_SPC) == null ? null
				: legData.get(LegDataAttributes.P_SPC).toString();

		/*
		 * match call tracing constraints
		 */
		List constraintIdList = cts.matchesCriteria(callingPartyAddress, idpCalledPartyAddress, calledPartyAddress);

		if (constraintIdList != null && !constraintIdList.isEmpty()) {
			callData.set(CallDataAttribute.P_TRACE_CONSTRAINT_ID, constraintIdList);
			callData.set(CallDataAttribute.P_TRACE_FLAG, PhConstants.TRUE);
			callData.set(CallDataAttribute.P_TRACE_MESSAGE, new StringBuilder());
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: constraintId=" + constraintIdList + ", TraceFlag="
					+ Boolean.valueOf((String) callData.get(CallDataAttribute.P_TRACE_FLAG)));
			logger.debug("[PH]:: processIdp return and start processing");
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " :: [PH] constraintId=" + constraintIdList + ", TraceFlag="
					+ Boolean.valueOf((String) callData.get(CallDataAttribute.P_TRACE_FLAG)));
			logger.debug(dialogId + " [PH]::processInfoAnalyzed return and start processing");
		}
	}


	/*
	 * This method is used to relay a AIN message
	 */
	private static void sendMRSRelay(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		LegData legdata = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendMRSRelay");
		}


		ArrayList<InvokeIndEvent> list = (ArrayList<InvokeIndEvent>) legdata.get(LegDataAttributes.P_MRS_COMP_LIST);
		if(list!=null){
			for(InvokeIndEvent Indevent: list) {
				InvokeReqEvent invokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), Indevent.getOperation());
				invokeReqEvent.setInvokeId(Indevent.getInvokeId());
				invokeReqEvent.setLastInvokeEvent(true);
				invokeReqEvent.setParameters(Indevent.getParameters());
				//	invokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
				//	invokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
				sendComponentReq(invokeReqEvent, callData, cCallTraceService);
			}
		}
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
		preProcessDroppedCall(tcapSession);
		sendBeginRequestEvent(tcapSession, false, cCallTraceService);
		postProcessDroppedCall(tcapSession, true);
	}

	/**
	 * This method is used to send a begin request
	 * @param tcapSession
	 * @param b
	 * @param cCallTraceService
	 * @throws Exception
	 */
	private static void sendBeginRequestEvent(TcapSession tcapSession,
			boolean b, CallTraceService cCallTraceService)  {

		try{
			CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
			int dialogId = tcapSession.getDialogueId();

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Inside sendBeginRequestEvent");
			}
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

			LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

			String destSsnStr=(String) leg2Data.get(LegDataAttributes.P_MR_SSN);

			String destPc=(String) leg2Data.get(LegDataAttributes.P_MR_PC);

			String destTT=(String)leg2Data.get(LegDataAttributes.P_MR_TT);

			SignalingPointCode destSpc=null;
			if (destPc != null && !destPc.isEmpty()) {
				String[] tmp = destPc.split("-");
				if (tmp.length == 3) {
					destSpc = new SignalingPointCode(Integer.parseInt(tmp[2]), Integer
							.parseInt(tmp[1]), Integer.parseInt(tmp[0]));
				}
			}
//			else{
//
//				if (logger.isDebugEnabled()) {
//					logger.debug(dialogId + " [PH]:: Dest SUA could not be created provide proper SUA PC parameters ");
//				}
//				return;
//			}
			short destSsn=-1;
			if (destSsnStr != null) {
				destSsn=Short.parseShort(destSsnStr);
			}
//			else{
//
//				if (logger.isDebugEnabled()) {
//					logger.debug(dialogId + " [PH]:: Dest SUA could not be created provide proper SUA SSN parameters ");
//				}
//				return;
//			}
			
			String CD_PTY_NO_PC =(String) leg2Data.get(LegDataAttributes.CD_PTY_NO_PC);
			String CG_PTY_NO_PC = (String) leg2Data.get(LegDataAttributes.CG_PTY_NO_PC);

			SccpUserAddress suaorig=(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS);
			
			SccpUserAddress suaDest=(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);
			
			if(StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"1")) {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to true in in orig addr beginRequest");
				}
				suaDest.setPtyNoPC(true);
			}else {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to false in orig addr beginRequest");
				}
				suaDest.setPtyNoPC(false);
			}
			if(StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"1")) {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to true in dest addr beginRequest");
				}
				suaorig.setPtyNoPC(true);
			}else {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to false in dest addr beginRequest");
				}
				suaorig.setPtyNoPC(false);
			}

		//	SccpUserAddress suaDest=(SccpUserAddress) legData.get(LegDataAttributes.P_SCC);

			String mrDigits=(String) leg2Data.get(LegDataAttributes.P_MR_GT_DIGITS);

			String routingInd=(String) leg2Data.get(LegDataAttributes.P_MR_ROUTING_INDICATOR);

			//   SccpUserAddress localSUA=suaRemote;

			byte destbyte=-1;
			if(destTT !=null){
				Integer destt= Integer.parseInt(destTT);
				destbyte=destt.byteValue();
			}
//			else{
//
//				if (logger.isDebugEnabled()) {
//					logger.debug(dialogId + " [PH]:: Dest SUA could not be created provide proper SUA TT parameters ");
//				}
//				return;
//			}
			
			if (destSpc!=null && destSsn!=-1) {
				SccpUserAddress newSUA = new SccpUserAddress(new SubSystemAddress(
						destSpc, destSsn));
				newSUA.setProtocolVariant(suaDest.getProtocolVariant());
				if (suaDest.isRoutingIndicatorPresent())
					newSUA.setRoutingIndicator(suaDest.getRoutingIndicator());

				if (suaDest.isGlobalTitlePresent()) {
					try {
						newSUA.setGlobalTitle(suaDest.getGlobalTitle());
					} catch (MandatoryParameterNotSetException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (ParameterNotSetException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}

				suaDest = newSUA;
				if (logger.isDebugEnabled()) {
					logger.debug(" [PH]::New created  SUA is" + suaDest);
				}
			}

			GlobalTitle gt=null;
			if(suaDest.isGlobalTitlePresent()&& destbyte!=-1){
				gt=updateGlobalTitle(suaDest,destbyte);///or will take as byte
			}else{

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " [PH]:: Globle Title not Present in P_SCCP_REMOTE_USER_ADDRESS ");
				}
			}

//			if(){
//			 SubSystemAddress destSSA= new SubSystemAddress(destSpc, destSsn);
//			  suaDest.setSubSystemAddress(destSSA);
//			}


			//	SccpUserAddress destAddr = new SccpUserAddress(new SubSystemAddress(destSpc, destSsn));
			//	suaorig.setProtocolVariant(localSUA.getProtocolVariant());

			if(routingInd==null ||routingInd.isEmpty()){
				suaDest.setRoutingIndicator(1);
			}else{
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " [PH]:: Set routing indicator in orig SUA " + routingInd);
				}
				suaDest.setRoutingIndicator( Integer.parseInt(routingInd));
			}

			suaDest.setGlobalTitle(gt);


			if (gt != null && mrDigits != null) {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " [PH]:: set GT digits in address information "
							+ mrDigits);
				}

				byte[] encodedGtd = null;
				if(StringUtils.isNotBlank(mrDigits)){
					encodedGtd = AinScfProtocolFieldCodec.encodeDigitsInBcdFormat(mrDigits);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ " [PH]:: set GT digits byte[] "+ encodedGtd);
					}
				}
				gt.setAddressInformation(encodedGtd);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ " [PH]:: not setting GT digits as GT not set in orig SUA");
				}
			}
			byte qos=(Byte) legData.get(LegDataAttributes.P_BEGIN_QOS);
			
			BeginReqEvent beginReqEvent = new BeginReqEvent(src, tcapSession.getDialogueId(),suaorig,suaDest);
			beginReqEvent.setAllowedPermission(true);
			beginReqEvent.setQualityOfService(qos);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Origin SUA "+suaDest);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Dest SUA "+suaorig);
			}

            String mtp3=(String)leg2Data.get(LegDataAttributes.MTP3_DPC);
			
			SignalingPointCode mtp3dpc=AinScfProtocolUtil.createSignallingPointCode(mtp3);
			if(mtp3dpc!=null) {
				logger.debug("Setting Mtp3 DPC with value :- "+ mtp3dpc);
				beginReqEvent.setMtp3Dpc(mtp3dpc);
			}
			
			String mtp3op=(String)leg2Data.get(LegDataAttributes.MTP3_OPC);
			SignalingPointCode mtp3opc =AinScfProtocolUtil.createSignallingPointCode(mtp3op);
			if(mtp3opc!=null) {
				logger.debug("Setting Mtp3 OPC with value :- "+ mtp3opc);

				beginReqEvent.setMtp3Opc(mtp3opc);
			}


			DialoguePortion dialoguePortion = (DialoguePortion) legData.get(LegDataAttributes.P_DIALOG_PORTION);
			if (dialoguePortion != null) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + " [PH]:: Set Dialogue portion");
				}
				beginReqEvent.setDialoguePortion(dialoguePortion);
			}

			tcapSession.setAttribute(PhConstants.MRS_RELAY, PhConstants.TRUE);
			sendDialogueReq(beginReqEvent, callData, cCallTraceService);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: leave sendBeginRequestEvent");
			}

			//			AinScfProtocolUtil.setLastInvokeIdStartRange(AinScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1,
			//					callData);
			//			AinScfProtocolUtil.setLastInvokeIdEndRange(AinScfProtocolUtil.getLastInvokeId(callData), callData);

		} catch(Exception e){
			logger.error("error whil sending begin for MS relay "+ e);
		}
	}


	public static byte[] longToBytes(long x) {
		ByteBuffer buffer = ByteBuffer.allocate(Long.BYTES);
		buffer.putLong(x);
		return buffer.array();
	}

	private static GlobalTitle updateGlobalTitle(SccpUserAddress userAdd,
			byte translationType) {

		GlobalTitle gt = null;
		// specific GT params

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: updateGlobalTitle Enter translationType "+ translationType);
		}
		try {
			if (userAdd.getGlobalTitle().getGTIndicator() == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0001) {
				GTIndicator0001 gt1 = (GTIndicator0001) userAdd
						.getGlobalTitle();
				gt1.setTranslationType(translationType);
				gt = gt1;

			} else if (userAdd.getGlobalTitle().getGTIndicator() == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0010) {
				GTIndicator0010 gt2 = (GTIndicator0010) userAdd
						.getGlobalTitle();
				gt2.setTranslationType(translationType);
				gt = gt2;
			} else if (userAdd.getGlobalTitle().getGTIndicator() == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0011) {
				GTIndicator0011 gt3 = (GTIndicator0011) userAdd
						.getGlobalTitle();

				gt3.setTranslationType(translationType);
				gt = gt3;
				// adding Translation Type
			} else if (userAdd.getGlobalTitle().getGTIndicator() == jain.protocol.ss7.tcap.TcapConstants.GTINDICATOR_0100) {
				GTIndicator0100 gt4 = (GTIndicator0100) userAdd
						.getGlobalTitle();
				gt4.setTranslationType(translationType);
				gt = gt4;
			}
		} catch (MandatoryParameterNotSetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParameterNotSetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] :: updateGlobalTitle exit  "+gt);
		}
		return gt;

	}

	/**
	 * This metod is used to process isvm query
	 * @param componentIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 * @throws Exception
	 */
	public static Action[] processISVMQuery(
			ComponentIndEvent componentIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processISVMQuery Enter ");
		}
		callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.ISVM_QUERY);
		/*
		 * store invoke id
		 */
		int invokeId = ((InvokeIndEvent) componentIndEvent).getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke ISVM Query::" + invokeId);
		}
		//setting service type to MRS		
		legData.set(LegDataAttributes.P_SERVICE_TYPE, PhConstants.MRS);
		legData.set(LegDataAttributes.P_MRS_QTYPE, PhConstants.ISVM);

		if(legData.get(LegDataAttributes.P_MRS_COMP_LIST) == null) {
			ArrayList<InvokeIndEvent> invokeIndEventList = new ArrayList<InvokeIndEvent>();
			legData.set(LegDataAttributes.P_MRS_COMP_LIST, invokeIndEventList);
		}
		if (!componentIndEvent.isLastComponent()) {
			//check need to decode or not
			if(MRSMessageHandler.isSixDigitsSCCP(callData)) {
				MRSMessageHandler.parseISVMtcapInitialQuery((InvokeIndEvent) componentIndEvent, callData);
			}
			//NOT LAST COMPONENT
			@SuppressWarnings("unchecked")
			ArrayList<InvokeIndEvent> list = (ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST) == null?
					new ArrayList<InvokeIndEvent>():(ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST);
					list.add((InvokeIndEvent) componentIndEvent);
					legData.set(LegDataAttributes.P_MRS_COMP_LIST, list);
					if (logger.isInfoEnabled()) {
						logger.info("ISVM Query component list (NOT LAST):"+list);
					}

		} else {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.ISVM_QUERY);
			@SuppressWarnings("unchecked")
			ArrayList<InvokeIndEvent> list = (ArrayList<InvokeIndEvent>) legData.get(LegDataAttributes.P_MRS_COMP_LIST);
			list.add((InvokeIndEvent) componentIndEvent);
			legData.set(LegDataAttributes.P_MRS_COMP_LIST, list);
			if (logger.isInfoEnabled()) {
				logger.info("ISVM Query component list (LAST):"+list);
			}
		}
		return null;
	}

	/**
	 * This method executes LIDB get Data Query as per standard GR2838co.i02.pdf
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 */
	public static Action[] processLidbGetData(InvokeIndEvent invokeIndEvent,
			TcapSession tcapSession, CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processLidbGetData Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke processLidbGetData::" + invokeId);
		}

		/*
		 * parse GetDataQuery Query
		 */
		try {
			AinScfProtocolParser.parseGetDataQuery(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.GET_DATA_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.GET_DATA_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in GET_DATA_QUERY.", ape);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in OLNS_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in GN Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.OLNS_QUERY);
		}
		return null;
	}

	
	/**
	 * This method executes LIDB CC1 Query as per standard GR1149 OSSGR System interfaces 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 */
	public static Action[] processLidbCC1Query(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processLidbCC1Query Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke CC1_Query::" + invokeId);
		}

		/*
		 * parse CC1 Query
		 */
		try {
			AinScfProtocolParser.parseCc1Query(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.CC1_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.CC1_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in CC1_QUERY.", ape);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in CC1_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in CC1 Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.CC1_QUERY);
		}
		return null;
	}

	/**
	 * This method executes LIDB CC2 Query as per standard GR1149 OSSGR System interfaces 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 */
	public static Action[] processLidbCC2Query(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processLidbCC2Query Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke CC2_Query::" + invokeId);
		}

		/*
		 * parse CC2 Query
		 */
		try {
			AinScfProtocolParser.parseCc2Query(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.CC2_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.CC2_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in CC2_QUERY.", ape);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}
			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in CC2_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in CC2 Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.CC2_QUERY);
		}
		return null;
	}
	
	/**
	 * This method executes LIDB TLNS Query as per standard GR1149 OSSGR System interfaces 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 */
	public static Action[] processLidbTLNSQuery(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processLidbTLNSQuery Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke TLNS_Query::" + invokeId);
		}

		/*
		 * parse TLNS Query
		 */
		try {
			AinScfProtocolParser.parseTlnsQuery(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.TLNS_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TLNS_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in TLNS_QUERY.", ape);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}
			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in TLNS_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in TLNS Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.TLNS_QUERY);
		}
		return null;
	}

	
	/**
	 * This method executes LIDB Intercept Query as per standard GR1149 OSSGR System interfaces 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 */
	public static Action[] processLidbInterceptQuery(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processLidbInterceptQuery Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke INTERCEPT_QUERY::" + invokeId);
		}

		/*
		 * parse Intercept Query
		 */
		try {
			AinScfProtocolParser.parseInterceptQuery(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.INTERCEPT_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.INTERCEPT_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in INTERCEPT_QUERY.", ape);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in INTERCEPT_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in Intercept Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.INTERCEPT_QUERY);
		}
		return null;
	}	

	/**
	 * This method executes LIDB ICDC Query as per standard GR1149 OSSGR System interfaces 
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 */
	public static Action[] processLidbICDCQuery(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processLidbICDCQuery Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke ICDC_QUERY::" + invokeId);
		}

		/*
		 * parse ICDC Query
		 */
		try {
			AinScfProtocolParser.parseICDCQuery(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.ICDC_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.ICDC_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in ICDC_QUERY.", ape);
			if (legData.get(LegDataAttributes.P_LEG_SS7_STATE) != AinCallStates.LIDB_APP_ERR_PROBLEM_DATA) {
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			}			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in ICDC_QUERY.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in ICDC Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ICDC_QUERY);
		}
		return null;
	}	
	
	
	
	/**
	 * This method is used to create decoded message buffer for initial message 
	 * @param callData
	 * @return
	 */
	private static StringBuilder dumpTraceMsgForInitialMsg(CallData callData){

		if(logger.isDebugEnabled()){
			logger.debug("Inside dumpTraceMsgForInitialMsg");
		}

		StringBuilder traceMsg = new StringBuilder();

		try{
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			PhoneNumber phoneNumbervalue = null;
			String stringValue = null;
			Integer intValue = null; 

			// User ID
			if(legData.get(LegDataAttributes.P_USER_ID) != null){
				com.agnity.mphdata.common.UserID userId = 
						(com.agnity.mphdata.common.UserID) legData.get(LegDataAttributes.P_USER_ID);

				traceMsg.append("UserID           : \n");

				if(userId.getDn() != null){
					traceMsg.append("\tDn : ").append(userId.getDn()).append("\n");
				}else if(userId.getFacilityId() != null){
					traceMsg.append("\tPrivateFacilityGID : ").append(userId.getFacilityId()).append("\n");
				}else if(userId.getTrunkGroup() != null){
					traceMsg.append("\tTrunkGroupID : ").append(userId.getTrunkGroup()).append("\n");
				}else if(userId.getAdsIcpeId() != null){
					traceMsg.append("\tADSIcpeID : ").append(userId.getAdsIcpeId()).append("\n");
				}
			}

			if(legData.get(LegDataAttributes.P_BEAR_CAP) != null){
				stringValue = (String) legData.get(LegDataAttributes.P_BEAR_CAP);
				traceMsg.append("BearerCapability : ").append(stringValue).append("\n");
			}

			if(legData.get(LegDataAttributes.P_CALLED_PARTY) != null){
				phoneNumbervalue = (PhoneNumber) legData.get(LegDataAttributes.P_CALLED_PARTY);
				traceMsg.append("Called Party     : ")
				.append(phoneNumbervalue.getAddress() + "-" + phoneNumbervalue.getNumberingPlan()).append("\n");
			}

			if(legData.get(LegDataAttributes.P_LATA) != null){
				stringValue = (String) legData.get(LegDataAttributes.P_LATA);
				traceMsg.append("LATA             : ").append(stringValue).append("\n");
			}

			if(legData.get(LegDataAttributes.P_TRIGGER_CRITERIA) != null){
				stringValue = (String) legData.get(LegDataAttributes.P_TRIGGER_CRITERIA);
				traceMsg.append("TriggerCriteria  : ").append(stringValue).append("\n");
			}

			if(legData.get(LegDataAttributes.P_CHARGE_NUMBER) != null){
				phoneNumbervalue = (PhoneNumber) legData.get(LegDataAttributes.P_CHARGE_NUMBER);
				traceMsg.append("ChargeNumber     : ")
				.append(phoneNumbervalue.getAddress() + "-" + phoneNumbervalue.getNumberingPlan()).append("\n");
			}

			if(legData.get(LegDataAttributes.P_CALLING_PARTY) != null){
				phoneNumbervalue = (PhoneNumber) legData.get(LegDataAttributes.P_CALLING_PARTY);
				traceMsg.append("CallingNumber    : ")
				.append(phoneNumbervalue.getAddress() + "-" + phoneNumbervalue.getNumberingPlan()).append("\n");
			}

			if(legData.get(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE) != null){
				intValue = (Integer) legData.get(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE);
				traceMsg.append("ChargePartyStationType : ").append(intValue).append("\n");
			}

			if(legData.get(LegDataAttributes.P_CARRIER) != null){
				CarrierInfo cic = (CarrierInfo)legData.get(LegDataAttributes.P_CARRIER);
				traceMsg.append("Carrier          : ")
				.append(cic.getAddress()+ "-" + cic.getCarrierSelection()).append("\n");
			}

			if(legData.get(LegDataAttributes.P_ACCESS_CODE) != null){
				phoneNumbervalue = (PhoneNumber) legData.get(LegDataAttributes.P_ACCESS_CODE);
				traceMsg.append("AccessCode       : ")
				.append(phoneNumbervalue.getAddress() + "-" + phoneNumbervalue.getNumberingPlan()).append("\n");
			}

			if(legData.get(LegDataAttributes.P_COLLECTED_ADDRESS_INFO) != null){
				phoneNumbervalue = (PhoneNumber) legData.get(LegDataAttributes.P_COLLECTED_ADDRESS_INFO);
				traceMsg.append("CollectedAddressInfo : ")
				.append(phoneNumbervalue.getAddress() + "-" + phoneNumbervalue.getNumberingPlan()).append("\n");
			}

			if(legData.get(LegDataAttributes.P_COLLECTED_DIGITS) != null){
				phoneNumbervalue = (PhoneNumber) legData.get(LegDataAttributes.P_COLLECTED_DIGITS);
				traceMsg.append("CollectedDigits  : ")
				.append(phoneNumbervalue.getAddress() + "-" + phoneNumbervalue.getNumberingPlan()).append("\n");
			}

			if(legData.get(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER) != null){
				phoneNumbervalue = (PhoneNumber) legData.get(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER);
				traceMsg.append("OriginalCalledPartyId : ")
				.append(phoneNumbervalue.getAddress() + "-" + phoneNumbervalue.getNumberingPlan()).append("\n");
			}

			if(legData.get(LegDataAttributes.P_REDIRECTING_PARTY_ID) != null){
				phoneNumbervalue = (PhoneNumber) legData.get(LegDataAttributes.P_REDIRECTING_PARTY_ID);
				traceMsg.append("RedirectingPartyId : ")
				.append(phoneNumbervalue.getAddress() + "-" + phoneNumbervalue.getNumberingPlan()).append("\n");
			}

			if(legData.get(LegDataAttributes.P_GN_M_GENERIC_NAME) != null){
				stringValue = (String) legData.get(LegDataAttributes.P_GN_M_GENERIC_NAME);

				traceMsg.append("GenericName      : ").append(stringValue);

				if(legData.get(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION) != null){
					stringValue = (String) legData.get(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION); 
					traceMsg.append("\nGenericNamePresentation      : ").append(stringValue);
				}
				traceMsg.append("\n");
			}

			if(callData.get(CallDataAttribute.P_ACG_ENCOUNTERED) != null){
				intValue = (Integer) callData.get(CallDataAttribute.P_ACG_ENCOUNTERED);
				traceMsg.append("ACGEncountered : ").append(intValue).append("\n");
			}
		}catch(Exception exp){ }

		if(logger.isDebugEnabled()){
			logger.debug(traceMsg.toString());
		}
		return traceMsg;
	}
	
	/**
	 * This method executes Account Code TCAP Query (for Rogers)
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param callTraceService
	 * @return
	 */
	public static Action[] processAccountCodeQuery(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService callTraceService) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processAccountCodeQuery Enter ");
		}
		/*
		 * store invoke id
		 */
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] GOT invoke AccountCodeQuery::" + invokeId);
		}

		/*
		 * Parse Account code Query
		 */
		try {
			AinScfProtocolParser.parseAccountCodeQuery(invokeIndEvent, callData);
			callData.set(CallDataAttribute.P_LAST_OPERATION_RX, MESSAGE.ACCOUNT_CODE_QUERY);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.ACCOUNT_CODE_QUERY);
			checkCallTracingEnabled(dialogId, callData, callTraceService);
		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in ACCOUNT_CODE_QUERY.", ape);
			legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.LIDB_PROTOCOL_ERR);
			switch (ape.getParseFailType()) {
			case DEFAULT: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}

			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in ACCOUNT_CODE Query.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in ACCOUNT_CODE Query.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.OLNS_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.TLNS_QUERY);
		}
		return null;
	}
	
	/**
	 * Method to parse phone number 
	 * @param dialogueId
	 * @param callingPartyNum
	 * @return
	 */
	public static PhoneNumber parseT1PhoneNumber(int dialogueId, T1Digits callingPartyNum) {
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside parseCallingPartyNum");
		}

		PhoneNumber callingNumber = new PhoneNumber();

		/*
		 * Nature Of Address
		 */
		NatureOfNumEnum natureOfAddrEnum = callingPartyNum.getNoa();
		callingNumber.setNatureOfAddress(natureOfAddrEnum.getCode());

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Nature of Address is " + natureOfAddrEnum.getCode());
		}

		/*
		 * Numbering Plan Indicator
		 */
		if(callingPartyNum.getNumPlanEnum() != null) {
			callingNumber.setNumberingPlan(callingPartyNum.getNumPlanEnum().getCode());
			if (logger.isDebugEnabled()) {
				logger.debug(
						dialogueId + "::[PH] Extracted Numbering Plan is " + callingPartyNum.getNumPlanEnum().getCode());
			}
		}

		/*
		 * Address
		 */
		String addrSignal = callingPartyNum.getAddrSignal();
		callingNumber.setAddress(addrSignal);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Address Signal is " + addrSignal);
		}

		return callingNumber;
	}

	/**
	 * This method is used when service chaningis invoked. 
	 * @param tcapSession
	 * @param callData
	 * @param action
	 */
	public static boolean invokeServiceChaining(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(" AinScfProtocolHelper- invokeServiceChaining Entering ...");
		}
		if(!action.isInvokeServiceChaining()){
			if(logger.isDebugEnabled()){
				logger.debug("No need to invoke chaining as flag is not set by application..so connecting to term"); 
			}
			return false;
		}
		String currentSvcId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		String prevSvcId=(String) callData.get(CallDataAttribute.PREV_SERVICE_ID);
		@SuppressWarnings("unchecked")
		Event event = new Event(EventType.EVENT_INITIAL,
				Protocol.AIN_SCF, CallDataAttribute.P_LEG1.name());
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int  sua=(Integer) leg1Data.get(LegDataAttributes.P_OPC);
		String origInfo=sua+"|" +callData.get(CallDataAttribute.P_SERVICE_KEY);
		if (logger.isDebugEnabled()) {
			logger.debug(" Current service id is  "+ currentSvcId +" Prev svc id "+prevSvcId);
		}
		AseAppChainManager acm=PhUtilityServices.getInstance(currentSvcId).getAppChainManager();
		Map<String,Map<CallChainedAttributes, Object>> chainingMap=
			(Map<String,Map<CallChainedAttributes, Object>>)callData.get(CallDataAttribute.P_SVC_CHAINING_MAP);
		Object modCallingNum=null;
		Object modCalledNum=null;
		Object modOrigInfo=null;
		if (chainingMap!=null&&chainingMap.get(currentSvcId)!=null) {
			Map<CallChainedAttributes,Object>  chainedAttributes=chainingMap.get(currentSvcId);
			modCallingNum = chainedAttributes
					.get(CallChainedAttributes.MODIFIED_CALLING_NUM);
			modCalledNum = chainedAttributes
					.get(CallChainedAttributes.MODIFIED_CALLED_NUM);
			if (chainedAttributes
					.get(CallChainedAttributes.MODIFIED_ORIGIN_INFO) != null) {
				origInfo = (String) chainedAttributes
						.get(CallChainedAttributes.MODIFIED_ORIGIN_INFO);
			}
			if(chainedAttributes.get(CallChainedAttributes.CHAIN_START_TIME)==null){
				chainedAttributes.put(CallChainedAttributes.CHAIN_START_TIME, callData.get(CallDataAttribute.P_CALL_START_TIME));
			}
			chainedAttributes.put(CallChainedAttributes.END_TIME, new Date());
		} else{
			LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
			modCallingNum=leg1Data.get(LegDataAttributes.P_CALLING_PARTY);
			modCalledNum=leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);
		}
		@SuppressWarnings("unchecked")
		Map<String,Object> addressesMap =(Map<String,Object>)callData.get(CallDataAttribute.ADDRESS_MAP);
		if(addressesMap==null){
			addressesMap= new HashMap<String,Object>();	
		}
		addressesMap.put(AseAppChainManager.CALLING_NUM,modCallingNum);
		addressesMap.put(AseAppChainManager.MODIFIED_DIALLED_NUMBER,modCalledNum);
		Integer dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		Protocol protocol = (Protocol) callData.get(CallDataAttribute.P_PROTOCOL);
		String nextServiceId = acm
				.getNextInterestedService(""+dialogId,currentSvcId,
						prevSvcId,
						addressesMap,
						action.getEvent(), origInfo, protocol);
		if (nextServiceId != null) {
			AinScfProtocolUtil.resetNetworkTransactions(callData);
			if(chainingMap==null){
				chainingMap = new HashMap<String,Map<CallChainedAttributes, Object>>();
				callData.set(CallDataAttribute.P_SVC_CHAINING_MAP,chainingMap);
			}
			Map<CallChainedAttributes, Object> nexChainedMap= new HashMap<CallChainedAttributes, Object>();
			nexChainedMap.put(CallChainedAttributes.MODIFIED_CALLING_NUM, modCallingNum);
			nexChainedMap.put(CallChainedAttributes.MODIFIED_CALLED_NUM, modCalledNum);
			nexChainedMap.put(CallChainedAttributes.CHAIN_START_TIME, new Date());
			nexChainedMap.put(CallChainedAttributes.MODIFIED_ORIGIN_INFO, origInfo);
			chainingMap.put(nextServiceId, nexChainedMap);
			if (logger.isDebugEnabled()) {
				logger.debug(" next service found is "+nextServiceId + " update serviceid in call data and changed map "+ chainingMap);
			}
			callData
			.set(CallDataAttribute.SERVICE_ID,nextServiceId);
			callData
			.set(CallDataAttribute.PREV_SERVICE_ID,currentSvcId);
			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN, PhConstants.FALSE);
			tcapSession.setAttribute(PhConstants.SERVICE_ID, nextServiceId);
			if (logger.isDebugEnabled()) {
				logger.debug(" call invokeServiceChaining on app chain manager  !!! ");
			}
			acm.invokeServiceChaining(
					currentSvcId,
					nextServiceId,
					addressesMap,
					event, tcapSession, null, action.isRemainInPath());//action.getEvent()
			if (logger.isDebugEnabled()) {
				logger.debug("Service chaining is invoked !!! ");
			}
			MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.AIN_SCF);
			if(measurementCounter != null) {
				measurementCounter.incrementServiceTriggeredCount(nextServiceId, true);
			}
			return true;
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug("No next service is available for current service  "+ currentSvcId);
			}
			if(action.isDropCallOnNoNextSvc()){
				if(logger.isDebugEnabled()){
					logger.debug( "Drop call as service want to drop call here"); 
				}
				callData.set(
						CallDataAttribute.NP_REASON_FOR_RELEASE,
						AinScfRelReasonCode.REJECT_NO_NEXT_SERVICE);
				dropCall(tcapSession, callData);
				return true;
			}
			return false;
		}
	}
}
