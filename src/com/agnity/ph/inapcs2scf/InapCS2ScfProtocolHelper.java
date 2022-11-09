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

import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.lPad;
import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.src;
import jain.MandatoryParameterNotSetException;
import jain.ParameterNotSetException;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;
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
import jain.protocol.ss7.tcap.dialogue.BeginIndEvent;
import jain.protocol.ss7.tcap.dialogue.ContinueReqEvent;
import jain.protocol.ss7.tcap.dialogue.DialogueConstants;
import jain.protocol.ss7.tcap.dialogue.DialoguePortion;
import jain.protocol.ss7.tcap.dialogue.EndReqEvent;
import jain.protocol.ss7.tcap.dialogue.UserAbortReqEvent;

import java.io.IOException;
import java.util.Date;
import java.util.EventObject;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.sip.ServletParseException;
import javax.servlet.sip.SipApplicationSession;
import javax.servlet.sip.SipFactory;
import javax.servlet.sip.SipServletRequest;
import javax.servlet.sip.SipSession;
import javax.servlet.sip.URI;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.asngenerated.UpdateRequestArg;
import com.agnity.inapitutcs2.asngenerated.FCIBillingChargingCharacteristics;
import com.agnity.inapitutcs2.asngenerated.FurnishChargingInformationArg;
import com.agnity.inapitutcs2.enumdata.CauseValEnum;
import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.inapitutcs2.operations.InapOperationsCoding;
import com.agnity.inapitutcs2.util.Util;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.Action.CONNECTIONMODE;
import com.agnity.mphdata.common.Action.CONTINUE_MODE;
import com.agnity.mphdata.common.Action.DROP_CALL_MODE;
import com.agnity.mphdata.common.Action.SEND_MODE;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.InapCallStates;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.ainscf.AinScfProtocolFieldCodec;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
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
import com.agnity.ph.inapcs2scf.InapCS2ScfProtocolFSMHandler.AbortInfoEnum;
import com.agnity.ph.inapcs2scf.flowhelper.InapCS2ATHelper;
import com.agnity.ph.inapcs2scf.flowhelper.InapCS2BCSMHelper;
import com.agnity.ph.inapcs2scf.flowhelper.InapCS2MediaServerHelper;
import com.agnity.ph.sip.SipProtocolConfig;
import com.baypackets.ase.router.acm.AseAppChainManager;
import com.baypackets.ase.sbb.OutboundGateway;
import com.baypackets.ase.sbb.OutboundGatewaySelector;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;

/**
 * This class is a helper class which is used by INAP protocol handler to delegate
 * the message processing of incoming messages and and creation of outgoing INAP messages
 */
public class InapCS2ScfProtocolHelper {

	private static Logger logger = Logger.getLogger(InapCS2ScfProtocolHelper.class);

	/**
	 * This method performs the processing of Begin dialogue indication event.
	 *
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action objects.
	 * @throws MandatoryParameterNotSetException
	 */
	static void processBegin(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) throws MandatoryParameterNotSetException {

		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: processBegin Enter ");
		}

		/*
		 * call data can't be null if it has passed validation method so moving
		 * ahead without null check store dialog
		 */

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Set Dialogue Portion in call data");
			}
			try {
				//Store Dialogue Portion in Leg Data.
				legData.set(LegDataAttributes.P_DIALOG_PORTION, dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn("[PH]:: Error getting dialogue portion " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn("[PH]:: Error getting dialog portion.", e);
					logger.info("[PH]:: IGNORE ParameterNotSetException fetching " + "dialoguePortion from BEGIN dialogue event");
				}
			}
		}
		
		if(dialogueIndEvent.isReturnOptionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] Set Return Option value" + dialogueIndEvent.getM_returnOption());
			}
			legData.set(LegDataAttributes.P_RETURN_OPTION, dialogueIndEvent.getM_returnOption());
		}
		
		if(dialogueIndEvent.isSequenceControlPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] Set Sequence Control value" + dialogueIndEvent.getM_sequenceControl());
			}
			legData.set(LegDataAttributes.P_SEQUENCE_CONTROL, dialogueIndEvent.getM_sequenceControl());
		}
		
		if(dialogueIndEvent.isMessagePriorityPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] Set Message Priority value" + dialogueIndEvent.getM_messagePriority());
			}
			legData.set(LegDataAttributes.P_MESSAGE_PRIORITY, dialogueIndEvent.getM_messagePriority());
		}

		//perform ACN check
		BeginIndEvent beginIndEvent = (BeginIndEvent) dialogueIndEvent;
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: isAppContextNamePresent = " + beginIndEvent.isAppContextNamePresent());
		}
		if (beginIndEvent.isAppContextNamePresent()) {
			try {
				byte[] appContextName = beginIndEvent.getAppContextName();
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Application Context version from IDP = " + Util.formatBytes(appContextName));
				}

				/*
				 * As isAppContextNamePresent always returns true even if
				 * appCOntextName is null making null check
				 */
				if (appContextName != null) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Application Context not null");
					}
					legData.set(LegDataAttributes.P_ACTIVITY_CONTEXT_VERSION, appContextName[appContextName.length - 1]);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Application Context is null");
					}
					/*
					 * valid acn handling is odne at service in test call only
					 * return handleInavlidAcn(tcapSession);
					 */
				}
			} catch (Exception e) {
				logger.warn("[PH]:: Error fetching AC version " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn("[PH]:: Exception fetching AC Version", e);
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Set Application Context version " + legData.get(LegDataAttributes.P_ACTIVITY_CONTEXT_VERSION) + " in call data");
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: isAppContextNamePresent is false");
			}
		}

		try {
			SccpUserAddress sccpUserAddress = beginIndEvent.getOriginatingAddress();
			SignalingPointCode signalingPointCode = sccpUserAddress.getSubSystemAddress().getSignalingPointCode();
			int zone = signalingPointCode.getZone();
			int cluster = signalingPointCode.getCluster();
			int member = signalingPointCode.getMember();

			logger.debug("[PH]:: Signal Point Code :: " + signalingPointCode);
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Origin zone= " + zone + " cluster=" + cluster + " member=" + member);
			}
			if(zone==0 && cluster==0 && member==0) {
				logger.debug("0 found for zone , cluster and member replacing with mtp3Opc");
				SignalingPointCode spc =beginIndEvent.getMtp3Opc();
				zone= spc.getZone();
				cluster=spc.getCluster();
				member=spc.getMember();
				legData.set(LegDataAttributes.P_SPC, spc);
			}else {
				legData.set(LegDataAttributes.P_SPC, signalingPointCode);
			}
			logger.debug("Zone:- "+ zone+ " cluster:- "+ cluster + " member:- "+ member);
			String pcBitStr = lPad(Integer.toBinaryString(zone), 3) + lPad(Integer.toBinaryString(cluster), 8) + lPad(Integer.toBinaryString(member), 3);
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: pcBitStr =" + pcBitStr);
			}
			

			
			int pc = Integer.parseInt(pcBitStr, 2);
			
			legData.set(LegDataAttributes.P_OPC, pc);
			callData.set(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS, beginIndEvent.getDestinationAddress());
			callData.set(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS, beginIndEvent.getOriginatingAddress());
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Calculated Origin Point Code form IDP-Begin =" + legData.get(LegDataAttributes.P_OPC));
			}
		} catch (ParameterNotSetException e1) {
			logger.error("[PH]:: Failed to get origin point code from Dialog Indication event. " + e1.getMessage());
		}

		// Use the sccp address received in IDP to support multiple pc-ssn
		legData.set(LegDataAttributes.P_SUA, beginIndEvent.getDestinationAddress());
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: processBegin Exit ");
		}
	}

	/**
	 * This method performs the processing of continue dialogue indication
	 * event.
	 *
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action Objects
	 * @throws MandatoryParameterNotSetException 
	 */
	static Action[] processContinue(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) throws MandatoryParameterNotSetException {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: processContinue Enter ");
		}

		if (dialogueIndEvent.isDialoguePortionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Set Dialogue Portion in call data");
			}

			try {
				legData.set(LegDataAttributes.P_DIALOG_PORTION, dialogueIndEvent.getDialoguePortion());
			} catch (ParameterNotSetException e) {
				logger.warn("[PH]:: Error getting dialogue portion. " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn("[PH]:: Error getting dialog portion.", e);
					logger.info("[PH]:: IGNORE ParameterNotSetException fetching " + "dialoguePortion from CONTINUE dialogue event");
				}
			}
		}
		
		if(dialogueIndEvent.isReturnOptionPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] Set Return Option value" + dialogueIndEvent.getM_returnOption());
			}
			legData.set(LegDataAttributes.P_RETURN_OPTION, dialogueIndEvent.getM_returnOption());
		}		
		
		if(dialogueIndEvent.isSequenceControlPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] Set Sequence Control value" + dialogueIndEvent.getM_sequenceControl());
			}
			legData.set(LegDataAttributes.P_SEQUENCE_CONTROL, dialogueIndEvent.getM_sequenceControl());
		}
		
		if(dialogueIndEvent.isMessagePriorityPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] Set Message Priority value" + dialogueIndEvent.getM_messagePriority());
			}
			legData.set(LegDataAttributes.P_MESSAGE_PRIORITY, dialogueIndEvent.getM_messagePriority());
		}
		
		
		return null;
	}

	/**
	 * This method performs the processing of TC_END dialogue indication event.
	 *
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processEnd(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]::  processEnd Enter ");
		}
		return null;
	}

	/**
	 * This method performs the processing of Abort dialogue indication event.
	 * Marks the cause value as 41 for failed calls.
	 *
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processAbort(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]::  processAbort Enter ");
		}
		return null;
	}

	/**
	 * This method performs the processing of UAbort dialogue indication event.
	 * Marks the cause value as 31 because user hung up the call and related
	 * events are not armed..
	 *
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @param tcapSession      represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] processUAbort(DialogueIndEvent dialogueIndEvent, TcapSession tcapSession) {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]::  processUAbort Enter ");
		}
		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever an IDP is
	 * received.
	 *
	 * @param invokeIndEvent represents the instance of InvokeIndEvent
	 * @param tcapSession    represents the instance of TcapSession
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] processIdp(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		CallTraceService cts = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]::  processIdp Enter ");
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processIdp setting service state");
		}

		// change state to service logic
		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);

		// store invoke id
		int invokeId = invokeIndEvent.getInvokeId();
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: GOT invoke ID::" + invokeId);
		}

		// parse IDP
		try {
			InapCS2ScfProtocolParser.parseIdp(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error("[PH]:: ASN pasring Exception in IDP.", ape);

			switch (ape.getParseFailType()) {
			case CPC_MISSING: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.IDP_MISSING_CPC);
				break;
			}
			case TMR_MISSING: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.IDP_MISSING_TMR);
				break;
			}
			case ACPC_MISSING: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.IDP_MISSING_ACPC);
				break;
			}
			case FCI_MISSING: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.IDP_MISSING_FCI);
				break;
			}
			default: {
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.IDP_ASN_PARSING_FAIL);
				break;
			}
			}
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error("[PH]:: CriticalityTypeException in IDP.", cte);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.IDP_INVALID_EXTN_TYPE);
			return getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error("[PH]:: ParameterOutOfRangeException in IDP.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.IDP_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.IDP);
		}

		/*
		 * Unable to take TraceCall by OriginatingAddress in case INAP IDP
		 * includes Contract Number.
		 * fetch calling party address first check contractor; if contractor not
		 * present get callingNum
		 */
		String callingPartyAddress = legData.get(LegDataAttributes.P_CALLING_PARTY) == null ? null : ((PhoneNumber) legData.get(LegDataAttributes.P_CALLING_PARTY)).getAddress();
		String calledPartyAddress = legData.get(LegDataAttributes.P_CALLED_PARTY) == null ? null : ((PhoneNumber) legData.get(LegDataAttributes.P_CALLED_PARTY)).getAddress();
		String idpCalledPartyAddress = legData.get(LegDataAttributes.P_IDP_CALLED_PARTY) == null ? null : ((PhoneNumber) legData.get(LegDataAttributes.P_IDP_CALLED_PARTY)).getAddress();
		String signalingPointCode = legData.get(LegDataAttributes.P_SPC) == null ? null : legData.get(LegDataAttributes.P_SPC).toString();
		String serviceKey = callData.get(CallDataAttribute.P_SERVICE_KEY) == null ? null : callData.get(CallDataAttribute.P_SERVICE_KEY).toString();


		/*
		 * match call tracing constraints
		 */
		List constraintIdList = cts.matchesCriteria(callingPartyAddress, idpCalledPartyAddress, calledPartyAddress, null, signalingPointCode, serviceKey);

		if (constraintIdList != null && !constraintIdList.isEmpty()) {
			callData.set(CallDataAttribute.P_TRACE_CONSTRAINT_ID, constraintIdList);
			callData.set(CallDataAttribute.P_TRACE_FLAG, PhConstants.TRUE);
			callData.set(CallDataAttribute.P_TRACE_MESSAGE, new StringBuilder());
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: constraintId=" + constraintIdList + ", TraceFlag=" + Boolean.valueOf((String) callData.get(CallDataAttribute.P_TRACE_FLAG)));
			logger.debug("[PH]:: processIdp return and start processing");
		}

		return null;
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
	public static Action[] getASNParsingFailureAction(InvokeIndEvent invokeIndEvent, TcapSession tcapSession, ASNParsingException ape) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside handleASNParsingFailure with exception::" + ape);
		}
		MESSAGE message = ape.getInapMessage();
		return getASNParsingFailureAction(tcapSession, message, ape.getParseFailType());

	}

	/**
	 * This method is called by the Protocol handler whenever
	 * CriticalityTypeException is thrown while parsing IDP received.
	 *
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param tcapSession    represents an instance of TcapSession
	 * @param cte            represents an instance of CriticalityTypeException
	 * @return an array of Action objects.
	 * @throws Exception
	 */
	static Action[] getInvalidExtensionTypeAction(InvokeIndEvent invokeIndEvent, TcapSession tcapSession, CriticalityTypeException cte) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getInvalidExtensionTypeAction:: Drop call");
		}
		Action action = new Action(ActionType.ACTION_END_CALL);
		logger.warn(cte.getMessage());
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: cte error", cte);
		}
		if (cte.getCriticality().equals(CRITICALITY.IGNORE)) {
			/*
			 * this case is never reached as we continue on ignore
			 */
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			action.setReleaseCauseValue(CauseValEnum.Mandatory_information_element_missing.getCode());
		} else if (cte.getCriticality().equals(CRITICALITY.ABORT)) {
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(AbortInfoEnum.UNRECOGNIZED_EXTENSION_PARAMETER.getCode());
		}

		return (new Action[]{action});
	}

	/**
	 * This method is called by the Protocol handler whenever
	 * ParameterOutOfRangeException is thrown while parsing INAP signal
	 * received.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @param message     represents an instance of MESSAGE
	 * @return an array of Action objects.
	 */
	public static Action[] getOutOfRangeParamterAction(TcapSession tcapSession, MESSAGE message) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getOutOfRangeParamterAction::" + message);
		}
		// handle as ASN parsing
		return getASNParsingFailureAction(tcapSession, message, FAILTYPE.DEFAULT);
	}

	/**
	 * This method return actions to be performed to Protocol handler when
	 * calling party is missing from IDP.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] getCallingPartyMissingAction(TcapSession tcapSession) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getCallingPartyMissingAction:: Drop call");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
		return (new Action[]{action});

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
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getASNParsingFailureAction with message::" + message + "   failtype::" + failtype);
		}
		Action[] actionArr = null;

		switch (message) {
		case IDP: {
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
			actionArr = (new Action[]{action});
			break;
		}
		case UERROR: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_PARAMETER);
			actionArr = (new Action[]{action});
			break;
		}
		case UREJECT: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_COMPONENT);
			actionArr = (new Action[]{action});
			break;
		}
		default: {
			// reject as continue
			Action action1 = new Action(ActionType.ACTION_CONTINUE);
			action1.setContinueMode(CONTINUE_MODE.USER_REJECT);
			action1.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_PARAMETER);

			// set Release cause as tc end
			Action action2 = new Action(ActionType.ACTION_END_CALL);
			action2.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);

			actionArr = (new Action[]{action1, action2});
			break;
		}
		}
		return actionArr;
	}

	/**
	 * This method is called by the Protocol handler whenever an ENC is
	 * received.
	 *
	 * @param invokeIndEvent represents the instance of InvokeIndEvent
	 * @param tcapSession    represents the instance of TcapSession
	 * @return an array of Action Objects
	 * @throws Exception
	 */
	static Action[] processEnc(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processEnc");
		}
		try {
			InapCS2ScfProtocolParser.parseEnc(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error("[PH]:: ASN pasring Exception in ENC.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ENC_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error("[PH]:: ParameterOutOfRangeException in ENC.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ENC_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ENC);

		}
		logger.error("[PH]:: Not supported processEnc() .....");
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
	static Action[] processResult(ResultIndEvent resultIndEvent, TcapSession tcapSession) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processResult");
		}

		byte[] operCode = null;
		Action[] action = null;
		if (resultIndEvent.isLastResultEvent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: It is last result event");
			}
			// check if opcode present in result if yes get opercode directly
			if (resultIndEvent.isOperationPresent()) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Operation Present in event");
				}
				try {
					operCode = resultIndEvent.getOperation().getOperationCode();
				} catch (MandatoryParameterNotSetException e) {
					logger.error("[PH]:: Error getting operation code " + e.getMessage());
					throw e;
				} catch (ParameterNotSetException e) {
					logger.warn("[PH]:: Error getting operation code " + e.getMessage());
					if (logger.isInfoEnabled()) {
						logger.warn("[PH]:: Error getting operation code", e);
						logger.info("[PH]:: IGNORE ParameterNotSetException in getOperation.");
					}
				}
			} else if (resultIndEvent.isInvokeIdPresent()) {
				// opcode not present get invoke id and fetch opcode form there
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Invoke Id present in event");
				}
				try {
					int resultInvokeId = resultIndEvent.getInvokeId();

					Integer atInvokeId = (Integer) callData.get(CallDataAttribute.P_AT_INVOKE_ID);
					Integer pacInvokeId = (Integer) callData.get(CallDataAttribute.PROMPT_COLLECT_INVOKE_ID);
					Integer disconnectLegInvokeId = (Integer) callData.get(CallDataAttribute.P_DISCONNECT_LEG_INVOKE_ID);

					if (atInvokeId != null && atInvokeId == resultInvokeId) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: It is Activity Test Event");
						}
						operCode = new byte[1];
						operCode[0] = InapOpCodes.ACTIVITY_TEST_BYTE;
					} else if (pacInvokeId != null && pacInvokeId == resultInvokeId) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: It is Prompt & Collect Event");
						}
						operCode = new byte[1];
						operCode[0] = InapOpCodes.PAC_BYTE;
					} else if (disconnectLegInvokeId != null && disconnectLegInvokeId ==  resultInvokeId)  {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: It is Disconnect leg Id");
						}
						operCode = new byte[1];
						operCode[0] = InapOpCodes.DISCONNECT_LEG_BYTE;
					} else {
						logger.warn("[PH]:: Unknown invoke id received: " + resultInvokeId +
								". Valid invokeIds in current context: [" + atInvokeId + "," + pacInvokeId + "]");
					}
				} catch (ParameterNotSetException e) {
					logger.warn("[PH]:: Error getting invoke id " + e.getMessage());
					if (logger.isInfoEnabled()) {
						logger.warn("[PH]:: Error getting invoke id", e);
						logger.debug("[PH]:: IGNORE ParameterNotSetException in getInvokeId.", e);
					}
				}
			}

			if (operCode != null) {
				String operCodeStr = CommonUtils.formatBytes(operCode);
				byte operCodeByte = operCode[0];
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Operation Code is " + operCodeStr);
				}
				switch (operCodeByte) {
				case InapOpCodes.ACTIVITY_TEST_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Cancel the Activity Test Acknowledgement Timer");
					}
					InapCS2ATHelper.cancelATTimer(tcapSession);
				}
				break;
				case InapOpCodes.PAC_BYTE: {
					InapCS2MediaServerHelper.parsePACResult(resultIndEvent, callData);
					Event event = new Event(EventType.EVENT_PNC_SUCCESS, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG1.name());
					try {
						ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
					} catch (Exception e) {
						logger.error("[PH]:: Exception thrown while parsing PAC result");
					}
				}
				break;
				case InapOpCodes.DISCONNECT_LEG_BYTE: {
					Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG2.name());
					try {
						ProtocolRouter.getInstance().execute(event, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler());
					} catch (Exception e) {
						logger.error("[PH]:: Exception thrown while parsing DisconnectLeg result");
					}
				}
				break;
				default: {
					logger.warn("[PH]:: Received Result for unknown Component Indication Event " + operCodeStr);
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNKNOWN_RESULT);
					action = getUnknownResultAction(tcapSession);
					break;
				}
				}
			} else {
				logger.warn("[PH]:: Result Operation code is unknown.");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNKNOWN_RESULT);
				action = getUnknownResultAction(tcapSession);
			}
		}
		return action;
	}


	/**
	 * This method is for handling any unknown result events. It just ignores
	 * unknown Result opcode.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @return an array of Action objects.
	 */
	static Action[] getUnknownResultAction(TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getUnknownResultAction:: " + "Drop call with U-Reject");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
		action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_RETURN_RESULT_UNEXPECTED);

		return (new Action[]{action});

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
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processError");
		}

		int invokeId = -1;
		byte[] errorCode = null;

		/*
		 * setting DFC to false as Inap call is failed
		 */
		callData.set(CallDataAttribute.P_DFC_REQUIRED_FLAG, PhConstants.FALSE);

		try {
			invokeId = errorIndEvent.getInvokeId();
			errorCode = errorIndEvent.getErrorCode();
		} catch (MandatoryParameterNotSetException mpne) {
			logger.error("[PH]:: MandatoryParameterNotSetException in RE", mpne);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERR_MSG_MAND_PARAM_MISSING);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UERROR, FAILTYPE.DEFAULT);
		} catch (ParameterNotSetException pne) {
			logger.error("[PH]:: ParameterNotSetException in RE", pne);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERR_MSG_PARAM_NOT_SET);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UERROR, FAILTYPE.DEFAULT);
		}

		int lastInvokeIdStart = InapCS2ScfProtocolUtil.getLastInvokeIdStartRange(callData);
		int lastInvokeIdEnd = InapCS2ScfProtocolUtil.getLastInvokeIdEndRange(callData);
		/*
		 * validateInvokeId
		 */
		// the below check is not valid in case Reset Timer is sent after ETC. 
		// In this case invoke ID is 1 (that of ETC) whereas lastInvokeIdStart would be 2, that of ResetTimer
		// the below validation will always fail. Commenting it. 
		//if ((invokeId < lastInvokeIdStart) || (invokeId > lastInvokeIdEnd)) {
		//	logger.warn("[PH]:: Invoke id invalid in U error; recived:" + invokeId + " valid range for current message:: " + lastInvokeIdStart + " to " + lastInvokeIdEnd);
		//	callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS1ScfRelReasonCode.ERR_MSG_INVALID_INVOK_ID);
		//	return getInvalidInvokeIdAction(tcapSession, MESSAGE.UERROR);
		//}

		if (errorCode != null && logger.isDebugEnabled()) {
			String errorCodeStr = CommonUtils.formatBytes(errorCode);
			logger.debug("[PH]:: Error code is " + errorCodeStr);

		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Return action to Drop the call");
		}
		Action action = new Action(Action.ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		/*
		 * set cause as temporary failure
		 */
		action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());

		return (new Action[]{action});

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
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getInvalidInvokeIdAction with message::" + message);
		}

		Action[] actionArr = null;

		switch (message) {
		case UERROR: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_UNRECOGNIZED_INVOKE_ID);
			actionArr = new Action[]{action};
			break;
		}
		case UREJECT: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_COMPONENT);
			actionArr = (new Action[]{action});
			break;
		}

		} // @end switch

		return actionArr;
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

		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processReject");
		}
		int rejectType = -1;
		int problemType = -1;
		int problem = -1;

		/*
		 * setting DFC to false as Inap call is failed
		 */
		callData.set(CallDataAttribute.P_DFC_REQUIRED_FLAG, PhConstants.FALSE);

		/*
		 * get reject type
		 */
		if (rejectIndEvent.isRejectTypePresent()) {
			try {
				rejectType = rejectIndEvent.getRejectType();
			} catch (ParameterNotSetException e) {
				logger.warn("[PH]:: Error in getting reject type " + e.getMessage());
				if (logger.isInfoEnabled()) {
					logger.warn("[PH]:: Error getting reject type", e);
					logger.info("[PH]:: IGNORE ParameterNotSetException in getting reject type.");
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Reject Type is [" + rejectType + "]");
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
			logger.error("[PH]:: MandatoryParameterNotSetException in UREJECT", mpne);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.REJECT_MSG_MAND_PARAM_MISSING);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UREJECT, FAILTYPE.DEFAULT);
		} catch (ParameterNotSetException pne) {
			logger.error("[PH]:: ParameterNotSetException in UREJECT", pne);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.REJECT_MSG_PARAM_NOT_SET);
			return getASNParsingFailureAction(tcapSession, MESSAGE.UREJECT, FAILTYPE.DEFAULT);
		}

		/*
		 * validateInvokeId
		 */

		int lastInvokeIdStart = InapCS2ScfProtocolUtil.getLastInvokeIdStartRange(callData);
		int lastInvokeIdEnd = InapCS2ScfProtocolUtil.getLastInvokeIdEndRange(callData);
		if ((invokeId < lastInvokeIdStart) || (invokeId > lastInvokeIdEnd)) {
			logger.error("[PH]:: Invoke id invalid in U Reject; recived:" + invokeId + " valid range for current message:: " + lastInvokeIdStart + " to " + lastInvokeIdEnd);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.REJECT_INVALID_INVOK_ID);
			return getInvalidInvokeIdAction(tcapSession, MESSAGE.UREJECT);
		}

		/*
		 * validate problem type
		 */
		if (problemType != ComponentConstants.PROBLEM_TYPE_INVOKE) {
			logger.error("[PH]:: Problem type invalid in U reject; recived:" + invokeId + " problemType:" + problemType + " Expected problem type:" + ComponentConstants.PROBLEM_TYPE_INVOKE);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.REJECT_INVALID_PROBLEM_TYPE);
			return getInvalidProblemAction(tcapSession, MESSAGE.UREJECT);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Problem Type is [" + problemType + "]  problem is  [" + problem + "]");
		}

		Action action = new Action(Action.ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.NULL_END);
		action.setReleaseCauseValue(41);

		return (new Action[]{action});
	}

	/**
	 * This method is called by protocol handler whenever a component indication
	 * event of type PRIMITIVE_REJECT is received and problem received is
	 * invalid.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @param message     represents an instance of MESSAGE
	 * @return an array of Action Objects
	 */
	static Action[] getInvalidProblemAction(TcapSession tcapSession, MESSAGE message) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getInvalidProblemAction with message::" + message);
		}
		Action[] actionArr = null;

		switch (message) {
		case UREJECT: {
			Action action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_REJECT);
			action.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_COMPONENT);
			actionArr = (new Action[]{action});
			break;
		}

		} // @end switch

		return actionArr;
	}

	/**
	 * This method is for handling any unknown dialogue events. It just ignores
	 * unknown dialog events returning null.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @return an array of Action objects.
	 */
	static Action[] getUnknownDialogAction(TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getUnknownDialogAction:: " + "ignore unknown dialog events");
		}
		/*
		 * ignore unknown dialog events returning null.
		 */
		return null;
	}

	/**
	 * This method returns action to be taken when an out of sequence dialogue
	 * indication event is received. In case TC_END is not received just release
	 * the call with CV=41.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @param dialogType  represents an integer representation of dialogue type.
	 * @return an array of Action Objects
	 */
	static Action[] getOutOfSequenceDialogAction(TcapSession tcapSession, int dialogType) {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getOutOfSequenceDialogAction:: Drop Call");
		}
		Action action = null;

		InapCallStates callState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		if (callState == InapCallStates.INIT) {
			/*
			 * send UAbort for terminated calls
			 */
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Last Rx dlg not END do release calls:: ");
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
					logger.debug("[PH]:: Inside handleOutOfSequenceDialog:: unknown begin/continue");
				}
				action = new Action(ActionType.ACTION_END_CALL);
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
				break;
			}
			default: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Inside handleOutOfSequenceDialog:: unknown dialog " + dialogType);
				}
				action = new Action(ActionType.ACTION_END_CALL);
				action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
				action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
				break;
			}
			}
		}
		return (new Action[]{action});
	}

	/**
	 * This method return actions to be performed to Protocol handler when
	 * unknown message is received.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @param event       represents an instance of InapEvent
	 * @return an array of Action Objects
	 */
	static Action[] getUnknownMessageAction(TcapSession tcapSession, InapCS2ScfProtocolEvent event) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getUnknownMessageAction:: same as handleOutOfSequenceMsg");
		}
		return getOutOfSequenceMsgAction(tcapSession, event);

	}

	/**
	 * This method return actions to be performed to Protocol handler when out
	 * of sequence message is received.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @param event       represents an instance of InapEvent
	 * @return an array of Action Objects
	 */
	static Action[] getOutOfSequenceMsgAction(TcapSession tcapSession, InapCS2ScfProtocolEvent event) {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getOutOfSequenceMsgAction:: Drop call");
		}

		// release call if end dialog not exchanged yet.
		Action action = null;
		InapCallStates callState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		if (callState == InapCallStates.INIT) {
			// send UAbort for terminated calls
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Last Rx dlg not END do release calls:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			// set reason as no reason given
			action.setReleaseCauseValue(AbortInfoEnum.NO_REASON_GIVEN.getCode());
		} else if (callState != InapCallStates.TERMINATED) {
			// check if it was unarmed event case or event in invalid call state
			Object unarmedEventError = tcapSession.getAttribute(PhConstants.UNARMED_ERROR_TYPE);
			if (unarmedEventError != null) {
				// case of unArmed ERB event
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Unarmed ERB event:: ");
				}
				return getUnArmedErbEventAction(tcapSession, event);
			}
			// send release call for in progress calls
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Last call state is not terminated create action:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());
		} else {
			// send UAbort for terminated calls
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Last Rx dlg not END do release calls:: ");
			}
			action = new Action(ActionType.ACTION_END_CALL);
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			// set reason as no reason given
			action.setReleaseCauseValue(1);
		}
		return (new Action[]{action});
	}

	/**
	 * This method return actions to be performed to Protocol handler when
	 * unarmed ERB event is received.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @param event       represents an instance of InapEvent
	 * @return an array of Action Objects
	 */
	static Action[] getUnArmedErbEventAction(TcapSession tcapSession, InapCS2ScfProtocolEvent event) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getUnArmedErbEventAction send reject and RC");
		}

		// reject as continue
		Action action1 = new Action(ActionType.ACTION_CONTINUE);
		action1.setContinueMode(CONTINUE_MODE.USER_REJECT);
		action1.setReleaseCauseValue(ComponentConstants.PROBLEM_CODE_MISTYPED_PARAMETER);

		// release cause as tc end
		Action action2 = new Action(ActionType.ACTION_END_CALL);
		action2.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		action2.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());

		Action[] actionArr = (new Action[]{action1, action2});

		return actionArr;

	}

	/**
	 * This method returns ER response action whenever an ER event is received.
	 *
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param tcapSession    represents an instance of TcapSession
	 * @return an array of Action objects
	 */
	static Action[] getEntityReleasedAction(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside getEntityReleasedAction:: Drop call");
		}
		Action action = null;

		/*
		 * setting DFC to false as Inap call si cleaned
		 */
		callData.set(CallDataAttribute.P_DFC_REQUIRED_FLAG, PhConstants.FALSE);
		/*
		 * parses and store cv from ER
		 */
		try {
			InapCS2ScfProtocolParser.parseEntityRelease(invokeIndEvent, callData);
		} catch (Exception e) {
			logger.error("[PH]:: cause value pasring from ER failed", e);
			/*
			 * using default as message type as ER is not supported and rare
			 * message
			 */
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ER_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(tcapSession, MESSAGE.DEFAULT, FAILTYPE.DEFAULT);
		}

		action = new Action(ActionType.ACTION_END_CALL);
		action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
		action.setReleaseCauseValue(CauseValEnum.Temporary_failure.getCode());

		return (new Action[]{action});

	}

	/**
	 * This method is called by protocol handler for sending continue message.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */
	public static void sendContinueMessage(TcapSession tcapSession, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendContinueMessage::");
		}
		CONTINUE_MODE continueMode = action.getContinueMode();

		switch (continueMode) {
		case INITIAL_ERROR: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send RETURN_ERROR with TC_CON");
			}
			sendErrorReqEvent(tcapSession, action);
			sendContinueRequestEvent(tcapSession);
			break;
		}
		case USER_REJECT: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send U_REJECT with TC_CON");
			}
			sendRejectReqEvent(tcapSession, action);
			sendContinueRequestEvent(tcapSession);
			break;
		}
		case ENTITY_RELEASE: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send ER with TC_CON");
			}
			sendEntityRelease(tcapSession, action);
			sendContinueRequestEvent(tcapSession);
			break;
		}
		}
	}

	/**
	 * This method is called by protocol handler for sending error request
	 * event.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */
	static void sendErrorReqEvent(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendErrorReqEvent");
		}

		byte[] reason = new byte[]{0x07};
		if (action.getReleaseCauseValue() > 0) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Got custom reason for u error::" + action.getReleaseCauseValue());
			}
			reason = new byte[]{(byte) action.getReleaseCauseValue()};
		}

		ErrorReqEvent errorReqEvent = new ErrorReqEvent(src, tcapSession.getDialogueId(), ComponentConstants.ERROR_LOCAL, reason);
		errorReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getLastRxInvokeId(callData));
		sendComponentReq(errorReqEvent, callData);
	}

	/**
	 * This method is called by protocol handler for sending continue request
	 * event.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @throws Exception
	 */
	public static void sendContinueRequestEvent(TcapSession tcapSession) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendContinueRequestEvent");
		}

		ContinueReqEvent continueReqEvent = new ContinueReqEvent(src, tcapSession.getDialogueId());
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
		
//		String routingInd=(String) leg2Data.get(LegDataAttributes.P_MR_ROUTING_INDICATOR);
//		
//		SccpUserAddress sua = (SccpUserAddress) legData.get(LegDataAttributes.P_SUA);
		
//		if(StringUtils.isNotBlank(routingInd)){
//			if (logger.isDebugEnabled()) {
//				logger.debug("[PH]:: updateRoutingInd for GT based routing-->");
//			}
//			sua=updateRoutingInd(callData, leg2Data);
//		}
//		continueReqEvent.setOriginatingAddress(sua);
		
		SccpUserAddress origAddr=InapCS2ScfProtocolUtil.updateCallingAddress(callData, leg2Data);
		continueReqEvent.setOriginatingAddress(origAddr);
		
		SccpUserAddress destAddr=InapCS2ScfProtocolUtil.updateCalledAddress(callData, leg2Data);
		continueReqEvent.setDestinationAddress(destAddr);
		

		
		DialoguePortion dialoguePortion = (DialoguePortion) legData.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			continueReqEvent.setDialoguePortion(dialoguePortion);
		}

		if(leg2Data != null) {
			String returnOption= (String) leg2Data.get(LegDataAttributes.P_RETURN_OPTION);
		    
		    if(returnOption != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug("[PH]:: return Option is "+ returnOption);
		    	}
		    	continueReqEvent.setReturnOption(returnOption);
		    }
		    
			String CD_PTY_NO_PC =(String) legData.get(LegDataAttributes.CD_PTY_NO_PC);
			String CG_PTY_NO_PC = (String) legData.get(LegDataAttributes.CG_PTY_NO_PC);
				if(StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"1")) {
					if(logger.isDebugEnabled()) {
						logger.debug("Setting PtyNoPc to true in in orig addr endReqEvent1 in inapcs2");
					}
					destAddr.setPtyNoPC(true);
				}else {
					if(logger.isDebugEnabled()) {
						logger.debug("Setting PtyNoPc to false in orig addr endReqEvent1 in inapcs2");
					}
					destAddr.setPtyNoPC(false);
				}
				if(StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"1")) {
					if(logger.isDebugEnabled()) {
						logger.debug("Setting PtyNoPc to true in dest addr endReqEvent1 in inapcs2");
					}
					origAddr.setPtyNoPC(true);
				}else {
					if(logger.isDebugEnabled()) {
						logger.debug("Setting PtyNoPc to false in dest addr endReqEvent1 in inapcs2");
					}
					origAddr.setPtyNoPC(false);
				}
				continueReqEvent.setOriginatingAddress(origAddr);
				continueReqEvent.setDestinationAddress(destAddr);	
			
		    String sequenceControl= (String) leg2Data.get(LegDataAttributes.P_SEQUENCE_CONTROL);
		    
		    if(sequenceControl != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug("[PH]:: sequenceControl is "+ sequenceControl);
		    	}
		    	continueReqEvent.setSequenceControl(sequenceControl);
		    }
		    
		    String messagePriority= (String) leg2Data.get(LegDataAttributes.P_MESSAGE_PRIORITY);
		    
		    if(messagePriority != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug("[PH]:: messagePriority is "+ messagePriority);
		    	}
		    	continueReqEvent.setMessagePriority(messagePriority);
		    }
		}
		sendDialogueReq(continueReqEvent, callData);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: leave sendContinueRequestEvent");
		}

		InapCS2ScfProtocolUtil.setLastInvokeIdStartRange(InapCS2ScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1, callData);
		InapCS2ScfProtocolUtil.setLastInvokeIdEndRange(InapCS2ScfProtocolUtil.getLastInvokeId(callData), callData);
	}
	
	
	/**
	 * 
	 * @param callData
	 * @param leg2Data
	 * @return
	 */
	private static SccpUserAddress updateRoutingInd(CallData callData,LegData leg2Data){
		
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Entering updateRoutingInd");
		}
		SccpUserAddress selfSUA=(SccpUserAddress) callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);

		String routingInd=(String) leg2Data.get(LegDataAttributes.P_MR_ROUTING_INDICATOR);

		if(routingInd==null ||routingInd.isEmpty()){
			selfSUA.setRoutingIndicator(1);
		}else{
			if (logger.isDebugEnabled()) {
				logger.debug(" [PH]:: Update only routing indicator in self SUA " + routingInd);
			}
			selfSUA.setRoutingIndicator(Integer.parseInt(routingInd));
		}
		
		return selfSUA;
	}

	/**
	 * This method is called by protocol handler for sending Reject request
	 * event.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */
	private static void sendRejectReqEvent(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendRejectReqEvent");
		}

		RejectReqEvent rejectReqEvent = new RejectReqEvent(src);
		rejectReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getLastRxInvokeId(callData));
		rejectReqEvent.setDialogueId(tcapSession.getDialogueId());

		/*
		 * reject type from service will always be user
		 */
		rejectReqEvent.setRejectType(ComponentConstants.REJECT_TYPE_USER);
		int problem = action.getReleaseCauseValue();
		rejectReqEvent.setProblem(problem);
		rejectReqEvent.setProblemType(problem);
		sendComponentReq(rejectReqEvent, callData);
	}

	/**
	 * This method is called by protocol handler for sending Entity release
	 * event.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */
	static void sendEntityRelease(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createEntityRelease");
		}

		byte[] releaseCall = InapCS2ScfProtocolParser.createEntityRelease(callData, action);
		byte[] erOpCode = {InapOpCodes.ER_BYTE};

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, erOpCode);

		InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rcOperation);
		erInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		erInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, releaseCall));
		erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(erInvokeReqEvent, callData);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: leave createEntityRelease");
		}
	}

	/**
	 * This method is called by protocol handler for sending INAP component
	 * indication event.
	 *
	 * @param cre      represents the instance of ComponentReqEvent
	 * @param callData represents the instance of CallData
	 * @throws Exception
	 */
	public static void sendComponentReq(ComponentReqEvent cre, CallData callData) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendComponentReq");
		}
		PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider().sendComponentReqEvent(cre);
		InapCS2ScfCallTraceHelper.traceComponent(cre, callData);
	}

	/**
	 * This method is used by protocol handler for sending dialogue request.
	 *
	 * @param dre      represents the instance of DialogueReqEvent
	 * @param callData represents the instance of CallData
	 * @throws Exception
	 */
	static void sendDialogueReq(DialogueReqEvent dre, CallData callData) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendDialogueReq");
		}
		incrementTcapCounters(dre);
		PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider().sendDialogueReqEvent(dre);
		InapCS2ScfCallTraceHelper.traceDialog(dre, callData, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());
	}

	/**
	 * This method is for creating FCI
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @throws Exception
	 */
	public static void sendFci(TcapSession tcapSession, boolean sendDialogue) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside sendFci");
		}
		if(legData==null){
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "[PH]:: Inside sendFci leg2 data is null s returning from ere");
			}
			return;
		}

		String fciChrInfoStr = (String) legData.get(LegDataAttributes.P_FCI_BILLING_CHARACTERISTICS);
		if(!StringUtils.isNotBlank(fciChrInfoStr)){
			if(logger.isDebugEnabled()){
				logger.debug(dialogueId 
						+ "[PH]:: sendFci - do not send FCI as P_FCI_BILLING_CHARACTERISTICS is null in Leg2");
			}
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside sendFci:: fci dump=" + fciChrInfoStr);
		}

		FCIBillingChargingCharacteristics fCharacteristics = new FCIBillingChargingCharacteristics();
		byte[] fciChrInfo = CommonUtils.convertHexStringToByteArray(fciChrInfoStr);
		fCharacteristics.setValue(fciChrInfo);
		
		// check of array out of bound
		if(fciChrInfo != null && (fciChrInfo.length < 6 || fciChrInfo.length > 90)){
			logger.error("FCI out of bound may happen:" 
		          + CommonUtils.formatBytes(fciChrInfo) + ", fci string:" + fciChrInfoStr);
		}

		FurnishChargingInformationArg fciArg = new FurnishChargingInformationArg();
		fciArg.setValue(fCharacteristics);

		LinkedList opObjects = new LinkedList();
		opObjects.add(fciArg);
		LinkedList opCodes = new LinkedList();
		opCodes.add(InapOpCodes.FCI);
		LinkedList<byte[]> fciLinkList = InapOperationsCoding.encodeOperations(opObjects, opCodes);
		byte[] fci = fciLinkList.getFirst();
		byte[] fciOpCode = {InapOpCodes.FCI_BYTE};

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: FCI encoded fci dump=" + CommonUtils.formatBytes(fci));
		}

		Operation fciOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, fciOpCode);
		InvokeReqEvent fciInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), fciOperation);
		fciInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		fciInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, fci));
		fciInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		sendComponentReq(fciInvokeReqEvent, callData);

		callData.set(CallDataAttribute.IS_FCI_SENT, true);

		if(sendDialogue){
			// 	Send Dialogue as Continue
			sendContinueRequestEvent(tcapSession);
		}
		
		// after consumption remove FCI
		legData.remove(LegDataAttributes.P_FCI_BILLING_CHARACTERISTICS);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Exit FCI send as Continue Dialogue.");
		}
	}

	/**
	 * This method is used to connect the terminating party
	 *
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void connectTerm(TcapSession tcapSession, CallData callData, Action action) throws Exception {

		Integer dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		InapCallStates inapCallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		Boolean dialoutCheck = (Boolean) callData.get(CallDataAttribute.P_DIALOUT);
		if(dialoutCheck == null){
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: [PH] Setting dialout Check as False for Value null");
			}
			dialoutCheck = false;
		}
		if (action.getConnectionMode() == Action.CONNECTIONMODE.EQSROUTING && dialoutCheck) {
			if(logger.isDebugEnabled()){
				logger.debug(dialogId+":: Condition match for PSX Route");
			}
			sendSipInviteFrmInapCS2(tcapSession, callData);
			return;
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside connectTerm with tcapSession");
			logger.debug("[PH]:: InapCallState is " + legData.get(LegDataAttributes.P_LEG_SS7_STATE));
		}

		switch (inapCallState) {
		case SERVICE_LOGIC:
		case ASSIST:
		case PSX_ROUTING:
		case MS_DISCONNECTED:{
			if (action.getConnectionMode() == CONNECTIONMODE.REROUTING) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Initiating Terminating Connection in REROUTING mode");
				}
				sendConnectTerm(tcapSession, action);
				InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.TERM_CONNECT_IN_PROGRESS);
				/*
				 * Set assist appsession timeout to 2 miniutes
				 */
				SipApplicationSession assistAppSession = InapCS2ScfProtocolUtil.getAssistAppSession(tcapSession,
						PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());
				if (assistAppSession != null) {
					CommonUtils.setAppSessionTimeout(assistAppSession, PhConstants.CLEANUP_APPSESSION_TIMEOUT, dialogId);
				}
			} else if (action.getConnectionMode() == CONNECTIONMODE.PORTROUTING) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Initiating Terminating Connection in PORTROUTING mode");
				}

				sendConnectPort(tcapSession, action);
			} else if ((action.getConnectionMode() == CONNECTIONMODE.REDIRECTION)
					|| (action.getConnectionMode() == CONNECTIONMODE.B2BUA)) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Initiating Terminating Connection with handoff");
				}


				String correlationId = (String) callData.get(CallDataAttribute.P_CORRELATION_ID);
				String sharedPoolEnabled=InapCS2ScfProtocolConfig.getConfigData(InapCS2ScfProtocolConfig.SHARED_TOKEN_POOL_ENABLED);

				if (PhConstants.TRUE.equals(sharedPoolEnabled)) {

					correlationId = InapCS2ScfProtocolUtil.getTokenFromSharedPool(callData);

					if(correlationId == null) {
						logger.error("PRI number exhausted while connecting Term. Cleaning up call for DialogueID:" + 
								tcapSession.getDialogueId());
					}
				}

				if (correlationId != null) {

					sendConnectHandoff(tcapSession, action);
					InapCS2ScfProtocolUtil.updateSS7CallState(legData,
							InapCallStates.HANDOFF);

					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Start correlation timer");
					}

					InapCS2ScfProtocolUtil
					.startTimer(tcapSession, InapCS2ScfProtocolUtil
							.getCorrelationTime(callData), true,
							PhConstants.CORRELATION_TIMER);

					/*
					 * setting tcapsession id in corr map; not setting entire
					 * object to avoid ft issues
					 */

					PhUtilityServices
					.getInstance(
							(String) callData
							.get(CallDataAttribute.SERVICE_ID))
					.getCorrelationMap()
					.put(correlationId,
							tcapSession.getDialogueId());
					tcapSession.setAttribute(PhConstants.CORRELATION_ID,
							correlationId);

				}else{

					logger.error("[PH]:: Connect term invoked with invalid correlationID");

					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
					dropCall(tcapSession, callData);
				}
			}else if (action.getConnectionMode() == CONNECTIONMODE.CONTINUE){
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Connection Mode as Continue, sending continue");
				}

				sendContinue(tcapSession, action);
			}else { 
				logger.error("[PH]:: Connect term invoked with invalid connection mode, drop call");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.INVALID_CONNMODE_TERM);

				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				dropCall(tcapSession, callData);
			}
			break;
		}
		default: {
			logger.error("[PH]:: Connect term invoked in invalid state, drop call");
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXP_ACT_CONNECT_TERM);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			dropCall(tcapSession, callData);
			break;
		}
		}
	}

	/**
	 * Default drop call method to create default action on drop call based on
	 * call state.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void dropCall(TcapSession tcapSession, CallData callData) {
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside dropCall with tcapSession");
		}

		Action action = new Action(ActionType.ACTION_END_CALL);
		InapCallStates inapCallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		// check if drop call is due to ETC send failure. if yes then set it 
		// in action class for further processing. 
		if(callData.get(CallDataAttribute.NP_ETC_SEND_FAILURE) != null){
			action.setErrorInSendingEtc(true);
			// remove attribute from callData. Not required anymore from here
			callData.remove(CallDataAttribute.NP_ETC_SEND_FAILURE);
		}

		/*
		 * Constructing default action. For RC use cause value set by service.
		 * For other cases use default.
		 */
		switch (inapCallState) {
		case INIT:
		case SERVICE_LOGIC:
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: For service logic state default drop Mode is USER_ABORT");
			}
			action.setDropCallMode(DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(InapCS2ScfProtocolFSMHandler.AbortInfoEnum.NO_REASON_GIVEN.getCode());
			break;
		case HANDOFF:
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: For HANDOFF state default drop Mode is NONE");
			}
			action.setDropCallMode(DROP_CALL_MODE.NONE);
			break;
		case TERMINATION_IN_PROGRESS:
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: For TERMINATION_IN_PROGRESS state default drop Mode is NULL_END");
			}
			action.setDropCallMode(DROP_CALL_MODE.NULL_END);
			break;
		case TERMINATED:
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: For TERMINATED state default drop Mode is USER_ABORT");
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
				logger.debug("[PH]:: For CONNECT_INPROGRESS/CONNECTED/ASSIST/default states " + "use default drop Mode is RELEASE_CALL");
			}
			action.setDropCallMode(DROP_CALL_MODE.RELEASE_CALL);
			action.setReleaseCauseValue(CauseValEnum.Normal_call_clearing.getCode());

		}
		dropCall(tcapSession, callData, action);
	}

	/**
	 * Executes drop call action passed to method based on Inap call state.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 */
	public static void dropCall(TcapSession tcapSession, CallData callData, Action action) {
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		// postProcessDroppedCall in finally need not to be called in case 
		// last operation received as END or Abort. 
		boolean notifyApplicationDropStatus = true;
		try {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Inside dropCall with tcapSession");
				logger.debug("[PH]:: InapCallState is " + legData.get(LegDataAttributes.P_LEG_SS7_STATE));
				logger.debug("[PH]:: DropCallMode is " + action.getDropCallMode().name());
			}

			/*
			 * if last dialog is END; No need to term message
			 */
			int rxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
			if (rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END) {
				logger.warn("[PH]:: Last Rx dialog primitive is END; Clean the call locally.");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.END_RCVD_WITH_COMP);
				/*
				 * before return pre process dropped call as in finally block
				 * CDRs will be written
				 */
				preProcessDroppedCall(tcapSession);
				notifyApplicationDropStatus = false;
				return;
			}
			if (rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_USER_ABORT || rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_PROVIDER_ABORT || rxDialoguePrimitiveType == TcapConstants.PRIMITIVE_NOTICE) {
				logger.error("[PH]:: Last dialogue received, so clean the call locally");
				/*
				 * before return pre process dropped call as in finally block
				 * CDRs will be written
				 */
				preProcessDroppedCall(tcapSession);
				return;
			}

			InapCallStates inapCallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
			logger.debug("InapCallState in dropCall: " + inapCallState);

			switch (inapCallState) {
			case INIT:
			case SERVICE_LOGIC:
			case TERM_CONNECT_IN_PROGRESS:
			case TERM_CONNECTED:
			case TERMINATED:
			case PSX_ROUTING:
			case MS_DISCONNECTED: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: dropCall(session,action)");
				}
			}
			break;
			case TERMINATION_IN_PROGRESS: {
				/*
				 * This is internal stage after this stage is set.. No messages
				 * will be received by ph before state is changed to Terminated.
				 */
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: dropCall(session,action)");
				}
				/*
				 * Force drop call mode to null end as state happens on
				 * odisconnect and oAbndon only
				 */
				action.setDropCallMode(DROP_CALL_MODE.NULL_END);
			}
			break;
			case ASSIST: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: dropCall(session,action), send DFC and remove entry from correlation map");
					logger.debug("[PH]:: clean sip leg if any");
				}

				String dfcFlag = (String) callData.get(CallDataAttribute.P_DFC_REQUIRED_FLAG);
				//	if (dfcFlag!=null &&!PhConstants.FALSE.equals(dfcFlag)) {

				if (tcapSession.getAttribute(PhConstants.DFC_SENT) == null &&
						action.isErrorInSendingEtc()) {

					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: isDfcRequired is true; Orig sip call state:" + "origSipCallState");
					}
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: sending DFC");
					}
					InapCS2MediaServerHelper.sendDfc(tcapSession, action);
				} else if (action.isErrorInSendingEtc()) {
					// It seems there was an issue in sending ETC. We don't have to send DFC
					if(logger.isDebugEnabled()) {
						logger.debug("Not sending DFC. Cleaning up call.");
					}
				}
			}
			/*
			 * Clean sip leg if presentCleanup all sip leg according to
			 * their sip session state
			 */
			break;
			case HANDOFF: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: dropCall(session,action) HANDOFF, resource cleanup on common action");
				}
				/*
				 * For handoff no message is required to be sent out
				 */
				action.setDropCallMode(DROP_CALL_MODE.NONE);
			}
			break;
			default: {
				logger.warn("[PH]:: dropCall(session,action) Unhandled INAP call state");
				action.setDropCallMode(DROP_CALL_MODE.NONE);
			}
			break;
			}

			/*
			 * marking call for clean up and cleaning timers/correlation
			 * resources before sending
			 */
			preProcessDroppedCall(tcapSession);

			//dropping the call as per action
			sendDropMessage(tcapSession, action);
		} catch (Exception ex) {
			logger.error("[PH]:: Failed to drop the call.", ex);
		} finally {
			// In postProcessDroppedCall, application is notified about event disconnected.
			// This may cause looping and double cdr writing, where application send END_CALL
			// and again application is notified with event disconnected. 
			// So based on serviceAction we need to stop it in.
			boolean executeServiceAction = (notifyApplicationDropStatus ||
					action.isServiceComplete()) ? false: true;

			postProcessDroppedCall(tcapSession, executeServiceAction);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Inform service that call is dropped and write CDR, executeServiceAction: " 
						+ executeServiceAction);
			}
		}
	}

	/**
	 * This method marks call state, set disconnection time. Also cleans
	 * correlation timers. Invoke this method before sending termination message
	 *
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void preProcessDroppedCall(TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside preProcessDroppedCall");
		}
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
			InapCS2ScfProtocolUtil.cleanupCorrelationResources(tcapSession);
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.TERMINATED);
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit preProcessDroppedCall");
		}
	}

	/**
	 * In case of drop call event, this method sends appropriate INAP message to
	 * the switch.
	 *
	 * @param tcapSession represents an instance of TcapSession
	 * @param action      represents an instance of Action
	 * @throws Exception
	 */
	public static void sendDropMessage(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendDropMessage");
		}
		DROP_CALL_MODE dropCallMode = action.getDropCallMode();

		/*
		 * set cause value for CDRS
		 */
		setCallDataParamsForCDR(callData, action);

		switch (dropCallMode) {
		case INITIAL_ERROR: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send RETURN_ERROR with TC_END");
			}
			sendErrorReqEvent(tcapSession, action);
			sendEndRequestEvent(tcapSession, false);
			break;
		}
		case RELEASE_CALL: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send FCI+REL with TC_END");
			}
			sendFci(tcapSession, false);
			sendReleaseCall(tcapSession, action);
			sendEndRequestEvent(tcapSession, false);
			break;
		}
		case USER_ABORT: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send USER_ABORT");
			}
			sendUAbortRequestEvent(tcapSession, action);
			break;
		}
		case USER_REJECT: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send U_REJECT with TC_CON");
			}
			sendRejectReqEvent(tcapSession, action);
			sendEndRequestEvent(tcapSession, false);
			break;
		}
		case NULL_END: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send NULL_END");
			}
			sendEndRequestEvent(tcapSession, false);
			break;
		}
		case NULL_END_PREARRANGED: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send NULL_END_PREARRANGED");
			}
			sendEndRequestEvent(tcapSession, true);
			break;
		}
		case CONTINUE: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send FCI+CONTINUE with TC_END");
			}
			// set sendMode as TC_END - explictly
			action.setSendMode(SEND_MODE.END);
			sendContinue(tcapSession, action);
			break;
		}
		case NONE: {
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Do nothing for drop call mode NONE");
			}
			break;
		}
		}
	}

	/**
	 * This method notifies service that call is dropped and executres actions
	 * reurned from service Also writes CDR after service notifictaion. invoke
	 * thsi method afters ending termination mesage
	 *
	 * @param tcapSession          represents the instance of TcapSession
	 * @param executeServiceAction represents the instance of boolean
	 */
	public static void postProcessDroppedCall(TcapSession tcapSession, boolean executeServiceAction) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside postProcessDroppedCall");
		}
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		/*
		 * Set the call state again just to handle the case where
		 * preProcessDroppedCall() is not called due to some exception during
		 * execution.
		 */
		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.TERMINATED);

		// No need to notify application if received END_CALL from service. 
		if(executeServiceAction) {

			/*
			 * Notify service that call is dropped
			 */
			notifyCallDropped(tcapSession, executeServiceAction);

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Write CDR");
			}
		}

		/*
		 * Set the call disconnect time again just to make sure that CDR is
		 * written properly using call disconnect time. This is to handle the
		 * case where preProcessDroppedCall() is not called due to some
		 * exception during execution.
		 */
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());

		/*
		 * write CDR, CDR should be written after notifying service
		 */
		if (!PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {
			InapCS2ScfProtocolUtil.writeServiceCdr(tcapSession);
		}

		/*
		 * Assist appsession cleanup moved after writing CDRs. otherwise
		 * P-CDR-INFO will be sent instead of DSI CDR so moved after writing
		 * CDR; This will also mean if ABORT/ER/REJ/RE/timeout is recived after
		 * assist SIP leg is cleaned
		 */
		SipApplicationSession sipApplicationSession = InapCS2ScfProtocolUtil.getAssistAppSession(tcapSession,
				PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipSessionsUtil());

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

		String serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		if(PhUtilityServices.getInstance(serviceId).getAppChainManager() != null) {
			boolean isRemoved = PhUtilityServices.getInstance(serviceId).getAppChainManager().removeTriggeredServices(serviceId, sipApplicationSession, tcapSession);
			if(logger.isDebugEnabled()) {
				logger.debug("[PH]:: triggeredServices removed : " + isRemoved);
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit postProcessDroppedCall");
		}
	}

	/**
	 * This method is called from sendDropMessage, and is responsible for
	 * setting relevant information regarding CDR into CallData object being
	 * passed.
	 *
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 */
	public static void setCallDataParamsForCDR(CallData callData, Action action) {
		DROP_CALL_MODE dropCallMode = action.getDropCallMode();
		int releaseCauseValue = action.getReleaseCauseValue();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: setCallDataParamsForCDR:: Drop call mode is " + dropCallMode);
		}

		/*
		 * setting cause value
		 */
		switch (dropCallMode) {
		case RELEASE_CALL: {
			String valueFromMsg = (String) callData.get(CallDataAttribute.P_CAUSE_VALUE_FROM_MSG);

			if (!(Boolean.valueOf(valueFromMsg))) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Set cause value from action " + action.getReleaseCauseValue());
				}
				if(StringUtils.isNotBlank(valueFromMsg) && !StringUtils.equalsIgnoreCase(valueFromMsg, "0")){
					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, action.getReleaseCauseValue());
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Cause value in callData is " + releaseCauseValue);
				}
			}
			break;
		}
		case NULL_END: {
			if (releaseCauseValue <= 0 && action.getReleaseCauseValue() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Set release cause value from action to " + action.getReleaseCauseValue());
				}
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, action.getReleaseCauseValue());
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Release cause value in callData is " + releaseCauseValue);
				}
			}
			break;
		}
		case INITIAL_ERROR: {
			if (action.getReleaseCauseValue() == 6) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Set attempted indicator to 5 as action cause value is 6");
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
					logger.debug("[PH]:: Set release cause value to 41");
				}
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Release cause value in callData is " + releaseCauseValue);
				}
			}
			break;
		}
		}
	}

	/**
	 * This method is called by protocol handler for sending END request event.
	 *
	 * @param tcapSession    represents the instance of TcapSession
	 * @param preArrangedEnd represents the instance of boolean
	 * @throws Exception
	 */
	static void sendEndRequestEvent(TcapSession tcapSession, boolean preArrangedEnd) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendEndRequestEvent");
		}
		EndReqEvent endReqEvent = new EndReqEvent(src, tcapSession.getDialogueId());
		
		SccpUserAddress origAddr=InapCS2ScfProtocolUtil.updateCallingAddress(callData, leg2Data);
		endReqEvent.setOriginatingAddress(origAddr);
		
		SccpUserAddress destAddr=InapCS2ScfProtocolUtil.updateCalledAddress(callData, leg2Data);
		endReqEvent.setDestinationAddress(destAddr);
		
		//added for Protcol Version changes
		HashMap<String,Integer> hashMap =  InapCS2ScfProtocolUtil.getProtocolVersionMap();
		String serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		if(hashMap.containsKey(serviceId)) {
			Integer value = hashMap.get(serviceId);
			endReqEvent.setProtocolVersion(value);
		}
		
		if (preArrangedEnd) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send Pre-arranged END");
			}
			endReqEvent.setTermination(DialogueConstants.TC_PRE_ARRANGED_END);
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send Basic END");
			}
			endReqEvent.setTermination(DialogueConstants.TC_BASIC_END);
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		DialoguePortion dialoguePortion = (DialoguePortion) legData.get(LegDataAttributes.P_DIALOG_PORTION);
		if (dialoguePortion != null) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Set Dialogue portion");
			}
			endReqEvent.setDialoguePortion(dialoguePortion);
		}
		if (leg2Data != null) {
			String returnOption= (String) leg2Data.get(LegDataAttributes.P_RETURN_OPTION);
		    
		    if(returnOption != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug("[PH]:: return Option is "+ returnOption);
		    	}
		    	endReqEvent.setReturnOption(returnOption);
		    }
		    String CD_PTY_NO_PC =(String) legData.get(LegDataAttributes.CD_PTY_NO_PC);
			String CG_PTY_NO_PC = (String) legData.get(LegDataAttributes.CG_PTY_NO_PC);
			if(StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CG_PTY_NO_PC,"1")) {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to true in in orig addr endReqEvent1 in inapcs2");
				}
				destAddr.setPtyNoPC(true);
			}else {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to false in orig addr endReqEvent1 in inapcs2");
				}
				destAddr.setPtyNoPC(false);
			}
			if(StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"true") || StringUtils.equalsIgnoreCase(CD_PTY_NO_PC,"1")) {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to true in dest addr endReqEvent1 in inapcs2");
				}
				origAddr.setPtyNoPC(true);
			}else {
				if(logger.isDebugEnabled()) {
					logger.debug("Setting PtyNoPc to false in dest addr endReqEvent1 in inapcs2");
				}
				origAddr.setPtyNoPC(false);
			}
			endReqEvent.setOriginatingAddress(origAddr);
			endReqEvent.setDestinationAddress(destAddr);
		    
		    String sequenceControl= (String) leg2Data.get(LegDataAttributes.P_SEQUENCE_CONTROL);
		    
		    if(sequenceControl != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug("[PH]:: sequenceControl is "+ sequenceControl);
		    	}
		    	endReqEvent.setSequenceControl(sequenceControl);
		    }
		    
		    String messagePriority= (String) leg2Data.get(LegDataAttributes.P_MESSAGE_PRIORITY);
		    
		    if(messagePriority != null) {
		    	if(logger.isDebugEnabled()) {
		    		logger.debug("[PH]:: messagePriority is "+ messagePriority);
		    	}
		    	endReqEvent.setMessagePriority(messagePriority);
		    }
		}
		
		sendDialogueReq(endReqEvent, callData);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: leave sendEndRequestEvent");
		}
		InapCS2ScfProtocolUtil.setLastInvokeIdStartRange(InapCS2ScfProtocolUtil.getLastInvokeIdEndRange(callData) + 1, callData);
		InapCS2ScfProtocolUtil.setLastInvokeIdEndRange(InapCS2ScfProtocolUtil.getLastInvokeId(callData), callData);
	}

	/**
	 * This method is called by protocol handler for sending Release Call.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */
	static void sendReleaseCall(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendReleaseCall");
		}

		byte[] releaseCall = InapCS2ScfProtocolParser.createReleaseCall(callData, action);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit createReleaseCall");
		}
		byte[] rcOpCode = {InapOpCodes.RELEASE_CALL_BYTE};

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, rcOpCode);

		InvokeReqEvent rcInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rcOperation);
		rcInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		rcInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, releaseCall));
		rcInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(rcInvokeReqEvent, callData);
	}

	/**
	 * This method notify service that call is dropped.
	 *
	 * @param tcapSession          represents the instance of TcapSession
	 * @param executeServiceAction represents the instance of boolean
	 */
	public static void notifyCallDropped(TcapSession tcapSession, boolean executeServiceAction) {
		try {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Inside notifyCallDropped");
			}
			CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Notify service that call is dropped");
			}
			ServiceInterface serviceHandler = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event event = new Event(EventType.EVENT_CALL_DROPPED, Protocol.ITUINAPCS2_SCF, null);

			ProtocolRouter.getInstance().execute(event, callData, serviceHandler);

		} catch (Exception ex) {
			logger.error("[PH]:: Error in notifyCallDropped " + ex.getMessage());
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Error in processing notifyCallDropped", ex);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit notifyCallDropped");
		}
	}

	/**
	 * This method is called by PH to Send SCI, RNC, RRBCSM for Arming, CONNECT
	 * with CONTINUE. It internally calls other methods like
	 * sendChargingInformation, sendRequestNotificationChargingEvent
	 * sendRRBCSMForArming, sendConnect.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action.
	 * @throws Exception
	 */
	private static void sendConnectTerm(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendConnectTerm");
			logger.debug("[PH]:: Send FCI, CONNECT with mode: " + action.getSendMode());
		}

		// sendFCI 
		sendFci(tcapSession, false);
		
		if(action.getSendMode() != Action.SEND_MODE.END) {
			// Request Report BCSM Event
			InapCS2BCSMHelper.sendRRBCSMForArming(tcapSession, action, null);
		}

		if (action.getSendMode() == Action.SEND_MODE.END) {
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.INAP_CALL_CONNECTED);
			preProcessDroppedCall(tcapSession);
			// Connect
			sendConnect(tcapSession, CONNECT_TYPE.TERMINATING, action);
			sendEndRequestEvent(tcapSession, false);

			postProcessDroppedCall(tcapSession, true);
		} else {
			// Connect
			sendConnect(tcapSession, CONNECT_TYPE.TERMINATING, action);
			sendContinueRequestEvent(tcapSession);
		}
	}

	/**
	 * This method is for creating INAP component indication event CONNECT
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param connectType represents CONNECT_TYPE
	 * @throws Exception
	 */
	private static void sendConnect(TcapSession tcapSession, CONNECT_TYPE connectType, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendConnect");
			logger.debug("[PH]:: connectType is " + connectType.name());
		}

		byte[] connect = null;
		if (connectType == CONNECT_TYPE.TERMINATING) {
			connect = InapCS2ScfProtocolParser.createConnectForTerm(callData, action);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exit createConnectForTerm");
			}
		} else if (connectType == CONNECT_TYPE.PORTED) {
			/*
			 * Set the call data in appsession as tcap session would be
			 * invalidated in this case on CONNECT + TC-END.
			 */
			SipApplicationSession appSession = InapCS2ScfProtocolUtil.getAppSession(tcapSession);
			appSession.setAttribute(CallData.CALL_DATA, callData);
			connect = InapCS2ScfProtocolParser.createConnectForPort(callData);
		} else if (connectType == CONNECT_TYPE.CORRELATION) {
			tcapSession.setAttribute(PhConstants.FOR_HANDOFF, PhConstants.TRUE);
			SccpUserAddress sua = (SccpUserAddress) legData.get(LegDataAttributes.P_SUA);
			connect = InapCS2ScfProtocolParser.createConnectForHandoff(callData, sua);
		} else {
			logger.error("[PH]:: Invalid connection type");
			connect = InapCS2ScfProtocolParser.createConnectForTerm(callData, action);
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exit createConnectForTerm");
			}
		}

		byte[] connectOpCode = {InapOpCodes.CONNECT_BYTE};

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, connectOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, connect));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(connectInvokeReqEvent, callData);
		// callData.incrementReconnectionCount();
	}

	/**
	 * This method is used for sending component indication event CONNECT with
	 * dialogue indication event TC_END.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action.
	 * @throws Exception
	 */
	private static void sendConnectPort(TcapSession tcapSession, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendConnectPort");
			logger.debug("[PH]:: Send CONNECT with TC_END");
		}

		// Connect
		sendConnect(tcapSession, CONNECT_TYPE.PORTED, action);
		// End Request Event (TC_END)
		sendEndRequestEvent(tcapSession, false);
		// Set disconnect time etc as TC-END has been sent
		preProcessDroppedCall(tcapSession);
		// notify service and write CDRs
		postProcessDroppedCall(tcapSession, true);
	}

	/**
	 * This method is for sending component indication event CONNECT for
	 * handoff. So it sends RRBCSM for disarming followed by CONNECT with
	 * TC_END.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action.
	 * @throws Exception
	 */
	public static void sendConnectHandoff(TcapSession tcapSession, Action action) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendConnectHandoff");
		}

		// Request Report BCSM Event
		String leg1TriggersArmed = (String) tcapSession.getAttribute(PhConstants.LEG1_TRIGGERS_ARMED);
		if (leg1TriggersArmed != null && leg1TriggersArmed.equals(PhConstants.TRUE)) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Send RRBCSM for disarming");
			}
			InapCS2BCSMHelper.sendRRBCSMForDisarming(tcapSession, action);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Send CONNECT with TC_END");
		}
		// Connect
		sendConnect(tcapSession, CONNECT_TYPE.CORRELATION, action);

		// End Request Event (TC_END)
		sendEndRequestEvent(tcapSession, false);
	}


	public static void performCharging(TcapSession tcapSession, CallData callData, Action action) {

	}

	public static void sendHttpRequest(TcapSession tcapSession, CallData callData, Action action) {

	}

	public static void sendLsRequest(TcapSession tcapSession, CallData callData, Action action) {

	}

	public static void disconnectTerm(TcapSession tcapSession, CallData callData, Action action) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside disconnectTerm() ");
		}
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		try {
			if(action.getDropCallMode() == Action.DROP_CALL_MODE.DISCONNECT_TERM_LEG) {
				sendDisconnectLeg(tcapSession, action);
			} else {
				sendUAbortRequestEvent(tcapSession, action);
			}
		} catch (Exception e) {
			logger.error("[PH]:: Exception thrown while disconnecting term", e);
			e.printStackTrace();
		}
	}

	/**
	 * This method is used by protocol handler to send Disconnect Leg. 
	 * Disconnect leg is supported in INCAP CS2 only. 
	 * @param tcapSession
	 * @param action
	 */
	private static void sendDisconnectLeg(TcapSession tcapSession, Action action) throws Exception{
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendDisconnectLeg");
		}

		byte[] disconnectLeg = InapCS2ScfProtocolParser.createDisconnectLeg(callData, action);
		byte[] discOpcode = {InapOpCodes.DISCONNECT_LEG_BYTE};

		Operation rcOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, discOpcode);

		InvokeReqEvent erInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rcOperation);
		erInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		erInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, disconnectLeg));
		erInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);	

		// We will get return result for Disconnect Leg. The return result may contain 
		// either opcode as Disconnect leg or Invoke ID. Setting up Invoke ID in Call data 
		// to correlate return result. 
		callData.set(CallDataAttribute.P_DISCONNECT_LEG_INVOKE_ID, erInvokeReqEvent.getInvokeId());

		sendComponentReq(erInvokeReqEvent, callData);
		sendContinueRequestEvent(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: leave createEntityRelease");
		}

	}

	/**
	 * This method is called by protocol handler for sending UAbort request
	 * event.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action
	 * @throws Exception
	 */
	static void sendUAbortRequestEvent(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendUAbortRequestEvent");
		}

		UserAbortReqEvent uAbortReqEvent = new UserAbortReqEvent(src, tcapSession.getDialogueId());

		int reason = action.getReleaseCauseValue();
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Set U-Abort Reason=" + reason);
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
				logger.debug("[PH]:: set dialogue portion");
			}
			uAbortReqEvent.setDialoguePortion(dialoguePortion);
		}
		sendDialogueReq(uAbortReqEvent, callData);
	}

	/**
	 * Prepare Uabort INfo with hardocded object identifier
	 *
	 * @param code represents integer representation of UAbort Info
	 * @return an instance of byte[]
	 */
	private static byte[] getInformationBytes(int code) {
		byte[] uAbortInfo = new byte[]{(byte) 0x28, (byte) 0x0F, (byte) 0x06, (byte) 0x08, (byte) 0x02, (byte) 0x83, (byte) 0x38, (byte) 0x66, (byte) 0x03, (byte) 0x02, (byte) 0x06, (byte) 0x00, (byte) 0xA0, (byte) 0x03, (byte) 0x0A, (byte) 0x01, (byte) 0x01};

		int pos = uAbortInfo.length - 1;
		uAbortInfo[pos] = (byte) code;
		return uAbortInfo;
	}

	public static void redirect(TcapSession tcapSession, CallData callData, Action action) throws Exception{
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Manually setting the SendMode as END for redirect case.");
		}
		action.setSendMode(Action.SEND_MODE.END);
		InapCS2ScfProtocolHelper.connectTerm(tcapSession, callData, action);
	}

	public static Action[] processAcr(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) throws Exception {

		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processAcr");
		}

		// parse ACR
		try {
			InapCS2ScfProtocolParser.parseAcr(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error("[PH]:: ASN pasring Exception in ACR.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ACR_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error("[PH]:: ParameterOutOfRangeException in ACR.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ACR_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ACR);

		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting processACR() .....");
		}
		return null;
	}


	public static void sendContinueComponent(TcapSession tcapSession, CallData callData, Action action) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendContinueComponent");
		}

		byte[] cont = InapCS2ScfProtocolParser.createContinue(callData);
		byte[] contOpCode = {InapOpCodes.CONTINUE_BYTE};

		Operation continueOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, contOpCode);

		InvokeReqEvent continueInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), continueOperation);
		continueInvokeReqEvent.setInvokeId(InapCS2ScfProtocolUtil.getNextInvokeId(callData));
		//continueInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, cont));
		continueInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		sendComponentReq(continueInvokeReqEvent, callData);

		//		if(action.isSendDialogue()) {
		//			sendContinueRequestEvent(tcapSession);
		//		}
	}

	/**
	 * This method is used to process the Assist Request Instructions
	 *
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws Exception
	 */
	public static Action[] processARI(InvokeIndEvent invokeIndEvent, TcapSession tcapSession) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside processARI");
		}
		// parse ARI
		try {
			InapCS2ScfProtocolParser.parseAri(invokeIndEvent, callData);
		} catch (ASNParsingException ape) {
			logger.error("[PH]:: ASN pasring Exception in ARI.", ape);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ARI_ASN_PARSING_FAIL);
			return getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (ParameterOutOfRangeException pore) {
			logger.error("[PH]:: ParameterOutOfRangeException in ARI.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ACR_PARAM_OUT_OF_RANGE);
			return getOutOfRangeParamterAction(tcapSession, MESSAGE.ACR);

		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting processARI() .....");
		}
		/*
		 * When ARI is received we need to cleanup the correlation id and send
		 * the connect to resource to IP media server
		 */
		InapCS2ScfProtocolUtil.cleanupCorrelationResources(tcapSession);
		InapCS2MediaServerHelper.sendConnectToResource(tcapSession, callData, null);
		return null;
	}

	/**
	 * This method is called to start the CDR timer for the intermediate CDRs
	 * @param tcapSession represents the instance of TcapSession
	 */
	private void startCdrTimer(TcapSession tcapSession, CallData callData) {
		SipApplicationSession appSession = InapCS2ScfProtocolUtil.getAppSession(tcapSession);
		String dialogId = (String) callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		long initialDelay = 0L;
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Start Intermediate CDR Timer");
		}
		/*
		 * moved before timer creation to replicate updated value
		 */
		CommonUtils.setAppSessionTimeout(appSession, (int) (initialDelay * 60000) + 5, dialogId);
		InapCS2ScfProtocolUtil.startTimer(tcapSession, initialDelay, false, PhConstants.CDR_TIMER);
	}

	/**
	 * It defines the different connect types possible in INAP call.
	 */
	private static enum CONNECT_TYPE {
		TERMINATING, PORTED, CORRELATION
	}

	/**
	 * This method is called by service to call service comeplte
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void serviceComplete(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(" Call serviceComplete on app chain manager ");
		}

		String serviceId=action.getApplicationName();

		/*
		 * if service id not provided by applictaion then use current service id ,application should set this because if applictaion calls
		 * service comeplete after invokeservice chaining then current service id in calldata will change to next service id , so applictaion should
		 * specify service id on calling service complete
		 */
		if(serviceId==null){
			serviceId = (String) callData
					.get(CallDataAttribute.SERVICE_ID);
		}
		PhUtilityServices.getInstance(serviceId).getAppChainManager().serviceComplete(serviceId, action.isNotifyPrevService(), action.getEvent(), null, tcapSession);


	}

	/**
	 * This method is called when service returns invoke service chaining action
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception 
	 */
	public static boolean invokeServiceChaining(TcapSession tcapSession,
			CallData callData, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug(" invokeServiceChaining Entering ...");
		}

		if(!action.isInvokeServiceChaining()){

			if(logger.isDebugEnabled()){
				logger.debug("No need to invoke chaining as flag is not set by application..so connecting to term"); 
			}
			return false;
		}

		String currentSvcId = (String) callData
				.get(CallDataAttribute.SERVICE_ID);
		String prevSvcId=(String) callData
				.get(CallDataAttribute.PREV_SERVICE_ID);

		Event event = new Event(EventType.EVENT_INITIAL,
				Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG1.name());

		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		int  sua=(Integer) leg1Data.get(LegDataAttributes.P_OPC);

		String origInfo=sua+"|" +callData.get(CallDataAttribute.P_SERVICE_KEY);

		if (logger.isDebugEnabled()) {
			logger.debug(" Current service id is  "+ currentSvcId +" Prev svc id "+prevSvcId);
		}

		AseAppChainManager acm=PhUtilityServices.getInstance(currentSvcId).getAppChainManager();

		/**
		 * get calling called and origin information if found in chanied map
		 */
		Map<String,Map<CallChainedAttributes, Object>> chainingMap =(Map<String,Map<CallChainedAttributes, Object>>)callData.get(CallDataAttribute.P_SVC_CHAINING_MAP);

		Object modCallingNum=null;
		Object modCalledNum=null;

		Object modOrigInfo=null;

		if (chainingMap != null && chainingMap.get(currentSvcId) != null) {

			Map<CallChainedAttributes, Object> chainedAttributes = (Map<CallChainedAttributes, Object>) chainingMap
					.get(currentSvcId);
			modCallingNum = chainedAttributes
					.get(CallChainedAttributes.MODIFIED_CALLING_NUM);
			modCalledNum = chainedAttributes
					.get(CallChainedAttributes.MODIFIED_CALLED_NUM);

			if (chainedAttributes
					.get(CallChainedAttributes.MODIFIED_ORIGIN_INFO) != null) {
				origInfo = (String) chainedAttributes
						.get(CallChainedAttributes.MODIFIED_ORIGIN_INFO);
			}

			if (chainedAttributes.get(CallChainedAttributes.CHAIN_START_TIME) == null) {
				chainedAttributes.put(CallChainedAttributes.CHAIN_START_TIME,
						callData.get(CallDataAttribute.P_CALL_START_TIME));
			}
			chainedAttributes.put(CallChainedAttributes.END_TIME, new Date());

		} else {

			LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

			modCallingNum = leg1Data.get(LegDataAttributes.P_CALLING_PARTY);
			modCalledNum = leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);
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

			/**
			 * Reset N/W transactions
			 */

			InapCS2ScfProtocolUtil.resetNetworkTransactions(callData);

			/**
			 * Creating chaining data for next service and updating it in chaining map
			 */
			Map<CallChainedAttributes, Object> nexChainedMap= new HashMap<CallChainedAttributes, Object>();

			nexChainedMap.put(CallChainedAttributes.MODIFIED_CALLING_NUM, modCallingNum);
			nexChainedMap.put(CallChainedAttributes.MODIFIED_CALLED_NUM, modCalledNum);
			nexChainedMap.put(CallChainedAttributes.CHAIN_START_TIME, new Date());
			nexChainedMap.put(CallChainedAttributes.MODIFIED_ORIGIN_INFO, origInfo);

			if(chainingMap==null){
				chainingMap = new HashMap<String,Map<CallChainedAttributes, Object>>();
				callData.set(CallDataAttribute.P_SVC_CHAINING_MAP,chainingMap);
			}

			chainingMap.put(nextServiceId, nexChainedMap);

			//	leg1Data.set(LegDataAttributes.P_CALLED_PARTY,leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER));

			if (logger.isDebugEnabled()) {
				logger.debug(" next service found is "+nextServiceId + " update serviceid in call data and changed map "+ chainingMap);
			}

			callData
			.set(CallDataAttribute.SERVICE_ID,nextServiceId);
			callData
			.set(CallDataAttribute.PREV_SERVICE_ID,currentSvcId);

			// reset Final CDR flag so that next service can write CDR
			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN, PhConstants.FALSE);

			tcapSession.setAttribute(PhConstants.SERVICE_ID, nextServiceId);


			if (logger.isDebugEnabled()) {
				logger.debug(" call invokeServiceChaining on app chain manager  !!! ");
			}

			acm.invokeServiceChaining(
					currentSvcId,
					nextServiceId,
					addressesMap,
					event, tcapSession, null, action.isRemainInPath());//	action.getEvent()

			if (logger.isDebugEnabled()) {
				logger.debug(" Service chaining is invoked !!! ");
			}


			MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.ITUINAPCS2_SCF);
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
						InapCS2ScfRelReasonCode.REJECT_NO_NEXT_SERVICE);
				/*
				 * Do not mark sip session as ready to invalidate so that
				 * cleanupSipLeg() can give an attempt to cleanup this leg.
				 */
				dropCall(tcapSession, callData);

				return true;
			}
			return false;

		}
	}

	private static void incrementTcapCounters(EventObject eventObject) {
		MeasurementCounter measurementCounter = PhMeasurementService.getInstance().getMeasurementCounter(Protocol.ITUINAPCS2_SCF);
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

	/**
	 * 
	 * @param tacpSession
	 * @param callData
	 */
	public static void sendSipInviteFrmInapCS2(TcapSession tacpSession, CallData callData) throws Exception  {

		if(logger.isDebugEnabled()){
			logger.debug("Enter sendSipInviteFrmInapCS2");
		}
		boolean outboundGwNotFound = true;

		try {
			if(logger.isDebugEnabled()){
				logger.debug("Inside sendSipInviteFrmInapCS2 ");
			}

			createSipInviteFrmInapCS2(tacpSession, callData);
			outboundGwNotFound = false;
			if(logger.isDebugEnabled()){
				logger.debug("sendSipInviteFrmInapCS2: exit");
			}

		} catch (ServletParseException e) {
			logger.info("Exception inside sendSipInviteFrmInapCS2:: " + e);
		} catch (IOException e) {
			logger.info("Exception inside sendSipInviteFrmInapCS2:: " + e);
		} catch (ServletException e) {
			logger.info("Exception inside sendSipInviteFrmInapCS2:: " + e);
		}

		if(outboundGwNotFound) {
			Action lastAction = (Action) callData.get(CallDataAttribute.P_LAST_CALL_ACTION);
			String legId = lastAction.getLeg();

			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
			legData.set(LegDataAttributes.P_CAUSE_CODE, InapCS2ScfRelReasonCode.OBGW_NOT_AVAILABLE);

			logger.error(tacpSession.getDialogueId() + ": OutboundGateway not found");
			Event event = new Event(EventType.EVENT_GW_NOT_FOUND, Protocol.ITUINAPCS2_SCF, legId);
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
	private static void createSipInviteFrmInapCS2(TcapSession tcapsesion, CallData calldata)
			throws IOException, ServletException {
		if(logger.isDebugEnabled()){
			logger.debug("Inside createSipInviteFrmInapCS2::enter ");
		}

		String origLegCallId = (String) calldata
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		LegData legdata1 = (LegData) calldata.get(CallDataAttribute.P_LEG1);
		legdata1.set(LegDataAttributes.P_LEG_SS7_STATE, InapCallStates.PSX_ROUTING);
		if(logger.isDebugEnabled()){
			logger.debug("LegData1 Inside createSipInviteFrmInapCS2 is:: " + legdata1);
		}

		LegData legdata2 = (LegData) calldata.get(CallDataAttribute.P_LEG2);
		if(logger.isDebugEnabled()){
			logger.debug("LegData2 Inside createSipInviteFrmInapCS2 is:: " + legdata2);
		}

		String ip =null;
		int port =5060;
		String eqspoolId = (String)legdata2.get(LegDataAttributes.NP_GW_POOL_ID);
		if(logger.isDebugEnabled()){
			logger.debug("NP_GW_POOL_ID in createSipInviteFrmInapCS2::"+eqspoolId);
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
				logger.debug("Creating Caller URI ::");
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

		SipApplicationSession tcapApplicationSession = InapCS2ScfProtocolUtil.getAppSession(tcapsesion);

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
		// String appName =
		// PhUtilityServices.getInstance((String)calldata.get(CallDataAttribute.SERVICE_ID)).getServiceHandler().getApplicationName();
		// logger.info("the sip request created for appliaction "+appName);
		if(logger.isDebugEnabled()){
			logger.debug("srviceHandler: " + srviceHandler);
		}

		SipSession legSipSession = sipRequest.getSession();

		legdata1.set(LegDataAttributes.P_SESSION_ID, legSipSession.getId());
		if(logger.isDebugEnabled()){
			logger.debug("tcap-session at the time of creating Invite msg: " + tcapsesion);
		}

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
		action.setProtocol(Protocol.ITUINAPCS2_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		actionArr[0] = action;
		calldata.set(CallDataAttribute.P_CURRENT_ACTION, action);

		legSipSession.setHandler(srviceHandler.getServletName());
		if(logger.isDebugEnabled()){
			logger.debug("setting servlet as: " + srviceHandler.getServletName());
		}
		if(logger.isDebugEnabled()){
			logger.debug("sipRequest is created  as" + sipRequest);
			logger.debug("sending INVITE request");
		}
		sipRequest.send();
		if(logger.isDebugEnabled()){
			logger.debug("Inside createSipInviteFrmInapCS2::exit");
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
			logger.debug(origLegCallId + ":: Inside startApplicationTimer: "
					+ action.getTimerName() + ", timervalue:" + action.getTimerTimeInMills());
		}

		String timerName = PhConstants.APP_TIMER;

		if (action.getTimerName() != null) {
			timerName = timerName + action.getTimerName();
		}
		InapCS2ScfProtocolUtil.startTimer(appSession, action.getTimerTimeInMills(),
				false, timerName);

		ServiceInterface serviceHandler = PhUtilityServices
				.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
		Event appEvent = new Event(EventType.EVENT_APP_TIMER_STARTED, Protocol.ITUINAPCS2_SCF, action.getLeg());

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
		InapCS2ScfProtocolUtil.stopTimer(appSession, timerName);

		try{
			ServiceInterface serviceHandler = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
			Event appEvent = new Event(EventType.EVENT_APP_TIMER_STOPPED, Protocol.ITUINAPCS2_SCF, action.getLeg());

			ProtocolRouter.getInstance().execute(appEvent, callData, serviceHandler);
		}catch(Exception ex){
			logger.error("Error occured while stopping application timer : " + action.getTimerName() );
			logger.error("Error occured : " + ex);
			throw ex;
		}
	}

	/**
	 * Method used to send RRBCSM + Continue
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	private static void sendContinue(TcapSession tcapSession, Action action) throws Exception {

		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendContinue");
		}

		// send FCI - FCI may be sent in either of the case
		sendFci(tcapSession, false);

		if(action.getSendMode() != Action.SEND_MODE.END) {

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + " [PH]:: Sending RRBCSM along with continue as send mode is continue");
			}

			// Request Report BCSM Event
			InapCS2BCSMHelper.sendRRBCSMForArming(tcapSession, action, null);
		}

		// send Continue
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Sending Continue");
		}

		sendContinueComponent(tcapSession, callData, action);

		if(action.getSendMode() == Action.SEND_MODE.END){
			preProcessDroppedCall(tcapSession);
			sendEndRequestEvent(tcapSession, false);
			postProcessDroppedCall(tcapSession, true);
		}else{
			sendContinueRequestEvent(tcapSession);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Exiting sendContinue");
		}
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

}
