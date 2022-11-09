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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;
import org.apache.log4j.MDC;

import com.agnity.ain.operations.AinOpCodes;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.ainscf.gr533.Gr533MessageHandler;
import com.agnity.ph.ainscf.lidb.BNSQuery;
import com.agnity.ph.ainscf.lidb.LidbQueryHandler;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.SS7ProtocolHandler;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.measurement.PhMeasurementService;
import com.agnity.ph.sip.SipProtocolConfig;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapProvider;
import com.genband.tcap.provider.TcapSession;

import jain.MandatoryParameterNotSetException;
import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.DialogueIndEvent;
import jain.protocol.ss7.tcap.TcapConstants;
import jain.protocol.ss7.tcap.component.ErrorIndEvent;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.RejectIndEvent;
import jain.protocol.ss7.tcap.component.ResultIndEvent;
import jain.protocol.ss7.tcap.dialogue.EndIndEvent;

/**
 * This class is used to handle AIN Protocol Specific events. this handler class
 * is invoked by ProtocolHandlerServlet for processing of dialog and components
 * events for AIn Protocol. This class processes these events and delegates the
 * processing of the messages to AIN Protocol Helper class
 * 
 * A new CallData Object is created on initial INFO_ANALYZE/INFO_COLLECTED
 * invokcation and saved in TCapSession and appsession for this call. This
 * object then remains same for all the subsequent messages and works as a data
 * object for whole call
 */
public class AinScfProtocolHandler implements SS7ProtocolHandler {

	private static final AinScfProtocolHandler INSTANCE = new AinScfProtocolHandler();
	private static Logger logger = Logger.getLogger(AinScfProtocolHandler.class);

	private AinScfProtocolHandler() {
	}

	public static AinScfProtocolHandler getInstance() {
		return INSTANCE;
	}
	
	public static ServiceInterface serviceInterfaceRef;
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.agnity.ph.common.ProtocolHandler#executeAction(com.agnity.mphdata.common.
	 * CallData, com.agnity.mphdata.common.Action)
	 */
	public void executeAction(CallData callData, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] executeAction() Enter with callData and action ");
		}

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		TcapSession tcapSession = PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
				.getTcapProvider().getTcapSession(dialogId);

		AinScfProtocolHelper.traceMessage(callData, tcapSession);
		
		if (action == null|| tcapSession==null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: [PH] Do nothing as action array is null or tcap session is null/invalidated");
			}
			return;
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] Action to be performed is " + action.getActionType());
		}

		callData.set(CallDataAttribute.P_LAST_CALL_ACTION, action);
		switch (action.getActionType()) {
		case ACTION_CONNECT: {
			//
			AinScfProtocolHelper.connectTerm(tcapSession, callData, action);
			break;
		}
		case ACTION_DISCONNECT: {
			AinScfProtocolHelper.disconnctTerm(tcapSession, callData, action);
			break;
		}
		case ACTION_END_CALL: {
			AinScfProtocolHelper.dropCall(tcapSession, callData, action);
			break;
		}
		case ACTION_REDIRECT: {
			AinScfProtocolHelper.redirect(tcapSession, callData, action);
			break;
		}
		case ACTION_CONNECT_MS: {
			AinScfProtocolHelper.initiateMediaServerConnection(tcapSession, callData, action);
			break;
		}
		case ACTION_DISCONNECT_MS: {
			AinScfProtocolHelper.disconnectIvr(tcapSession, callData, action);
			break;
		}
		case ACTION_PLAY: {
			//			AinScfProtocolHelper.sendPlayAnnouncement(tcapSession, callData,
			//					action);
			AinScfProtocolHelper.handlePlayAnnouncement(tcapSession, callData,
					action);
			break;
		}
		case ACTION_PLAY_COLLECT: {
			//			AinScfProtocolHelper.sendPlayAndCollect(tcapSession, callData,
			//					action);
			AinScfProtocolHelper.handlePlayAndCollect(tcapSession, callData,
					action);
			break;
		}
		case ACTION_PLAY_RECORD: {
			AinScfProtocolHelper.playAndRecord(tcapSession, callData, action);
			break;
		}
		case ACTION_CALL_HB: {
			// AinScfProtocolHelper.callHeartBeat(tcapSession, callData,
			// action);
			break;
		}
		case ACTION_PROCESS_CALLS: {
			break;
		}
		case ACTION_RESYNC_CALL: {
			break;
		}
		case ACTION_HOLD_CALL: {
			break;
		}
		case ACTION_CONTINUE: {
			// AinScfProtocolHelper.sendContinueComponent(tcapSession,
			// callData, action);
			break;
		}
		case ACTION_CHRG: {
			AinScfProtocolHelper.performCharging(tcapSession, callData, action);
			break;
		}
		case ACTION_HTTP_REQ: {
			AinScfProtocolHelper.sendHttpRequest(tcapSession, callData, action);
			break;
		}
		case ACTION_START_TIMER: {
			AinScfProtocolHelper.startApplicationTimer(tcapSession, callData, action);
			break;
		}
		case ACTION_STOP_TIMER: {
			AinScfProtocolHelper.stopApplicationTimer(tcapSession, callData, action);
			break;
		}
		case ACTION_INVOKE_SVC_CHAINING: {
			AinScfProtocolHelper.invokeServiceChaining(tcapSession, callData,
					action);
			break;
		}
		default: {
			logger.error("AinScfProtocolHandler - Requested Action not supported:"+action.getActionType());
			break;
		}
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH] executeAction() Exit with callData and action");
		}
	}

	/**
	 * Tcaplistener API implemented by the service for receiving a callback on a
	 * dialogue indication event occuring on the node.This method check the
	 * primitive type for the dialogue indication event.
	 * <ul>
	 * <li>If Primitive Type is PRIMITIVE_BEGIN (for QwP or QwoP for ANSI) for a new
	 * call then New calldata is created and set in TCAP session. Store the tcap
	 * session reference in appSession as in timeout it would be required.
	 * 
	 * If this PRIMITIVE_BEGIN is for an existing call then validate the message for
	 * FSM state. After validation call the processBegin method of
	 * AinScfProtocolHelper to get next action to be executed. For Invalid dialog
	 * BEGIN, drop call.
	 * <li>For primitive type PRIMITIVE_CONTINUE validate the FSM state and call
	 * processContinue method of AinScfProtocolHelper to get next action to be
	 * executed. For Invalid dialog CONTINUE, drop call.
	 * <li>For primitive type PRIMITIVE_END validate the FSM state and call
	 * processEnd method of AinScfProtocolHelper to get next action to be
	 * executed.For end message no need to execute processing failure action
	 * executeAction(tcapSession, action).Just check if call needs to be cleaned now
	 * or after processing components,drop call if no component is present.For
	 * Invalid dialog END, drop call.
	 * <li>For primitive type PRIMITIVE_USER_ABORT action not executed as already
	 * term message is received executeAction(tcapSession, action).Clean call if no
	 * component is present and notify service and write CDRs.
	 * <li>For primitive type PRIMITIVE_PROVIDER_ABORT and PRIMITIVE_NOTICE action
	 * not executed as already term message is received executeAction(tcapSession,
	 * action).Clean call if no component is present and notify service and write
	 * CDRs.
	 * <li>For unrecognized Primitive Type execute drop action with force false.
	 * </ul>
	 * Always store dialog primitive type before returning from the method. For
	 * MandatoryParameterNotSetException for dialogue event or Failed to process
	 * Dialogue Indication event release the call with CV=41.
	 * 
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @see jain.protocol.ss7.tcap.JainTcapListener#processDialogueIndEvent(jain.protocol.ss7.tcap.DialogueIndEvent)
	 */
	public void processDialogueIndEvent(DialogueIndEvent dialogueIndEvent, ServiceInterface serviceHandler) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processDialogueIndEvent() Enter");
		}
		int dialogueId = 0;
		TcapSession tcapSession = null;
		TcapProvider sTcapProvider = PhUtilityServices.getInstance(serviceHandler.getApplicationName())
				.getTcapProvider();

		CallData callData = null;
		LegData legData = null;
		try {
			dialogueId = dialogueIndEvent.getDialogueId();

			if (logger.isInfoEnabled()) {
				logger.info(dialogueId + "::[PH] Processing Dialogue Indication Event "
						+ dialogueIndEvent.getPrimitiveType());
			}

			tcapSession = sTcapProvider.getTcapSession(dialogueId);

			/*
			 * fetch call data from tcap session
			 */
			callData = AinScfProtocolUtil.getCallData(tcapSession);

			if (callData != null) {
				AinScfProtocolHelper.traceDialog(dialogueIndEvent, callData);

				/*
				 * set last Rx dialogue primitive to validate component against dialogue
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + " ::[PH] Setting rx dialog primitive for future reference");
				}
				callData.set(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE, dialogueIndEvent.getPrimitiveType());
				((MutableInt) callData.get(CallDataAttribute.P_NETWORK_TRANSACTION)).increment();
			}

			switch (dialogueIndEvent.getPrimitiveType()) {
			case TcapConstants.PRIMITIVE_BEGIN: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Primitive Type is PRIMITIVE_BEGIN");
				}

				/*
				 * creating and storing call data
				 */
				if (callData == null) {
					callData = new CallData();
					legData = new LegData();
					callData.set(CallDataAttribute.NP_FT_CALL, true);
					callData.set(CallDataAttribute.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(CallDataAttribute.P_CALL_START_TIME, new Date());
					callData.set(CallDataAttribute.P_LEG1, legData);
					callData.set(CallDataAttribute.P_PROTOCOL, Protocol.AIN_SCF);
					callData.set(CallDataAttribute.P_DIALOG_ID, dialogueId);
					callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, String.valueOf(dialogueId));
					callData.set(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE, dialogueIndEvent.getPrimitiveType());

					SipApplicationSession appSession = AinScfProtocolUtil.getAppSession(tcapSession);

					appSession.setAttribute(PhConstants.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(CallDataAttribute.P_APP_SESSION_ID, appSession.getId());

					/*
					 * Store the tcap session reference in appSession as in timeout it would be
					 * required
					 */
					appSession.setAttribute(PhConstants.TCAP_SESSION_ID, tcapSession.getDialogueId());

					legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.INIT);
					legData.set(LegDataAttributes.P_DIALOG_ID, tcapSession.getDialogueId());
					legData.set(LegDataAttributes.P_CONNECTION_TYPE, ConnectionType.ORIG_CONNECTION);
					tcapSession.setAttribute(CallData.CALL_DATA, callData);
					appSession.setAttribute(CallData.CALL_DATA, callData);
					callData.set(CallDataAttribute.P_TRACE_FLAG, PhConstants.FALSE);
					callData.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(1));

				} else {
					logger.error(dialogueId + ":: Recieveid BEGIN for existing call.");
					/*
					 * not doing anything as call will be cleaned through validate fsm state
					 */
				}
				/*
				 * validating message
				 */
				boolean isValidEvent = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.BEGIN,
						tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Valid BEGIN start processing");
					}
					Boolean dumpCounters = Boolean
							.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.DUMP_COUNTERS));
					if (dumpCounters) {
						PhMeasurementService.getInstance().getMeasurementCounter(Protocol.AIN_SCF)
						.incrementServiceTriggeredCount(serviceHandler.getApplicationName(), false);

					}
					AinScfProtocolHelper.processBegin(dialogueIndEvent, tcapSession);

				} else {
					logger.error(dialogueId + ":: Invalid dialoge BEGIN, drop call");

					/*
					 * invalid event get action from helper and do the same as drop call
					 */
					Action[] action = AinScfProtocolHelper.getOutOfSequenceDialogAction(tcapSession,
							TcapConstants.PRIMITIVE_BEGIN);
					/*
					 * execute drop action with force false;
					 */
					for (Action actionField : action) {
						AinScfProtocolHelper.dropCall(tcapSession, callData, actionField);
					}
				}
				break;
			}
			case TcapConstants.PRIMITIVE_CONTINUE: {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Primitive Type is PRIMITIVE_CONTINUE");
				}

				boolean isValidEvent = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.CONTINUE,
						tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Valid CONTINUE event start processing");
					}

					Action[] action = AinScfProtocolHelper.processContinue(dialogueIndEvent, tcapSession);
					executeAction(tcapSession, action);
				} else {
					logger.error(dialogueId + ":: Invalid dialoge CONTINUE, drop call");

					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_CONTINUE);

					/*
					 * Helper returns out of sequence action for the case; drop call with the action
					 */
					Action[] action = AinScfProtocolHelper.getOutOfSequenceDialogAction(tcapSession,
							TcapConstants.PRIMITIVE_CONTINUE);

					/*
					 * execute drop action with force false;
					 */
					for (Action actionField : action) {
						AinScfProtocolHelper.dropCall(tcapSession, callData, actionField);
					}
				}
				break;
			}
			case TcapConstants.PRIMITIVE_END: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Primitive Type is PRIMITIVE_END");
				}

				boolean isValidEvent = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.END, tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Valid EVENT event start processing");
					}
					EndIndEvent endIndEvent = (EndIndEvent) dialogueIndEvent;

					/*
					 * usually end is sent by service.. its error flow so setting default cause
					 * value as 41 for CDR and marking call as failed call if there is some
					 * component in end it will update CDR Mark cause val as 41 for failed call
					 */
					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

					/*
					 * drop call if no component is present
					 */
					if (!(endIndEvent.isComponentsPresent())) {
						logger.warn(dialogueId + ":: END with no component, drop call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.END_RCVD_WITHOUT_COMP);

						/*
						 * clean call as no component is present
						 */
						AinScfProtocolHelper.preProcessDroppedCall(tcapSession);
						/*
						 * notify service and write CDRs
						 */
						AinScfProtocolHelper.postProcessDroppedCall(tcapSession, true);

					}
				} else {
					logger.error(dialogueId + ":: Invalid dialoge END, drop call");

					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_END);

					Action[] action = AinScfProtocolHelper.getOutOfSequenceDialogAction(tcapSession,
							TcapConstants.PRIMITIVE_END);
					/*
					 * execute drop action with force false;
					 */
					for (Action actionField : action) {
						AinScfProtocolHelper.dropCall(tcapSession, callData, actionField);
					}

				}
				break;
			}
			case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Primitive Type is PRIMITIVE_UNIDIRECTIONAL");
				}
				// creating call data
				if (callData == null) {
					callData = new CallData();
					legData = new LegData();
					callData.set(CallDataAttribute.NP_FT_CALL, true);
					callData.set(CallDataAttribute.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(CallDataAttribute.P_CALL_START_TIME, new Date());
					callData.set(CallDataAttribute.P_LEG1, legData);
					callData.set(CallDataAttribute.P_PROTOCOL, Protocol.AIN_SCF);
					callData.set(CallDataAttribute.P_DIALOG_ID, dialogueId);
					callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, String.valueOf(dialogueId));
					callData.set(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE, dialogueIndEvent.getPrimitiveType());

					SipApplicationSession appSession = AinScfProtocolUtil.getAppSession(tcapSession);

					appSession.setAttribute(PhConstants.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(CallDataAttribute.P_APP_SESSION_ID, appSession.getId());

					/*
					 * Store the tcap session reference in appSession as in timeout it would be
					 * required
					 */
					appSession.setAttribute(PhConstants.TCAP_SESSION_ID, tcapSession.getDialogueId());

					legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.INIT);
					legData.set(LegDataAttributes.P_DIALOG_ID, tcapSession.getDialogueId());
					legData.set(LegDataAttributes.P_CONNECTION_TYPE, ConnectionType.ORIG_CONNECTION);
					tcapSession.setAttribute(CallData.CALL_DATA, callData);
					appSession.setAttribute(CallData.CALL_DATA, callData);
					callData.set(CallDataAttribute.P_TRACE_FLAG, PhConstants.FALSE);
					callData.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(1));

				} else {
					logger.error(dialogueId + ":: Recieveid BEGIN for existing call.");
				}

				// Unidirectional can come anytime. No need to check in FSM

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Valid Unidirectional start processing");
				}
				Boolean dumpCounters = Boolean
						.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.DUMP_COUNTERS));
				if (dumpCounters) {
					PhMeasurementService.getInstance().getMeasurementCounter(Protocol.AIN_SCF)
					.incrementServiceTriggeredCount(serviceHandler.getApplicationName(), false);

				}
				AinScfProtocolHelper.processUnidirectional(dialogueIndEvent, tcapSession);
				break;
			}
			case TcapConstants.PRIMITIVE_USER_ABORT: {

				if (logger.isInfoEnabled()) {
					logger.info(dialogueId + ":: Handling U-ABORT");
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UABORT_RCVD);

				/*
				 * Mark cause val as 31 as u-abort will be rcvd when user hung up and related
				 * events are not armed.
				 */
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 31);

				Action[] action = AinScfProtocolHelper.processUAbort(dialogueIndEvent, tcapSession);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Returned actions are " + action);
				}

				/*
				 * clean the call ..
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::Cleaning the call");
				}

				/*
				 * clean call if no component is present*
				 */
				AinScfProtocolHelper.preProcessDroppedCall(tcapSession);
				/*
				 * notify service and write CDRs
				 */
				AinScfProtocolHelper.postProcessDroppedCall(tcapSession, true);

				break;

			}
			case TcapConstants.PRIMITIVE_PROVIDER_ABORT:
			case TcapConstants.PRIMITIVE_NOTICE: {
				if (logger.isInfoEnabled()) {
					logger.info(dialogueId + ":: Handling P-ABORT/NOTICE");
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.PABORT_NOTICE_RCVD);

				/*
				 * Mark cause val as 41 for failed call
				 */
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Cleaning the call");
				}
				/*
				 * clean call if no component is present
				 */
				AinScfProtocolHelper.preProcessDroppedCall(tcapSession);

				/*
				 * notify service and write CDRs
				 */
				AinScfProtocolHelper.postProcessDroppedCall(tcapSession, true);

				break;
			} default: {
				logger.error(dialogueId + ":: unrecognized Primitive Type " + dialogueIndEvent.getPrimitiveType());

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNKNOWN_DIALOG_RCVD);

				Action[] action = AinScfProtocolHelper.getUnknownDialogAction(tcapSession);

				/*
				 * execute drop action with force false;
				 */
				for (Action actionField : action) {
					AinScfProtocolHelper.dropCall(tcapSession, callData, actionField);
				}
				break;
			}
			}

		} catch (MandatoryParameterNotSetException e) {
			logger.error(dialogueId + ":: MandatoryParameterNotSetException for dialogue event", e);
			CallData newcallData = AinScfProtocolUtil.getCallData(tcapSession);
			if (newcallData != null) {
				newcallData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						AinScfRelReasonCode.DIAGLOG_MANDATORY_PARAM_MIS);
				newcallData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				AinScfProtocolHelper.dropCall(tcapSession, newcallData);
			}
		} catch (Exception ex) {
			logger.error(dialogueId + ":: Failed to process Dialogue Indication event", ex);
			CallData newcallData = AinScfProtocolUtil.getCallData(tcapSession);
			if (newcallData != null) {
				newcallData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.EXCEP_IN_DIALOG_IND);
				newcallData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				AinScfProtocolHelper.dropCall(tcapSession, newcallData);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processDialogueIndEvent() Exit");
		}
	}

	/**
	 * Tcaplistener API implemented by the service for receiving a callback on a
	 * component indication event occurring on the node.This method check the
	 * primitive type for the component indication event. For Primitive Type
	 * PRIMITIVE_INVOKE, PH checks for the operation code.
	 * <ul>
	 * <li>If operation code is INFO_ANALYZED_BYTE, then validate the message for
	 * FSM state.After validation call the processInfoAnalyzed method of
	 * AinScfProtocolHelper to get next action to be executed.If it is a first call
	 * CAS will generate an invite and send a warm up call to service. Helper return
	 * action in case of error so drop call with action drop action with force
	 * false. For no error case do service specific InfoAnalyze processing notify
	 * service to start call execution. For Invalid InfoAnalyze, drop the call.
	 * Similarly for other components similar handling is performed.
	 * <li>for Unknown INVOKE Indication Event, just execute drop action with force
	 * false.
	 * </ul>
	 * For Primitive Type PRIMITIVE_RESULT ,PRIMITIVE_REJECT and PRIMITIVE_ERROR
	 * just execute drop action with force false. Process end dialog if call is not
	 * terminated and last component is processed. For
	 * MandatoryParameterNotSetException for component event or Failed to process
	 * component Indication event release the call with CV=41.
	 * 
	 * @param componentIndEvent represents the instance of ComponentIndEvent
	 * @see jain.protocol.ss7.tcap.JainTcapListener#processComponentIndEvent(jain.protocol.ss7.tcap.ComponentIndEvent)
	 */

	public void processComponentIndEvent(ComponentIndEvent componentIndEvent, ServiceInterface serviceInterface) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processComponentIndEvent() Enter");
		}
		this.serviceInterfaceRef = serviceInterface;
		int dialogueId = 0;
		TcapSession tcapSession = null;
		try {
			dialogueId = componentIndEvent.getDialogueId();
			if (logger.isInfoEnabled()) {
				logger.info(dialogueId + ":: Processing Component Indication Event "
						+ componentIndEvent.getPrimitiveType());
			}

			tcapSession = PhUtilityServices.getInstance(serviceInterface.getApplicationName()).getTcapProvider()
					.getTcapSession(dialogueId);

			CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

			int lastRxDialoguePrimitiveType = -1;

			if (callData != null) {
				
				if (logger.isInfoEnabled()) {
					logger.info(dialogueId + ":: setting  P_LAST_RX_INVOKE_ID value: "
							+ componentIndEvent.getInvokeId());
				}
				
				lastRxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
				legData.set(LegDataAttributes.P_LAST_RX_INVOKE_ID, componentIndEvent.getInvokeId());
				AinScfProtocolHelper.traceComponent(componentIndEvent, callData, false);
			}

			switch (componentIndEvent.getPrimitiveType()) {
			case TcapConstants.PRIMITIVE_INVOKE: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Primitive Type is PRIMITIVE_INVOKE");
				}

				InvokeIndEvent invokeIndEvent = (InvokeIndEvent) componentIndEvent;
				byte[] operCode = invokeIndEvent.getOperation().getOperationCode();

				int operCodeStr = CommonUtils.formatBytesToInt(operCode);
				if (logger.isDebugEnabled()) {
					logger.debug(
							dialogueId + ":: Operation Code is " + operCodeStr + ", OpcCode length:" + operCode.length);
				}
				callData.set(CallDataAttribute.P_AIN_TRIGGERING_MESSAGE, null);
				
				switch (operCodeStr) {
				case AinOpCodes.INFO_ANALYZED_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process INFO_ANALYZED_BYTE");
					}

					/*
					 * Drop the call, if service failed to read configuration
					 */
					if (!PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						AinScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}
					/*
					 * validate
					 */
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.INFO_ANALYZE,
							tcapSession);
					callData.set(CallDataAttribute.P_AIN_TRIGGERING_MESSAGE, "Info_Analyzed");
					if (isValid) {

						/*
						 * process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid INFO_ANALYZE event start processing");
						}

						Action[] action = AinScfProtocolHelper.processInfoAnalyzed(invokeIndEvent, tcapSession,
								PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
								.getCallTraceService());

						if (logger.isDebugEnabled()) {
							logger.debug("[PH] :: processInfoAnalyzed Exit ");
						}
						/*
						 * First SIP-T and AIN call were taking almost 11 seconds to process. To solve
						 * this issue SAS will generate an invite and send a warm up call to service.
						 */
						if (tcapSession.getAttribute(PhConstants.WARMUP_HEADER) != null) {
							try {
								if (logger.isInfoEnabled()) {
									logger.info(dialogueId + ":: This is a warm-up AIN call.");
								}
								legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERMINATED);
								/*
								 * Notify service that call is dropped
								 */
								AinScfProtocolHelper.notifyCallDropped(tcapSession, false);
								SipApplicationSession appSession = AinScfProtocolUtil.getAppSession(tcapSession);
								tcapSession.invalidate();

								if (appSession != null && appSession.isValid()) {
									appSession.invalidate();
								}
								/*
								 * Note:- DO Not write CDR for warm-up calls
								 */
							} catch (Throwable e) {
								logger.warn(dialogueId + " :: Failed to handle AIN warm-up call" + e.getMessage());
							}
							return;
						}

						/*
						 * special case of InfoAnalyze for tracing as tracing is decided after
						 * processing InfoAnalyze
						 */
						AinScfProtocolHelper.traceComponent(componentIndEvent, callData, true);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);
						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to start call execution");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}

							Event event = new Event(EventType.EVENT_INITIAL, Protocol.AIN_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);

						}
					} else {
						logger.error(dialogueId + ":: Invalid InfoAnalyzed, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXP_INFO_ANALYZE);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.INFO_ANALYZE);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}

					break;
				}
				case AinOpCodes.TERMINATION_ATTEMPT_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process TERMINATION_ATTEMPT");
					}

					// Drop call if service is not able to read configuration
					if (!PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						AinScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}

					// check if event is valid as per FSM
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.TERMINATION_ATTEMPT,
							tcapSession);
					callData.set(CallDataAttribute.P_AIN_TRIGGERING_MESSAGE, "Termination_Attempt");
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid TERMINATION_ATTEMPT event start processing");
						}

						Action[] action = AinScfProtocolHelper.processTerminationAttempt(invokeIndEvent, tcapSession,
								PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
								.getCallTraceService());
						if (logger.isDebugEnabled()) {
							logger.debug("[PH] :: processTerminationAttempt Exit ");
						}
						/*
						 * First SIP-T and AIN call were taking almost 11 seconds to process. To solve
						 * this issue SAS will generate an invite and send a warm up call to service.
						 */
						if (tcapSession.getAttribute(PhConstants.WARMUP_HEADER) != null) {
							try {
								if (logger.isInfoEnabled()) {
									logger.info(dialogueId + ":: This is a warm-up AIN call.");
								}
								legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERMINATED);
								/*
								 * Notify service that call is dropped
								 */
								AinScfProtocolHelper.notifyCallDropped(tcapSession, false);
								SipApplicationSession appSession = AinScfProtocolUtil.getAppSession(tcapSession);
								tcapSession.invalidate();

								if (appSession != null && appSession.isValid()) {
									appSession.invalidate();
								}
								/*
								 * Note:- DO Not write CDR for warm-up calls
								 */
							} catch (Throwable e) {
								logger.warn(dialogueId + " :: Failed to handle AIN warm-up call" + e.getMessage());
							}
							return;
						}

						/*
						 * special case of InfoAnalyze for tracing as tracing is decided after
						 * processing InfoAnalyze
						 */
						AinScfProtocolHelper.traceComponent(componentIndEvent, callData, true);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);
						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to start call execution");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}

							Event event = new Event(EventType.EVENT_INITIAL, Protocol.AIN_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);

						}
					} else {
						logger.error(dialogueId + ":: Invalid TerminationAttempt, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.UNEXP_TERMINATION_ATTEMPT);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.TERMINATION_ATTEMPT);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}

					break;
				}
				case AinOpCodes.INFO_COLLECTED_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process Info collected");
					}

					/*
					 * Drop the call, if service failed to read configuration
					 */
					if (!PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);

						AinScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}

					/*
					 * validate
					 */
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.INFO_COLLECT,
							tcapSession);
					callData.set(CallDataAttribute.P_AIN_TRIGGERING_MESSAGE, "Info_Collected");
					if (isValid) {

						/*
						 * process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid Info collect event start processing");
						}

						Action[] action = AinScfProtocolHelper.processInfoCollected(invokeIndEvent, tcapSession,
								PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
								.getCallTraceService());
						if (logger.isDebugEnabled()) {
							logger.debug("[PH] :: processInfoCollected Exit ");
						}
						/*
						 * First SIP-T and AIN call were taking almost 11 seconds to process. To solve
						 * this issue SAS will generate an invite and send a warm up call to service.
						 */
						if (tcapSession.getAttribute(PhConstants.WARMUP_HEADER) != null) {
							try {
								if (logger.isInfoEnabled()) {
									logger.info(dialogueId + ":: This is a warm-up AIN call.");
								}
								legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERMINATED);
								/*
								 * Notify service that call is dropped
								 */
								AinScfProtocolHelper.notifyCallDropped(tcapSession, false);
								SipApplicationSession appSession = AinScfProtocolUtil.getAppSession(tcapSession);
								tcapSession.invalidate();
								if (appSession != null && appSession.isValid()) {
									appSession.invalidate();
								}
								/*
								 * Note:- DO Not write CDR for warm-up calls
								 */
							} catch (Throwable e) {
								logger.warn(dialogueId + " :: Failed to handle AIN warm-up call" + e.getMessage());
							}
							return;

						}

						/*
						 * special case of INFO_COLLECT for tracing as tracing is decided after
						 * processing INFO_COLLECT
						 */
						AinScfProtocolHelper.traceComponent(componentIndEvent, callData, true);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);
						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to start call execution");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}
							Event event = new Event(EventType.EVENT_INITIAL, Protocol.AIN_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);

						}
					} else {
						logger.error(dialogueId + ":: Invalid InfoCollected, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.UNEXP_INFO_COLLECTED);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.INFO_COLLECT);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}

					break;
				}
				case AinOpCodes.NETWORK_BUSY_BYTE: {
					// Network Busy can be received as triggering event i.e., BEGIN or as CWP
					// Check for Dialogue type in which it is received then process it accordindly

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process Network busy recevied in dialogue:"
								+ callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE));
					}

					// Drop call if service is not able to read configuration
					if (!PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						AinScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}

					// Check if valid as per FSM. Network Busy allowed in
					// both INIT and CONTINUE state
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.NTWK_BUSY,
							tcapSession);
					callData.set(CallDataAttribute.P_AIN_TRIGGERING_MESSAGE, "Network_Busy");
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid Network busy event start processing");
						}

						// check if Network Busy received in Begin or CWP.
						int rxDialogueType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
						boolean componentInBegin = (rxDialogueType == TcapConstants.PRIMITIVE_BEGIN) ? true : false;

						Action[] action = AinScfProtocolHelper.processNetworkBusy(invokeIndEvent, tcapSession,
								PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
								.getCallTraceService(),
								componentInBegin);

						if (componentInBegin) {
							AinScfProtocolHelper.traceComponent(componentIndEvent, callData, true);
						}

						executeAction(tcapSession, action);

						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId
										+ " :: Notify service to start call execution for NetworkBusy Trigger");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}

							// generate event depending on type of dialogue.
							// Since network Busy may come as BEGIN or CWP so generate event
							// accordingly. For BEGIN - generate EVENT_INITIAL else FAIELD
							Event event = null;
							if (componentInBegin) {
								event = new Event(EventType.EVENT_INITIAL, Protocol.AIN_SCF, null);
							} else {
								event = new Event(EventType.EVENT_FAILURE, Protocol.AIN_SCF,
										CallDataAttribute.P_LEG1.name());
							}
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}
					} else {
						logger.error(dialogueId + ":: Invalid Network busy, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.UNEXPECTED_NTWK_BUSY);
						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.NTWK_BUSY);

						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case AinOpCodes.O_ANSWER_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process OAnswer");
					}
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.ERB_ANS,
							tcapSession);
					if (isValid) {
						/*
						 * process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: VALID OAnswer, start processing");
						}
						Action[] action = AinScfProtocolHelper.processOAnswer(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);

						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to handle term oAnswer");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}
							Event event = new Event(EventType.EVENT_SUCCESS, Protocol.AIN_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}

					} else {
						logger.error(dialogueId + ":: Invalid OAnswer, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_ERB);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.ERB);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case AinOpCodes.O_DISCONNECT_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process ODisconnect");
					}
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.ERB_DISCONNECT,
							tcapSession);
					if (isValid) {
						/*
						 * process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: VALID ODisconnect, start processing");
						}
						Action[] action = AinScfProtocolHelper.processODisconnect(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);

						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to handle ODisconnect");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}
							Event event = new Event(EventType.EVENT_DISCONNECT, Protocol.AIN_SCF,
									CallDataAttribute.P_LEG1.name());
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);

						}

					} else {
						logger.error(dialogueId + ":: Invalid ODisconnect, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_ERB);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.ERB_DISCONNECT);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case AinOpCodes.O_CALLED_PARTY_BUSY_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process OCalledPartyBusy");
					}
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.ERB_BUSY,
							tcapSession);
					if (isValid) {
						/*
						 * process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: VALID OCalledPartyBusy, start processing");
						}
						Action[] action = AinScfProtocolHelper.processOCalledBusy(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);

						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to handle OCalledPartyBusy");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}
							Event event = new Event(EventType.EVENT_FAILURE, Protocol.AIN_SCF,
									CallDataAttribute.P_LEG1.name());
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}

					} else {
						logger.error(dialogueId + ":: Invalid OCalledPartyBusy, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_ERB);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.ERB);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case AinOpCodes.O_NO_ANSWER_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process ONoAnswer");
					}
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.ERB_NOANS,
							tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: VALID ONoAnswer, start processing");
						}

						Action[] action = AinScfProtocolHelper.processONoAnswer(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);

						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to handle term no answer");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}

							Event event = new Event(EventType.EVENT_FAILURE, Protocol.AIN_SCF,
									CallDataAttribute.P_LEG1.name());
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}
					} else {
						logger.error(dialogueId + ":: Invalid ONoAnswer, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_ERB);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.ERB_NOANS);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case AinOpCodes.O_TERM_SIZED_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process OTermSiezed");
					}
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.ERB_TERM_SEIZED,
							tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: VALID OTermSiezed, start processing");
						}
						Action[] action = AinScfProtocolHelper.processOTermSeized(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);

						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to handle term Seized");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}
							Event event = new Event(EventType.EVENT_FAILURE, Protocol.AIN_SCF,
									CallDataAttribute.P_LEG1.name());
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}

					} else {
						logger.error(dialogueId + ":: Invalid OTermSiezed, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_ERB);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.ERB);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case AinOpCodes.RESOURCE_CLEAR_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process Resource clear ");
					}

					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.RES_CLR,
							tcapSession);
					/*
					 * process if valid
					 */
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid Resource clear event start processing");
						}

						Action[] action = AinScfProtocolHelper.processResourceClear(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);

						// commenting below as application shall be notified from
						// processResourceClear method.

						// if (action == null) {
						//
						// if (logger.isDebugEnabled()) {
						// logger.debug(dialogueId + " :: Notify service that MS has Resource clear");
						// }
						// /*
						// * Drop the call, if failed to determine service
						// * type. This case will happen only if some DB error
						// * happens while determining service type.
						// */
						// if (serviceInterface == null) {
						// logger.error(dialogueId + ":: Failed to find service type. Drop the call");
						// callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						// AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
						// callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						// AinScfProtocolHelper.dropCall(tcapSession, callData);
						// return;
						// }
						//
						// Event event = new Event(EventType.EVENT_MS_DISCONNECT, Protocol.AIN_SCF,
						// null);
						// ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						// }

					} else {
						logger.error(dialogueId + ":: Invalid Resource clear, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_RES_CLR);
						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.RES_CLR);

						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case AinOpCodes.CALL_INFO_FROM_RESOURCE_BYTE: {
					/**
					 * when Send_To_Resource message sent to SSP then response from SSP can be
					 * Call_Info_from_Resource or Resource_clear
					 * 
					 */
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process Call Info From Resource ");
					}

					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.CALL_INFO_FRM_RESRC,
							tcapSession);
					/*
					 * process if valid
					 */
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid Call Info From Resource event start processing");
						}
						Action[] action = AinScfProtocolHelper.processCallInfoFrmResrc
								(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);


					}else{
						logger.error(dialogueId
								+ ":: Invalid Call Info From Resource, drop the call");
					}
					break;
				}
				case AinOpCodes.CLOSE_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process close byte");
					}

					/*
					 * validate
					 * 
					 */
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.CLOSE, tcapSession);
					/*
					 * process if valid
					 */
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid Resource clear event start processing");
						}
						Action[] action = AinScfProtocolHelper.processClose(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);
					} else {
						logger.error(dialogueId + ":: Invalid Close, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXPECTED_CLOSE);
						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.CLOSE);

						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case AinOpCodes.TERMINATION_NOTIFICATION_BYTE: {
					// TermNotification comes as Unidirectional message. So no
					// need to check in FSM. TermNotification contains the
					// dialogue Id of ongoing call. Check if session exists then clean up
					// dialogue.
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process TermNotification byte");
					}

					CallData origCallData = null;

					origCallData = AinScfProtocolHelper.processTerminationNotification(invokeIndEvent, tcapSession);

					/*
					 * Helper return action in case of error so drop call with action execute drop
					 * action with force false;
					 */
					// executeAction(tcapSession, action);

					if(origCallData != null){
						if(logger.isDebugEnabled()){
							logger.debug(dialogueId + " :: TerminationNotification rxed- previous call cleaned up "+
									"sending async event");
						}
						// Generate new eventEVENT_TERM_NOTIFICATION
						Event event = new Event(EventType.EVENT_TERM_NOTIFICATION, Protocol.AIN_SCF, null);
						ProtocolRouter.getInstance().execute(event, origCallData, serviceInterface);
					}else {
						logger.debug("TerminationNotification returned NULL call data");
					}
					break;
				}
				case AinOpCodes.LIDB_QUERY_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process LIDB Query");
					}

					/*
					 * Drop the call, if service failed to r ead configuration
					 */
					if (!PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						AinScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}

					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.LIDB_QUERY,
							tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid LIDB event start processing");
						}

						Action[] action = LidbQueryHandler.processLIDBQuery(invokeIndEvent, tcapSession,
								PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
								.getCallTraceService());
						if (logger.isDebugEnabled()) {
							logger.debug("[PH] :: processLIDBQuery Exit ");
						}
						AinScfProtocolHelper.traceComponent(componentIndEvent, callData, true);
						executeAction(tcapSession, action);
						if (action == null && invokeIndEvent.isLastComponent()) {
							//invokeinEvent  is last component
							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to start call execution");
							}
							Event event = new Event(EventType.EVENT_INITIAL, Protocol.AIN_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}
					} else {
						logger.error(dialogueId + ":: Invalid LIDB");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.INVALID_BNS_QUERY);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.LIDB_QUERY);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				// TODO: PROVIDE_INSTRUCTION_BYTE = (-31999) 7C FF
				case AinOpCodes.PROVIDE_INSTRUCTION_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process PROVIDER_INSTRUCTION_BYTE");
					}
					/*
					 * Drop the call, if service failed to read configuration
					 */
					if (!PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						AinScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.PROVIDE_INSTRUCTION,
							tcapSession);
					callData.set(CallDataAttribute.P_AIN_TRIGGERING_MESSAGE, "Provide_Instruction");
					if (isValid) {
						/*
						 * process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid PROVIDER_INSTRUCTION event start processing");
						}

						Action[] action = Gr533MessageHandler.processProvideInstruction(invokeIndEvent, tcapSession,
								PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
								.getCallTraceService());

						if (logger.isDebugEnabled()) {
							logger.debug("[PH] :: processProviderInstruction Exit ");
						}
						AinScfProtocolHelper.traceComponent(componentIndEvent, callData, true);

						/*
						 * Helper return action in case of error so drop call with action execute drop
						 * action with force false;
						 */
						executeAction(tcapSession, action);
						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to start call execution");
							}
							/*
							 * Drop the call, if failed to determine service type. This case will happen
							 * only if some DB error happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId + ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										AinScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								AinScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}

							Event event = new Event(EventType.EVENT_INITIAL, Protocol.AIN_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);

						}
					} else {
						logger.error(dialogueId + ":: Invalid provideInstruction, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.UNEXP_PROVIDE_INSTRUCTION);

						/*
						 * Helper returns out of sequence action for the case; drop call with the action
						 */
						Action[] action = AinScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession,
								AinScfProtocolEvent.PROVIDE_INSTRUCTION);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}

					break;
				}
				case AinOpCodes.AC_QUERY_BYTE:
				case AinOpCodes.ARC_QUERY_BYTE:{
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process AC/ARC Query");
					}
					/*
					 * Drop the call, if service failed to read configuration
					 */
					if (!PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						AinScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}
					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.AC_QUERY,
							tcapSession);
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::AC Query FSM validation isValid :"+isValid);
					}
					if (isValid) {
						Action[] action = AinScfProtocolHelper.processAcQuery(componentIndEvent, tcapSession,
								PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
								.getCallTraceService());
						executeAction(tcapSession, action);
						if (action == null) {
							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to start call execution");
							}
							Event event = new Event(EventType.EVENT_INITIAL, Protocol.AIN_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}
					}
					if (logger.isDebugEnabled()) {
						logger.debug("[PH] :: processAcQuery [NOT LAST COMPONENT] Exit ");
					}
					break;	
				}
				case AinOpCodes.ISVM_QUERY_BYTE:
				case AinOpCodes.ISVM_QUERY_WITH_REPLY_BYTE:{
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process ISVM Query");
					}
					/*
					 * Drop the call, if service failed to read configuration
					 */
					if (!PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
							.isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								AinScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						AinScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}

					boolean isValid = AinScfProtocolFSMHandler.validateFSMState(AinScfProtocolEvent.ISVM_QUERY,
							tcapSession);
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::ISVM_QUERY FSM validation isValid :"+isValid);
					}
					if (isValid) {
						Action[] action = AinScfProtocolHelper.processISVMQuery(componentIndEvent, tcapSession,
								PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
								.getCallTraceService());
						executeAction(tcapSession, action);
						if (action == null) {
							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId + " :: Notify service to start call execution");
							}
							Event event = new Event(EventType.EVENT_INITIAL, Protocol.AIN_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}
					}
					if (logger.isDebugEnabled()) {
						logger.debug("[PH] :: processISVMQuery [NOT LAST COMPONENT] Exit ");
					}
					break;
				}
				default: {
					logger.error(dialogueId + ":: Unknown INVOKE Indication Event " + operCodeStr);

					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNKNOWN_COMPIND_RCVD);

					Action[] action = AinScfProtocolHelper.getUnknownMessageAction(tcapSession,
							AinScfProtocolEvent.UNKNOWN);
					/*
					 * execute drop action with force false;
					 */
					executeAction(tcapSession, action);
					break;
				}
				} // end of switch operCodeStr

				break;
			} // end of case TcapConstants.PRIMITIVE_INVOKE

			case TcapConstants.PRIMITIVE_RESULT: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Primitive Type is PRIMITIVE_RESULT");
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.PRIM_RESULT_RCVD);

				ResultIndEvent resultIndEvent = (ResultIndEvent) componentIndEvent;
				Action[] action = AinScfProtocolHelper.processResult(resultIndEvent, tcapSession);
				/*
				 * execute drop action with force false;
				 */
				executeAction(tcapSession, action);

				break;
			}
			case TcapConstants.PRIMITIVE_ERROR: {
				logger.warn(dialogueId + ":: PRIMITIVE_ERROR is received");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.PRIM_ERROR_RCVD);

				ErrorIndEvent errorIndEvent = (ErrorIndEvent) componentIndEvent;
				Action[] action = AinScfProtocolHelper.processError(errorIndEvent, tcapSession);
				/*
				 * execute drop action with force false;
				 */
				executeAction(tcapSession, action);

				break;
			}

			case TcapConstants.PRIMITIVE_REJECT: {
				logger.warn(dialogueId + ":: PRIMITIVE_REJECT is received");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.PRIM_REJECT_RCVD);

				RejectIndEvent rejectIndEvent = (RejectIndEvent) componentIndEvent;
				Action[] action = AinScfProtocolHelper.processReject(rejectIndEvent, tcapSession);

				/*
				 * execute drop action with force false;
				 */
				executeAction(tcapSession, action);
				break;
			}
			}

			boolean isLastComponent = componentIndEvent.isLastComponent();
			AinCallStates Ain_CALL_STATES = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
			/*
			 * process end dialog if call is not terminated and last component is processed.
			 */

			if (isLastComponent && (lastRxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END)
					&& Ain_CALL_STATES == AinCallStates.INIT.TERMINATED) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: last componet of End dialog processed");
					logger.debug(dialogueId + ":: Notify service of call dropped.");
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.END_RCVD_WITH_COMP);
				AinScfProtocolHelper.preProcessDroppedCall(tcapSession);

				/*
				 * notify service and write CDRs
				 */

				AinScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
			}
		} catch (MandatoryParameterNotSetException e) {
			logger.error(dialogueId + ":: MandatoryParameterNotSetException fetching component event", e);
			CallData newCallData = AinScfProtocolUtil.getCallData(tcapSession);
			if (newCallData != null) {

				/*
				 * Should not change the Ain call state
				 */

				newCallData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.COMP_MANDATORY_PARAM_MIS);
				newCallData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				AinScfProtocolHelper.dropCall(tcapSession, newCallData);
			}
		} catch (Exception ex) {
			logger.error(dialogueId + ":: Failed to process Component Indication event", ex);
			CallData newCallData = AinScfProtocolUtil.getCallData(tcapSession);
			if (newCallData != null) {
				/*
				 * Should not change the Ain call state
				 */
				newCallData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.EXCEP_IN_COMP_IND);
				newCallData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				AinScfProtocolHelper.dropCall(tcapSession, newCallData);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processComponentIndEvent() Exit");
		}
	}

	/**
	 * This method is used to execute the action returned by AIN PH
	 * 
	 * @param tcapSession
	 * @param actionArr
	 * @throws Exception
	 */
	public static void executeAction(TcapSession tcapSession, Action[] actionArr) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		Integer dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		int dialogueId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: inside executeAction with tcapSession and action array");
		}

		if (actionArr == null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Do nothing as action array is null");
			}
			return;
		}

		int lastRxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);

		if (lastRxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Last dialogue is PRIMITIVE_END");
			}
			List<Action> actArr = new ArrayList<Action>(actionArr.length);
			for (int i = 0; i < actionArr.length; i++) {
				Action action = actionArr[i];
				if (action.getActionType() == ActionType.ACTION_END_CALL
						|| action.getActionType() == ActionType.ACTION_DISCONNECT) {
					AinScfProtocolHelper.setCallDataParamsForCDR(callData, action);
				}
				if (action.getActionType() == ActionType.ACTION_END_CALL
						|| action.getActionType() == ActionType.ACTION_DISCONNECT) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Allowed action is " + action.getActionType());
					}
					actArr.add(action);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Ignored action is " + action.getActionType());
					}
				}
			}
			actionArr = actArr.toArray(new Action[actArr.size()]);
		}

		int callState = CallTraceService.CALL_IN_PROGRESS;

		/*
		 * moved tracing in begin as it should be done before message is sent. and used
		 * call in progress state as term message will use terminated state.
		 */
		String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);

		if (PhConstants.TRUE.equals(traceFlag)) {

			StringBuilder traceMsg = (StringBuilder) callData.get(CallDataAttribute.P_TRACE_MESSAGE);

			if (traceMsg.length() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Trace message is " + traceMsg);
				}

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);

					for (int constraint : constraintList) {
						PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService()
						.trace(constraint, String.valueOf(dialogueId), traceMsg.toString(), callState);
					}
				}
				traceMsg.delete(0, traceMsg.length());
			}
		}
		AinCallStates lastCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		/*
		 * To maintain the order of action in actionArr, iterate using index
		 */
		for (int i = 0; i < actionArr.length; i++) {
			Action action = actionArr[i];

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Action to be performed is " + action.getActionType());
			}

			callData.set(CallDataAttribute.P_LAST_CALL_ACTION, action);
			switch (action.getActionType()) {
			case ACTION_CONNECT:
				AinScfProtocolHelper.connectTerm(tcapSession, callData, action);
				break;
			case ACTION_END_CALL:
			case ACTION_DISCONNECT:
				AinScfProtocolHelper.dropCall(tcapSession, callData, action);
				break;
			case ACTION_CONTINUE:
				AinScfProtocolHelper.sendContinueMessage(tcapSession, action, PhUtilityServices
						.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());
				break;
			default:
				logger.error(dialogId + ":: Unsupported Action, drop the call");
				logger.error("AIN Call State:" + lastCallState);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.UNEXP_ACTION);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				AinScfProtocolHelper.dropCall(tcapSession, callData);
				break;
			}
		}

		AinCallStates currCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		/*
		 * replicate if last call state is assist and curr call state is not
		 * terminated/term in progress
		 */
		if (lastCallState == AinCallStates.ASSIST && currCallState != AinCallStates.TERMINATED
				&& currCallState != AinCallStates.TERMINATION_IN_PROGRESS) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Replicate TcapSession");
			}
			tcapSession.replicate();
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: Exit from executeAction with tcapSession and action array");
		}
	}

	/**
	 * This method id used to handle timers created by AIN PH
	 * 
	 * @param timer
	 * @return void
	 */
	public final void timeout(ServletTimer timer) {
		SipApplicationSession appSession = null;
		CallData callData = null;
		int dialogId = 0;
		LegData origLegData = null;

		if (logger.isInfoEnabled()) {
			logger.info(dialogId + ":: Received timeout for AIN Timer");
		}
		try {
			appSession = timer.getApplicationSession();
			callData = AinScfProtocolUtil.getCallData(appSession);
			dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
			MDC.put(PhConstants.MDC_CALL_ID_CONST, "[" + dialogId + "]");

			if (appSession == null || !appSession.isValid()) {
				if (logger.isInfoEnabled()) {
					logger.info(dialogId + ":: Do nothing as timer appsession is null or invalidated");
				}
				return;
			}

			TcapSession tcapSession = PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
					.getTcapProvider().getTcapSession(dialogId);

			PhTimerInfo phTimerInfo = (PhTimerInfo) timer.getInfo();
			String timerName = phTimerInfo.getTimerName();

			CallTraceService cCallTraceService = PhUtilityServices
					.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

			if (logger.isInfoEnabled()) {
				logger.info(dialogId + ":: Received timeout for the timer " + timerName);
			}
			/*
			 * This check is to avoid un-necessary processing on timeout of a timer, which
			 * has been removed from appSession due to some call cleanup activity or
			 * successful handoff/assist handling.
			 */
			if (appSession.getAttribute(timerName) == null) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Do nothing as the timer is not in appsession");
					logger.debug(dialogId + ":: Cleanup has been performed");
				}
				return;
			}

			origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			AinCallStates origState = (AinCallStates) origLegData.get(LegDataAttributes.P_LEG_SS7_STATE);

			if (logger.isInfoEnabled()) {
				logger.info(dialogId + ":: AIN call state of orig is  " + origState);
			}

			if (timerName.equals(PhConstants.CDR_TIMER)) {
				boolean timerProcessingRequired = AinScfProtocolFSMHandler
						.validateFSMState(AinScfProtocolEvent.CDR_TIMEOUT, tcapSession);

				if (timerProcessingRequired) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Handle timeout in AinCallState=CONNECTED");
					}

					// CommonUtils.incrementIntermediateCdr(callData);

					/*
					 * Set App-session timeout to 24 hours and 5 minute
					 */
					CommonUtils.setAppSessionTimeout(appSession, 1445, dialogId);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Write intermediate CDR.");
					}
					AinScfProtocolUtil.writeServiceCdr(tcapSession);

					/*
					 * this is a special case replicating tcapsesison as it will be required for CDR
					 * params
					 */
					tcapSession.replicate();

				} else {
					logger.error(dialogId + ":: Do nothing as timeout received in invalid call state");
					logger.error(origLegData.get(LegDataAttributes.P_LEG_SIP_STATE) + " "
							+ origLegData.get(LegDataAttributes.P_LEG_SS7_STATE));
				}
			} else if (timerName.equals(PhConstants.CORRELATION_TIMER)) {

				boolean timerProcessingRequired = AinScfProtocolFSMHandler
						.validateFSMState(AinScfProtocolEvent.CORRELATION_TIMEOUT, tcapSession);
				if (timerProcessingRequired) {
					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
					callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Set failed call indicator to 1 and drop the call");
					}
					logger.error(dialogId + ":: Drop the call as correlation timer expired for "
							+ callData.get(CallDataAttribute.P_CORRELATION_ID));
					callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE, AinScfRelReasonCode.CORRELATION_TIMEOUT);
					AinScfProtocolHelper.dropCall(tcapSession, callData);
				}
			}else if (timerName.startsWith(PhConstants.APP_TIMER)) {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Application timer timedout ");
					logger.debug(dialogId + ":: Hence notifying the service for application timer timeout event");
				}

				timerName = timerName.substring(timerName.indexOf(PhConstants.APP_TIMER) + 9);

				if (timerName.equals("")) {
					timerName = PhConstants.APP_TIMER;
				}

				ServiceInterface serviceHandler = PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();
				Event event = new Event(EventType.EVENT_APP_TIMER_TIMEOUT,
						Protocol.AIN_SCF, null);
				event.setTimerName(timerName);

				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			}

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Exiting timeout");
			}

		} catch (Exception e) {
			logger.error(dialogId + ":: Exception  timeout " + e);

			e.printStackTrace();
		}
	}

}
