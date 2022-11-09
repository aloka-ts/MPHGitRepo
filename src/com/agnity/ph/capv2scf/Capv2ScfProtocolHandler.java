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

import jain.MandatoryParameterNotSetException;
import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.DialogueIndEvent;
import jain.protocol.ss7.tcap.TcapConstants;
import jain.protocol.ss7.tcap.component.ErrorIndEvent;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.RejectIndEvent;
import jain.protocol.ss7.tcap.component.ResultIndEvent;
import jain.protocol.ss7.tcap.dialogue.EndIndEvent;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;

import com.agnity.camelv2.operations.CapV2OpCodes;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CapV2CallStates;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
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

/**
 * This class is used to handle CAP Protocol Specific events. This handler class 
 * is invoked by ProtocolHandler servlet for processing of dialog and components events 
 * This class processes these events and delegates the processing of the messages
 * to CAP Protocol Helper class
 * 
 * A new CallData Object is created on initial DP invocation and saved
 * in TCapSession and appsession for this call. This object then remains same for all the subsequent messages
 * and works as a data object for whole call
 */
public class Capv2ScfProtocolHandler implements SS7ProtocolHandler {

	private static final Capv2ScfProtocolHandler INSTANCE = new Capv2ScfProtocolHandler();
	private static Logger logger = Logger
			.getLogger(Capv2ScfProtocolHandler.class);

	/**
	 * 
	 */
	private Capv2ScfProtocolHandler() {
	}

	/**
	 * @return
	 */
	public static Capv2ScfProtocolHandler getInstance() {
		return INSTANCE;
	}

	/**
	 * This Method receives the action set by application and executes appropriate 
	 * handler based upon the action code
	 * 
	 * @param callData
	 * @param action
	 */

	public void executeAction(CallData callData, Action action)
			throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] executeAction() Enter with callData and action ");
		}

		int dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		TcapSession tcapSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getTcapProvider().getTcapSession(dialogId);
		
		Capv2ScfProtocolHelper.traceMessage(callData, tcapSession);

		if (action == null || tcapSession == null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId
						+ ":: [PH] Do nothing as action array is null or tcapSession is null, tcapSession:" + tcapSession);
			}
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: [PH] Action to be performed is "
					+ action.getActionType());
		}

		callData.set(CallDataAttribute.P_LAST_CALL_ACTION,
				action);
		switch (action.getActionType()) {
		case ACTION_CONNECT: {
			Capv2ScfProtocolHelper.connectTerm(tcapSession, callData, action);
			break;
		}
		case ACTION_DISCONNECT: {
			Capv2ScfProtocolHelper.disconnctTerm(tcapSession, callData,
					action);
			break;
		}
		case ACTION_END_CALL: {
			Capv2ScfProtocolHelper.dropCall(tcapSession, callData, action);
			break;
		}
		case ACTION_REDIRECT: {
			Capv2ScfProtocolHelper.redirect(tcapSession, callData, action);
			break;
		}
		case ACTION_CONNECT_MS: {
			Capv2ScfProtocolHelper.initiateMediaServerConnection(tcapSession,
					callData, action);
			break;
		}
		case ACTION_DISCONNECT_MS: {
			Capv2ScfProtocolHelper.disconnectIvr(tcapSession, callData,
					action);
			break;
		}

		case ACTION_CALL_HB: {
			Capv2ScfProtocolHelper.callHeartBeat(tcapSession, callData,
					action);
			break;
		}

		case ACTION_CONTINUE: {
			Capv2ScfProtocolHelper.sendContinueComponent(tcapSession,
					callData, action);
			break;
		}
		case ACTION_CHRG: {
			Capv2ScfProtocolHelper.performCharging(tcapSession, callData,
					action);
			break;
		}
		case ACTION_RESET_TMR: {
			Capv2ScfProtocolHelper.sendResetTimer(tcapSession, callData,
					action);
			break;
		}
		/*case ACTION_PROCESS_CALLS: {
			break;
		}
		case ACTION_RESYNC_CALL: {
			break;
		}
		case ACTION_HOLD_CALL: {
			break;
		}
		case ACTION_HTTP_REQ: {
			Capv2ScfProtocolHelper.sendHttpRequest(tcapSession, callData,
					action);
			break;
		}
		case ACTION_LS_CMD: {
			Capv2ScfProtocolHelper.sendLsRequest(tcapSession, callData,
					action);
			break;
		}
		*/
		case ACTION_PLAY: {
			Capv2ScfProtocolHelper.playAnnouncementWrapper(tcapSession, callData,
					action);
			break;
		}
		case ACTION_PLAY_COLLECT: {
			Capv2ScfProtocolHelper.playAndCollect(tcapSession, callData,
					action);
			break;
		}
		/*
		case ACTION_PLAY_RECORD: {
			Capv2ScfProtocolHelper.playAndRecord(tcapSession, callData,
					action);
			break;
		}*/
		case ACTION_START_TIMER: {
			Capv2ScfProtocolHelper.startApplicationTimer(tcapSession, callData, action);
			break;
		}
		case ACTION_STOP_TIMER: {
			Capv2ScfProtocolHelper.stopApplicationTimer(tcapSession, callData, action);
			break;
		}
		default: {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] executeAction():: Unknown Action Received.");
			}
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
	 * <li>If Primitive Type is PRIMITIVE_BEGIN for a new call then New calldata
	 * is created and set in TCAP session.Store the tcap session reference in
	 * appSession as in timeout it would be required. If this PRIMITIVE_BEGIN is
	 * for an existing call then validate the message for FSM state. After
	 * validation call the processBegin method of CAPSmHelper to get next
	 * action to be executed. For Invalid dialoge BEGIN, drop call.
	 * <li>For primitive type PRIMITIVE_CONTINUE validate the FSM state and call
	 * processContinue method of CAPSmHelper to get next action to be executed.
	 * For Invalid dialoge CONTINUE, drop call.
	 * <li>For primitive type PRIMITIVE_END validate the FSM state and call
	 * processEnd method of CAPSmHelper to get next action to be executed.For
	 * end message no need to excute processing failure action
	 * executeAction(tcapSession, action).Just check if call needs to be cleaned
	 * now or after processing components,drop call if no component is
	 * present.For Invalid dialoge END, drop call.
	 * <li>For primitive type PRIMITIVE_USER_ABORT action not excuted as already
	 * term message is recieved executeAction(tcapSession, action).Clean call if
	 * no component is present and notify service and write CDRs.
	 * <li>For primitive type PRIMITIVE_PROVIDER_ABORT and PRIMITIVE_NOTICE
	 * action not excuted as already term message is recieved
	 * executeAction(tcapSession, action).Clean call if no component is present
	 * and notify service and write CDRs.
	 * <li>For unrecognized Primitive Type execute drop action with force false.
	 * </ul>
	 * Always store dialoge primitive type and Increment Interconnection Count
	 * before returning from the method. For MandatoryParameterNotSetException
	 * for dialogue event or Failed to process Dialogue Indication event release
	 * the call with CV=41.
	 * 
	 * @param dialogueIndEvent
	 *            represents the instance of DialogueIndEvent
	 * @see jain.protocol.ss7.tcap.JainTcapListener#processDialogueIndEvent(jain.protocol.ss7.tcap.DialogueIndEvent)
	 */
	public void processDialogueIndEvent(DialogueIndEvent dialogueIndEvent,
			ServiceInterface serviceHandler) {
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
				logger.info(dialogueId
						+ "::[PH] Processing Dialogue Indication Event "
						+ dialogueIndEvent.getPrimitiveType());
			}

			tcapSession = sTcapProvider.getTcapSession(dialogueId);

			/*
			 * fetch call data from tcap session
			 */
			callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);

			if (callData != null) {
				Capv2ScfProtocolHelper
				.traceDialog(dialogueIndEvent, callData);

				/*
				 * set last Rx dialogue primitive to validate component against
				 * dialogue
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ " ::[PH] Setting rx dialog primitive for future reference");
				}
				callData.set(
						CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE,
						dialogueIndEvent.getPrimitiveType());
				((MutableInt) callData.get(CallDataAttribute.P_NETWORK_TRANSACTION)).increment();
			}

			switch (dialogueIndEvent.getPrimitiveType()) {
			case TcapConstants.PRIMITIVE_BEGIN: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ ":: Primitive Type is PRIMITIVE_BEGIN");

				}

				/*
				 * creating and storing call data
				 */
				if (callData == null) {
					callData = new CallData();
					callData.set(CallDataAttribute.SERVICE_ID, serviceHandler.getApplicationName());
					legData = new LegData();

					callData.set(CallDataAttribute.P_CALL_START_TIME, new Date());
					callData.set(CallDataAttribute.P_LEG1, legData);
					callData.set(CallDataAttribute.P_PROTOCOL, Protocol.CAPV2_SCF);
					callData.set(CallDataAttribute.P_DIALOG_ID, dialogueId);
					callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, String.valueOf(dialogueId));
					callData.set(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE,
							dialogueIndEvent.getPrimitiveType());

					SipApplicationSession appSession = Capv2CS1ScfProtocolUtil
							.getAppSession(tcapSession);

					appSession.setAttribute(PhConstants.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(CallDataAttribute.P_APP_SESSION_ID,
							appSession.getId());

					/*
					 * Store the tcap session reference in appSession as in
					 * timeout it would be required
					 */
					appSession.setAttribute(PhConstants.TCAP_SESSION_ID,
							tcapSession.getDialogueId());

					legData.set(
							LegDataAttributes.P_LEG_SS7_STATE,
							CapV2CallStates.INIT);
					legData.set(LegDataAttributes.P_DIALOG_ID,
							tcapSession.getDialogueId());
					legData.set(
							LegDataAttributes.P_CONNECTION_TYPE,
							ConnectionType.ORIG_CONNECTION);
					tcapSession.setAttribute(CallData.CALL_DATA, callData);
					appSession.setAttribute(CallData.CALL_DATA, callData);
					callData.set(CallDataAttribute.P_TRACE_FLAG,
							PhConstants.FALSE);
					callData.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(1));

				} else {
					logger.error(dialogueId
							+ ":: Recieveid BEGIN for existing call.");
					/*
					 * not doing anything as call will be cleaned through
					 * validate fsm state
					 */
				}
				/*
				 *  validating message
				 */
				boolean isValidEvent = Capv2ScfProtocolFSMHandler
						.validateFSMState(Capv2ScfProtocolEvent.BEGIN,
								tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ ":: Valid BEGIN start processing");
					}
					Boolean dumpCounters = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.DUMP_COUNTERS));
					if(dumpCounters) {
						PhMeasurementService.getInstance().getMeasurementCounter(Protocol.CAPV2_SCF).
						incrementServiceTriggeredCount(serviceHandler.getApplicationName(), false);

					}
					Capv2ScfProtocolHelper.processBegin(dialogueIndEvent,
							tcapSession);

				} else {
					logger.error(dialogueId
							+ ":: Invalid dialoge BEGIN, drop call");

					/*
					 * invalid event get action from helper and do the same as
					 * drop call
					 */
					Action[] action = Capv2ScfProtocolHelper
							.getOutOfSequenceDialogAction(tcapSession,
									TcapConstants.PRIMITIVE_BEGIN);
					/*
					 *  execute drop action with force false;
					 */
					for (Action actionField : action) {
						Capv2ScfProtocolHelper.dropCall(tcapSession,
								callData, actionField);
					}
				}
				break;
			}
			case TcapConstants.PRIMITIVE_CONTINUE: {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ ":: Primitive Type is PRIMITIVE_CONTINUE");
				}

				boolean isValidEvent = Capv2ScfProtocolFSMHandler
						.validateFSMState(Capv2ScfProtocolEvent.CONTINUE,
								tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ ":: Valid CONTINUE event start processing");
					}

					Action[] action = Capv2ScfProtocolHelper.processContinue(
							dialogueIndEvent, tcapSession);
					executeAction(tcapSession, action);
				} else {
					logger.error(dialogueId
							+ ":: Invalid dialoge CONTINUE, drop call");

					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_CODE,
							Capv2ScfRelReasonCode.UNEXPECTED_CONTINUE);

					/*
					 * Helper returns out of sequence action for the case; drop
					 * call with the action
					 */
					Action[] action = Capv2ScfProtocolHelper
							.getOutOfSequenceDialogAction(tcapSession,
									TcapConstants.PRIMITIVE_CONTINUE);

					/*
					 *  execute drop action with force false;
					 */
					for (Action actionField : action) {
						Capv2ScfProtocolHelper.dropCall(tcapSession,
								callData, actionField);
					}
				}
				break;
			}
			case TcapConstants.PRIMITIVE_END: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ ":: Primitive Type is PRIMITIVE_END");
				}

				boolean isValidEvent = Capv2ScfProtocolFSMHandler
						.validateFSMState(Capv2ScfProtocolEvent.END,
								tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ ":: Valid EVENT event start processing");
					}
					EndIndEvent endIndEvent = (EndIndEvent) dialogueIndEvent;

					/*
					 * usually end is sent by service.. its error flow so
					 * setting default cause value as 41 for CDR and marking
					 * call as failed call if there is some component in end it
					 * will update CDR Mark cause val as 41 for failed call
					 */
					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

					/*
					 * drop call if no component is present
					 */
					if (!(endIndEvent.isComponentsPresent())) {
						logger.warn(dialogueId
								+ ":: END with no component, drop call");

						callData.set(
								CallDataAttribute.NP_RELEASE_REASON_CODE,
								Capv2ScfRelReasonCode.END_RCVD_WITHOUT_COMP);

						/*
						 * clean call as no component is present
						 */
						Capv2ScfProtocolHelper
						.preProcessDroppedCall(tcapSession);
						/*
						 * notify service and write CDRs
						 */
						Capv2ScfProtocolHelper.postProcessDroppedCall(
								tcapSession, true);

					}
				} else {
					logger.error(dialogueId
							+ ":: Invalid dialoge END, drop call");

					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_CODE,
							Capv2ScfRelReasonCode.UNEXPECTED_END);

					Action[] action = Capv2ScfProtocolHelper
							.getOutOfSequenceDialogAction(tcapSession,
									TcapConstants.PRIMITIVE_END);
					/*
					 *  execute drop action with force false;
					 */
					for (Action actionField : action) {
						Capv2ScfProtocolHelper.dropCall(tcapSession,
								callData, actionField);
					}

				}
				break;
			}
			case TcapConstants.PRIMITIVE_USER_ABORT: {

				if (logger.isInfoEnabled()) {
					logger.info(dialogueId + ":: Handling U-ABORT");
				}

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.UABORT_RCVD);

				/*
				 * Mark cause val as 31 as u-abort will be rcvd when user hung
				 * up and related events are not armed.
				 */
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 31);

				Action[] action = Capv2ScfProtocolHelper.processUAbort(
						dialogueIndEvent, tcapSession);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Returned actions are "
							+ action);
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
				Capv2ScfProtocolHelper.preProcessDroppedCall(tcapSession);
				/*
				 *  notify service and write CDRs
				 */
				Capv2ScfProtocolHelper.postProcessDroppedCall(tcapSession,
						true);

				break;

			}
			case TcapConstants.PRIMITIVE_PROVIDER_ABORT:
			case TcapConstants.PRIMITIVE_NOTICE: {
				if (logger.isInfoEnabled()) {
					logger.info(dialogueId + ":: Handling P-ABORT/NOTICE");
				}

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.PABORT_NOTICE_RCVD);

				/*
				 * Mark cause val as 41 for failed call
				 */
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Cleaning the call");
				}
				/*
				 * clean call if no component is present
				 */
				Capv2ScfProtocolHelper.preProcessDroppedCall(tcapSession);

				/*
				 * notify service and write CDRs
				 */
				Capv2ScfProtocolHelper.postProcessDroppedCall(tcapSession,
						true);

				break;
			}
			default: {
				logger.error(dialogueId + ":: unrecognized Primitive Type "
						+ dialogueIndEvent.getPrimitiveType());

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.UNKNOWN_DIALOG_RCVD);

				Action[] action = Capv2ScfProtocolHelper
						.getUnknownDialogAction(tcapSession);

				/*
				 * execute drop action with force false;
				 */
				for (Action actionField : action) {
					Capv2ScfProtocolHelper.dropCall(tcapSession, callData,
							actionField);
				}
				break;
			}
			}

		} catch (MandatoryParameterNotSetException e) {
			logger.error(
					dialogueId
					+ ":: MandatoryParameterNotSetException for dialogue event",
					e);
			CallData newcallData = Capv2CS1ScfProtocolUtil
					.getCallData(tcapSession);
			if (newcallData != null) {
				newcallData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.DIAGLOG_MANDATORY_PARAM_MIS);
				newcallData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				Capv2ScfProtocolHelper.dropCall(tcapSession, newcallData);
			}
		} catch (Exception ex) {
			logger.error(dialogueId
					+ ":: Failed to process Dialogue Indication event", ex);
			CallData newcallData = Capv2CS1ScfProtocolUtil
					.getCallData(tcapSession);
			if (newcallData != null) {
				newcallData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.EXCEP_IN_DIALOG_IND);
				newcallData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				Capv2ScfProtocolHelper.dropCall(tcapSession, newcallData);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processDialogueIndEvent() Exit");
		}
	}

	/**
	 * Tcaplistener API implemented by the service for receiving a callback on a
	 * component indication event occuring on the node.This method check the
	 * primitive type for the component indication event. For Primitive Type
	 * PRIMITIVE_INVOKE, PH checks for the opreation code.
	 * <ul>
	 * <li>If operation code is IDP_BYTE, then validate the message for FSM
	 * state.After validation call the processIdp method of CAPSmHelper to get
	 * next action to be executed.If it is a first call SAS will generate an
	 * invite and send a warm up call to service.Helper return action in case of
	 * error so drop call with action drop action with force false. For no error
	 * case do service specific IDP processing notify service to start call
	 * execution. For Invalid IDP, drop the call.
	 * <li>If operation code is ENC_BYTE, then validate the message for FSM
	 * state.After validation call the processENC method of CAPSmHelper to get
	 * next action to be executed.For Invalid ENC, drop the call
	 * <li>If operation code is ERB_BYTE, then validate the message for FSM
	 * state.After validation call the processENC method of CAPSmHelper to get
	 * next action to be executed.For Invalid ERB, drop the call.
	 * <li>If operation code is ER_BYTE, no validation is needed for ER just
	 * execute drop action with force false.
	 * <li>for Unknown INVOKE Indication Event, just execute drop action with
	 * force false.
	 * </ul>
	 * For Primitive Type PRIMITIVE_RESULT ,PRIMITIVE_REJECT and PRIMITIVE_ERROR
	 * just execute drop action with force false. Process end dialog if call is
	 * not termninated and last component is processed. For
	 * MandatoryParameterNotSetException for component event or Failed to
	 * process component Indication event release the call with CV=41.
	 * 
	 * @param componentIndEvent
	 *            represents the instance of ComponentIndEvent
	 * @see jain.protocol.ss7.tcap.JainTcapListener#processComponentIndEvent(jain.protocol.ss7.tcap.ComponentIndEvent)
	 */

	public void processComponentIndEvent(ComponentIndEvent componentIndEvent,
			ServiceInterface serviceInterface) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processComponentIndEvent() Enter");
		}
		int dialogueId = 0;
		TcapSession tcapSession = null;
		try {
			dialogueId = componentIndEvent.getDialogueId();
			boolean isLastComponent = componentIndEvent.isLastComponent();

			if (logger.isInfoEnabled()) {
				logger.info(dialogueId
						+ ":: Processing Component Indication Event "
						+ componentIndEvent.getPrimitiveType() + ", isLastComponent:" + isLastComponent);
			}

			tcapSession = PhUtilityServices.getInstance(serviceInterface.getApplicationName()).getTcapProvider()
					.getTcapSession(dialogueId);

			CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

			int lastRxDialoguePrimitiveType = -1;

			if (callData != null) {
				lastRxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);			
				legData.set(LegDataAttributes.P_LAST_RX_INVOKE_ID, componentIndEvent.getInvokeId());
				Capv2ScfProtocolHelper.traceComponent(componentIndEvent, callData);
			}

			switch (componentIndEvent.getPrimitiveType()) {
			case TcapConstants.PRIMITIVE_INVOKE: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId	+ ":: Primitive Type is PRIMITIVE_INVOKE");
				}

				InvokeIndEvent invokeIndEvent = (InvokeIndEvent) componentIndEvent;
				byte[] operCode = invokeIndEvent.getOperation()
						.getOperationCode();
				byte operCodeByte = operCode[0];
				String operCodeStr = CommonUtils.formatBytes(operCode);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Operation Code is " + operCodeStr);
				}

				switch (operCodeByte) {
				case CapV2OpCodes.IDP_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process IDP");
					}

					/*
					 * Drop the call, if service failed to read configuration
					 */
					if (!PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(
								CallDataAttribute.NP_RELEASE_REASON_CODE,
								Capv2ScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						Capv2ScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}

					/*
					 * validate
					 */
					boolean isValid = Capv2ScfProtocolFSMHandler.validateFSMState(Capv2ScfProtocolEvent.IDP,
							tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: Valid IDP event start processing");
						}

						Action[] action = Capv2ScfProtocolHelper.processIdp(
								invokeIndEvent, tcapSession, PhUtilityServices
								.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService());

						if (logger.isDebugEnabled()) {
							logger.debug("[PH] :: processIdp Exit");
						}
						/*
						 * First SIP-T and CAP call were taking almost 11
						 * seconds to process. To solve this issue SAS will
						 * generate an invite and send a warm up call to
						 * service.
						 */
						if (tcapSession.getAttribute(PhConstants.WARMUP_HEADER) != null) {
							try {
								if (logger.isInfoEnabled()) {
									logger.info(dialogueId + ":: This is a warm-up CAP call.");
								}
								legData.set(
										LegDataAttributes.P_LEG_SS7_STATE,
										CapV2CallStates.TERMINATED);
								/*
								 *  Notify service that call is dropped
								 */
								Capv2ScfProtocolHelper.notifyCallDropped(tcapSession, false);
								SipApplicationSession appSession = Capv2CS1ScfProtocolUtil
										.getAppSession(tcapSession);
								tcapSession.invalidate();
								if (appSession != null && appSession.isValid()) {
									appSession.invalidate();
								}

								// Note:- DO Not write CDR for warm-up calls
							} catch (Throwable e) {
								logger.warn(dialogueId + " :: Failed to handle CAP warm-up call"
										+ e.getMessage());
							}
							return;
						}

						/*
						 * special case of IDP for tracing as tracing is decided
						 * after processing IDP
						 */
						Capv2ScfProtocolHelper.traceComponent(componentIndEvent, callData);

						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);
						if (action == null) {

							if (logger.isDebugEnabled()) {
								logger.debug(dialogueId
										+ " :: Notify service to start call execution");
							}
							/*
							 * Drop the call, if failed to determine service
							 * type. This case will happen only if some DB error
							 * happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error(dialogueId
										+ ":: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
										Capv2ScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								Capv2ScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}

							// Notify Application 
							Event event = new Event(EventType.EVENT_INITIAL, Protocol.CAPV2_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}
					} else {
						logger.error(dialogueId + ":: Invalid IDP, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								Capv2ScfRelReasonCode.UNEXPECTED_IDP);
						/*
						 * Helper returns out of sequence action for the case;
						 * drop call with the action
						 */
						Action[] action = Capv2ScfProtocolHelper
								.getOutOfSequenceMsgAction(tcapSession,
										Capv2ScfProtocolEvent.IDP);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}

					break;
				}

				case CapV2OpCodes.APPLY_CHARGING_REPORT_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process ACR");
					}
					boolean isValid = Capv2ScfProtocolFSMHandler
							.validateFSMState(Capv2ScfProtocolEvent.ACR, tcapSession);

					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: VALID ACR, start processing");
						}

						Action[] action = Capv2ScfProtocolHelper.processAcr(
								invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);

						if (action == null && isLastComponent) {
							// Notify service of charging report event
							Event event = new Event(EventType.EVENT_APPLY_CHG_REPORT, Protocol.CAPV2_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}else{
							if(logger.isDebugEnabled()){
								logger.debug("ACR IS NOT LAST COMPONENT. SO NOT GENERATING ACR EVENT");
							}
						}
					} else {
						logger.error(dialogueId + ":: Invalid ACR, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
								Capv2ScfRelReasonCode.UNEXPECTED_ACR);

						/*
						 * Helper returns out of sequence action for the case;
						 * drop call with the action
						 */
						Action[] action = Capv2ScfProtocolHelper
								.getOutOfSequenceMsgAction(tcapSession,
										Capv2ScfProtocolEvent.ACR);

						// execute drop action with force false;
						executeAction(tcapSession, action);
					}
					break;
				}
				case CapV2OpCodes.EVENT_REPORT_BCSM_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process ERBCSM");
					}
					
					boolean isValid = Capv2ScfProtocolFSMHandler
							.validateFSMState(Capv2ScfProtocolEvent.ERB, tcapSession);
					
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId + ":: VALID ERB, start processing");
						}
						
						Action[] action = Capv2ScfProtocolHelper.processErb(
								invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);

					} else {
						logger.error(dialogueId
								+ ":: Invalid ERB, drop the call");

						callData.set(
								CallDataAttribute.NP_RELEASE_REASON_CODE,
								Capv2ScfRelReasonCode.UNEXPECTED_ERB);

						/*
						 * Helper returns out of sequence action for the case;
						 * drop call with the action
						 */
						Action[] action = Capv2ScfProtocolHelper
								.getOutOfSequenceMsgAction(tcapSession,
										Capv2ScfProtocolEvent.ERB);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case CapV2OpCodes.ASSIST_REQ_INST_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process ARI");
					}
					boolean isValid = Capv2ScfProtocolFSMHandler
							.validateFSMState(Capv2ScfProtocolEvent.ARI,
									tcapSession);
					if (isValid) {
						/*
						 *  process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ ":: VALID ARI, start processing");
						}

						Action[] action = Capv2ScfProtocolHelper.processARI(
								invokeIndEvent, tcapSession);
						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);

					} else {
						logger.error(dialogueId
								+ ":: Invalid ARI, drop the call");

						callData.set(
								CallDataAttribute.NP_RELEASE_REASON_CODE,
								Capv2ScfRelReasonCode.UNEXPECTED_ERB);

						/*
						 * Helper returns out of sequence action for the case;
						 * drop call with the action
						 */
						Action[] action = Capv2ScfProtocolHelper
								.getOutOfSequenceMsgAction(tcapSession,
										Capv2ScfProtocolEvent.ERB);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}

				case CapV2OpCodes.SPECIALIZED_RSOURCE_RPRT_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process SRR");
					}
					boolean isValid = Capv2ScfProtocolFSMHandler
							.validateFSMState(Capv2ScfProtocolEvent.SRR,
									tcapSession);
					if (isValid) {

						/*
						 *  process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ ":: VALID SRR, start processing");
						}

						Action[] action = Capv2ScfProtocolHelper.processSRR(
								invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);

					} else {
						logger.error(dialogueId
								+ ":: Invalid SRR, drop the call");

						callData.set(
								CallDataAttribute.NP_RELEASE_REASON_CODE,
								Capv2ScfRelReasonCode.UNEXPECTED_SRR);

						/*
						 * Helper returns out of sequence action for the case;
						 * drop call with the action
						 */
						Action[] action = Capv2ScfProtocolHelper
								.getOutOfSequenceMsgAction(tcapSession,
										Capv2ScfProtocolEvent.SRR);
						/*
						 * execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					}
					break;
				}
				case CapV2OpCodes.ENTITY_RELEASED_BYTE: {
					if (logger.isInfoEnabled()) {
						logger.info(dialogueId
								+ ":: Process Enity Released(ER)");
					}

					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_CODE,
							Capv2ScfRelReasonCode.ENTITY_RELEASE_RCVD);

					/*
					 *  no validation needed for ER
					 */
					Action[] action = Capv2ScfProtocolHelper
							.getEntityReleasedAction(invokeIndEvent,
									tcapSession);
					/*
					 * execute drop action with force false;
					 */
					executeAction(tcapSession, action);
					break;
				}
				default: {
					logger.error(dialogueId
							+ ":: Unknown INVOKE Indication Event "
							+ operCodeStr);

					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_CODE,
							Capv2ScfRelReasonCode.UNKNOWN_COMPIND_RCVD);

					Action[] action = Capv2ScfProtocolHelper
							.getUnknownMessageAction(tcapSession,
									Capv2ScfProtocolEvent.UNKNOWN);
					/*
					 *  execute drop action with force false;
					 */
					executeAction(tcapSession, action);
					break;
				}
				}

				break;
			}

			case TcapConstants.PRIMITIVE_RESULT: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ ":: Primitive Type is PRIMITIVE_RESULT");
				}

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.PRIM_RESULT_RCVD);

				ResultIndEvent resultIndEvent = (ResultIndEvent) componentIndEvent;
				Action[] action = Capv2ScfProtocolHelper.processResult(
						resultIndEvent, tcapSession);
				/*
				 * execute drop action with force false;
				 */
				executeAction(tcapSession, action);

				break;
			}
			case TcapConstants.PRIMITIVE_ERROR: {
				logger.warn(dialogueId + ":: PRIMITIVE_ERROR is received");

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.PRIM_ERROR_RCVD);

				ErrorIndEvent errorIndEvent = (ErrorIndEvent) componentIndEvent;
				Action[] action = Capv2ScfProtocolHelper.processError(
						errorIndEvent, tcapSession);
				/*
				 * execute drop action with force false;
				 */
				if(action != null){
					executeAction(tcapSession, action);
				}

				break;
			}

			case TcapConstants.PRIMITIVE_REJECT: {
				logger.warn(dialogueId + ":: PRIMITIVE_REJECT is received");

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.PRIM_REJECT_RCVD);

				RejectIndEvent rejectIndEvent = (RejectIndEvent) componentIndEvent;
				Action[] action = Capv2ScfProtocolHelper.processReject(
						rejectIndEvent, tcapSession);

				/*
				 * execute drop action with force false;
				 */
				executeAction(tcapSession, action);
				break;
			}
			}

			isLastComponent = componentIndEvent.isLastComponent();
			CapV2CallStates cap_CALL_STATES = (CapV2CallStates) legData
					.get(LegDataAttributes.P_LEG_SS7_STATE);
			/*
			 * process end dialog if call is not terminated and last component
			 * is processed.
			 */

			if (isLastComponent
					&& (lastRxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END)
					&& cap_CALL_STATES == CapV2CallStates.INIT.TERMINATED) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ ":: last componet of End dialog processed");
					logger.debug(dialogueId
							+ ":: Notify service of call dropped.");
				}

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.END_RCVD_WITH_COMP);
				Capv2ScfProtocolHelper.preProcessDroppedCall(tcapSession);

				/*
				 *  notify service and write CDRs
				 */

				Capv2ScfProtocolHelper.postProcessDroppedCall(tcapSession,
						true);
			}
		} catch (MandatoryParameterNotSetException e) {
			logger.error(
					dialogueId
					+ ":: MandatoryParameterNotSetException fetching component event",
					e);
			CallData newCallData = Capv2CS1ScfProtocolUtil
					.getCallData(tcapSession);
			if (newCallData != null) {

				/*
				 * Should not change the CAP call state
				 */

				newCallData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.COMP_MANDATORY_PARAM_MIS);
				newCallData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				Capv2ScfProtocolHelper.dropCall(tcapSession, newCallData);
			}
		} catch (Exception ex) {
			logger.error(dialogueId
					+ ":: Failed to process Component Indication event", ex);
			CallData newCallData = Capv2CS1ScfProtocolUtil
					.getCallData(tcapSession);
			if (newCallData != null) {
				/*
				 * Should not change the CAP call state
				 */
				newCallData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.EXCEP_IN_COMP_IND);
				newCallData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				Capv2ScfProtocolHelper.dropCall(tcapSession, newCallData);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processComponentIndEvent() Exit");
		}
	}

	/**
	 * This method is used to execute the action returned by CAP PH
	 * 
	 * @param tcapSession
	 * 					represents an instance of TcapSession
	 * @param actionArr
	 * 				Represents an array of Action
	 * @throws Exception
	 */
	private void executeAction(TcapSession tcapSession, Action[] actionArr)
			throws Exception {
		CallData callData = Capv2CS1ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);
		Integer dialogId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]:: inside executeAction with tcapSession and action array");
		}

		if (actionArr == null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Do nothing as action array is null");
			}
			return;
		}
		int lastRxDialoguePrimitiveType = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
		if (lastRxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Last dialogue is PRIMITIVE_END");
			}
			List<Action> actArr = new ArrayList<Action>(actionArr.length);
			for (int i = 0; i < actionArr.length; i++) {
				Action action = actionArr[i];
				if (action.getActionType() == ActionType.ACTION_END_CALL
						|| action.getActionType() == ActionType.ACTION_DISCONNECT) {
					Capv2ScfProtocolHelper.setCallDataParamsForCDR(callData,
							action);
				}
				if (action.getActionType() == ActionType.ACTION_END_CALL
						|| action.getActionType() == ActionType.ACTION_DISCONNECT) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Allowed action is "
								+ action.getActionType());
					}
					actArr.add(action);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Ignored action is "
								+ action.getActionType());
					}
				}
			}
			actionArr = actArr.toArray(new Action[actArr.size()]);
		}

		int callState = CallTraceService.CALL_IN_PROGRESS;

		/*
		 * moved tracing in begin as it should be done before message is
		 * sent. and used call in progress state as term message will use
		 * terminated state.
		 */
		String traceFlag = (String) callData
				.get(CallDataAttribute.P_TRACE_FLAG);

		if (PhConstants.TRUE.equals(traceFlag)) {

			StringBuilder traceMsg = (StringBuilder) callData
					.get(CallDataAttribute.P_TRACE_MESSAGE);

			if (traceMsg.length() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId + ":: Trace message is " + traceMsg);
				}

				if (callData
						.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData
							.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);

					for (int constraint : constraintList) {
						PhUtilityServices
						.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService()
						.trace(constraint, String.valueOf(dialogueId),
								traceMsg.toString(), callState);
					}
				}
				traceMsg.delete(0, traceMsg.length());
			}
		}
		CapV2CallStates lastCallState = (CapV2CallStates) legData
				.get(LegDataAttributes.P_LEG_SS7_STATE);

		/*
		 * To maintain the order of action in actionArr, iterate using index
		 */
		for (int i = 0; i < actionArr.length; i++) {
			Action action = actionArr[i];

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Action to be performed is "
						+ action.getActionType());
			}

			callData.set(CallDataAttribute.P_LAST_CALL_ACTION,
					action);
			switch (action.getActionType()) {
			case ACTION_CONNECT:
				Capv2ScfProtocolHelper.connectTerm(tcapSession, callData,
						action);
				break;
			case ACTION_END_CALL:
			case ACTION_DISCONNECT:
				Capv2ScfProtocolHelper
				.dropCall(tcapSession, callData, action);
				break;
			case ACTION_CONTINUE:
				Capv2ScfProtocolHelper.sendContinueMessage(tcapSession,
						action, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService());
				break;
			default:
				logger.error(dialogId + ":: Unsupported Action, drop the call");
				logger.error("CAP v2  Call State:" + lastCallState);
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.UNEXP_ACTION);
				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				Capv2ScfProtocolHelper.dropCall(tcapSession, callData);
				break;
			}
		}

		CapV2CallStates currCallState = (CapV2CallStates) legData
				.get(LegDataAttributes.P_LEG_SS7_STATE);

		/*
		 * replicate if last call state is assist and curr call state is not
		 * terminated/term in progress
		 */
		if (lastCallState == CapV2CallStates.ASSIST
				&& currCallState != CapV2CallStates.TERMINATED
				&& currCallState != CapV2CallStates.TERMINATION_IN_PROGRESS) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Replicate TcapSession");
			}
			tcapSession.replicate();
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId
					+ "[PH]:: Exit from executeAction with tcapSession and action array");
		}
	}

	/**
	 * This method id used to handle timers created by CAP PH
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
			logger.info(dialogId + ":: Received timeout for CAP Timer");
		}
		try {
			appSession = timer.getApplicationSession();

			if (appSession == null || !appSession.isValid()) {
				if (logger.isInfoEnabled()) {
					logger.info(dialogId
							+ ":: Do nothing as timer appsession is null or invalidated");
				}
				return;
			}
			callData = Capv2CS1ScfProtocolUtil.getCallData(appSession);

			dialogId = (Integer) callData
					.get(CallDataAttribute.P_DIALOG_ID);

			TcapSession tcapSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getTcapProvider().getTcapSession(dialogId);

			PhTimerInfo phTimerInfo = (PhTimerInfo) timer.getInfo();
			String timerName = phTimerInfo.getTimerName();

			CallTraceService cCallTraceService = PhUtilityServices
					.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

			if (logger.isInfoEnabled()) {
				logger.info(dialogId + ":: Received timeout for the timer "
						+ timerName);
			}
			/*
			 * This check is to avoid un-necessary processing on timeout of a
			 * timer, which has been removed from appSession due to some call
			 * cleanup activity or successful handoff/assist handling.
			 */
			if (appSession.getAttribute(timerName) == null) {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogId
							+ ":: Do nothing as the timer is not in appsession");
					logger.debug(dialogId + ":: Cleanup has been performed");
				}
				return;
			}

			origLegData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
			CapV2CallStates origState = (CapV2CallStates) origLegData
					.get(LegDataAttributes.P_LEG_SS7_STATE);

			if (logger.isInfoEnabled()) {
				logger.info(dialogId + ":: CAP call state of orig is  "
						+ origState);
			}

			if (timerName.equals(PhConstants.CDR_TIMER)) {
				boolean timerProcessingRequired = Capv2ScfProtocolFSMHandler
						.validateFSMState(Capv2ScfProtocolEvent.CDR_TIMEOUT,
								tcapSession);
				if (timerProcessingRequired) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Handle timeout in CAPCallState=CONNECTED");
					}

					// CommonUtils.incrementIntermediateCdr(callData);

					/*
					 *  Set App-session timeout to 24 hours and 5 minute
					 */
					CommonUtils
					.setAppSessionTimeout(appSession, 1445, dialogId);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Write intermediate CDR.");
					}
					Capv2CS1ScfProtocolUtil.writeServiceCdr(tcapSession);

					/*
					 * this is a special case replicating tcapsesison as it will
					 * be required for CDR params
					 */
					tcapSession.replicate();

				} else {
					logger.error(dialogId
							+ ":: Do nothing as timeout received in invalid call state");
					logger.error(origLegData
							.get(LegDataAttributes.P_LEG_SIP_STATE)
							+ " "
							+ origLegData
							.get(LegDataAttributes.P_LEG_SS7_STATE));
				}
			} else if (timerName.equals(PhConstants.AT_ACK_TIMER)) {
				boolean timerProcessingRequired = Capv2ScfProtocolFSMHandler
						.validateFSMState(Capv2ScfProtocolEvent.AT_TIMEOUT,
								tcapSession);
				if (timerProcessingRequired) {
					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
					callData.set(
							CallDataAttribute.NP_FAILED_CALL_IND, 1);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Set failed call indicator to 1");
						logger.debug(dialogId
								+ ":: Notify service that call is dropped");
						logger.debug(dialogId + ":: Write CDR");
					}

					callData.set(
							CallDataAttribute.P_DFC_REQUIRED_FLAG, PhConstants.FALSE);

					/*
					 * mark call state to terminated to avoid other action
					 */
					Capv2ScfProtocolHelper.preProcessDroppedCall(tcapSession);

					logger.error(dialogId + ":: AT timedout, send U-Abort");

					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							Capv2ScfRelReasonCode.ACT_TEST_TIMEOUT);

					Action action = new Action(ActionType.ACTION_END_CALL);
					action.setDropCallMode(Action.DROP_CALL_MODE.USER_ABORT);

					action.setReleaseCauseValue(Capv2ScfProtocolFSMHandler.AbortInfoEnum.ABNORMAL_PROCESSING
							.getCode());

					Capv2ScfProtocolHelper.sendDropMessage(tcapSession,
							action, cCallTraceService);
					/*
					 * notify service and write CDrs
					 */
					Capv2ScfProtocolHelper.postProcessDroppedCall(
							tcapSession, true);

				}

			} else if (timerName.equals(PhConstants.CORRELATION_TIMER)) {

				boolean timerProcessingRequired = Capv2ScfProtocolFSMHandler
						.validateFSMState(
								Capv2ScfProtocolEvent.CORRELATION_TIMEOUT,
								tcapSession);
				if (timerProcessingRequired) {
					callData.set(
							CallDataAttribute.P_DFC_REQUIRED_FLAG,
							PhConstants.TRUE);
					callData.set(
							CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
					callData.set(
							CallDataAttribute.NP_FAILED_CALL_IND, 1);
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId
								+ ":: Set failed call indicator to 1 and drop the call");
					}
					logger.error(dialogId
							+ ":: Drop the call as correlation timer expired for "
							+ callData
							.get(CallDataAttribute.P_CORRELATION_ID));
					callData.set(
							CallDataAttribute.NP_REASON_FOR_RELEASE,
							Capv2ScfRelReasonCode.CORRELATION_TIMEOUT);
					Capv2ScfProtocolHelper.dropCall(tcapSession, callData);
				}
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
