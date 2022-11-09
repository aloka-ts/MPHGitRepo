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
import java.util.Iterator;
import java.util.List;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;

import org.apache.log4j.Logger;
import org.apache.log4j.MDC;

import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.Action.ActionType;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.InapCallStates;
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
import com.agnity.ph.inapcs2scf.flowhelper.InapCS2ATHelper;
import com.agnity.ph.inapcs2scf.flowhelper.InapCS2BCSMHelper;
import com.agnity.ph.inapcs2scf.flowhelper.InapCS2MediaServerHelper;
import com.agnity.ph.sip.SipProtocolConfig;
import com.agnity.ph.sip.SipProtocolRelReasonCode;
import com.baypackets.ase.common.Registry;
import com.baypackets.ase.container.AseApplicationSession;
import com.baypackets.ase.container.AseContext;
import com.baypackets.ase.container.AseHost;
import com.baypackets.ase.util.CallTraceService;
import com.baypackets.ase.util.Constants;
import com.genband.tcap.provider.TcapProvider;
import com.genband.tcap.provider.TcapSession;

import  org.apache.commons.lang3.mutable.MutableInt;

/**
 * This class is used to handle INAP Protocol Specific events. this handler class
 * is invoked by ProtocolHandler servlet for processing of dialog and components events
 * This class processes these events and delegates the processing of the messages
 * to Inap Protocol Helper class
 * <p>
 * A new CallData Object is created on initial DP invocation and saved
 * in TCapSession and appsession for this call. This object then remains same for all the subsequent messages
 * and works as a data object for whole call
 */

public class InapCS2ScfProtocolHandler implements SS7ProtocolHandler {

	private static final InapCS2ScfProtocolHandler INSTANCE = new InapCS2ScfProtocolHandler();
	private static Logger logger = Logger.getLogger(InapCS2ScfProtocolHandler.class);

	private InapCS2ScfProtocolHandler() {
	}

	public static InapCS2ScfProtocolHandler getInstance() {
		return INSTANCE;
	}

	/**
	 * This method is called by the Protocolrouter to execute a  action returned by the service
	 *
	 * @param callData data object for the call
	 * @param action   action returned by the application
	 */
	public void executeAction(CallData callData, Action action) throws Exception {


		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH]:: executeAction() Enter with callData and action ");
		}
		TcapSession tcapSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider().getTcapSession(dialogId);
		InapCS2ScfCallTraceHelper.traceMessage(callData, tcapSession);
		
		AseApplicationSession appSession=(AseApplicationSession)InapCS2ScfProtocolUtil.getAppSession(tcapSession);
		
		
		AseHost Host= (AseHost) Registry.lookup(Constants.NAME_HOST);
		Iterator contexts =Host.findContextByNamePrefix("tcap-provider");
		AseContext actxt=null;
		while (contexts!=null && contexts.hasNext()) {
			 actxt = (AseContext) contexts.next();

			if (logger.isDebugEnabled()) {
				logger.debug(" Asecontext is: " + actxt);
			}
			
			break;
		}
		if (actxt != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(" update tcap-provider Asecontext ");
			}
			appSession.setContext(actxt);

			actxt.addApplicationSession((AseApplicationSession) appSession);
		}

		if (action == null) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Do nothing as action array is null");
			}
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Action to be performed is " + action.getActionType());
		}

		callData.set(CallDataAttribute.P_LAST_CALL_ACTION, action);
		switch (action.getActionType()) {
		case ACTION_CONNECT: {

			String isChainingEnabled=InapCS2ScfProtocolConfig.getConfigData(InapCS2ScfProtocolConfig.SERVICE_CHAINING_ENABLED);

			if (PhConstants.FALSE.equals(isChainingEnabled)
					|| !InapCS2ScfProtocolHelper.invokeServiceChaining(tcapSession, callData, action)) {
				InapCS2ScfProtocolHelper.connectTerm(tcapSession, callData, action);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("::Could not invoke Connect Action "
							+ action.getActionType());
				}
			}
			break;
		}
		case ACTION_DISCONNECT: {
			InapCS2ScfProtocolHelper.disconnectTerm(tcapSession, callData, action);
			break;
		}
		case ACTION_END_CALL: {
			action.setServiceComplete(true);
			InapCS2ScfProtocolHelper.dropCall(tcapSession, callData, action);
			break;
		}
		case ACTION_REDIRECT: {

			String isChainingEnabled=InapCS2ScfProtocolConfig.getConfigData(InapCS2ScfProtocolConfig.SERVICE_CHAINING_ENABLED);

			if (PhConstants.FALSE.equals(isChainingEnabled)
					|| !InapCS2ScfProtocolHelper.invokeServiceChaining(tcapSession, callData, action)) {
				InapCS2ScfProtocolHelper.redirect(tcapSession, callData, action);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("::Could not invoke Connect Action "
							+ action.getActionType());
				}
			}    
			break;
		}
		case ACTION_CONNECT_MS: {
			InapCS2MediaServerHelper.initiateMediaServerConnection(tcapSession, callData, action);
			break;
		}
		case ACTION_DISCONNECT_MS: {
			InapCS2MediaServerHelper.disconnectIvr(tcapSession, callData, action);
			break;
		}
		case ACTION_PLAY: {
			InapCS2MediaServerHelper.sendPlayAnnouncement(tcapSession, callData, action);
			break;
		}
		case ACTION_PLAY_COLLECT: {
			InapCS2MediaServerHelper.sendPromptAndCollectAnnouncement(tcapSession, callData, action);
			break;
		}
		case ACTION_PLAY_RECORD: {
			InapCS2MediaServerHelper.playAndRecord(tcapSession, callData, action);
			break;
		}
		case ACTION_PROCESS_CALLS: {
			break;
		}
		case ACTION_CONTINUE: {
			InapCS2ScfProtocolHelper.sendContinueComponent(tcapSession, callData, action);
			break;
		}
		case ACTION_CHRG: {
			InapCS2ScfProtocolHelper.performCharging(tcapSession, callData, action);
			break;
		}
		case ACTION_HTTP_REQ: {
			InapCS2ScfProtocolHelper.sendHttpRequest(tcapSession, callData, action);
			break;
		}
		case ACTION_LS_CMD: {
			InapCS2ScfProtocolHelper.sendLsRequest(tcapSession, callData, action);
			break;
		}
		case ACTION_SERVICE_COMPLETE: {
			InapCS2ScfProtocolHelper.serviceComplete(tcapSession, callData,
					action);
			break;
		}
		case ACTION_INVOKE_SVC_CHAINING: {
			InapCS2ScfProtocolHelper.invokeServiceChaining(tcapSession, callData,
					action);
			break;
		}
		case ACTION_WRITE_CDR: {
			InapCS2ScfProtocolUtil.writeServiceCdr(tcapSession);
		}break;
		case ACTION_START_TIMER: {
			InapCS2ScfProtocolHelper.startApplicationTimer(tcapSession, callData, action);
			break;
		}
		case ACTION_STOP_TIMER: {
			InapCS2ScfProtocolHelper.stopApplicationTimer(tcapSession, callData, action);
			break;
		}
		default: {
			logger.error("INAPCS2ProtocolHandler Not supported:" + action.getActionType());
			break;
		}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: executeAction() Exit with callData and action");
		}
	}

	/**
	 * This method id used to handle timers created by INAP PH
	 *
	 * @param timer
	 * @return void
	 */
	public final void timeout(ServletTimer timer) {
		SipApplicationSession appSession;
		CallData callData;
		int dialogId;
		LegData origLegData;

		if (logger.isInfoEnabled()) {
			logger.info("[PH]:: Received timeout for INAP Timer");
		}
		try {
			appSession = timer.getApplicationSession();
			callData = (CallData) appSession.getAttribute(CallData.CALL_DATA);
			dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
			MDC.put(PhConstants.MDC_CALL_ID_CONST, "[" + dialogId + "]");

			TcapSession tcapSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider().getTcapSession(dialogId);

			tcapSession.setAttribute(CallData.CALL_DATA, callData);
			CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
			InapCallStates origState = (InapCallStates) origLegData.get(LegDataAttributes.P_LEG_SS7_STATE);

			if (appSession == null || !appSession.isValid()) {
				if (logger.isInfoEnabled()) {
					logger.info("[PH]:: Do nothing as timer appsession is null or invalidated");
				}
				return;
			}

			String timerName;
			if (timer.getInfo() instanceof PhTimerInfo) {
				timerName = ((PhTimerInfo) timer.getInfo()).getTimerName();
			} else if (timer.getInfo() instanceof String) {
				timerName = (String) timer.getInfo();
			} else {
				return;
			}

			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Received timeout for the timer " + timerName);
			}

			/*
			 * This check is to avoid un-necessary processing on timeout of a
			 * timer, which has been removed from appSession due to some call
			 * cleanup activity or successful handoff/assist handling.
			 */
			if (appSession.getAttribute(timerName) == null) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Do nothing as the timer is not in appsession");
					logger.debug("[PH]:: Cleanup has been performed");
				}
				return;
			}

			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: INAP call state of orig is  " + origState);
			}

			if (timerName.equals(PhConstants.CDR_TIMER)) {
				boolean timerProcessingRequired = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.CDR_TIMEOUT, tcapSession);
				if (timerProcessingRequired) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Handle timeout in InapCallState = CONNECTED");
					}

					// Set App-session timeout to 24 hours and 5 minute
					CommonUtils.setAppSessionTimeout(appSession, 1445, dialogId);

					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Write intermediate CDR.");
					}
					InapCS2ScfProtocolUtil.writeServiceCdr(tcapSession);

					// This is a special case replicating tcapsesison as it will be required for CDR params
					tcapSession.replicate();
				} else {
					logger.error("[PH]:: Do nothing as timeout received in invalid call state");
					logger.error(origLegData.get(LegDataAttributes.P_LEG_SIP_STATE) + " " + origLegData.get(LegDataAttributes.P_LEG_SS7_STATE));
				}
			} else if (timerName.equals(PhConstants.AT_ACK_TIMER)) {
				InapCS2ATHelper.dropCallOnATTimeout(callData);
			} else if (timerName.equals(PhConstants.CORRELATION_TIMER)) {
				boolean timerProcessingRequired = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.CORRELATION_TIMEOUT, tcapSession);
				if (timerProcessingRequired) {
					callData.set(CallDataAttribute.P_DFC_REQUIRED_FLAG, PhConstants.TRUE);
					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
					callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Set failed call indicator to 1 and drop the call");
					}
					logger.error("[PH]:: Drop the call as correlation timer expired for " + callData.get(CallDataAttribute.P_CORRELATION_ID));
					callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE, InapCS2ScfRelReasonCode.CORRELATION_TIMEOUT);
					InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
				}
			} else if (timerName.equals(com.baypackets.ase.util.Constants.ACTIVITY_TEST_TIMER)) {
				InapCS2ATHelper.callHeartBeat(tcapSession, callData);
			} else if (timerName.startsWith(PhConstants.MAX_CALL_DURATION_TIMER)) {

				if (logger.isDebugEnabled()) {
					logger.debug(":: Max call duration timer timedout drop the call  ");
				}

				callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE,
						SipProtocolRelReasonCode.MAX_DURATION_REACHED);
				InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
				return;

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
						Protocol.ITUINAPCS2_SCF, null);
				event.setTimerName(timerName);

				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			} else {
				logger.warn("[PH]:: Unknown timer: " + timerName);
			}
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Exiting timeout");
			}
		} catch (Exception e) {
			logger.error("[PH]:: Exception  timeout " + e.getMessage());
			if (logger.isDebugEnabled()) {
				logger.error("[PH]:: Exception  timeout " + e);
			}
		} finally {
			MDC.remove(PhConstants.MDC_CALL_ID_CONST);
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
	 * validation call the processBegin method of InapSmHelper to get next
	 * action to be executed. For Invalid dialoge BEGIN, drop call.
	 * <li>For primitive type PRIMITIVE_CONTINUE validate the FSM state and call
	 * processContinue method of InapSmHelper to get next action to be executed.
	 * For Invalid dialoge CONTINUE, drop call.
	 * <li>For primitive type PRIMITIVE_END validate the FSM state and call
	 * processEnd method of InapSmHelper to get next action to be executed.For
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
	 * @param dialogueIndEvent represents the instance of DialogueIndEvent
	 * @see jain.protocol.ss7.tcap.JainTcapListener#processDialogueIndEvent(jain.protocol.ss7.tcap.DialogueIndEvent)
	 */
	public void processDialogueIndEvent(DialogueIndEvent dialogueIndEvent, ServiceInterface serviceHandler) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: processDialogueIndEvent() Enter");
		}
		int dialogueId = 0;
		TcapSession tcapSession = null;

		TcapProvider sTcapProvider = PhUtilityServices.getInstance(serviceHandler.getApplicationName()).getTcapProvider();

		CallData callData = null;
		LegData legData = null;
		try {
			dialogueId = dialogueIndEvent.getDialogueId();
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Processing Dialogue Indication Event " + dialogueIndEvent.getPrimitiveType());
			}
			tcapSession = sTcapProvider.getTcapSession(dialogueId);
			// fetch call data from tcap session
			callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);

			if (callData != null) {
				InapCS2ScfCallTraceHelper.traceDialog(dialogueIndEvent, callData);
				//set last Rx dialogue primitive to validate component against dialogue
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Setting rx dialog primitive for future reference");
				}
				callData.set(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE, dialogueIndEvent.getPrimitiveType());
				((MutableInt) callData.get(CallDataAttribute.P_NETWORK_TRANSACTION)).increment();
			}

			switch (dialogueIndEvent.getPrimitiveType()) {
			case TcapConstants.PRIMITIVE_BEGIN: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Primitive Type is PRIMITIVE_BEGIN");
				}

				//creating and storing call dat
				if (callData == null) {
					callData = new CallData();
					legData = new LegData();
					callData.set(CallDataAttribute.NP_FT_CALL, true);
					callData.set(CallDataAttribute.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(CallDataAttribute.P_CALL_START_TIME, new Date());
					callData.set(CallDataAttribute.P_LEG1, legData);
					callData.set(CallDataAttribute.P_PROTOCOL, Protocol.ITUINAPCS2_SCF);
					callData.set(CallDataAttribute.P_DIALOG_ID, dialogueId);
					callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, String.valueOf(dialogueId));
					callData.set(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE, dialogueIndEvent.getPrimitiveType());

					SipApplicationSession appSession = InapCS2ScfProtocolUtil.getAppSession(tcapSession);
					appSession.setAttribute(PhConstants.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(CallDataAttribute.P_APP_SESSION_ID, appSession.getId());

					//Store the tcap session reference in appSession as in timeout it would be required
					appSession.setAttribute(PhConstants.TCAP_SESSION_ID, tcapSession.getDialogueId());

					InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.INIT);
					legData.set(LegDataAttributes.P_DIALOG_ID, tcapSession.getDialogueId());
					legData.set(LegDataAttributes.P_CONNECTION_TYPE, ConnectionType.ORIG_CONNECTION);
					tcapSession.setAttribute(CallData.CALL_DATA, callData);
					appSession.setAttribute(CallData.CALL_DATA, callData);
					callData.set(CallDataAttribute.P_TRACE_FLAG, PhConstants.FALSE);

					callData.set(CallDataAttribute.P_NETWORK_TRANSACTION, new MutableInt(1));

				} else {
					logger.error("[PH]:: Recieveid BEGIN for existing call.");
					// not doing anything as call will be cleaned through validate fsm state
				}

				//validating message
				boolean isValidEvent = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.BEGIN, tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Valid BEGIN start processing");
					}
					Boolean dumpCounters = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.DUMP_COUNTERS));
					if(dumpCounters) {
						PhMeasurementService.getInstance().getMeasurementCounter(Protocol.ITUINAPCS2_SCF).
						incrementServiceTriggeredCount(serviceHandler.getApplicationName(), false);

					}
					InapCS2ScfProtocolHelper.processBegin(dialogueIndEvent, tcapSession);
				} else {
					logger.error("[PH]:: Invalid dialoge BEGIN, drop call");

					//invalid event get action from helper and do the same as drop call
					Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceDialogAction(tcapSession, TcapConstants.PRIMITIVE_BEGIN);
					//execute drop action with force false;
					for (Action actionField : action) {
						InapCS2ScfProtocolHelper.dropCall(tcapSession, callData, actionField);
					}
				}
				break;
			}
			case TcapConstants.PRIMITIVE_CONTINUE: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Primitive Type is PRIMITIVE_CONTINUE");
				}
				boolean isValidEvent = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.CONTINUE, tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Valid CONTINUE event start processing");
					}
					Action[] action = InapCS2ScfProtocolHelper.processContinue(dialogueIndEvent, tcapSession);
					executeAction(tcapSession, action);
				} else {
					logger.error("[PH]:: Invalid dialoge CONTINUE, drop call");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXPECTED_CONTINUE);

					//Helper returns out of sequence action for the case; dropcall with the action
					Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceDialogAction(tcapSession, TcapConstants.PRIMITIVE_CONTINUE);

					// execute drop action with force false;
					for (Action actionField : action) {
						InapCS2ScfProtocolHelper.dropCall(tcapSession, callData, actionField);
					}
				}
				break;
			}
			case TcapConstants.PRIMITIVE_END: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Primitive Type is PRIMITIVE_END");
				}

				boolean isValidEvent = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.END, tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Valid EVENT event start processing");
					}
					EndIndEvent endIndEvent = (EndIndEvent) dialogueIndEvent;
					/*
					 * usually end is sent by service.. its error flow so
					 * setting default cause value as 41 for CDR and marking
					 * call as failed call if there is some component in end it
					 * will update CDR Mark cause val as 41 for failed call
					 */
					callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

					// drop call if no component is present
					if (!(endIndEvent.isComponentsPresent())) {
						logger.warn("END with no component, drop call");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.END_RCVD_WITHOUT_COMP);

						// clean call as no component is present
						InapCS2ScfProtocolHelper.preProcessDroppedCall(tcapSession);

						//notify service and write CDRs
						InapCS2ScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
					}
				} else {
					logger.error("[PH]:: Invalid dialoge END, drop call");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXPECTED_END);

					Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceDialogAction(tcapSession, TcapConstants.PRIMITIVE_END);

					// execute drop action with force false;
					for (Action actionField : action) {
						InapCS2ScfProtocolHelper.dropCall(tcapSession, callData, actionField);
					}
				}
				break;
			}
			case TcapConstants.PRIMITIVE_USER_ABORT: {
				if (logger.isInfoEnabled()) {
					logger.info("[PH]:: Handling U-ABORT");
				}
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UABORT_RCVD);

				/*
				 * Mark cause val as 31 as u-abort will be rcvd when user hung
				 * up and related events are not armed.
				 */
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 31);

				Action[] action = InapCS2ScfProtocolHelper.processUAbort(dialogueIndEvent, tcapSession);
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Returned actions are " + action);
				}

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Cleaning the call");
				}

				//clean call if no component is present
				InapCS2ScfProtocolHelper.preProcessDroppedCall(tcapSession);

				//notify service and write CDRs
				InapCS2ScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
				break;
			}
			case TcapConstants.PRIMITIVE_PROVIDER_ABORT:
			case TcapConstants.PRIMITIVE_NOTICE: {
				if (logger.isInfoEnabled()) {
					logger.info("[PH]:: Handling P-ABORT/NOTICE");
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.PABORT_NOTICE_RCVD);

				//Mark cause val as 41 for failed call
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Cleaning the call");
				}

				//clean call if no component is present
				InapCS2ScfProtocolHelper.preProcessDroppedCall(tcapSession);

				//notify service and write CDRs
				InapCS2ScfProtocolHelper.postProcessDroppedCall(tcapSession, true);

				break;
			}
			default: {
				logger.error("[PH]:: unrecognized Primitive Type " + dialogueIndEvent.getPrimitiveType());
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNKNOWN_DIALOG_RCVD);
				Action[] action = InapCS2ScfProtocolHelper.getUnknownDialogAction(tcapSession);
				// execute drop action with force false;
				for (Action actionField : action) {
					InapCS2ScfProtocolHelper.dropCall(tcapSession, callData, actionField);
				}
				break;
			}
			}
		} catch (MandatoryParameterNotSetException e) {
			logger.error("[PH]:: MandatoryParameterNotSetException for dialogue event", e);
			CallData newcallData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
			if (newcallData != null) {
				newcallData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.DIAGLOG_MANDATORY_PARAM_MIS);
				newcallData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				InapCS2ScfProtocolHelper.dropCall(tcapSession, newcallData);
			}
		} catch (Exception ex) {
			logger.error("[PH]:: Failed to process Dialogue Indication event", ex);
			CallData newcallData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
			if (newcallData != null) {
				newcallData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.EXCEP_IN_DIALOG_IND);
				newcallData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				InapCS2ScfProtocolHelper.dropCall(tcapSession, newcallData);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: processDialogueIndEvent() Exit");
		}
	}

	/**
	 * Tcaplistener API implemented by the service for receiving a callback on a
	 * component indication event occuring on the node.This method check the
	 * primitive type for the component indication event. For Primitive Type
	 * PRIMITIVE_INVOKE, PH checks for the opreation code.
	 * <ul>
	 * <li>If operation code is IDP_BYTE, then validate the message for FSM
	 * state.After validation call the processIdp method of InapSmHelper to get
	 * next action to be executed.If it is a first call SAS will generate an
	 * invite and send a warm up call to service.Helper return action in case of
	 * error so drop call with action drop action with force false. For no error
	 * case do service specific IDP processing notify service to start call
	 * execution. For Invalid IDP, drop the call.
	 * <li>If operation code is ENC_BYTE, then validate the message for FSM
	 * state.After validation call the processENC method of InapSmHelper to get
	 * next action to be executed.For Invalid ENC, drop the call
	 * <li>If operation code is ERB_BYTE, then validate the message for FSM
	 * state.After validation call the processENC method of InapSmHelper to get
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
	 * @param componentIndEvent represents the instance of ComponentIndEvent
	 * @see jain.protocol.ss7.tcap.JainTcapListener#processComponentIndEvent(jain.protocol.ss7.tcap.ComponentIndEvent)
	 */

	public void processComponentIndEvent(ComponentIndEvent componentIndEvent, ServiceInterface serviceInterface) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: processComponentIndEvent() Enter");
		}
		int dialogueId = 0;
		TcapSession tcapSession = null;
		try {
			dialogueId = componentIndEvent.getDialogueId();
			if (logger.isInfoEnabled()) {
				logger.info("[PH]:: Processing Component Indication Event " + componentIndEvent.getPrimitiveType());
			}

			tcapSession = PhUtilityServices.getInstance(serviceInterface.getApplicationName()).getTcapProvider().getTcapSession(dialogueId);

			CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
			LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

			int lastRxDialoguePrimitiveType = -1;

			if (callData != null) {
				lastRxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
				legData.set(LegDataAttributes.P_LAST_RX_INVOKE_ID, componentIndEvent.getInvokeId());
				InapCS2ScfCallTraceHelper.traceComponent(componentIndEvent, callData);
			}

			switch (componentIndEvent.getPrimitiveType()) {
			case TcapConstants.PRIMITIVE_INVOKE: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Primitive Type is PRIMITIVE_INVOKE");
				}

				InvokeIndEvent invokeIndEvent = (InvokeIndEvent) componentIndEvent;
				byte[] operCode = invokeIndEvent.getOperation().getOperationCode();
				byte operCodeByte = operCode[0];
				String operCodeStr = CommonUtils.formatBytes(operCode);
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Operation Code is " + operCodeStr);
				}

				switch (operCodeByte) {
				case InapOpCodes.IDP_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Process IDP");
					}

					//Drop the call, if service failed to read configuration
					if (!PhUtilityServices.getInstance(serviceInterface.getApplicationName()).isServiceInitialized()) {
						logger.error("[PH]:: Service failed to read configuration. Drop Call. Restart SAS to solve the issue");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);
						InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}

					boolean isValid = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.IDP, tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: Valid IDP event start processing");
						}

						Action[] action = InapCS2ScfProtocolHelper.processIdp(invokeIndEvent, tcapSession);
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]::  processIdp Exit ");
						}
						/*
						 * First SIP-T and INAP call were taking almost 11
						 * seconds to process. To solve this issue SAS will
						 * generate an invite and send a warm up call to
						 * service.
						 */
						if (tcapSession.getAttribute(PhConstants.WARMUP_HEADER) != null) {
							try {
								if (logger.isInfoEnabled()) {
									logger.info("[PH]:: This is a warm-up INAP call.");
								}
								InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.TERMINATED);

								// Notify service that call is dropped
								InapCS2ScfProtocolHelper.notifyCallDropped(tcapSession, false);
								SipApplicationSession appSession = InapCS2ScfProtocolUtil.getAppSession(tcapSession);
								tcapSession.invalidate();
								if (appSession != null && appSession.isValid()) {
									appSession.invalidate();
								}
								//Note:- DO Not write CDR for warm-up calls
							} catch (Throwable e) {
								logger.warn("[PH]:: Failed to handle INAP warm-up call" + e.getMessage());
							}
							return;
						}
						/*
						 * special case of IDP for tracing as tracing is decided
						 * after processing IDP
						 */
						InapCS2ScfCallTraceHelper.traceComponent(componentIndEvent, callData);

						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);
						if (action == null) {
							if (logger.isDebugEnabled()) {
								logger.debug("[PH]:: Notify service to start call execution");
							}
							/*
							 * Drop the call, if failed to determine service
							 * type. This case will happen only if some DB error
							 * happens while determining service type.
							 */
							if (serviceInterface == null) {
								logger.error("[PH]:: Failed to find service type. Drop the call");
								callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
								callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
								InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
								return;
							}
							Event event = new Event(EventType.EVENT_INITIAL, Protocol.ITUINAPCS2_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}
					} else {
						logger.error("[PH]:: Invalid IDP, drop the call");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXPECTED_IDP);

						// Helper returns out of sequence action for the case drop call with the action
						Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession, InapCS2ScfProtocolEvent.IDP);

						//execute drop action with force false;
						executeAction(tcapSession, action);
					}
					break;
				}
				case InapOpCodes.ENC_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Process ENC");
					}
					boolean isValid = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.ENC, tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: Valid ENC event start processing");
						}
						Action[] action = InapCS2ScfProtocolHelper.processEnc(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					} else {
						logger.error("[PH]:: Invalid ENC, drop the call");

						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXPECTED_ENC);

						// Helper returns out of sequence action for the case drop call with the action
						Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession, InapCS2ScfProtocolEvent.ENC);

						//execute drop action with force false
						executeAction(tcapSession, action);
					}
					break;
				}
				case InapOpCodes.APPLY_CHARGING_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Process ACR");
					}
					boolean isValid = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.ACR, tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: VALID ACR, start processing");
						}
						Action[] action = InapCS2ScfProtocolHelper.processAcr(invokeIndEvent, tcapSession);

						// Helper return action in case of error so drop call with action execute drop action with force false;
						executeAction(tcapSession, action);

						if (action == null) {
							//Notify service of charging report event
							Event event = new Event(EventType.EVENT_CHRG, Protocol.ITUINAPCS2_SCF, null);
							ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
						}
					} else {
						logger.error("[PH]:: Invalid ACR, drop the call");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXPECTED_ACR);

						// Helper returns out of sequence action for the case drop call with the action
						Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession, InapCS2ScfProtocolEvent.ACR);

						// execute drop action with force false;
						executeAction(tcapSession, action);
					}
					break;
				}
				case InapOpCodes.ERB_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Process ERBCSM");
					}
					boolean isValid = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.ERB, tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: VALID ERB, start processing");
						}
						Action[] action = InapCS2BCSMHelper.processErb(invokeIndEvent, tcapSession);

						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					} else {
						logger.error("Invalid ERB, drop the call");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXPECTED_ERB);

						//Helper returns out of sequence action for the case drop call with the action
						Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession, InapCS2ScfProtocolEvent.ERB);

						//execute drop action with force false;
						executeAction(tcapSession, action);
					}
					break;
				}
				case InapOpCodes.ARI_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Process ARI");
					}
					boolean isValid = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.ARI, tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: VALID ARI, start processing");
						}
						Action[] action = InapCS2ScfProtocolHelper.processARI(invokeIndEvent, tcapSession);
						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					} else {
						logger.error("[PH]:: Invalid ARI, drop the call");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXPECTED_ERB);

						//Helper returns out of sequence action for the case; drop call with the action
						Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession, InapCS2ScfProtocolEvent.ERB);

						// execute drop action with force false;
						executeAction(tcapSession, action);
					}
					break;
				}
				case InapOpCodes.SRR_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Process SRR");
					}
					boolean isValid = InapCS2ScfProtocolFSMHandler.validateFSMState(InapCS2ScfProtocolEvent.SRR, tcapSession);
					if (isValid) {
						if (logger.isDebugEnabled()) {
							logger.debug("[PH]:: VALID SRR, start processing");
						}
						Action[] action = InapCS2MediaServerHelper.processSRR(invokeIndEvent, tcapSession);
						/*
						 * Helper return action in case of error so drop call
						 * with action execute drop action with force false;
						 */
						executeAction(tcapSession, action);
					} else {
						logger.error("[PH]:: Invalid SRR, drop the call");
						callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXPECTED_SRR);

						//Helper returns out of sequence action for the case drop call with the action
						Action[] action = InapCS2ScfProtocolHelper.getOutOfSequenceMsgAction(tcapSession, InapCS2ScfProtocolEvent.SRR);

						// execute drop action with force false;
						executeAction(tcapSession, action);
					}
					break;
				}
				case InapOpCodes.ER_BYTE: {
					if (logger.isInfoEnabled()) {
						logger.info("[PH]:: Process Enity Released(ER)");
					}

					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ENTITY_RELEASE_RCVD);

					//  no validation needed for ER
					Action[] action = InapCS2ScfProtocolHelper.getEntityReleasedAction(invokeIndEvent, tcapSession);

					//execute drop action with force false;
					executeAction(tcapSession, action);
					break;
				}
				default: {
					logger.error("[PH]:: Unknown INVOKE Indication Event " + operCodeStr);
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNKNOWN_COMPIND_RCVD);
					Action[] action = InapCS2ScfProtocolHelper.getUnknownMessageAction(tcapSession, InapCS2ScfProtocolEvent.UNKNOWN);

					// execute drop action with force false;
					executeAction(tcapSession, action);
					break;
				}
				}
				break;
			}

			case TcapConstants.PRIMITIVE_RESULT: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Primitive Type is PRIMITIVE_RESULT");
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.PRIM_RESULT_RCVD);

				ResultIndEvent resultIndEvent = (ResultIndEvent) componentIndEvent;
				Action[] action = InapCS2ScfProtocolHelper.processResult(resultIndEvent, tcapSession);

				//execute drop action with force false;
				executeAction(tcapSession, action);

				break;
			}
			case TcapConstants.PRIMITIVE_ERROR: {
				logger.warn("[PH]:: PRIMITIVE_ERROR is received");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.PRIM_ERROR_RCVD);

				ErrorIndEvent errorIndEvent = (ErrorIndEvent) componentIndEvent;
				Action[] action = InapCS2ScfProtocolHelper.processError(errorIndEvent, tcapSession);

				// execute drop action with force false;
				executeAction(tcapSession, action);

				break;
			}
			case TcapConstants.PRIMITIVE_REJECT: {
				logger.warn("[PH]:: PRIMITIVE_REJECT is received");

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.PRIM_REJECT_RCVD);

				RejectIndEvent rejectIndEvent = (RejectIndEvent) componentIndEvent;
				Action[] action = InapCS2ScfProtocolHelper.processReject(rejectIndEvent, tcapSession);

				//execute drop action with force false;
				executeAction(tcapSession, action);
				break;
			}
			}

			boolean isLastComponent = componentIndEvent.isLastComponent();
			InapCallStates inap_CALL_STATES = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

			// process end dialog if call is not terminated and last component is processed.
			if (isLastComponent &&
					(lastRxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END) &&
					inap_CALL_STATES == InapCallStates.INIT.TERMINATED) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: last componet of End dialog processed");
					logger.debug("[PH]:: Notify service of call dropped.");
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.END_RCVD_WITH_COMP);
				InapCS2ScfProtocolHelper.preProcessDroppedCall(tcapSession);

				//notify service and write CDRs
				InapCS2ScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
			}
		} catch (MandatoryParameterNotSetException e) {
			logger.error("[PH]:: MandatoryParameterNotSetException fetching component event", e);
			CallData newCallData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
			if (newCallData != null) {

				//Should not change the inap call state
				newCallData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.COMP_MANDATORY_PARAM_MIS);
				newCallData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				InapCS2ScfProtocolHelper.dropCall(tcapSession, newCallData);
			}
		} catch (Exception ex) {
			logger.error("[PH]:: Failed to process Component Indication event", ex);
			CallData newCallData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
			if (newCallData != null) {

				//Should not change the inap call state
				newCallData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.EXCEP_IN_COMP_IND);
				newCallData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				InapCS2ScfProtocolHelper.dropCall(tcapSession, newCallData);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: processComponentIndEvent() Exit");
		}
	}

	/**
	 * This method is used to execute the action returned by INAP PH
	 *
	 * @param tcapSession
	 * @param actionArr
	 * @throws Exception
	 */
	private void executeAction(TcapSession tcapSession, Action[] actionArr) throws Exception {
		CallData callData = InapCS2ScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogueId = tcapSession.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: inside executeAction with tcapSession and action array");
		}

		if (actionArr == null) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Do nothing as action array is null");
			}
			return;
		}
		int lastRxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
		if (lastRxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Last dialogue is PRIMITIVE_END");
			}
			List<Action> actArr = new ArrayList<Action>(actionArr.length);
			for (int i = 0; i < actionArr.length; i++) {
				Action action = actionArr[i];
				if (action.getActionType() == ActionType.ACTION_END_CALL || action.getActionType() == ActionType.ACTION_DISCONNECT) {
					InapCS2ScfProtocolHelper.setCallDataParamsForCDR(callData, action);
				}
				if (action.getActionType() == ActionType.ACTION_END_CALL || action.getActionType() == ActionType.ACTION_DISCONNECT) {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Allowed action is " + action.getActionType());
					}
					actArr.add(action);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Ignored action is " + action.getActionType());
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
		String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);

		if (PhConstants.TRUE.equals(traceFlag)) {
			StringBuilder traceMsg = (StringBuilder) callData.get(CallDataAttribute.P_TRACE_MESSAGE);
			if (traceMsg.length() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Trace message is " + traceMsg);
				}

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);

					for (int constraint : constraintList) {
						PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService().trace(constraint, String.valueOf(dialogueId), traceMsg.toString(), callState);
					}
				}
				traceMsg.delete(0, traceMsg.length());
			}
		}
		InapCallStates lastCallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		/*
		 * To maintain the order of action in actionArr, iterate using index
		 */
		for (int i = 0; i < actionArr.length; i++) {
			Action action = actionArr[i];

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Action to be performed is " + action.getActionType());
			}

			callData.set(CallDataAttribute.P_LAST_CALL_ACTION, action);
			switch (action.getActionType()) {
			case ACTION_CONNECT:
				InapCS2ScfProtocolHelper.connectTerm(tcapSession, callData, action);
				break;
			case ACTION_END_CALL:
			case ACTION_DISCONNECT:
				InapCS2ScfProtocolHelper.dropCall(tcapSession, callData, action);
				break;
			case ACTION_CONTINUE:
				InapCS2ScfProtocolHelper.sendContinueMessage(tcapSession, action);
				break;
			default:
				logger.error("[PH]:: Unsupported Action, drop the call");
				logger.error("[PH]:: INAP Call State:" + lastCallState);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.UNEXP_ACTION);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
				InapCS2ScfProtocolHelper.dropCall(tcapSession, callData);
				break;
			}
		}

		InapCallStates currCallState = (InapCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		/*
		 * replicate if last call state is assist and curr call state is not
		 * terminated/term in progress
		 */
		if (lastCallState == InapCallStates.ASSIST &&
				currCallState != InapCallStates.TERMINATED &&
				currCallState != InapCallStates.TERMINATION_IN_PROGRESS) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Replicate TcapSession");
			}
			tcapSession.replicate();
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit from executeAction with tcapSession and action array");
		}
	}

}
