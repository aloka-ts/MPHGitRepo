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

import com.agnity.mphdata.common.*;

import jain.MandatoryParameterNotSetException;
import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.DialogueIndEvent;
import jain.protocol.ss7.tcap.TcapConstants;
import jain.protocol.ss7.tcap.component.ErrorIndEvent;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.RejectIndEvent;
import jain.protocol.ss7.tcap.component.ResultIndEvent;
import jain.protocol.ss7.tcap.dialogue.EndIndEvent;

import java.util.Date;
import java.util.List;

import javax.servlet.sip.ServletTimer;
import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.log4j.Logger;

import com.agnity.map.operations.MapOpCodes;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.measurement.PhMeasurementService;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhTimerInfo;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.SS7ProtocolHandler;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.sip.SipProtocolConfig;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapListener;
import com.genband.tcap.provider.TcapProvider;
import com.genband.tcap.provider.TcapSession;

/**
 * This class is used to handle MAP Protocol Specific events. this handler class
 * is invoked by ProtocolHandler servlet for processing of dialog and components
 * events This class processes these events and delegates the processing of the
 * messages to MAP Protocol Helper class
 */

public class MapScfProtocolHandler implements SS7ProtocolHandler {

	private static final MapScfProtocolHandler INSTANCE = new MapScfProtocolHandler();
	private static Logger logger = Logger
			.getLogger(MapScfProtocolHandler.class);

	private MapScfProtocolHandler() {
	}

	public static MapScfProtocolHandler getInstance() {
		return INSTANCE;
	}

	/**
	 * This method is called by the Protocolrouter to execute a action returned
	 * by the service
	 * 
	 * @param callData
	 *            data object for the call
	 * @param action
	 *            action returned by the application
	 */
	public void executeAction(CallData callData, Action action)
			throws Exception {
		
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] executeAction() Enter with callData and action ");
		}

		TcapSession tcapSession = null;
		int dialogueId = -1;
		// case of outgoing call 
		if(callData.get(CallDataAttribute.P_DIALOG_ID) == null){
			tcapSession = createNewTcapSession(callData);
			dialogueId = tcapSession.getDialogueId();
			
			if(logger.isDebugEnabled()){
				logger.debug("MAP executeAction: outgoing dialogue: dlgId:"+ dialogueId + 
						", Tcapsession:" + tcapSession);
			}
		}else{
			dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
			tcapSession = PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
					.getTcapProvider().getTcapSession(dialogueId);
			
			if(logger.isDebugEnabled()){
				logger.debug("MAP executeAction: incoming dialogue response: dlgId:"+ dialogueId + 
						", Tcapsession:" + tcapSession);
			}
		}

	
		CallTraceService cts = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
				.getCallTraceService();

		traceMessage(callData, tcapSession);

		if (action == null) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId+ ":: [PH] Do nothing as action array is null");
			}
			return;
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] Action to be performed is "
					+ action.getActionType());
		}
    //    callData.set(CallDataAttribute.P_ORIG_LEG_CALL_ID, dialogueId);
//		callData.set(
//				CallDataAttribute.P_ORIG_LEG_CALL_ID,
//				String.valueOf(dialogueId));
		callData.set(CallDataAttribute.P_LAST_CALL_ACTION,action);
		
		switch (action.getActionType()) {
		case ACTION_CONNECT:{
			MapScfProtocolHelper.sendRequestResult(tcapSession,
					callData, action, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getCallTraceService());
		}
		break;
		case ACTION_INTERROGATE: {
			
			MapScfProtocolHelper.sendAnyTimeInterrogation(tcapSession,
					callData, action, cts);
			break;
		}
		case ACTION_INTERROGATE_SUBS: {
			MapScfProtocolHelper.sendAnyTimeSubsInterrogation(tcapSession,
					callData, action, cts);
		}
		break;
		case ACTION_MODIFY_INFO: {
			MapScfProtocolHelper.sendAnyTimeModification(tcapSession, callData,
					action, cts);
			break;
		}
		case ACTION_SEND_ROUTING_INFO: {
			MapScfProtocolHelper.sendRoutingInformation(tcapSession, callData,
					action, cts);
			break;
		}
		case ACTION_STOP_TIMER: {
			MapScfProtocolUtil.stopTimer(tcapSession, action.getTimerName());
			break;
		}
		default: {
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
	 * validation call the processBegin method of MAPSmHelper to get next
	 * action to be executed. For Invalid dialoge BEGIN, drop call.
	 * <li>For primitive type PRIMITIVE_CONTINUE validate the FSM state and call
	 * processContinue method of MAPSmHelper to get next action to be executed.
	 * For Invalid dialoge CONTINUE, drop call.
	 * <li>For primitive type PRIMITIVE_END validate the FSM state and call
	 * processEnd method of MAPSmHelper to get next action to be executed.For
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
			callData = MapScfProtocolUtil.getCallData(tcapSession);

			if (callData != null) {
				MapScfProtocolHelper.traceDialog(dialogueIndEvent, callData);

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
					legData = new LegData();
					callData.set(CallDataAttribute.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(
							CallDataAttribute.P_CALL_START_TIME, new Date());
					callData.set(CallDataAttribute.P_LEG1,
							legData);
					callData.set(CallDataAttribute.P_PROTOCOL,
							Protocol.MAP_SCF);
					callData.set(CallDataAttribute.P_DIALOG_ID,
							dialogueId);
					callData.set(
							CallDataAttribute.P_ORIG_LEG_CALL_ID,
							String.valueOf(dialogueId));
					callData.set(
							CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE,
							dialogueIndEvent.getPrimitiveType());

					SipApplicationSession appSession = MapScfProtocolUtil
							.getAppSession(tcapSession);

					appSession.setAttribute(PhConstants.SERVICE_ID, serviceHandler.getApplicationName());
					callData.set(
							CallDataAttribute.P_APP_SESSION_ID,
							appSession.getId());

					/*
					 * Store the tcap session reference in appSession as in
					 * timeout it would be required
					 */
					appSession.setAttribute(PhConstants.TCAP_SESSION_ID,
							tcapSession.getDialogueId());

					legData.set(
							LegDataAttributes.P_LEG_MAP_STATE,
							MapCallStates.INIT);
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
				 * validating message
				 */
				boolean isValidEvent = MapScfProtocolFSMHandler
						.validateFSMState(MapScfProtocolEvent.BEGIN,
								tcapSession);
				if (isValidEvent) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ ":: Valid BEGIN start processing");
					}
					Boolean dumpCounters = Boolean.valueOf(SipProtocolConfig.getConfigData(SipProtocolConfig.DUMP_COUNTERS));
					if(dumpCounters) {
						PhMeasurementService.getInstance().getMeasurementCounter(Protocol.MAP_SCF).
						incrementServiceTriggeredCount(serviceHandler.getApplicationName(), false);

					}
					MapScfProtocolHelper.processBegin(dialogueIndEvent,
							tcapSession);

				} else {
					logger.error(dialogueId
							+ ":: Invalid dialoge BEGIN..");
				}
				break;
			}
			case TcapConstants.PRIMITIVE_END: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ ":: Primitive Type is PRIMITIVE_END");
				}

				boolean isValidEvent = MapScfProtocolFSMHandler
						.validateFSMState(MapScfProtocolEvent.END, tcapSession);
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

					/*
					 * drop call if no component is present
					 */
					if (!(endIndEvent.isComponentsPresent())) {
						logger.warn(dialogueId
								+ ":: END with no component, ");

						executeEvent(EventType.EVENT_FAILURE, callData, serviceHandler,  
								MapScfRelReasonCode.END_RCVD_WITHOUT_COMP, 41);
					}
				} else {
					logger.error(dialogueId
							+ ":: Invalid dialoge END");

					executeEvent(EventType.EVENT_FAILURE, callData, serviceHandler,
							MapScfRelReasonCode.UNEXPECTED_END, 41);
				}
				break;
			}
			case TcapConstants.PRIMITIVE_USER_ABORT: {

				if (logger.isInfoEnabled()) {
					logger.info(dialogueId + ":: Handling U-ABORT");
				}
				executeEvent(EventType.EVENT_FAILURE, callData, serviceHandler,
						MapScfRelReasonCode.UABORT_RCVD, 31);

				/*
				 * clean the call ..
				 */
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::Cleaning the call");
				}
				break;

			}
			case TcapConstants.PRIMITIVE_PROVIDER_ABORT:
			case TcapConstants.PRIMITIVE_NOTICE: {
				if (logger.isInfoEnabled()) {
					logger.info(dialogueId + ":: Handling P-ABORT/NOTICE");
				}


				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Cleaning the call");
				}

				executeEvent(EventType.EVENT_FAILURE, callData, serviceHandler,MapScfRelReasonCode.PABORT_NOTICE_RCVD, 41);
				break;
			}
			default: {
				logger.error(dialogueId + ":: unrecognized Primitive Type "
						+ dialogueIndEvent.getPrimitiveType());

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						MapScfRelReasonCode.UNKNOWN_DIALOG_RCVD);
				executeEvent(EventType.EVENT_FAILURE, callData, serviceHandler,MapScfRelReasonCode.UNKNOWN_DIALOG_RCVD, 41);
				break;
			}
			}

		} catch (MandatoryParameterNotSetException e) {
			logger.error(
					dialogueId
					+ ":: MandatoryParameterNotSetException for dialogue event",
					e);

			executeEvent(EventType.EVENT_FAILURE, callData, serviceHandler,
					MapScfRelReasonCode.DIAGLOG_MANDATORY_PARAM_MIS, 41);
		} catch (Exception ex) {
			logger.error(dialogueId
					+ ":: Failed to process Dialogue Indication event", ex);

			executeEvent(EventType.EVENT_FAILURE, callData, serviceHandler,MapScfRelReasonCode.EXCEP_IN_DIALOG_IND, 41);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processDialogueIndEvent() Exit");
		}
	}

	/**
	 * Tcaplistener API implemented by the service for receiving a callback on a
	 * component indication event occuring on the node.This method check the
	 * primitive type for the component indication event. For Primitive Type
	 * PRIMITIVE_INVOKE, PH checks for the operation code.
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
			if (logger.isInfoEnabled()) {
				logger.info(dialogueId
						+ ":: Processing Component Indication Event "
						+ componentIndEvent.getPrimitiveType());
			}

			tcapSession = PhUtilityServices.getInstance(serviceInterface.getApplicationName()).getTcapProvider()
					.getTcapSession(dialogueId);

			CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
			LegData legData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);

			int lastRxDialoguePrimitiveType = -1;

			if (callData != null) {

				lastRxDialoguePrimitiveType = (Integer) callData
						.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
				legData.set(
						LegDataAttributes.P_LAST_RX_INVOKE_ID,
						componentIndEvent.getInvokeId());
				MapScfProtocolHelper
				.traceComponent(componentIndEvent, callData);
			}

			switch (componentIndEvent.getPrimitiveType()) {
			case TcapConstants.PRIMITIVE_INVOKE: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ ":: Primitive Type is PRIMITIVE_INVOKE");
				}

				InvokeIndEvent invokeIndEvent = (InvokeIndEvent) componentIndEvent;
				byte[] operCode = invokeIndEvent.getOperation()
						.getOperationCode();
				byte operCodeByte = operCode[0];
				String operCodeStr = CommonUtils.formatBytes(operCode);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Operation Code is "
							+ operCodeStr);
				}

				switch (operCodeByte) {
				case MapOpCodes.MAP_SS_INVOCATION_NOTIFICATION_BYTE:{
					HandleMapSSInvocationNotification(componentIndEvent,
							serviceInterface,dialogueId,callData,legData,tcapSession,invokeIndEvent);
					break;
				}
				case MapOpCodes.MAP_NOTE_SUBSCRIBER_DATA_MODIFIED_BYTE: {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Process NDSM");
					}

					/*
					 * Drop the call, if service failed to read configuration
					 */
					if (!PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).isServiceInitialized()) {
						logger.error(dialogueId
								+ ":: Service failed to read configuration.. Restart SAS to solve the issue");
						callData.set(
								CallDataAttribute.NP_RELEASE_REASON_CODE,
								MapScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
						callData.set(
								CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
						callData.set(
								CallDataAttribute.NP_FAILED_CALL_IND, 1);
						//MapScfProtocolHelper.dropCall(tcapSession, callData);
						return;
					}

					/*
					 * validate
					 */
					boolean isValid = MapScfProtocolFSMHandler
							.validateFSMState(MapScfProtocolEvent.NSDM,
									tcapSession);
					if (isValid) {

						/*
						 * process if valid
						 */
						if (logger.isDebugEnabled()) {
							logger.debug(dialogueId
									+ ":: Valid Notify SubsData modified event start processing");
						}

						Action[] action = MapScfProtocolHelper
								.processNotifySubsDataModified(invokeIndEvent,
										tcapSession, PhUtilityServices
										.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
										.getCallTraceService());
						if (logger.isDebugEnabled()) {
							logger.debug("[PH] :: processNotifySubsDataModified Exit ");
						}

						/*
						 * special case of IDP for tracing as tracing is decided
						 * after processing IDP
						 */
						MapScfProtocolHelper.traceComponent(componentIndEvent,
								callData);

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
								setServiceInterfaceNotFound(callData);
								//								MapScfProtocolHelper.dropCall(tcapSession,
								//										callData);
								return;
							}

							executeEvent(EventType.EVENT_INITIAL, callData, serviceInterface, -1, -1);

						}

					} else {
						logger.error(dialogueId
								+ ":: Invalid Notify sub Data modified");

						executeEvent(EventType.EVENT_FAILURE, callData, serviceInterface, MapScfRelReasonCode.UNEXPECTED_NSDM, -1);
					}

					break;
				}
				}
			}
			break;
			case TcapConstants.PRIMITIVE_RESULT: {
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ ":: Primitive Type is PRIMITIVE_RESULT");
				}

				ResultIndEvent resultIndEvent = (ResultIndEvent) componentIndEvent;
				Action[] action = MapScfProtocolHelper.processResult(
						resultIndEvent, tcapSession);
				/*
				 * execute drop action with force false;
				 */
				//	executeAction(tcapSession, action);

				if (serviceInterface == null) {
					logger.error(dialogueId
							+ ":: Failed to find service type. ");
					setServiceInterfaceNotFound(callData);
					return;
				}

				if (action == null) {

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ " :: Notify service of event success");
					}
					executeEvent(EventType.EVENT_SUCCESS, callData,
							serviceInterface, -1, -1);

				} else {

					executeEvent(EventType.EVENT_FAILURE, callData,
							serviceInterface,
							MapScfRelReasonCode.UNKNOWN_RESULT, -1);
				}
				MapScfProtocolUtil.writeServiceCdr(tcapSession);
				break;
			}
			case TcapConstants.PRIMITIVE_ERROR: {
				logger.warn(dialogueId + ":: PRIMITIVE_ERROR is received");

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						MapScfRelReasonCode.PRIM_ERROR_RCVD);

				ErrorIndEvent errorIndEvent = (ErrorIndEvent) componentIndEvent;
				Action[] action = MapScfProtocolHelper.processError(
						errorIndEvent, tcapSession);

				if(action != null){
					executeAction(tcapSession, action);
				}
				MapScfProtocolUtil.writeServiceCdr(tcapSession);
				break;
			}

			case TcapConstants.PRIMITIVE_REJECT: {
				logger.warn(dialogueId + ":: PRIMITIVE_REJECT is received");

				callData.set(
						CallDataAttribute.NP_RELEASE_REASON_CODE,
						MapScfRelReasonCode.PRIM_REJECT_RCVD);

				RejectIndEvent rejectIndEvent = (RejectIndEvent) componentIndEvent;
				Action[] action = MapScfProtocolHelper.processReject(
						rejectIndEvent, tcapSession);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId
							+ " :: Notify service of event success");
				}
				/*
				 * Drop the call, if failed to determine service type. This case
				 * will happen only if some DB error happens while determining
				 * service type.
				 */
				if (serviceInterface == null) {
					logger.error(dialogueId
							+ ":: Failed to find service type. ");
					setServiceInterfaceNotFound(callData);
					return;
				}
				executeEvent(EventType.EVENT_FAILURE, callData, serviceInterface, MapScfRelReasonCode.PRIM_REJECT_RCVD, -1);

				break;
			}

			}

		} catch (MandatoryParameterNotSetException e) {
			logger.error(
					dialogueId
					+ ":: MandatoryParameterNotSetException fetching component event",
					e);
			CallData newCallData = MapScfProtocolUtil.getCallData(tcapSession);
			if (newCallData != null) {

				if (serviceInterface == null) {
					logger.error(dialogueId
							+ ":: Failed to find service type. ");
					setServiceInterfaceNotFound(newCallData);
					return;
				}

				executeEvent(EventType.EVENT_FAILURE, newCallData, serviceInterface, MapScfRelReasonCode.COMP_MANDATORY_PARAM_MIS, 41);
			}
		} catch (Exception ex) {
			logger.error(dialogueId
					+ ":: Failed to process Component Indication event", ex);
			CallData newCallData = MapScfProtocolUtil.getCallData(tcapSession);

			if (newCallData != null) {

				if (serviceInterface == null) {
					logger.error(dialogueId
							+ ":: Failed to find service type. ");
					setServiceInterfaceNotFound(newCallData);
					return;
				}

				executeEvent(EventType.EVENT_FAILURE, newCallData, serviceInterface, MapScfRelReasonCode.EXCEP_IN_COMP_IND, 41);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH] processComponentIndEvent() Exit");
		}
	}



	private void HandleMapSSInvocationNotification(
			ComponentIndEvent componentIndEvent,
			ServiceInterface serviceInterface,
			int dialogueId,
			CallData callData,
			LegData legData,
			TcapSession tcapSession,
			InvokeIndEvent invokeIndEvent) throws Exception
	{
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Process MapSSInvocationNotification");
		}

		/*
		 * Drop the call, if service failed to read configuration
		 */
		if (!PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).isServiceInitialized()) {
			logger.error(dialogueId
					+ ":: Service failed to read configuration.. Restart SAS to solve the issue");
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE,
					MapScfRelReasonCode.FAILED_TO_INITIALIZE_SRV);
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			callData.set(
					CallDataAttribute.NP_FAILED_CALL_IND, 1);
			//MapScfProtocolHelper.dropCall(tcapSession, callData);
			return;
		}

		/*
		 * validate
		 */
		boolean isValid = MapScfProtocolFSMHandler.validateFSMState(MapScfProtocolEvent.SSIN,tcapSession);
		if (isValid) {
			/*
			 * process if valid
			 */
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId
						+ ":: Valid SSInvocationNotification event start processing");
			}

			Action[] action = MapScfProtocolHelper.
					processSSInvocationNotification(invokeIndEvent,
							tcapSession, PhUtilityServices
							.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
							.getCallTraceService());
			if (logger.isDebugEnabled()) {
				logger.debug("[PH] :: processSSInvocationNotification Exit ");
			}

			/*
			 * special case of IDP for tracing as tracing is decided
			 * after processing IDP
			 */
			MapScfProtocolHelper.traceComponent(componentIndEvent,
					callData);

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
					setServiceInterfaceNotFound(callData);
					//								MapScfProtocolHelper.dropCall(tcapSession,
					//										callData);
					return;
				}

				executeEvent(EventType.EVENT_INITIAL, callData, serviceInterface, -1, -1);

			}

		} else {
			logger.error(dialogueId
					+ ":: Invalid Notify sub Data modified");

			executeEvent(EventType.EVENT_FAILURE, callData, serviceInterface, MapScfRelReasonCode.UNEXPECTED_NSDM, -1);
		}



	}
	/**
	 * This method is used to release reason and parametere in case service interface not found
	 * @param callData
	 */
	private void setServiceInterfaceNotFound(CallData callData){

		callData.set(
				CallDataAttribute.NP_RELEASE_REASON_CODE,
				MapScfRelReasonCode.SERVICE_TYPE_NOT_FOUND);
		callData.set(
				CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);

	}

	/**
	 * This method is used to raise success and failure events on service interface
	 * @param eventType
	 * @param callData
	 * @param serviceIf
	 * @param reasonCode
	 * @param reasonVal
	 */
	private void executeEvent(EventType eventType, CallData callData,
			ServiceInterface serviceIf, int reasonCode, int reasonVal) {

		if (reasonCode != -1) {
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_CODE, reasonCode);
		}
		if (reasonVal != -1) {
			callData.set(
					CallDataAttribute.NP_RELEASE_REASON_VALUE, reasonVal);
		}
		Event event = new Event(eventType, Protocol.MAP_SCF, null);
		try {
			ProtocolRouter.getInstance().execute(event, callData, serviceIf);
		} catch (Exception e1) {

			e1.printStackTrace();
		}

	}
	/**
	 * This method is used to execute the action returned by MAP PH
	 * 
	 * @param tcapSession
	 * @param actionArr
	 * @throws Exception
	 */
	private void executeAction(TcapSession tcapSession, Action[] actionArr)
			throws Exception {
		CallData callData = MapScfProtocolUtil.getCallData(tcapSession);
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

		int callState = CallTraceService.CALL_IN_PROGRESS;

		/*
		 * moved tracing in begin as it should be done before message is sent.
		 * and used call in progress state as term message will use terminated
		 * state.
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
		Object lastCallState = legData
				.get(LegDataAttributes.P_LEG_MAP_STATE);

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
			case ACTION_INTERROGATE:
				MapScfProtocolHelper.sendAnyTimeInterrogation(tcapSession,
						callData, action, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService());
				break;
			case ACTION_INTERROGATE_SUBS:
				MapScfProtocolHelper.sendAnyTimeSubsInterrogation(tcapSession,
						callData, action, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService());
				break;
			case ACTION_MODIFY_INFO:
				MapScfProtocolHelper.sendAnyTimeModification(tcapSession,
						callData, action, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService());
				break;
			case ACTION_SEND_ROUTING_INFO:
				MapScfProtocolHelper.sendRoutingInformation(tcapSession,
						callData, action, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService());
				break;
			case ACTION_REJECT_REQUEST:
				MapScfProtocolHelper.sendRejectRequest(tcapSession,
						callData, action, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService());
				break;
			case ACTION_CONNECT:
				MapScfProtocolHelper.sendRequestResult(tcapSession,
						callData, action, PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
						.getCallTraceService());
				break;
			case ACTION_STOP_TIMER:
				MapScfProtocolUtil.stopTimer(tcapSession, action.getTimerName());
				break;
			default:
				logger.error(dialogId + ":: Unsupported Action, drop the call");
				logger.error("MAP Call State:" + lastCallState);

				break;
			}
		}

	}

	/**
	 * This method id used to handle timers created by MAP PH
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
			logger.info(dialogId + ":: Received timeout for MAP Timer");
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
			callData = MapScfProtocolUtil.getCallData(appSession);

			dialogId = (Integer) callData
					.get(CallDataAttribute.P_DIALOG_ID);

			TcapSession tcapSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID))
					.getTcapProvider().getTcapSession(dialogId);

			PhTimerInfo phTimerInfo = (PhTimerInfo) timer.getInfo();
			String timerName = phTimerInfo.getTimerName();

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

			if (logger.isInfoEnabled()) {
				logger.info(dialogId
						+ ":: Map SS7 call state of orig is  "
						+ origLegData
						.get(LegDataAttributes.P_LEG_MAP_STATE));
			}

			if (timerName.equals(PhConstants.CDR_TIMER)) {
				boolean timerProcessingRequired = MapScfProtocolFSMHandler
						.validateFSMState(MapScfProtocolEvent.CDR_TIMEOUT,
								tcapSession);
				if (timerProcessingRequired) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Handle timeout ");
					}

					// CommonUtils.incrementIntermediateCdr(callData);

					/*
					 * Set App-session timeout to 24 hours and 5 minute
					 */
					CommonUtils
					.setAppSessionTimeout(appSession, 1445, dialogId);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogId + ":: Write intermediate CDR.");
					}
					MapScfProtocolUtil.writeServiceCdr(tcapSession);

					/*
					 * this is a special case replicating tcapsesison as it will
					 * be required for CDR params
					 */
					tcapSession.replicate();

				} else {
					logger.error(dialogId
							+ ":: Do nothing as timeout received in invalid call state");
					logger.error(
							""
									+ origLegData
									.get(LegDataAttributes.P_LEG_MAP_STATE));
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

	@SuppressWarnings("unchecked")
	private void traceMessage(CallData callData, TcapSession tcapSession) {

		if (logger.isDebugEnabled()) {
			logger.debug("Inside MAP traceMessage() for service...");
		}

		String traceFlag = (String) callData
				.get(CallDataAttribute.P_TRACE_FLAG);
		int dialogueId = tcapSession.getDialogueId();
		int callState = CallTraceService.CALL_IN_PROGRESS;

		if (PhConstants.TRUE.equals(traceFlag)) {

			StringBuilder traceMsg = (StringBuilder) callData
					.get(CallDataAttribute.P_TRACE_MESSAGE);

			if (traceMsg.length() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug(":: Trace message is " + traceMsg);
				}

				traceMsg.append(PhConstants.TRACE_MESSAGE_END_FOOTER);

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

		if (logger.isDebugEnabled()) {
			logger.debug("Exitting MAP  traceMessage()...");
		}
	}

	/**
	 * Method used for creating new Tcapsession for outgoing dialogue
	 * @param callData
	 * @return
	 */
	private TcapSession createNewTcapSession(CallData callData){
		TcapSession tcapSession = null;
		int dialogId = -1;
		try{
			TcapListener tcapListener = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapListener();

			// need to confirm from reeta
			SipApplicationSession appSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getSipFactory().createApplicationSession();

			tcapSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).
					getTcapFactory().createTcapSession(tcapListener, appSession);

			//set the calldata in tcapsession
			tcapSession.setAttribute(CallData.CALL_DATA, callData);

			// obtain the dialog from newly created tcapSession
			dialogId = tcapSession.getDialogueId();

			// set the tcap session reference in appsession 
			appSession.setAttribute(PhConstants.TCAP_SESSION_ID, dialogId);
		}catch(Exception ex){
			logger.error("Error creating new tcap Session for MAP protocol" + ex);
		}

		if(logger.isDebugEnabled()) {
			logger.debug("Map - generated new tcapsesion with a new Dialog ["+dialogId+"]");
		}
		return tcapSession;
	}
}

