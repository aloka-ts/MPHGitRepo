package com.agnity.ph.inapcs1scf.flowhelper;

import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.inapcs1scf.*;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.tcap.component.InvokeReqEvent;
import jain.protocol.ss7.tcap.component.Operation;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.src;

/**
 * Created by ankitsinghal on 24/10/16.
 */
public class InapCS1ATHelper {
	private static Logger logger = Logger.getLogger(InapCS1ATHelper.class);

	public static void dropCallOnATTimeout(CallData callData) throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		TcapSession tcapSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider().getTcapSession(dialogId);
		boolean timerProcessingRequired = InapCS1ScfProtocolFSMHandler.validateFSMState(InapCS1ScfProtocolEvent.AT_TIMEOUT, tcapSession);

		if (timerProcessingRequired) {
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, 41);
			callData.set(CallDataAttribute.NP_FAILED_CALL_IND, 1);

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Set failed call indicator to 1");
				logger.debug("[PH]:: Notify service that call is dropped");
				logger.debug("[PH]:: Write CDR");
			}

			callData.set(CallDataAttribute.P_DFC_REQUIRED_FLAG, PhConstants.FALSE);

			// mark call state to terminated to avoid other action
			InapCS1ScfProtocolHelper.preProcessDroppedCall(tcapSession);

			logger.error("[PH]:: AT timedout, send U-Abort");

			callData.set(CallDataAttribute.NP_REASON_FOR_RELEASE, InapCS1ScfRelReasonCode.ACT_TEST_TIMEOUT);

			Action action = new Action(Action.ActionType.ACTION_END_CALL);
			action.setDropCallMode(Action.DROP_CALL_MODE.USER_ABORT);
			action.setReleaseCauseValue(InapCS1ScfProtocolFSMHandler.AbortInfoEnum.ABNORMAL_PROCESSING.getCode());

			InapCS1ScfProtocolHelper.sendDropMessage(tcapSession, action);

			//notify service and write CDRs
			InapCS1ScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
		}
	}

	/**
	 * This method is used to Cancel the Activity Test Acknowledgment Timer.
	 *
	 * @param tcapSession represents the instance of TcapSession
	 */
	public static void cancelATTimer(TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside cancelATTimer");
		}
		InapCS1ScfProtocolUtil.stopTimer(tcapSession, PhConstants.AT_ACK_TIMER);

		// start Activity timer 
		String toStartATAgain = InapCS1ScfProtocolConfig.getConfigData(InapCS1ScfProtocolConfig.SEND_AT_PERIODICALLY);

		if(StringUtils.isNotBlank(toStartATAgain) && 
				StringUtils.equalsIgnoreCase(toStartATAgain, PhConstants.TRUE)){
			CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);
			int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
			
			if(logger.isDebugEnabled()){
				logger.debug("AT: AT ACK Timer Stopped, starting AT Timer again");
			}
			InapCS1ScfProtocolUtil.startActivityTestTimer(dialogueId, callData, tcapSession);
		} else {
			if(logger.isDebugEnabled()){
				logger.debug("AT: Not starting Activity Timer again as SEND_AT_PERIODICALLY is set as " + toStartATAgain);
			}
		}

	}

	/**
	 * This method is used to send AT
	 */
	public static void callHeartBeat(TcapSession tcapSession, CallData callData) throws Exception {
		sendActivityTestForHB(tcapSession);
		InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);
		/*
		 * Start activity test timer . call will be dropped if timer gets
		 * timeout before getting response from SCCP
		 */
		InapCS1ScfProtocolUtil.startTimer(tcapSession,
				InapCS1ScfProtocolUtil.getActivityTestTime(callData),
				true,
				PhConstants.AT_ACK_TIMER);
	}

	private static void sendActivityTestForHB(TcapSession tcapSession) throws Exception {
		CallData callData = InapCS1ScfProtocolUtil.getCallData(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendActivityTestForHB");
		}

		byte[] atOpCode = {InapOpCodes.ACTIVITY_TEST_BYTE};

		Operation atOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, atOpCode);

		InvokeReqEvent atInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), atOperation);
		atInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
		callData.set(CallDataAttribute.P_AT_INVOKE_ID, atInvokeReqEvent.getInvokeId());
		atInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		InapCS1ScfProtocolHelper.sendComponentReq(atInvokeReqEvent, callData);
	}
}
