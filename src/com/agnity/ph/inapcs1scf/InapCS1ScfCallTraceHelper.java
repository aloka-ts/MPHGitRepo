package com.agnity.ph.inapcs1scf;

import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.inapitutcs2.util.Util;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.tcap.*;
import jain.protocol.ss7.tcap.component.*;
import jain.protocol.ss7.tcap.dialogue.*;

import org.apache.log4j.Logger;

import java.util.Iterator;
import java.util.List;

/**
 * Created by ankitsinghal on 06/10/16.
 */
public class InapCS1ScfCallTraceHelper {

	private static Logger logger = Logger.getLogger(InapCS1ScfCallTraceHelper.class);

	@SuppressWarnings("unchecked")
	public static void traceMessage(CallData callData, TcapSession tcapSession) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside INAP traceMessage() for service...");
		}
		String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);
		
		// check for tcapsession. If not nul then continue
		if(tcapSession == null){
			return;
		}
		
		int dialogueId = tcapSession.getDialogueId();
		int callState = CallTraceService.CALL_IN_PROGRESS;

		if (PhConstants.TRUE.equals(traceFlag)) {

			StringBuilder traceMsg = (StringBuilder) callData.get(CallDataAttribute.P_TRACE_MESSAGE);

			if (traceMsg.length() > 0) {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Trace message is " + traceMsg);
				}

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					for (int constraint : constraintList) {
						PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService().trace(constraint, InapCS1ScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(), callState);
					}
				}
				traceMsg.delete(0, traceMsg.length());
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exitting INAP traceMessage()...");
		}
	}

	/**
	 * This method will trace incoming INAP Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error;
	 *
	 * @param die      represents the instance of DialogueIndEvent
	 * @param callData represents the instance of CallData
	 */
	public static void traceDialog(DialogueIndEvent die, CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside traceDialog");
		}

		boolean dumpTrace = false;
		CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		try {
			String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);

			if (traceFlag.equals(PhConstants.TRUE)) {
				int primitive = die.getPrimitiveType();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				StringBuilder traceMsg = new StringBuilder();

				traceMsg.append(PhConstants.incomingMsgHdr);
				traceMsg.append("DIALOGUE Received: ");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_BEGIN: {
					traceMsg.append("BEGIN \n");
					break;
				}
				case TcapConstants.PRIMITIVE_CONTINUE: {
					traceMsg.append("CONTINUE \n");
					break;
				}
				case TcapConstants.PRIMITIVE_END: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END \n");
					break;
				}
				case TcapConstants.PRIMITIVE_END_PRE_ARRANGED: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("END \n");
					traceMsg.append("Pre-Arranged:: true \n");
					break;
				}
				case TcapConstants.PRIMITIVE_PROVIDER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("P-ABORT \n");

					traceMsg.append("Information::");
					traceMsg.append(((ProviderAbortIndEvent) die).getPAbort() + "\n");
					traceMsg.append(PhConstants.traceDelim);
					dumpTrace = true;
					break;
				}
				case TcapConstants.PRIMITIVE_USER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("U-ABORT \n");

					traceMsg.append("Information::");
					traceMsg.append(Util.formatBytes(((UserAbortIndEvent) die).getUserAbortInformation()) + "\n");

					try {
						int abortReason = ((UserAbortIndEvent) die).getAbortReason();
						traceMsg.append("Generic Tcap Reason:: " + abortReason + "\n");
					} catch(Exception ex){
						logger.warn("Error in getting abort reason in call trace");
					}
					traceMsg.append(PhConstants.traceDelim);
					dumpTrace = true;

					break;
				}
				case TcapConstants.PRIMITIVE_NOTICE: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("NOTICE \n");
					traceMsg.append(PhConstants.traceDelim);
					dumpTrace = true;
					break;
				}
				case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
					traceMsg.append("UNIDIRECTIONAL \n");
					break;
				}
				default: {
					traceMsg.append("CODE:" + primitive + " \n");
					dumpTrace = true;
					break;
				}
				}

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null && dumpTrace) {
					@SuppressWarnings("unchecked") List<Integer> constraintList = (List<Integer>) callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(), InapCS1ScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(), callState);
					}
				}

			} // end if tracing required
		} catch (Throwable t) {
			logger.warn("[PH]:: Error tracing Dialog ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("[PH]:: Error tracing dialogue ind event", t);
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit traceDialog");
		}
	}

	/**
	 * This method will trace outgoing INAP component; We catch inside try catch
	 * throwable to avoid any impact on call in case of error;
	 *
	 * @param cre      represents the instance of ComponentReqEvent
	 * @param callData represents the instance of CallData
	 */

	public static void traceComponent(ComponentReqEvent cre, CallData callData) {
		try {
			CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			String isTraceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);

			if (PhConstants.TRUE.equals(isTraceFlag)) {
				
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
				int primitive = cre.getPrimitiveType();

				StringBuilder traceMsg = new StringBuilder();

				traceMsg.append(PhConstants.outgoingMsgHdr);

				int callState = CallTraceService.CALL_IN_PROGRESS;

				LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
				LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

				PhoneNumber callingParty = null;
				if(leg1Data.get(LegDataAttributes.P_CALLING_PARTY) != null){
					callingParty = (PhoneNumber) leg1Data.get(LegDataAttributes.P_CALLING_PARTY);
				}

				PhoneNumber calledParty = (PhoneNumber) leg1Data.get(LegDataAttributes.P_CALLED_PARTY);

				PhoneNumber destinitionNumber = null;
				if(leg2Data != null && leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER) != null){
					destinitionNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);
				}
					
				switch (primitive) {
				case TcapConstants.PRIMITIVE_INVOKE: {

					traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cre.getInvokeId() + " ComponentType: INVOKE" + "\n");
					traceMsg.append(PhConstants.inapOperation);

					byte[] operCode = ((InvokeReqEvent) cre).getOperation().getOperationCode();
					byte operCodeByte = operCode[0];

					switch (operCodeByte) {
					case InapOpCodes.IDP_BYTE: {
						traceMsg.append("InitialDP" + "\n");
						if(callingParty != null){
							traceMsg.append("Calling Party Info:  Digits - [" + callingParty.getAddress() + "] Nature Of Number - [" + callingParty.getNatureOfAddress() + "]").append("\n");
						}else{
							traceMsg.append("Calling Party Info: Address Not Present").append("\n");
						}
						traceMsg.append("Called Party Info:  Digits - [" + calledParty.getAddress() + "] Nature Of Number - [" + calledParty.getNatureOfAddress() + "]").append("\n");
						break;
					}
					case InapOpCodes.ENC_BYTE: {
						traceMsg.append("EventNotificationCharging" + "\n");
						break;
					}
					case InapOpCodes.ERB_BYTE: {
						traceMsg.append("EventReportBCSM" + "\n");
						break;
					}
					case InapOpCodes.SCI_BYTE: {
						traceMsg.append("SpecificChargingInformation" + "\n");
						break;
					}
					case InapOpCodes.RRBE_BYTE: {
						traceMsg.append("RequestReportBCSM" + "\n");
						break;
					}
					case InapOpCodes.RNCE_BYTE: {
						traceMsg.append("RequestNotificationChargingEvent" + "\n");
						break;
					}
					case InapOpCodes.CONNECT_BYTE: {
						traceMsg.append("Connect" + "\n");
						if(callingParty != null){
							traceMsg.append("Calling Party Info:  Digits - [" + callingParty.getAddress() + "] Nature Of Number - [" + callingParty.getNatureOfAddress() + "]").append("\n");
						}else{
							traceMsg.append("Calling Party Info: Address Not Present").append("\n");
						}
						if(destinitionNumber != null){
							traceMsg.append("Called Party Info:  Digits - [" + destinitionNumber.getAddress() + "] Nature Of Number - [" + destinitionNumber.getNatureOfAddress() + "]").append("\n");
						}else{
							traceMsg.append("Called Party Info: Address Not Present").append("\n");
						}

						break;
					}
					case InapOpCodes.ETC_BYTE: {
						traceMsg.append("EstablishTemporaryConnection" + "\n");
						traceMsg.append("PRI Number: " + callData.get(CallDataAttribute.P_CORRELATION_ID).toString() + "\n");

						break;
					}
					case InapOpCodes.DFC_BYTE: {
						traceMsg.append("DisconnectForwardConnection" + "\n");
						break;
					}
					case InapOpCodes.RELEASE_CALL_BYTE: {
						traceMsg.append("ReleaseCall" + "\n");
						break;
					}
					case InapOpCodes.ACTIVITY_TEST_BYTE: {
						traceMsg.append("ActivityTest" + "\n");
						break;
					}
					case InapOpCodes.DFC_WITHOUT_ARGS_BYTE: {
						traceMsg.append("DFC wo Args" + "\n");
						break;
					}
					case InapOpCodes.FCI_BYTE: {
						traceMsg.append("FurnishChargingInfo" + "\n");
						if(callingParty != null){
							traceMsg.append("Calling Party Info:  Digits - [" + callingParty.getAddress() + "] Nature Of Number - [" + callingParty.getNatureOfAddress() + "]").append("\n");
						}else{
							traceMsg.append("Calling Party Info: Address Not Present").append("\n");
						}
						if(destinitionNumber != null){
							traceMsg.append("Called Party Info:  Digits - [" + destinitionNumber.getAddress() + "] Nature Of Number - [" + destinitionNumber.getNatureOfAddress() + "]").append("\n");
						}else{
							traceMsg.append("Called Party Info: Address Not Present").append("\n");
						}

						break; 
					}
					case InapOpCodes.RESET_TIMER_BYTE: {
						traceMsg.append("ResetTimer" + "\n");
						break;
					}
					case InapOpCodes.PA_BYTE: {
						traceMsg.append("PlayAnnouncement" + "\n");
						break;
					}
					case InapOpCodes.PAC_BYTE: {
						traceMsg.append("PromptAndCollectUserInfo" + "\n");
						break;
					}
					case InapOpCodes.CONNECT_TO_RESOURCE_BYTE: {
						traceMsg.append("ConnectToResource" + "\n");
						break;
					}
					default: {
						traceMsg.append("OPCODE BYTE:" + operCodeByte);
						break;
					}
					}

					// Print Component buffer
					if (((InvokeReqEvent) cre).isParametersPresent()) {
						byte[] parms = ((InvokeReqEvent) cre).getParameters().getParameter();
						traceMsg.append("Component Buffer::");
						traceMsg.append(Util.formatBytes(parms));
						traceMsg.append("\n");
					}

					break;
				} // @end switch message type
				case TcapConstants.PRIMITIVE_RESULT: {

					traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cre.getInvokeId() + " ComponentType: RETURN_RESULT" + "\n");

					if (((ResultReqEvent) cre).isParametersPresent()) {
						byte[] parms = ((ResultReqEvent) cre).getParameters().getParameter();
						traceMsg.append("Componen Buffer::");
						traceMsg.append(Util.formatBytes(parms));
						traceMsg.append("\n");
					}
					break;
				}
				case TcapConstants.PRIMITIVE_ERROR: {

					traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cre.getInvokeId() + " ComponentType: ERROR" + "\n");

					try {
						ErrorReqEvent errorReqEvent = (ErrorReqEvent) cre;
						traceMsg.append("ERROR sent for last operation with invokeID:" + errorReqEvent.getInvokeId() + "\n");

						if(errorReqEvent.getErrorCode() != null){
							traceMsg.append("Error Code: " + Util.formatBytes(errorReqEvent.getErrorCode()));
						}
					} catch (Exception ex) {
						logger.warn("Error in extracting Error for trace call");
					}

					break;
				}
				case TcapConstants.PRIMITIVE_REJECT: {

					traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cre.getInvokeId() + " ComponentType: REJECT" + "\n");

					try {
						RejectReqEvent rejectReqEvent = (RejectReqEvent) cre;
						traceMsg.append("REJECT sent for last operation with invokeID:" + rejectReqEvent.getInvokeId() + "\n");
						traceMsg.append(" ProblemType: " + rejectReqEvent.getProblemType() + "Problem:" + rejectReqEvent.getProblem() + "/n");
					} catch (Exception ex) {
						logger.warn("Error in extracting Reject for trace call");
					}
				}
				} // @switch primitive

				traceMsg.append(PhConstants.traceDelim);

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(), InapCS1ScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(), callState,caller,called);
					}
				}

			} // end if tracing required
		} catch (Throwable t) {
			logger.warn("[PH]:: Error tracing component req event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("[PH]:: Error in traceComponent", t);
			}
		}
	}

	/**
	 * This method will trace incoming INAP component; inside try catch
	 * throwable to avoid any impact on call in case of error;
	 *
	 * @param cie      represents the instance of ComponentIndEvent
	 * @param callData represents the instance of CallData
	 */
	static void traceComponent(ComponentIndEvent cie, CallData callData) {
		try {
			CallTraceService cCallTraceService = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
			String isTraceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);

			if (PhConstants.TRUE.equals(isTraceFlag)) {
				int primitive = cie.getPrimitiveType();
				
				
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

				StringBuilder traceMsg = new StringBuilder();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				switch (primitive) {
				case TcapConstants.PRIMITIVE_INVOKE: {

					byte[] operCode = ((InvokeIndEvent) cie).getOperation().getOperationCode();
					byte operCodeByte = operCode[0];

					switch (operCodeByte) {
					case InapOpCodes.IDP_BYTE: {
						/*
						 * special case where we need to trace begin also
						 */
						traceBegin(callData, cCallTraceService, traceMsg);

						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "InitialDP" + "\n");
						break;
					}
					case InapOpCodes.ENC_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");	
						traceMsg.append(PhConstants.inapOperation + "EventNotificationCharging" + "\n");
						break;
					}
					case InapOpCodes.ERB_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "EventReportBCSM" + "\n");
						break;
					}
					case InapOpCodes.SCI_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "SpecificChargingInformation" + "\n");
						break;
					}
					case InapOpCodes.RRBE_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "RequestReportBCSM" + "\n");
						break;
					}
					case InapOpCodes.RNCE_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "RequestNotificationChargingEvent" + "\n");
						break;
					}
					case InapOpCodes.CONNECT_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "Connect" + "\n");
						break;
					}
					case InapOpCodes.ETC_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "EstablishTemporaryConnection" + "\n");
						break;
					}
					case InapOpCodes.DFC_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "DisconnectForwardConnection" + "\n");
						break;
					}
					case InapOpCodes.RELEASE_CALL_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "ReleaseCall" + "\n");
						break;
					}
					case InapOpCodes.ACTIVITY_TEST_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "ActivityTest" + "\n");
						break;
					}
					case InapOpCodes.DFC_WITHOUT_ARGS_BYTE: {
						traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: INVOKE" + "\n");
						traceMsg.append(PhConstants.inapOperation + "DFC wo Args" + "\n");
						break;
					}
					default: {
						traceMsg.append("OPCODE BYTE:" + operCodeByte);
						traceMsg.append("\n");
						break;
					}
					}

					// No need to dump inap buffer. Enough to put decoded messages. 
					// commenting it. Might add it in future based for trace levels
					// Object traceCall = callData.get(CallDataAttribute.P_TRACE_FLAG);
					//					if (traceCall != null && traceCall.equals(PhConstants.TRUE)) {
					//
					//						if (((InvokeIndEvent) cie).isParametersPresent()) {
					//							byte[] parms = ((InvokeIndEvent) cie).getParameters().getParameter();
					//							traceMsg.append("BUFFER::");
					//							traceMsg.append(Util.formatBytes(parms));
					//							traceMsg.append("\n");
					//						}
					//					}

					break;
				} // @end switch message type
				case TcapConstants.PRIMITIVE_RESULT: {
					traceMsg.append(PhConstants.incomingMsgHdr);
					traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: RETURN_RESULT" + "\n");
					ResultIndEvent rsltIndEvt = (ResultIndEvent) cie;

					try {
						if(rsltIndEvt.isOperationPresent()){
							byte [] operCode = rsltIndEvt.getOperation().getOperationCode();
							byte operCodeByte = operCode[0];

							switch(operCodeByte) {
							case InapOpCodes.ACTIVITY_TEST_BYTE:
								traceMsg.append(PhConstants.inapOperation + "ACTIVITY_TEST" + "\n");
								break;
							case InapOpCodes.PAC_BYTE:
								traceMsg.append(PhConstants.inapOperation + "PROMPT_AND_COLLECT" + "\n");
								break;
							default:
								traceMsg.append(PhConstants.inapOperation + CommonUtils.formatBytes(operCode));
								break;

							}
						}else if(rsltIndEvt.isInvokeIdPresent()){
							traceMsg.append("RETURN-RESULT recevied for last operation with invokeID:" + rsltIndEvt.getInvokeId());
						}
					} catch(Exception ex) {
						logger.warn("Error in extracting return result for trace call");
					}

					break;
				}
				case TcapConstants.PRIMITIVE_ERROR: {
					traceMsg.append(PhConstants.incomingMsgHdr);
					traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: ERROR" + "\n");

					try {
						ErrorIndEvent errorIndEvent = (ErrorIndEvent) cie;
						traceMsg.append("ERROR recevied for last operation with invokeID:" + errorIndEvent.getInvokeId() + "\n");

						if(errorIndEvent.getErrorCode() != null){
							traceMsg.append("Error Code: " + CommonUtils.formatBytes(errorIndEvent.getErrorCode()));
						}
					} catch (Exception ex) {
						logger.warn("Error in extracting Error for trace call");
					}

					break;
				}
				case TcapConstants.PRIMITIVE_REJECT: {

					traceMsg.append(PhConstants.incomingMsgHdr);
					traceMsg.append(PhConstants.tcapParams + "Invoke Id:" + cie.getInvokeId() + " ComponentType: REJECT" + "\n");
					RejectIndEvent rejectIndEvent = (RejectIndEvent) cie;

					try {

						traceMsg.append("REJECT recevied for last operation with invokeID:" + rejectIndEvent.getInvokeId() + "\n");
						if(rejectIndEvent.isRejectTypePresent()) {
							traceMsg.append("Reject Type:" + rejectIndEvent.getRejectType());
						}

						traceMsg.append(" ProblemType: " + rejectIndEvent.getProblemType() + "Problem:" + rejectIndEvent.getProblem() + "/n");

					} catch (Exception ex) {
						logger.warn("Error in extracting Reject for trace call");
					}

				}
				} // @switch primitive

				traceMsg.append(PhConstants.traceDelim);

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {

					List<Integer> constraintList = (List<Integer>) callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(), InapCS1ScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(), callState,caller,called);
					}
				}

			} // end if tracing required
		} catch (Throwable t) {
			logger.warn("[PH]:: Error tracing component ind event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("[PH]:: Error in tracing component ind event", t);
			}
		}
	}

	/**
	 * This method will trace incoming BEGIN Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error; invoked
	 * from trace component ind for IDP
	 *
	 * @param callData          represents an instance of CallData
	 * @param cCallTraceService represents an instance of CallData CallTraceService
	 */
	private static void traceBegin(CallData callData, CallTraceService cCallTraceService, StringBuilder traceMsg) {
		try {

			int callState = CallTraceService.CALL_IN_PROGRESS;
			traceMsg.append(PhConstants.traceDelim);
			traceMsg.append(PhConstants.protocolType);
			traceMsg.append("Dialogue ID: " + callData.get(CallDataAttribute.P_ORIG_LEG_CALL_ID) + "\n");

			// get Point code and SSN for this call
			SccpUserAddress cgpnAddr= null;
			SccpUserAddress cdpnAddr=null;

			try {

				if(callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS) != null &&
						((SccpUserAddress)callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS)).isSubSystemAddressPresent()) {

					cdpnAddr = (SccpUserAddress)callData.get(CallDataAttribute.P_SCCP_LOCAL_USER_ADDRESS);			
				}

				if(callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS) != null &&
						((SccpUserAddress)callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS)).isSubSystemAddressPresent()) {

					cgpnAddr = (SccpUserAddress)callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS);

				}

				// SCCP Parameters
				traceMsg.append(PhConstants.sccpParams);

				if(cgpnAddr != null) {
					traceMsg.append(" - Calling Party Addr: " 
							+ cgpnAddr.getSubSystemAddress().getSignalingPointCode() +  "(PC), "
							+ cgpnAddr.getSubSystemAddress().getSubSystemNumber() +"\n");
				}

				if(cdpnAddr != null) {
					traceMsg.append(" - Called Party Addr : " 
							+ cdpnAddr.getSubSystemAddress().getSignalingPointCode() +  "(PC), "
							+ cdpnAddr.getSubSystemAddress().getSubSystemNumber() +"\n");
				}

			} catch(Exception ex) {
				logger.warn("Error is fecthing calling and called party SCCP address");
			}

		} catch (Throwable t) {
			logger.warn("[PH]:: Error tracing Dialog ind event BEGINS" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("[PH]:: Error tracing begin event", t);
			}
		}
	}

	/**
	 * This method will trace outgoing INAP Dialogue; From try block we are
	 * catching throwable to avoid any impact on call in case of error;
	 *
	 * @param dre               represents the instance of DialogueReqEvent
	 * @param callData          represents the instance of CallData
	 * @param cCallTraceService represents the instance of CallTraceService
	 */
	public static void traceDialog(DialogueReqEvent dre, CallData callData, CallTraceService cCallTraceService) {
		try {
			String traceFlag = (String) callData.get(CallDataAttribute.P_TRACE_FLAG);
			if (PhConstants.TRUE.equals(traceFlag)) {

				int primitive = dre.getPrimitiveType();

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
				StringBuilder traceMsg = new StringBuilder();

				int callState = CallTraceService.CALL_IN_PROGRESS;

				traceMsg.append(PhConstants.traceDelim);
				traceMsg.append("Last Operation(s) were sent on a ");

				switch (primitive) {
				case TcapConstants.PRIMITIVE_BEGIN: {
					traceMsg.append("TCAP BEGIN PACKAGE \n");
					break;
				}
				case TcapConstants.PRIMITIVE_CONTINUE: {
					traceMsg.append("TCAP CONTINUE PACKAGE \n");
					break;
				}
				case TcapConstants.PRIMITIVE_END: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("TCAP END PACKAGE ");

					traceMsg.append("[Pre-Arranged: ");
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
					traceMsg.append("P-ABORT \n");
					break;
				}
				case TcapConstants.PRIMITIVE_USER_ABORT: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("U-ABORT \n");

					traceMsg.append("Information: ");
					traceMsg.append(Util.formatBytes(((UserAbortReqEvent) dre).getUserAbortInformation()));
					traceMsg.append("\n");

					traceMsg.append("Generic Tcap Reason::");
					traceMsg.append(((UserAbortReqEvent) dre).getAbortReason());
					traceMsg.append("\n");

					break;
				}
				case TcapConstants.PRIMITIVE_NOTICE: {
					callState = CallTraceService.CALL_TERMINATED;
					traceMsg.append("NOTICE \n");
					break;
				}
				case TcapConstants.PRIMITIVE_UNIDIRECTIONAL: {
					traceMsg.append("UNIDIRECTIONAL \n");
					break;
				}
				default: {
					traceMsg.append("CODE:" + primitive);
					traceMsg.append("\n");
					break;
				}
				} // @switch primitive

				traceMsg.append(PhConstants.traceDelim);

				if (callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID) != null) {
					List<Integer> constraintList = (List<Integer>) callData.get(CallDataAttribute.P_TRACE_CONSTRAINT_ID);
					Iterator<Integer> constraintIterator = constraintList.iterator();
					while (constraintIterator.hasNext()) {
						cCallTraceService.trace(constraintIterator.next(), InapCS1ScfProtocolUtil.getUniqueCallIDForTracing(callData), traceMsg.toString(), callState,caller,called);
					}
				}

			} // end if tracing required
		} catch (Throwable t) {
			logger.warn("[PH]:: Error tracing Dialog req event" + t.getMessage());
			if (logger.isInfoEnabled()) {
				logger.warn("[PH]:: Error tracing dialogue event", t);
			}
		}

	}
}
