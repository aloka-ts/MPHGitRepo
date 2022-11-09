/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.


Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.ainscf.tr533;

import java.util.LinkedList;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.TR533.TR533ConnectionControlArg;
import com.agnity.ain.TR533.TR533FeatureType;
import com.agnity.ain.TR533.TR533NoAnswerTimer;
import com.agnity.ain.TR533.TR533SpecialRouting;
import com.agnity.ain.enumdata.NatureOfNumEnum;
import com.agnity.ain.enumdata.NumPlanEnum;
import com.agnity.ain.enumdata.TypeOfDigitEnum;
import com.agnity.ain.operations.AinOpCodes;
import com.agnity.ain.operations.AinOperationsCoding;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CarrierInfo;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.AnnSpec.PlayMessage;
import com.agnity.ph.ainscf.AinScfProtocolHelper;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
import com.agnity.ph.ainscf.AinScfRelReasonCode;
import com.agnity.ph.ainscf.gr533.Gr533MessageHandler;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.CriticalityTypeException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
import com.baypackets.ase.util.CallTraceService;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.InvokeReqEvent;
import jain.protocol.ss7.tcap.component.Operation;
import jain.protocol.ss7.tcap.component.Parameters;


/**
 * @author manish.kumar
 *
 */
public class TR533MessageHandler {

	private static Logger logger = Logger.getLogger(TR533MessageHandler.class);
	
	private static Object src = "source".intern();
	
	/**
	 * Method is called to process Provide instruction buffer received
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param cts
	 * @return
	 * @throws Exception
	 */
	static Action[] processTR533ProvideInstruction(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService cts) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processTR533ProvideInstruction Enter, invokeId:" 
					+ invokeIndEvent.getInvokeId());
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

		try {
			Gr533MessageHandler.parseProvideInstruction(invokeIndEvent, callData);

		} catch (ASNParsingException ape) {
			logger.error(dialogId + ":: ASN pasring Exception in processTR533ProvideInstruction.", ape);

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
			return AinScfProtocolHelper.getASNParsingFailureAction(invokeIndEvent, tcapSession, ape);
		} catch (CriticalityTypeException cte) {
			logger.error(dialogId + ":: [PH] CriticalityTypeException in PROVIDER_INSTRUCTION.", cte);

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
					AinScfRelReasonCode.PROVIDE_INSTRUCTION_INVALID_EXTN_TYPE);
			return AinScfProtocolHelper.getInvalidExtensionTypeAction(invokeIndEvent, tcapSession, cte);
		} catch (ParameterOutOfRangeException pore) {
			logger.error(dialogId + ":: [PH] ParameterOutOfRangeException in PROVIDER_INSTRUCTION.", pore);
			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
					AinScfRelReasonCode.PROVIDE_INSTRUCTION_PARAM_OUT_OF_RANGE);
			return AinScfProtocolHelper.getOutOfRangeParamterAction(tcapSession, MESSAGE.PROVIDE_INSTRUCTION);
		}
		return null;

	}
	
	/**
	 * This method is called by the protocol handler for creating connection control
	 * response
	 * 
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createTR533ConnectionControlTerm(String ci,String cicSupport, PhoneNumber destNumber,
			String AMACallType, String featureTypeInd, String noAnsTime, String spRouting) throws Exception {
		byte[] returnBuffer = null;
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createTR533ConnectionControlTerm");
		}
		TR533ConnectionControlArg connectionControlArg = new TR533ConnectionControlArg();
		// Encode Carrier ID type-8
//		else if(leg2Data.get(LegDataAttributes.PI_CARRIER) != null){
//			String cic = (String) leg2Data.get(LegDataAttributes.PI_CARRIER);
//
//			if(StringUtils.isNotBlank(cic)){
//				ci = new CarrierInfo();
//				ci.setAddress(cic);
//
//				logger.debug("createTR533ConnectionControlTerm: CIC for PI_CARRIER is " + cic);
//			}else{
//				logger.debug("createTR533ConnectionControlTerm: CIC for PI_CARRIER is null");
//			}
//		}

		if(ci != null){
			com.agnity.ain.TR533.TR533Digits cic = new com.agnity.ain.TR533.TR533Digits();

			String cicDigits = ci;

			if(cicSupport != null){
				cicDigits = StringUtils.rightPad(cicDigits, 4, '0');
			}


			// as per T1.TRQ3 standard, if provide instruction contains CIC extension then 
			// we need to send 3 digits of carrier else 4 digits
			byte [] cicByte = Gr533MessageHandler.encodeT1RoutingDigits(
					TypeOfDigitEnum.CARRIER_IDENTIFICATION, cicDigits,
					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.SPARE, 0);

			if(cicSupport == null){
				int len = cicByte.length;
				cicByte[len-1] |= 0xF0;
			}

			cic.setValue(Gr533MessageHandler.encodeT1RoutingDigits(
					TypeOfDigitEnum.CARRIER_IDENTIFICATION, cicDigits,
					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.SPARE, 0));

			connectionControlArg.setCarrierId(cic);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting Carrier as:[" + ci + "], byte: " + CommonUtils.formatBytes(cic.getValue()));
			}
		}else{
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: Carrier not set");
			}			
		}


		if (destNumber != null) {
			PhoneNumber routingNumber = (PhoneNumber) destNumber;

			// encode ROuting Number
			com.agnity.ain.TR533.TR533Digits routingNumberDigits = new com.agnity.ain.TR533.TR533Digits();
			routingNumberDigits.setValue(Gr533MessageHandler.encodeT1RoutingDigits(
					TypeOfDigitEnum.ROUTING_NUMBER, routingNumber.getAddress(),
					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.TELEPHONY_NP, 0));

			connectionControlArg.setNetworkRoutingNumber(routingNumberDigits);

			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: RoutingNumber:" +
						CommonUtils.formatBytes(routingNumberDigits.getValue()));
			}
		}else{
			// throw end error
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: RoutingNumber not set");
			}
		}

		
		// Encode BillingIndicator
		if (!StringUtils.isBlank(AMACallType)) {
			String callTypeStr = (String)AMACallType;
			if(StringUtils.isNotBlank(callTypeStr)  ){
				com.agnity.ain.TR533.TR533Digits billIndDigits = new com.agnity.ain.TR533.TR533Digits();
				billIndDigits.setValue(Gr533MessageHandler.encodeT1RoutingDigits(
						TypeOfDigitEnum.BILLING_NUMBER, (callTypeStr),
						NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.TELEPHONY_NP, 0));

				if (logger.isDebugEnabled()) {
					logger.debug("Provide Instruction: Billing Num: " + CommonUtils.formatBytes(billIndDigits.getValue()));
				}

				connectionControlArg.setBillingIndicators(billIndDigits);
			}
		}else{
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: Billing number not set");
			}	
		}
		
		//Feature Type Indicator
		if (!StringUtils.isBlank(featureTypeInd)) {
			TR533FeatureType tr533FeatureType = new TR533FeatureType();//DF
			byte[] output = CommonUtils.convertHexStringToByteArray(featureTypeInd);
			tr533FeatureType.setValue(output);
			connectionControlArg.setFeatureType(tr533FeatureType);
			
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: FTI:" +
						CommonUtils.formatBytes(tr533FeatureType.getValue()));
			}
		}else{
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: Feature Type Indicator not set");
			}	
		}
		
		//no answer time
		if (!StringUtils.isBlank(noAnsTime) ) {
			TR533NoAnswerTimer nat = new TR533NoAnswerTimer();
			byte[] output = CommonUtils.convertHexStringToByteArray(noAnsTime);
			nat.setValue(output);
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: No Answer Time:" +
						CommonUtils.formatBytes(nat.getValue()));
			}
			connectionControlArg.setNoAnswerTime(nat);
		}else{
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: No answer Time not set");
			}	
		}
		
		//special Routing
		if (!StringUtils.isBlank(spRouting) ) {
			TR533SpecialRouting tr533SpecialRouting = new TR533SpecialRouting();
			byte[] output = CommonUtils.convertHexStringToByteArray(spRouting);
			tr533SpecialRouting.setValue(output);
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: special Routing :" +
						CommonUtils.formatBytes(tr533SpecialRouting.getValue()));
			}
			connectionControlArg.setSpecialRouting(tr533SpecialRouting);
		}else{
			if(logger.isDebugEnabled()){
				logger.debug("createTR533ConnectionControlTerm: special Rounting not set");
			}	
		}
	
		
		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.CONNECTION_CONTROL);
		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectionControlArg);
		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
		returnBuffer = encodeList.getFirst();
		System.out.println(CommonUtils.formatBytes(returnBuffer));
		//changing zero'th octet to 0xF2 for parameter set.
		returnBuffer[0] = (byte) 0xF2;
		//changing second octet to 0x84 for TCAP digits.
		returnBuffer[2] = (byte) 0x84;
		//changes routing number tag value from 0x82 to 0x84
		int len = returnBuffer[3];
		len+=4;
		returnBuffer[len] = (byte) 0x84;
		if(logger.isDebugEnabled()){
			logger.debug("createTR533ConnectionControl: Encoded Params: " + CommonUtils.formatBytes(returnBuffer));
		}
		return returnBuffer;
	}

	/**
	 * Method used for sending TR533 Play Announcement
	 * @param callData
	 * @param tcapSession
	 * @throws Exception 
	 */
	public static void sendTR533PlayAnnouncement(TcapSession tcapSession,CallData callData,Action action) throws Exception{
		if (logger.isDebugEnabled()) {
			logger.debug("Inside sendTR533STR()");
		}
		//encoded STR
		byte[] strArgByte = createTR533PlayAnnouncement(callData);
		
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		byte[] strOpCode = CommonUtils.formatIntToByte(AinOpCodes.SEND_TO_RESOURCE_BYTE);
		Operation etcoprn = new Operation(Operation.OPERATIONTYPE_GLOBAL, strOpCode);

		InvokeReqEvent strInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), etcoprn);
		strInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		strInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, strArgByte));
		strInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		strInvokeReqEvent.setLastInvokeEvent(true);
		strInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		//send Component
		AinScfProtocolHelper.sendComponentReq(strInvokeReqEvent, callData, cCallTraceService);
		//end Dialoguel
		AinScfProtocolHelper.sendEndRequestEvent(tcapSession, false, cCallTraceService);
		AinScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendTR533STR()");
		}
	}
	
	/**
	 * Method return encoded data for Play Announcement
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createTR533PlayAnnouncement(CallData callData) throws Exception{
		
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] Inside createTR533PlayAnnouncement");
		}

		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) leg1.get(LegDataAttributes.NP_ANN_SPEC);

		if (annSpec == null) {
			logger.error(dialogId
					+ ":: No announmcement spec specified by Service");
			throw new Exception(dialogId
					+ ":: :: No announmcement spec specified by Service");
		}
		
		//get play message
		PlayMessage playMessage = (PlayMessage) annSpec.getPlayMsgList().get(0);

		if (playMessage == null) {
			logger.error(dialogId
					+ ":: No announmcement specified by Service");
			throw new Exception(dialogId
					+ "::  No announmcement specified by Service");
		}

		int getSs7AnnouncementId;
		AnnSpec.ANN_TYPE annType = playMessage.getAnnType();
		getSs7AnnouncementId = Integer.parseInt(playMessage.getMessageId());
		
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: annType is " + annType
					+ " msgToPlay is " + getSs7AnnouncementId);
		}

		
		byte resBuffer[] = new byte[5];
		int index = 0;
		resBuffer[index++] = TR533Constants.TR533_PLAY_ANN_TAG1;//0xF2
		resBuffer[index++] = TR533Constants.TR533_PLAY_ANN_LEN;//0x03
		resBuffer[index++] = TR533Constants.TR533_PLAY_ANN_TAG2;//0x82
		resBuffer[index++] = TR533Constants.TR533_PLAY_ANN_INDICATOR_LEN;//0x01
		resBuffer[index++] = (byte) getSs7AnnouncementId;
		
		if(logger.isDebugEnabled()){
			logger.debug("createTR533PlayAnnouncement: Encoded Params: " + CommonUtils.formatBytes(resBuffer));
		}
 		return resBuffer;
	}
	
	public static void sendConnectionControlWrapper(TcapSession tcapSession, Action action) throws Exception {
		//multiple times call
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
	
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendConnectionControlWrapper");
		}
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if(leg2Data.get(LegDataAttributes.P_TR533_CARRIER1) != null 
				&& leg2Data.get(LegDataAttributes.P_TR533_DESTINATION_NUMBER1) !=null
				|| !StringUtils.isBlank((String)leg2Data.get(LegDataAttributes.PI_TR533_AMA_CALLTYPE1))
				|| !StringUtils.isBlank((String)leg2Data.get(LegDataAttributes.P_TR533_FEATURE_TYPE_INDICATOR1))) {
			byte[] returnBuffer = createTR533ConnectionControlTerm((String)leg2Data.get(LegDataAttributes.P_TR533_CARRIER1),
					(String)leg1Data.get(LegDataAttributes.P_PI_TR533_O_CIC_SUPPORT1),
					(PhoneNumber)leg2Data.get(LegDataAttributes.P_TR533_DESTINATION_NUMBER1),
					(String)leg2Data.get(LegDataAttributes.PI_TR533_AMA_CALLTYPE1),
					(String)leg2Data.get(LegDataAttributes.P_TR533_FEATURE_TYPE_INDICATOR1),
					(String)leg2Data.get(LegDataAttributes.P_TR533_NO_ANSWER_TIME1),
					(String)leg2Data.get(LegDataAttributes.P_TR533_SPECIAL_ROUTING1));
			if (logger.isInfoEnabled()) {
				logger.info("CONNECTION CONTROL ::" + CommonUtils.formatBytes(returnBuffer));
			}
			sendConnectionControlComponent(tcapSession, returnBuffer);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TR533_CARRIER2) != null 
				&& leg2Data.get(LegDataAttributes.P_TR533_DESTINATION_NUMBER2) !=null
				|| !StringUtils.isBlank((String)leg2Data.get(LegDataAttributes.PI_TR533_AMA_CALLTYPE2))
				|| !StringUtils.isBlank((String)leg2Data.get(LegDataAttributes.P_TR533_FEATURE_TYPE_INDICATOR2))) {
			byte[] returnBuffer = createTR533ConnectionControlTerm((String)leg2Data.get(LegDataAttributes.P_TR533_CARRIER2),
					(String)leg1Data.get(LegDataAttributes.P_PI_TR533_O_CIC_SUPPORT2),
					(PhoneNumber)leg2Data.get(LegDataAttributes.P_TR533_DESTINATION_NUMBER2),
					(String)leg2Data.get(LegDataAttributes.PI_TR533_AMA_CALLTYPE2),
					(String)leg2Data.get(LegDataAttributes.P_TR533_FEATURE_TYPE_INDICATOR2),
					(String)leg2Data.get(LegDataAttributes.P_TR533_NO_ANSWER_TIME2),
					(String)leg2Data.get(LegDataAttributes.P_TR533_SPECIAL_ROUTING2));
			if (logger.isInfoEnabled()) {
				logger.info("CONNECTION CONTROL ::" + CommonUtils.formatBytes(returnBuffer));
			}
			sendConnectionControlComponent(tcapSession, returnBuffer);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TR533_CARRIER3) != null 
				&& leg2Data.get(LegDataAttributes.P_TR533_DESTINATION_NUMBER3) !=null
				|| !StringUtils.isBlank((String)leg2Data.get(LegDataAttributes.PI_TR533_AMA_CALLTYPE3))
				|| !StringUtils.isBlank((String)leg2Data.get(LegDataAttributes.P_TR533_FEATURE_TYPE_INDICATOR3))) {
			byte[] returnBuffer = createTR533ConnectionControlTerm((String)leg2Data.get(LegDataAttributes.P_TR533_CARRIER3),
					(String)leg1Data.get(LegDataAttributes.P_PI_TR533_O_CIC_SUPPORT3),
					(PhoneNumber)leg2Data.get(LegDataAttributes.P_TR533_DESTINATION_NUMBER3),
					(String)leg2Data.get(LegDataAttributes.PI_TR533_AMA_CALLTYPE3),
					(String)leg2Data.get(LegDataAttributes.P_TR533_FEATURE_TYPE_INDICATOR3),
					(String)leg2Data.get(LegDataAttributes.P_TR533_NO_ANSWER_TIME3),
					(String)leg2Data.get(LegDataAttributes.P_TR533_SPECIAL_ROUTING3));
			if (logger.isInfoEnabled()) {
				logger.info("CONNECTION CONTROL ::" + CommonUtils.formatBytes(returnBuffer));
			}
			sendConnectionControlComponent(tcapSession, returnBuffer);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TR533_CARRIER4) != null 
				&& leg2Data.get(LegDataAttributes.P_TR533_DESTINATION_NUMBER4) !=null
				|| !StringUtils.isBlank((String)leg2Data.get(LegDataAttributes.PI_TR533_AMA_CALLTYPE4))
				|| !StringUtils.isBlank((String)leg2Data.get(LegDataAttributes.P_TR533_FEATURE_TYPE_INDICATOR4))) {
			byte[] returnBuffer = createTR533ConnectionControlTerm((String)leg2Data.get(LegDataAttributes.P_TR533_CARRIER4),
					(String)leg1Data.get(LegDataAttributes.P_PI_TR533_O_CIC_SUPPORT4),
					(PhoneNumber)leg2Data.get(LegDataAttributes.P_TR533_DESTINATION_NUMBER4),
					(String)leg2Data.get(LegDataAttributes.PI_TR533_AMA_CALLTYPE4),
					(String)leg2Data.get(LegDataAttributes.P_TR533_FEATURE_TYPE_INDICATOR4),
					(String)leg2Data.get(LegDataAttributes.P_TR533_NO_ANSWER_TIME4),
					(String)leg2Data.get(LegDataAttributes.P_TR533_SPECIAL_ROUTING4));
			if (logger.isInfoEnabled()) {
				logger.info("CONNECTION CONTROL ::" + CommonUtils.formatBytes(returnBuffer));
			}
			sendConnectionControlComponent(tcapSession, returnBuffer);
		}
		
		if (action.getSendMode() == Action.SEND_MODE.END) {

			callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
			AinScfProtocolHelper.preProcessDroppedCall(tcapSession);

			AinScfProtocolHelper.sendEndRequestEvent(tcapSession, false, cCallTraceService);

			AinScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
		} 
		
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: exit sendConnectionControlWrapper");
		}
	}
	
	public static void sendConnectionControlComponent(TcapSession tcapSession, byte[] connectionControlTerm) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendConnectionControlComponent");
		}
		
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		byte[] connectionControlOpcode = CommonUtils.formatIntToByte(AinOpCodes.CONNECTION_CONTROL_BYTE);// 1025
		if(logger.isInfoEnabled()) {
			logger.info("connectionControlOpcode"+CommonUtils.formatBytes(connectionControlOpcode));
		}
		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_GLOBAL, connectionControlOpcode);
		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setLastInvokeEvent(true);
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, connectionControlTerm));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		connectInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		AinScfProtocolHelper.sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: exit sendConnectionControlComponent");
		}
	}
	
}
