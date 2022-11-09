/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.


Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.ainscf.gr533;

import java.util.Arrays;
import java.util.LinkedList;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.T1_660.CICExpansion;
import com.agnity.ain.T1_660.ConnectionControlArg;
import com.agnity.ain.T1_660.Digits;
import com.agnity.ain.T1_660.OriginatingStationType;
import com.agnity.ain.T1_660.ProviderInstructionArg;
import com.agnity.ain.datatypes.AddressSignal;
import com.agnity.ain.datatypes.T1Digits;
import com.agnity.ain.enumdata.EncodingSchemeEnum;
import com.agnity.ain.enumdata.NatureOfNumEnum;
import com.agnity.ain.enumdata.NumPlanEnum;
import com.agnity.ain.enumdata.TypeOfDigitEnum;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.operations.AinOpCodes;
import com.agnity.ain.operations.AinOperationsCoding;
import com.agnity.ain.util.Constant;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CarrierInfo;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Action.DROP_CALL_MODE;
import com.agnity.mphdata.common.AnnSpec.PlayMessage;
import com.agnity.ph.ainscf.AinScfProtocolHelper;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
import com.agnity.ph.ainscf.AinScfRelReasonCode;
import com.agnity.ph.ainscf.gr533.GR533Constants;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
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

import jain.protocol.ss7.tcap.component.ComponentConstants;
import jain.protocol.ss7.tcap.component.ErrorReqEvent;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.InvokeReqEvent;
import jain.protocol.ss7.tcap.component.Operation;
import jain.protocol.ss7.tcap.component.Parameters;

/**
 * @author rarya
 *
 */
public class Gr533MessageHandler {
	private static Logger logger = Logger.getLogger(Gr533MessageHandler.class);
	private static Object src = "source".intern();

	/**
	 * Method is called to process Provide instruction buffer received
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @param cts
	 * @return
	 * @throws Exception
	 */
	public static Action[] processProvideInstruction(InvokeIndEvent invokeIndEvent, TcapSession tcapSession,
			CallTraceService cts) throws Exception {

		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "[PH] :: processProviderInstruction Enter, invokeId:" 
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
			parseProvideInstruction(invokeIndEvent, callData);

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
	 * Method is called to parse Provide Instruction
	 * @param invokeIndEvent
	 * @param callData
	 * @throws Exception
	 */
	public static void parseProvideInstruction(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH]  Enter :: parseProviderInstruction");
		}

		ProviderInstructionArg providerInstructionArg;
		try {
			Parameters parameters = invokeIndEvent.getParameters();
			byte[] parameter = parameters.getParameter();

			// To parse the parameter set through BinaryNotes Need to convert F2 to 31.
			parameter[0] = 0x31;
			if(parameter.length>=35) {
				parameter[35] = 0x25;	
			}

			parameters.setParameter(parameter);

			if (logger.isInfoEnabled()) {
				logger.info("Provide Instruction Buffer with 0xF2 repalced with 0x31 [" 
						+ CommonUtils.formatBytes(parameter) + "]");
			}

			invokeIndEvent.setParameters(parameters);

			providerInstructionArg = (ProviderInstructionArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
			if (logger.isInfoEnabled()) {
				logger.info(dialogueId + " ProvideIstruction decoded successfully" 
						+ providerInstructionArg.toString());
			}
		} catch (Exception e) {
			// in case there is an exception in deocding Provide instruction we need to send Reject
			// with problem cod
			logger.error(dialogueId + ":: [PH] Exception in Provider Instruction (Exception)" + e.getMessage());
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, ProvideInstruction parsing failure occured.", e,
					MESSAGE.PROVIDE_INSTRUCTION);
		}

		try {
			// Called Party ANI
			if (providerInstructionArg.getEndUserNumber() != null
					&& providerInstructionArg.getEndUserNumber().getDigits() != null) {
				T1Digits t1Digit = new T1Digits();

				T1Digits calledAin = t1Digit.decodeDigits(
						providerInstructionArg.getEndUserNumber().getDigits().getValue(), Constant.CALLED);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Called party number from Provider Instruction arg is "
							+ calledAin);
				}
				if (calledAin == null || calledAin.getAddrSignal() == null
						|| "".equals(calledAin.getAddrSignal().trim())) {
					/*
					 * call should be handled as ASn parse failure as address signal is missing
					 */
					logger.error(dialogueId + ":: [PH] called party num address signal missing in provide instruction");
					throw new ASNParsingException(
							dialogueId + ":: [PH] called party num address signal missing in provide instruction",
							MESSAGE.PROVIDE_INSTRUCTION);
				}
				PhoneNumber calledNumber = AinScfProtocolHelper.parseT1PhoneNumber(dialogueId, calledAin);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parseCalledPartyNum, CalledNumber:" + calledAin);
				}
				legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("calledPartyNumber not present in Provide Instruction");
				}
			}

			// Calling Party ANI
			if (providerInstructionArg.getCallingPartyAni() != null) {
				T1Digits t1Digits = new T1Digits();
				T1Digits callingNum = t1Digits.decodeDigits(providerInstructionArg.getCallingPartyAni().getValue(),
						Constant.CALLING);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted calling party number from Provider Instruction arg is "
							+ callingNum);
				}
				if (callingNum == null || callingNum.getAddrSignal() == null
						|| "".equals(callingNum.getAddrSignal().trim())) {
					logger.error(
							dialogueId + ":: [PH] calling party num address signal missing in provide instruction");
					throw new ASNParsingException(
							dialogueId + ":: [PH] calling party num address signal missing in provide instruction",
							MESSAGE.PROVIDE_INSTRUCTION);
				}
				PhoneNumber callingNumber = AinScfProtocolHelper.parseT1PhoneNumber(dialogueId, callingNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parsecallingPartyNum, callingNumber:" + callingNum);
				}
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
			} else {
				// fetch the offset 
				Parameters parameters = invokeIndEvent.getParameters();
				byte[] parameter = parameters.getParameter();
				int offset = 6 + providerInstructionArg.getEndUserNumber().getDigits().getValue().length;
				int len = parameter[offset+1];
				byte[] callingPartyBuf = Arrays.copyOfRange(parameter, offset+2, offset+2+len);
				if(logger.isInfoEnabled()) {
					logger.info("Calling party null: offset:"+offset + ": len:" + len 
							+ ": " +CommonUtils.formatBytes(callingPartyBuf));
				}
				//Calling party null: offset:15: len:9: 0x02 0x00 0x21 0x0a 0x21 0x43 0x65 0x87 0x09
				T1Digits callingPartyT1Digits = new T1Digits();
				T1Digits t1Digits = new T1Digits();

				callingPartyT1Digits = t1Digits.decodeDigits(callingPartyBuf, "CALLING");
				PhoneNumber callingPartyPhoneNumber = AinScfProtocolHelper.parseT1PhoneNumber(dialogueId, callingPartyT1Digits);
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingPartyPhoneNumber);
			}

			// LATA
			if (providerInstructionArg.getCgpLata() != null) {
				T1Digits t1Digit = new T1Digits();

				T1Digits lataNum = t1Digit.decodeDigits(providerInstructionArg.getCgpLata().getValue(),
						Constant.CALLING);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted lata from Provider Instruction arg is " + lataNum);
				}
				if (lataNum == null || lataNum.getAddrSignal() == null || "".equals(lataNum.getAddrSignal().trim())) {
					logger.error(dialogueId + ":: [PH] lata address signal missing in provide instruction");
					throw new ASNParsingException(
							dialogueId + ":: [PH] lata address signal missing in provide instruction",
							MESSAGE.PROVIDE_INSTRUCTION);
				}
				PhoneNumber lataNumber = AinScfProtocolHelper.parseT1PhoneNumber(dialogueId, lataNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parsecallingPartyNum, Lata:" + lataNum);
				}

				if(StringUtils.isNoneBlank(lataNumber.getAddress())){
					legData.set(LegDataAttributes.P_LATA, lataNumber.getAddress());
				}else{
					logger.debug(dialogueId + ":: lata number not present");
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("LATA not present in Provider Instruction");
				}
			}

			/*
			 * provideInstruction . originating station type
			 */
			if (providerInstructionArg.getOli() != null) {
				OriginatingStationType originatingStationType = providerInstructionArg.getOli();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted OriginatingStationType from provideInstruction: "
							+ originatingStationType.getValue());
				}

				int cpc = -1;
				if(originatingStationType != null && originatingStationType.getValue() != null){
					byte[] val = originatingStationType.getValue();

					if(val.length >= 1){
						cpc = val[0] & 0x0F;
					}
				}

				if(cpc != -1){
					legData.set(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE, cpc);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: [PH] Setting OriginatingStationType: "
								+ cpc);
					}
				}else{
					logger.debug(dialogueId + ":: not setting cpc");
				}
			} 

			/*
			 * provideInstruction . cic support
			 */
			if (providerInstructionArg.getCicSupport() != null) {
				CICExpansion cicSupport = providerInstructionArg.getCicSupport();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted cicSupport from provide Instruction: "
							+ cicSupport.getValue());
				}

				if(cicSupport.getValue() != null){
					legData.set(LegDataAttributes.P_PI_O_CIC_SUPPORT, cicSupport.getValue().ordinal());
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("cicSupport not present in Provider Instruction");
				}
			}
		} catch (InvalidInputException e) {
			logger.error(dialogueId + ":: Error in parseProvideInstruction (InvalidInputException)" + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::ASN Parsing Failure: Provide Instruction parsing failure occured.:", e,
					MESSAGE.PROVIDE_INSTRUCTION);
		} catch (ASNParsingException e) {
			logger.error(dialogueId + "::[PH] Error in parseProvideInstruction (ASNParsingException)" + e.getMessage());
			throw e;
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseProvideInstruction (Exception)" + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure:  ProvideInstruction parsing failure occured.", e,
					MESSAGE.PROVIDE_INSTRUCTION);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Provide Instruction parsed successfully");
		}
	}

	/**
	 * Method encodes ACG from AJ to AQ
	 * Control Cause Indicator 
	 * Duration
	 * Gap 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createGr533Acg(CallData callData) throws Exception {

		if(logger.isDebugEnabled()){
			logger.debug("Inside createGr553ACG");
		}

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		//encode DIaled digit
		byte[] encodedDialedPartyByte = encodeDialedParty(callData);

		// encode ACG params
		byte[] encodedACGByte = encodeACGParams(leg2Data);

		// Complete Buffer= Dialed Digit + ACF params
		byte[] ACGBuffer = new byte[2 + encodedDialedPartyByte.length + encodedACGByte.length];

		int index = 0; 
		ACGBuffer[index++] = GR533Constants.ACG_PARAMETER_SET_ID; // Tag 0xF2
		ACGBuffer[index++] = (byte) (encodedDialedPartyByte.length + encodedACGByte.length);

		for(int j=0; j< encodedDialedPartyByte.length; j++){
			ACGBuffer[index++] = encodedDialedPartyByte[j];
		}

		for(int j=0; j< encodedACGByte.length; j++){
			ACGBuffer[index++] = encodedACGByte[j];
		}

		if(logger.isDebugEnabled()){
			logger.debug("createGr553ACG: Encoded ACG Params: " + CommonUtils.formatBytes(ACGBuffer));
		}

		return ACGBuffer;
	}

	/**
	 * Method encodes SendNotification from AZ to BD
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createGr533SendNotification(CallData callData) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug(" Inside createGr533SendNotification");
		}

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		String echoData = (String) leg2Data.get(LegDataAttributes.P_ECHO_DATA);

		if(echoData == null || StringUtils.isBlank(echoData)){
			if(logger.isDebugEnabled()){
				logger.debug("Exit createGr533SendNotification, echo data not set by application");
			}
			return null;
		}
		int echoDataLen = echoData.length();
		int sendNotificationParameterSetLen = 3+ echoDataLen;
		byte[] sendNotificationBuffer = new byte[4 + echoDataLen];
		int index = 0;
		sendNotificationBuffer[index++] = GR533Constants.SEND_NOTFICATION_PARAMETER_SET_ID;//0xF2
		sendNotificationBuffer[index++] = (byte)sendNotificationParameterSetLen;
		sendNotificationBuffer[index++] = GR533Constants.ECHO_DATA_TAG1;//0xDF
		sendNotificationBuffer[index++] = GR533Constants.ECHO_DATA_TAG2;//0x43
		sendNotificationBuffer[index++] = (byte)echoDataLen;

		//convert string to byte array & return 4 octets
		byte[] echoDataBuffer = CommonUtils.convertHexStringToByteArray(echoData); //need to check once

		if(logger.isDebugEnabled()){
			logger.debug("createGr533SendNotification: Encoded echo Data: " + CommonUtils.formatBytes(echoDataBuffer));
		}

		for(int i=0;i<echoDataBuffer.length;i++) {
			sendNotificationBuffer[index++] = echoDataBuffer[i];
		}

		if(logger.isDebugEnabled()){
			logger.debug("createGr533SendNotification: Encoded sendNotificationBuffer: " + CommonUtils.formatBytes(sendNotificationBuffer));
		}
		return sendNotificationBuffer;	
	}

	/**
	 * @description here used to encode DialedParty Number for GR533 
	 * @return byte array containing encoded byte data
	 * @param addrSignal
	 * @param natureOfNumberEnum
	 * @param numberingPlanEnum
	 * @return
	 * @throws Exception 
	 * @throws com.genband.isup.exceptions.InvalidInputException
	 */
	public static byte[] encodeDialedParty(CallData callData) throws Exception{

		if(logger.isDebugEnabled()){
			logger.debug("Inside encodeDialedParty");
		}

		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		//Called party fetched from leg1 data
		PhoneNumber calledNumber = (PhoneNumber) leg1Data.get(LegDataAttributes.P_CALLED_PARTY);

		// digits
		byte[] bcdDigits = AddressSignal.encodeAdrsSignal(calledNumber.getAddress());

		int i = 0;
		// total buffer shall be from AL to AN
		byte[] myParms = new byte[6 + bcdDigits.length];

		myParms[i++] = GR533Constants.GR533_ACG_DIGITS_TAG;  // 0x84
		myParms[i++] = (byte) (bcdDigits.length + 4);  // this length might be overwritten 
		myParms[i++] = GR533Constants.GR533_ACG_DIGITS_TYPE; // 0x01
		myParms[i++] = GR533Constants.GR533_ACG_DIGITS_NOA;  // 0x00
		myParms[i++] = GR533Constants.GR533_ACG_DIGITS_NP;   // 0x21
		myParms[i++] = (byte) calledNumber.getAddress().length();   // Number of digits

		// copy encoded digits
		for (int j = 0; j < bcdDigits.length; j++) {
			myParms[i++] = bcdDigits[j];
		}

		if(logger.isDebugEnabled()) {
			logger.debug("encodeDialedParty:Encoded Called Party Num: "+ CommonUtils.formatBytes(myParms));
		}

		return myParms;
	}

	/**
	 * Method encodes ACG based on GR533 section B.2.9
	 * @description here used to encode ACG Params for GR533 
	 * @return byte array containing encoded data
	 * @param legData
	 * @return
	 */
	public static byte[] encodeACGParams(LegData legData) {
		if(logger.isDebugEnabled()){
			logger.debug("Inside encodeACGParams");
		}

		//control cause indicator
		byte[] ACGParameters = new byte[GR533Constants.GR533_ACG_LEN];
		ACGParameters[0] = GR533Constants.GR533_ACG_TAG1; //0xDF
		ACGParameters[1] = GR533Constants.GR533_ACG_TAG2; //0x47
		ACGParameters[2]=  GR533Constants.GR533_ACG_INDICATOR_LEN;//0x47

		// ACG contains 
		// Control Cause Indicator - possible values
		// This is different from GR1299. SO shall create new field. 
		// 0 0 0 0 0 0 0 0 : not used
		// 0 0 0 0 0 0 0 1 : vacant code
		// 0 0 0 0 0 0 1 0 : out of band
		// 0 0 0 0 0 0 1 1 : database overload
		// 0 0 0 0 0 1 0 0 : destination mass calling
		// 0 0 0 0 0 1 0 1 : SMS initiated

		// Duration 
		// 0 0 0 0 0 0 0 0 : not used
		// 0 0 0 0 0 0 0 1 : 1 second
		// 0 0 0 0 0 0 1 0 : 2 seconds
		// 0 0 0 0 0 0 1 1 : 4 seconds
		// 0 0 0 0 0 1 0 0 : 8 seconds
		// 0 0 0 0 0 1 0 1 : 16 seconds
		// 0 0 0 0 0 1 1 0 : 32 seconds
		// 0 0 0 0 0 1 1 1 : 64 seconds
		// 0 0 0 0 1 0 0 0 : 128 seconds
		// 0 0 0 0 1 0 0 1 : 256 seconds
		// 0 0 0 0 1 0 1 0 : 512 seconds
		// 0 0 0 0 1 0 1 1 : 1024 seconds
		// 0 0 0 0 1 1 0 0 : 2048 seconds
		// 0 0 0 0 1 1 0 1 : infinity

		// Gap
		//0 0 0 0 0 0 0 0 : 0 seconds
		// 0 0 0 0 0 0 0 1 : 3 seconds
		// 0 0 0 0 0 0 1 0 : 4 seconds
		// 0 0 0 0 0 0 1 1 : 6 seconds
		// 0 0 0 0 0 1 0 0 : 8 seconds
		// 0 0 0 0 0 1 0 1 : 11 seconds
		// 0 0 0 0 0 1 1 0 : 16 seconds
		// 0 0 0 0 0 1 1 1 : 22 seconds
		// 0 0 0 0 1 0 0 0 : 30 seconds
		// 0 0 0 0 1 0 0 1 : 42 seconds
		// 0 0 0 0 1 0 1 0 : 58 seconds
		// 0 0 0 0 1 0 1 1 : 81 seconds
		// 0 0 0 0 1 1 0 0 : 112 seconds
		// 0 0 0 0 1 1 0 1 : 156 seconds
		// 0 0 0 0 1 1 1 0 : 217 seconds
		// 0 0 0 0 1 1 1 1 : 300 second

		int scpOverloadControlInd = 0;
		if(legData.get(LegDataAttributes.GR533_ACG_CONTROL_CAUSE_IND)!= null){
			scpOverloadControlInd = Integer.parseInt((String)legData.get(LegDataAttributes.GR533_ACG_CONTROL_CAUSE_IND));
		}

		// possible value should be between 0 and 6
		if(scpOverloadControlInd < 0 || scpOverloadControlInd > 6){
			scpOverloadControlInd = 0;

			if(logger.isDebugEnabled()){
				logger.debug("GR533_ACG_CONTROL_CAUSE_IND is set out of valid range 0 and 6, setting 0");
			}
		}
		// Set Control Cause 
		ACGParameters[3]  = (byte) (scpOverloadControlInd);

		//Gap Duration 
		int gapDur = 512; // default value
		if(legData.get(LegDataAttributes.ACG_GAP_DURATION)!= null){
			gapDur = Integer.parseInt((String)legData.get(LegDataAttributes.ACG_GAP_DURATION));
		}

		if(logger.isDebugEnabled()){
			logger.debug("encodeACGParams: gap Duration : "+ gapDur);
		}

		// GapDur shall be one of the value from 1,2,4,8,16,32,64 etc.. 
		// depending on value we need to encodeACG byte
		switch(gapDur){
		case 1:   ACGParameters[4] = (byte)0x01;break;//1 seconds
		case 2:   ACGParameters[4] = (byte)0x02;break;//2 seconds
		case 4:   ACGParameters[4] = (byte)0x03;break;//4 seconds
		case 8:   ACGParameters[4] = (byte)0x04;break;//8 seconds
		case 16:  ACGParameters[4] = (byte)0x05;break;//16 seconds
		case 32:  ACGParameters[4] = (byte)0x06;break;//32 seconds
		case 64:  ACGParameters[4] = (byte)0x07;break;//64 seconds
		case 128: ACGParameters[4] = (byte)0x08;break;//128 seconds
		case 256: ACGParameters[4] = (byte)0x09;break;//256 seconds
		case 512: ACGParameters[4] = (byte)0x0A;break;//512 seconds
		case 1024:ACGParameters[4] = (byte)0x0B;break;//1024 seconds
		case 2048:ACGParameters[4] = (byte)0x0C;break;//2048 seconds
		case -1:  ACGParameters[4] = (byte)0x0D;break;//infinty seconds
		default:  ACGParameters[4] = (byte)0x00;break;//default not used
		}

		//set gap bits
		int gapIntervalValue = 30;
		if(legData.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE) != null){
			gapIntervalValue = Integer.parseInt((String) legData.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
		}
		if(logger.isDebugEnabled()) {
			logger.debug("encodeACGParams: gapIntervalValue : "+ gapIntervalValue);
		}

		switch(gapIntervalValue){
		case 0:  ACGParameters[5] = (byte)0x00;break;//0 seconds
		case 3:  ACGParameters[5] = (byte)0x01;break;//3 seconds
		case 4:  ACGParameters[5] = (byte)0x02;break;//4 seconds
		case 6:  ACGParameters[5] = (byte)0x03;break;//6 seconds
		case 8:  ACGParameters[5] = (byte)0x04;break;//8 seconds
		case 11: ACGParameters[5] = (byte)0x05;break;//11 seconds
		case 16: ACGParameters[5] = (byte)0x06;break;//16 seconds
		case 22: ACGParameters[5] = (byte)0x07;break;//22 seconds
		case 30: ACGParameters[5] = (byte)0x08;break;//30 seconds
		case 42: ACGParameters[5] = (byte)0x09;break;//42 seconds
		case 58: ACGParameters[5] = (byte)0x0A;break;//58 seconds
		case 81: ACGParameters[5] = (byte)0x0B;break;//81 seconds
		case 112:ACGParameters[5] = (byte)0x0C;break;//112 seconds
		case 156:ACGParameters[5] = (byte)0x0D;break;//156 seconds
		case 217:ACGParameters[5] = (byte)0x0E;break;//217 seconds
		case 300:ACGParameters[5] = (byte)0x0F;break;//300 seconds
		default: ACGParameters[5] = (byte)0x00;break;// same as default value
		}

		if(logger.isDebugEnabled()){
			logger.debug("encodeACGParams:Encoded ACGParameters: "+ CommonUtils.formatBytes(ACGParameters));
		}

		return ACGParameters;
	}

	/**
	 * method parse buffer recieved in case of Termination Notification
	 * @param invokeIndEvent
	 * @param callData
	 * @throws Exception
	 */
	public static void parseTerminationNotification(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("Inside parseTerminationNotification, serviceId");
		}

		byte[] input = invokeIndEvent.getParameters().getParameter();

		if (logger.isDebugEnabled()) {
			logger.debug("parseTerminationNotification: input buffer " + CommonUtils.formatBytes(input));
		}
		int bufferIndex = 0;
		//fields decoded from B.4.6 -  SSP Termination Information Message
		if (input[bufferIndex++] != GR533Constants.TN_PACKAGE_TYPE) {//0xE1
			logger.error("parseTerminationNotification: Package Type is not correct: " + CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTerminationNotification: Package Type is not correct");
		}

		if (input.length <= GR533Constants.TN_TOTAL_LEN) {
			bufferIndex++;
			logger.error("parseTerminationNotification: Length of the buffer must be 29 without the Error Code or Problem Data parameters : " + CommonUtils.formatBytes(input));
			throw new ParameterOutOfRangeException("Length of the buffer must be 29 without the Error Code or Problem Data parameters");
		}else {
			bufferIndex++;
		}

		if(input[bufferIndex++] != GR533Constants.TN_TRANSACTION_ID) {//0xC7
			logger.error("parseTermintationNotification: Transaction ID is not correct: "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTermintationNotification: Transaction ID is not correct");
		}

		if(input[bufferIndex++] != GR533Constants.TN_TRANSACTION_ID_LEN) {//0x00
			logger.error("parseTermintationNotification: Transaction ID length should be zero:  "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("Transaction ID length should be zero");
		}

		if(input[bufferIndex++] != GR533Constants.TN_COMPONENT_SEQ_ID) {//0xE8
			logger.error("parseTermintationNotification: Component Sequence ID is not correct: "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTermintationNotification: Component Sequence ID is not correct");
		}

		if(input[bufferIndex++] != GR533Constants.TN_COMPONENT_SEQ_LEN) {//0x19
			logger.error("parseTermintationNotification: Component Sequence ID length must be 25 without Error Code or Problem Data parameters :  "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("Component Sequence ID length must be 25 without Error Code or Problem Data parameters");
		}

		if(input[bufferIndex++] != GR533Constants.TN_COMPONENT_TYPE_ID) {//0xEA
			logger.error("parseTermintationNotification: Component Type ID is not correct: "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTermintationNotification: Component Type ID is not correct");
		}

		if(input[bufferIndex++] != GR533Constants.TN_COMPONENT_LEN) {//0x17
			logger.error("parseTermintationNotification: The total length of component is 23 octets long, excluding the Type Identifier and Length octets :  "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The total length of component is 23 octets long, excluding the Type Identifier and Length octets");
		}

		if(input[bufferIndex++] != GR533Constants.TN_COMPONENT_ID) {//0xCF
			logger.error("parseTermintationNotification: Component ID is not correct: "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTermintationNotification: Component ID is not correct");
		}

		if(input[bufferIndex++] != GR533Constants.TN_COMPONENT_ID_LEN) {//0x00
			logger.error("parseTermintationNotification: The length of the Component ID is 0 octets :  "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of the Component ID is 0 octets ");
		}

		if(input[bufferIndex++] != GR533Constants.TN_PARAMETER_ID) {//0xF2
			logger.error("parseTermintationNotification: Parameter Set Identifier is not correct: "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTermintationNotification: Parameter Set Identifier is not correct");
		}

		if(input[bufferIndex++] != GR533Constants.TN_PARAMETER_LEN) {//0x13
			logger.error("parseTermintationNotification: The length of this Component ID must be 19 octets :  "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of the Component ID must be 19 octets ");
		}

		if(input[bufferIndex++] != GR533Constants.TN_TERMINATION_ID1 || input[bufferIndex++] != GR533Constants.TN_TERMINATION_ID2) {//0xDF & 0x46
			logger.error("parseTermintationNotification: Termination Indicators Identifier Tags are not correct: "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTermintationNotification: Termination Indicators Identifier Tags are not correct");
		}

		if(input[bufferIndex++] != GR533Constants.TN_TERMINATION_ID_LEN) {//0x01
			logger.error("parseTermintationNotification: The length of Termination Indicators ID must be 1 octet :  "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of Termination Indicators ID must be 1 octet ");
		}

		//Termination Indicator
		int terminationIndicator = (input[bufferIndex++] & 0x0F);

		// possible value
		// a/1 bit - NM Control List Overflow Indicator
		// b/2 bit - Answer Indication
		// c/3 bit - Error Indication
		// d,e,f,g,h bit - spare

		switch (terminationIndicator) {
		case 1: {
			if (logger.isDebugEnabled()) {
				logger.debug(
						"GR533 TerminationIndicator received: NM Control List Overflow Indicator" + terminationIndicator);
			}
			callData.set(CallDataAttribute.P_AIN_TERMINATION_IND, "1");
			break;
		}
		case 2: {
			if (logger.isDebugEnabled()) {
				logger.debug("GR533 TerminationIndicator received: Answer Indication" + terminationIndicator);
			}
			callData.set(CallDataAttribute.P_AIN_TERMINATION_IND, "2");
			break;
		}
		case 4: {
			if (logger.isDebugEnabled()) {
				logger.debug("GR533 TerminationIndicator received: Error Indication" + terminationIndicator);
			}
			callData.set(CallDataAttribute.P_AIN_TERMINATION_IND, "4");
			break;
		}
		default:
			logger.error("Unknown GR533 TerminationIndicator received:" + terminationIndicator);
		}

		//Echo Data
		if(input[bufferIndex++] != GR533Constants.ECHO_DATA_TAG1 || input[bufferIndex++] != GR533Constants.ECHO_DATA_TAG2 ) {//0xDF && 0x43
			logger.error("parseTermintationNotification: Echo Data Identifier is not correct: "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTermintationNotification:Echo Data Identifier is not correct");
		}

		if(input[bufferIndex++] != (byte)GR533Constants.ECHO_DATA_LEN) {//0x04
			logger.error("parseTermintationNotification: The length of Echo Data  ID must be 4 octets :  "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of the Echo Data ID must be 4 octets ");
		}

		int echoStartIndex = bufferIndex; //endIndex is 22
		byte[] echoDataByte = new byte[GR533Constants.ECHO_DATA_LEN];
		int index=0;
		for(int i = echoStartIndex; i< (echoStartIndex+GR533Constants.ECHO_DATA_LEN);i++) { //4 octets
			echoDataByte[index++] = input[i];
		}
		bufferIndex+=4;
		if (logger.isDebugEnabled()) {
			logger.debug("parseTerminationNotification: echo Data Bytes " + CommonUtils.formatBytes(echoDataByte));
		}

		String echoDataString = CommonUtils.convertByteArrayToString(echoDataByte);

		if (logger.isDebugEnabled()) {
			logger.debug("parseTerminationNotification: echo Data String " + echoDataString);
		}

		callData.set(CallDataAttribute.P_AIN_ECHO_DATA, echoDataString);

		//Connect Time
		if(input[bufferIndex++] != GR533Constants.CONNECT_TIME_TAG1 || input[bufferIndex++] != GR533Constants.CONNECT_TIME_TAG2 ) {//0xDE && 0x42
			logger.error("parseTermintationNotification: Connect Time Identifier is not correct: "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("parseTermintationNotification:Connect Time Identifier is not correct");
		}

		if(input[bufferIndex++] != (byte)GR533Constants.CONNECT_TIME_LEN) {//0x05
			logger.error("parseTermintationNotification: The length of Connect Time  ID must be 5 octets :  "+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of the Connect Time ID must be 5 octets ");
		}

		int connectTimeStartIndex = bufferIndex; //endIndex is 30
		byte[] connectTimeByte = new byte[GR533Constants.CONNECT_TIME_LEN];
		int index1=0;
		for(int i = connectTimeStartIndex; i< (connectTimeStartIndex+GR533Constants.CONNECT_TIME_LEN);i++) { //5 octets
			connectTimeByte[index1++] = input[i];
		}
		bufferIndex+=5;
		if (logger.isDebugEnabled()) {
			logger.debug("parseTerminationNotification: connect Time Bytes " + CommonUtils.formatBytes(connectTimeByte));
		}

		String connectTime = AddressSignal.decodeAdrsSignal(connectTimeByte, 0, 1);

		// First Digit is filler. 
		// First 5 digits are minutes
		// Next 2 digits are seconds
		// Next digit is tenths of seconds. 
		// last digit is Sign Field
		connectTime = connectTime.substring(1);
		String min = connectTime.substring(0, 5);
		String sec = connectTime.substring(5, 7);
		String tenthOfSec = connectTime.substring(7,8);

		if(logger.isDebugEnabled()){
			logger.debug("parseTerminationNotification:" + connectTime +
					" Min:" + min + ":Sec: "+ sec + ":tenthofSec:" + tenthOfSec);
		}
		callData.set(CallDataAttribute.P_AIN_TN_CONNECT_TIME_MIN, min);
		callData.set(CallDataAttribute.P_AIN_TN_CONNECT_TIME_SEC, sec);
		callData.set(CallDataAttribute.P_AIN_TN_CONNECT_TIME_TENTH_SEC, tenthOfSec);

		//error buffer is present
		if(input.length-2 > GR533Constants.TN_TOTAL_LEN) { //as GR533Constants.TN_Total_len does not includes package Type Id & its length
			if(input[bufferIndex++] != GR533Constants.ERROR_CODES_ID) {//0xD4
				logger.error("parseTermintationNotification: Error Code Identifier Tag is not correct: "+ CommonUtils.formatBytes(input));
				throw new AINCodecException("parseTermintationNotification: Error Code Identifier Tag is not correct");
			}

			if(input[bufferIndex++] != GR533Constants.ERROR_CODES_LEN) {//0x01
				logger.error("parseTermintationNotification: The length of Error Code  ID must be 1 octet :  "+ CommonUtils.formatBytes(input));
				throw new AINCodecException("The length of Error Code  ID must be 1 octet ");
			}
			int errorCode = input[bufferIndex++];
			
			LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
			leg2Data.set(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER,"1");
			leg2Data.set(LegDataAttributes.P_APP_ERR_CODE,String.valueOf(errorCode));
		}
		if (logger.isDebugEnabled()) {
			logger.debug("exit parseTerminationNotification");
		}

	}

	/**
	 * Method encodes STR from AB to AI
	 * Caller Interaction
	 * Play Announcement
	 * B.4.3 Database Response-Announcement
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createGr533STR(CallData callData) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if(logger.isDebugEnabled()){
			logger.debug("Inside createGr533STR");
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
		resBuffer[index++] = GR533Constants.GR533_PLAY_ANN_TAG1;//0xF2
		resBuffer[index++] = GR533Constants.GR533_PLAY_ANN_LEN;//0x03
		resBuffer[index++] = GR533Constants.GR533_PLAY_ANN_TAG2;//0x82
		resBuffer[index++] = GR533Constants.GR533_PLAY_ANN_INDICATOR_LEN;//0x01
		resBuffer[index++] = (byte) getSs7AnnouncementId;
		
		if(logger.isDebugEnabled()){
			logger.debug("createGr533STR: Encoded Params: " + CommonUtils.formatBytes(resBuffer));
		}
 		return resBuffer;
	}

	/**
	 * This method is called by PH to send connectionControl.
	 * 
	 * @param tcapSession represents the instance of TcapSession
	 * @param action      represents the instance of Action.
	 * @throws Exception
	 */
	public static void sendConnectionControl(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendConnectionControl");
		}

		byte[] ar = null;
		ar = createConnectionControlTerm(callData);

		if (logger.isInfoEnabled()) {
			logger.info("CONNECTION CONTROL ::" + CommonUtils.formatBytes(ar));
		}
		byte[] connectionControlOpcode = CommonUtils.formatIntToByte(AinOpCodes.CONNECTION_CONTROL_BYTE);// 1025
		if(logger.isInfoEnabled()) {
			logger.info("connectionControlOpcode"+CommonUtils.formatBytes(connectionControlOpcode));
		}
		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_GLOBAL, connectionControlOpcode);
		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setLastInvokeEvent(true);
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, ar));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		connectInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		AinScfProtocolHelper.sendComponentReq(connectInvokeReqEvent, callData, cCallTraceService);
		
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		// Check if Application want to send ACG 
		// encode ACG
		// Send Component
		if(leg2Data.get(LegDataAttributes.SEND_ACG) != null){
			boolean isLastInvoke = (action.getSendMode() == Action.SEND_MODE.END) ? true : false;
			sendGr533Acg(tcapSession,callData, isLastInvoke);
		}

		// Check if application want to send Sent Notification
		// encode Send Notfication 
		// SendComponent 
		if (leg2Data.get(LegDataAttributes.NP_AIN_SEND_NOTIFICATION) != null){
			sendGR533SendNotification(tcapSession, callData);
		}
		
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.AIN_CALL_CONNECTED);
		AinScfProtocolHelper.preProcessDroppedCall(tcapSession);
		AinScfProtocolHelper.sendEndRequestEvent(tcapSession, false, cCallTraceService);
		AinScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
	}

	/**
	 * This method is called by the protocol handler for creating connection control
	 * response
	 * 
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createConnectionControlTerm(CallData callData) throws Exception {
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
		byte[] returnBuffer = null;
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectionControlTerm");
		}
		ConnectionControlArg connectionControlArg = new ConnectionControlArg();
		//if (leg2Data.get(LegDataAttributes.P_NETWORK_ROUTING_NUMBER) != null) {

		if (leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER) != null) {
			PhoneNumber routingNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);

			// encode ROuting Number
			Digits routingNumberDigits = new Digits();
			routingNumberDigits.setValue(encodeT1RoutingDigits(
					TypeOfDigitEnum.ROUTING_NUMBER, routingNumber.getAddress(),
					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.ISDN_NP, 0));

			connectionControlArg.setCarrierId(routingNumberDigits);

			if(logger.isDebugEnabled()){
				logger.debug("ConnectionControl: RoutingNumber:" +
						CommonUtils.formatBytes(routingNumberDigits.getValue()));
			}
		}else{
			// throw end error
			if(logger.isDebugEnabled()){
				logger.debug("ConnectionControl: RoutingNumber not set");
			}
		}

		// Encode Carrier ID type-8
		CarrierInfo ci = null;
		if (leg2Data.get(LegDataAttributes.P_CARRIER) != null) {
			ci = (CarrierInfo) leg2Data.get(LegDataAttributes.P_CARRIER);
		}else if(leg2Data.get(LegDataAttributes.PI_CARRIER) != null){
			String cic = (String) leg2Data.get(LegDataAttributes.PI_CARRIER);

			if(StringUtils.isNotBlank(cic)){
				ci = new CarrierInfo();
				ci.setAddress(cic);

				logger.debug("createConnectionControlTerm: CIC for PI_CARRIER is " + cic);
			}else{
				logger.debug("createConnectionControlTerm: CIC for PI_CARRIER is null");
			}
		}

		if(ci != null){
			Digits cic = new Digits();

			String cicDigits = ci.getAddress();
			cicDigits = StringUtils.rightPad(cicDigits, 4, '0');

			if(leg1Data.get(LegDataAttributes.P_PI_O_CIC_SUPPORT) != null){
				cicDigits = StringUtils.rightPad(cicDigits, 4, '0');
			}else{
				cicDigits = StringUtils.rightPad(cicDigits, 4, '0');
				cicDigits = cicDigits.substring(0, 4);
			}

			// as per T1.TRQ3 standard, if provide instruction contains CIC extension then 
			// we need to send 3 digits of carrier else 4 digits
			byte [] cicByte = encodeT1RoutingDigits(
					TypeOfDigitEnum.CARRIER_IDENTIFICATION, cicDigits,
					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.ISDN_NP, 0);

			if(leg1Data.get(LegDataAttributes.P_PI_O_CIC_SUPPORT) == null){
				int len = cicByte.length;
				cicByte[len-1] |= 0xF0;
			}

			cic.setValue(encodeT1RoutingDigits(
					TypeOfDigitEnum.CARRIER_IDENTIFICATION, cicDigits,
					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.ISDN_NP, 0));

			connectionControlArg.setNetworkRoutingNumber(cic);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting Carrier as:[" + ci + "], byte: " + CommonUtils.formatBytes(cic.getValue()));
			}
		}else{
			if(logger.isDebugEnabled()){
				logger.debug("ConnectionControl: Carrier not set");
			}			
		}

		// Encode BillingIndicator
		if (leg2Data.get(LegDataAttributes.PI_AMA_CALLTYPE) != null && 
				leg2Data.get(LegDataAttributes.PI_SERVICE_FEATURE_ID) != null) {

			String callTypeStr = (String)leg2Data.get(LegDataAttributes.PI_AMA_CALLTYPE);
			String srvFeatureStr = (String)leg2Data.get(LegDataAttributes.PI_SERVICE_FEATURE_ID);

			if(StringUtils.isNotBlank(callTypeStr)  && StringUtils.isNotBlank(srvFeatureStr)){
				// right pad with 0
				callTypeStr = StringUtils.rightPad(callTypeStr, 4, '0');
				srvFeatureStr = StringUtils.rightPad(srvFeatureStr, 4, '0');

				Digits billIndDigits = new Digits();
				billIndDigits.setValue(encodeT1RoutingDigits(
						TypeOfDigitEnum.BILLING_NUMBER, (callTypeStr+srvFeatureStr),
						NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.ISDN_NP, 0));

				if (logger.isDebugEnabled()) {
					logger.debug("Provide Instruction: Billing Num: " + CommonUtils.formatBytes(billIndDigits.getValue()));
				}

				connectionControlArg.setBillingIndicators(billIndDigits);
			}
		}else{
			if(logger.isDebugEnabled()){
				logger.debug("ConnectionControl: Billing number not set");
			}	
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.CONNECTION_CONTROL);
		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectionControlArg);
		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
		returnBuffer = encodeList.getFirst();
		//changing zero'th octet to 0xF2 for parameter set.
		returnBuffer[0] = (byte) 0xF2;
		//changing second octet to 0x84 for TCAP digits.
		returnBuffer[2] = (byte) 0x84;
		return returnBuffer;
	}


	/**
	 * Method used for sending GR533 ACG
	 * @param callData
	 * @param tcapSession
	 * @throws Exception 
	 */
	public static void sendGr533Acg(TcapSession tcapSession,CallData callData,boolean isLastInvoke) throws Exception{
		
		byte[] acgArgByte = createGr533Acg(callData);
		
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();
		
		byte[] acgOpCode = CommonUtils.formatIntToByte(AinOpCodes.ACG_BYTE);

		Operation operation = new Operation(Operation.OPERATIONTYPE_GLOBAL, acgOpCode);
		InvokeReqEvent invokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), operation);
		invokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		invokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, acgArgByte));
		invokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		invokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		if(isLastInvoke){
			invokeReqEvent.setLastInvokeEvent(true);
		}else{
			invokeReqEvent.setLastInvokeEvent(false);
		}

		AinScfProtocolHelper.sendComponentReq(invokeReqEvent, callData, cCallTraceService);
	}
	/**
	 * This method is for sending SEND NOTIFICATION component
	 * 
	 * @param tcapSession  represents the instance of TcapSession
	 * @param action       represents the instance of Action
	 * @param isInvokeLast
	 * @throws Exception
	 */
	private static void sendGR533SendNotification(TcapSession tcapSession, CallData callData) throws Exception {
	
		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendGR533SendNotification");
		}

		byte[] sendNotifyByte = createGr533SendNotification(callData);
		
		byte[] sendNotifOpCode = CommonUtils.formatIntToByte(AinOpCodes.SEND_NOTIFICATION_BYTE);

		Operation rrbeOperation = new Operation(Operation.OPERATIONTYPE_GLOBAL, sendNotifOpCode);

		InvokeReqEvent sendNotifInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), rrbeOperation);
		sendNotifInvokeReqEvent.setInvokeId(AinScfProtocolUtil.getNextInvokeId(callData));
		sendNotifInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, sendNotifyByte));
		sendNotifInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);
		sendNotifInvokeReqEvent.setLinkedId(AinScfProtocolUtil.getLastRxInvokeId(callData));
		//always be the last event
		sendNotifInvokeReqEvent.setLastInvokeEvent(true);

		AinScfProtocolHelper.sendComponentReq(sendNotifInvokeReqEvent, callData, cCallTraceService);
	}
	
	static byte[] encodeT1Digits(PhoneNumber ph) throws InvalidInputException {

		NatureOfNumEnum noa = NatureOfNumEnum.fromInt(ph.getNatureOfAddress());

		if (noa == null) {
			noa = NatureOfNumEnum.NATIONAL_NPR;
		}

		NumPlanEnum numberPlan = NumPlanEnum.fromInt(ph.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		T1Digits ainDigits = new T1Digits();
		ainDigits.setAddrSignal(ph.getAddress());
		ainDigits.setNoa(NatureOfNumEnum.fromInt(ph.getNatureOfAddress()));
		ainDigits.setNumPlanEnum(numberPlan);
		ainDigits.setEncodingSchemeEnum(EncodingSchemeEnum.BCD_EVEN);
		ainDigits.setNumOfDigits(ph.getAddress().length());
		return  ainDigits.encodeDigits();

	}
	
	/**
	 * This method encodes digits as per T.660 and T1.114 standard. 
	 * @param typeOfDig
	 * @param addressSig
	 * @param noa
	 * @param np
	 * @param presRestricted
	 * @return
	 * @throws InvalidInputException
	 */
	public static byte[] encodeT1RoutingDigits(TypeOfDigitEnum typeOfDig, String addressSig, 
			NatureOfNumEnum noa, NumPlanEnum np, int presRestricted) throws InvalidInputException {

		T1Digits ainDigits = new T1Digits();
		ainDigits.setTypeOfDigit(typeOfDig);
		ainDigits.setAddrSignal(addressSig);
		ainDigits.setNoa(noa);
		ainDigits.setNumPlanEnum(np);
		ainDigits.setPresentationRestricted(0);
		ainDigits.setEncodingSchemeEnum(EncodingSchemeEnum.BCD_ODD); // has to be 1 so setting BCD_ODD
		ainDigits.setNumOfDigits(addressSig.length());
		
		return  ainDigits.encodeDigits();

	}

	
	/**
	 * Method used for sending GR533 STR
	 * @param callData
	 * @param tcapSession
	 * @throws Exception 
	 */
	public static void sendGr533STR(TcapSession tcapSession,CallData callData,Action action) throws Exception{
		if (logger.isDebugEnabled()) {
			logger.debug("Inside sendGr533STR()");
		}
		//encoded STR
		byte[] strArgByte = createGr533STR(callData);
		
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
		
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		// Check if Application want to send ACG 
		// encode ACG
		// Send Component
		if(leg2Data !=null && leg2Data.get(LegDataAttributes.SEND_ACG) != null){
			boolean isLastInvoke = (action.getSendMode() == Action.SEND_MODE.END) ? true : false;
			sendGr533Acg(tcapSession,callData, isLastInvoke);
		}
		//end Dialogue		
		AinScfProtocolHelper.sendEndRequestEvent(tcapSession, false, cCallTraceService);		
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendGr533STR()");
		}
	}
	
	/**
	 * Method used to return error for Provide Instruction State
	 * @param tcapSession
	 * @param action
	 * @throws Exception
	 */
	public static void sendGR533ReturnError(TcapSession tcapSession, Action action) throws Exception {
		CallData callData = AinScfProtocolUtil.getCallData(tcapSession);
		int dialogId = tcapSession.getDialogueId();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + " [PH]:: Inside sendGR533ReturnError");
		}

		CallTraceService cCallTraceService = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getCallTraceService();

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
		int errorType = 0; // 0 - private, 1 - national 

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

		if(logger.isDebugEnabled()){
			logger.debug("sendGR533ReturnError: errortype:" + errorType);
		}
		
		//create appError
		byte[] appError = new byte[3];
		int index = 0;
		appError[index++] = GR533Constants.ERROR_CODES_ID;
		appError[index++] = GR533Constants.ERROR_CODES_LEN;
		appError[index++] = encodedErrorCode[0];
		

		ErrorReqEvent errorReqEvent = new ErrorReqEvent(src, tcapSession.getDialogueId(),
				(errorType == 0)?ComponentConstants.ERROR_LOCAL:ComponentConstants.ERROR_GLOBAL, encodedErrorCode);

		errorReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SET, appError));
		int linkId = AinScfProtocolUtil.getLastRxInvokeId(callData);
		errorReqEvent.setLinkId(linkId);
		AinScfProtocolHelper.sendComponentReq(errorReqEvent, callData, cCallTraceService);
		AinScfProtocolHelper.sendEndRequestEvent(tcapSession, false, cCallTraceService);

		action.setDropCallMode(DROP_CALL_MODE.BNS_APPLICATION_ERROR); //need to verify

		AinScfProtocolHelper.preProcessDroppedCall(tcapSession);

		AinScfProtocolHelper.postProcessDroppedCall(tcapSession, true);
		if (logger.isDebugEnabled()) {
			logger.debug("Exit sendGR533ReturnError");
		}
	}
}
