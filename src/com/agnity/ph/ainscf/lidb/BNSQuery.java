/****
Copyright (c) 2020 Agnity, Inc. All rights reserved.
This is proprietary source code of Agnity, Inc.

Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.ainscf.lidb;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.enumdata.CalledNatOfNumEnum;
import com.agnity.ain.enumdata.NumPlanEnum;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.util.Constant;
import com.agnity.ain.util.Util;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.ainscf.AinScfProtocolFieldCodec;
import com.agnity.ph.ainscf.AinScfProtocolParser;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;

import static com.agnity.ph.ainscf.lidb.BnsConstants.*;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.BitSet;

/**
 * @author rarya
 * @author stiwari
 * @author Vikas
 */
public class BNSQuery {

	private static Logger logger = Logger.getLogger(BNSQuery.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
	'f' };

	private BNSQuery() {

	}

	// Testing buffer for Bns Query decodeBuffer()
	/**
	 * byte[] testBuf = { (byte) 0xF2, //Parameter Set ID (byte) 0x26, //Parameter
	 * Set Length (byte) 0xDF, (byte)0x71, //BNS Info ID (byte) 0x00, //BNS Info
	 * Length (byte) 0xAA, //Service Key ID (byte) 0x21, //Service Key Length (byte)
	 * 0x84, //Digits Id [Billing Number] (byte) 0x09, //Digits Length (byte) 0x05,
	 * (byte) 0x00, (byte) 0x11, (byte) 0x0A, 0x21, 0x43, 0x65, (byte) 0x87, 0x09,
	 * //Digits (byte) 0x84, //Digits Id [Called Party] (byte) 0x09, //Digits Length
	 * (byte) 0x01, 0x00, (byte) 0x11, 0x0A, 0x18, 0x17, 0x21, 0x00, 0x76, //Digits
	 * (byte) 0x84, //Digits Id [Calling Party] (byte) 0x09, //Digits Length (byte)
	 * 0x02, 0x00, (byte) 0x11, 0x0A, 0x79, 0x12, 0x70, 0x76, 0x24 //Digits };
	 * 
	 * //BIT MANIPULATION LOGIC: int a = 4; int b= 0; int c = (a & 0x07)<<3; c |= (b
	 * & 0x07); System.out.println(c); // Testing buffer for encodeBns()
	 * 
	 * CallData callData = new CallData(); LegData legData = new LegData();
	 * callData.set(CallDataAttribute.P_LEG2, legData); LegData leg1Data = (LegData)
	 * callData.get(CallDataAttribute.P_LEG2);
	 * leg1Data.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.BNS);
	 * leg1Data.set(LegDataAttributes.P_BNS_M_COMPANY_ID,"1234");
	 * leg1Data.set(LegDataAttributes.P_BNS_M_RECORD_STATUS_INDICATOR,"1");
	 * leg1Data.set(LegDataAttributes.P_BNS_M_COLLECT_ACCEPTANCE_INDICATOR,"14");
	 * leg1Data.set(LegDataAttributes.P_BNS_M_THIRD_NUMBER_ACCEPTANCE_INDICATOR,
	 * "33" ); leg1Data.set(LegDataAttributes.P_BNS_M_TREATMENT_INDICATOR,"11");
	 * leg1Data.set(LegDataAttributes.P_BNS_M_SERVICE_OR_EQUIPMENT_INDICATOR,"61");
	 * leg1Data.set(LegDataAttributes.P_BNS_M_IC_INDICATOR_APIC,1);
	 * leg1Data.set(LegDataAttributes.P_BNS_M_IC_INDICATOR_PPIC,1);
	 * 
	 * leg1Data.set(LegDataAttributes.P_BNS_M_IC_INDICATOR_PINC,1);
	 * leg1Data.set(LegDataAttributes.P_BNS_M_INTERCEPT_INDICATOR,"1");
	 * 
	 * PhoneNumber preferedIC = new PhoneNumber(); preferedIC.setAddress("1233");
	 * preferedIC.setNumberingPlan(6); preferedIC.setNatureOfAddress(1); PhoneNumber
	 * altIC = new PhoneNumber(); altIC.setAddress("1233"); PhoneNumber prefINC =
	 * new PhoneNumber(); prefINC.setAddress("8171"); PhoneNumber refNum = new
	 * PhoneNumber(); refNum.setAddress("8171120061");
	 * 
	 * leg1Data.set(LegDataAttributes.P_BNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER,
	 * preferedIC );
	 * leg1Data.set(LegDataAttributes.P_BNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER,altIC);
	 * leg1Data.set(LegDataAttributes.P_BNS_O_PREF_INC_DIGIT_IDENTIFIER,prefINC);
	 * leg1Data.set(LegDataAttributes.P_BNS_O_REF_NUM,refNum);
	 * 
	 * leg1Data.set(LegDataAttributes.P_BNS_O_ACC_OWNER,"AAAAAAAA");
	 * leg1Data.set(LegDataAttributes.P_BNS_O_BILING_SER_PROV,"AAAAAAAA");
	 * 
	 * System.out.println(Util.formatBytes(encodeBnsQuery(callData)));
	 * 
	 * /** method decode the input buffer
	 * 
	 * @param callData
	 * @param input
	 * @throws InvalidInputException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void decodeBuffer(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {
		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:Enter::" + Util.formatBytes(input));
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.BNS.name());

		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:set P_LIDB_QUERY_TYPE == BNS");
		}
		// Parameter Set ID - Row P Table 8-19 GR 1149
		// (byte) 0xF2
		int currentIndex = 0;
		int currentElementIdLen = 1; // number of bytes of current element Identifier
		int currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[0] != (byte) 0xF2 && input[0] != (byte)0x31) {
			logger.error("decodeBuffer:parameter set ID is not correct. rxed:" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("parameter set ID is not correct");
		}
		// operation perform to set--> parameter set id

		// check for parameter set length - Row Q
		if (input.length <= BnsConstants.BNS_MANDATORY_LEN) {
			logger.error("decodeBuffer:input buffer can not be less then 28 at pos1 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new ParameterOutOfRangeException("Length of the buffer must be Greater then 28");
		}
		// operation perform to set -->parameter set length

		// check for BNS Info ID - Row R
		currentIndex = 2;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[2] != (byte) 0xDF || input[3] != (byte) 0x71) {
			logger.error("decodeBuffer:BNS info id is not correct at pos2 & 3 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("BNS info id is not correct");
		}
		// operation to perform to set--->BNS Info ID

		// check for BNS Info length. - Row S
		if (input[4] != (byte) 0x00) {
			logger.error("decodeBuffer:BNS info length should be zero at pos4. " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("BNS info length should be zero");
		}
		// operation to perform on--->BNS Info length.

		// check for Service key ID - Row T
		currentIndex = 5;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[5] != (byte) 0xAA) {
			logger.error("decodeBuffer:Service key ID is not correct at pos5 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Service key ID is not correct ");
		}

		// From GR: Service Key Length - The service key has a length of 6 octets plus
		// the length of the
		// called number digits field.
		int serviceKeyLen = input[6] & 0xFF;
		//		if (serviceKeyLen < 6 || serviceKeyLen > 21) {
		//			logger.error("decodeBuffer:Service key Length is not correct at pos 6 " + CommonUtils.formatBytes(input));
		//			throw new AINCodecException("Service key Length is not correct ");
		//		}

		// check for Digits Id [Billing Number] - Row V
		currentIndex = 7;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[7] != (byte) 0x84) {
			logger.error(
					"decodeBuffer:Digits Id [Billing Number] is not correct as pos7 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Id [Billing Number] is not correct ");
		}

		// operation to perform ont --->Digits Id [Billing Number]

		// check for Digits length. - Row W
		if (input[8] != (byte) 0x09) {
			logger.error("decodeBuffer:Digits length[Billing Number] should be correct at pos8. "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits  length[Billing Number] should be correct.");
		}
		// operation to perform on --> digits length.

		// check for digits - Row X
		if (input[9] != (byte) 0x05 || input[10] != (byte) 0x00 || input[12] != (byte) 0x0A) {
			logger.error("decodeBuffer:Digits[Billing Number] should be correct at pos 9, 10, 11, 12 "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits[Billing Number] should be correct.");
		}

		int billingNumDigitLen = input[8] & 0xFF;// it will always be 9
		if (billingNumDigitLen != 9) {
			logger.error("decodeBuffer:billingNumDigitLen should be correct at pos 8" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("billingNumDigitLen should be correct.");
		}

		/*
		 * format for digits: 0x05 0x00 0x11 0x0A [5 octet BCD Format 10 digits]
		 */
		AinDigits ainDigitForBillingNum = new AinDigits();
		// Creating format for AIN digit decoder
		int arraysize = 5; // 5 octet BCD format 10 digits
		byte[] inputforDecodeAin = new byte[arraysize];
		int natureOfBns = input[10] & 0xFF;
		for (int i = 0; i < inputforDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputforDecodeAin[i] = input[13 + i];
		}
		ainDigitForBillingNum.setAddrSignal(decodeAdrsSignalForBns(inputforDecodeAin, 0, 0));
		PhoneNumber bilingNumber = AinScfProtocolParser.parseAinDigits(ainDigitForBillingNum, natureOfBns);
		legData.set(LegDataAttributes.BNS_BILLING_NUM, bilingNumber);

		// check for Digits Id [Called Party] - Row Y
		currentIndex = 18;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[18] != (byte) 0x84) {
			logger.error("decodeBuffer:Digits Id [Called Party] should be correct as pos18"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Id [Called Party] should be correct.");
		}
		// operation to perform on Digits Id [Called Party]
		// get the Digits[Called Party] Length - Row Z
		int cldPartyDigitLen = input[19] & 0xFF;
		/**
		 * format for Digits[Called Party]--> d0x01 [NOA] 0x11 [Num of Dig] [BCD Format]
		 * [Num of Dig] 10 - National Max 15 - International check for Digit[Called
		 * Party] Row AA
		 */
		if (input[20] != (byte) 0x01) {
			logger.error(
					"decodeBuffer:Digit[Called Party] should be correct at pos20" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] should be correct.");
		}

		// check for Digit[Called Party] -->NOA - Row AA (2)
		if (input[21] != (byte) 0x00 && input[21] != (byte) 0x01 && input[21] != (byte) 0x02
				&& input[21] != (byte) 0x03) {
			logger.error("decodeBuffer:Digit[Called Party] -->NOA should be correct at pos 21 "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] -->NOA should be correct.");
		}

		// Pos 22 skipped- Should be BCD

		// num digit length
		int numdigilen = input[23] & 0xFF;
		if (numdigilen > 15 || numdigilen < 10) {
			logger.error("decodeBuffer:Digit[Called Party] -->num digit length should be btw 10 to 15 at pos 23 "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] -->num digit length should be correct.");
		}

		// set called party digit
		AinDigits aindigitcalled = new AinDigits();
		// Creating format for AIN digit decoder
		int arrsize;
		if (numdigilen % 2 == 0) {
			arrsize = (numdigilen / 2);
		} else {
			arrsize = (numdigilen + 1) / 2;
		}
		byte[] inputfrDecodeAin = new byte[arrsize + 2];
		inputfrDecodeAin[0] = input[21]; // adding NOA-->called
		for (int i = 1; i < inputfrDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAin[i] = input[22 + i];
		}
		// getting called party num
		AinDigits calledPartyNum = aindigitcalled.decodeAinDigits(inputfrDecodeAin, Constant.CALLED);

		// perform operation calledPartyNum
		PhoneNumber calledNumber = AinScfProtocolParser.parseCalledPartyNum(calledPartyNum);
		legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);

		// check for Digits Id [Calling Party] - Row AB
		currentIndex = arrsize + 24;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[arrsize + 24] != (byte) 0x84) {
			logger.error("decodeBuffer:Digits Id [Calling Party] should be 0x84" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Id [Calling Party] should be correct.");
		}

		// [Calling Party] -->Digits Length - Row AC
		int clgPartyDigitLen = input[arrsize + 25] & 0xFF;

		// check for [Calling Party] -->Digits format --> 0x02 [NOA] 0x11 [Num of Dig]
		// [BCD Format] - Row AD
		if (input[arrsize + 26] != (byte) 0x02) {
			logger.error("decodeBuffer:Digit[Calling Party] should be 0x02 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Calling Party] should be correct.");
		}

		// check for Digit[Called Party] -->NOA
		if (input[arrsize + 27] != (byte) 0x00 && input[arrsize + 27] != (byte) 0x01
				&& input[arrsize + 27] != (byte) 0x02 && input[arrsize + 27] != (byte) 0x03) {
			logger.error("decodeBuffer:Digit[Calling Party] -->NOA should be correct at pos27"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Calling Party] -->NOA should be correct.");
		}

		// num digit length
		int numdigilenclg = input[arrsize + 29] & 0xFF;

		if (numdigilenclg > 15 || numdigilenclg < 10) {
			logger.error("decodeBuffer:Digit[Calling Party] -->num digit length should be between 10 and 15 "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Calling Party] --> number digit length should be correct.");
		}
		// set calling party digit
		AinDigits aindigitcalling = new AinDigits();
		// Creating format for AIN digit decoder
		int arrsizeclg;
		if (numdigilenclg % 2 == 0) {
			arrsizeclg = (numdigilenclg / 2);
		} else {
			arrsizeclg = (numdigilenclg + 1) / 2;
		}
		byte[] inputfrDecodeAinclg = new byte[arrsizeclg + 2];
		inputfrDecodeAinclg[0] = input[arrsize + 27]; // adding NOA-->called
		for (int i = 1; i < inputfrDecodeAinclg.length; i++) {
			// adding number of digit and bcd format of digit
			inputfrDecodeAinclg[i] = input[arrsize + 28 + i];
		}
		// getting calling party num
		AinDigits callingPartyNum = aindigitcalling.decodeAinDigits(inputfrDecodeAinclg, Constant.CALLING);

		// perform operation callingPartyNum
		PhoneNumber callingNumber = AinScfProtocolParser.parseCallingPartyNum(callingPartyNum);
		legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);

		//Checking for Primary/secondary Service Request optional fields
		currentIndex = arrsize + 29 + arrsizeclg + 1;
		if (input.length >= (currentIndex + 8) && input[currentIndex] == (byte) 0xDF){ // 8 is the total length of PSRID (PSR identifier + psrid param)
			currentElementIdLen = 3; // number of bytes of current element Identifier
			currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

			int inputIndex = currentIndex;
			// primary Service Requester ID. Note: the correct bytes details for PSRID is not given in gr 1149. The values used here is in line with DTSRID identifier with private tcap id 165 (Hex Dx C1 25). So tcap id 163 of PSRID is converted to (Hex Dx C1 23)
			if (input[inputIndex] == (byte) 0xDF && input[inputIndex + 1] == (byte) 0xC1 && input[inputIndex + 2] == (byte) 0x23) {
				logger.info("decodeBuffer:primary Service Requester ID is present in the request");
				inputIndex = inputIndex + 3;
				// primary Service Requester ID Length
				if (input[inputIndex++] != (byte) 0x05) {
					logger.info("decodeBuffer:throwing exception ");
					logger.error("decodeBuffer:primary Service Requester ID Length is not correct");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new ParameterOutOfRangeException("primary Service Requester ID Length is not correct");
				}

				int psridContext = input[inputIndex++] & 0xFF;
				if (psridContext != 0x09 && psridContext != 0x0A) {
					logger.info("decodeBuffer:throwing exception ");
					logger.error("decodeBuffer:primary Service Requester ID spare is not correct");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("primary Service Requester ID spare is not correct");
				}

				int psridType = psridContext & 0x07; // extract lower 3 digits
				legData.set(LegDataAttributes.BNS_O_PSRID_TYPE, psridType);
				byte[] primaryServiceRequesterIdtable = { input[inputIndex++], input[inputIndex++], input[inputIndex++], input[inputIndex++] };
				String primaryServiceRequesterId = new String(primaryServiceRequesterIdtable);
				legData.set(LegDataAttributes.BNS_O_PRIMARY_SERVICE_REQUESTER, primaryServiceRequesterId);

				currentIndex = inputIndex;
				// Checking if SSRID is present after PSRID
				if (input.length > currentIndex && primaryServiceRequesterId != null) {
					logger.info("decodeBuffer:secondary Service Requester ID is present in the request");
					inputIndex = inputIndex + 3;
					// secondary Service Requester ID Length
					if (input[inputIndex++] != (byte) 0x05) {
						logger.info("decodeBuffer:throwing exception ");
						logger.error("decodeBuffer:secondary Service Requester ID Length is not correct");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new ParameterOutOfRangeException("secondary Service Requester ID Length is not correct");
					}

					int ssridContext = input[inputIndex++] & 0xFF;
					if (ssridContext != 0x09 && ssridContext != 0x0A) {
						logger.info("decodeBuffer:throwing exception ");
						logger.error("decodeBuffer:secondary Service Requester ID spare is not correct");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("secondary Service Requester ID spare is not correct");
					}

					int ssridType = ssridContext & 0x07; // extract lower 3 digits
					legData.set(LegDataAttributes.BNS_O_SSRID_TYPE, ssridType);
					byte[] secondaryServiceRequesterIdtable = { input[inputIndex++], input[inputIndex++], input[inputIndex++], input[inputIndex++] };
					String secondaryServiceRequesterId = new String(secondaryServiceRequesterIdtable);
					legData.set(LegDataAttributes.BNS_O_SECONDARY_SERVICE_REQUESTER, secondaryServiceRequesterId);
				}

			} else {
				logger.info("decodeBuffer:secondary Service Requester ID is present in the request");
				inputIndex = inputIndex + 3;
				// secondary Service Requester ID Length
				if (input[inputIndex++] != (byte) 0x05) {
					logger.info("decodeBuffer:throwing exception ");
					logger.error("decodeBuffer:secondary Service Requester ID Length is not correct");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new ParameterOutOfRangeException("secondary Service Requester ID Length is not correct");
				}

				int ssridContext = input[inputIndex++] & 0xFF;
				if (ssridContext != 0x09 && ssridContext != 0x0A) {
					logger.info("decodeBuffer:throwing exception ");
					logger.error("decodeBuffer:secondary Service Requester ID spare is not correct");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("secondary Service Requester ID spare is not correct");
				}

				int ssridType = ssridContext & 0x07; // extract lower 3 digits
				legData.set(LegDataAttributes.BNS_O_SSRID_TYPE, ssridType);
				byte[] secondaryServiceRequesterIdtable = { input[inputIndex++], input[inputIndex++], input[inputIndex++], input[inputIndex++] };
				String secondaryServiceRequesterId = new String(secondaryServiceRequesterIdtable);
				legData.set(LegDataAttributes.BNS_O_SECONDARY_SERVICE_REQUESTER, secondaryServiceRequesterId);

			}


		} else if (input.length > (currentIndex) && input.length < (currentIndex + 8)) {
			currentElementIdLen = 0; // number of bytes of current element Identifier (unknown)
			currentElementLen = input[input.length - currentIndex ] & 0xFF;
			logger.info("decodeBuffer:throwing exception ");
			logger.error("decodeBuffer:Invalid parameter received, expected PSRID or SSRID");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Invalid parameter received, expected PSRID or SSRID");			
		}

		if (logger.isDebugEnabled()) {
			logger.debug("decode:Exit");
		}
	}

	/**
	 * method to encode the BNS Query
	 * 
	 * @param callData
	 * @return byte[]
	 * @throws InvalidInputException, AINCodecException
	 */
	public static byte[] encodeBnsQuery(CallData callData) throws InvalidInputException, AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeBnsQuery:Enter");
		}

		// encode and set the data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int index = 0;
		// total length of output array
		int totalLength = 0;
		// totalLength += BnsConstants.BNS_PARAM_SET_ID_LEN;

		// company id should be long value in string format
		// Row O, P Q
		if (leg2Data.get(LegDataAttributes.P_BNS_M_COMPANY_ID) == null) {
			logger.error("encodeBnsQuery:parameter companyId is Mandatory");
			throw new AINCodecException("parameter companyId is Mandatory");
		}

		String bnsCompId = (String) leg2Data.get(LegDataAttributes.P_BNS_M_COMPANY_ID);
		//byte[] companyId = decimalToBcdForBns(bnsCompId);
		byte[] companyId = AinScfProtocolFieldCodec.encodeDigitsInBcdFormat(bnsCompId);
		if (companyId.length != 2) {
			logger.error("encodeBnsQuery : company id length should be 2");
			throw new AINCodecException("company id length should be 2");
		}

		// increment length by 5
		totalLength += BnsConstants.BNS_COMPANY_ID_LEN;

		// Record Status Indicator, Row R, S, T
		if (leg2Data.get(LegDataAttributes.P_BNS_M_RECORD_STATUS_INDICATOR) == null) {
			logger.error("encodeBnsQuery:parameter record status indicator is Mandatory");
			throw new AINCodecException("parameter record status indicator is Mandatory");
		}

		String recStatInd = (String) leg2Data.get(LegDataAttributes.P_BNS_M_RECORD_STATUS_INDICATOR);
		int recStatIndInt = Integer.parseInt(recStatInd);
		//byte[] encodedRecStatInd = asciToHex(recStatInd);
		byte[] encodedRecStatInd = CommonUtils.formatIntToByte(recStatIndInt);
		if (encodedRecStatInd.length != 1) {
			logger.info("encodeBnsQuery:throwing exception ");
			logger.error("encodeBnsQuery:parameter record status indicator length should be 1");
			throw new AINCodecException("parameter record status indicatorlength should be 1");
		}

		// Increment length by 4
		totalLength += BnsConstants.BNS_REC_STATUS_IND_LEN;

		// Collect Acceptance Indicator - Row U, V, W
		if (leg2Data.get(LegDataAttributes.P_BNS_M_COLLECT_ACCEPTANCE_INDICATOR) == null) {
			logger.error("encodeBnsQuery:parameter collect status indicator is Mandatory");
			throw new AINCodecException("parameter collect status indicator is Mandatory");
		}

		String collectAcceptenceIndicator = (String) leg2Data
				.get(LegDataAttributes.P_BNS_M_COLLECT_ACCEPTANCE_INDICATOR);
		int collectAcceptenceIndInt = Integer.parseInt(collectAcceptenceIndicator);
		//byte[] encodeCollAccInd = asciToHex(collectAcceptenceIndicator);
		byte[] encodeCollAccInd = CommonUtils.formatIntToByte(collectAcceptenceIndInt);
		totalLength += BnsConstants.BNS_COL_ACPT_IND_LEN;

		// Third Number Acceptance Indicator - Row X, Y, Z
		if (leg2Data.get(LegDataAttributes.P_BNS_M_THIRD_NUMBER_ACCEPTANCE_INDICATOR) == null) {
			logger.error("encodeBnsQuery:parameter third number acceptence indicator is Mandatory");
			throw new AINCodecException("parameter third number acceptence indicator is Mandatory");
		}

		String thrdNoAccInd = (String) leg2Data.get(LegDataAttributes.P_BNS_M_THIRD_NUMBER_ACCEPTANCE_INDICATOR);
		int thrdNoAccIndInt = Integer.parseInt(thrdNoAccInd);
		byte[] encodedthrdNoAccInd = CommonUtils.formatIntToByte(thrdNoAccIndInt);
		//byte[] encodedthrdNoAccInd = asciToHex(thrdNoAccInd);
		totalLength += BnsConstants.BNS_THRD_NUM_ACPT_IND_LEN;

		// Treatment Indicator, Row AA, AB, AC
		if (leg2Data.get(LegDataAttributes.P_BNS_M_TREATMENT_INDICATOR) == null) {
			logger.error("encodeBnsQuery:parameter treatment indicator is Mandatory");
			throw new AINCodecException("parameter treatment indicator is Mandatory");
		}

		String trtmntInd = (String) leg2Data.get(LegDataAttributes.P_BNS_M_TREATMENT_INDICATOR);
		int trtmntIndInt = Integer.parseInt(trtmntInd);
		byte[] encodedTrtmntInd = CommonUtils.formatIntToByte(trtmntIndInt);
		//byte[] encodedTrtmntInd = asciToHex(trtmntInd);
		totalLength += BnsConstants.BNS_TRTMNT_IND_LEN;

		// Service or equipment indicator, Row AD, AE, AF
		if (leg2Data.get(LegDataAttributes.P_BNS_M_SERVICE_OR_EQUIPMENT_INDICATOR) == null) {
			logger.error("encodeBnsQuery:parameter service or equipment indicator is Mandatory");
			throw new AINCodecException("parameter service or equipment indicator is Mandatory");
		}

		String serOrEquInd = (String) leg2Data.get(LegDataAttributes.P_BNS_M_SERVICE_OR_EQUIPMENT_INDICATOR);
		int serOrEquIndInt = Integer.parseInt(serOrEquInd);
		byte[] encodedSerOrEquInd = CommonUtils.formatIntToByte(serOrEquIndInt);
		//byte[] encodedSerOrEquInd = asciToHex(serOrEquInd);
		totalLength += BnsConstants.BNS_SER_EQP_IND_LEN;

		// Intercept Indicator, Row AG, AH, AI
		if (leg2Data.get(LegDataAttributes.P_BNS_M_INTERCEPT_INDICATOR) == null) {
			logger.error("encodeBnsQuery:parameter intercept indicator is Mandatory");
			throw new AINCodecException("parameter intercept indicator is Mandatory");
		}

		String inrcptInd = (String) leg2Data.get(LegDataAttributes.P_BNS_M_INTERCEPT_INDICATOR);
		int inrcptIndInt = Integer.parseInt(inrcptInd);
		byte[] encodedinrcptInd = CommonUtils.formatIntToByte(inrcptIndInt);
		//byte[] encodedinrcptInd = asciToHex(inrcptInd);
		totalLength += BnsConstants.BNS_INTERCPT_IND_LEN;

		// RAO Digit Identifier, Row AJ, AK, AL (Optional)
		String raoDigit = (String) leg2Data.get(LegDataAttributes.P_BNS_O_RAO_DIGIT);

		// RAO digit length has to be 3
		byte[] digitIdentifier = null;
		if (raoDigit != null && raoDigit.length() == 3) {
			digitIdentifier = encodeAdrsSignalForBns(raoDigit);
			totalLength += BnsConstants.BNS_O_DIGIT_IDENTIFIER_LEN;
		}

		// IC Indicator ,Row -AM, AN, AO
		if (leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_APIC) == null
				|| leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_PPIC) == null
				|| leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_PINC) == null) {
			logger.error(
					"encodeBnsQuery:parameter ic indicators (APIC, PPIC & PINC) is Mandatory..found NULL setting to 0");
			// throw new AINCodecException("parameter ic indicators (APIC, PPIC & PINC) is
			// Mandatory");
		}
		totalLength += BnsConstants.BNS_IC_INDICATOR_LEN;
		int icIndicatorAPIC = leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_APIC) == null ? 0
				: (Integer) leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_APIC);
		int icIndicatorPPIC = leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_PPIC) == null ? 0
				: (Integer) leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_PPIC);
		int icIndicatorAPIC_PPIC = (icIndicatorAPIC & 0x07) << 3;
		icIndicatorAPIC_PPIC |= (icIndicatorPPIC & 0x07);

		int icIndicatorPINC = leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_PINC) == null ? 0
				: (Integer) leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_PINC);

		byte[] byteIcIndicator = { (byte) icIndicatorAPIC_PPIC, (byte) icIndicatorPINC };

		// Preferred IC Digits ,Row AP, AQ, AR
		PhoneNumber primaryPreferedIcDigits = null;

		if (leg2Data.get(LegDataAttributes.P_BNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER) != null){
			primaryPreferedIcDigits = (PhoneNumber) leg2Data
					.get(LegDataAttributes.P_BNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER);

			if(!StringUtils.isNotBlank(primaryPreferedIcDigits.getAddress())){
				primaryPreferedIcDigits = null;
			}	
		}

		// Alternate IC Digits Row AS, AT, AU
		PhoneNumber altPrefIcDigits = null;
		if (leg2Data.get(LegDataAttributes.P_BNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER) != null){
			altPrefIcDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_BNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER);

			if(!StringUtils.isNotBlank(altPrefIcDigits.getAddress())){
				altPrefIcDigits = null;
			}	
		}

		// Preferred INC Digits Row AV, AW, AX
		PhoneNumber prefIncDigits = null;
		if (leg2Data.get(LegDataAttributes.P_BNS_O_PREF_INC_DIGIT_IDENTIFIER) != null){
			prefIncDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_BNS_O_PREF_INC_DIGIT_IDENTIFIER);

			if(!StringUtils.isNotBlank(prefIncDigits.getAddress())){
				prefIncDigits = null;
			}
		}

		// Reference Number Row AY, AZ, BA
		PhoneNumber refNum = null;
		if (leg2Data.get(LegDataAttributes.P_BNS_O_REF_NUM) != null) {
			refNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_BNS_O_REF_NUM);

			if(!StringUtils.isNotBlank(refNum.getAddress())){
				refNum = null;
			}else{
				// as per standard 
				// National presentation allowed - 0
				// International presentation allowed - 1
				if (leg2Data.get(LegDataAttributes.P_BNS_O_REF_NUM_TYPE) == null) {
					logger.error("encodeBnsQuery:Referral number type is missing");
					throw new AINCodecException("parameter Referral number type is missing");
				}
				int refNumType = (Integer) leg2Data.get(LegDataAttributes.P_BNS_O_REF_NUM_TYPE);
				refNum.setNatureOfAddress(refNumType);
			}
		}

		byte[] accOwnerEncoded = null;
		byte[] billingServiceProviderEncoded = null;

		/// Account Owner Identifier Row- BB, BC, BD
		String accOwnerIdentifier = null;
		if (leg2Data.get(LegDataAttributes.P_BNS_O_ACC_OWNER) != null) {
			accOwnerIdentifier = (String) leg2Data.get(LegDataAttributes.P_BNS_O_ACC_OWNER);
			if(logger.isDebugEnabled()){
				logger.debug("accOwnerIdentifier value::"+accOwnerIdentifier);
			}

			if(!StringUtils.isNotBlank(accOwnerIdentifier)){
				accOwnerIdentifier = null;
			}
		}

		if (accOwnerIdentifier != null && !accOwnerIdentifier.isEmpty()) {
			accOwnerEncoded = encodeIa5(accOwnerIdentifier);
			logger.debug("accOwnerIdentifier::"+accOwnerIdentifier+" encoded as::"+accOwnerEncoded);
			totalLength += BnsConstants.BNS_ACC_OWNER_LEN;
		}
		// Billing Service Provider Row- BE, BF, BG
		String billingServiceProvider = null;

		if (leg2Data.get(LegDataAttributes.P_BNS_O_BILING_SER_PROV) != null){
			billingServiceProvider = (String) leg2Data.get(LegDataAttributes.P_BNS_O_BILING_SER_PROV);

			if(!StringUtils.isNotBlank(billingServiceProvider)){
				billingServiceProvider = null;
			}
		}

		if (billingServiceProvider != null && !billingServiceProvider.isEmpty()) {
			billingServiceProviderEncoded = encodeIa5(billingServiceProvider);
			totalLength += BnsConstants.BNS_BILLING_SER_PROV_LEN;
		}

		byte encodedPrimaryPreferedIcDigitsNumDigit = 0;
		byte encodedAltPrefIcDigitsNumDigit = 0;
		byte encodedPrefIncDigitsNumDigit = 0;
		byte encodedRefNumNumDigit = 0;
		byte[] encodedPrimaryPreferedIcDigits = null;
		byte[] encodedAltPrefIcDigits = null;
		byte[] encodedPrefIncDigits = null;
		byte[] encodedRefNum = null;

		if (primaryPreferedIcDigits != null) {
			encodedPrimaryPreferedIcDigits = encodeAdrsSignalForBns(primaryPreferedIcDigits.getAddress());
			encodedPrimaryPreferedIcDigitsNumDigit = (byte) encodedPrimaryPreferedIcDigits.length;
			totalLength += BnsConstants.BNS_PRIM_PREF_IC_LEN;
		}
		if (altPrefIcDigits != null) {
			encodedAltPrefIcDigits = encodeAdrsSignalForBns(altPrefIcDigits.getAddress());
			encodedAltPrefIcDigitsNumDigit = (byte) encodedAltPrefIcDigits.length;
			totalLength += BnsConstants.BNS_ALT_PREF_IC_LEN;
		}
		if (prefIncDigits != null) {
			encodedPrefIncDigits = encodeAdrsSignalForBns(prefIncDigits.getAddress());
			encodedPrefIncDigitsNumDigit = (byte) encodedPrefIncDigits.length;
			totalLength += BnsConstants.BNS_PREF_INC_LEN;
		}
		if (refNum != null) {
			encodedRefNum = encodeAdrsSignalForBns(refNum.getAddress());
			encodedRefNumNumDigit = (byte) encodedRefNum.length;
			totalLength += encodedRefNum.length;
			totalLength += BnsConstants.BNS_REF_DIFIT_IDENTIFIER_LEN;
		}

		String[] parameterSetLength = convertHexRepersentaion(Integer.toHexString(totalLength));
		byte[] outputArray = new byte[totalLength + 2];

		outputArray[index++] = PARAMETER_SET_ID;
		outputArray[index++] = (byte) Byte.decode(parameterSetLength[0]);
		outputArray[index++] = COMPANY_ID_IDENTIFIER_TAG1;
		outputArray[index++] = COMPANY_ID_IDENTIFIER_TAG2;
		outputArray[index++] = COMPANY_ID_LENGTH;
		for (byte encodedVal : companyId) {
			outputArray[index++] = encodedVal;
		}
		outputArray[index++] = RECORD_STATUS_INDICATOR_ID_TAG1;
		outputArray[index++] = RECORD_STATUS_INDICATOR_ID_TAG2;
		outputArray[index++] = RECORD_STATUS_INDICATOR_ID_LENGTH;
		for (byte encodedVal : encodedRecStatInd) {
			outputArray[index++] = encodedVal;
		}
		// Collect Acceptance Indicator - Row U, V, W
		outputArray[index++] = COLLECT_ACCEPTANCE_INDICATOR_ID_TAG1;
		outputArray[index++] = COLLECT_ACCEPTANCE_INDICATOR_ID_TAG2;
		outputArray[index++] = COLLECT_ACCEPTANCE_INDICATOR_LENGTH;

		for (byte encodedVal : encodeCollAccInd) {
			outputArray[index++] = encodedVal;
		}
		// Third Number Acceptance Indicator - Row X, Y, Z
		outputArray[index++] = THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_TAG1;
		outputArray[index++] = THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_TAG2;
		outputArray[index++] = THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_LENGTH;
		for (byte encodedVal : encodedthrdNoAccInd) {
			outputArray[index++] = encodedVal;
		}
		// Treatment Indicator, Row AA, AB, AC
		outputArray[index++] = TREATMENT_INDICATOR_ID_TAG1;
		outputArray[index++] = TREATMENT_INDICATOR_ID_TAG2;
		outputArray[index++] = TREATMENT_INDICATOR_LENGTH;
		for (byte encodedVal : encodedTrtmntInd) {

			outputArray[index++] = encodedVal;
		}
		// Service or equipment indicator, Row AD, AE, AF
		outputArray[index++] = SERVICE_OR_EQUIPMENT_INDICATOR_ID_TAG1;
		outputArray[index++] = SERVICE_OR_EQUIPMENT_INDICATOR_ID_TAG2;
		outputArray[index++] = SERVICE_OR_EQUIPMENT_INDICATOR_LENGTH;
		for (byte encodedVal : encodedSerOrEquInd) {
			outputArray[index++] = encodedVal;
		}
		outputArray[index++] = INTERCEPT_INDICATOR_ID_TAG1;
		outputArray[index++] = INTERCEPT_INDICATOR_ID_TAG2;
		outputArray[index++] = INTERCEPT_INDICATOR_LENGTH;
		for (byte encodedVal : encodedinrcptInd) {
			outputArray[index++] = encodedVal;
		}
		// RAO Digit Identifier, Row AJ, AK, AL (Optional)
		if (digitIdentifier != null) {
			outputArray[index++] = DIGITS_IDENTIFIER_TAG1;
			outputArray[index++] = DIGITS_IDENTIFIER_TAG2;
			outputArray[index++] = DIGITS_IDENTIFIER_LENGTH;
			outputArray[index++] = 0x16;
			outputArray[index++] = 0x00;
			outputArray[index++] = 0x01;
			outputArray[index++] = 0x03;
			for (byte encodedVal : digitIdentifier) {
				outputArray[index++] = encodedVal;
			}
		}
		// IC Indicator ,Row -AM, AN, AO
		if (byteIcIndicator != null) {
			outputArray[index++] = IC_INDICATOR_ID_TAG1;
			outputArray[index++] = IC_INDICATOR_ID_TAG2;
			outputArray[index++] = IC_INDICATOR_LENGTH;
			for (byte encodedVal : byteIcIndicator) {
				outputArray[index++] = encodedVal;
			}
		}
		// Preferred IC Digits ,Row AP, AQ, AR
		if (encodedPrimaryPreferedIcDigits != null) {
			outputArray[index++] = DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_TAG1;
			outputArray[index++] = DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_TAG2;
			outputArray[index++] = PRIMARY_PREFERRED_IC_DIGITS_IDENTIFIER_LENGTH;
			outputArray[index++] = (byte) 0x0D;
			outputArray[index++] = DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_NOA;
			outputArray[index++] = (byte) 0x01;
			outputArray[index++] = encodedPrimaryPreferedIcDigitsNumDigit;
			for (byte encodedVal : encodedPrimaryPreferedIcDigits) {
				outputArray[index++] = encodedVal;
			}
		}
		// Alternate IC Digits Row AS, AT, AU
		if (encodedAltPrefIcDigits != null) {
			outputArray[index++] = DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_TAG1;
			outputArray[index++] = DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_TAG2;
			outputArray[index++] = ALTERNATE_PREFERRED_IC_DIGITS_IDENTIFIER_LENGTH;
			outputArray[index++] = (byte) 0x0B;
			outputArray[index++] = DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_NOA;
			outputArray[index++] = (byte) 0x01;
			outputArray[index++] = encodedAltPrefIcDigitsNumDigit;
			for (byte encodedVal : encodedAltPrefIcDigits) {
				outputArray[index++] = encodedVal;
			}
		}
		// Preferred INC Digits Row AV, AW, AX
		if (encodedPrefIncDigits != null) {
			outputArray[index++] = DIGITS_IDENTIFIER_ID_PREFERRED_INC_TAG1;
			outputArray[index++] = DIGITS_IDENTIFIER_ID_PREFERRED_INC_TAG2;
			outputArray[index++] = PREFERRED_INC_DIGITS_IDENTIFIER_LENGTH;
			outputArray[index++] = (byte) 0x0C;
			outputArray[index++] = DIGITS_IDENTIFIER_ID_PREFERRED_INC_NOA;
			outputArray[index++] = (byte) 0x01;
			outputArray[index++] = encodedPrefIncDigitsNumDigit;
			for (byte encodedVal : encodedPrefIncDigits) {
				outputArray[index++] = encodedVal;
			}
		}

		// Reference Number Row AY, AZ, BA
		if (encodedRefNum != null) {
			outputArray[index++] = DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG1;
			outputArray[index++] = DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG2;
			outputArray[index++] = (byte) (4 + encodedRefNumNumDigit);
			outputArray[index++] = (byte) 0x09;
			outputArray[index++] = (byte) refNum.getNatureOfAddress();
			outputArray[index++] = (byte) 0x11;
			outputArray[index++] = (byte) refNum.getAddress().length();
			for (byte encodedVal : encodedRefNum) {
				outputArray[index++] = encodedVal;
			}
		}
		/// Account Owner Identifier Row- BB, BC, BD
		if (accOwnerEncoded != null) {
			outputArray[index++] = (byte) 0xDF;
			outputArray[index++] = (byte) 0xC1;
			outputArray[index++] = (byte) 0x02;
			outputArray[index++] = (byte) 0x04;
			for (byte encodedVal : accOwnerEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// Billing Service Provider Row- BE, BF, BG
		if (billingServiceProviderEncoded != null) {
			outputArray[index++] = (byte) 0xDF;
			outputArray[index++] = (byte) 0xC1;
			outputArray[index++] = (byte) 0x03;
			outputArray[index++] = (byte) 0x04;
			for (byte encodedVal : billingServiceProviderEncoded) {
				logger.info("setting billing service provider");

				outputArray[index++] = encodedVal;
			}
		}

		return outputArray;
	}

	/**
	 * This method is used for decoding billing Number Address Signal
	 * 
	 * @param data   represents 5 octet BCD input
	 * @param offcet and @param parity are set to 0 for billing Number
	 * @return a String
	 */
	private static String decodeAdrsSignalForBns(byte[] data, int offset, int parity) throws InvalidInputException {
		if (logger.isDebugEnabled()) {
			logger.debug("Enter: decodeAdrsSignal:Input--> data:" + Util.formatBytes(data) + " ,offset:" + offset
					+ " ,parity" + parity);
		}
		if (data == null) {
			logger.error("decodeAdrsSignal: InvalidInputException(data is null)");
			throw new InvalidInputException("data is null");
		}
		int len = data.length;
		char output[] = new char[2 * (len - offset)];
		int top = 0;
		for (int i = offset; i < len; i++) {
			output[top++] = hexcodes[data[i] & 0xf];
			output[top++] = hexcodes[(data[i] >> 4) & 0xf];
		}
		String tmpStr = new String(output);
		tmpStr = tmpStr.substring(0, tmpStr.length() - parity);
		if (logger.isDebugEnabled()) {
			logger.debug("Exit: decodeAdrsSignal:Output<-- adrssignal:" + tmpStr);
		}
		return tmpStr;
	}

	/**
	 * This method is used for encode Address Signal into byte array
	 * 
	 * @param addrSignal String addrSignal
	 * @return a byte array
	 */
	private static byte[] encodeAdrsSignalForBns(String addrSignal) throws InvalidInputException {

		if (logger.isDebugEnabled()) {
			logger.debug("Enter: encodeAdrsSignal:Input--> addrSignal:" + addrSignal);
		}
		if (addrSignal == null || addrSignal.equals(" ")) {
			logger.error("encodeAdrsSignal: InvalidInputException(AddressSignal is null or blank)");
			throw new InvalidInputException("AddressSignal is null or blank");
		}
		int len = addrSignal.length();
		int size = (len + 1) / 2;
		byte[] out = new byte[size];
		for (int i = 0, j = 0; i < len; i += 2, j++) {
			byte b1 = (byte) (addrSignal.charAt(i) - '0');
			byte b2 = 0;
			if ((i + 1) < len) {
				b2 = (byte) (addrSignal.charAt(i + 1) - '0');
			}
			out[j] = (byte) ((b2 << 4) | b1);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("Exit: encodeAdrsSignal:Output<-- byte[]:" + Util.formatBytes(out));
		}
		return out;
	}

	/**
	 * this method is used to return application error
	 * 
	 * @param callData
	 * @return byte[]
	 */
	public static byte[] getApplicationErrorForBns(CallData callData) {
		// error code will be in leg2
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int totalLength = 0;
		int index = 0;
		totalLength += BnsConstants.TOTAL_FIXED_LEN_APP_ERR;

		byte encodedErrorCodeIden = 0;
		totalLength += 1;
		encodedErrorCodeIden = (byte) 0xD3;

		if (leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			String errCodeIden = (String) leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
			switch (Integer.parseInt(errCodeIden)) {
			case 1:
				encodedErrorCodeIden = (byte) 0xD3;
				break;
			case 2:
				encodedErrorCodeIden = (byte) 0xD4;
				break;
			default:
				break;
			}
		}

		byte[] encodedErrorCode = { 0x06 };
		totalLength += 1;
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
		byte[] buffer = new byte[totalLength];
		buffer[index++] = (byte) 0x01;
		buffer[index++] = encodedErrorCodeIden;
		buffer[index++] = (byte) 0x01;
		if (encodedErrorCode != null) {
			for (byte val : encodedErrorCode) {
				buffer[index++] = val;
			}
		}
		buffer[index++] = (byte) 0xF2;
		buffer[index] = (byte) 0x00;

		if(logger.isDebugEnabled()){
			logger.debug("Application Error for BNS:" + CommonUtils.formatBytes(buffer));
		}
		return buffer;
	}

	//	public static byte[] getProtocolErrorForBns(CallData callData) { // Error
	//		// code will be in leg1 LegData leg2Data = (LegData)
	//		logger.info("inside : getProtocolErrorForBns");
	//		callData.get(CallDataAttribute.P_LEG2);
	//		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
	//
	//		int totalLength = 6;
	//		int index = 0;  
	//
	//		int probType = 0;
	//		byte[] encodedProblemType = null;
	//		if (leg2Data.get(LegDataAttributes.P_BNS_PROTOCOL_ERR_PROBLEM_TYPE) != null) {
	//			probType = (Integer.parseInt((String) leg2Data.get(LegDataAttributes.P_BNS_PROTOCOL_ERR_PROBLEM_TYPE)));
	//			encodedProblemType = CommonUtils.formatIntToByte(probType);
	//		}
	//		logger.info("encodedProblemType ::"+CommonUtils.formatBytes(encodedProblemType));
	//
	//		int probSpec = 0;
	//		byte[] encodedProblemSpecifier = null;
	//		if (leg2Data.get(LegDataAttributes.P_BNS_PROTOCOL_ERR_PROBLEM_SPECIFIER) != null) {
	//			probSpec = (Integer.parseInt((String) leg2Data
	//					.get(LegDataAttributes.P_BNS_PROTOCOL_ERR_PROBLEM_SPECIFIER)));
	//			encodedProblemSpecifier = CommonUtils.formatIntToByte(probSpec);
	//		}
	//		logger.info("encodedProblemSpecifier ::"+CommonUtils.formatBytes(encodedProblemSpecifier));
	//		byte[] buffer = new byte[totalLength];
	//		buffer[index++] = (byte) 0xC5;
	//		buffer[index++] = (byte) 0x01;
	//		if (encodedProblemType != null) {
	//			for (byte val : encodedProblemType) {
	//				buffer[index++] = val;
	//			}
	//		}
	//		if (encodedProblemSpecifier != null) {
	//			for (byte val : encodedProblemSpecifier) {
	//				buffer[index++] = val;
	//			}
	//		}
	//		buffer[index++] = (byte) 0xF2;
	//		buffer[index++] = (byte) 0x00;
	//
	//		return buffer;
	//	}

	/**
	 * This method is used for encode PhoneNumber into byte array
	 * 
	 * @param PhoneNumber PhoneNumber ph
	 * @return a byte array
	 */
	private static byte[] encodeBnsAINDigits(PhoneNumber ph) throws InvalidInputException {

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeAINDigits");
		}

		CalledNatOfNumEnum natureOfAddrEnum = CalledNatOfNumEnum.fromInt(ph.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
		}

		NumPlanEnum numberPlan = NumPlanEnum.fromInt(ph.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		AinDigits ainDigits = new AinDigits();
		ainDigits.setAddrSignal(ph.getAddress());
		ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
		ainDigits.setNumPlanEnum(numberPlan);

		byte[] buffer = ainDigits.encodeAinDigits();

		return buffer;
	}

	/**
	 * this method converts ASCII string to hex byte array
	 * 
	 * @param String asciiVal
	 * @return byte []
	 */
	private static byte[] asciToHex(String asciiVal) {
		asciiVal = asciiVal.toLowerCase();
		int len = asciiVal.length();
		int size = (len + 1) / 2;
		byte[] out = new byte[size];

		for (int i = 0, j = 0; i < len; i += 2, j++) {
			byte b1 = (byte) (asciiVal.charAt(i) - '0');
			if (b1 > 9 || b1 < 0) {
				b1 = (byte) (asciiVal.charAt(i) - 87);
			}
			byte b2 = 0;
			if ((i + 1) < len) {
				b2 = (byte) (asciiVal.charAt(i + 1) - '0');
				if (b2 > 9 || b2 < 0) {
					b2 = (byte) (asciiVal.charAt(i) - 87);
				}
			}
			if (len >= 2) {
				out[j] = (byte) ((b1 << 4) | b2);
			} else {
				out[j] = (byte) b1;
			}
			logger.debug("b1: " + b1 + " b2: " + b2 + " i: " + i + " j:" + j + " out[j]: " + out[j]);
		}
		return out;
	}

	/**
	 * this method converts string decimal number to hex byte array
	 * 
	 * @param input
	 * @return byte []
	 */
	public static byte[] decimalToBcdForBns(String input) {
		long num = 0;
		try {
			num = Long.parseLong(input);
		} catch (Exception e) {
			throw new IllegalArgumentException("input should be a number: " + input);
		}
		if (num < 0)
			throw new IllegalArgumentException(
					"The method decimalToBcd doesn't support negative numbers." + " Invalid argument: " + num);

		int digits = 0;

		long temp = num;
		while (temp != 0) {
			digits++;
			temp /= 10;
		}

		int byteLen = digits % 2 == 0 ? digits / 2 : (digits + 1) / 2;

		byte[] bcd = new byte[byteLen];

		for (int i = 0; i < digits; i++) {
			byte tmp = (byte) (num % 10);

			if (i % 2 == 0) {
				bcd[i / 2] = tmp;
			} else {
				bcd[i / 2] |= (byte) (tmp << 4);
			}

			num /= 10;
		}

		for (int i = 0; i < byteLen / 2; i++) {
			byte tmp = bcd[i];
			bcd[i] = bcd[byteLen - i - 1];
			bcd[byteLen - i - 1] = tmp;
		}

		return bcd;
	}

	/**
	 * method use to convert into hex format e.g. input "FAE" output= 0x0F 0xAE
	 * 
	 * @param input
	 * @return
	 */
	public static String[] convertHexRepersentaion(String input) {
		if (input.length() % 2 != 0) {
			input = 0 + input;
		}
		char[] output = new char[5 * (input.length() / 2)];
		int top = 0;
		int temp = 0;
		for (int i = 0; i < input.length() / 2; i++) {
			output[top++] = '0';

			output[top++] = 'x';

			output[top++] = input.charAt(temp++);

			output[top++] = input.charAt(temp++);

			output[top++] = ' ';
		}
		String tmpStr = new String(output);
		String[] hexString = tmpStr.split(" ");
		return hexString;

	}

	private static byte[] encodeIa5(String asciiVal) {
		return asciiVal.getBytes(StandardCharsets.US_ASCII);
	}
	
	/**
	 * method here used to reset all Global Variables, leg1Data & leg2Data attributes 
	 * @param callData
	 */
	public static void resetBNS(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetBNS:Enter");
		}
		
		//getting leg1Data to remove its attributes
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		//getting leg2Data to remove its attributes 
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
		//resetting leg1Data Attributes
		if(legData.get(LegDataAttributes.P_LIDB_QUERY_TYPE) != null) {
			legData.remove(LegDataAttributes.P_LIDB_QUERY_TYPE);
		}
		
		if(legData.get(LegDataAttributes.P_LEG_SS7_STATE) != null) {
			legData.remove(LegDataAttributes.P_LEG_SS7_STATE);
		}
				
		if(legData.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			legData.remove(LegDataAttributes.P_APP_ERR_CODE);
		}
				
		if(legData.get(LegDataAttributes.P_APP_ERR_PROBLEM_DATA) != null) {
			legData.remove(LegDataAttributes.P_APP_ERR_PROBLEM_DATA);
		}
				
		if(legData.get(LegDataAttributes.BNS_BILLING_NUM) != null) {
			legData.remove(LegDataAttributes.BNS_BILLING_NUM);
		}
				
		if(legData.get(LegDataAttributes.P_CALLED_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLED_PARTY);
		}
			
		if(legData.get(LegDataAttributes.P_CALLING_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLING_PARTY);
		}
				
		if(legData.get(LegDataAttributes.BNS_O_PSRID_TYPE) != null) {
			legData.remove(LegDataAttributes.BNS_O_PSRID_TYPE);
		}
		
		if(legData.get(LegDataAttributes.BNS_O_PRIMARY_SERVICE_REQUESTER) != null) {
			legData.remove(LegDataAttributes.BNS_O_PRIMARY_SERVICE_REQUESTER);
		}
			
		if(legData.get(LegDataAttributes.BNS_O_SSRID_TYPE) != null) {
			legData.remove(LegDataAttributes.BNS_O_SSRID_TYPE);
		}
			
		if(legData.get(LegDataAttributes.BNS_O_SECONDARY_SERVICE_REQUESTER) != null) {
			legData.remove(LegDataAttributes.BNS_O_SECONDARY_SERVICE_REQUESTER);
		}
		
		//resetting leg2Data Attributes
		if(leg2Data.get(LegDataAttributes.P_BNS_M_COMPANY_ID) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_COMPANY_ID);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_M_RECORD_STATUS_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_RECORD_STATUS_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_M_COLLECT_ACCEPTANCE_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_COLLECT_ACCEPTANCE_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_M_THIRD_NUMBER_ACCEPTANCE_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_THIRD_NUMBER_ACCEPTANCE_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_M_TREATMENT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_TREATMENT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_M_SERVICE_OR_EQUIPMENT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_SERVICE_OR_EQUIPMENT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_M_INTERCEPT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_INTERCEPT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_O_RAO_DIGIT) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_O_RAO_DIGIT);
		}
		if(leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_APIC) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_IC_INDICATOR_APIC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_PPIC) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_IC_INDICATOR_PPIC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_M_IC_INDICATOR_PINC) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_M_IC_INDICATOR_PINC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_O_PREF_INC_DIGIT_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_O_PREF_INC_DIGIT_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_O_REF_NUM) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_O_REF_NUM);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_O_REF_NUM_TYPE) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_O_REF_NUM_TYPE);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_O_ACC_OWNER) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_O_ACC_OWNER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_BNS_O_BILING_SER_PROV) != null) {
			leg2Data.remove(LegDataAttributes.P_BNS_O_BILING_SER_PROV);
		}
		
		if (logger.isDebugEnabled()) {
			logger.info("resetBNS:Exit");
		}
	}
}