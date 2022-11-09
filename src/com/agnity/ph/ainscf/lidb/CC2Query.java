package com.agnity.ph.ainscf.lidb;

import com.agnity.ph.ainscf.lidb.CcConstants;

import java.nio.charset.StandardCharsets;

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

public class CC2Query {


	private static Logger logger = Logger.getLogger(CC2Query.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
	'f' };

	private CC2Query() {

	}



	/** method decode the input buffer
	 * 
	 * @param callData
	 * @param input
	 * @throws InvalidInputException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void decodeCc2Query(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {
		if (logger.isDebugEnabled()) {
			logger.debug("decodeCc2Query:Enter::" + Util.formatBytes(input));
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.CC2.name());

		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:set P_LIDB_QUERY_TYPE == CC2");
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
		// check for parameter set length - Row Q
		if (input[1] <= CcConstants.CC_MANDATORY_LEN) {
			logger.error("decodeBuffer:input buffer can not be less then 33 at pos1 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new ParameterOutOfRangeException("Length of the buffer must be Greater than 33");
		}
		// operation perform to set -->parameter set length

		// check for CC2 Info ID - Row R
		currentIndex = 2;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[2] != (byte) 0xDF || input[3] != (byte) 0x5F) {
			logger.error("decodeBuffer:CC2 info id is not correct at pos2 & 3 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("CC2 info id is not correct");
		}
		// operation to perform to set--->CC2 Info ID

		// check for CC2 Info length. - Row S
		if (input[4] != (byte) 0x00) {
			logger.error("decodeBuffer:CC2 info length should be zero at pos4. " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("CC2 info length should be zero");
		}
		// operation to perform on--->CC2 Info length.

		// check for Service key ID - Row T
		currentIndex = 5;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[5] != (byte) 0xAA) {
			logger.error("decodeBuffer:Service key ID is not correct at pos5 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Service key ID is not correct ");
		}

		// From GR: Service Key Length - The service key has a length of 28 octets plus
		// the length of the
		// called number digits field. - Row U
		int serviceKeyLen = input[6] & 0xFF;
		if (serviceKeyLen < 28) {
			logger.error("decodeBuffer:Service key Length is not correct at pos 6 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Service key Length is not correct ");
		}
		// check for Digits Id [Billing Number] - Row V
		currentIndex = 7;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[7] != (byte) 0x84) {
			logger.error(
					"decodeBuffer:Digits Identifier [Billing Number] is not correct as pos7 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Id [Billing Number] is not correct ");
		}

		// check for Digits length. - Row W
		int billingNumberType = 0; //0=CCAN, 1 = ACCAN
		if (input[8] == (byte) 0x09) {
			logger.info("decodeBuffer:Digits length[Billing Number] is CCAN type. "
					+ CommonUtils.formatBytes(input));
			billingNumberType = 0;
		}else {
			logger.info("decodeBuffer:Digits length[Billing Number] is ACCAN type. "
					+ CommonUtils.formatBytes(input));
			billingNumberType = 1;

		}
		// operation to perform on --> digits length.

		// check for digits - Row X
		int numDigiLenBilling = input[12];
		int newIndex = 0;
		if (input[9] == (byte) 0x05 && input[10] == (byte) 0x00 &&  input[11] == (byte) 0x11 &&  input[12] == (byte) 0x0A) {
			logger.info("decodeBuffer:Digits[Billing Number] type is CCAN with 10 digit length"
					+ CommonUtils.formatBytes(input));
			legData.set(LegDataAttributes.CC2_BILLING_NUM_TYPE, 0); //CCAN - This will be used only if ACG component is encoded, to set the numbering plan for ACG controlledCode.
		} else if(input[9] == (byte) 0x05 &&  input[10] == (byte) 0x00 &&  input[11] == (byte) 0x01 &&  input[12] != (byte) 0x0A) {
			logger.info("decodeBuffer:Digits[Billing Number] type is ACCAN with digit length "
					+ input[12]);
			legData.set(LegDataAttributes.CC2_BILLING_NUM_TYPE, 1); //ACCAN - This will be used only if ACG component is encoded, to set the numbering plan for ACG controlledCode.
			if(numDigiLenBilling < 16) {
				logger.error("decodeBuffer:Digits [Billing Number] should be minimum 16 digits in case of ACCAN at pos 12."
						+ CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digits [Billing Number] should be minimum 16 digits in case of ACCAN at pos 12.");
			}
		} else {
			logger.error("decodeBuffer:Digits [Billing Number] should be either CCAN or ACCAN at pos 9,10,11,12. "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits [Billing Number] should be either CCAN or ACCAN at pos 9,10,11,12.");
		}


		/*
		 * format for digits: 0x05 0x00 0x11 0x0A [5 octet BCD Format 10 digits]
		 */
		AinDigits ainDigitForBillingNum = new AinDigits();
		// Creating format for AIN digit decoder
		int billNumArrSize;
		if (numDigiLenBilling % 2 == 0) {
			billNumArrSize = (numDigiLenBilling / 2);
		} else {
			billNumArrSize = (numDigiLenBilling + 1) / 2;
		}
		byte[] inputforDecodeAin = new byte[billNumArrSize];
		int natureOfBns = input[10] & 0xFF;
		for (int i = 0; i < inputforDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputforDecodeAin[i] = input[13 + i];
		}
		ainDigitForBillingNum.setAddrSignal(decodeAdrsSignalForCc2(inputforDecodeAin, 0, 0));
		PhoneNumber bilingNumber = AinScfProtocolParser.parseAinDigits(ainDigitForBillingNum, natureOfBns);
		legData.set(LegDataAttributes.CC2_BILLING_NUM, bilingNumber);

		//Setting the new index for input based on billing number length
		if (numDigiLenBilling % 2 == 0) {
			newIndex = (numDigiLenBilling / 2) + 13;
		} else {
			newIndex = (numDigiLenBilling + 1) / 2 + 13;
		}
		// check for PIN identifier  - Row Y
		currentIndex = newIndex;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[newIndex] != (byte) 0xDF && input[newIndex + 1] != (byte) 0x49) {
			logger.error("decodeBuffer:Digit Id should be correct as pos " +newIndex
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit Identifier should be correct.");
		}
		newIndex = newIndex +2;

		// check for Digits Id [Called Party] - Row Z
		// The CC2 PIN is of variable length in case of ACCN
		//if (input[newIndex] != (byte) 0x02) {
		//	logger.error("decodeBuffer:PIN length should be correct as pos " +newIndex
		//			+ CommonUtils.formatBytes(input));
		//	throw new AINCodecException("PIN length should be correct.");
		//}

		newIndex = newIndex +1;
		//Row  AA-1 Type of digits = PIN
		if (input[newIndex] != (byte) 0x0E) {
			logger.error("decodeBuffer:Type of digits should be PIN at pos21"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Type of digits should be PIN at pos21.");
		}

		newIndex = newIndex +1;
		//Row  AA-2 NON 
		if (input[newIndex] != (byte) 0x00) {
			logger.error("decodeBuffer:Nature of Number should be 0 at pos22"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Nature of Number should be 0 at pos22.");
		}
		newIndex = newIndex +1;
		//Row  AA-3 Encoding/Numbering plan
		if (input[newIndex] != (byte) 0x01) {
			logger.error("decodeBuffer:Encoding schema/Numbering plan should be correct at pos23"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Nature of Number should be 0 at pos23.");
		}
		newIndex = newIndex +1;
		//Row  AA-4 Number of digits is of variable length in case of ACCN
		int pinLength = input[newIndex] & 0xFF;
		if (billingNumberType == 0) { //i.e if CCAN type
			if (input[newIndex] != (byte) 0x04) {
				logger.error("decodeBuffer:Number of digits should be 4 in case CCN type at pos24"
						+ CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Number of digits should be 4 in case CCN type at pos24.");
			}
		}
		newIndex = newIndex +1;
		//Row  AA-5 PIN digits
		AinDigits ainDigitForPinNum = new AinDigits();
		// Creating format for AIN digit decoder
		int arraySizePin; 
		// CCAN type : 2 octet BCD format 4 digits, ACCAN type : variable length
		if (pinLength % 2 == 0) {
			arraySizePin = (pinLength / 2);
		} else {
			arraySizePin = (pinLength + 1) / 2;
		}
		byte[] inputforDecodeAinPin = new byte[arraySizePin];
		for (int i = 0; i < inputforDecodeAinPin.length; i++) {
			// adding num of digit and bcd format of digit
			inputforDecodeAinPin[i] = input[newIndex + i];
		}
		String cC2PinNum = decodeAdrsSignalForCc2(inputforDecodeAinPin, 0, 0);
		legData.set(LegDataAttributes.CC2_PIN, cC2PinNum);
		newIndex = newIndex + arraySizePin;

		// row AB  calling Party
		currentIndex = newIndex;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[newIndex] != (byte) 0x84) {
			logger.error(
					"decodeBuffer:Digit identifier [calling Party] should be correct at pos " +newIndex + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit identifier [calling Party] should be correct.");
		}

		//row AC calling number digit length is variable
		//input[24]

		//row AD -1
		newIndex = newIndex +2;
		if (input[newIndex] != (byte) 0x02) {
			logger.error(
					"decodeBuffer:type of Digit [calling Party] should be correct at pos " +newIndex + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("type of Digit [calling Party] should be correct.");
		}
		// check for Digit[CAlling Party] -->NOA - Row AD -2
		newIndex = newIndex +1;
		int natureOfGNDigitsClg = input[newIndex] & 0xFF;
		if (input[newIndex] != (byte) 0x00 && input[newIndex] != (byte) 0x01) {
			logger.error("decodeBuffer:Digit[calling Party] -->NOA should be correct at pos" +newIndex
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[calling Party] -->NOA should be correct.");
		}

		// AD - 3 encoding BCD, Numbering plan
		newIndex = newIndex +1;
		if (input[newIndex] != (byte) 0x11 && input[newIndex] != (byte) 0x31) {
			logger.error("decodeBuffer:Digit[calling Party] -->Encoding/numbering plan should be correct at pos "+newIndex
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[calling Party] -->Encoding/numbering plan should be correct.");
		}
		//AD - 4 num digit length variable
		newIndex = newIndex +1;
		int numdigilenClg = input[newIndex] & 0xFF;
		newIndex = newIndex +1;
		if (numdigilenClg > 15 || numdigilenClg <= 0) {
			logger.error("decodeBuffer:Digit[Called Party] -->num digit length should be max 15 and >0 at pos "+newIndex
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] -->num digit length should be correct.");
		}
		// AD - 5 set calling party digit
		AinDigits aindigitcalling = new AinDigits();
		// Creating format for AIN digit decoder
		int arrsizeclg;
		if (numdigilenClg % 2 == 0) {
			arrsizeclg = (numdigilenClg / 2);
		} else {
			arrsizeclg = (numdigilenClg + 1) / 2;
		}
		int numByteslenClg = arrsizeclg;
		byte[] inputfrDecodeAinclg = new byte[arrsizeclg];
		//inputfrDecodeAinclg[0] = input[arrsizeclg + 27]; // adding NOA-->called
		for (int i = 0; i < inputfrDecodeAinclg.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAinclg[i] = input[newIndex + i];
		}
		// Setting calling party num
		aindigitcalling.setAddrSignal(decodeAdrsSignalForCc2(inputfrDecodeAinclg, 0, 0));
		PhoneNumber phNumberClg = AinScfProtocolParser.parseAinDigits(aindigitcalling, natureOfGNDigitsClg);
		legData.set(LegDataAttributes.P_CALLING_PARTY, phNumberClg);


		// row AE  called Party
		int calldPtyIndex = newIndex + numByteslenClg;
		currentIndex = calldPtyIndex;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		//Called party is optional in case of billing number with ACCAN type 
		if (input.length > calldPtyIndex ) {
			if (input[calldPtyIndex] != (byte) 0x84) {
				logger.error(
						"decodeBuffer:Digit identifier [called party] should be correct at pos " +calldPtyIndex + CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digit identifier [called party] should be correct.");
			}

			//row AF called number digit length is variable
			//input[calldPtyIndex+1]

			//row AG -1
			if (input[calldPtyIndex + 2] != (byte) 0x01) {
				logger.error(
						"decodeBuffer:type of Digit [called party] should be correct " + CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("type of Digit [called party] should be correct.");
			}
			// check for Digit[called party] -->NOA - Row AG -2
			int natureOfGNDigitscld = input[calldPtyIndex + 3] & 0xFF;
			if (input[calldPtyIndex + 3] != (byte) 0x00 && input[calldPtyIndex + 3] != (byte) 0x01) {
				logger.error("decodeBuffer:Digit[called party] -->NOA should be correct at"
						+ CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digit[called party] -->NOA should be correct.");
			}

			// AG - 3 encoding BCD, Numbering plan
			if (input[calldPtyIndex + 4] != (byte) 0x11 && input[calldPtyIndex + 4] != (byte) 0x31) {
				logger.error("decodeBuffer:Digit[called party] -->Encoding/numbering plan should be correct"
						+ CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digit[called party] -->Encoding/numbering plan should be correct.");
			}
			//AG - 4 num digit length variable
			int numdigilen = input[calldPtyIndex + 5] & 0xFF;

			if (numdigilen > 15 || numdigilen <= 0) {
				logger.error("decodeBuffer:Digit[Called Party] -->num digit length should be max 15 and >0 "
						+ CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digit[Called Party] -->num digit length should be correct.");
			}
			// AG - 5 set called party digit
			AinDigits aindigitcalled = new AinDigits();
			// Creating format for AIN digit decoder
			int arrsizecld;
			if (numdigilen % 2 == 0) {
				arrsizecld = (numdigilen / 2);
			} else {
				arrsizecld = (numdigilen + 1) / 2;
			}
			byte[] inputfrDecodeAincld = new byte[arrsizecld];
			for (int i = 0; i < inputfrDecodeAincld.length; i++) {
				// adding num of digit and bcd format of digit
				inputfrDecodeAincld[i] = input[calldPtyIndex + 6 + i];
			}
			// Setting called party num
			aindigitcalled.setAddrSignal(decodeAdrsSignalForCc2(inputfrDecodeAincld, 0, 0));
			PhoneNumber phNumberCld = AinScfProtocolParser.parseAinDigits(aindigitcalled, natureOfGNDigitscld);
			legData.set(LegDataAttributes.P_CALLED_PARTY, phNumberCld);


			//Checking for Primary/secondary Service Request optional fields
			currentIndex = calldPtyIndex + 5 + arrsizecld + 1;
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
					legData.set(LegDataAttributes.CC_O_PSRID_TYPE, psridType);
					byte[] primaryServiceRequesterIdtable = { input[inputIndex++], input[inputIndex++], input[inputIndex++], input[inputIndex++] };
					String primaryServiceRequesterId = new String(primaryServiceRequesterIdtable);
					legData.set(LegDataAttributes.CC_O_PRIMARY_SERVICE_REQUESTER, primaryServiceRequesterId);

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
						legData.set(LegDataAttributes.CC_O_SSRID_TYPE, ssridType);
						byte[] secondaryServiceRequesterIdtable = { input[inputIndex++], input[inputIndex++], input[inputIndex++], input[inputIndex++] };
						String secondaryServiceRequesterId = new String(secondaryServiceRequesterIdtable);
						legData.set(LegDataAttributes.CC_O_SECONDARY_SERVICE_REQUESTER, secondaryServiceRequesterId);
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
					legData.set(LegDataAttributes.CC_O_SSRID_TYPE, ssridType);
					byte[] secondaryServiceRequesterIdtable = { input[inputIndex++], input[inputIndex++], input[inputIndex++], input[inputIndex++] };
					String secondaryServiceRequesterId = new String(secondaryServiceRequesterIdtable);
					legData.set(LegDataAttributes.CC_O_SECONDARY_SERVICE_REQUESTER, secondaryServiceRequesterId);

				}


			} else if (input.length > (currentIndex) && input.length < (currentIndex + 8)) {
				currentElementIdLen = 0; // number of bytes of current element Identifier (unknown)
				currentElementLen = input[input.length - currentIndex ] & 0xFF;
				logger.info("decodeBuffer:throwing exception ");
				logger.error("decodeBuffer:Invalid parameter received, expected PSRID or SSRID");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Invalid parameter received, expected PSRID or SSRID");			
			}

		}
		if (logger.isDebugEnabled()) {
			logger.debug("CC2 decode:Exit");
		}
	}

	/**
	 * method to encode the CC2 Query
	 * 
	 * @param callData
	 * @return byte[]
	 * @throws InvalidInputException, AINCodecException
	 */
	public static byte[] encodeCc2Query(CallData callData) throws InvalidInputException, AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeCc2Query:Enter");
		}

		// encode and set the data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int index = 0;
		// total length of output array
		int totalLength = 0;
		// totalLength += CcConstants.CC_PARAM_SET_ID_LEN;

		//Match
		if (leg2Data.get(LegDataAttributes.P_CC_MATCH) == null) {
			logger.error("encodeCc2Query:parameter MATCH is Mandatory");
			throw new AINCodecException("parameter MATCH is Mandatory");
		}

		byte[] encodeMatch = null;
		int match = (Integer) leg2Data
				.get(LegDataAttributes.P_CC_MATCH);
		encodeMatch = CommonUtils.formatIntToByte(match);
		totalLength += 4;

		// CSDI - 
		if (leg2Data.get(LegDataAttributes.P_CC_CCAN_SERVICE_DENIAL_INDICATOR) == null) {
			logger.error("encodeCc2Query:parameter CSDI is Mandatory");
			throw new AINCodecException("parameter CSDI is Mandatory");
		}
		byte[] encodeCsdi = null;
		int csdIndicator = (Integer) leg2Data
				.get(LegDataAttributes.P_CC_CCAN_SERVICE_DENIAL_INDICATOR);
		encodeCsdi = CommonUtils.formatIntToByte(csdIndicator);
		totalLength += 4;

		// CCSAN (applies only for normal response Table 8.18 Row U)
		byte[] encodeCcsan = null;
		if (match == 1) {
			if (leg2Data.get(LegDataAttributes.P_CC_CCSAN) == null) {
				logger.error("encodeCc2Query:parameter CCSAN is Mandatory in normal response");
				throw new AINCodecException("parameter CCSAN is Mandatory in normal response");
			}
			String ccsan = (String) leg2Data
					.get(LegDataAttributes.P_CC_CCSAN);
			encodeCcsan = AinScfProtocolFieldCodec.encodeDigitsInBcdFormat(ccsan);
			totalLength += 4;
		}

		// PSDI - (applies only for normal response Table 8.18 Row X)
		byte[] encodePSDI = null;
		if (match == 1) {
			if (leg2Data.get(LegDataAttributes.P_CC_PIN_SERVICE_DENIAL_INDICATOR) == null) {
				logger.error("encodeCc2Query:parameter PSDI is Mandatory in normal response");
				throw new AINCodecException("parameter PSDI is Mandatory in normal response");
			}
			int PSDIndicator = (Integer) leg2Data
					.get(LegDataAttributes.P_CC_PIN_SERVICE_DENIAL_INDICATOR);
			encodePSDI = CommonUtils.formatIntToByte(PSDIndicator);
			totalLength += 4;
		}

		// Pin restriction ind (applies only for normal response Table 8.18 Row AA)
		byte[] encodePinRestrInd = null;
		if (match == 1) {
			if (leg2Data.get(LegDataAttributes.P_CC_PIN_RESTRICTION_INDICATOR) != null) {
				logger.info("encodeCc2Query:parameter PRI is present");
				int pinRestrictionInd = (Integer) leg2Data
						.get(LegDataAttributes.P_CC_PIN_RESTRICTION_INDICATOR);
				encodePinRestrInd = CommonUtils.formatIntToByte(pinRestrictionInd);
				totalLength += 4;
			}
		}

		// company id is an optional field
		// Row O, P Q
		byte[] companyId = null;
		if (leg2Data.get(LegDataAttributes.P_CC_COMPANY_ID) != null) {
			logger.info("encodeCc2Query:parameter companyId is present");
			String bnsCompId = (String) leg2Data.get(LegDataAttributes.P_CC_COMPANY_ID);
			//byte[] companyId = decimalToBcdForCc2(bnsCompId);
			companyId = AinScfProtocolFieldCodec.encodeDigitsInBcdFormat(bnsCompId);
			if (companyId.length != 2) {
				logger.info("encodeCc2Query : company id length should be 2");
				throw new AINCodecException("company id length should be 2");
			}
			// increment length by 5
			totalLength += CcConstants.CC_COMPANY_ID_LEN;
		}

		// Record Status Indicator, Row R, S, T
		if (leg2Data.get(LegDataAttributes.P_CC_RECORD_STATUS_INDICATOR) == null) {
			logger.error("encodeCc2Query:parameter RECORD_STATUS_INDICATOR is Mandatory");
			throw new AINCodecException("parameter RECORD_STATUS_INDICATOR is Mandatory");
		}		
		byte[] encodedRecStatInd = null;
		int recStatInd = (Integer) leg2Data.get(LegDataAttributes.P_CC_RECORD_STATUS_INDICATOR);
		encodedRecStatInd = CommonUtils.formatIntToByte(recStatInd);
		if (encodedRecStatInd.length != 1) {
			logger.info("encodeCc2Query:throwing exception ");
			logger.error("encodeCc2Quer:parameter record status indicator length should be 1");
			throw new AINCodecException("parameter record status indicatorlength should be 1");
		}
		// Increment length by 4
		totalLength += CcConstants.CC_REC_STATUS_IND_LEN;


		// RAO Digit Identifier optional
		byte[] digitIdentifier = null;
		if (leg2Data.get(LegDataAttributes.P_CC_RAO_DIGIT) != null) {
			logger.info("encodeCc2Query:parameter RAO is present");

			String raoDigit = (String) leg2Data.get(LegDataAttributes.P_CC_RAO_DIGIT);

			// RAO digit length has to be 3
			digitIdentifier = null;
			if (raoDigit != null && raoDigit.length() == 3) {
				digitIdentifier = encodeAdrsSignalForCc2(raoDigit);
				totalLength += CcConstants.CC_O_DIGIT_IDENTIFIER_LEN;
			}
		}

		// IC Indicator ,Row -AM, AN, AO
		if (leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_APIC) == null) {
			logger.error("encodeCc2Query:parameter APIC_INDICATOR is Mandatory");
			throw new AINCodecException("parameter APIC_INDICATOR is Mandatory");
		}
		if (leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_PPIC) == null) {
			logger.error("encodeCc2Query:parameter PPIC_INDICATOR is Mandatory");
			throw new AINCodecException("parameter PPIC_INDICATOR is Mandatory");
		}
		if (leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_PINC) == null) {
			logger.error("encodeCc2Query:parameter PINC_INDICATOR is Mandatory");
			throw new AINCodecException("parameter PINC_INDICATOR is Mandatory");
		}
		byte[] byteIcIndicator = null;

		totalLength += CcConstants.CC_IC_INDICATOR_LEN;
		int icIndicatorAPIC = (Integer)leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_APIC);
		int icIndicatorPPIC = (Integer)leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_PPIC);
		int icIndicatorAPIC_PPIC = (icIndicatorAPIC & 0x07) << 3;
		icIndicatorAPIC_PPIC |= (icIndicatorPPIC & 0x07);

		int icIndicatorPINC = (Integer)leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_PINC);

		byte[] IcInds = { (byte) icIndicatorAPIC_PPIC, (byte) icIndicatorPINC };
		byteIcIndicator = IcInds;


		// Prim Preferred IC Digits ,Row AP, AQ, AR
		PhoneNumber primaryPreferedIcDigits = null;

		if (leg2Data.get(LegDataAttributes.P_CC_PRIMARY_PREFERRED_IC) != null){
			primaryPreferedIcDigits = (PhoneNumber) leg2Data
					.get(LegDataAttributes.P_CC_PRIMARY_PREFERRED_IC);

			if(!StringUtils.isNotBlank(primaryPreferedIcDigits.getAddress())){
				primaryPreferedIcDigits = null;
			}	
		}

		// Alternate IC Digits Row AS, AT, AU
		PhoneNumber altPrefIcDigits = null;
		if (leg2Data.get(LegDataAttributes.P_CC_ALTERNATE_PREFERRED_IC) != null){
			altPrefIcDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_CC_ALTERNATE_PREFERRED_IC);

			if(!StringUtils.isNotBlank(altPrefIcDigits.getAddress())){
				altPrefIcDigits = null;
			}	
		}

		// Preferred INC Digits Row AV, AW, AX
		PhoneNumber prefIncDigits = null;
		if (leg2Data.get(LegDataAttributes.P_CC_PREFERRED_INC) != null){
			prefIncDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_CC_PREFERRED_INC);

			if(!StringUtils.isNotBlank(prefIncDigits.getAddress())){
				prefIncDigits = null;
			}
		}

		// True billing Number Row AY, AZ, BA
		PhoneNumber trueBillNum = null;
		if (leg2Data.get(LegDataAttributes.P_CC_TRUE_BILLING_NUMBER) != null) {
			trueBillNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_CC_TRUE_BILLING_NUMBER);

			if(!StringUtils.isNotBlank(trueBillNum.getAddress())){
				trueBillNum = null;
			}else{
				trueBillNum.setNatureOfAddress(0);
			}
		}

		byte[] accOwnerEncoded = null;
		byte[] billingServiceProviderEncoded = null;

		/// Account Owner Identifier Row- BB, BC, BD
		String accOwnerIdentifier = null;
		if (leg2Data.get(LegDataAttributes.P_CC_ACC_OWNER) != null) {
			accOwnerIdentifier = (String) leg2Data.get(LegDataAttributes.P_CC_ACC_OWNER);
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
			totalLength += CcConstants.CC_ACC_OWNER_LEN;
		}
		// Billing Service Provider Row- BE, BF, BG
		String billingServiceProvider = null;

		if (leg2Data.get(LegDataAttributes.P_CC_BILING_SER_PROV) != null){
			billingServiceProvider = (String) leg2Data.get(LegDataAttributes.P_CC_BILING_SER_PROV);

			if(!StringUtils.isNotBlank(billingServiceProvider)){
				billingServiceProvider = null;
			}
		}

		if (billingServiceProvider != null && !billingServiceProvider.isEmpty()) {
			billingServiceProviderEncoded = encodeIa5(billingServiceProvider);
			totalLength += CcConstants.CC_BILLING_SER_PROV_LEN;
		}

		byte encodedPrimaryPreferedIcDigitsNumDigit = 0;
		byte encodedAltPrefIcDigitsNumDigit = 0;
		byte encodedPrefIncDigitsNumDigit = 0;
		byte encodedTrueBillNumDigit = 0;
		byte[] encodedPrimaryPreferedIcDigits = null;
		byte[] encodedAltPrefIcDigits = null;
		byte[] encodedPrefIncDigits = null;
		byte[] encodedTrueNum = null;

		if (primaryPreferedIcDigits != null) {
			encodedPrimaryPreferedIcDigits = encodeAdrsSignalForCc2(primaryPreferedIcDigits.getAddress());
			encodedPrimaryPreferedIcDigitsNumDigit = (byte) encodedPrimaryPreferedIcDigits.length;
			totalLength += BnsConstants.BNS_PRIM_PREF_IC_LEN;
		}
		if (altPrefIcDigits != null) {
			encodedAltPrefIcDigits = encodeAdrsSignalForCc2(altPrefIcDigits.getAddress());
			encodedAltPrefIcDigitsNumDigit = (byte) encodedAltPrefIcDigits.length;
			totalLength += BnsConstants.BNS_ALT_PREF_IC_LEN;
		}
		if (prefIncDigits != null) {
			encodedPrefIncDigits = encodeAdrsSignalForCc2(prefIncDigits.getAddress());
			encodedPrefIncDigitsNumDigit = (byte) encodedPrefIncDigits.length;
			totalLength += BnsConstants.BNS_PREF_INC_LEN;
		}
		if (trueBillNum != null) {
			encodedTrueNum = encodeAdrsSignalForCc2(trueBillNum.getAddress());
			encodedTrueBillNumDigit = (byte) encodedTrueNum.length;
			totalLength += encodedTrueNum.length;
			totalLength += CcConstants.CC_TRUE_BILLING_NUM_LEN;
		}



		//output
		String[] parameterSetLength = convertHexRepersentaion(Integer.toHexString(totalLength));
		byte[] outputArray = new byte[totalLength + 2];

		outputArray[index++] = CcConstants.PARAMETER_SET_ID;
		outputArray[index++] = (byte) Byte.decode(parameterSetLength[0]);

		if (encodeMatch != null) {
			outputArray[index++] = CcConstants.MATCH_TAG1;
			outputArray[index++] = CcConstants.MATCH_TAG2;
			outputArray[index++] = CcConstants.CC_MATCH_LEN;
			for (byte encodedVal : encodeMatch) {
				outputArray[index++] = encodedVal;
			}	
		}

		if (encodeCsdi != null) {
			outputArray[index++] = CcConstants.CSDI_TAG1;
			outputArray[index++] = CcConstants.CSDI_TAG2;
			outputArray[index++] = CcConstants.CC_CSDI_LEN;
			for (byte encodedVal : encodeCsdi) {
				outputArray[index++] = encodedVal;
			}	
		}

		if (encodeCcsan != null) {
			outputArray[index++] = CcConstants.CCSAN_TAG1;
			outputArray[index++] = CcConstants.CCSAN_TAG2;
			outputArray[index++] = CcConstants.CCSAN_LENGTH;
			for (byte encodedVal : encodeCcsan) {
				outputArray[index++] = encodedVal;
			}		
		}


		if (encodePSDI != null) {
			outputArray[index++] = CcConstants.PSDI_TAG1;
			outputArray[index++] = CcConstants.PSDI_TAG2;
			outputArray[index++] = CcConstants.CC_PSDI_LEN;
			for (byte encodedVal : encodePSDI) {
				outputArray[index++] = encodedVal;
			}	
		}

		if (encodePinRestrInd != null) {
			outputArray[index++] = CcConstants.PIN_RESTRICTION_IND_TAG1;
			outputArray[index++] = CcConstants.PIN_RESTRICTION_IND_TAG2;
			outputArray[index++] = CcConstants.PIN_RESTRICTION_IND_LENGTH;
			for (byte encodedVal : encodePinRestrInd) {
				outputArray[index++] = encodedVal;
			}		
		}
		if (companyId != null) {
			outputArray[index++] = CcConstants.COMPANY_ID_IDENTIFIER_TAG1;
			outputArray[index++] = CcConstants.COMPANY_ID_IDENTIFIER_TAG2;
			outputArray[index++] = CcConstants.COMPANY_ID_LENGTH;
			for (byte encodedVal : companyId) {
				outputArray[index++] = encodedVal;
			}
		}

		if (encodedRecStatInd != null) {
			outputArray[index++] = CcConstants.RECORD_STATUS_INDICATOR_ID_TAG1;
			outputArray[index++] = CcConstants.RECORD_STATUS_INDICATOR_ID_TAG2;
			outputArray[index++] = CcConstants.RECORD_STATUS_INDICATOR_ID_LENGTH;
			for (byte encodedVal : encodedRecStatInd) {
				outputArray[index++] = encodedVal;
			}
		}



		// RAO Digit Identifier, Row AJ, AK, AL (Optional)
		if (digitIdentifier != null) {
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_TAG1;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_TAG2;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_LENGTH;
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
			outputArray[index++] = CcConstants.IC_INDICATOR_ID_TAG1;
			outputArray[index++] = CcConstants.IC_INDICATOR_ID_TAG2;
			outputArray[index++] = CcConstants.IC_INDICATOR_LENGTH;
			for (byte encodedVal : byteIcIndicator) {
				outputArray[index++] = encodedVal;
			}
		}
		// Preferred IC Digits ,Row AP, AQ, AR
		if (encodedPrimaryPreferedIcDigits != null) {
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_TAG1;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_TAG2;
			outputArray[index++] = CcConstants.PRIMARY_PREFERRED_IC_DIGITS_IDENTIFIER_LENGTH;
			outputArray[index++] = (byte) 0x0D;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_NOA;
			outputArray[index++] = (byte) 0x01;
			outputArray[index++] = encodedPrimaryPreferedIcDigitsNumDigit;
			for (byte encodedVal : encodedPrimaryPreferedIcDigits) {
				outputArray[index++] = encodedVal;
			}
		}
		// Alternate IC Digits Row AS, AT, AU
		if (encodedAltPrefIcDigits != null) {
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_TAG1;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_TAG2;
			outputArray[index++] = CcConstants.ALTERNATE_PREFERRED_IC_DIGITS_IDENTIFIER_LENGTH;
			outputArray[index++] = (byte) 0x0B;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_NOA;
			outputArray[index++] = (byte) 0x01;
			outputArray[index++] = encodedAltPrefIcDigitsNumDigit;
			for (byte encodedVal : encodedAltPrefIcDigits) {
				outputArray[index++] = encodedVal;
			}
		}
		// Preferred INC Digits Row AV, AW, AX
		if (encodedPrefIncDigits != null) {
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_PREFERRED_INC_TAG1;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_PREFERRED_INC_TAG2;
			outputArray[index++] = CcConstants.PREFERRED_INC_DIGITS_IDENTIFIER_LENGTH;
			outputArray[index++] = (byte) 0x0C;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_PREFERRED_INC_NOA;
			outputArray[index++] = (byte) 0x01;
			outputArray[index++] = encodedPrefIncDigitsNumDigit;
			for (byte encodedVal : encodedPrefIncDigits) {
				outputArray[index++] = encodedVal;
			}
		}

		// True billing Number Row AY, AZ, BA
		if (encodedTrueNum != null) {
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_TRUE_BILLING_NUMBER_TAG1;
			outputArray[index++] = CcConstants.DIGITS_IDENTIFIER_ID_TRUE_BILLING_NUMBER_TAG2;
			outputArray[index++] = (byte) 0x09;
			outputArray[index++] = (byte) 0x0A;
			outputArray[index++] = (byte) 0x00;
			outputArray[index++] = (byte) 0x11;
			outputArray[index++] = (byte) trueBillNum.getAddress().length();
			for (byte encodedVal : encodedTrueNum) {
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
	private static String decodeAdrsSignalForCc2(byte[] data, int offset, int parity) throws InvalidInputException {
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
	private static byte[] encodeAdrsSignalForCc2(String addrSignal) throws InvalidInputException {

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
	public static byte[] getApplicationErrorForcc2(CallData callData) {
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
			logger.debug("Application Error for CC2:" + CommonUtils.formatBytes(buffer));
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
	private static byte[] encodeCc2AINDigits(PhoneNumber ph) throws InvalidInputException {

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
	public static byte[] decimalToBcdForCc2(String input) {
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
	public static void resetCC2(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetCC2:Enter");
		}
		
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
		//resetting leg1Data
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
				
		if(legData.get(LegDataAttributes.CC2_BILLING_NUM) != null) {
			legData.remove(LegDataAttributes.CC2_BILLING_NUM);
		}
				
		if(legData.get(LegDataAttributes.CC2_PIN) != null) {
			legData.remove(LegDataAttributes.CC2_PIN);
		}
				
		if(legData.get(LegDataAttributes.P_CALLING_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLING_PARTY);
		}
				
		if(legData.get(LegDataAttributes.P_CALLED_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLED_PARTY);
		}
				
		if(legData.get(LegDataAttributes.CC_O_PSRID_TYPE) != null) {
			legData.remove(LegDataAttributes.CC_O_PSRID_TYPE);
		}
				
		if(legData.get(LegDataAttributes.CC_O_PRIMARY_SERVICE_REQUESTER) != null) {
			legData.remove(LegDataAttributes.CC_O_PRIMARY_SERVICE_REQUESTER);
		}
			
		if(legData.get(LegDataAttributes.CC_O_SSRID_TYPE) != null) {
			legData.remove(LegDataAttributes.CC_O_SSRID_TYPE);
		}
				
		if(legData.get(LegDataAttributes.CC_O_SECONDARY_SERVICE_REQUESTER) != null) {
			legData.remove(LegDataAttributes.CC_O_SECONDARY_SERVICE_REQUESTER);
		}
				
		//resetting leg2Data
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_MATCH) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_MATCH);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_CCAN_SERVICE_DENIAL_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_CCAN_SERVICE_DENIAL_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_CCSAN) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_CCSAN);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_PIN_SERVICE_DENIAL_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_PIN_SERVICE_DENIAL_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_PIN_RESTRICTION_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_PIN_RESTRICTION_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_COMPANY_ID) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_COMPANY_ID);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_RECORD_STATUS_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_RECORD_STATUS_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_RAO_DIGIT) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_RAO_DIGIT);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_APIC) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_IC_INDICATOR_APIC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_PPIC) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_IC_INDICATOR_PPIC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_IC_INDICATOR_PINC) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_IC_INDICATOR_PINC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_PRIMARY_PREFERRED_IC) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_PRIMARY_PREFERRED_IC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_ALTERNATE_PREFERRED_IC) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_ALTERNATE_PREFERRED_IC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_PREFERRED_INC) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_PREFERRED_INC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_TRUE_BILLING_NUMBER) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_TRUE_BILLING_NUMBER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_ACC_OWNER) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_ACC_OWNER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_CC_BILING_SER_PROV) != null) {
			leg2Data.remove(LegDataAttributes.P_CC_BILING_SER_PROV);
		}
		if (logger.isDebugEnabled()) {
			logger.info("resetCC2:Exit");
		}
	}

}
