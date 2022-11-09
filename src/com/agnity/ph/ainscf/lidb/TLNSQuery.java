package com.agnity.ph.ainscf.lidb;

import static com.agnity.ph.ainscf.lidb.TlnsConstants.ALTERNATE_PREFERRED_IC_DIGITS_IDENTIFIER_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.COLLECT_ACCEPTANCE_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.COLLECT_ACCEPTANCE_INDICATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.COLLECT_ACCEPTANCE_INDICATOR_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.COMPANY_ID_IDENTIFIER_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.COMPANY_ID_IDENTIFIER_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.COMPANY_ID_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_NOA;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_ALTERNATE_PREFERRED_IC_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_PREFERRED_INC_NOA;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_PREFERRED_INC_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_PREFERRED_INC_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_NOA;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_PRIMARY_PREFERRED_IC_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.DIGITS_IDENTIFIER_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.IC_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.IC_INDICATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.IC_INDICATOR_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.INTERCEPT_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.INTERCEPT_INDICATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.INTERCEPT_INDICATOR_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.PARAMETER_SET_ID;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.PREFERRED_INC_DIGITS_IDENTIFIER_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.PRIMARY_PREFERRED_IC_DIGITS_IDENTIFIER_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.RECORD_STATUS_INDICATOR_ID_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.RECORD_STATUS_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.RECORD_STATUS_INDICATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.TERMINATING_LINE_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.TERMINATING_LINE_INDICATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.TERMINATING_LINE_INDICATOR_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.SERVICE_OR_EQUIPMENT_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.SERVICE_OR_EQUIPMENT_INDICATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.SERVICE_OR_EQUIPMENT_INDICATOR_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_LENGTH;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.THIRD_NUMBER_ACCEPTANCE_INDCIATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.TREATMENT_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.TREATMENT_INDICATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.TlnsConstants.TREATMENT_INDICATOR_LENGTH;

import java.nio.charset.StandardCharsets;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.enumdata.CalledNatOfNumEnum;
import com.agnity.ain.enumdata.NumPlanEnum;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.util.Constant;
import com.agnity.ain.util.Util;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.ainscf.AinScfProtocolFieldCodec;
import com.agnity.ph.ainscf.AinScfProtocolParser;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
public class TLNSQuery {


	private static Logger logger = Logger.getLogger(TLNSQuery.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
	'f' };

	private TLNSQuery() {

	}

	// Testing buffer for Tlns Query decodeBuffer()
	/**
	
	 * 
	 * @param callData
	 * @param input
	 * @throws InvalidInputException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void decodeTlnsQuery(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {
		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:Enter::" + Util.formatBytes(input));
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.TLNS.name());

		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:set P_LIDB_QUERY_TYPE == TLNS");
		}
		// Parameter Set ID - Row P Table 8-21 GR 1149
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
		if (input.length < TlnsConstants.TLNS_MANDATORY_LEN) {
			logger.error("decodeBuffer:input buffer can not be less than 18" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new ParameterOutOfRangeException("Length of the buffer must be Greater >= 18");
		}
		// check for parameter set length - Row Q
		
//		if (input[1] != (byte) 0x10) {
//			logger.error("decodeBuffer:TLNS parameter set length should be 16 at pos1. " + CommonUtils.formatBytes(input));
//			throw new AINCodecException("TLNS parameter set length should be 16");
//		}
		// operation perform to set -->parameter set length

		// check for TLNS Info ID - Row R
		currentIndex = 2;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[2] != (byte) 0xDF || input[3] != (byte) 0x69) {
			logger.error("decodeBuffer:TLNS info identifier is not correct at pos2 & 3 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("TLNS info identifier is not correct");
		}
		// operation to perform to set--->TLNS Info ID

		// check for TLNS Info length. - Row S
		if (input[4] != (byte) 0x00) {
			logger.error("decodeBuffer:TLNS info length should be zero at pos4. " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("TLNS info length should be zero");
		}
		// operation to perform on--->TLNS Info length.

		// check for Service key ID - Row T
		currentIndex = 5;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[5] != (byte) 0xAA) {
			logger.error("decodeBuffer:TLNS Service key ID is not correct at pos5 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("TLNS Service key ID is not correct ");
		}

		// From GR: Service Key Length - The service key has a length of 6 octets plus
		// the length of the
		// called number digits field.
		int serviceKeyLen = input[6] & 0xFF;
		if (serviceKeyLen != 11) {
			logger.error("decodeBuffer:TLNS Service key Length is not correct at pos 6 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("TLNS Service key Length is not correct ");
		}
		// check for Digits Id [Called Number] - Row V
		currentIndex = 7;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[7] != (byte) 0x84) {
			logger.error(
					"decodeBuffer:Digits Id [Called Number] is not correct at pos7 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Id [Called Number] is not correct ");
		}

		// operation to perform ont --->Digits Id [Called Number]

		// check for Digits length. - Row W
		if (input[8] != (byte) 0x09) {
			logger.error("decodeBuffer:Digits length[Called Number] should be correct at pos8. "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits  length[Called Number] should be correct.");
		}
		// operation to perform on --> digits length.

		// check for Digits Id [Called Party] - Row X -1
		if (input[9] != (byte) 0x01) {
			logger.error(
					"decodeBuffer:Digit[Called Party] should be correct at pos20" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] should be correct.");
		}

		// check for Digit[Called Party] -->NOA - Row X (2)
		if (input[10] != (byte) 0x00 && input[11] != (byte) 0x11 && input[12] != (byte) 0x0A) {
			logger.error("decodeBuffer:Digit[Called Party] -->NOA, Encoding, length should be correct at pos 10,11,12 "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] -->NOA, Encoding, length should be correct.");
		}

		// set called party digit
		int numdigilen = input[12] & 0xFF;
		int arrsize;
		if (numdigilen % 2 == 0) {
			arrsize = (numdigilen / 2);
		} else {
			arrsize = (numdigilen + 1) / 2;
		}
		AinDigits ainDigitForCC1 = new AinDigits();
		byte[] inputfrDecodeAin = new byte[arrsize];
		int natureOfCC1Digits = input[11] & 0xFF;
		for (int i = 0; i < inputfrDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAin[i] = input[13 + i];
		}
		ainDigitForCC1.setAddrSignal(decodeAdrsSignalForTlns(inputfrDecodeAin, 0, 0));
		PhoneNumber cc1CalledNumber = AinScfProtocolParser.parseAinDigits(ainDigitForCC1, natureOfCC1Digits);
		legData.set(LegDataAttributes.P_CALLED_PARTY, cc1CalledNumber);

		// check for Digits Id [Calling Party] - Row Y
		//if (input.length > 18) {}
		if (input.length > 18) {
			currentIndex = 18;
			currentElementIdLen = 1; // number of bytes of current element Identifier
			currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
			logger.info("decodeTLNSQuery: Input length is > 18, Chekcing for Optional parameters Billing number/Calling number");
			// check for Digits Id [Billing Party] Row Y, Z, AA
			if (input[18] != (byte) 0x84) {
				logger.info("decodeTLNSQuery:throwing exception ");
				logger.error("decodeTLNSQuery:Digits Id [Billing Party] at pos 18 should be correct. ");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digits Id [Billing Number] at pos 18 should be correct.");
			}
			// operation to perform on Digits Id [Billing Party]
			// get the Digits[Billing Party] Length
			//int tlnsCldPartyDigitLen = input[19] & 0xFF;
		
			//checking if digit type is either Calling party or billing number since both are optional parameters
			if (input[20] != (byte) 0x01 && input[20] != (byte) 0x05) {
				logger.info("decodeTLNSQuery:throwing exception ");
				logger.error("decodeTLNSQuery:Digit should be either Calling Party or Billing Number at pos 20");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digit should be either Calling Party or Billing Number at pos 20");
			}
			
			int tlnsArrsizeForBilling = 0;
			// if billing number digits type
			if (input[20] == (byte) 0x05) {
				// check for Digit[Billing Number] -->NOA
				if (input[21] != (byte) 0x00) {
					logger.info("decodeTLNSQuery:throwing exception ");
					logger.error("decodeTLNSQuery:Digit[Billing Number] -->NOA should be national. ");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("Digit[Billing Number] -->NOA should be national.");
				}

				// check for Digit[Billing Number] -->Encoding/NumberingPlan
				if (input[22] != (byte) 0x11 && input[22] != (byte) 0x01) {
					logger.info("decodeTLNSQuery:throwing exception ");
					logger.error("decodeTLNSQuery:Digit[Billing Number] -->Encoding/NumberingPlan at pos 22 should be correct. ");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("Digit[Billing Number] -->Encoding/NumberingPlan at pos 22 should be correct.");
				}
				// num digit length
				int tlnsNumdigilenBillingNum = input[23] & 0xFF;
				if (tlnsNumdigilenBillingNum > 15 || tlnsNumdigilenBillingNum < 10) {
					logger.info("decodeTLNSQuery:throwing exception ");
					logger.error("decodeTLNSQuery:Digit[Billing Number] -->num digit length should be correct. ");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("Digit[Billing Number] -->num digit length should be correct.");
				}

				// set Billing Number digit
				AinDigits aindigitcalled = new AinDigits();
				// Creating format for AIN digit decoder
				
				if (tlnsNumdigilenBillingNum % 2 == 0) {
					tlnsArrsizeForBilling = (tlnsNumdigilenBillingNum / 2);
				} else {
					tlnsArrsizeForBilling = (tlnsNumdigilenBillingNum + 1) / 2;
				}
				byte[] inputfrDecodeTlnsBillingNum = new byte[tlnsArrsizeForBilling + 2];
				inputfrDecodeTlnsBillingNum[0] = input[21]; // adding NOA-->called
				for (int i = 1; i < inputfrDecodeTlnsBillingNum.length; i++) {
					// adding num of digit and bcd format of digit
					inputfrDecodeTlnsBillingNum[i] = input[22 + i];
				}
				
				// getting Billing Number
				AinDigits tlnsBillingNum = aindigitcalled.decodeAinDigits(inputfrDecodeTlnsBillingNum, Constant.CALLED);
				// perform operation tlnsBillingNum 
				PhoneNumber billngNumber = AinScfProtocolParser.parseCalledPartyNum(tlnsBillingNum);
				legData.set(LegDataAttributes.CC1_BILLING_NUM, billngNumber);
			} else if (input[20] == (byte) 0x02) {
				// check for Digit[Calling Number] -->NOA
				if (input[21] != (byte) 0x00 && input[21] != (byte) 0x01) {
					logger.info("decodeTLNSQuery:throwing exception ");
					logger.error("decodeTLNSQuery:Digit[Calling Number] -->NOA at pos 21 should be correct. ");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("Digit[Calling Number] -->NOA at pos 21 should be correct.");
				}

				// check for Digit[Calling Number] -->Encoding/NumberingPlan
				if (input[22] != (byte) 0x11) {
					logger.info("decodeTLNSQuery:throwing exception ");
					logger.error("decodeTLNSQuery:Digit[Calling Number] -->Encoding/NumberingPlan at pos 22 should be correct. ");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("Digit[Calling Number] -->Encoding/NumberingPlan at pos 22 should be correct.");
				}
				// num digit length
				int numdigilenClngNum = input[23] & 0xFF;
				if (numdigilenClngNum > 15 || numdigilenClngNum < 10) {
					logger.info("decodeTLNSQuery:throwing exception ");
					logger.error("decodeTLNSQuery:Digit[Calling Number] -->num digit length should be correct. ");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("Digit[Calling Number] -->num digit length should be correct.");
				}

				// set Calling Number digit
				AinDigits aindigitCalling = new AinDigits();
				// Creating format for AIN digit decoder
				int arrsizeForCalling = 0;
				if (numdigilenClngNum % 2 == 0) {
					arrsizeForCalling = (numdigilenClngNum / 2);
				} else {
					arrsizeForCalling = (numdigilenClngNum + 1) / 2;
				}
				byte[] inputfrDecodeAinCalling = new byte[arrsizeForCalling];
				//inputfrDecodeAinCalling[0] = input[21]; // adding NOA-->billNum
				int noaCalling = input[21]; // adding NOA-->billNum
				for (int i = 0; i < arrsizeForCalling; i++) {
					// adding num of digit and bcd format of digit
					inputfrDecodeAinCalling[i] = input[24 + i];
				}
				// getting Calling Number num
				aindigitCalling.setAddrSignal(decodeAdrsSignalForTlns(inputfrDecodeAinCalling, 0, 0));
				PhoneNumber callingNumber = AinScfProtocolParser.parseAinDigits(aindigitCalling, noaCalling);
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);				
				
			}
			
			//Checking Calling Number if Calling Number is present followed by billing number
			if (tlnsArrsizeForBilling > 0) {
				if (input.length > (24 + tlnsArrsizeForBilling)) {
					logger.info("decodeTLNSQuery: Input length is > 24 + CalledPty length, Chekcing for Optional parameters Calling Number");
					
					 currentIndex = 24 + tlnsArrsizeForBilling;
					 currentElementIdLen = 1; // Digits Identifier is of 1 byte length
					 currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
					 
					if (input[24 + tlnsArrsizeForBilling] != (byte) 0x84) {
						logger.info("decodeTLNSQuery:throwing exception ");
						logger.error("decodeTLNSQuery:Digits Id [Calling Number] at pos " + (24 + tlnsArrsizeForBilling) + "should be correct.");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digits Id [Calling Number] at pos " + (24 + tlnsArrsizeForBilling) + "should be correct.");
					}
					int callingDigitLen = input[25 + tlnsArrsizeForBilling] & 0xFF;

					//logger.info("decodeTLNSQuery: input length is " +input.length + ". Checking for Calling Number, tlnsArrsizeForBilling " +tlnsArrsizeForBilling );
					if (input[tlnsArrsizeForBilling + 26] != 0x02) {
						logger.info("decodeTLNSQuery:throwing exception ");
						logger.error("decodeTLNSQuery:Digit[Calling Num] should be correct.");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Calling Num] should be correct.");
					}

					// check for Digit[Calling Number] -->NOA
					if (input[tlnsArrsizeForBilling + 27] != (byte) 0x00 && input[27] != (byte) 0x01) {
						logger.info("decodeTLNSQuery:throwing exception ");
						logger.error("decodeTLNSQuery:Digit[Biiling Num] -->NOA should be correct. ");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Calling Num] -->NOA should be correct.");
					}

					// check for Digit[Calling Number] -->Encoding/NumberingPlan
					if (input[tlnsArrsizeForBilling + 28] != (byte) 0x11 ) {
						logger.info("decodeTLNSQuery:throwing exception ");
						logger.error("decodeTLNSQuery:Digit[Calling Number] -->Encoding/NumberingPlan at pos " + (tlnsArrsizeForBilling + 28) + " should be correct. ");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Calling Number] -->Encoding/NumberingPlan at pos " + (tlnsArrsizeForBilling + 28) + " should be correct.");
					}

					int numdigilenCalling = input[tlnsArrsizeForBilling + 29] & 0xFF;

					if (numdigilenCalling > 15 || numdigilenCalling < 10) {
						logger.info("decodeTLNSQuery:throwing exception ");
						logger.error("decodeTLNSQuery:Digit[Calling Num] -->num digit length should be correct. ");
						logger.info("decodeTLNSQuery:Calling setApplicationErrorProblemData method to set problem data for return error");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Calling Num] -->num digit length should be correct.");
					}
					AinDigits aindigitCalling = new AinDigits();
					// Creating format for AIN digit decoder
					int arrsizeCalling;
					if (numdigilenCalling % 2 == 0) {
						arrsizeCalling = (numdigilenCalling / 2);
					} else {
						arrsizeCalling = (numdigilenCalling + 1) / 2;
					}
					byte[] inputfrDecodeAinCalling = new byte[arrsizeCalling];
					int noaCalling = input[tlnsArrsizeForBilling + 27];
					for (int i = 0; i < inputfrDecodeAinCalling.length; i++) {
						// adding num of digit and bcd format of digit
						inputfrDecodeAinCalling[i] = input[tlnsArrsizeForBilling + 30 + i];
					}
					aindigitCalling.setAddrSignal(decodeAdrsSignalForTlns(inputfrDecodeAinCalling, 0, 0));
					PhoneNumber callingNumber = AinScfProtocolParser.parseAinDigits(aindigitCalling, noaCalling);
					legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
				}
			}
		}
		
		
	}

	/**
	 * method to encode the TLNS Query
	 * 
	 * @param callData
	 * @return byte[]
	 * @throws InvalidInputException, AINCodecException
	 */
	public static byte[] encodeTlnssQuery(CallData callData) throws InvalidInputException, AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeTlnsQuery:Enter");
		}

		// encode and set the data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int index = 0;
		// total length of output array
		int totalLength = 0;
		// totalLength += TlnsConstants.TLNS_PARAM_SET_ID_LEN;

		// company id should be long value in string format
		// Row O, P Q
		if (leg2Data.get(LegDataAttributes.P_TLNS_M_COMPANY_ID) == null) {
			logger.error("encodeTlnsQuery:parameter companyId is Mandatory");
			throw new AINCodecException("parameter companyId is Mandatory");
		}

		String TlnsCompId = (String) leg2Data.get(LegDataAttributes.P_TLNS_M_COMPANY_ID);
		//byte[] companyId = decimalToBcdForTlns(TlnsCompId);
		byte[] companyId = AinScfProtocolFieldCodec.encodeDigitsInBcdFormat(TlnsCompId);
		if (companyId.length != 2) {
			logger.error("encodeTlnsQuery : company id length should be 2");
			throw new AINCodecException("company id length should be 2");
		}

		// increment length by 5
		totalLength += TlnsConstants.TLNS_COMPANY_ID_LEN;

		// Record Status Indicator, Row R, S, T
		if (leg2Data.get(LegDataAttributes.P_TLNS_M_RECORD_STATUS_INDICATOR) == null) {
			logger.error("encodeTlnsQuery:parameter record status indicator is Mandatory");
			throw new AINCodecException("parameter record status indicator is Mandatory");
		}

		int recStatInd = (Integer) leg2Data.get(LegDataAttributes.P_TLNS_M_RECORD_STATUS_INDICATOR);
		//int recStatIndInt = Integer.parseInt(recStatInd);
		//byte[] encodedRecStatInd = asciToHex(recStatInd);
		byte[] encodedRecStatInd = CommonUtils.formatIntToByte(recStatInd);
		if (encodedRecStatInd.length != 1) {
			logger.info("encodeTlnsQuery:throwing exception ");
			logger.error("encodeTlnsQuery:parameter record status indicator length should be 1");
			throw new AINCodecException("parameter record status indicatorlength should be 1");
		}

		// Increment length by 4
		totalLength += TlnsConstants.TLNS_REC_STATUS_IND_LEN;

		//terminating line indicator - Row U, V, W
		if (leg2Data.get(LegDataAttributes.P_TLNS_M_TERMINATING_LINE_INDICATORS) == null) {
			logger.error("encodeTlnsQuery:parameter terminating line indicator is Mandatory");
			throw new AINCodecException("parameter terminating line indicator is Mandatory");
		}
		
		int terminatingLineInd = (Integer) leg2Data
				.get(LegDataAttributes.P_TLNS_M_TERMINATING_LINE_INDICATORS);
		byte[] encodeterminatingLineInd = CommonUtils.formatIntToByte(terminatingLineInd);
		totalLength += TlnsConstants.TLNS_COL_TERM_LINE_IND_LEN;

		// message delivery Indicator , Operator Verification and Interrupt IndicatorNonpublished Number Callback Indicator elements
		// are not mentioned in GR 1149, but mentioned in GR 1158. So not adding them

		// Treatment Indicator, Row AA, AB, AC
		if (leg2Data.get(LegDataAttributes.P_TLNS_M_TREATMENT_INDICATOR) == null) {
			logger.error("encodeTlnsQuery:parameter treatment indicator is Mandatory");
			throw new AINCodecException("parameter treatment indicator is Mandatory");
		}

		int trtmntInd = (Integer) leg2Data.get(LegDataAttributes.P_TLNS_M_TREATMENT_INDICATOR);
		//int trtmntIndInt = Integer.parseInt(trtmntInd);
		byte[] encodedTrtmntInd = CommonUtils.formatIntToByte(trtmntInd);
		//byte[] encodedTrtmntInd = asciToHex(trtmntInd);
		totalLength += TlnsConstants.TLNS_TRTMNT_IND_LEN;

		// Service or equipment indicator, Row AD, AE, AF
		if (leg2Data.get(LegDataAttributes.P_TLNS_M_SERVICE_OR_EQUIPMENT_INDICATOR) == null) {
			logger.error("encodeTlnsQuery:parameter service or equipment indicator is Mandatory");
			throw new AINCodecException("parameter service or equipment indicator is Mandatory");
		}

		int serOrEquInd = (Integer) leg2Data.get(LegDataAttributes.P_TLNS_M_SERVICE_OR_EQUIPMENT_INDICATOR);
		//int serOrEquIndInt = Integer.parseInt(serOrEquInd);
		byte[] encodedSerOrEquInd = CommonUtils.formatIntToByte(serOrEquInd);
		//byte[] encodedSerOrEquInd = asciToHex(serOrEquInd);
		totalLength += TlnsConstants.TLNS_SER_EQP_IND_LEN;

		// Intercept Indicator, Row AG, AH, AI
		if (leg2Data.get(LegDataAttributes.P_TLNS_M_INTERCEPT_INDICATOR) == null) {
			logger.error("encodeTlnsQuery:parameter intercept indicator is Mandatory");
			throw new AINCodecException("parameter intercept indicator is Mandatory");
		}

		int inrcptInd = (Integer) leg2Data.get(LegDataAttributes.P_TLNS_M_INTERCEPT_INDICATOR);
		//int inrcptIndInt = Integer.parseInt(inrcptInd);
		byte[] encodedinrcptInd = CommonUtils.formatIntToByte(inrcptInd);
		//byte[] encodedinrcptInd = asciToHex(inrcptInd);
		totalLength += TlnsConstants.TLNS_INTERCPT_IND_LEN;

		// IC Indicator ,Row -AM, AN, AO
		if (leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_APIC) == null
				|| leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PPIC) == null
				|| leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PINC) == null) {
			logger.error(
					"encodeTlnsQuery:parameter ic indicators (APIC, PPIC & PINC) is Mandatory..found NULL setting to 0");
			// throw new AINCodecException("parameter ic indicators (APIC, PPIC & PINC) is
			// Mandatory");
		}
		totalLength += TlnsConstants.TLNS_IC_INDICATOR_LEN;
		int icIndicatorAPIC = leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_APIC) == null ? 0
				: (Integer) leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_APIC);
		int icIndicatorPPIC = leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PPIC) == null ? 0
				: (Integer) leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PPIC);
		int icIndicatorAPIC_PPIC = (icIndicatorAPIC & 0x07) << 3;
		icIndicatorAPIC_PPIC |= (icIndicatorPPIC & 0x07);

		int icIndicatorPINC = leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PINC) == null ? 0
				: (Integer) leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PINC);

		byte[] byteIcIndicator = { (byte) icIndicatorAPIC_PPIC, (byte) icIndicatorPINC };

		// Preferred IC Digits ,Row AP, AQ, AR
		PhoneNumber primaryPreferedIcDigits = null;

		if (leg2Data.get(LegDataAttributes.P_TLNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER) != null){
			primaryPreferedIcDigits = (PhoneNumber) leg2Data
			.get(LegDataAttributes.P_TLNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER);
			
			if(!StringUtils.isNotBlank(primaryPreferedIcDigits.getAddress())){
				primaryPreferedIcDigits = null;
			}	
		}

		// Alternate IC Digits Row AS, AT, AU
		PhoneNumber altPrefIcDigits = null;
		if (leg2Data.get(LegDataAttributes.P_TLNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER) != null){
			altPrefIcDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_TLNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER);
			
			if(!StringUtils.isNotBlank(altPrefIcDigits.getAddress())){
				altPrefIcDigits = null;
			}	
		}

		// Preferred INC Digits Row AV, AW, AX
		PhoneNumber prefIncDigits = null;
		if (leg2Data.get(LegDataAttributes.P_TLNS_O_PREF_INC_DIGIT_IDENTIFIER) != null){
			prefIncDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_TLNS_O_PREF_INC_DIGIT_IDENTIFIER);
			
			if(!StringUtils.isNotBlank(prefIncDigits.getAddress())){
				prefIncDigits = null;
			}
		}

		// Reference Number Row AY, AZ, BA
		PhoneNumber refNum = null;
		if (leg2Data.get(LegDataAttributes.P_TLNS_O_REF_NUM) != null) {
			refNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_TLNS_O_REF_NUM);
			
			if(!StringUtils.isNotBlank(refNum.getAddress())){
				refNum = null;
			}else{
				// as per standard 
				// National presentation allowed - 0
				// International presentation allowed - 1
				if (leg2Data.get(LegDataAttributes.P_TLNS_O_REF_NUM_TYPE) == null) {
					logger.error("encodeTlnsQuery:Referral number type is missing");
					throw new AINCodecException("parameter Referral number type is missing");
				}
				int refNumType = (Integer) leg2Data.get(LegDataAttributes.P_TLNS_O_REF_NUM_TYPE);
				refNum.setNatureOfAddress(refNumType);
			}
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
			encodedPrimaryPreferedIcDigits = encodeAdrsSignalForTlns(primaryPreferedIcDigits.getAddress());
			encodedPrimaryPreferedIcDigitsNumDigit = (byte) encodedPrimaryPreferedIcDigits.length;
			totalLength += TlnsConstants.TLNS_PRIM_PREF_IC_LEN;
		}
		if (altPrefIcDigits != null) {
			encodedAltPrefIcDigits = encodeAdrsSignalForTlns(altPrefIcDigits.getAddress());
			encodedAltPrefIcDigitsNumDigit = (byte) encodedAltPrefIcDigits.length;
			totalLength += TlnsConstants.TLNS_ALT_PREF_IC_LEN;
		}
		if (prefIncDigits != null) {
			encodedPrefIncDigits = encodeAdrsSignalForTlns(prefIncDigits.getAddress());
			encodedPrefIncDigitsNumDigit = (byte) encodedPrefIncDigits.length;
			totalLength += TlnsConstants.TLNS_PREF_INC_LEN;
		}
		if (refNum != null) {
			encodedRefNum = encodeAdrsSignalForTlns(refNum.getAddress());
			encodedRefNumNumDigit = (byte) encodedRefNum.length;
			totalLength += encodedRefNum.length;
			totalLength += TlnsConstants.TLNS_REF_DIFIT_IDENTIFIER_LEN;
		}

		
		//output array
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

		//encodeterminatingLineInd
		outputArray[index++] = TERMINATING_LINE_INDICATOR_ID_TAG1;
		outputArray[index++] = TERMINATING_LINE_INDICATOR_ID_TAG2;
		outputArray[index++] = TERMINATING_LINE_INDICATOR_LENGTH;
		for (byte encodedVal : encodeterminatingLineInd) {
			outputArray[index++] = encodedVal;
		}
		
		// Treatment Indicator
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

		return outputArray;
	}


	/**
	 * This method is used for decoding billing Number Address Signal
	 * 
	 * @param data   represents 5 octet BCD input
	 * @param offcet and @param parity are set to 0 for billing Number
	 * @return a String
	 */
	private static String decodeAdrsSignalForTlns(byte[] data, int offset, int parity) throws InvalidInputException {
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
	private static byte[] encodeAdrsSignalForTlns(String addrSignal) throws InvalidInputException {

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
	public static byte[] getApplicationErrorForTlns(CallData callData) {
		// error code will be in leg2
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int totalLength = 0;
		int index = 0;
		totalLength += TlnsConstants.TOTAL_FIXED_LEN_APP_ERR;

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
			logger.debug("Application Error for TLNS:" + CommonUtils.formatBytes(buffer));
		}
		return buffer;
	}

	//	public static byte[] getProtocolErrorForTlns(CallData callData) { // Error
	//		// code will be in leg1 LegData leg2Data = (LegData)
	//		logger.info("inside : getProtocolErrorForTlns");
	//		callData.get(CallDataAttribute.P_LEG2);
	//		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
	//
	//		int totalLength = 6;
	//		int index = 0;  
	//
	//		int probType = 0;
	//		byte[] encodedProblemType = null;
	//		if (leg2Data.get(LegDataAttributes.P_TLNS_PROTOCOL_ERR_PROBLEM_TYPE) != null) {
	//			probType = (Integer.parseInt((String) leg2Data.get(LegDataAttributes.P_TLNS_PROTOCOL_ERR_PROBLEM_TYPE)));
	//			encodedProblemType = CommonUtils.formatIntToByte(probType);
	//		}
	//		logger.info("encodedProblemType ::"+CommonUtils.formatBytes(encodedProblemType));
	//
	//		int probSpec = 0;
	//		byte[] encodedProblemSpecifier = null;
	//		if (leg2Data.get(LegDataAttributes.P_TLNS_PROTOCOL_ERR_PROBLEM_SPECIFIER) != null) {
	//			probSpec = (Integer.parseInt((String) leg2Data
	//					.get(LegDataAttributes.P_TLNS_PROTOCOL_ERR_PROBLEM_SPECIFIER)));
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
	private static byte[] encodeTlnsAINDigits(PhoneNumber ph) throws InvalidInputException {

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
	public static byte[] decimalToBcdForTlns(String input) {
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
	public static void resetTLNSQuery(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetTLNSQuery:Enter");
		}
		
		//resetting leg1Data
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

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
		
		if(legData.get(LegDataAttributes.P_CALLED_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLED_PARTY);
		}
		
		if(legData.get(LegDataAttributes.CC1_BILLING_NUM) != null) {
			legData.remove(LegDataAttributes.CC1_BILLING_NUM);
		}
		
		if(legData.get(LegDataAttributes.P_CALLING_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLING_PARTY);
		}

		//resetting leg2Data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2); 
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_COMPANY_ID) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_COMPANY_ID);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_RECORD_STATUS_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_RECORD_STATUS_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_TERMINATING_LINE_INDICATORS) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_TERMINATING_LINE_INDICATORS);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_TREATMENT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_TREATMENT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_SERVICE_OR_EQUIPMENT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_SERVICE_OR_EQUIPMENT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_INTERCEPT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_INTERCEPT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_APIC) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_IC_INDICATOR_APIC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PPIC) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PPIC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PINC) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_M_IC_INDICATOR_PINC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_O_PRIM_PREF_IC_DIGIT_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_O_ALT_PREF_IC_DIGIT_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_O_PREF_INC_DIGIT_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_O_PREF_INC_DIGIT_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_O_REF_NUM) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_O_REF_NUM);
		}
		
		if(leg2Data.get(LegDataAttributes.P_TLNS_O_REF_NUM_TYPE) != null) {
			leg2Data.remove(LegDataAttributes.P_TLNS_O_REF_NUM_TYPE);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
		}
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE);
		}
		if (logger.isDebugEnabled()) {
			logger.info("resetTLNSQuery:Exit");
		}
		
	}

}
