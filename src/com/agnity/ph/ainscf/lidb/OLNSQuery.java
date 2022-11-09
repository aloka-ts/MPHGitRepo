package com.agnity.ph.ainscf.lidb;

import java.nio.charset.StandardCharsets;

import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.util.Constant;
import com.agnity.ain.util.Util;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.ainscf.AinScfProtocolParser;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;

/**
 * This Class is used to encode and decode OLNS Query
 * 
 * @author Krishna
 */
public class OLNSQuery {

	private OLNSQuery() {
	}

	private static Logger logger = Logger.getLogger(OLNSQuery.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
	'f' };

	//  Test Buffer for decodeOLNSQuery ()
	//	byte[] testBuf = { (byte) 0xF2,  
	//			(byte) 0x26,  
	//			(byte) 0xDF, (byte) 0x6F, 
	//			(byte) 0x00,  
	//			(byte) 0xAA,  
	//			(byte) 0x0B,  
	//			(byte) 0x84,  
	//			(byte) 0x09,  
	//			(byte) 0x05, (byte) 0x00, (byte) 0x11, (byte) 0x0A, 0x21, 0x43, 0x65, (byte) 0x87, 0x09, // Digits
	//			(byte) 0x84,  
	//			(byte) 0x09,  
	//			(byte) 0x01, 0x00, (byte) 0x11, 0x0A, 0x18, 0x17, 0x21, 0x00, 0x76, // Digits
	//			(byte) 0x84, 
	//			(byte) 0x09,  
	//			(byte) 0x05, 0x00, (byte) 0x11, 0x0A, 0x79, 0x12, 0x70, 0x76, 0x24 // Digits
	//	};
	//		Test Data for encodeOLNSQuery   
	//	CallData callData = new CallData();
	//	LegData legData = new LegData();
	//	callData.set(CallDataAttribute.P_LEG2, legData);
	//	LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
	//	
	//	leg2Data.set(LegDataAttributes.P_OLNS_M_TREATMENT_INDICATOR, 1);
	//	leg2Data.set(LegDataAttributes.P_OLNS_M_SERV_EQP_INDICATOR, 2);
	//	leg2Data.set(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OIC, 0);
	//	leg2Data.set(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OINC, 0);
	//	leg2Data.set(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND1, 0);
	//	leg2Data.set(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND2, 0);
	//	leg2Data.set(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND3, 0);
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND1, 0);
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND2, 0);
	//	PhoneNumber phnIcDigits = new PhoneNumber();
	//	phnIcDigits.setAddress("5678");
	//	PhoneNumber phnIncDigits = new PhoneNumber();
	//	phnIncDigits.setAddress("1234");
	//	PhoneNumber phnDisalwd = new PhoneNumber();
	//	phnDisalwd.setAddress("1234567890");
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_ORIG_IC_DIGITS, phnIcDigits);
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_ORIG_INC_DIGITS, phnIncDigits);
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_FOREIGN_LANG_ID, 3);
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_GENERIC_NAME, "Vikas");
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_GENERIC_NAME_AVAILABILITY, 0);
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_GENERIC_NAME_PRESENTATION, 1);
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_ALPHANUMERIC_STRING, "Alpha");
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_DISALLOWED_CARD_ISSUER_CODE, phnDisalwd);
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_ACC_OWNER, "AAAAAAAA");
	//	leg2Data.set(LegDataAttributes.P_OLNS_O_BILING_SER_PROV, "1AAAAAA1");

	public static void decodeOLNSQuery(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException,ParameterOutOfRangeException {
		if (logger.isInfoEnabled()) {
			logger.info("decodeOLNSQuery:Enter");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.OLNS.name());

		if (logger.isInfoEnabled()) {
			logger.info("decodeOLNSQuery:set P_LIDB_QUERY_TYPE ==OLNS");
		}

		// Parameter Set ID Row P
		int currentIndex = 0;
		int currentElementIdLen = 1; // number of bytes of current element Identifier
		int currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[0] != (byte) 0xF2 && input[0] != (byte)0x31) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:parameter set ID is not correct");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("parameter set ID is not correct");
		}

		// check for parameter set length. Row Q
		if (input.length <= 16) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:input buffer can not be less then 16 ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Length of the buffer must be Greater then 16");
		}

		// check for OLNS Info ID Row R
		currentIndex = 2;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[2] != (byte) 0xDF || input[3] != (byte) 0x6F) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:OLNS info id is not correct ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("OLNS info id is not correct");
		}

		// check for OLNS Info length. Row S
		if (input[4] != (byte) 0x00) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:OLNS info length should be zero ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("OLNS info length should be zero");
		}
		// Service Key Identifier Row-T
		currentIndex = 5;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[5] != (byte) 0xAA) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:Service key ID is not correct ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Service key ID is not correct ");
		}
		// Service key Length Row-U
		if (input[6] != (byte) 0x0B) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:Service key Length is not correct ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Service key Length is not correct ");
		}

		// check for Digits Id [OLNS Calling Directory Number] Row V, W, X
		currentIndex = 7;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[7] != (byte) 0x84) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:Digits Id [OLNS] is not correct ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Id [OLNS] is not correct ");
		}

		// check for Digits Length [OLNS] //9 Row W
		if (input[8] != (byte) 0x09) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:Digits Length [OLNS] is not correct ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Length [OLNS] is not correct ");
		}

		// check for digits Row X
		if(input[9] != (byte) 0x02 && input[9] != (byte) 0x0B) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:Digits Type should be either OLNS Calling Directory Number or ANI Calling");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Type should be correct.");			
		}
		byte digitType = input[9];
		legData.set(LegDataAttributes.P_OLNS_SERVICEKEY_DIGITS_TYPE, digitType); //This will be used only if ACG component is encoded, to set the digit type for ACG controlledCode.
		if (input[10] != (byte) 0x00 || input[11] != (byte) 0x11 || input[12] != (byte) 0x0A) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:Digits[OLNS Calling Directory Number] should be correct. ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits[OLNS Calling Directory Number] should be correct.");
		}

		int digitLen = input[8] & 0xFF;
		if (digitLen != 9) {
			logger.info("decodeOLNSQuery:throwing exception ");
			logger.error("decodeOLNSQuery:Digit Length should be correct. ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit Length should be correct.");
		}
		int arrsize;
		if (digitLen % 2 == 0) {
			arrsize = (digitLen / 2);
		} else {
			arrsize = (digitLen + 1) / 2;
		}
		AinDigits ainDigitForOLNS = new AinDigits();
		byte[] inputfrDecodeAin = new byte[arrsize];

		for (int i = 0; i < inputfrDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAin[i] = input[13 + i];
		}
		int natureOfOLNSDigits = input[10] & 0xFF;
		ainDigitForOLNS.setAddrSignal(decodeAdrsSignalForOLNS(inputfrDecodeAin, 0, 0));
		PhoneNumber phNumber = AinScfProtocolParser.parseAinDigits(ainDigitForOLNS, natureOfOLNSDigits);
		legData.set(LegDataAttributes.P_CALLING_PARTY, phNumber);
		// set value in legData

		if (input.length > 18) {
			logger.info("decodeOLNSQuery: Input length is > 18, Chekcing for Optional parameters Called Party/Billing number");
			// check for Digits Id [Called Party] Row Y, Z, AA
			currentIndex = 18;
			currentElementIdLen = 1; // number of bytes of current element Identifier
			currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
			int arrsizeForCalled = 0;

			if (input[18] != (byte) 0x84 && input[18] != (byte) 0xDF) {
				logger.info("decodeOLNSQuery:throwing exception ");
				logger.error("decodeOLNSQuery:Digits Identifier[Called Number/Billing Number) or PSRID is expected at pos 18.");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digits Identifier[Called Number/Billing Number) or PSRID is expected at pos 18");
			}

			if (input[18] == (byte) 0x84) { //if first optional parameter is a digit identifier for called number/billing number
				// operation to perform on Digits Id [Called Party]
				// get the Digits[Called Party] Length
				int cldPartyDigitLen = input[19] & 0xFF;

				// * format for Digits[Called Party]--> d0x01 [NOA] 0x11 [Num of Dig] [BCD
				// Format]
				// * [Num of Dig] 10 - National Max 15 - International check for Digit[Called
				// * Party]

				//checking if digit type is either called party or billing number since both are optional parameters
				if (input[20] != (byte) 0x01 && input[20] != (byte) 0x05) {
					logger.info("decodeOLNSQuery:throwing exception ");
					logger.error("decodeOLNSQuery:Digit should be either Called Party or Billing Number at pos 20");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("Digit should be either Called Party or Billing Number at pos 20");
				}

				// if called party digits type
				if (input[20] == (byte) 0x01) {
					// check for Digit[Called Party] -->NOA
					if (input[21] != (byte) 0x00 && input[21] != (byte) 0x01) {
						logger.info("decodeOLNSQuery:throwing exception ");
						logger.error("decodeOLNSQuery:Digit[Called Party] -->NOA should be correct. ");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Called Party] -->NOA should be correct.");
					}

					// check for Digit[Called Party] -->Encoding/NumberingPlan
					if (input[22] != (byte) 0x11 ) {
						logger.info("decodeOLNSQuery:throwing exception ");
						logger.error("decodeOLNSQuery:Digit[Called Party] -->Encoding/NumberingPlan at pos 22 should be correct. ");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Called Party] -->Encoding/NumberingPlan at pos 22 should be correct.");
					}
					// num digit length
					int numdigilenCalledDir = input[23] & 0xFF;
					if (numdigilenCalledDir > 15 || numdigilenCalledDir < 10) {
						logger.info("decodeOLNSQuery:throwing exception ");
						logger.error("decodeOLNSQuery:Digit[Called Party] -->num digit length should be correct. ");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Called Party] -->num digit length should be correct.");
					}

					// set called party digit
					AinDigits aindigitcalled = new AinDigits();
					// Creating format for AIN digit decoder

					if (numdigilenCalledDir % 2 == 0) {
						arrsizeForCalled = (numdigilenCalledDir / 2);
					} else {
						arrsizeForCalled = (numdigilenCalledDir + 1) / 2;
					}
					byte[] inputfrDecodeAinCalled = new byte[arrsizeForCalled + 2];
					inputfrDecodeAinCalled[0] = input[21]; // adding NOA-->called
					for (int i = 1; i < inputfrDecodeAinCalled.length; i++) {
						// adding num of digit and bcd format of digit
						inputfrDecodeAinCalled[i] = input[22 + i];
					}
					// getting called party num
					AinDigits calledPartyNum = aindigitcalled.decodeAinDigits(inputfrDecodeAinCalled, Constant.CALLED);
					// perform operation calledPartyNum :vikas singh
					PhoneNumber calledNumber = AinScfProtocolParser.parseCalledPartyNum(calledPartyNum);
					legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);
				} else if (input[20] == (byte) 0x05) {
					// check for Digit[Billing Number] -->NOA
					if (input[21] != (byte) 0x00) {
						logger.info("decodeOLNSQuery:throwing exception ");
						logger.error("decodeOLNSQuery:Digit[Billing Number] -->NOA at pos 21 should be correct. ");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Billing Number] -->NOA at pos 21 should be correct.");
					}

					// check for Digit[Billing Number] -->Encoding/NumberingPlan
					if (input[22] != (byte) 0x11 && input[22] != (byte) 0x01) {
						logger.info("decodeOLNSQuery:throwing exception ");
						logger.error("decodeOLNSQuery:Digit[Billing Number] -->Encoding/NumberingPlan at pos 22 should be correct. ");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Billing Number] -->Encoding/NumberingPlan at pos 22 should be correct.");
					}
					// num digit length
					int numdigilenBillNum = input[23] & 0xFF;
					if (numdigilenBillNum > 15 || numdigilenBillNum < 10) {
						logger.info("decodeOLNSQuery:throwing exception ");
						logger.error("decodeOLNSQuery:Digit[Billing Number] -->num digit length should be correct. ");
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Digit[Billing Number] -->num digit length should be correct.");
					}

					// set Billing Number digit
					AinDigits aindigitBill = new AinDigits();
					// Creating format for AIN digit decoder
					int arrsizeForBill = 0;
					if (numdigilenBillNum % 2 == 0) {
						arrsizeForBill = (numdigilenBillNum / 2);
					} else {
						arrsizeForBill = (numdigilenBillNum + 1) / 2;
					}
					byte[] inputfrDecodeAinBill = new byte[arrsizeForBill];
					//inputfrDecodeAinBill[0] = input[21]; // adding NOA-->billNum
					int noaBill = input[21]; // adding NOA-->billNum
					for (int i = 0; i < arrsizeForBill; i++) {
						// adding num of digit and bcd format of digit
						inputfrDecodeAinBill[i] = input[24 + i];
					}
					// getting Billing Number num
					aindigitBill.setAddrSignal(decodeAdrsSignalForOLNS(inputfrDecodeAinBill, 0, 0));
					PhoneNumber bilingNumber = AinScfProtocolParser.parseAinDigits(aindigitBill, noaBill);
					legData.set(LegDataAttributes.P_OLNS_O_BILLING_NUM, bilingNumber);				

				}
			} else if (input[18] == (byte) 0xDF) {
				// if PSRID parameter is present as the first optional parameter
				if (input[18] != (byte) 0xDF || input[19] != (byte) 0xC1 || input[20] != (byte) 0x23) {
					logger.info("decodeOLNSQuery:throwing exception ");
					logger.error("decodeOLNSQuery:primary Service Requester ID is not correct");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("primary Service Requester ID is not correct");
				}
				// primary Service Requester ID Length
				if (input[21] != (byte) 0x05) {
					logger.info("decodeOLNSQuery:throwing exception ");
					logger.error("decodeOLNSQuery:primary Service Requester ID Length is not correct");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new ParameterOutOfRangeException("primary Service Requester ID Length is not correct");
				}

				int psridContext = input[22] & 0xFF;
				if (psridContext != 0x09 && psridContext != 0x0A) {
					logger.info("decodeOLNSQuery:throwing exception ");
					logger.error("decodeOLNSQuery:primary Service Requester ID spare is not correct");
					AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
					throw new AINCodecException("primary Service Requester ID spare is not correct");
				}

				int psridType = psridContext & 0x07; // extract lower 3 digits
				legData.set(LegDataAttributes.OLNS_O_PSRID_TYPE, psridType);
				byte[] primaryServiceRequesterIdtable = { input[23], input[24], input[25], input[26] };
				String primaryServiceRequesterId = new String(primaryServiceRequesterIdtable);
				legData.set(LegDataAttributes.OLNS_O_PRIMARY_SERVICE_REQUESTER, primaryServiceRequesterId);

			}

			//Checking billed Number if called party is present followed by billing number
			if (arrsizeForCalled > 0) { // i.e if Called Number parameter is present
				if (input.length > (24 + arrsizeForCalled)) {
					currentIndex = 24 + arrsizeForCalled;
					currentElementIdLen = 1; // number of bytes of current element Identifier
					currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
					int BillingDigitLen = 0;

					logger.info("decodeOLNSQuery: Input length is > 24 + CalledPty length, Chekcing for Optional parameters Billing number/PSRID");
					if (input[24 + arrsizeForCalled] != (byte) 0x84 && input[24 + arrsizeForCalled] != (byte) 0xDF) {
						logger.info("decodeOLNSQuery:throwing exception ");
						logger.error("decodeOLNSQuery:Either Digits Id [Billing number] or PSRID expected at pos " + (24 + arrsizeForCalled));
						AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
						throw new AINCodecException("Either Digits Id [Billing number] or PSRID expected at pos " + (24 + arrsizeForCalled));
					}

					if (input[24 + arrsizeForCalled] == (byte) 0x84) {
						logger.info("decodeOLNSQuery: Optional parameters Billing number is present");
						BillingDigitLen = input[25 + arrsizeForCalled] & 0xFF;

						//logger.info("decodeOLNSQuery: input length is " +input.length + ". Checking for billing number, arrsizeForCalled " +arrsizeForCalled );
						if (input[arrsizeForCalled + 26] != 0x05) {
							logger.info("decodeOLNSQuery:throwing exception ");
							logger.error("decodeOLNSQuery:Type of Digit[Billing Num] should be correct.");
							AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
							throw new AINCodecException("Type of Digit[Billing  Num] should be correct.");
						}

						// check for Digit[Called Party] -->NOA
						if (input[arrsizeForCalled + 27] != (byte) 0x00) {
							logger.info("decodeOLNSQuery:throwing exception ");
							logger.error("decodeOLNSQuery:Digit[Biiling Num] -->NOA should be correct. ");
							AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
							throw new AINCodecException("Digit[Billing Num] -->NOA should be correct.");
						}

						// check for Digit[Billing Number] -->Encoding/NumberingPlan
						if (input[arrsizeForCalled + 28] != (byte) 0x11 && input[arrsizeForCalled + 28] != (byte) 0x01) {
							logger.info("decodeOLNSQuery:throwing exception ");
							logger.error("decodeOLNSQuery:Digit[Billing Number] -->Encoding/NumberingPlan at pos " + (arrsizeForCalled + 28) + " should be correct. ");
							AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
							throw new AINCodecException("Digit[Billing Number] -->Encoding/NumberingPlan at pos " + (arrsizeForCalled + 28) + " should be correct.");
						}

						int numdigilenBilling = input[arrsizeForCalled + 29] & 0xFF;

						if (numdigilenBilling > 15 || numdigilenBilling < 10) {
							logger.info("decodeOLNSQuery:throwing exception ");
							logger.error("decodeOLNSQuery:Digit[Billing Num] -->num digit length should be correct. ");
							AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
							throw new AINCodecException("Digit[Billing Num] -->num digit length should be correct.");
						}
						AinDigits aindigitBilling = new AinDigits();
						// Creating format for AIN digit decoder
						int arrsizeBilling;
						if (numdigilenBilling % 2 == 0) {
							arrsizeBilling = (numdigilenBilling / 2);
						} else {
							arrsizeBilling = (numdigilenBilling + 1) / 2;
						}
						byte[] inputfrDecodeAinBilling = new byte[arrsizeBilling];
						int noaBilling = input[arrsizeForCalled + 27];
						for (int i = 0; i < inputfrDecodeAinBilling.length; i++) {
							// adding num of digit and bcd format of digit
							inputfrDecodeAinBilling[i] = input[arrsizeForCalled + 30 + i];
						}
						aindigitBilling.setAddrSignal(decodeAdrsSignalForOLNS(inputfrDecodeAinBilling, 0, 0));
						PhoneNumber bilingNumber = AinScfProtocolParser.parseAinDigits(aindigitBilling, noaBilling);
						legData.set(LegDataAttributes.P_OLNS_O_BILLING_NUM, bilingNumber);

						//checking if PSRID is followed by billing number
						if (input.length > (arrsizeForCalled + 29 + arrsizeBilling + 1) && input[arrsizeForCalled + 29 + arrsizeBilling + 1] == (byte) 0xDF) {
							if (logger.isInfoEnabled()) {
								logger.info("decodeOLNSQuery:PSRID parameter is present in the request");
							}

							int IndexOfPsrId = arrsizeForCalled + 29 + arrsizeBilling + 1;
							// primary Service Requester ID. Note: the correct bytes details for PSRID is not given in gr1149. The values used here is in line with DTSRID identifier with private tcap id 165 (Hex Dx C1 25). So tcap id 163 of PSRID is converted to (Hex Dx C1 23)
							if (input[IndexOfPsrId++] != (byte) 0xDF || input[IndexOfPsrId++] != (byte) 0xC1 || input[IndexOfPsrId++] != (byte) 0x23) {
								logger.info("decodeOLNSQuery:throwing exception ");
								logger.error("decodeOLNSQuery:primary Service Requester ID is not correct");
								AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
								throw new AINCodecException("primary Service Requester ID is not correct");
							}
							// primary Service Requester ID Length
							if (input[IndexOfPsrId++] != (byte) 0x05) {
								logger.info("decodeOLNSQuery:throwing exception ");
								logger.error("decodeOLNSQuery:primary Service Requester ID Length is not correct");
								AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
								throw new ParameterOutOfRangeException("primary Service Requester ID Length is not correct");
							}

							int psridContext = input[IndexOfPsrId++] & 0xFF;
							if (psridContext != 0x09 && psridContext != 0x0A) {
								logger.info("decodeOLNSQuery:throwing exception ");
								logger.error("decodeOLNSQuery:primary Service Requester ID spare is not correct");
								AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
								throw new AINCodecException("primary Service Requester ID spare is not correct");
							}

							int psridType = psridContext & 0x07; // extract lower 3 digits
							legData.set(LegDataAttributes.OLNS_O_PSRID_TYPE, psridType);
							byte[] primaryServiceRequesterIdtable = { input[IndexOfPsrId++], input[IndexOfPsrId++], input[IndexOfPsrId++], input[IndexOfPsrId++] };
							String primaryServiceRequesterId = new String(primaryServiceRequesterIdtable);
							legData.set(LegDataAttributes.OLNS_O_PRIMARY_SERVICE_REQUESTER, primaryServiceRequesterId);

						}
					} else if (input[24 + arrsizeForCalled] == (byte) 0xDF) {
						if (input[arrsizeForCalled + 24] != (byte) 0xDF || input[arrsizeForCalled + 25] != (byte) 0xC1 || input[arrsizeForCalled + 26] != (byte) 0x23) {
							logger.info("decodeOLNSQuery:throwing exception ");
							logger.error("decodeOLNSQuery:primary Service Requester ID is not correct");
							AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
							throw new AINCodecException("primary Service Requester ID is not correct");
						}
						// primary Service Requester ID Length
						if (input[arrsizeForCalled + 27] != (byte) 0x05) {
							logger.info("decodeOLNSQuery:throwing exception ");
							logger.error("decodeOLNSQuery:primary Service Requester ID Length is not correct");
							AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
							throw new ParameterOutOfRangeException("primary Service Requester ID Length is not correct");
						}

						int psridContext = input[arrsizeForCalled + 28] & 0xFF;
						if (psridContext != 0x09 && psridContext != 0x0A) {
							logger.info("decodeOLNSQuery:throwing exception ");
							logger.error("decodeOLNSQuery:primary Service Requester ID spare is not correct");
							AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
							throw new AINCodecException("primary Service Requester ID spare is not correct");
						}

						int psridType = psridContext & 0x07; // extract lower 3 digits
						legData.set(LegDataAttributes.OLNS_O_PSRID_TYPE, psridType);
						byte[] primaryServiceRequesterIdtable = { input[arrsizeForCalled + 29], input[arrsizeForCalled + 30], input[arrsizeForCalled + 31], input[arrsizeForCalled + 32] };
						String primaryServiceRequesterId = new String(primaryServiceRequesterIdtable);
						legData.set(LegDataAttributes.OLNS_O_PRIMARY_SERVICE_REQUESTER, primaryServiceRequesterId);

					}
				}
			}
		}
		if (logger.isInfoEnabled()) {
			logger.info("decodeOLNSQuery:Exit");
		}
	}

	public static byte[] encodeOLNSQuery(CallData callData) throws InvalidInputException, AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeOLNSQuery:Enter");
		}
		// contains decoded data Leg2
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		leg2Data.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.OLNS.name());

		int index = 0;
		// total length of output array
		int totalLength = 0;

		// Parameter Set Identifier Row - M, N
		byte encodedParameterSetIdentifier = OlnsConstants.OLNS_PARAM_SET_ID;
		// Originating Billing Service Indicator Row: O, P, Q
		if (leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND1) == null
				|| leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND2) == null
				|| leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND3) == null) {
			logger.info("encodeOLNSQuery: throwing exception ");
			logger.error("encodeOLNSQuery: orignating billing or service indicator is Mandatory");
			throw new AINCodecException("orignating billing or service indicator is Mandatory");
		}
		byte encodeBillingSerIndTag1 = OlnsConstants.OLNS_ORIG_BILLING_SER_IND_TAG1;
		byte encodeBillingSerIndTag2 = OlnsConstants.OLNS_ORIG_BILLING_SER_IND_TAG2;
		byte encodeBillingSerIndLength = OlnsConstants.OLNS_ORIG_BILLING_SER_IND_LENGTH;
		byte[] encodedOrignatingBillingOrServiceIndicator1 = { 0x00};
		byte[] encodedOrignatingBillingOrServiceIndicator2 = { 0x00};
		byte[] encodedOrignatingBillingOrServiceIndicator3 = { 0x00};

		int orignatingBillingOrServiceIndicator1 = (Integer) leg2Data
				.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND1);
		int orignatingBillingOrServiceIndicator2 = (Integer) leg2Data
				.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND2);
		int orignatingBillingOrServiceIndicator3 = (Integer) leg2Data
				.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND3);

		//		encodedOrignatingBillingOrServiceIndicator1 = CommonUtils.formatIntToByte(orignatingBillingOrServiceIndicator1);
		//		encodedOrignatingBillingOrServiceIndicator2 = CommonUtils.formatIntToByte(orignatingBillingOrServiceIndicator2);
		//		encodedOrignatingBillingOrServiceIndicator3 = CommonUtils.formatIntToByte(orignatingBillingOrServiceIndicator3);
		encodedOrignatingBillingOrServiceIndicator1[0] = (byte) orignatingBillingOrServiceIndicator1;
		encodedOrignatingBillingOrServiceIndicator2[0] = (byte) orignatingBillingOrServiceIndicator2;
		encodedOrignatingBillingOrServiceIndicator3[0] = (byte) orignatingBillingOrServiceIndicator3;

		totalLength += OlnsConstants.OLNS_ORIG_BILLING_OR_SERV_IND_LEN;

		// Treatment indicator Row R, S, T
		if (leg2Data.get(LegDataAttributes.P_OLNS_M_TREATMENT_INDICATOR) == null) {
			logger.info("encodeOLNSQuery: throwing exception ");
			logger.error("encodeOLNSQuery: treatment indicator is Mandatory");
			throw new AINCodecException("treatment indicator is Mandatory");
		}
		byte encodedTreatmentIndicatorTag1 = OlnsConstants.OLNS_TREATMENT_IND_TAG1;
		byte encodedTreatmentIndicatorTag2 = OlnsConstants.OLNS_TREATMENT_IND_TAG2;
		byte encodedTreatmentIndicatorLength = OlnsConstants.OLNS_TREATMENT_IND_LENGTH;
		byte[] encodedTreatmentIndicator = null;
		int treatmentInd = (Integer) leg2Data.get(LegDataAttributes.P_OLNS_M_TREATMENT_INDICATOR);
		encodedTreatmentIndicator = CommonUtils.formatIntToByte(treatmentInd);
		totalLength += OlnsConstants.OLNS_TREATMENT_IND_LENGTH_OCTET;

		// Service or equipment indicator Row- U, V, W
		if (leg2Data.get(LegDataAttributes.P_OLNS_M_SERV_EQP_INDICATOR) == null) {
			logger.info("encodeOLNSQuery: throwing exception ");
			logger.error("encodeOLNSQuery:Service or equipment indicator is Mandatory");
			throw new AINCodecException("Service or equipment indicator is Mandatory");
		}
		byte encodedSerOrEqpIndTag1 = OlnsConstants.OLNS_SERV_EQP_IND_TAG1;
		byte encodedSerOrEqpIndTag2 = OlnsConstants.OLNS_SERV_EQP_IND_TAG2;
		byte encodedSerOrEqpIndLength = OlnsConstants.OLNS_SERV_EQP_IND_LENGTH;
		byte[] encodedSerOrEqpInd = null;
		int serOrEqpInd = (Integer) leg2Data.get(LegDataAttributes.P_OLNS_M_SERV_EQP_INDICATOR);
		encodedSerOrEqpInd = CommonUtils.formatIntToByte(serOrEqpInd);
		totalLength += OlnsConstants.OLNS_SERV_EQP_IND_LENGTH_OCTET;

		// Originating IC indicator Identifier Row X, Y, Z

		if (leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OIC) == null) {
			logger.info("encodeOLNSQuery: throwing exception ");
			logger.error("encodeOLNSQuery:Originating IC indicator OIC is Mandatory");
			throw new AINCodecException("Originating IC indicator OIC is Mandatory");
		}
		if (leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OINC) == null) {
			logger.info("encodeOLNSQuery: throwing exception ");
			logger.error("encodeOLNSQuery:Originating IC indicator OINC is Mandatory");
			throw new AINCodecException("Originating IC indicator OINC is Mandatory");
		}
		byte encodedOrigICIndTag1 = OlnsConstants.OLNS_ORIG_IC_IND_TAG1;
		byte encodedOrigICIndTag2 = OlnsConstants.OLNS_ORIG_IC_IND_TAG2;
		byte encodedOrigICIndLength = OlnsConstants.OLNS_ORIG_IC_IND_LENGTH;
		byte[] encodedOrigIcIndOIC = null;
		byte[] encodedOrigIcIndOINC = null;

		int origIcIndOIC = (Integer) leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OIC);
		int origIcIndOINC = (Integer) leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OINC);
		encodedOrigIcIndOIC = CommonUtils.formatIntToByte(origIcIndOIC);
		encodedOrigIcIndOINC = CommonUtils.formatIntToByte(origIcIndOINC);
		totalLength += OlnsConstants.OLNS_ORIG_IC_IND_LENGTH_OCTET;

		// Additional Originating Billing Service (OPTIONAL) Row: AA, AB, AC
		byte encodeAdditionalBillingSerIndTag1 = OlnsConstants.OLNS_ADDITIONAL_ORIG_BILLING_SER_TAG1;
		byte encodeAdditionalBillingSerIndTag2 = OlnsConstants.OLNS_ADDITIONAL_ORIG_BILLING_SER_TAG2;
		byte encodeAdditionalBillingSerIndLength = OlnsConstants.OLNS_ADDITIONAL_ORIG_BILLING_SER_LENGTH;
		byte[] encodedAdditionalOrignatingBillingOrServiceIndicator1 = { 0x00 };
		byte[] encodedAdditionalOrignatingBillingOrServiceIndicator2 = { 0x00 };
		// refer section 9
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND1) != null
				&& leg2Data.get(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND2) != null) {
			encodeAdditionalBillingSerIndTag1 = OlnsConstants.OLNS_ADDITIONAL_ORIG_BILLING_SER_TAG1;
			encodeAdditionalBillingSerIndTag2 = OlnsConstants.OLNS_ADDITIONAL_ORIG_BILLING_SER_TAG2;
			encodeAdditionalBillingSerIndLength = OlnsConstants.OLNS_ADDITIONAL_ORIG_BILLING_SER_LENGTH;

			int additionalOrignationBillingOrServiceIndicator1 = (Integer) leg2Data
					.get(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND1);
			int additionalOrignationBillingOrServiceIndicator2 = (Integer) leg2Data
					.get(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND2);

			//			encodedAdditionalOrignatingBillingOrServiceIndicator1 = CommonUtils
			//					.formatIntToByte(additionalOrignationBillingOrServiceIndicator1);
			//			encodedAdditionalOrignatingBillingOrServiceIndicator2 = CommonUtils
			//					.formatIntToByte(additionalOrignationBillingOrServiceIndicator2);
			//CommonUtils.formatIntToByte will return 2 bytes if integer value is > 127. So commenting above lines
			encodedAdditionalOrignatingBillingOrServiceIndicator1[0] = (byte) additionalOrignationBillingOrServiceIndicator1;
			encodedAdditionalOrignatingBillingOrServiceIndicator2[0] = (byte) additionalOrignationBillingOrServiceIndicator2;
			totalLength += OlnsConstants.OLNS_ADDITIONAL_ORIG_BILLING_OR_SERV_IND_LEN;

		}

		// Orignating IC Digits (OPTIONAL) Row-AD AE AF

		byte encodedOrignatingIcDigitsTag1 = 0;
		byte encodedOrignatingIcDigitsTag2 = 0;
		byte encodedOrignatingIcDigitsLength = 0;
		byte encodedOrignatingIcDigitsType = 0;
		byte encodedOrignatingIcDigitsNOA = 0;
		byte encodedOrignatingIcDigitsNumPlan = 0;
		byte encodedOrignatingIcNumOfDigits = 0;
		byte[] encodedOrigIcAdrsSignal = null;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_IC_DIGITS) != null) {
			encodedOrignatingIcDigitsTag1 = OlnsConstants.OLNS_ORIG_IC_DIGITS_TAG1;
			encodedOrignatingIcDigitsTag2 = OlnsConstants.OLNS_ORIG_IC_DIGITS_TAG2;
			encodedOrignatingIcDigitsLength = OlnsConstants.OLNS_ORIG_IC_DIGITS_LENGTH;
			encodedOrignatingIcDigitsType = OlnsConstants.OLNS_ORIG_IC_DIGITS_TYPE;
			encodedOrignatingIcDigitsNOA = OlnsConstants.OLNS_ORIG_IC_DIGITS_NOA;
			encodedOrignatingIcDigitsNumPlan = OlnsConstants.OLNS_ORIG_IC_DIGITS_NUM_PLAN;
			encodedOrignatingIcNumOfDigits = OlnsConstants.OLNS_ORIG_IC_NUM_DIGITS;
			PhoneNumber origIcDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_IC_DIGITS);
			encodedOrigIcAdrsSignal = encodeAdrsSignalForOlns(origIcDigits.getAddress());
			totalLength += OlnsConstants.OLNS_ORIG_IC_DIGITS_LENGTH_OCTET;
		}

		// Orignating INC Digits (OPTIONAL) Row-AG AH AI

		byte encodedOrignatingIncDigitsTag1 = 0;
		byte encodedOrignatingIncDigitsTag2 = 0;
		byte encodedOrignatingIncDigitsLength = 0;
		byte encodedOrignatingIncDigitsType = 0;
		byte encodedOrignatingIncDigitsNOA = 0;
		byte encodedOrignatingIncDigitsNumPlan = 0;
		byte encodedOrignatingIncNumOfDigits = 0;
		byte[] encodedOrigIncAdrsSignal = null;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_INC_DIGITS) != null) {
			encodedOrignatingIncDigitsTag1 = OlnsConstants.OLNS_ORIG_INC_DIGITS_TAG1;
			encodedOrignatingIncDigitsTag2 = OlnsConstants.OLNS_ORIG_INC_DIGITS_TAG2;
			encodedOrignatingIncDigitsLength = OlnsConstants.OLNS_ORIG_INC_DIGITS_LENGTH;
			encodedOrignatingIncDigitsType = OlnsConstants.OLNS_ORIG_INC_DIGITS_TYPE;
			encodedOrignatingIncDigitsNOA = OlnsConstants.OLNS_ORIG_INC_DIGITS_NOA;
			encodedOrignatingIncDigitsNumPlan = OlnsConstants.OLNS_ORIG_INC_DIGITS_NUM_PLAN;
			encodedOrignatingIncNumOfDigits = OlnsConstants.OLNS_ORIG_INC_NUM_DIGITS;
			PhoneNumber origIncDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_INC_DIGITS);
			encodedOrigIncAdrsSignal = encodeAdrsSignalForOlns(origIncDigits.getAddress());
			totalLength += OlnsConstants.OLNS_ORIG_INC_DIGITS_LENGTH_OCTET;
		}

		// Foreign Language ID (OPTIONAL) Row : AJ, AK, AL
		byte encodedForeignLangIdenTag1 = 0;
		byte encodedForeignLangIdenTag2 = 0;
		byte encodedForeignLangIdenLength = 0;
		byte[] encodedForeignLangId = null;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_FOREIGN_LANG_ID) != null) {
			encodedForeignLangIdenTag1 = OlnsConstants.OLNS_FOREIGN_LANG_ID_TAG1;
			encodedForeignLangIdenTag2 = OlnsConstants.OLNS_FOREIGN_LANG_ID_TAG2;
			encodedForeignLangIdenLength = OlnsConstants.OLNS_FOREIGN_LANG_ID_LENGTH;
			int foreignLangId = (Integer) leg2Data.get(LegDataAttributes.P_OLNS_O_FOREIGN_LANG_ID);
			encodedForeignLangId = CommonUtils.formatIntToByte(foreignLangId);
			totalLength += OlnsConstants.OLNS_FOREIGN_LANG_ID_LENGTH_OCTET;
		}

		// Generic Name (OPTIONAL) Row: AM, AN, AO
		byte encodedGenericNameLen = 1;
		byte encodedGenericNameIdentifier = 0;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME) != null) {
			encodedGenericNameIdentifier = OlnsConstants.OLNS_GENERIC_NAME_ID;
			String genericName = (String) leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME);
			if (genericName.length() > 15) {
				logger.info("encodeOLNSQuery: throwing exception ");
				logger.error("encodeOLNSQuery: generic name length should be less than 15");
				throw new AINCodecException("generic name length should be less than 15");
			}
			totalLength += genericName.length();
			encodedGenericNameLen += (byte) (genericName.length());
			totalLength += OlnsConstants.OLNS_GENERIC_NAME_LENGTH_OCTET;
		}

		// Alphanumeric String (OPTIONAL) Row AP, AQ, AR
		byte encodedAlphanumericStringIdentifierTag1 = 0;
		byte encodedAlphanumericStringIdentifierTag2 = 0;
		byte encodedAlphanumericStringLen = 0;
		byte[] encodedAlphanumericString = null;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ALPHANUMERIC_STRING) != null) {
			encodedAlphanumericStringIdentifierTag1 = OlnsConstants.OLNS_ALPHANUMERIC_STRING_ID_TAG1;
			encodedAlphanumericStringIdentifierTag2 = OlnsConstants.OLNS_ALPHANUMERIC_STRING_ID_TAG2;
			String alphanumericString = (String) leg2Data.get(LegDataAttributes.P_OLNS_O_ALPHANUMERIC_STRING);
			if (alphanumericString.length() > 40) {
				logger.info("encodeOLNSQuery: throwing exception ");
				logger.error("encodeOLNSQuery: alphanumeric string length should be less than 40 char");
				throw new AINCodecException("alphanumeric string length should be less than 40 char");
			}
			encodedAlphanumericString = encodeIa5(alphanumericString);
			totalLength += alphanumericString.length();
			encodedAlphanumericStringLen += (byte) (alphanumericString.length());

			totalLength += OlnsConstants.OLNS_ALPHANUMERIC_STRING_LENGTH_OCTET;
		}
		// Disallowed Card Issuer code Row- AS, AT, AU
		byte encodedDisalwdCardIssCdTag1 = 0;
		byte encodedDisalwdCardIssCdTag2 = 0;
		byte encodedDisalwdCardIssCdLen = 0;
		byte encodedDisalwdCardIssCdType = 0;
		byte encodedDisalwdCardIssCdNoa = 0;
		byte encodedDisalwdCardIssCdEncdngScheme = 0;
		byte encodedDisalwdCardIssCdNumDigit = 0;
		byte[] encodedDisalwdCardIssCd = null;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_DISALLOWED_CARD_ISSUER_CODE) != null) {
			encodedDisalwdCardIssCdTag1 = OlnsConstants.OLNS_DISALWD_CARD_ISSUER_CODE_TAG1;
			encodedDisalwdCardIssCdTag2 = OlnsConstants.OLNS_DISALWD_CARD_ISSUER_CODE_TAG2;
			//encodedDisalwdCardIssCdLen = OlnsConstants.OLNS_DISALWD_CARD_ISSUER_CODE_LENGTH;
			encodedDisalwdCardIssCdType = OlnsConstants.OLNS_DISALWD_CARD_ISSUER_CODE_TYPE;
			encodedDisalwdCardIssCdNoa = OlnsConstants.OLNS_DISALWD_CARD_ISSUER_CODE_NOA;
			encodedDisalwdCardIssCdEncdngScheme = OlnsConstants.OLNS_DISALWD_CARD_ISSUER_CODE_ENCDNG_SCHEME;
			//encodedDisalwdCardIssCdNumDigit = OlnsConstants.OLNS_DISALWD_CARD_ISSUER_CODE_NUM_DIGIT;
			PhoneNumber disalwdCardIssCdDigits = (PhoneNumber) leg2Data
					.get(LegDataAttributes.P_OLNS_O_DISALLOWED_CARD_ISSUER_CODE);
			String addrString = disalwdCardIssCdDigits.getAddress();
			//			if (addrString.length() != 6) {
			//				logger.info("DISALLOWED_CARD_ISSUER_CODE : No of Digits should be 6");
			//				throw new AINCodecException("DISALLOWED_CARD_ISSUER_CODE : No of Digits should be 6");
			//			}

			encodedDisalwdCardIssCd = encodeAdrsSignalForOlns(addrString);
			int dcicDigLen = addrString.length();
			Integer dcicDigLength = new Integer(dcicDigLen);
			int dcicBytesLen = 0;
			if (addrString.length() % 2 == 0) {
				totalLength += addrString.length() / 2;
				dcicBytesLen = addrString.length() / 2;
			} else {
				totalLength += (addrString.length() + 1) / 2;
				dcicBytesLen = (addrString.length() + 1) / 2;
			}
			Integer dcicBytesLength = new Integer(dcicBytesLen + 4);
			totalLength += OlnsConstants.OLNS_DISALWD_CARD_ISSUER_CODE_LENGTH_OCTET;
			encodedDisalwdCardIssCdLen = dcicBytesLength.byteValue();
			encodedDisalwdCardIssCdNumDigit = dcicDigLength.byteValue();
		}

		// Originating Listing Services Indicator 1 , Row AV, AW, AX
		byte encodedOrigListServIndTag1 = 0;
		byte encodedOrigListServIndTag2 = 0;
		byte encodedOrigListServIndTagLen = 0;
		byte encodedOrigListServInd = 0;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_LISTING_SERV_IND1) != null) {
			encodedOrigListServIndTag1 = OlnsConstants.OLNS_ORIG_LIST_SERV_IND_TAG1;
			encodedOrigListServIndTag2 = OlnsConstants.OLNS_ORIG_LIST_SERV_IND_TAG2;
			encodedOrigListServIndTagLen = OlnsConstants.OLNS_ORIG_LIST_SERV_IND_LENGTH;
			encodedOrigListServInd = 0; // Reserved for future use
			totalLength += OlnsConstants.OLNS_ORIG_LIST_SERV_IND_LENGTH_OCTET;
		}
		// Account Owner Identifier Row- AY, AZ, BA
		byte[] accOwnerEncoded = null;
		byte encodedAccOwnerTag1 = 0;
		byte encodedAccOwnerTag2 = 0;
		byte encodedAccOwnerTag3 = 0;
		byte encodedAccOwnerLen = 0;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ACC_OWNER) != null) {
			encodedAccOwnerTag1 = OlnsConstants.OLNS_ACC_OWNER_TAG1;
			encodedAccOwnerTag2 = OlnsConstants.OLNS_ACC_OWNER_TAG2;
			encodedAccOwnerTag3 = OlnsConstants.OLNS_ACC_OWNER_TAG3;
			encodedAccOwnerLen = OlnsConstants.OLNS_ACC_OWNER_LENGTH;
			String accOwnerIdentifier = (String) leg2Data.get(LegDataAttributes.P_OLNS_O_ACC_OWNER);
			accOwnerEncoded = encodeIa5(accOwnerIdentifier);
			totalLength += OlnsConstants.OLNS_ACC_OWNER_LEN_OCTET;
		}

		// Billing Service Provider Row- BB, BC, BD
		byte[] billingServiceProviderEncoded = null;
		byte encodedBillServTag1 = 0;
		byte encodedBillServTag2 = 0;
		byte encodedBillServTag3 = 0;
		byte encodedBillServLen = 0;
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_BILING_SER_PROV) != null) {
			encodedBillServTag1 = OlnsConstants.OLNS_BILLING_SERV_PROV_TAG1;
			encodedBillServTag2 = OlnsConstants.OLNS_BILLING_SERV_PROV_TAG2;
			encodedBillServTag3 = OlnsConstants.OLNS_BILLING_SERV_PROV_TAG3;
			encodedBillServLen = OlnsConstants.OLNS_BILLING_SERV_PROV_LENGTH;
			String billingServiceProvider = (String) leg2Data.get(LegDataAttributes.P_OLNS_O_BILING_SER_PROV);
			billingServiceProviderEncoded = encodeIa5(billingServiceProvider);
			totalLength += OlnsConstants.OLNS_BILLING_SER_PROV_LEN_OCTET;
		}

		// WSOI
		byte encodedWsoiTag1 = 0;
		byte encodedWsoiTag2 = 0;
		byte encodedWsoiTag3 = 0;
		byte encodedWsoiLength = 0;
		byte[] encodedWsoi = null;

		if (leg2Data.get(LegDataAttributes.P_OLNS_O_WIRELESS_SERVICES_ORIG_INDICATOR) != null) {
		encodedWsoiTag1 = OlnsConstants.OLNS_WSOI_TAG1;
		encodedWsoiTag2 = OlnsConstants.OLNS_WSOI_TAG2;
		encodedWsoiTag3 = OlnsConstants.OLNS_WSOI_TAG3;
		encodedWsoiLength = OlnsConstants.OLNS_WSOI_LENGTH;
		encodedWsoi = null;
		int wsoi = (Integer) leg2Data.get(LegDataAttributes.P_OLNS_O_WIRELESS_SERVICES_ORIG_INDICATOR);
		encodedWsoi = CommonUtils.formatIntToByte(wsoi);
		totalLength += 5;
		}
		
		// OUTPUT ARRAY
		String[] parameterSetLength = convertHexRepersentaion(Integer.toHexString(totalLength));
		byte[] outputArray = new byte[totalLength + 2];

		outputArray[index++] = encodedParameterSetIdentifier;
		outputArray[index++] = (byte) Byte.decode(parameterSetLength[0]);
		outputArray[index++] = encodeBillingSerIndTag1;
		outputArray[index++] = encodeBillingSerIndTag2;
		outputArray[index++] = encodeBillingSerIndLength;
		if (encodedOrignatingBillingOrServiceIndicator1 != null) {
			for (byte val : encodedOrignatingBillingOrServiceIndicator1) {
				outputArray[index++] = val;
			}
		}
		if (encodedOrignatingBillingOrServiceIndicator2 != null) {
			for (byte val : encodedOrignatingBillingOrServiceIndicator2) {
				outputArray[index++] = val;
			}
		}
		if (encodedOrignatingBillingOrServiceIndicator3 != null) {
			for (byte val : encodedOrignatingBillingOrServiceIndicator3) {
				outputArray[index++] = val;
			}
		}

		outputArray[index++] = encodedTreatmentIndicatorTag1;
		outputArray[index++] = encodedTreatmentIndicatorTag2;
		outputArray[index++] = encodedTreatmentIndicatorLength;
		if (encodedTreatmentIndicator != null) {
			for (byte val : encodedTreatmentIndicator) {
				outputArray[index++] = val;
			}
		}
		outputArray[index++] = encodedSerOrEqpIndTag1;
		outputArray[index++] = encodedSerOrEqpIndTag2;
		outputArray[index++] = encodedSerOrEqpIndLength;
		if (encodedSerOrEqpInd != null) {
			for (byte val : encodedSerOrEqpInd) {
				outputArray[index++] = val;
			}
		}

		outputArray[index++] = encodedOrigICIndTag1;
		outputArray[index++] = encodedOrigICIndTag2;
		outputArray[index++] = encodedOrigICIndLength;
		if (encodedOrigIcIndOIC != null) {
			for (byte val : encodedOrigIcIndOIC) {
				outputArray[index++] = val;
			}
		}
		if (encodedOrigIcIndOINC != null) {
			for (byte val : encodedOrigIcIndOINC) {
				outputArray[index++] = val;
			}
		}
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND1) != null
				&& leg2Data.get(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND2) != null) {
			outputArray[index++] = encodeAdditionalBillingSerIndTag1;
			outputArray[index++] = encodeAdditionalBillingSerIndTag2;
			outputArray[index++] = encodeAdditionalBillingSerIndLength;
			if (encodedAdditionalOrignatingBillingOrServiceIndicator1 != null) {
				for (byte val : encodedAdditionalOrignatingBillingOrServiceIndicator1) {
					outputArray[index++] = val;
				}
			}
			if (encodedAdditionalOrignatingBillingOrServiceIndicator2 != null) {
				for (byte val : encodedAdditionalOrignatingBillingOrServiceIndicator2) {
					outputArray[index++] = val;
				}
			}
		}
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_IC_DIGITS) != null) {
			outputArray[index++] = encodedOrignatingIcDigitsTag1;
			outputArray[index++] = encodedOrignatingIcDigitsTag2;
			outputArray[index++] = encodedOrignatingIcDigitsLength;
			outputArray[index++] = encodedOrignatingIcDigitsType;
			outputArray[index++] = encodedOrignatingIcDigitsNOA;
			outputArray[index++] = encodedOrignatingIcDigitsNumPlan;
			outputArray[index++] = encodedOrignatingIcNumOfDigits;
			if (encodedOrigIcAdrsSignal != null) {
				for (byte val : encodedOrigIcAdrsSignal) {
					outputArray[index++] = val;
				}
			}
		}

		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_INC_DIGITS) != null) {
			outputArray[index++] = encodedOrignatingIncDigitsTag1;
			outputArray[index++] = encodedOrignatingIncDigitsTag2;
			outputArray[index++] = encodedOrignatingIncDigitsLength;
			outputArray[index++] = encodedOrignatingIncDigitsType;
			outputArray[index++] = encodedOrignatingIncDigitsNOA;
			outputArray[index++] = encodedOrignatingIncDigitsNumPlan;
			outputArray[index++] = encodedOrignatingIncNumOfDigits;
			if (encodedOrigIncAdrsSignal != null) {
				for (byte val : encodedOrigIncAdrsSignal) {
					outputArray[index++] = val;
				}
			}
		}
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_FOREIGN_LANG_ID) != null) {
			outputArray[index++] = encodedForeignLangIdenTag1;
			outputArray[index++] = encodedForeignLangIdenTag2;
			outputArray[index++] = encodedForeignLangIdenLength;
			if (encodedForeignLangId != null) {
				for (byte val : encodedForeignLangId) {
					outputArray[index++] = val;
				}
			}
		}
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME) != null) {
			outputArray[index++] = encodedGenericNameIdentifier;
			outputArray[index++] = encodedGenericNameLen;
			String genericName = (String) leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME);
			int gnAvailability = (Integer) leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME_AVAILABILITY);
			int presentationType = (Integer) leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME_PRESENTATION);
			int encodedGNTag = ((gnAvailability & 0x01) | 0x02) << 4;
			encodedGNTag |= (presentationType & 0x03);
			outputArray[index++] = (byte) encodedGNTag;
			String charSubfilds = genericName;
			if (charSubfilds != null) {
				byte[] encodedCharSubfilds = encodeIa5(charSubfilds);
				for (byte val : encodedCharSubfilds) {
					outputArray[index++] = val;
				}
			}
		}
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ALPHANUMERIC_STRING) != null) {
			outputArray[index++] = encodedAlphanumericStringIdentifierTag1;
			outputArray[index++] = encodedAlphanumericStringIdentifierTag2;
			outputArray[index++] = encodedAlphanumericStringLen;
			if (encodedAlphanumericString != null) {
				for (byte val : encodedAlphanumericString) {
					outputArray[index++] = val;
				}
			}
		}
		if (leg2Data.get(LegDataAttributes.P_OLNS_O_DISALLOWED_CARD_ISSUER_CODE) != null) {
			outputArray[index++] = encodedDisalwdCardIssCdTag1;
			outputArray[index++] = encodedDisalwdCardIssCdTag2;
			outputArray[index++] = encodedDisalwdCardIssCdLen;
			outputArray[index++] = encodedDisalwdCardIssCdType;
			outputArray[index++] = encodedDisalwdCardIssCdNoa;
			outputArray[index++] = encodedDisalwdCardIssCdEncdngScheme;
			outputArray[index++] = encodedDisalwdCardIssCdNumDigit;
			if (encodedDisalwdCardIssCd != null) {
				for (byte val : encodedDisalwdCardIssCd) {
					outputArray[index++] = val;
				}
			}
		}

		if (leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_LISTING_SERV_IND1) != null) {
			outputArray[index++] = encodedOrigListServIndTag1;
			outputArray[index++] = encodedOrigListServIndTag2;
			outputArray[index++] = encodedOrigListServIndTagLen;
			outputArray[index++] = encodedOrigListServInd; // Reserved for future use
		}

		if (accOwnerEncoded != null) {
			outputArray[index++] = encodedAccOwnerTag1;
			outputArray[index++] = encodedAccOwnerTag2;
			outputArray[index++] = encodedAccOwnerTag3;
			outputArray[index++] = encodedAccOwnerLen;
			for (byte encodedVal : accOwnerEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		if (billingServiceProviderEncoded != null) {
			outputArray[index++] = encodedBillServTag1;
			outputArray[index++] = encodedBillServTag2;
			outputArray[index++] = encodedBillServTag3;
			outputArray[index++] = encodedBillServLen;
			for (byte encodedVal : billingServiceProviderEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		
		if (encodedWsoi != null) {
			outputArray[index++] = encodedWsoiTag1;
			outputArray[index++] = encodedWsoiTag2;
			outputArray[index++] = encodedWsoiTag3;
			outputArray[index++] = encodedWsoiLength;
			for (byte encodedVal : encodedWsoi) {
				outputArray[index++] = encodedVal;
			}
		}
		if (logger.isInfoEnabled()) {
			logger.info("Exit: encodeOlnsQuery ::" + CommonUtils.formatBytes(outputArray));
		}
		return outputArray;
	}

	public static byte[] getApplicationErrorForOlnsQuery(CallData callData) {
		// error code will be in leg2
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int totalLength = 0;
		int index = 0;
		totalLength += OlnsConstants.TOTAL_FIXED_LEN_APP_ERR;

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
			try{
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
		return buffer;
	}

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

	private static byte[] encodeIa5(String asciiVal) {
		return asciiVal.getBytes(StandardCharsets.US_ASCII);
	}

	/**
	 * method to decode address signal from byte array
	 * 
	 * @param data
	 * @throws InvalidInputException
	 */
	private static String decodeAdrsSignalForOLNS(byte[] data, int offset, int parity) throws InvalidInputException {
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
	private static byte[] encodeAdrsSignalForOlns(String addrSignal) throws InvalidInputException {

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
	
	/**
	 * method here used to reset all Global Variables, leg1Data & leg2Data attributes 
	 * @param callData
	 */
	public static void resetOLNSQuery(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetOLNSQuery:Enter");
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
		
		if(legData.get(LegDataAttributes.P_CALLING_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLING_PARTY);
		}
		
		if(legData.get(LegDataAttributes.P_CALLED_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLED_PARTY);
		}
		
		if(legData.get(LegDataAttributes.P_OLNS_O_BILLING_NUM) != null) {
			legData.remove(LegDataAttributes.P_OLNS_O_BILLING_NUM);
		}
		
		if(legData.get(LegDataAttributes.OLNS_O_PSRID_TYPE) != null) {
			legData.remove(LegDataAttributes.OLNS_O_PSRID_TYPE);
		}
		
		if(legData.get(LegDataAttributes.OLNS_O_PRIMARY_SERVICE_REQUESTER) != null) {
			legData.remove(LegDataAttributes.OLNS_O_PRIMARY_SERVICE_REQUESTER);
		}

		//resetting leg2Data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
		if(leg2Data.get(LegDataAttributes.P_LIDB_QUERY_TYPE) != null) {
			leg2Data.remove(LegDataAttributes.P_LIDB_QUERY_TYPE);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND1) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND1);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND2) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND2);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND3) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_M_ORIG_BILL_SER_IND3);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_M_TREATMENT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_M_TREATMENT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_M_SERV_EQP_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_M_SERV_EQP_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OIC) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OIC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OINC) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_M_ORIG_IC_INDICATOR_OINC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND1) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND1);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND2) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_ADD_ORIG_BILL_SER_IND2);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_IC_DIGITS) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_ORIG_IC_DIGITS);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_INC_DIGITS) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_ORIG_INC_DIGITS);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_FOREIGN_LANG_ID) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_FOREIGN_LANG_ID);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_GENERIC_NAME);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_ALPHANUMERIC_STRING) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_ALPHANUMERIC_STRING);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_DISALLOWED_CARD_ISSUER_CODE) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_DISALLOWED_CARD_ISSUER_CODE);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_ORIG_LISTING_SERV_IND1) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_ORIG_LISTING_SERV_IND1);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_ACC_OWNER) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_ACC_OWNER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_BILING_SER_PROV) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_BILING_SER_PROV);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_WIRELESS_SERVICES_ORIG_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_WIRELESS_SERVICES_ORIG_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME_AVAILABILITY) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_GENERIC_NAME_AVAILABILITY);
		}
		
		if(leg2Data.get(LegDataAttributes.P_OLNS_O_GENERIC_NAME_PRESENTATION) != null) {
			leg2Data.remove(LegDataAttributes.P_OLNS_O_GENERIC_NAME_PRESENTATION);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE);
		}
		if (logger.isDebugEnabled()) {
			logger.info("resetOLNSQuery:Exit");
		}
	}

}
