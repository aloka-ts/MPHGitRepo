/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.


Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.ainscf.lidb;

import java.nio.charset.StandardCharsets;

import org.apache.commons.lang3.StringUtils;
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
import java.util.List; 
import java.util.ArrayList; 

/**
 * @author rarya
 * @author Krishna
 *
 */
public class GetDataQuery {

	private static Logger logger = Logger.getLogger(GetDataQuery.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
	'f' };

	/**
	 * @param callData
	 * @param input
	 * @throws AINCodecException
	 * @throws InvalidInputException
	 */
	public static void decodeGetDataQuery(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {
		if (logger.isInfoEnabled()) {
			logger.info("decodeGetDataQuery:Enter");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.GETDATA.name());

		if (logger.isInfoEnabled()) {
			logger.info("decodeGetDataQuery:set P_LIDB_QUERY_TYPE ==GETDATA");
		}

		// Parameter Set ID Row P
		int currentIndex = 0;
		int currentElementIdLen = 1; // number of bytes of current element Identifier
		int currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[0] != (byte) 0xF2 && input[0] != (byte)0x31) {
			logger.info("decodeGetDataQuery:throwing exception ");
			logger.error("decodeGetDataQuery:parameter set ID is not correct");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("parameter set ID is not correct");
		}

		//		if (input.length < 22) {
		//			logger.info("decodeGetDataQuery:throwing exception ");
		//			logger.error("decodeGetDataQuery:input buffer can not be less then 22 ");
		//			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//			throw new AINCodecException("Length of the buffer must be Greater then 22");
		//		}
		int totalLen = input[1];

		// check for GETDATA Info ID Row R
		currentIndex = 2;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[2] != (byte) 0xDF || input[3] != (byte) 0x75) {
			logger.info("decodeGetDataQuery:throwing exception ");
			logger.error("decodeGetDataQuery:GETDATA info id is not correct ");
			throw new AINCodecException("GETDATA info id is not correct");
		}

		// check for GETDATA Info length. Row S
		if (input[4] != (byte) 0x00) {
			logger.info("decodeGetDataQuery:throwing exception ");
			logger.error("decodeGetDataQuery:GETDATA info length should be zero ");
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("GETDATA info length should be zero");
		}
		// Service Key Identifier Row-T
		currentIndex = 5;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[5] != (byte) 0xAA) {
			//logger.info("decodeGetDataQuery:throwing exception ");
			logger.error("decodeGetDataQuery:Service key ID is not correct ");
			//AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//throw new AINCodecException("Service key ID is not correct ");
			return; // In this case, Application will check if service key is null and return lpe-missingOrIncorrectParameter error code 
		}
		// Service key Length Row-U
		if (input[6] != (byte) 0x0C) {
			//logger.info("decodeGetDataQuery:throwing exception ");
			logger.error("decodeGetDataQuery:Service key Length is not correct ");
			//AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//throw new AINCodecException("Service key Length is not correct ");
			return; // In this case, Application will check if service key is null and return lpe-missingOrIncorrectParameter error code
		}

		// check for Digits Id [GETDATA QUERIED NUMBER] Row V, W, X
		currentIndex = 7;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[7] != (byte) 0xDF || input[8] != (byte) 0x49) {
			//logger.info("decodeGETDATAQuery:throwing exception ");
			logger.error("decodeGETDATAQuery:Digits Id [GETDATA] is not correct ");
			//AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//throw new AINCodecException("Digits Id [GETDATA] is not correct ");
			return; // In this case, Application will check if service key is null and return lpe-missingOrIncorrectParameter error code
		}

		// check for Digits Length [GETDATA] //9 Row W
		if (input[9] != (byte) 0x09) {
			//logger.info("decodeGETDATAQuery:throwing exception ");
			logger.error("decodeGETDATAQuery:Digits Length [GETDATA] is not correct ");
			//AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//throw new AINCodecException("Digits Length [GETDATA] is not correct ");
			return; // In this case, Application will check if service key is null and return lpe-missingOrIncorrectParameter error code
		}

		// check for digits Row X
		if (input[10] != (byte) 0x17 || input[11] != (byte) 0x00 || input[12] != (byte) 0x11 || input[13] != (byte) 0x0A) {
			//logger.info("decodeGETDATAQuery:throwing exception ");
			logger.error("decodeGETDATAQuery:Digits[GETDATA Queried Number] should be correct. ");
			//AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//throw new AINCodecException("Digits[GETDATA Queried ] should be correct.");
			return; // In this case, Application will check if service key is null and return lpe-missingOrIncorrectParameter error code
		}

		int digitLen = input[9] & 0xFF;
		if (digitLen != 9) {
			//logger.info("decodeGETDATAQuery:throwing exception ");
			logger.error("decodeGETDATAQuery:Digit Length should be correct. ");
			//AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//throw new AINCodecException("Digit Length should be correct.");
			return; // In this case, Application will check if service key is null and return lpe-missingOrIncorrectParameter error code
		}

		int queriedNumLen = input[13] & 0xFF;
		int arrsize;
		if (queriedNumLen % 2 == 0) {
			arrsize = (queriedNumLen / 2);
		} else {
			arrsize = (queriedNumLen + 1) / 2;
		}
		AinDigits ainDigitForGETDATA = new AinDigits();
		byte[] inputfrDecodeAin = new byte[arrsize];

		for (int i = 0; i < inputfrDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAin[i] = input[14 + i];
		}
		int natureOfGETDATADigits = input[11] & 0xFF;
		//ainDigitForGETDATA.setAddrSignal(decodeAdrsSignalForGetQuery(data, offset, parity)(inputfrDecodeAin, 0, 0));
		ainDigitForGETDATA.setAddrSignal(decodeAdrsSignalForGetQuery(inputfrDecodeAin, 0, 0));
		PhoneNumber phNumber = AinScfProtocolParser.parseAinDigits(ainDigitForGETDATA, natureOfGETDATADigits);

		// set value in legData
		legData.set(LegDataAttributes.GETDATA_QUERIED_NUM, phNumber);

		// check for LIDBDataElementList Y
		currentIndex = 19;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[19] != (byte) 0xFF || input[20] != (byte) 0x7D) {
			//logger.info("decodeGETDATAQuery:throwing exception ");
			logger.error("decodeGETDATAQuery:LIDBDataElementList Id [GETDATA] is not correct ");
			//AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//throw new AINCodecException("LIDBDataElementList Id [GETDATA] is not correct ");
			return; // In this case, Application will check if service key is null and return lpe-missingOrIncorrectParameter error code
		}

		int lidbDataElementsLength = input[21] & 0xFF; //Z. LIDBDataElementList Length
		if (lidbDataElementsLength == 0) {
			//logger.info("decodeGETDATAQuery:throwing exception ");
			logger.error("decodeGETDATAQuery:LIDBDataElementList Length should be >0. ");
			//throw new AINCodecException("LIDBDataElementList Length should be >0.");
			return; // In this case, Application will check if DataElementList is empty and return lpe-missingOrIncorrectParameter error code
		}

		int inputIndex = 22; //index for AA. LIDBElementIdentifier Tag
		currentIndex = inputIndex;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		List<Integer> invalidTcapIdList = new ArrayList<>();
		List<Integer> receivedTcapIdList = new ArrayList<>();

		int dataElementsLen = lidbDataElementsLength; //This will be decremented in while loop to iterate till data elements length >0
		while (dataElementsLen > 0 ){
			// LIDBElementIdentifier tag and length
			int dataElementIdTag = input[inputIndex++];
			if (dataElementIdTag != 2) {
				logger.info("decodeGETDATAQuery:throwing exception ");
				logger.error("decodeGETDATAQuery:Data Element Id Tag should be correct. ");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Data Element Id Tag should be correct.");
			}			
			int dataElementIdLength = input[inputIndex++];
			int dataElementIdvalue = 0; //initialize dataElementIdValue

			if ( dataElementIdLength == 1) {
				dataElementIdvalue = input[inputIndex++];
			} else if ( dataElementIdLength == 2) {
				dataElementIdvalue = input[inputIndex++] + input[inputIndex++];
			} else {
				logger.info("decodeGETDATAQuery:throwing exception ");
				logger.error("decodeGETDATAQuery:LIDBDataElementId Length should not be > 2. ");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("LIDBDataElementId Length should not be > 2.");				
			}

			switch(dataElementIdvalue){
			case (byte) 0x82: {
				logger.info("decodeGETDATAQuery:Account Owner element is requested in the query");
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ACCOUNT_OWNER, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}
			case (byte) 0x79: {
				logger.info("decodeGETDATAQuery:ADDITIONAL ORIG BILLING SERVICE INDICATORS element is requested in the query");
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x95: {
				logger.info("decodeGETDATAQuery:ADDITIONAL ORIG THIRD NUMBER INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x96: {
				logger.info("decodeGETDATAQuery:ADDITIONAL ORIG CREDIT CARD INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x97: {
				logger.info("decodeGETDATAQuery:ADDITIONAL ORIG SPECIAL BNS INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x98: {
				logger.info("decodeGETDATAQuery:ADDITIONAL ORIG SENTPAID INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x99: {
				logger.info("decodeGETDATAQuery:ADDITIONAL ORIG BILLING SERVICE SPARE INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case 72: {
				logger.info("decodeGETDATAQuery:ALPHANUMERIC STRING element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ALPHANUMERIC_STRING, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x9F: {
				logger.info("decodeGETDATAQuery:ALTERNATE PREFERRED IC element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ALTERNATE_PREFERRED_IC, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x83: {
				logger.info("decodeGETDATAQuery:BILLING SERVICE PROVIDER element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_BILLING_SERVICE_PROVIDER, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case 76: {
				logger.info("decodeGETDATAQuery:SERVICE DENIAL INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_SERVICE_DENIAL_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case 82: {
				logger.info("decodeGETDATAQuery:COLLECT ACCEPTANCE INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_COLLECT_ACCEPTANCE_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0xA8: {
				logger.info("decodeGETDATAQuery:DIVERSION ROUTING NUMBER element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_DIVERSION_ROUTING_NUMBER, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x74: {
				logger.info("decodeGETDATAQuery:FOREIGN LANGUAGE IDENTIFIER element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case 23: {
				logger.info("decodeGETDATAQuery:GENERIC NAME element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_GENERIC_NAME, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				logger.info("decodeGETDATAQuery:GENERIC NAME element is added in the receivedTcapID list");
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case 87: {
				logger.info("decodeGETDATAQuery:IC INDICATORS element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_IC_INDICATORS, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x84: {
				logger.info("decodeGETDATAQuery:PRIMARY PREFERRED IC INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x85: {
				logger.info("decodeGETDATAQuery:ALTERNATE PREFERRED IC INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x86: {
				logger.info("decodeGETDATAQuery:PREFERRED INC INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_PREFERRED_INC_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x78: {
				logger.info("decodeGETDATAQuery:ILP CIC INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ILP_CIC_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x94: {
				logger.info("decodeGETDATAQuery:ILP CIC element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ILP_CIC, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x5A: {
				logger.info("decodeGETDATAQuery:INTERCEPT INDICATOR element is requested  in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_INTERCEPT_INDICATOR, true);
				// decrement length

				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x5E: {
				logger.info("decodeGETDATAQuery:ORIG BILLING SERVICE INDICATORS element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_BILLING_SERVICE_INDICATORS, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x8B: {
				logger.info("decodeGETDATAQuery:ORIG COLLECT BILLING INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_COLLECT_BILLING_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x8C: {
				logger.info("decodeGETDATAQuery:ORIG THIRD NUMBER BILLING INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x8D: {
				logger.info("decodeGETDATAQuery:ORIG LOCAL NONTOLL CALL INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x8E: {
				logger.info("decodeGETDATAQuery:ORIG CREDIT CARD INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_CREDIT_CARD_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x8F: {
				logger.info("decodeGETDATAQuery:ORIG FREE DA INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_FREE_DA_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x90: {
				logger.info("decodeGETDATAQuery:ORIG SPECIAL BNS INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_SPECIAL_BNS_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x91: {
				logger.info("decodeGETDATAQuery:ORIG SENTPAID INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_SENTPAID_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x92: {
				logger.info("decodeGETDATAQuery:ORIG DACC INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_DACC_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x93: {
				logger.info("decodeGETDATAQuery:ORIG BILLING SERVICE SPARE INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x4B: {
				logger.info("decodeGETDATAQuery:ORIG IC INDICATORS element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_IC_INDICATORS, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x87: {
				logger.info("decodeGETDATAQuery:ORIG IC INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_IC_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x88: {
				logger.info("decodeGETDATAQuery:ORIG INC INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_INC_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x89: {
				logger.info("decodeGETDATAQuery:ORIG IC element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_IC, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x8A: {
				logger.info("decodeGETDATAQuery:ORIG INC element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_INC, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x7A: {
				logger.info("decodeGETDATAQuery:ORIG LISTING SERVICES INDICATOR 1 element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0xA0: {
				logger.info("decodeGETDATAQuery:PREFERRED INC element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_PREFERRED_INC, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0xA9: {
				logger.info("decodeGETDATAQuery:PREFERRED CODE LIST element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_PREFERRED_CODE_LIST, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x9E: {
				logger.info("decodeGETDATAQuery:PRIMARY PREFERRED IC element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_PRIMARY_PREFERRED_IC, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x9A: {
				logger.info("decodeGETDATAQuery:RAO element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_RAO, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x67: {
				logger.info("decodeGETDATAQuery:RECORD STATUS INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_RECORD_STATUS_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0xA1: {
				logger.info("decodeGETDATAQuery:REFERRAL NUMBER element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_REFERRAL_NUMBER, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x68: {
				logger.info("decodeGETDATAQuery:SERVICE OR EQUIPMENT INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x6A: {
				logger.info("decodeGETDATAQuery:THIRD NUMBER ACCEPTANCE INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x6C: {
				logger.info("decodeGETDATAQuery:TREATMENT INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_TREATMENT_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0x64: {
				logger.info("decodeGETDATAQuery:TRUE BILLING NUMBER element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_TRUE_BILLING_NUMBER, true);
				// decrement length
				dataElementsLen = dataElementsLen - 3;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0xA6: {
				logger.info("decodeGETDATAQuery:WIRELESS SERVICES ORIG INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR, true);
				// decrement length
				if(dataElementIdvalue < 128) {
					dataElementsLen = dataElementsLen - 3;
				}
				else {
					dataElementsLen = dataElementsLen - 4;

				}
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}

			case (byte) 0xA7: {
				logger.info("decodeGETDATAQuery:WIRELESS SERVICES TERMINATING INDICATOR element is requested in the query");
				// convert into field
				// set in leg variable
				legData.set(LegDataAttributes.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR, true);
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}
			default: {
				logger.error("Invalid TCAP ID is received in the GetData request");
				// decrement length
				dataElementsLen = dataElementsLen - 4;
				invalidTcapIdList.add(dataElementIdvalue);
				receivedTcapIdList.add(dataElementIdvalue);
				break;
			}
			}
		}

		String invalidTcapIds = null;
		String receivedTcapIds = null;
		long unSignedTcapId = 0; 

		if (!invalidTcapIdList.isEmpty()) {
			for (int i = 0; i < invalidTcapIdList.size(); i++) {
				unSignedTcapId = invalidTcapIdList.get(i) & 0xff; //dataElementIdvalue is a signed integer value; so need to convert to unsigned integer
				if (invalidTcapIds != null) {
					invalidTcapIds = invalidTcapIds + ' ' + unSignedTcapId;
				}
				else {
					invalidTcapIds = String.valueOf(unSignedTcapId);
				}
			}
		}

		if (!receivedTcapIdList.isEmpty()) {
			for (int i = 0; i < receivedTcapIdList.size(); i++) {
				unSignedTcapId = receivedTcapIdList.get(i) & 0xff;
				if(receivedTcapIds != null) {
					receivedTcapIds = receivedTcapIds + ' ' + unSignedTcapId;
				}
				else {
					receivedTcapIds = String.valueOf(unSignedTcapId);
				}
			}
		}

		if (invalidTcapIds != null) {
			logger.info("GetData - Setting GETDATA_INVALID_TCAPID_LIST with value " + invalidTcapIds);
			legData.set(LegDataAttributes.GETDATA_INVALID_TCAPID_LIST, invalidTcapIds);
		}
		if (receivedTcapIds != null) {
			logger.info("GetData - Setting GETDATA_RECEIVED_TCAPID_LIST with value " + receivedTcapIds);
			legData.set(LegDataAttributes.GETDATA_RECEIVED_TCAPID_LIST, receivedTcapIds);
		}
		if (logger.isInfoEnabled()) {
			logger.info("GetData received tcap ids are " + receivedTcapIds);
			logger.info("GetData received invalid tcap ids are " + invalidTcapIds);
		}

		//Checking for Primary Service Request optional field
		if (input.length > (lidbDataElementsLength + 22)  && input[inputIndex] == (byte) 0xDF){ 
			currentIndex = inputIndex;
			currentElementIdLen = 3; // number of bytes of current element Identifier
			currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

			if (logger.isInfoEnabled()) {
				logger.info("PRIMARY SERVICE REQUESTER ID IS PRESENT");
			}
			// primary Service Requester ID. Note: the correct bytes details for PSRID is not given in gr2838. The values used here is in line with DTSRID identifier with private tcap id 165 (Hex Df C1 25). So tcap id 163 of PSRID is converted to (Hex Df C1 23)
			if (input[inputIndex++] != (byte) 0xDF || input[inputIndex++] != (byte) 0xC1 || input[inputIndex++] != (byte) 0x23) {
				logger.info("decodeGetDataQuery:throwing exception ");
				logger.error("decodeGetDataQuery:primary Service Requester ID is not correct");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("primary Service Requester ID is not correct");
			}
			// primary Service Requester ID Length
			if (input[inputIndex++] != (byte) 0x05) {
				logger.info("decodeGetDataQuery:throwing exception ");
				logger.error("decodeGetDataQuery:primary Service Requester ID Length is not correct");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new ParameterOutOfRangeException("primary Service Requester ID Length is not correct");
			}

			int psridContext = input[inputIndex++] & 0xFF;
			if (psridContext != 0x09 && psridContext != 0x0A) {
				logger.info("decodeGetDataQuery:throwing exception ");
				logger.error("decodeGetDataQuery:primary Service Requester ID spare is not correct");
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("primary Service Requester ID spare is not correct");
			}

			int psridType = psridContext & 0x07; // extract lower 3 digits
			legData.set(LegDataAttributes.GETDATA_PSRID_TYPE, psridType);
			byte[] primaryServiceRequesterIdtable = { input[inputIndex++], input[inputIndex++], input[inputIndex++], input[inputIndex++] };
			String primaryServiceRequesterId = new String(primaryServiceRequesterIdtable);
			legData.set(LegDataAttributes.GETDATA_PRIMARY_SERVICE_REQUESTER, primaryServiceRequesterId);

		} else {
			if (logger.isInfoEnabled()) {
				logger.info("PRIMARY SERVICE REQUESTER ID IS NOT PRESENT");
			}
		}
		if (logger.isInfoEnabled()) {
			logger.info("decodeGetDataQuery:Exit");
		}
	}

	/**
	 * @param callData
	 * @return
	 * @throws InvalidInputException
	 * @throws AINCodecException
	 */
	public static byte[] encodeGetDataQuery(CallData callData) throws InvalidInputException, AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeGETDATAQuery:Enter");
		}
		// contains decoded data Leg2
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		List<Integer> InvalidTcapIdsList = new ArrayList<Integer>();
		leg2Data.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.GETDATA.name());

		int index = 0;
		// total length of output array
		int totalLength = 0;

		// Parameter Set Identifier Row - M
		byte encodedParameterSetIdentifier = GetDataConstants.GETDATA_PARAM_SET_ID;

		//O. LIDBDataElementBlock (2 bytes - Hex FF 7E)
		byte encodedDataElementBlockTag1 = GetDataConstants.GETDATA_LIDB_DATA_ELEMENT_BLOCK_TAG1;
		byte encodedDataElementBlockTag2 = GetDataConstants.GETDATA_LIDB_DATA_ELEMENT_BLOCK_TAG2;
		//P. LIDBDataElementBlock Length - variable
		int lidbDataElementBlockLength = 0;
		totalLength += 3; // length of encodedDataElementBlockTag1,encodedDataElementBlockTag2,encodedDataElementBlockLength


		//Each Element in the response will be have following series of sequences if there is data available for the element. 
		// SEQUENCE
		// SEQUENCE Length		
		// LIDBElementIdentifier Tag
		// LIDBElementIdentifier Len
		// LIDBElementIdentifier value
		// LIDBElementData Tag OR LIDBElementError Tag  
		// LIDBElementData Len OR LIDBElementError Len
		// LIDBElementData OR OR LIDBElementError Value

		//accountOwner Element 
		String accountOwner = null;
		int accountOwnerErr = 0;
		byte encodeGetdataAccountOwnerSeq = 0;
		byte encodeGetdataAccountOwnerSeqLen = 0;
		byte encodeGetdataAccountOwnerElementIdTag = 0;
		byte encodeGetdataAccountOwnerElementIdLen = 0;
		byte[] accountOwnerEncoded = null;
		byte encodedAccountOwnerTag1 = 0;
		byte encodedAccountOwnerTag2 = 0;
		byte encodeGetdataAccountOwnerElementDataTag = 0;
		byte encodeGetdataAccountOwnerElementDataLen = 0;

		byte encodeGetdataAccountOwnerElementErrorTag = 0;
		byte encodeGetdataAccountOwnerElementErrorLen = 0;
		byte[] accountOwnerErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ACCOUNT_OWNER) != null) {
			accountOwner = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ACCOUNT_OWNER);
			if(logger.isDebugEnabled()){
				logger.debug("accountOwner element Data is present, value:: "+accountOwner);
			}

			if(!StringUtils.isNotBlank(accountOwner)){
				accountOwner = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ACCOUNT_OWNER_ERR) != null) {
			accountOwnerErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ACCOUNT_OWNER_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("accountOwner element Error is present, value:: "+accountOwnerErr);
			}
		}


		if ( (accountOwner != null && !accountOwner.isEmpty()) || accountOwnerErr != 0) {
			//SEQUENCE of accountOwner
			encodeGetdataAccountOwnerSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTH of accountOwner
			encodeGetdataAccountOwnerSeqLen = GetDataConstants.GETDATA_ACCOUNT_OWNER_SEQ_LEN;

			//accountOwner LIDBElementIdentifier TAG
			encodeGetdataAccountOwnerElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//accountOwner LIDBElementIdentifier LENGTH
			encodeGetdataAccountOwnerElementIdLen = GetDataConstants.GETDATA_ACCOUNT_OWNER_TAG_LEN;
			accountOwnerEncoded = null;
			//accountOwner LIDBElementIdentifier VALUE. i.e accountOwner identifier
			encodedAccountOwnerTag1 = GetDataConstants.GETDATA_ACCOUNT_OWNER_TAG1;
			encodedAccountOwnerTag2 = GetDataConstants.GETDATA_ACCOUNT_OWNER_TAG2;

			if ( accountOwner != null && !accountOwner.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdataAccountOwnerElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_IA5String;
				//LIDBElementData LENGTH (length of accountOwner)
				encodeGetdataAccountOwnerElementDataLen = GetDataConstants.GETDATA_ACCOUNT_OWNER_LENGTH;
				//LIDBElementData VALUE . i.e Account Owner value
				accountOwnerEncoded = encodeIa5(accountOwner);
				if(logger.isDebugEnabled()){
					logger.debug("accOwnerIdentifier::"+accountOwner+" encoded as::"+accountOwnerEncoded);
				}
				totalLength += GetDataConstants.GETDATA_ACCOUNT_OWNER_LEN_OCTET;
			}
			else if (accountOwnerErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataAccountOwnerElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (accountOwnerErr != 6 ) {
					encodeGetdataAccountOwnerElementErrorLen = 0x02;
					encodeGetdataAccountOwnerSeqLen = 0x08;
					//LIDBElementError VALUE
					accountOwnerErrorEncoded = CommonUtils.formatIntToByte(accountOwnerErr);
					if(logger.isDebugEnabled()){
						logger.debug("accountOwner Error::"+accountOwnerErr+" encoded as::"+accountOwnerErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ACCOUNT_OWNER_ERR_LEN + 2;

				}
				else {
					encodeGetdataAccountOwnerElementErrorLen = 0x01;
					encodeGetdataAccountOwnerSeqLen = 0x07;
					accountOwnerErrorEncoded = CommonUtils.formatIntToByte(accountOwnerErr);
					if(logger.isDebugEnabled()){
						logger.debug("accountOwner Error::"+accountOwnerErr+" encoded as::"+accountOwnerErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ACCOUNT_OWNER_ERR_LEN + 1;
				}				
			}
		}


		// additionalOrigBillingServiceIndicators element (Collective)
		String additionalOrigBillingServiceIndicators = null;
		byte encodeGetdataadditionalOrigBillingServiceIndicatorsSeq = 0;
		byte encodeGetdataadditionalOrigBillingServiceIndicatorsSeqLen = 0;
		byte encodeGetdataadditionalOrigBillingServiceIndicatorsElementIdTag = 0;
		byte encodeGetdataadditionalOrigBillingServiceIndicatorsElementIdLen = 0;
		byte encodedadditionalOrigBillingServiceIndicatorsTag = 0;
		byte encodeGetdataadditionalOrigBillingServiceIndicatorsElementDataTag = 0;
		byte encodeGetdataadditionalOrigBillingServiceIndicatorsElementDataLen = 0;
		int additionalOrigBillingServiceIndicators1 = 0;
		byte[] additionalOrigBillingServiceIndicatorsEncoded  = null;
		int additionalOrigBillingServiceIndicatorsErr = 0;
		byte encodeGetdataadditionalOrigBillingServiceIndicatorsElementErrorTag = 0;
		byte encodeGetdataadditionalOrigBillingServiceIndicatorsElementErrorLen = 0;
		byte[] additionalOrigBillingServiceIndicatorsErrorEncoded = null;

		String additionalOrigThirdNumberIndicator = null;
		byte encodeGetdataadditionalOrigThirdNumberIndicatorSeq = 0;
		byte encodeGetdataadditionalOrigThirdNumberIndicatorSeqLen = 0;
		byte encodeGetdataadditionalOrigThirdNumberIndicatorElementIdTag = 0;
		byte encodeGetdataadditionalOrigThirdNumberIndicatorElementIdLen = 0;
		byte[] additionalOrigThirdNumberIndicatorEncoded = null;
		byte encodedadditionalOrigThirdNumberIndicatorTag1 = 0;
		byte encodedadditionalOrigThirdNumberIndicatorTag2 = 0;
		byte encodeGetdataadditionalOrigThirdNumberIndicatorElementDataTag = 0;
		byte encodeGetdataadditionalOrigThirdNumberIndicatorElementDataLen = 0;
		int additionalOrigThirdNumberIndicatorErr = 0;
		byte encodeGetdataadditionalOrigThirdNumberIndicatorElementErrorTag = 0;
		byte encodeGetdataadditionalOrigThirdNumberIndicatorElementErrorLen = 0;
		byte[] additionalOrigThirdNumberIndicatorErrorEncoded = null;

		String  additionalOrigCreditCardIndicator = null;
		byte encodeGetdataadditionalOrigCreditCardIndicatorSeq = 0;
		byte encodeGetdataadditionalOrigCreditCardIndicatorSeqLen = 0;
		byte encodeGetdataadditionalOrigCreditCardIndicatorElementIdTag = 0;
		byte encodeGetdataadditionalOrigCreditCardIndicatorElementIdLen = 0;
		byte[] additionalOrigCreditCardIndicatorEncoded = null;
		byte encodedadditionalOrigCreditCardIndicatorTag1 = 0;
		byte encodedadditionalOrigCreditCardIndicatorTag2 = 0;
		byte encodeGetdataadditionalOrigCreditCardIndicatorElementDataTag = 0;
		byte encodeGetdataadditionalOrigCreditCardIndicatorElementDataLen = 0;
		int additionalOrigCreditCardIndicatorErr = 0;
		byte encodeGetdataadditionalOrigCreditCardIndicatorElementErrorTag = 0;
		byte encodeGetdataadditionalOrigCreditCardIndicatorElementErrorLen = 0;
		byte[] additionalOrigCreditCardIndicatorErrorEncoded = null;

		String additionalOrigSpecialBnsIndicator = null;
		byte encodeGetdataadditionalOrigSpecialBnsIndicatorSeq = 0;
		byte encodeGetdataadditionalOrigSpecialBnsIndicatorSeqLen = 0;
		byte encodeGetdataadditionalOrigSpecialBnsIndicatorElementIdTag = 0;
		byte encodeGetdataadditionalOrigSpecialBnsIndicatorElementIdLen = 0;
		byte[] additionalOrigSpecialBnsIndicatorEncoded = null;
		byte encodedadditionalOrigSpecialBnsIndicatorTag1 = 0;
		byte encodedadditionalOrigSpecialBnsIndicatorTag2 = 0;
		byte encodeGetdataadditionalOrigSpecialBnsIndicatorElementDataTag = 0;
		byte encodeGetdataadditionalOrigSpecialBnsIndicatorElementDataLen = 0;
		int additionalOrigSpecialBnsIndicatorErr = 0;
		byte encodeGetdataadditionalOrigSpecialBnsIndicatorElementErrorTag = 0;
		byte encodeGetdataadditionalOrigSpecialBnsIndicatorElementErrorLen = 0;
		byte[] additionalOrigSpecialBnsIndicatorErrorEncoded = null;

		int additionalOrigSentpaidIndicator = -1;
		byte encodeGetdataadditionalOrigSentpaidIndicatorSeq = 0;
		byte encodeGetdataadditionalOrigSentpaidIndicatorSeqLen = 0;
		byte encodeGetdataadditionalOrigSentpaidIndicatorElementIdTag = 0;
		byte encodeGetdataadditionalOrigSentpaidIndicatorElementIdLen = 0;
		byte[] additionalOrigSentpaidIndicatorEncoded = null;
		byte encodedadditionalOrigSentpaidIndicatorTag1 = 0;
		byte encodedadditionalOrigSentpaidIndicatorTag2 = 0;
		byte encodeGetdataadditionalOrigSentpaidIndicatorElementDataTag = 0;
		byte encodeGetdataadditionalOrigSentpaidIndicatorElementDataLen = 0;
		int additionalOrigSentpaidIndicatorErr = 0;
		byte encodeGetdataadditionalOrigSentpaidIndicatorElementErrorTag = 0;
		byte encodeGetdataadditionalOrigSentpaidIndicatorElementErrorLen = 0;
		byte[] additionalOrigSentpaidIndicatorErrorEncoded = null;

		int additionalOrigBillingServiceSpareIndicator = -1;
		byte encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeq = 0;
		byte encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeqLen = 0;
		byte encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementIdTag = 0;
		byte encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementIdLen = 0;
		byte[] additionalOrigBillingServiceSpareIndicatorEncoded = null;
		byte encodedadditionalOrigBillingServiceSpareIndicatorTag1 = 0;
		byte encodedadditionalOrigBillingServiceSpareIndicatorTag2 = 0;
		byte encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementDataTag = 0;
		byte encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementDataLen = 0;
		int additionalOrigBillingServiceSpareIndicatorErr = 0;
		byte encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementErrorTag = 0;
		byte encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementErrorLen = 0;
		byte[] additionalOrigBillingServiceSpareIndicatorErrorEncoded = null;

		//if additionalOrigBillingServiceIndicators collective element is requested LIDB will return additionalOrigBillingServiceIndicators collective value
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS) != null){
			additionalOrigBillingServiceIndicators = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigBillingServiceIndicators element is present, values:: "
						+additionalOrigBillingServiceIndicators);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_ERR) != null) {
			additionalOrigBillingServiceIndicatorsErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigBillingServiceIndicators element Error is present, value:: "+additionalOrigBillingServiceIndicatorsErr);
			}
		}
		//			Bits CBA : Additional Originating Third Number Billing Indicator
		//			000 : Screened data
		//			001 : Allowed from this line
		//			010 : Not allowed from this line
		//			011 : Allow with operator verification
		//			100 : Allow with operator or automated verification
		//			101 : Spare
		//			110 : Spare
		//			111 : Spare
		//			
		//			Bits FED : Additional Originating Credit Card Indicator
		//			000 : Screened data
		//			001 : Allowed from this line
		//			010 : Not allowed from this line
		//			011 : Card Issuer restrictions associated with this line
		//			100 : Spare
		//			101 : Spare
		//			110 : Spare
		//			111 : Spare
		//			
		//			Bits HG : Additional Originating Special BNS Indicator
		//			00 : Screened data
		//			01 : Allowed from this line �
		//			10 : Not allowed from this line �
		//			11 : Spare �
		//			
		//			Bits KJI : Additional Originating Sent-Paid Indicator
		//			000 : Screened data
		//			001 : Allowed from this line
		//			010 : Not allowed from this line
		//			011 : Spare
		//			100 : Spare
		//			101 : Spare
		//			110 : Spare
		//			111 : Spare
		//			
		//			Bits PONML : Additional Originating Billing/Service Spare Indicator 
		//			00000 : Screened data
		//			00001
		//			to : Spare
		//			11111
		if (additionalOrigBillingServiceIndicators != null  || additionalOrigBillingServiceIndicatorsErr != 0) {

			encodeGetdataadditionalOrigBillingServiceIndicatorsSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataadditionalOrigBillingServiceIndicatorsSeqLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_SEQ_LEN;
			encodeGetdataadditionalOrigBillingServiceIndicatorsElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataadditionalOrigBillingServiceIndicatorsElementIdLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_TAG_LEN;
			encodedadditionalOrigBillingServiceIndicatorsTag = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_TAG;
			if ( additionalOrigBillingServiceIndicators != null) {
				int additionalOrigThirdNumberInd = Integer.parseInt(additionalOrigBillingServiceIndicators.substring(0,1));
				int additionalOrigCreditCardInd = Integer.parseInt(additionalOrigBillingServiceIndicators.substring(1,2));
				int additionalOrigSpecialBnsInd = Integer.parseInt(additionalOrigBillingServiceIndicators.substring(2,3));
				int additionalOrigSentpaidInd = Integer.parseInt(additionalOrigBillingServiceIndicators.substring(3,4));
				int additionalOrigBillingServiceSpareInd = Integer.parseInt(additionalOrigBillingServiceIndicators.substring(4,6));

				additionalOrigBillingServiceIndicators1 = (additionalOrigSpecialBnsInd & 0x07) << 3;
				additionalOrigBillingServiceIndicators1 |= (additionalOrigCreditCardInd & 0x07);
				additionalOrigBillingServiceIndicators1 = additionalOrigBillingServiceIndicators1 << 3;
				additionalOrigBillingServiceIndicators1 |= (additionalOrigThirdNumberInd & 0x07);

				int additionalOrigBillingServiceIndicators2 = (additionalOrigBillingServiceSpareInd & 0x07) << 3;
				additionalOrigBillingServiceIndicators2 |= (additionalOrigSentpaidInd & 0x07);

				byte[] addOrigBillServiceByteInd = {(byte) additionalOrigBillingServiceIndicators1, (byte) additionalOrigBillingServiceIndicators2};
				additionalOrigBillingServiceIndicatorsEncoded = addOrigBillServiceByteInd;

				if(logger.isDebugEnabled()){
					logger.debug("additionalOrigBillingServiceIndicators element is present, value:: "+additionalOrigBillingServiceIndicatorsEncoded);
				}
				encodeGetdataadditionalOrigBillingServiceIndicatorsElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_OCTET;
				encodeGetdataadditionalOrigBillingServiceIndicatorsElementDataLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_LENGTH;
				totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_LEN_OCTET;

			}
			else if (additionalOrigBillingServiceIndicatorsErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataadditionalOrigBillingServiceIndicatorsElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (additionalOrigBillingServiceIndicatorsErr != 6 ) {
					encodeGetdataadditionalOrigBillingServiceIndicatorsElementErrorLen = 0x02;
					encodeGetdataadditionalOrigBillingServiceIndicatorsSeqLen = 0x07;
					//LIDBElementError VALUE
					additionalOrigBillingServiceIndicatorsErrorEncoded = CommonUtils.formatIntToByte(additionalOrigBillingServiceIndicatorsErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigBillingServiceIndicators Error::"+additionalOrigBillingServiceIndicatorsErr+" encoded as::"+additionalOrigBillingServiceIndicatorsErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_ERR_LEN + 2;

				}
				else {
					encodeGetdataadditionalOrigBillingServiceIndicatorsElementErrorLen = 0x01;
					encodeGetdataadditionalOrigBillingServiceIndicatorsSeqLen = 0x06;
					additionalOrigBillingServiceIndicatorsErrorEncoded = CommonUtils.formatIntToByte(additionalOrigBillingServiceIndicatorsErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigBillingServiceIndicators Error::"+additionalOrigBillingServiceIndicatorsErr+" encoded as::"+additionalOrigBillingServiceIndicatorsErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_ERR_LEN + 1;
				}				
			}
		}
		// The individual addtnl orig. billing indicators can also be requested. 
		//additionalOrigThirdNumberIndicator
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR) != null) {
			additionalOrigThirdNumberIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigThirdNumberIndicator element is present, value:: "+additionalOrigThirdNumberIndicator);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_ERR) != null) {
			additionalOrigThirdNumberIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigThirdNumberIndicator element Error is present, value:: "+additionalOrigThirdNumberIndicatorErr);
			}
		}
		if (additionalOrigThirdNumberIndicator != null  || additionalOrigThirdNumberIndicatorErr != 0) {

			encodeGetdataadditionalOrigThirdNumberIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataadditionalOrigThirdNumberIndicatorSeqLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_SEQ_LEN;
			encodeGetdataadditionalOrigThirdNumberIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataadditionalOrigThirdNumberIndicatorElementIdLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_TAG_LEN;
			encodedadditionalOrigThirdNumberIndicatorTag1 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_TAG1;
			encodedadditionalOrigThirdNumberIndicatorTag2 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_TAG2;
			if ( additionalOrigThirdNumberIndicator != null) {
				encodeGetdataadditionalOrigThirdNumberIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataadditionalOrigThirdNumberIndicatorElementDataLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_LENGTH;
				additionalOrigThirdNumberIndicatorEncoded = asciToHex(additionalOrigThirdNumberIndicator);
				logger.debug("additionalOrigThirdNumberIndicator::"+additionalOrigThirdNumberIndicator+" encoded as::"+additionalOrigThirdNumberIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_LEN_OCTET;
			}
			else if (additionalOrigThirdNumberIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataadditionalOrigThirdNumberIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (additionalOrigThirdNumberIndicatorErr != 6 ) {
					encodeGetdataadditionalOrigThirdNumberIndicatorElementErrorLen = 0x02;
					encodeGetdataadditionalOrigThirdNumberIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					additionalOrigThirdNumberIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigThirdNumberIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigThirdNumberIndicator Error::"+additionalOrigThirdNumberIndicatorErr+" encoded as::"+additionalOrigThirdNumberIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataadditionalOrigThirdNumberIndicatorElementErrorLen = 0x01;
					encodeGetdataadditionalOrigThirdNumberIndicatorSeqLen = 0x07;
					additionalOrigThirdNumberIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigThirdNumberIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigThirdNumberIndicator Error::"+additionalOrigThirdNumberIndicatorErr+" encoded as::"+additionalOrigThirdNumberIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//additionalOrigCreditCardIndicator
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR) != null) {
			additionalOrigCreditCardIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigCreditCardIndicator element is present, value:: "+additionalOrigCreditCardIndicator);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_ERR) != null) {
			additionalOrigCreditCardIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigCreditCardIndicator element Error is present, value:: "+additionalOrigCreditCardIndicatorErr);
			}
		}
		if (additionalOrigCreditCardIndicator != null  || additionalOrigCreditCardIndicatorErr != 0) {

			encodeGetdataadditionalOrigCreditCardIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataadditionalOrigCreditCardIndicatorSeqLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_SEQ_LEN;
			encodeGetdataadditionalOrigCreditCardIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataadditionalOrigCreditCardIndicatorElementIdLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_TAG_LEN;
			encodedadditionalOrigCreditCardIndicatorTag1 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_TAG1;
			encodedadditionalOrigCreditCardIndicatorTag2 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_TAG2;
			if ( additionalOrigCreditCardIndicator != null) {
				encodeGetdataadditionalOrigCreditCardIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataadditionalOrigCreditCardIndicatorElementDataLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_LENGTH;
				additionalOrigCreditCardIndicatorEncoded = asciToHex(additionalOrigCreditCardIndicator);
				logger.debug("additionalOrigCreditCardIndicator::"+additionalOrigCreditCardIndicator+" encoded as::"+additionalOrigCreditCardIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_LEN_OCTET;
			}
			else if (additionalOrigCreditCardIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataadditionalOrigCreditCardIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (additionalOrigCreditCardIndicatorErr != 6 ) {
					encodeGetdataadditionalOrigCreditCardIndicatorElementErrorLen = 0x02;
					encodeGetdataadditionalOrigCreditCardIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					additionalOrigCreditCardIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigCreditCardIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigCreditCardIndicator Error::"+additionalOrigCreditCardIndicatorErr+" encoded as::"+additionalOrigCreditCardIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataadditionalOrigCreditCardIndicatorElementErrorLen = 0x01;
					encodeGetdataadditionalOrigCreditCardIndicatorSeqLen = 0x07;
					additionalOrigCreditCardIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigCreditCardIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigCreditCardIndicator Error::"+additionalOrigCreditCardIndicatorErr+" encoded as::"+additionalOrigCreditCardIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//additionalOrigSpecialBnsIndicator

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR) != null) {
			additionalOrigSpecialBnsIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigSpecialBnsIndicator element is present, value:: "+additionalOrigSpecialBnsIndicator);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_ERR) != null) {
			additionalOrigSpecialBnsIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigSpecialBnsIndicator element Error is present, value:: "+additionalOrigSpecialBnsIndicatorErr);
			}
		}
		if (additionalOrigSpecialBnsIndicator != null  || additionalOrigSpecialBnsIndicatorErr != 0) {

			encodeGetdataadditionalOrigSpecialBnsIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataadditionalOrigSpecialBnsIndicatorSeqLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_SEQ_LEN;
			encodeGetdataadditionalOrigSpecialBnsIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataadditionalOrigSpecialBnsIndicatorElementIdLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_TAG_LEN;
			encodedadditionalOrigSpecialBnsIndicatorTag1 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_TAG1;
			encodedadditionalOrigSpecialBnsIndicatorTag2 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_TAG2;
			if ( additionalOrigSpecialBnsIndicator != null) {
				encodeGetdataadditionalOrigSpecialBnsIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataadditionalOrigSpecialBnsIndicatorElementDataLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_LENGTH;
				additionalOrigSpecialBnsIndicatorEncoded = asciToHex(additionalOrigSpecialBnsIndicator);
				logger.debug("additionalOrigSpecialBnsIndicator::"+additionalOrigSpecialBnsIndicator+" encoded as::"+additionalOrigSpecialBnsIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_LEN_OCTET;
			}
			else if (additionalOrigSpecialBnsIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataadditionalOrigSpecialBnsIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (additionalOrigSpecialBnsIndicatorErr != 6 ) {
					encodeGetdataadditionalOrigSpecialBnsIndicatorElementErrorLen = 0x02;
					encodeGetdataadditionalOrigSpecialBnsIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					additionalOrigSpecialBnsIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigSpecialBnsIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigSpecialBnsIndicator Error::"+additionalOrigSpecialBnsIndicatorErr+" encoded as::"+additionalOrigSpecialBnsIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataadditionalOrigSpecialBnsIndicatorElementErrorLen = 0x01;
					encodeGetdataadditionalOrigSpecialBnsIndicatorSeqLen = 0x07;
					additionalOrigSpecialBnsIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigSpecialBnsIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigSpecialBnsIndicator Error::"+additionalOrigSpecialBnsIndicatorErr+" encoded as::"+additionalOrigSpecialBnsIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_ERR_LEN + 1;
				}				
			}
		}
		//additionalOrigSentpaidIndicator
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SENTPAID_INDICATOR) != null) {
			additionalOrigSentpaidIndicator = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SENTPAID_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigSentpaidIndicator element is present, value:: "+additionalOrigSentpaidIndicator);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SENTPAID_INDICATOR_ERR) != null) {
			additionalOrigSentpaidIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SENTPAID_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigSentpaidIndicator element Error is present, value:: "+additionalOrigSentpaidIndicatorErr);
			}
		}
		if (additionalOrigSentpaidIndicator != -1  || additionalOrigSentpaidIndicatorErr != 0) {

			encodeGetdataadditionalOrigSentpaidIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataadditionalOrigSentpaidIndicatorSeqLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_SEQ_LEN;
			encodeGetdataadditionalOrigSentpaidIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataadditionalOrigSentpaidIndicatorElementIdLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_TAG_LEN;
			encodedadditionalOrigSentpaidIndicatorTag1 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_TAG1;
			encodedadditionalOrigSentpaidIndicatorTag2 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_TAG2;
			if ( additionalOrigSentpaidIndicator != -1) {
				encodeGetdataadditionalOrigSentpaidIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataadditionalOrigSentpaidIndicatorElementDataLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_LENGTH;
				additionalOrigSentpaidIndicatorEncoded = CommonUtils.formatIntToByte(additionalOrigSentpaidIndicator);
				logger.debug("additionalOrigSentpaidIndicator::"+additionalOrigSentpaidIndicator+" encoded as::"+additionalOrigSentpaidIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_LEN_OCTET;
			}
			else if (additionalOrigSentpaidIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataadditionalOrigSentpaidIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (additionalOrigSentpaidIndicatorErr != 6 ) {
					encodeGetdataadditionalOrigSentpaidIndicatorElementErrorLen = 0x02;
					encodeGetdataadditionalOrigSentpaidIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					additionalOrigSentpaidIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigSentpaidIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigSentpaidIndicator Error::"+additionalOrigSentpaidIndicatorErr+" encoded as::"+additionalOrigSentpaidIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataadditionalOrigSentpaidIndicatorElementErrorLen = 0x01;
					encodeGetdataadditionalOrigSentpaidIndicatorSeqLen = 0x07;
					additionalOrigSentpaidIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigSentpaidIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigSentpaidIndicator Error::"+additionalOrigSentpaidIndicatorErr+" encoded as::"+additionalOrigSentpaidIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//additionalOrigBillingServiceSpareIndicator
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR) != null) {
			additionalOrigBillingServiceSpareIndicator = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigBillingServiceSpareIndicator element is present, value:: "+additionalOrigBillingServiceSpareIndicator);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR) != null) {
			additionalOrigBillingServiceSpareIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("additionalOrigBillingServiceSpareIndicator element Error is present, value:: "+additionalOrigBillingServiceSpareIndicatorErr);
			}
		}
		if (additionalOrigBillingServiceSpareIndicator != -1  || additionalOrigBillingServiceSpareIndicatorErr != 0) {

			encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeqLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_SEQ_LEN;
			encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementIdLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG_LEN;
			encodedadditionalOrigBillingServiceSpareIndicatorTag1 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG1;
			encodedadditionalOrigBillingServiceSpareIndicatorTag2 = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG2;
			if ( additionalOrigBillingServiceSpareIndicator != -1) {
				encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementDataLen = GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_LENGTH;
				additionalOrigBillingServiceSpareIndicatorEncoded = CommonUtils.formatIntToByte(additionalOrigBillingServiceSpareIndicator);
				logger.debug("additionalOrigBillingServiceSpareIndicator::"+additionalOrigBillingServiceSpareIndicator+" encoded as::"+additionalOrigBillingServiceSpareIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_LEN_OCTET;
			}
			else if (additionalOrigBillingServiceSpareIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (additionalOrigBillingServiceSpareIndicatorErr != 6 ) {
					encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementErrorLen = 0x02;
					encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					additionalOrigBillingServiceSpareIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigBillingServiceSpareIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigBillingServiceSpareIndicator Error::"+additionalOrigBillingServiceSpareIndicatorErr+" encoded as::"+additionalOrigBillingServiceSpareIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementErrorLen = 0x01;
					encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeqLen = 0x07;
					additionalOrigBillingServiceSpareIndicatorErrorEncoded = CommonUtils.formatIntToByte(additionalOrigBillingServiceSpareIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("additionalOrigBillingServiceSpareIndicator Error::"+additionalOrigBillingServiceSpareIndicatorErr+" encoded as::"+additionalOrigBillingServiceSpareIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//alphanumeric string
		String alphanumericString = null;
		byte encodeGetdataalphanumericStringSeq = 0;
		byte encodeGetdataalphanumericStringSeqLen = 0;
		byte encodeGetdataalphanumericStringElementIdTag = 0;
		byte encodeGetdataalphanumericStringElementIdLen = 0;
		byte[] alphanumericStringEncoded = null;
		byte encodedalphanumericStringTag = 0;
		byte encodeGetdataalphanumericStringElementDataTag = 0;
		byte encodeGetdataalphanumericStringElementDataLen = 0;

		int alphanumericStringErr = 0;
		byte encodeGetdataalphanumericStringElementErrorTag = 0;
		byte encodeGetdataalphanumericStringElementErrorLen = 0;
		byte[] alphanumericStringErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ALPHANUMERIC_STRING) != null) {
			alphanumericString = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ALPHANUMERIC_STRING);
			if(logger.isDebugEnabled()){
				logger.debug("alphanumericString element is present, value:: "+alphanumericString);
			}

			if(!StringUtils.isNotBlank(alphanumericString)){
				alphanumericString = null;
			}
		}

		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ALPHANUMERIC_STRING_ERR) != null) {
			alphanumericStringErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ALPHANUMERIC_STRING_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("alphanumericString element Error is present, value:: "+alphanumericStringErr);
			}
		}

		if ( (alphanumericString != null && !alphanumericString.isEmpty()) || alphanumericStringErr != 0) {
			encodeGetdataalphanumericStringSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataalphanumericStringElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataalphanumericStringElementIdLen = GetDataConstants.GETDATA_ALPHANUMERIC_STRING_TAG_LEN;
			encodedalphanumericStringTag = GetDataConstants.GETDATA_ALPHANUMERIC_STRING_TAG;
			if ( alphanumericString != null && !alphanumericString.isEmpty()) {
				if (alphanumericString.length() > 40) {
					logger.info("encodeGetDataQuery: throwing exception ");
					logger.error("encodeGetDataQuery: alphanumeric string length should be less than 40 char");
					throw new AINCodecException("alphanumeric string length should be less than 40 char");
				}
				//encodeGetdataalphanumericStringSeqLen += (byte) (alphanumericString.length());
				encodeGetdataalphanumericStringElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_IA5String;

				alphanumericStringEncoded = encodeIa5(alphanumericString);
				logger.debug("alphanumericString::"+alphanumericString+" encoded as::"+alphanumericStringEncoded);
				int alphanumericStringSeqLen =  alphanumericStringEncoded.length + 5; // LIDBElementIdentifier Tag,LIDBElementIdentifier Length,LIDBElementIdentifier Value,LIDBElementData Tag,LIDBElementData Length , alphanumericString 
				encodeGetdataalphanumericStringSeqLen += (byte) alphanumericStringSeqLen;
				encodeGetdataalphanumericStringElementDataLen = (byte) alphanumericStringEncoded.length;
				totalLength += alphanumericString.length() + 7; // SEQUENCE, SEQUENCE Length,  LIDBElementIdentifier Tag,LIDBElementIdentifier Length,LIDBElementIdentifier Value,LIDBElementData Tag,LIDBElementData Length , alphanumericString
			}
			else if (alphanumericStringErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataalphanumericStringElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (alphanumericStringErr != 6 ) {
					encodeGetdataalphanumericStringElementErrorLen = 0x02;
					encodeGetdataalphanumericStringSeqLen = 0x07;
					//LIDBElementError VALUE
					alphanumericStringErrorEncoded = CommonUtils.formatIntToByte(alphanumericStringErr);
					if(logger.isDebugEnabled()){
						logger.debug("alphanumericString Error::"+alphanumericStringErr+" encoded as::"+alphanumericStringErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ALPHANUMERIC_STRING_ERR_LEN + 2;

				}
				else {
					encodeGetdataalphanumericStringElementErrorLen = 0x01;
					encodeGetdataalphanumericStringSeqLen = 0x06;
					alphanumericStringErrorEncoded = CommonUtils.formatIntToByte(alphanumericStringErr);
					if(logger.isDebugEnabled()){
						logger.debug("alphanumericString Error::"+alphanumericStringErr+" encoded as::"+alphanumericStringErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ALPHANUMERIC_STRING_ERR_LEN + 1;
				}				
			}
		}


		//alternate preferred IC
		byte encodeGetdataalternatePreferredIcSeq = 0;
		byte encodeGetdataalternatePreferredIcSeqLen = 0;
		byte encodeGetdataalternatePreferredIcElementIdTag = 0;
		byte encodeGetdataalternatePreferredIcElementIdLen = 0;
		byte[] alternatePreferredIcEncoded = null;
		byte encodedalternatePreferredIcTag1 = 0;
		byte encodedalternatePreferredIcTag2 = 0;
		byte encodedalternatePreferredIcLength = 0;
		byte encodeGetdataalternatePreferredIcElementDataTag1 = 0;
		byte encodeGetdataalternatePreferredIcElementDataTag2 = 0;
		byte encodeGetdataalternatePreferredIcElementDataLen = 0;
		String alternatePreferredIc = null;
		int alternatePreferredIcErr = 0;
		byte encodeGetdataalternatePreferredIcElementErrorTag = 0;
		byte encodeGetdataalternatePreferredIcElementErrorLen = 0;
		byte[] alternatePreferredIcErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC) != null) {
			alternatePreferredIc = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC);
			if(logger.isDebugEnabled()){
				logger.debug("alternatePreferredIc element is present, value:: "+alternatePreferredIc);
			}

			if(!StringUtils.isNotBlank(alternatePreferredIc)){
				alternatePreferredIc = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_ERR) != null) {
			alternatePreferredIcErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("alternatePreferredIc element Error is present, value:: "+alternatePreferredIcErr);
			}
		}


		if ( (alternatePreferredIc != null && !alternatePreferredIc.isEmpty()) || alternatePreferredIcErr != 0) {
			encodeGetdataalternatePreferredIcSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataalternatePreferredIcSeqLen = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_SEQ_LEN;
			encodeGetdataalternatePreferredIcElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataalternatePreferredIcElementIdLen = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_TAG_LEN;
			encodedalternatePreferredIcTag1 = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_TAG1;
			encodedalternatePreferredIcTag2 = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_TAG2;
			if ( alternatePreferredIc != null && !alternatePreferredIc.isEmpty()) {
				encodedalternatePreferredIcLength = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_LENGTH;
				encodeGetdataalternatePreferredIcElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_1;
				encodeGetdataalternatePreferredIcElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_2;
				encodeGetdataalternatePreferredIcElementDataLen = GetDataConstants.GETDATA_LIDB_ELEMENT_DATA_BCD_DIG;
				alternatePreferredIcEncoded = encodeAdrsSignalForGetQuery(alternatePreferredIc);
				logger.debug("alternatePreferredIc::"+alternatePreferredIc+" encoded as::"+alternatePreferredIcEncoded);
				totalLength += GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_LEN_OCTET;
			}
			else if (alternatePreferredIcErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataalternatePreferredIcElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (alternatePreferredIcErr != 6 ) {
					encodeGetdataalternatePreferredIcElementErrorLen = 0x02;
					encodeGetdataalternatePreferredIcSeqLen = 0x08;
					//LIDBElementError VALUE
					alternatePreferredIcErrorEncoded = CommonUtils.formatIntToByte(alternatePreferredIcErr);
					if(logger.isDebugEnabled()){
						logger.debug("alternatePreferredIc Error::"+alternatePreferredIcErr+" encoded as::"+alternatePreferredIcErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_ERR_LEN + 2;

				}
				else {
					encodeGetdataalternatePreferredIcElementErrorLen = 0x01;
					encodeGetdataalternatePreferredIcSeqLen = 0x07;
					alternatePreferredIcErrorEncoded = CommonUtils.formatIntToByte(alternatePreferredIcErr);
					if(logger.isDebugEnabled()){
						logger.debug("alternatePreferredIc Error::"+alternatePreferredIcErr+" encoded as::"+alternatePreferredIcErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_ERR_LEN + 1;
				}				
			}
		}

		//Billing service provider
		String billingServiceProvider = null;
		byte encodeGetdatabillingServiceProviderSeq = 0;
		byte encodeGetdatabillingServiceProviderSeqLen = 0;
		byte encodeGetdatabillingServiceProviderElementIdTag = 0;
		byte encodeGetdatabillingServiceProviderElementIdLen = 0;
		byte encodedbillingServiceProviderTag1 = 0;
		byte encodedbillingServiceProviderTag2 = 0;
		byte encodedbillingServiceProviderLength = 0;
		byte encodeGetdatabillingServiceProviderElementDataTag = 0;
		byte encodeGetdatabillingServiceProviderElementDataLen = 0;
		byte[] billingServiceProviderEncoded = null;
		int billingServiceProviderErr = 0;
		byte encodeGetdatabillingServiceProviderElementErrorTag = 0;
		byte encodeGetdatabillingServiceProviderElementErrorLen = 0;
		byte[] billingServiceProviderErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_BILLING_SERVICE_PROVIDER) != null) {
			billingServiceProvider = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_BILLING_SERVICE_PROVIDER);
			if(logger.isDebugEnabled()){
				logger.debug("billingServiceProvider element is present, value:: "+billingServiceProvider);
			}

			if(!StringUtils.isNotBlank(billingServiceProvider)){
				billingServiceProvider = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_BILLING_SERVICE_PROVIDER_ERR) != null) {
			billingServiceProviderErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_BILLING_SERVICE_PROVIDER_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("billingServiceProvider element Error is present, value:: "+billingServiceProviderErr);
			}
		}

		if ( (billingServiceProvider != null && !billingServiceProvider.isEmpty()) || billingServiceProviderErr != 0) {
			encodeGetdatabillingServiceProviderSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatabillingServiceProviderSeqLen = GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_SEQ_LEN;
			encodeGetdatabillingServiceProviderElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatabillingServiceProviderElementIdLen = GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_TAG_LEN;
			encodedbillingServiceProviderTag1 = GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_TAG1;
			encodedbillingServiceProviderTag2 = GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_TAG2;
			encodedbillingServiceProviderLength = GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_LENGTH;
			if ( billingServiceProvider != null && !billingServiceProvider.isEmpty()) {
				encodeGetdatabillingServiceProviderElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_IA5String;
				encodeGetdatabillingServiceProviderElementDataLen = GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_LENGTH;
				billingServiceProviderEncoded = encodeIa5(billingServiceProvider);
				logger.debug("billingServiceProvider::"+billingServiceProvider+" encoded as::"+billingServiceProviderEncoded);
				totalLength += GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_LEN_OCTET;
			}
			else if (billingServiceProviderErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatabillingServiceProviderElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (billingServiceProviderErr != 6 ) {
					encodeGetdatabillingServiceProviderElementErrorLen = 0x02;
					encodeGetdatabillingServiceProviderSeqLen = 0x08;
					//LIDBElementError VALUE
					billingServiceProviderErrorEncoded = CommonUtils.formatIntToByte(billingServiceProviderErr);
					if(logger.isDebugEnabled()){
						logger.debug("billingServiceProvider Error::"+billingServiceProviderErr+" encoded as::"+billingServiceProviderErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_ERR_LEN + 2;

				}
				else {
					encodeGetdatabillingServiceProviderElementErrorLen = 0x01;
					encodeGetdatabillingServiceProviderSeqLen = 0x07;
					billingServiceProviderErrorEncoded = CommonUtils.formatIntToByte(billingServiceProviderErr);
					if(logger.isDebugEnabled()){
						logger.debug("billingServiceProvider Error::"+billingServiceProviderErr+" encoded as::"+billingServiceProviderErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_BILLING_SERVICE_PROVIDER_ERR_LEN + 1;
				}				
			}
		}


		//serviceDenialIndicator
		String serviceDenialIndicator = null;
		byte encodeGetdataserviceDenialIndicatorSeq = 0;
		byte encodeGetdataserviceDenialIndicatorSeqLen = 0;
		byte encodeGetdataserviceDenialIndicatorElementIdTag = 0;
		byte encodeGetdataserviceDenialIndicatorElementIdLen = 0;
		byte[] serviceDenialIndicatorEncoded = null;
		byte encodedserviceDenialIndicatorTag = 0;
		byte encodeGetdataserviceDenialIndicatorElementDataTag = 0;
		byte encodeGetdataserviceDenialIndicatorElementDataLen = 0;
		int serviceDenialIndicatorErr = 0;
		byte encodeGetdataserviceDenialIndicatorElementErrorTag = 0;
		byte encodeGetdataserviceDenialIndicatorElementErrorLen = 0;
		byte[] serviceDenialIndicatorErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_DENIAL_INDICATOR) != null) {
			serviceDenialIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_DENIAL_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("serviceDenialIndicator element is present, value:: "+serviceDenialIndicator);
			}

			if(!StringUtils.isNotBlank(serviceDenialIndicator)){
				serviceDenialIndicator = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_DENIAL_INDICATOR_ERR) != null) {
			serviceDenialIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_DENIAL_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("serviceDenialIndicator element Error is present, value:: "+serviceDenialIndicatorErr);
			}
		}

		if ( (serviceDenialIndicator != null && !serviceDenialIndicator.isEmpty()) || serviceDenialIndicatorErr != 0) {
			encodeGetdataserviceDenialIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataserviceDenialIndicatorSeqLen = GetDataConstants.GETDATA_SERVICE_DENIAL_INDICATOR_SEQ_LEN;
			encodeGetdataserviceDenialIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataserviceDenialIndicatorElementIdLen = GetDataConstants.GETDATA_SERVICE_DENIAL_INDICATOR_TAG_LEN;
			encodedserviceDenialIndicatorTag = GetDataConstants.GETDATA_SERVICE_DENIAL_INDICATOR_TAG;
			if ( serviceDenialIndicator != null && !serviceDenialIndicator.isEmpty()) {
				encodeGetdataserviceDenialIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataserviceDenialIndicatorElementDataLen = GetDataConstants.GETDATA_SERVICE_DENIAL_INDICATOR_LENGTH;
				int serviceDenialIndInt = Integer.parseInt(serviceDenialIndicator);
				serviceDenialIndicatorEncoded = CommonUtils.formatIntToByte(serviceDenialIndInt);
				//serviceDenialIndicatorEncoded = asciToHex(serviceDenialIndicator);
				logger.debug("serviceDenialIndicator::"+serviceDenialIndicator+" encoded as::"+serviceDenialIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_SERVICE_DENIAL_INDICATOR_LEN_OCTET;
			}
			else if (serviceDenialIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataserviceDenialIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (serviceDenialIndicatorErr != 6 ) {
					encodeGetdataserviceDenialIndicatorElementErrorLen = 0x02;
					encodeGetdataserviceDenialIndicatorSeqLen = 0x07;
					//LIDBElementError VALUE
					serviceDenialIndicatorErrorEncoded = CommonUtils.formatIntToByte(serviceDenialIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("serviceDenialIndicator Error::"+serviceDenialIndicatorErr+" encoded as::"+serviceDenialIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_SERVICE_DENIAL_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataserviceDenialIndicatorElementErrorLen = 0x01;
					encodeGetdataserviceDenialIndicatorSeqLen = 0x06;
					serviceDenialIndicatorErrorEncoded = CommonUtils.formatIntToByte(serviceDenialIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("serviceDenialIndicator Error::"+serviceDenialIndicatorErr+" encoded as::"+serviceDenialIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_SERVICE_DENIAL_INDICATOR_ERR_LEN + 1;
				}				
			}
		}


		//collectAcceptanceIndicator
		String collectAcceptanceIndicator = null;
		byte encodeGetdatacollectAcceptanceIndicatorSeq =  0;
		byte encodeGetdatacollectAcceptanceIndicatorSeqLen =  0;
		byte encodeGetdatacollectAcceptanceIndicatorElementIdTag =  0;
		byte encodeGetdatacollectAcceptanceIndicatorElementIdLen =  0;
		byte encodedcollectAcceptanceIndicatorTag =  0;
		byte encodedcollectAcceptanceIndicatorLength =  0;
		byte encodeGetdatacollectAcceptanceIndicatorElementDataTag =  0;
		byte encodeGetdatacollectAcceptanceIndicatorElementDataLen =  0;
		byte[] collectAcceptanceIndicatorEncoded =  null;
		int collectAcceptanceIndicatorErr = 0;
		byte encodeGetdatacollectAcceptanceIndicatorElementErrorTag = 0;
		byte encodeGetdatacollectAcceptanceIndicatorElementErrorLen = 0;
		byte[] collectAcceptanceIndicatorErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_COLLECT_ACCEPTANCE_INDICATOR) != null) {
			collectAcceptanceIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_COLLECT_ACCEPTANCE_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("collectAcceptanceIndicator element is present, value:: "+collectAcceptanceIndicator);
			}

			if(!StringUtils.isNotBlank(collectAcceptanceIndicator)){
				collectAcceptanceIndicator = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_COLLECT_ACCEPTANCE_INDICATOR_ERR) != null) {
			collectAcceptanceIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_COLLECT_ACCEPTANCE_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("collectAcceptanceIndicator element Error is present, value:: "+collectAcceptanceIndicatorErr);
			}
		}


		if ( (collectAcceptanceIndicator != null && !collectAcceptanceIndicator.isEmpty()) || collectAcceptanceIndicatorErr != 0) {
			encodeGetdatacollectAcceptanceIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatacollectAcceptanceIndicatorSeqLen = GetDataConstants.GETDATA_COLLECT_ACCEPTANCE_INDICATOR_SEQ_LEN;
			encodeGetdatacollectAcceptanceIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatacollectAcceptanceIndicatorElementIdLen = GetDataConstants.GETDATA_COLLECT_ACCEPTANCE_INDICATOR_TAG_LEN;
			encodedcollectAcceptanceIndicatorTag = GetDataConstants.GETDATA_COLLECT_ACCEPTANCE_INDICATOR_TAG;
			encodedcollectAcceptanceIndicatorLength = GetDataConstants.GETDATA_COLLECT_ACCEPTANCE_INDICATOR_LENGTH;
			if ( collectAcceptanceIndicator != null && !collectAcceptanceIndicator.isEmpty()) {
				encodeGetdatacollectAcceptanceIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdatacollectAcceptanceIndicatorElementDataLen = GetDataConstants.GETDATA_COLLECT_ACCEPTANCE_INDICATOR_LENGTH;
				//collectAcceptanceIndicatorEncoded = asciToHex(collectAcceptanceIndicator);
				int collectAcceptanceIndInt = Integer.parseInt(collectAcceptanceIndicator);
				collectAcceptanceIndicatorEncoded = CommonUtils.formatIntToByte(collectAcceptanceIndInt);
				logger.debug("collectAcceptanceIndicator::"+collectAcceptanceIndicator+" encoded as::"+collectAcceptanceIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_COLLECT_ACCEPTANCE_INDICATOR_LEN_OCTET;
			}
			else if (collectAcceptanceIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatacollectAcceptanceIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (collectAcceptanceIndicatorErr != 6 ) {
					encodeGetdatacollectAcceptanceIndicatorElementErrorLen = 0x02;
					encodeGetdatacollectAcceptanceIndicatorSeqLen = 0x07;
					//LIDBElementError VALUE
					collectAcceptanceIndicatorErrorEncoded = CommonUtils.formatIntToByte(collectAcceptanceIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("collectAcceptanceIndicator Error::"+collectAcceptanceIndicatorErr+" encoded as::"+collectAcceptanceIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_COLLECT_ACCEPTANCE_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdatacollectAcceptanceIndicatorElementErrorLen = 0x01;
					encodeGetdatacollectAcceptanceIndicatorSeqLen = 0x06;
					collectAcceptanceIndicatorErrorEncoded = CommonUtils.formatIntToByte(collectAcceptanceIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("collectAcceptanceIndicator Error::"+collectAcceptanceIndicatorErr+" encoded as::"+collectAcceptanceIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_COLLECT_ACCEPTANCE_INDICATOR_ERR_LEN + 1;
				}				
			}
		}


		//diversionRoutingNumber
		PhoneNumber diversionRoutingNumber = null;
		byte encodeGetdatadiversionRoutingNumberSeq =  0;
		byte encodeGetdatadiversionRoutingNumberSeqLen =  0;
		byte encodeGetdatadiversionRoutingNumberElementIdTag =  0;
		byte encodeGetdatadiversionRoutingNumberElementIdLen =  0;
		byte encodeddiversionRoutingNumberTag1 =  0;
		byte encodeddiversionRoutingNumberTag2 =  0;
		byte encodeGetdatadiversionRoutingNumberElementDataTag1 =  0;
		byte encodeGetdatadiversionRoutingNumberElementDataTag2 =  0;
		byte encodeGetdatadiversionRoutingNumberElementDataLen =  0;
		byte[] diversionRoutingNumberEncoded =  null;
		int diversionRoutingNumberErr = 0;
		byte encodeGetdatadiversionRoutingNumberElementErrorTag = 0;
		byte encodeGetdatadiversionRoutingNumberElementErrorLen = 0;
		byte[] diversionRoutingNumberErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_DIVERSION_ROUTING_NUMBER) != null) {
			diversionRoutingNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GETDATA_O_DIVERSION_ROUTING_NUMBER);
			if(logger.isDebugEnabled()){
				logger.debug("diversionRoutingNumber element is present, value:: "+diversionRoutingNumber);
			}

			if(!StringUtils.isNotBlank(diversionRoutingNumber.getAddress())){
				diversionRoutingNumber = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_DIVERSION_ROUTING_NUMBER_ERR) != null) {
			diversionRoutingNumberErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_DIVERSION_ROUTING_NUMBER_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("diversionRoutingNumber element Error is present, value:: "+diversionRoutingNumberErr);
			}
		}


		if ( diversionRoutingNumber != null || diversionRoutingNumberErr != 0) {
			encodeGetdatadiversionRoutingNumberSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatadiversionRoutingNumberSeqLen = 0;
			encodeGetdatadiversionRoutingNumberElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatadiversionRoutingNumberElementIdLen = GetDataConstants.GETDATA_DIVERSION_ROUTING_NUMBER_TAG_LEN;
			encodeddiversionRoutingNumberTag1 = GetDataConstants.GETDATA_DIVERSION_ROUTING_NUMBER_TAG1;
			encodeddiversionRoutingNumberTag2 = GetDataConstants.GETDATA_DIVERSION_ROUTING_NUMBER_TAG2;
			if ( diversionRoutingNumber != null ) {
				if(diversionRoutingNumber.getAddress().length() > 10){
					diversionRoutingNumber.setNatureOfAddress(1);
				}else if(diversionRoutingNumber.getAddress().length() <= 10){
					diversionRoutingNumber.setNatureOfAddress(0);
				}
				encodeGetdatadiversionRoutingNumberElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_LIDB_DIGITS_1;
				encodeGetdatadiversionRoutingNumberElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_LIDB_DIGITS_2;
				diversionRoutingNumberEncoded = encodeAdrsSignalForGetQuery(diversionRoutingNumber.getAddress());
				logger.debug("diversionRoutingNumber::"+diversionRoutingNumber+" encoded as::"+diversionRoutingNumberEncoded);
				int diversionRoutingNumberSeqLen = diversionRoutingNumberEncoded.length + 6;
				encodeGetdatadiversionRoutingNumberSeqLen = (byte) diversionRoutingNumberSeqLen ;
				encodeGetdatadiversionRoutingNumberElementDataLen = (byte) diversionRoutingNumberEncoded.length;
				totalLength += diversionRoutingNumberEncoded.length + 13;
			}
			else if (diversionRoutingNumberErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatadiversionRoutingNumberElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (diversionRoutingNumberErr != 6 ) {
					encodeGetdatadiversionRoutingNumberElementErrorLen = 0x02;
					encodeGetdatadiversionRoutingNumberSeqLen = 0x08;
					//LIDBElementError VALUE
					diversionRoutingNumberErrorEncoded = CommonUtils.formatIntToByte(diversionRoutingNumberErr);
					if(logger.isDebugEnabled()){
						logger.debug("diversionRoutingNumber Error::"+diversionRoutingNumberErr+" encoded as::"+diversionRoutingNumberErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_DIVERSION_ROUTING_NUMBER_ERR_LEN + 2;

				}
				else {
					encodeGetdatadiversionRoutingNumberElementErrorLen = 0x01;
					encodeGetdatadiversionRoutingNumberSeqLen = 0x07;
					diversionRoutingNumberErrorEncoded = CommonUtils.formatIntToByte(diversionRoutingNumberErr);
					if(logger.isDebugEnabled()){
						logger.debug("diversionRoutingNumber Error::"+diversionRoutingNumberErr+" encoded as::"+diversionRoutingNumberErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_DIVERSION_ROUTING_NUMBER_ERR_LEN + 1;
				}				
			}

		}

		//foreignLanguageIdentifier Element 
		String foreignLanguageIdentifier = null;
		byte encodeGetdataforeignLanguageIdentifierSeq = 0;
		byte encodeGetdataforeignLanguageIdentifierSeqLen = 0;
		byte encodeGetdataforeignLanguageIdentifierElementIdTag = 0;
		byte encodeGetdataforeignLanguageIdentifierElementIdLen = 0;
		byte[] foreignLanguageIdentifierEncoded = null;
		byte encodedforeignLanguageIdentifierTag = 0;
		byte encodeGetdataforeignLanguageIdentifierElementDataTag = 0;
		byte encodeGetdataforeignLanguageIdentifierElementDataLen = 0;
		int foreignLanguageIdentifierErr = 0;
		byte encodeGetdataforeignLanguageIdentifierElementErrorTag = 0;
		byte encodeGetdataforeignLanguageIdentifierElementErrorLen = 0;
		byte[] foreignLanguageIdentifierErrorEncoded = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_FOREIGN_LANGUAGE_IDENTIFIER) != null) {
			foreignLanguageIdentifier = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_FOREIGN_LANGUAGE_IDENTIFIER);
			if(logger.isDebugEnabled()){
				logger.debug("foreignLanguageIdentifier element is present, value:: "+foreignLanguageIdentifier);
			}

			if(!StringUtils.isNotBlank(foreignLanguageIdentifier)){
				foreignLanguageIdentifier = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_FOREIGN_LANGUAGE_IDENTIFIER_ERR) != null) {
			foreignLanguageIdentifierErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_FOREIGN_LANGUAGE_IDENTIFIER_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("foreignLanguageIdentifier element Error is present, value:: "+foreignLanguageIdentifierErr);
			}
		}

		if ( (foreignLanguageIdentifier != null && !foreignLanguageIdentifier.isEmpty()) || foreignLanguageIdentifierErr != 0) {
			//SEQUENCE of foreignLanguageIdentifier
			encodeGetdataforeignLanguageIdentifierSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof foreignLanguageIdentifier
			encodeGetdataforeignLanguageIdentifierSeqLen = GetDataConstants.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_SEQ_LEN;

			//foreignLanguageIdentifier LIDBElementIdentifier TAG
			encodeGetdataforeignLanguageIdentifierElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//foreignLanguageIdentifier LIDBElementIdentifier LENGTH
			encodeGetdataforeignLanguageIdentifierElementIdLen = GetDataConstants.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_TAG_LEN;


			//foreignLanguageIdentifier LIDBElementIdentifier VALUE. i.e foreignLanguageIdentifier identifier
			encodedforeignLanguageIdentifierTag = GetDataConstants.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_TAG;
			if ( foreignLanguageIdentifier != null && !foreignLanguageIdentifier.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdataforeignLanguageIdentifierElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				//LIDBElementData LENGTH (length of foreignLanguageIdentifier)
				encodeGetdataforeignLanguageIdentifierElementDataLen = GetDataConstants.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_LENGTH;
				//LIDBElementData VALUE . i.e foreignLanguageIdentifier value
				//foreignLanguageIdentifierEncoded = asciToHex(foreignLanguageIdentifier);
				int foreignLanguageIdentifierInt = Integer.parseInt(foreignLanguageIdentifier);
				foreignLanguageIdentifierEncoded = CommonUtils.formatIntToByte(foreignLanguageIdentifierInt);
				logger.debug("foreignLanguageIdentifier::"+foreignLanguageIdentifier+" encoded as::"+foreignLanguageIdentifierEncoded);
				totalLength += GetDataConstants.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_LEN_OCTET;
			}
			else if (foreignLanguageIdentifierErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataforeignLanguageIdentifierElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (foreignLanguageIdentifierErr != 6 ) {
					encodeGetdataforeignLanguageIdentifierElementErrorLen = 0x02;
					encodeGetdataforeignLanguageIdentifierSeqLen = 0x07;
					//LIDBElementError VALUE
					foreignLanguageIdentifierErrorEncoded = CommonUtils.formatIntToByte(foreignLanguageIdentifierErr);
					if(logger.isDebugEnabled()){
						logger.debug("foreignLanguageIdentifier Error::"+foreignLanguageIdentifierErr+" encoded as::"+foreignLanguageIdentifierErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_ERR_LEN + 2;

				}
				else {
					encodeGetdataforeignLanguageIdentifierElementErrorLen = 0x01;
					encodeGetdataforeignLanguageIdentifierSeqLen = 0x06;
					foreignLanguageIdentifierErrorEncoded = CommonUtils.formatIntToByte(foreignLanguageIdentifierErr);
					if(logger.isDebugEnabled()){
						logger.debug("foreignLanguageIdentifier Error::"+foreignLanguageIdentifierErr+" encoded as::"+foreignLanguageIdentifierErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER_ERR_LEN + 1;
				}				
			}
		}		


		//genericName Element 
		String genericName = null;
		byte encodeGetdatagenericNameSeq = 0;
		byte encodeGetdatagenericNameSeqLen = 0;
		byte encodeGetdatagenericNameElementIdTag = 0;
		byte encodeGetdatagenericNameElementIdLen = 0;
		byte[] genericNameEncoded = null;
		byte encodedgenericNameTag = 0;
		byte encodeGetdatagenericNameElementDataTag = 0;
		byte encodeGetdatagenericNameElementDataLen = 0;
		int genericNameErr = 0;
		byte encodeGetdatagenericNameElementErrorTag = 0;
		byte encodeGetdatagenericNameElementErrorLen = 0;
		byte[] genericNameErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_GENERIC_NAME) != null) {
			genericName = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_GENERIC_NAME);
			if(logger.isDebugEnabled()){
				logger.debug("genericName element is present, value:: "+genericName);
			}

			if(!StringUtils.isNotBlank(genericName)){
				genericName = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_GENERIC_NAME_ERR) != null) {
			genericNameErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_GENERIC_NAME_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("genericName element Error is present, value:: "+genericNameErr);
			}
		}

		if ( (genericName != null && !genericName.isEmpty()) || genericNameErr != 0) {
			//			if (genericName.length() > 15) {
			//				logger.info("encodeGetDataQuery: throwing exception ");
			//				logger.error("encodeGetDataQuery: generic name length should be less than 15");
			//				throw new AINCodecException("generic name length should be less than 15");
			//			}
			//SEQUENCE of genericName
			encodeGetdatagenericNameSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof genericName	

			//genericName LIDBElementIdentifier TAG
			encodeGetdatagenericNameElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;

			//genericName LIDBElementIdentifier LENGTH
			encodeGetdatagenericNameElementIdLen = GetDataConstants.GETDATA_GENERIC_NAME_TAG_LEN;

			//genericName LIDBElementIdentifier VALUE. i.e genericName identifier
			encodedgenericNameTag = GetDataConstants.GETDATA_GENERIC_NAME_TAG;

			if ( genericName != null && !genericName.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdatagenericNameElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_IA5String;

				//LIDBElementData LENGTH (length of genericName)
				int genericNameElementDataLen = genericName.length();
				int genericNameSeqLen =  genericName.length() + 5;
				encodeGetdatagenericNameElementDataLen = (byte) genericNameElementDataLen;
				encodeGetdatagenericNameSeqLen = (byte) genericNameSeqLen;

				//LIDBElementData VALUE . i.e genericName value
				genericNameEncoded = encodeIa5(genericName);
				logger.debug("genericName::"+genericName+" encoded as::"+genericNameEncoded);
				totalLength += genericNameElementDataLen + 7;
			}
			else if (genericNameErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatagenericNameElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (genericNameErr != 6 ) {
					encodeGetdatagenericNameElementErrorLen = 0x02;
					encodeGetdatagenericNameSeqLen = 0x07;

					//LIDBElementError VALUE
					genericNameErrorEncoded = CommonUtils.formatIntToByte(genericNameErr);
					if(logger.isDebugEnabled()){
						logger.debug("genericName Error::"+genericNameErr+" encoded as::"+genericNameErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_GENERIC_NAME_ERR_LEN + 2;

				}
				else {
					encodeGetdatagenericNameElementErrorLen = 0x01;
					encodeGetdatagenericNameSeqLen = 0x06;
					genericNameErrorEncoded = CommonUtils.formatIntToByte(genericNameErr);
					if(logger.isDebugEnabled()){
						logger.debug("genericName Error::"+genericNameErr+" encoded as::"+genericNameErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_GENERIC_NAME_ERR_LEN + 1;
				}				
			}
		}

		// icIndicators element
		byte encodeGetdataicIndicatorsSeq = 0;
		byte encodeGetdataicIndicatorsSeqLen = 0;
		byte encodeGetdataicIndicatorsElementIdTag = 0;
		byte encodeGetdataicIndicatorsElementIdLen = 0;
		byte[] icIndicatorsEncoded = null;
		byte encodedicIndicatorsTag = 0;
		byte encodeGetdataicIndicatorsElementDataTag = 0;
		byte encodeGetdataicIndicatorsElementDataLen = 0;
		int icIndicatorsErr = 0;
		byte encodeGetdataicIndicatorsElementErrorTag = 0;
		byte encodeGetdataicIndicatorsElementErrorLen = 0;
		byte[] icIndicatorsErrorEncoded = null;

		int primaryPreferredIcIndicator = -1;
		byte encodeGetdataprimaryPreferredIcIndicatorSeq = 0;
		byte encodeGetdataprimaryPreferredIcIndicatorSeqLen = 0;
		byte encodeGetdataprimaryPreferredIcIndicatorElementIdTag = 0;
		byte encodeGetdataprimaryPreferredIcIndicatorElementIdLen = 0;
		byte[] primaryPreferredIcIndicatorEncoded = null;
		byte encodedprimaryPreferredIcIndicatorTag1 = 0;
		byte encodedprimaryPreferredIcIndicatorTag2 = 0;
		byte encodeGetdataprimaryPreferredIcIndicatorElementDataTag = 0;
		byte encodeGetdataprimaryPreferredIcIndicatorElementDataLen = 0;
		int primaryPreferredIcIndicatorErr = 0;
		byte encodeGetdataprimaryPreferredIcIndicatorElementErrorTag = 0;
		byte encodeGetdataprimaryPreferredIcIndicatorElementErrorLen = 0;
		byte[] primaryPreferredIcIndicatorErrorEncoded = null;

		int alternatePreferredIcIndicator = -1;
		byte encodeGetdataalternatePreferredIcIndicatorSeq = 0;
		byte encodeGetdataalternatePreferredIcIndicatorSeqLen = 0;
		byte encodeGetdataalternatePreferredIcIndicatorElementIdTag = 0;
		byte encodeGetdataalternatePreferredIcIndicatorElementIdLen = 0;
		byte[] alternatePreferredIcIndicatorEncoded = null;
		byte encodedalternatePreferredIcIndicatorTag1 = 0;
		byte encodedalternatePreferredIcIndicatorTag2 = 0;
		byte encodeGetdataalternatePreferredIcIndicatorElementDataTag = 0;
		byte encodeGetdataalternatePreferredIcIndicatorElementDataLen = 0;
		int alternatePreferredIcIndicatorErr = 0;
		byte encodeGetdataalternatePreferredIcIndicatorElementErrorTag = 0;
		byte encodeGetdataalternatePreferredIcIndicatorElementErrorLen = 0;
		byte[] alternatePreferredIcIndicatorErrorEncoded = null;

		int preferredIncIndicator = -1;
		byte encodeGetdatapreferredIncIndicatorSeq = 0;
		byte encodeGetdatapreferredIncIndicatorSeqLen = 0;
		byte encodeGetdatapreferredIncIndicatorElementIdTag = 0;
		byte encodeGetdatapreferredIncIndicatorElementIdLen = 0;
		byte[] preferredIncIndicatorEncoded = null;
		byte encodedpreferredIncIndicatorTag1 = 0;
		byte encodedpreferredIncIndicatorTag2 = 0;
		byte encodeGetdatapreferredIncIndicatorElementDataTag = 0;
		byte encodeGetdatapreferredIncIndicatorElementDataLen = 0;
		int preferredIncIndicatorErr = 0;
		byte encodeGetdatapreferredIncIndicatorElementErrorTag = 0;
		byte encodeGetdatapreferredIncIndicatorElementErrorLen = 0;
		byte[] preferredIncIndicatorErrorEncoded = null;

		//Checking if ic indiator collective element is requested, in this case LIDB will return all three indicators
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR) != null &&
				leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR) != null &&
				leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR) != null) {
			int icIndicatorsPPIC = (Integer) leg2Data
					.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR);
			int icIndicatorsAPIC = (Integer) leg2Data
					.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR);
			int icIndicatorsPINC = (Integer) leg2Data
					.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR);
			int icIndicatorAPIC_PPIC = (icIndicatorsAPIC & 0x07) << 3;
			icIndicatorAPIC_PPIC |= (icIndicatorsPPIC & 0x07);

			byte[] byteIcIndicator = { (byte) icIndicatorAPIC_PPIC, (byte) icIndicatorsPINC };
			icIndicatorsEncoded = byteIcIndicator;

			if(logger.isDebugEnabled()){
				logger.debug("icIndicators collective element is present" );
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_IC_INDICATORS_ERR) != null) {
			icIndicatorsErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_IC_INDICATORS_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("icIndicators collective element Error is present, value:: "+icIndicatorsErr);
			}
		}

		if (icIndicatorsEncoded != null  || icIndicatorsErr != 0) {
			encodeGetdataicIndicatorsSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataicIndicatorsSeqLen = GetDataConstants.GETDATA_IC_INDICATORS_SEQ_LEN;
			encodeGetdataicIndicatorsElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataicIndicatorsElementIdLen = GetDataConstants.GETDATA_IC_INDICATORS_TAG_LEN;
			encodedicIndicatorsTag = GetDataConstants.GETDATA_IC_INDICATORS_TAG;

			if (icIndicatorsEncoded != null) {
				encodeGetdataicIndicatorsElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_OCTET;
				encodeGetdataicIndicatorsElementDataLen = GetDataConstants.GETDATA_IC_INDICATORS_LENGTH;

				totalLength += GetDataConstants.GETDATA_IC_INDICATORS_LEN_OCTET;
			}
			else if (icIndicatorsErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataicIndicatorsElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (icIndicatorsErr != 6 ) {
					encodeGetdataicIndicatorsElementErrorLen = 0x02;
					encodeGetdataicIndicatorsSeqLen = 0x07;
					//LIDBElementError VALUE
					icIndicatorsErrorEncoded = CommonUtils.formatIntToByte(icIndicatorsErr);
					if(logger.isDebugEnabled()){
						logger.debug("icIndicators Error::"+icIndicatorsErr+" encoded as::"+icIndicatorsErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_IC_INDICATORS_ERR_LEN + 2;

				}
				else {
					encodeGetdataicIndicatorsElementErrorLen = 0x01;
					encodeGetdataicIndicatorsSeqLen = 0x06;
					icIndicatorsErrorEncoded = CommonUtils.formatIntToByte(icIndicatorsErr);
					if(logger.isDebugEnabled()){
						logger.debug("icIndicators Error::"+icIndicatorsErr+" encoded as::"+icIndicatorsErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_IC_INDICATORS_ERR_LEN + 1;
				}				
			}
		}

		//Checking if individual ic indicator elements are present
		else {
			//primaryPreferredIcIndicator
			if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR) != null) {
				primaryPreferredIcIndicator = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR);
				if(logger.isDebugEnabled()){
					logger.debug("primaryPreferredIcIndicator element is present, value:: "+primaryPreferredIcIndicator);
				}
			}
			else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR_ERR) != null) {
				primaryPreferredIcIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR_ERR);
				if(logger.isDebugEnabled()){
					logger.debug("primaryPreferredIcIndicator element Error is present, value:: "+primaryPreferredIcIndicatorErr);
				}
			}
			if (primaryPreferredIcIndicator != -1  || primaryPreferredIcIndicatorErr != 0) {

				encodeGetdataprimaryPreferredIcIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
				encodeGetdataprimaryPreferredIcIndicatorSeqLen = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_SEQ_LEN;
				encodeGetdataprimaryPreferredIcIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
				encodeGetdataprimaryPreferredIcIndicatorElementIdLen = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_TAG_LEN;
				encodedprimaryPreferredIcIndicatorTag1 = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_TAG1;
				encodedprimaryPreferredIcIndicatorTag2 = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_TAG2;
				if ( primaryPreferredIcIndicator != -1) {
					encodeGetdataprimaryPreferredIcIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
					encodeGetdataprimaryPreferredIcIndicatorElementDataLen = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_LENGTH;
					primaryPreferredIcIndicatorEncoded = CommonUtils.formatIntToByte(primaryPreferredIcIndicator);
					logger.debug("primaryPreferredIcIndicator::"+primaryPreferredIcIndicator+" encoded as::"+primaryPreferredIcIndicatorEncoded);
					totalLength += GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_LEN_OCTET;
				}
				else if (primaryPreferredIcIndicatorErr != 0 ) {
					//LIDBElementError TAG 
					encodeGetdataprimaryPreferredIcIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
					//LIDBElementError LENGTH (length of Error)
					if (primaryPreferredIcIndicatorErr != 6 ) {
						encodeGetdataprimaryPreferredIcIndicatorElementErrorLen = 0x02;
						encodeGetdataprimaryPreferredIcIndicatorSeqLen = 0x08;
						//LIDBElementError VALUE
						primaryPreferredIcIndicatorErrorEncoded = CommonUtils.formatIntToByte(primaryPreferredIcIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("primaryPreferredIcIndicator Error::"+primaryPreferredIcIndicatorErr+" encoded as::"+primaryPreferredIcIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_ERR_LEN + 2;

					}
					else {
						encodeGetdataprimaryPreferredIcIndicatorElementErrorLen = 0x01;
						encodeGetdataprimaryPreferredIcIndicatorSeqLen = 0x07;
						primaryPreferredIcIndicatorErrorEncoded = CommonUtils.formatIntToByte(primaryPreferredIcIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("primaryPreferredIcIndicator Error::"+primaryPreferredIcIndicatorErr+" encoded as::"+primaryPreferredIcIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR_ERR_LEN + 1;
					}				
				}
			}

			//alternatePreferredIcIndicator
			if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR) != null) {
				alternatePreferredIcIndicator = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR);
				if(logger.isDebugEnabled()){
					logger.debug("alternatePreferredIcIndicator element is present, value:: "+alternatePreferredIcIndicator);
				}
			}
			else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR_ERR) != null) {
				alternatePreferredIcIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR_ERR);
				if(logger.isDebugEnabled()){
					logger.debug("alternatePreferredIcIndicator element Error is present, value:: "+alternatePreferredIcIndicatorErr);
				}
			}
			if (alternatePreferredIcIndicator != -1  || alternatePreferredIcIndicatorErr != 0) {

				encodeGetdataalternatePreferredIcIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
				encodeGetdataalternatePreferredIcIndicatorSeqLen = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_SEQ_LEN;
				encodeGetdataalternatePreferredIcIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
				encodeGetdataalternatePreferredIcIndicatorElementIdLen = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_TAG_LEN;
				encodedalternatePreferredIcIndicatorTag1 = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_TAG1;
				encodedalternatePreferredIcIndicatorTag2 = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_TAG2;
				if ( alternatePreferredIcIndicator != -1) {
					encodeGetdataalternatePreferredIcIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
					encodeGetdataalternatePreferredIcIndicatorElementDataLen = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_LENGTH;
					alternatePreferredIcIndicatorEncoded = CommonUtils.formatIntToByte(alternatePreferredIcIndicator);
					logger.debug("alternatePreferredIcIndicator::"+alternatePreferredIcIndicator+" encoded as::"+alternatePreferredIcIndicatorEncoded);
					totalLength += GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_LEN_OCTET;
				}
				else if (alternatePreferredIcIndicatorErr != 0 ) {
					//LIDBElementError TAG 
					encodeGetdataalternatePreferredIcIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
					//LIDBElementError LENGTH (length of Error)
					if (alternatePreferredIcIndicatorErr != 6 ) {
						encodeGetdataalternatePreferredIcIndicatorElementErrorLen = 0x02;
						encodeGetdataalternatePreferredIcIndicatorSeqLen = 0x08;
						//LIDBElementError VALUE
						alternatePreferredIcIndicatorErrorEncoded = CommonUtils.formatIntToByte(alternatePreferredIcIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("alternatePreferredIcIndicator Error::"+alternatePreferredIcIndicatorErr+" encoded as::"+alternatePreferredIcIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_ERR_LEN + 2;

					}
					else {
						encodeGetdataalternatePreferredIcIndicatorElementErrorLen = 0x01;
						encodeGetdataalternatePreferredIcIndicatorSeqLen = 0x07;
						alternatePreferredIcIndicatorErrorEncoded = CommonUtils.formatIntToByte(alternatePreferredIcIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("alternatePreferredIcIndicator Error::"+alternatePreferredIcIndicatorErr+" encoded as::"+alternatePreferredIcIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR_ERR_LEN + 1;
					}				
				}
			}

			//preferredIncIndicator
			if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR) != null) {
				preferredIncIndicator = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR);
				if(logger.isDebugEnabled()){
					logger.debug("preferredIncIndicator element is present, value:: "+preferredIncIndicator);
				}
			}
			else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR_ERR) != null) {
				preferredIncIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR_ERR);
				if(logger.isDebugEnabled()){
					logger.debug("preferredIncIndicator element Error is present, value:: "+preferredIncIndicatorErr);
				}
			}
			if (preferredIncIndicator != -1  || preferredIncIndicatorErr != 0) {

				encodeGetdatapreferredIncIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
				encodeGetdatapreferredIncIndicatorSeqLen = GetDataConstants.GETDATA_PREFERRED_INC_INDICATOR_SEQ_LEN;
				encodeGetdatapreferredIncIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
				encodeGetdatapreferredIncIndicatorElementIdLen = GetDataConstants.GETDATA_PREFERRED_INC_INDICATOR_TAG_LEN;
				encodedpreferredIncIndicatorTag1 = GetDataConstants.GETDATA_PREFERRED_INC_INDICATOR_TAG1;
				encodedpreferredIncIndicatorTag2 = GetDataConstants.GETDATA_PREFERRED_INC_INDICATOR_TAG2;
				if ( preferredIncIndicator != -1) {
					encodeGetdatapreferredIncIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
					encodeGetdatapreferredIncIndicatorElementDataLen = GetDataConstants.GETDATA_PREFERRED_INC_INDICATOR_LENGTH;
					preferredIncIndicatorEncoded = CommonUtils.formatIntToByte(preferredIncIndicator);
					logger.debug("preferredIncIndicator::"+preferredIncIndicator+" encoded as::"+preferredIncIndicatorEncoded);
					totalLength += GetDataConstants.GETDATA_PREFERRED_INC_INDICATOR_LEN_OCTET;
				}
				else if (preferredIncIndicatorErr != 0 ) {
					//LIDBElementError TAG 
					encodeGetdatapreferredIncIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
					//LIDBElementError LENGTH (length of Error)
					if (preferredIncIndicatorErr != 6 ) {
						encodeGetdatapreferredIncIndicatorElementErrorLen = 0x02;
						encodeGetdatapreferredIncIndicatorSeqLen = 0x08;
						//LIDBElementError VALUE
						preferredIncIndicatorErrorEncoded = CommonUtils.formatIntToByte(preferredIncIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("preferredIncIndicator Error::"+preferredIncIndicatorErr+" encoded as::"+preferredIncIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_PREFERRED_INC_INDICATOR_ERR_LEN + 2;

					}
					else {
						encodeGetdatapreferredIncIndicatorElementErrorLen = 0x01;
						encodeGetdatapreferredIncIndicatorSeqLen = 0x07;
						preferredIncIndicatorErrorEncoded = CommonUtils.formatIntToByte(preferredIncIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("preferredIncIndicator Error::"+preferredIncIndicatorErr+" encoded as::"+preferredIncIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_PREFERRED_INC_INDICATOR_ERR_LEN + 1;
					}				
				}
			}
		}

		//ilpCicIndicator Element 
		String ilpCicIndicator = null;
		byte encodeGetdatailpCicIndicatorSeq = 0;
		byte encodeGetdatailpCicIndicatorElementIdTag = 0 ;
		byte encodeGetdatailpCicIndicatorElementIdLen = 0 ;
		byte[] ilpCicIndicatorEncoded = null;
		byte encodedilpCicIndicatorTag = 0;
		byte encodeGetdatailpCicIndicatorElementDataTag = 0 ;
		byte encodeGetdatailpCicIndicatorElementDataLen = 0 ;
		byte encodeGetdatailpCicIndicatorSeqLen = 0;
		int ilpCicIndicatorErr = 0;
		byte encodeGetdatailpCicIndicatorElementErrorTag = 0;
		byte encodeGetdatailpCicIndicatorElementErrorLen = 0;
		byte[] ilpCicIndicatorErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_INDICATOR) != null) {
			ilpCicIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("ilpCicIndicator element is present, value:: "+ilpCicIndicator);
			}

			if(!StringUtils.isNotBlank(ilpCicIndicator)){
				ilpCicIndicator = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_INDICATOR_ERR) != null) {
			ilpCicIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("ilpCicIndicator element Error is present, value:: "+ilpCicIndicatorErr);
			}
		}

		if ( (ilpCicIndicator != null && !ilpCicIndicator.isEmpty()) || ilpCicIndicatorErr != 0) {
			//SEQUENCE of ilpCicIndicator
			encodeGetdatailpCicIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof ilpCicIndicator
			encodeGetdatailpCicIndicatorSeqLen = GetDataConstants.GETDATA_ILP_CIC_INDICATOR_SEQ_LEN;

			//ilpCicIndicator LIDBElementIdentifier TAG
			encodeGetdatailpCicIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//ilpCicIndicator LIDBElementIdentifier LENGTH
			encodeGetdatailpCicIndicatorElementIdLen = GetDataConstants.GETDATA_ILP_CIC_INDICATOR_TAG_LEN;
			//ilpCicIndicator LIDBElementIdentifier VALUE. i.e ilpCicIndicator identifier
			encodedilpCicIndicatorTag = GetDataConstants.GETDATA_ILP_CIC_INDICATOR_TAG;
			if ( ilpCicIndicator != null && !ilpCicIndicator.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdatailpCicIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				//LIDBElementData LENGTH (length of ilpCicIndicator)
				encodeGetdatailpCicIndicatorElementDataLen = GetDataConstants.GETDATA_ILP_CIC_INDICATOR_LENGTH;
				//LIDBElementData VALUE . i.e ilpCicIndicator value
				//ilpCicIndicatorEncoded = asciToHex(ilpCicIndicator);
				int ilpCicIndInt = Integer.parseInt(ilpCicIndicator);
				ilpCicIndicatorEncoded = CommonUtils.formatIntToByte(ilpCicIndInt);
				logger.debug("ilpCicIndicator::"+ilpCicIndicator+" encoded as::"+ilpCicIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ILP_CIC_INDICATOR_LEN_OCTET;
			}
			else if (ilpCicIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatailpCicIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (ilpCicIndicatorErr != 6 ) {
					encodeGetdatailpCicIndicatorElementErrorLen = 0x02;
					encodeGetdatailpCicIndicatorSeqLen = 0x07;
					//LIDBElementError VALUE
					ilpCicIndicatorErrorEncoded = CommonUtils.formatIntToByte(ilpCicIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("ilpCicIndicator Error::"+ilpCicIndicatorErr+" encoded as::"+ilpCicIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ILP_CIC_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdatailpCicIndicatorElementErrorLen = 0x01;
					encodeGetdatailpCicIndicatorSeqLen = 0x06;
					ilpCicIndicatorErrorEncoded = CommonUtils.formatIntToByte(ilpCicIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("ilpCicIndicator Error::"+ilpCicIndicatorErr+" encoded as::"+ilpCicIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ILP_CIC_INDICATOR_ERR_LEN + 1;
				}				
			}
		}


		//ilpCic Element 
		String ilpCic = null;
		byte encodeGetdatailpCicSeq  = 0;
		byte encodeGetdatailpCicSeqLen  = 0;
		byte encodeGetdatailpCicElementIdTag  = 0;
		byte encodeGetdatailpCicElementIdLen  = 0;
		byte[] ilpCicEncoded  = null;
		byte encodedilpCicTag1  = 0;
		byte encodedilpCicTag2  = 0;
		byte encodeGetdatailpCicElementDataTag1  = 0;
		byte encodeGetdatailpCicElementDataTag2  = 0;
		byte encodeGetdatailpCicElementDataLen  = 0;
		int ilpCicErr = 0;
		byte encodeGetdatailpCicElementErrorTag = 0;
		byte encodeGetdatailpCicElementErrorLen = 0;
		byte[] ilpCicErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC) != null) {
			ilpCic = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC);
			if(logger.isDebugEnabled()){
				logger.debug("ilpCic element is present, value:: "+ilpCic);
			}

			if(!StringUtils.isNotBlank(ilpCic)){
				ilpCic = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_ERR) != null) {
			ilpCicErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("ilpCic element Error is present, value:: "+ilpCicErr);
			}
		}

		if ( (ilpCic != null && !ilpCic.isEmpty()) || ilpCicErr != 0) {
			//SEQUENCE of ilpCic
			encodeGetdatailpCicSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof ilpCic
			encodeGetdatailpCicSeqLen = GetDataConstants.GETDATA_ILP_CIC_SEQ_LEN;

			//ilpCic LIDBElementIdentifier TAG
			encodeGetdatailpCicElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//ilpCic LIDBElementIdentifier LENGTH
			encodeGetdatailpCicElementIdLen = GetDataConstants.GETDATA_ILP_CIC_TAG_LEN;
			encodedilpCicTag1 = 0;
			encodedilpCicTag2 = 0;
			//ilpCic LIDBElementIdentifier VALUE. i.e ilpCic identifier
			encodedilpCicTag1 = GetDataConstants.GETDATA_ILP_CIC_TAG1;
			encodedilpCicTag2 = GetDataConstants.GETDATA_ILP_CIC_TAG2;
			if ( ilpCic != null && !ilpCic.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdatailpCicElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_1;
				encodeGetdatailpCicElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_2;
				//LIDBElementData LENGTH (length of ilpCic)
				encodeGetdatailpCicElementDataLen = GetDataConstants.GETDATA_LIDB_ELEMENT_DATA_BCD_DIG;
				//LIDBElementData VALUE . i.e Account Owner value
				ilpCicEncoded = encodeAdrsSignalForGetQuery(ilpCic);
				logger.debug("ilpCic::"+ilpCic+" encoded as::"+ilpCicEncoded);
				totalLength += GetDataConstants.GETDATA_ILP_CIC_LEN_OCTET;
			}
			else if (ilpCicErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatailpCicElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (ilpCicErr != 6 ) {
					encodeGetdatailpCicElementErrorLen = 0x02;
					encodeGetdatailpCicSeqLen = 0x08;
					//LIDBElementError VALUE
					ilpCicErrorEncoded = CommonUtils.formatIntToByte(ilpCicErr);
					if(logger.isDebugEnabled()){
						logger.debug("ilpCic Error::"+ilpCicErr+" encoded as::"+ilpCicErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ILP_CIC_ERR_LEN + 2;

				}
				else {
					encodeGetdatailpCicElementErrorLen = 0x01;
					encodeGetdatailpCicSeqLen = 0x07;
					ilpCicErrorEncoded = CommonUtils.formatIntToByte(ilpCicErr);
					if(logger.isDebugEnabled()){
						logger.debug("ilpCic Error::"+ilpCicErr+" encoded as::"+ilpCicErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ILP_CIC_ERR_LEN + 1;
				}				
			}
		}


		//interceptIndicator Element 
		String interceptIndicator = null;
		byte encodeGetdatainterceptIndicatorSeq = 0;
		byte encodeGetdatainterceptIndicatorElementIdTag = 0 ;
		byte encodeGetdatainterceptIndicatorElementIdLen = 0 ;
		byte[] interceptIndicatorEncoded = null;
		byte encodedinterceptIndicatorTag = 0;
		byte encodeGetdatainterceptIndicatorElementDataTag = 0 ;
		byte encodeGetdatainterceptIndicatorElementDataLen = 0 ;
		byte encodeGetdatainterceptIndicatorSeqLen = 0;
		int interceptIndicatorErr = 0;
		byte encodeGetdatainterceptIndicatorElementErrorTag = 0;
		byte encodeGetdatainterceptIndicatorElementErrorLen = 0;
		byte[] interceptIndicatorErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_INTERCEPT_INDICATOR) != null) {
			interceptIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_INTERCEPT_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("interceptIndicator element is present, value:: "+interceptIndicator);
			}

			if(!StringUtils.isNotBlank(interceptIndicator)){
				interceptIndicator = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_INTERCEPT_INDICATOR_ERR) != null) {
			interceptIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_INTERCEPT_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("interceptIndicator element Error is present, value:: "+interceptIndicatorErr);
			}
		}

		if ( (interceptIndicator != null && !interceptIndicator.isEmpty()) || interceptIndicatorErr != 0) {
			//SEQUENCE of interceptIndicator
			encodeGetdatainterceptIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof interceptIndicator
			encodeGetdatainterceptIndicatorSeqLen = GetDataConstants.GETDATA_INTERCEPT_INDICATOR_SEQ_LEN;

			//interceptIndicator LIDBElementIdentifier TAG
			encodeGetdatainterceptIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//interceptIndicator LIDBElementIdentifier LENGTH
			encodeGetdatainterceptIndicatorElementIdLen = GetDataConstants.GETDATA_INTERCEPT_INDICATOR_TAG_LEN;
			//interceptIndicator LIDBElementIdentifier VALUE. i.e interceptIndicator identifier
			encodedinterceptIndicatorTag = GetDataConstants.GETDATA_INTERCEPT_INDICATOR_TAG;
			if ( interceptIndicator != null && !interceptIndicator.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdatainterceptIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				//LIDBElementData LENGTH (length of interceptIndicator)
				encodeGetdatainterceptIndicatorElementDataLen = GetDataConstants.GETDATA_INTERCEPT_INDICATOR_LENGTH;
				//LIDBElementData VALUE . i.e interceptIndicator value
				//interceptIndicatorEncoded = asciToHex(interceptIndicator);
				int interceptIndInt = Integer.parseInt(interceptIndicator);
				interceptIndicatorEncoded = CommonUtils.formatIntToByte(interceptIndInt);
				logger.debug("interceptIndicator::"+interceptIndicator+" encoded as::"+interceptIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_INTERCEPT_INDICATOR_LEN_OCTET;
			}
			else if (interceptIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatainterceptIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (interceptIndicatorErr != 6 ) {
					encodeGetdatainterceptIndicatorElementErrorLen = 0x02;
					encodeGetdatainterceptIndicatorSeqLen = 0x07;
					//LIDBElementError VALUE
					interceptIndicatorErrorEncoded = CommonUtils.formatIntToByte(interceptIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("interceptIndicator Error::"+interceptIndicatorErr+" encoded as::"+interceptIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_INTERCEPT_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdatainterceptIndicatorElementErrorLen = 0x01;
					encodeGetdatainterceptIndicatorSeqLen = 0x06;
					interceptIndicatorErrorEncoded = CommonUtils.formatIntToByte(interceptIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("interceptIndicator Error::"+interceptIndicatorErr+" encoded as::"+interceptIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_INTERCEPT_INDICATOR_ERR_LEN + 1;
				}				
			}
		}



		// origBillingServiceIndicators element
		String origBillingServiceIndicators = null;
		byte encodeGetdataorigBillingServiceIndicatorsSeq = 0;
		byte encodeGetdataorigBillingServiceIndicatorsSeqLen = 0;
		byte encodeGetdataorigBillingServiceIndicatorsElementIdTag = 0;
		byte encodeGetdataorigBillingServiceIndicatorsElementIdLen = 0;
		byte[] OrigBillingServiceIndicatorsEncoded = null;
		byte encodedorigBillingServiceIndicatorsTag = 0;
		byte encodeGetdataorigBillingServiceIndicatorsElementDataTag = 0;
		byte encodeGetdataorigBillingServiceIndicatorsElementDataLen = 0;
		int origBillingServiceIndicatorsErr = 0;
		byte encodeGetdataorigBillingServiceIndicatorsElementErrorTag = 0;
		byte encodeGetdataorigBillingServiceIndicatorsElementErrorLen = 0;
		byte[] origBillingServiceIndicatorsErrorEncoded = null;

		int origCollectBillingIndicator = -1;
		byte encodeGetdataorigCollectBillingIndicatorSeq = 0;
		byte encodeGetdataorigCollectBillingIndicatorSeqLen = 0;
		byte encodeGetdataorigCollectBillingIndicatorElementIdTag = 0;
		byte encodeGetdataorigCollectBillingIndicatorElementIdLen = 0;
		byte[] origCollectBillingIndicatorEncoded = null;
		byte encodedorigCollectBillingIndicatorTag1 = 0;
		byte encodedorigCollectBillingIndicatorTag2 = 0;
		byte encodeGetdataorigCollectBillingIndicatorElementDataTag = 0;
		byte encodeGetdataorigCollectBillingIndicatorElementDataLen = 0;
		int origCollectBillingIndicatorErr = 0;
		byte encodeGetdataorigCollectBillingIndicatorElementErrorTag = 0;
		byte encodeGetdataorigCollectBillingIndicatorElementErrorLen = 0;
		byte[] origCollectBillingIndicatorErrorEncoded = null;

		int origThirdNumberBillingIndicator = -1;
		byte encodeGetdataorigThirdNumberBillingIndicatorSeq = 0;
		byte encodeGetdataorigThirdNumberBillingIndicatorSeqLen = 0;
		byte encodeGetdataorigThirdNumberBillingIndicatorElementIdTag = 0;
		byte encodeGetdataorigThirdNumberBillingIndicatorElementIdLen = 0;
		byte[] origThirdNumberBillingIndicatorEncoded = null;
		byte encodedorigThirdNumberBillingIndicatorTag1 = 0;
		byte encodedorigThirdNumberBillingIndicatorTag2 = 0;
		byte encodeGetdataorigThirdNumberBillingIndicatorElementDataTag = 0;
		byte encodeGetdataorigThirdNumberBillingIndicatorElementDataLen = 0;
		int origThirdNumberBillingIndicatorErr = 0;
		byte encodeGetdataorigThirdNumberBillingIndicatorElementErrorTag = 0;
		byte encodeGetdataorigThirdNumberBillingIndicatorElementErrorLen = 0;
		byte[] origThirdNumberBillingIndicatorErrorEncoded = null;

		int origLocalNontollCallIndicator = -1;
		byte encodeGetdataorigLocalNontollCallIndicatorSeq = 0;
		byte encodeGetdataorigLocalNontollCallIndicatorSeqLen = 0;
		byte encodeGetdataorigLocalNontollCallIndicatorElementIdTag = 0;
		byte encodeGetdataorigLocalNontollCallIndicatorElementIdLen = 0;
		byte[] origLocalNontollCallIndicatorEncoded = null;
		byte encodedorigLocalNontollCallIndicatorTag1 = 0;
		byte encodedorigLocalNontollCallIndicatorTag2 = 0;
		byte encodeGetdataorigLocalNontollCallIndicatorElementDataTag = 0;
		byte encodeGetdataorigLocalNontollCallIndicatorElementDataLen = 0;
		int origLocalNontollCallIndicatorErr = 0;
		byte encodeGetdataorigLocalNontollCallIndicatorElementErrorTag = 0;
		byte encodeGetdataorigLocalNontollCallIndicatorElementErrorLen = 0;
		byte[] origLocalNontollCallIndicatorErrorEncoded = null;

		int origCreditCardIndicator = -1;
		byte encodeGetdataorigCreditCardIndicatorSeq = 0;
		byte encodeGetdataorigCreditCardIndicatorSeqLen = 0;
		byte encodeGetdataorigCreditCardIndicatorElementIdTag = 0;
		byte encodeGetdataorigCreditCardIndicatorElementIdLen = 0;
		byte[] origCreditCardIndicatorEncoded = null;
		byte encodedorigCreditCardIndicatorTag1 = 0;
		byte encodedorigCreditCardIndicatorTag2 = 0;
		byte encodeGetdataorigCreditCardIndicatorElementDataTag = 0;
		byte encodeGetdataorigCreditCardIndicatorElementDataLen = 0;
		int origCreditCardIndicatorErr = 0;
		byte encodeGetdataorigCreditCardIndicatorElementErrorTag = 0;
		byte encodeGetdataorigCreditCardIndicatorElementErrorLen = 0;
		byte[] origCreditCardIndicatorErrorEncoded = null;

		int origFreeDaIndicator = -1;
		byte encodeGetdataorigFreeDaIndicatorSeq = 0;
		byte encodeGetdataorigFreeDaIndicatorSeqLen = 0;
		byte encodeGetdataorigFreeDaIndicatorElementIdTag = 0;
		byte encodeGetdataorigFreeDaIndicatorElementIdLen = 0;
		byte[] origFreeDaIndicatorEncoded = null;
		byte encodedorigFreeDaIndicatorTag1 = 0;
		byte encodedorigFreeDaIndicatorTag2 = 0;
		byte encodeGetdataorigFreeDaIndicatorElementDataTag = 0;
		byte encodeGetdataorigFreeDaIndicatorElementDataLen = 0;
		int origFreeDaIndicatorErr = 0;
		byte encodeGetdataorigFreeDaIndicatorElementErrorTag = 0;
		byte encodeGetdataorigFreeDaIndicatorElementErrorLen = 0;
		byte[] origFreeDaIndicatorErrorEncoded = null;

		int origSpecialBnsIndicator = -1;
		byte encodeGetdataorigSpecialBnsIndicatorSeq = 0;
		byte encodeGetdataorigSpecialBnsIndicatorSeqLen = 0;
		byte encodeGetdataorigSpecialBnsIndicatorElementIdTag = 0;
		byte encodeGetdataorigSpecialBnsIndicatorElementIdLen = 0;
		byte[] origSpecialBnsIndicatorEncoded = null;
		byte encodedorigSpecialBnsIndicatorTag1 = 0;
		byte encodedorigSpecialBnsIndicatorTag2 = 0;
		byte encodeGetdataorigSpecialBnsIndicatorElementDataTag = 0;
		byte encodeGetdataorigSpecialBnsIndicatorElementDataLen = 0;
		int origSpecialBnsIndicatorErr = 0;
		byte encodeGetdataorigSpecialBnsIndicatorElementErrorTag = 0;
		byte encodeGetdataorigSpecialBnsIndicatorElementErrorLen = 0;
		byte[] origSpecialBnsIndicatorErrorEncoded = null;

		int origSentpaidIndicator = -1;
		byte encodeGetdataorigSentpaidIndicatorSeq = 0;
		byte encodeGetdataorigSentpaidIndicatorSeqLen = 0;
		byte encodeGetdataorigSentpaidIndicatorElementIdTag = 0;
		byte encodeGetdataorigSentpaidIndicatorElementIdLen = 0;
		byte[] origSentpaidIndicatorEncoded = null;
		byte encodedorigSentpaidIndicatorTag1 = 0;
		byte encodedorigSentpaidIndicatorTag2 = 0;
		byte encodeGetdataorigSentpaidIndicatorElementDataTag = 0;
		byte encodeGetdataorigSentpaidIndicatorElementDataLen = 0;
		int origSentpaidIndicatorErr = 0;
		byte encodeGetdataorigSentpaidIndicatorElementErrorTag = 0;
		byte encodeGetdataorigSentpaidIndicatorElementErrorLen = 0;
		byte[] origSentpaidIndicatorErrorEncoded = null;

		int origDaccIndicator = -1;
		byte encodeGetdataorigDaccIndicatorSeq = 0;
		byte encodeGetdataorigDaccIndicatorSeqLen = 0;
		byte encodeGetdataorigDaccIndicatorElementIdTag = 0;
		byte encodeGetdataorigDaccIndicatorElementIdLen = 0;
		byte[] origDaccIndicatorEncoded = null;
		byte encodedorigDaccIndicatorTag1 = 0;
		byte encodedorigDaccIndicatorTag2 = 0;
		byte encodeGetdataorigDaccIndicatorElementDataTag = 0;
		byte encodeGetdataorigDaccIndicatorElementDataLen = 0;
		int origDaccIndicatorErr = 0;
		byte encodeGetdataorigDaccIndicatorElementErrorTag = 0;
		byte encodeGetdataorigDaccIndicatorElementErrorLen = 0;
		byte[] origDaccIndicatorErrorEncoded = null;

		int origBillingServiceSpareIndicator = -1;
		byte encodeGetdataorigBillingServiceSpareIndicatorSeq = 0;
		byte encodeGetdataorigBillingServiceSpareIndicatorSeqLen = 0;
		byte encodeGetdataorigBillingServiceSpareIndicatorElementIdTag = 0;
		byte encodeGetdataorigBillingServiceSpareIndicatorElementIdLen = 0;
		byte[] origBillingServiceSpareIndicatorEncoded = null;
		byte encodedorigBillingServiceSpareIndicatorTag1 = 0;
		byte encodedorigBillingServiceSpareIndicatorTag2 = 0;
		byte encodeGetdataorigBillingServiceSpareIndicatorElementDataTag = 0;
		byte encodeGetdataorigBillingServiceSpareIndicatorElementDataLen = 0;
		int origBillingServiceSpareIndicatorErr = 0;
		byte encodeGetdataorigBillingServiceSpareIndicatorElementErrorTag = 0;
		byte encodeGetdataorigBillingServiceSpareIndicatorElementErrorLen = 0;
		byte[] origBillingServiceSpareIndicatorErrorEncoded = null;

		//if origBillingServiceIndicators collective element is requested, LIDB will send origBillingServiceIndicators collective element
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_INDICATORS) != null) {
			origBillingServiceIndicators = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_INDICATORS);

			if(logger.isDebugEnabled()){
				logger.debug("origBillingServiceIndicators element is present, values:: " + origBillingServiceIndicators);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_INDICATORS_ERR) != null) {
			origBillingServiceIndicatorsErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_INDICATORS_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origBillingServiceIndicators element Error is present, value:: "+origBillingServiceIndicatorsErr);
			}
		}

		if (origBillingServiceIndicators != null  || origBillingServiceIndicatorsErr != 0) {
			encodeGetdataorigBillingServiceIndicatorsSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigBillingServiceIndicatorsSeqLen = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_INDICATORS_SEQ_LEN;
			encodeGetdataorigBillingServiceIndicatorsElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigBillingServiceIndicatorsElementIdLen = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_INDICATORS_TAG_LEN;
			encodedorigBillingServiceIndicatorsTag = 0;
			encodedorigBillingServiceIndicatorsTag = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_INDICATORS_TAG;

			if ( origBillingServiceIndicators != null) {
				origCollectBillingIndicator = Integer.parseInt(origBillingServiceIndicators.substring(0,1));
				origThirdNumberBillingIndicator = Integer.parseInt(origBillingServiceIndicators.substring(1,2));
				origLocalNontollCallIndicator = Integer.parseInt(origBillingServiceIndicators.substring(2,3));
				origCreditCardIndicator = Integer.parseInt(origBillingServiceIndicators.substring(3,5));
				origFreeDaIndicator = Integer.parseInt(origBillingServiceIndicators.substring(5,6));
				origSpecialBnsIndicator = Integer.parseInt(origBillingServiceIndicators.substring(6,7));
				origSentpaidIndicator = Integer.parseInt(origBillingServiceIndicators.substring(7,8));
				origDaccIndicator = Integer.parseInt(origBillingServiceIndicators.substring(8,9));
				origBillingServiceSpareIndicator = Integer.parseInt(origBillingServiceIndicators.substring(9,10));	

				int OrigBillingServiceIndicators1 = (origLocalNontollCallIndicator & 0x07) << 3;
				OrigBillingServiceIndicators1 |= (origThirdNumberBillingIndicator & 0x07);
				OrigBillingServiceIndicators1 = OrigBillingServiceIndicators1 << 3;
				OrigBillingServiceIndicators1 |= (origCollectBillingIndicator & 0x07);

				//2nd byte
				int OrigBillingServiceIndicators2 = (origSpecialBnsIndicator & 0x03) << 2;
				OrigBillingServiceIndicators2 |= (origFreeDaIndicator & 0x03);
				OrigBillingServiceIndicators2 = OrigBillingServiceIndicators2 << 4;
				OrigBillingServiceIndicators2 |= (origCreditCardIndicator & 0x0F);

				//3rd byte
				int OrigBillingServiceIndicators3 = (origBillingServiceSpareIndicator & 0x07) << 3;
				OrigBillingServiceIndicators3 |= (origDaccIndicator & 0x07);
				OrigBillingServiceIndicators3 = OrigBillingServiceIndicators3 << 3;
				OrigBillingServiceIndicators3 |= (origSentpaidIndicator & 0x07);					
				byte[] OrigBillServiceByteInd = {(byte) OrigBillingServiceIndicators1, (byte) OrigBillingServiceIndicators2, (byte) OrigBillingServiceIndicators3};
				OrigBillingServiceIndicatorsEncoded = OrigBillServiceByteInd;



				encodeGetdataorigBillingServiceIndicatorsElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_OCTET;
				encodeGetdataorigBillingServiceIndicatorsElementDataLen = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_INDICATORS_LENGTH;

				totalLength += GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_INDICATORS_LEN_OCTET;

			}
			else if (origBillingServiceIndicatorsErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigBillingServiceIndicatorsElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origBillingServiceIndicatorsErr != 6 ) {
					encodeGetdataorigBillingServiceIndicatorsElementErrorLen = 0x02;
					encodeGetdataorigBillingServiceIndicatorsSeqLen = 0x07;
					//LIDBElementError VALUE
					origBillingServiceIndicatorsErrorEncoded = CommonUtils.formatIntToByte(origBillingServiceIndicatorsErr);
					if(logger.isDebugEnabled()){
						logger.debug("origBillingServiceIndicators Error::"+origBillingServiceIndicatorsErr+" encoded as::"+origBillingServiceIndicatorsErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_INDICATORS_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigBillingServiceIndicatorsElementErrorLen = 0x01;
					encodeGetdataorigBillingServiceIndicatorsSeqLen = 0x06;
					origBillingServiceIndicatorsErrorEncoded = CommonUtils.formatIntToByte(origBillingServiceIndicatorsErr);
					if(logger.isDebugEnabled()){
						logger.debug("origBillingServiceIndicators Error::"+origBillingServiceIndicatorsErr+" encoded as::"+origBillingServiceIndicatorsErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_INDICATORS_ERR_LEN + 1;
				}				
			}
		}

		// The individual orig. billing indicators can also be requested separately 

		//origCollectBillingIndicator
		String origCollectBillingIndicatorStr = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_COLLECT_BILLING_INDICATOR) != null) {
			origCollectBillingIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_COLLECT_BILLING_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origCollectBillingIndicator element is present, value:: "+origCollectBillingIndicator);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_COLLECT_BILLING_INDICATOR_ERR) != null) {
			origCollectBillingIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_COLLECT_BILLING_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origCollectBillingIndicator element Error is present, value:: "+origCollectBillingIndicatorErr);
			}
		}
		if (origCollectBillingIndicatorStr != null  || origCollectBillingIndicatorErr != 0) {

			encodeGetdataorigCollectBillingIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigCollectBillingIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_COLLECT_BILLING_INDICATOR_SEQ_LEN;
			encodeGetdataorigCollectBillingIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigCollectBillingIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_COLLECT_BILLING_INDICATOR_TAG_LEN;
			encodedorigCollectBillingIndicatorTag1 = GetDataConstants.GETDATA_ORIG_COLLECT_BILLING_INDICATOR_TAG1;
			encodedorigCollectBillingIndicatorTag2 = GetDataConstants.GETDATA_ORIG_COLLECT_BILLING_INDICATOR_TAG2;
			if ( origCollectBillingIndicatorStr != null) {
				encodeGetdataorigCollectBillingIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigCollectBillingIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_COLLECT_BILLING_INDICATOR_LENGTH;
				origCollectBillingIndicatorEncoded = asciToHex(origCollectBillingIndicatorStr);
				logger.debug("origCollectBillingIndicator::"+origCollectBillingIndicatorStr+" encoded as::"+origCollectBillingIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_COLLECT_BILLING_INDICATOR_LEN_OCTET;
			}
			else if (origCollectBillingIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigCollectBillingIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origCollectBillingIndicatorErr != 6 ) {
					encodeGetdataorigCollectBillingIndicatorElementErrorLen = 0x02;
					encodeGetdataorigCollectBillingIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origCollectBillingIndicatorErrorEncoded = CommonUtils.formatIntToByte(origCollectBillingIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origCollectBillingIndicator Error::"+origCollectBillingIndicatorErr+" encoded as::"+origCollectBillingIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_COLLECT_BILLING_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigCollectBillingIndicatorElementErrorLen = 0x01;
					encodeGetdataorigCollectBillingIndicatorSeqLen = 0x07;
					origCollectBillingIndicatorErrorEncoded = CommonUtils.formatIntToByte(origCollectBillingIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origCollectBillingIndicator Error::"+origCollectBillingIndicatorErr+" encoded as::"+origCollectBillingIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_COLLECT_BILLING_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//origThirdNumberBillingIndicator
		String origThirdNumberBillingIndicatorStr = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_THIRD_NUMBER_BILLING_INDICATOR) != null) {
			origThirdNumberBillingIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_THIRD_NUMBER_BILLING_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origThirdNumberBillingIndicator element is present, value:: "+origThirdNumberBillingIndicator);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_THIRD_NUMBER_BILLING_INDICATOR_ERR) != null) {
			origThirdNumberBillingIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_THIRD_NUMBER_BILLING_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origThirdNumberBillingIndicator element Error is present, value:: "+origThirdNumberBillingIndicatorErr);
			}
		}
		if (origThirdNumberBillingIndicatorStr != null  || origThirdNumberBillingIndicatorErr != 0) {

			encodeGetdataorigThirdNumberBillingIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigThirdNumberBillingIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_SEQ_LEN;
			encodeGetdataorigThirdNumberBillingIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigThirdNumberBillingIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_TAG_LEN;
			encodedorigThirdNumberBillingIndicatorTag1 = GetDataConstants.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_TAG1;
			encodedorigThirdNumberBillingIndicatorTag2 = GetDataConstants.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_TAG2;
			if ( origThirdNumberBillingIndicatorStr != null) {
				encodeGetdataorigThirdNumberBillingIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigThirdNumberBillingIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_LENGTH;
				origThirdNumberBillingIndicatorEncoded = asciToHex(origThirdNumberBillingIndicatorStr);
				logger.debug("origThirdNumberBillingIndicator::"+origThirdNumberBillingIndicatorStr+" encoded as::"+origThirdNumberBillingIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_LEN_OCTET;
			}
			else if (origThirdNumberBillingIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigThirdNumberBillingIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origThirdNumberBillingIndicatorErr != 6 ) {
					encodeGetdataorigThirdNumberBillingIndicatorElementErrorLen = 0x02;
					encodeGetdataorigThirdNumberBillingIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origThirdNumberBillingIndicatorErrorEncoded = CommonUtils.formatIntToByte(origThirdNumberBillingIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origThirdNumberBillingIndicator Error::"+origThirdNumberBillingIndicatorErr+" encoded as::"+origThirdNumberBillingIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigThirdNumberBillingIndicatorElementErrorLen = 0x01;
					encodeGetdataorigThirdNumberBillingIndicatorSeqLen = 0x07;
					origThirdNumberBillingIndicatorErrorEncoded = CommonUtils.formatIntToByte(origThirdNumberBillingIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origThirdNumberBillingIndicator Error::"+origThirdNumberBillingIndicatorErr+" encoded as::"+origThirdNumberBillingIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//origLocalNontollCallIndicator
		String origLocalNontollCallIndicatorStr = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LOCAL_NONTOLL_CALL_INDICATOR) != null) {
			origLocalNontollCallIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LOCAL_NONTOLL_CALL_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origLocalNontollCallIndicator element is present, value:: "+origLocalNontollCallIndicatorStr);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_ERR) != null) {
			origLocalNontollCallIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origLocalNontollCallIndicator element Error is present, value:: "+origLocalNontollCallIndicatorErr);
			}
		}
		if (origLocalNontollCallIndicatorStr != null || origLocalNontollCallIndicatorErr != 0) {

			encodeGetdataorigLocalNontollCallIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigLocalNontollCallIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_SEQ_LEN;
			encodeGetdataorigLocalNontollCallIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigLocalNontollCallIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_TAG_LEN;
			encodedorigLocalNontollCallIndicatorTag1 = GetDataConstants.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_TAG1;
			encodedorigLocalNontollCallIndicatorTag2 = GetDataConstants.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_TAG2;
			if ( origLocalNontollCallIndicatorStr != null) {
				encodeGetdataorigLocalNontollCallIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigLocalNontollCallIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_LENGTH;
				origLocalNontollCallIndicatorEncoded = asciToHex(origLocalNontollCallIndicatorStr);
				logger.debug("origLocalNontollCallIndicator::"+origLocalNontollCallIndicatorStr+" encoded as::"+origLocalNontollCallIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_LEN_OCTET;
			}
			else if (origLocalNontollCallIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigLocalNontollCallIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origLocalNontollCallIndicatorErr != 6 ) {
					encodeGetdataorigLocalNontollCallIndicatorElementErrorLen = 0x02;
					encodeGetdataorigLocalNontollCallIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origLocalNontollCallIndicatorErrorEncoded = CommonUtils.formatIntToByte(origLocalNontollCallIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origLocalNontollCallIndicator Error::"+origLocalNontollCallIndicatorErr+" encoded as::"+origLocalNontollCallIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigLocalNontollCallIndicatorElementErrorLen = 0x01;
					encodeGetdataorigLocalNontollCallIndicatorSeqLen = 0x07;
					origLocalNontollCallIndicatorErrorEncoded = CommonUtils.formatIntToByte(origLocalNontollCallIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origLocalNontollCallIndicator Error::"+origLocalNontollCallIndicatorErr+" encoded as::"+origLocalNontollCallIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//origCreditCardIndicator
		String origCreditCardIndicatorStr =null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_CREDIT_CARD_INDICATOR) != null) {
			origCreditCardIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_CREDIT_CARD_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origCreditCardIndicator element is present, value:: "+origCreditCardIndicatorStr);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_CREDIT_CARD_INDICATOR_ERR) != null) {
			origCreditCardIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_CREDIT_CARD_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origCreditCardIndicator element Error is present, value:: "+origCreditCardIndicatorErr);
			}
		}
		if (origCreditCardIndicatorStr != null  || origCreditCardIndicatorErr != 0) {

			encodeGetdataorigCreditCardIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigCreditCardIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_CREDIT_CARD_INDICATOR_SEQ_LEN;
			encodeGetdataorigCreditCardIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigCreditCardIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_CREDIT_CARD_INDICATOR_TAG_LEN;
			encodedorigCreditCardIndicatorTag1 = GetDataConstants.GETDATA_ORIG_CREDIT_CARD_INDICATOR_TAG1;
			encodedorigCreditCardIndicatorTag2 = GetDataConstants.GETDATA_ORIG_CREDIT_CARD_INDICATOR_TAG2;
			if ( origCreditCardIndicatorStr != null) {
				encodeGetdataorigCreditCardIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigCreditCardIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_CREDIT_CARD_INDICATOR_LENGTH;
				int origCreditCardIndicatorInt = Integer.parseInt(origCreditCardIndicatorStr);
				origCreditCardIndicatorEncoded = CommonUtils.formatIntToByte(origCreditCardIndicatorInt);
				logger.debug("origCreditCardIndicator::"+origCreditCardIndicatorStr+" encoded as::"+origCreditCardIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_CREDIT_CARD_INDICATOR_LEN_OCTET;
			}
			else if (origCreditCardIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigCreditCardIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origCreditCardIndicatorErr != 6 ) {
					encodeGetdataorigCreditCardIndicatorElementErrorLen = 0x02;
					encodeGetdataorigCreditCardIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origCreditCardIndicatorErrorEncoded = CommonUtils.formatIntToByte(origCreditCardIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origCreditCardIndicator Error::"+origCreditCardIndicatorErr+" encoded as::"+origCreditCardIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_CREDIT_CARD_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigCreditCardIndicatorElementErrorLen = 0x01;
					encodeGetdataorigCreditCardIndicatorSeqLen = 0x07;
					origCreditCardIndicatorErrorEncoded = CommonUtils.formatIntToByte(origCreditCardIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origCreditCardIndicator Error::"+origCreditCardIndicatorErr+" encoded as::"+origCreditCardIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_CREDIT_CARD_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//origFreeDaIndicator
		String origFreeDaIndicatorStr = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_FREE_DA_INDICATOR) != null) {
			origFreeDaIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_FREE_DA_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origFreeDaIndicator element is present, value:: "+origFreeDaIndicatorStr);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_FREE_DA_INDICATOR_ERR) != null) {
			origFreeDaIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_FREE_DA_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origFreeDaIndicator element Error is present, value:: "+origFreeDaIndicatorErr);
			}
		}
		if (origFreeDaIndicatorStr != null  || origFreeDaIndicatorErr != 0) {

			encodeGetdataorigFreeDaIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigFreeDaIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_FREE_DA_INDICATOR_SEQ_LEN;
			encodeGetdataorigFreeDaIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigFreeDaIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_FREE_DA_INDICATOR_TAG_LEN;
			encodedorigFreeDaIndicatorTag1 = GetDataConstants.GETDATA_ORIG_FREE_DA_INDICATOR_TAG1;
			encodedorigFreeDaIndicatorTag2 = GetDataConstants.GETDATA_ORIG_FREE_DA_INDICATOR_TAG2;
			if ( origFreeDaIndicatorStr != null) {
				encodeGetdataorigFreeDaIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigFreeDaIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_FREE_DA_INDICATOR_LENGTH;
				origFreeDaIndicatorEncoded = asciToHex(origFreeDaIndicatorStr);
				logger.debug("origFreeDaIndicator::"+origFreeDaIndicatorStr+" encoded as::"+origFreeDaIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_FREE_DA_INDICATOR_LEN_OCTET;
			}
			else if (origFreeDaIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigFreeDaIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origFreeDaIndicatorErr != 6 ) {
					encodeGetdataorigFreeDaIndicatorElementErrorLen = 0x02;
					encodeGetdataorigFreeDaIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origFreeDaIndicatorErrorEncoded = CommonUtils.formatIntToByte(origFreeDaIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origFreeDaIndicator Error::"+origFreeDaIndicatorErr+" encoded as::"+origFreeDaIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_FREE_DA_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigFreeDaIndicatorElementErrorLen = 0x01;
					encodeGetdataorigFreeDaIndicatorSeqLen = 0x07;
					origFreeDaIndicatorErrorEncoded = CommonUtils.formatIntToByte(origFreeDaIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origFreeDaIndicator Error::"+origFreeDaIndicatorErr+" encoded as::"+origFreeDaIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_FREE_DA_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//origSpecialBnsIndicator
		String origSpecialBnsIndicatorStr = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SPECIAL_BNS_INDICATOR) != null) {
			origSpecialBnsIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SPECIAL_BNS_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origSpecialBnsIndicator element is present, value:: "+origSpecialBnsIndicatorStr);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SPECIAL_BNS_INDICATOR_ERR) != null) {
			origSpecialBnsIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SPECIAL_BNS_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origSpecialBnsIndicator element Error is present, value:: "+origSpecialBnsIndicatorErr);
			}
		}
		if (origSpecialBnsIndicatorStr != null  || origSpecialBnsIndicatorErr != 0) {

			encodeGetdataorigSpecialBnsIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigSpecialBnsIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_SPECIAL_BNS_INDICATOR_SEQ_LEN;
			encodeGetdataorigSpecialBnsIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigSpecialBnsIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_SPECIAL_BNS_INDICATOR_TAG_LEN;
			encodedorigSpecialBnsIndicatorTag1 = GetDataConstants.GETDATA_ORIG_SPECIAL_BNS_INDICATOR_TAG1;
			encodedorigSpecialBnsIndicatorTag2 = GetDataConstants.GETDATA_ORIG_SPECIAL_BNS_INDICATOR_TAG2;
			if ( origSpecialBnsIndicatorStr != null) {
				encodeGetdataorigSpecialBnsIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigSpecialBnsIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_SPECIAL_BNS_INDICATOR_LENGTH;
				origSpecialBnsIndicatorEncoded = asciToHex(origSpecialBnsIndicatorStr);
				logger.debug("origSpecialBnsIndicator::"+origSpecialBnsIndicatorStr+" encoded as::"+origSpecialBnsIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_SPECIAL_BNS_INDICATOR_LEN_OCTET;
			}
			else if (origSpecialBnsIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigSpecialBnsIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origSpecialBnsIndicatorErr != 6 ) {
					encodeGetdataorigSpecialBnsIndicatorElementErrorLen = 0x02;
					encodeGetdataorigSpecialBnsIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origSpecialBnsIndicatorErrorEncoded = CommonUtils.formatIntToByte(origSpecialBnsIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origSpecialBnsIndicator Error::"+origSpecialBnsIndicatorErr+" encoded as::"+origSpecialBnsIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_SPECIAL_BNS_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigSpecialBnsIndicatorElementErrorLen = 0x01;
					encodeGetdataorigSpecialBnsIndicatorSeqLen = 0x07;
					origSpecialBnsIndicatorErrorEncoded = CommonUtils.formatIntToByte(origSpecialBnsIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origSpecialBnsIndicator Error::"+origSpecialBnsIndicatorErr+" encoded as::"+origSpecialBnsIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_SPECIAL_BNS_INDICATOR_ERR_LEN + 1;
				}				
			}
		}
		//origSentpaidIndicator
		String origSentpaidIndicatorStr = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SENTPAID_INDICATOR) != null) {
			origSentpaidIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SENTPAID_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origSentpaidIndicator element is present, value:: "+origSentpaidIndicatorStr);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SENTPAID_INDICATOR_ERR) != null) {
			origSentpaidIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SENTPAID_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origSentpaidIndicator element Error is present, value:: "+origSentpaidIndicatorErr);
			}
		}
		if (origSentpaidIndicatorStr != null || origSentpaidIndicatorErr != 0) {

			encodeGetdataorigSentpaidIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigSentpaidIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_SENTPAID_INDICATOR_SEQ_LEN;
			encodeGetdataorigSentpaidIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigSentpaidIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_SENTPAID_INDICATOR_TAG_LEN;
			encodedorigSentpaidIndicatorTag1 = GetDataConstants.GETDATA_ORIG_SENTPAID_INDICATOR_TAG1;
			encodedorigSentpaidIndicatorTag2 = GetDataConstants.GETDATA_ORIG_SENTPAID_INDICATOR_TAG2;
			if ( origSentpaidIndicatorStr != null) {
				encodeGetdataorigSentpaidIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigSentpaidIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_SENTPAID_INDICATOR_LENGTH;
				origSentpaidIndicatorEncoded = asciToHex(origSentpaidIndicatorStr);
				logger.debug("origSentpaidIndicator::"+origSentpaidIndicatorStr+" encoded as::"+origSentpaidIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_SENTPAID_INDICATOR_LEN_OCTET;
			}
			else if (origSentpaidIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigSentpaidIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origSentpaidIndicatorErr != 6 ) {
					encodeGetdataorigSentpaidIndicatorElementErrorLen = 0x02;
					encodeGetdataorigSentpaidIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origSentpaidIndicatorErrorEncoded = CommonUtils.formatIntToByte(origSentpaidIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origSentpaidIndicator Error::"+origSentpaidIndicatorErr+" encoded as::"+origSentpaidIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_SENTPAID_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigSentpaidIndicatorElementErrorLen = 0x01;
					encodeGetdataorigSentpaidIndicatorSeqLen = 0x07;
					origSentpaidIndicatorErrorEncoded = CommonUtils.formatIntToByte(origSentpaidIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origSentpaidIndicator Error::"+origSentpaidIndicatorErr+" encoded as::"+origSentpaidIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_SENTPAID_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//origDaccIndicator
		String origDaccIndicatorStr = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_DACC_INDICATOR) != null) {
			origDaccIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_DACC_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origDaccIndicator element is present, value:: "+origDaccIndicatorStr);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_DACC_INDICATOR_ERR) != null) {
			origDaccIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_DACC_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origDaccIndicator element Error is present, value:: "+origDaccIndicatorErr);
			}
		}
		if (origDaccIndicatorStr != null  || origDaccIndicatorErr != 0) {

			encodeGetdataorigDaccIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigDaccIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_DACC_INDICATOR_SEQ_LEN;
			encodeGetdataorigDaccIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigDaccIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_DACC_INDICATOR_TAG_LEN;
			encodedorigDaccIndicatorTag1 = GetDataConstants.GETDATA_ORIG_DACC_INDICATOR_TAG1;
			encodedorigDaccIndicatorTag2 = GetDataConstants.GETDATA_ORIG_DACC_INDICATOR_TAG2;
			if ( origDaccIndicatorStr != null) {
				encodeGetdataorigDaccIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigDaccIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_DACC_INDICATOR_LENGTH;
				origDaccIndicatorEncoded = asciToHex(origDaccIndicatorStr);
				logger.debug("origDaccIndicator::"+origDaccIndicatorStr+" encoded as::"+origDaccIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_DACC_INDICATOR_LEN_OCTET;
			}
			else if (origDaccIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigDaccIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origDaccIndicatorErr != 6 ) {
					encodeGetdataorigDaccIndicatorElementErrorLen = 0x02;
					encodeGetdataorigDaccIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origDaccIndicatorErrorEncoded = CommonUtils.formatIntToByte(origDaccIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origDaccIndicator Error::"+origDaccIndicatorErr+" encoded as::"+origDaccIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_DACC_INDICATOR_ERR_LEN + 2;


				}
				else {
					encodeGetdataorigDaccIndicatorElementErrorLen = 0x01;
					encodeGetdataorigDaccIndicatorSeqLen = 0x07;
					origDaccIndicatorErrorEncoded = CommonUtils.formatIntToByte(origDaccIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origDaccIndicator Error::"+origDaccIndicatorErr+" encoded as::"+origDaccIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_DACC_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//origBillingServiceSpareIndicator
		String origBillingServiceSpareIndicatorStr = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_SPARE_INDICATOR) != null) {
			origBillingServiceSpareIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_SPARE_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("origBillingServiceSpareIndicator element is present, value:: "+origBillingServiceSpareIndicatorStr);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR) != null) {
			origBillingServiceSpareIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origBillingServiceSpareIndicator element Error is present, value:: "+origBillingServiceSpareIndicatorErr);
			}
		}
		if (origBillingServiceSpareIndicatorStr != null  || origBillingServiceSpareIndicatorErr != 0) {

			encodeGetdataorigBillingServiceSpareIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigBillingServiceSpareIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_SEQ_LEN;
			encodeGetdataorigBillingServiceSpareIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigBillingServiceSpareIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG_LEN;
			encodedorigBillingServiceSpareIndicatorTag1 = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG1;
			encodedorigBillingServiceSpareIndicatorTag2 = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_TAG2;
			if ( origBillingServiceSpareIndicatorStr != null) {
				encodeGetdataorigBillingServiceSpareIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigBillingServiceSpareIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_LENGTH;
				origBillingServiceSpareIndicatorEncoded = asciToHex(origBillingServiceSpareIndicatorStr);
				logger.debug("origBillingServiceSpareIndicator::"+origBillingServiceSpareIndicatorStr+" encoded as::"+origBillingServiceSpareIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_LEN_OCTET;
			}
			else if (origBillingServiceSpareIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigBillingServiceSpareIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origBillingServiceSpareIndicatorErr != 6 ) {
					encodeGetdataorigBillingServiceSpareIndicatorElementErrorLen = 0x02;
					encodeGetdataorigBillingServiceSpareIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					origBillingServiceSpareIndicatorErrorEncoded = CommonUtils.formatIntToByte(origBillingServiceSpareIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origBillingServiceSpareIndicator Error::"+origBillingServiceSpareIndicatorErr+" encoded as::"+origBillingServiceSpareIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigBillingServiceSpareIndicatorElementErrorLen = 0x01;
					encodeGetdataorigBillingServiceSpareIndicatorSeqLen = 0x07;
					origBillingServiceSpareIndicatorErrorEncoded = CommonUtils.formatIntToByte(origBillingServiceSpareIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("origBillingServiceSpareIndicator Error::"+origBillingServiceSpareIndicatorErr+" encoded as::"+origBillingServiceSpareIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		// origIcIndicators element
		byte encodeGetdataorigIcIndicatorsSeq = 0;
		byte encodeGetdataorigIcIndicatorsSeqLen = 0;
		byte encodeGetdataorigIcIndicatorsElementIdTag = 0;
		byte encodeGetdataorigIcIndicatorsElementIdLen = 0;
		byte[] origIcIndicatorsEncoded1 = null;
		byte[] origIcIndicatorsEncoded2 = null;
		byte encodedorigIcIndicatorsTag = 0;
		byte encodeGetdataorigIcIndicatorsElementDataTag = 0;
		byte encodeGetdataorigIcIndicatorsElementDataLen = 0;
		int origIcIndicatorsErr = 0;
		byte encodeGetdataorigIcIndicatorsElementErrorTag = 0;
		byte encodeGetdataorigIcIndicatorsElementErrorLen = 0;
		byte[] origIcIndicatorsErrorEncoded = null;

		int origIcIndicator = -1;
		byte encodeGetdataorigIcIndicatorSeq = 0;
		byte encodeGetdataorigIcIndicatorSeqLen = 0;
		byte encodeGetdataorigIcIndicatorElementIdTag = 0;
		byte encodeGetdataorigIcIndicatorElementIdLen = 0;
		byte[] origIcIndicatorEncoded = null;
		byte encodedorigIcIndicatorTag1 = 0;
		byte encodedorigIcIndicatorTag2 = 0;
		byte encodeGetdataorigIcIndicatorElementDataTag = 0;
		byte encodeGetdataorigIcIndicatorElementDataLen = 0;

		int origIncIndicator = -1;
		byte encodeGetdataorigIncIndicatorSeq = 0;
		byte encodeGetdataorigIncIndicatorSeqLen = 0;
		byte encodeGetdataorigIncIndicatorElementIdTag = 0;
		byte encodeGetdataorigIncIndicatorElementIdLen = 0;
		int origIcIndicatorErr = 0;
		byte encodeGetdataorigIcIndicatorElementErrorTag = 0;
		byte encodeGetdataorigIcIndicatorElementErrorLen = 0;
		byte[] origIcIndicatorErrorEncoded = null;

		byte[] origIncIndicatorEncoded = null;
		byte encodedorigIncIndicatorTag1 = 0;
		byte encodedorigIncIndicatorTag2 = 0;
		byte encodeGetdataorigIncIndicatorElementDataTag = 0;
		byte encodeGetdataorigIncIndicatorElementDataLen = 0;
		int origIncIndicatorErr = 0;
		byte encodeGetdataorigIncIndicatorElementErrorTag = 0;
		byte encodeGetdataorigIncIndicatorElementErrorLen = 0;
		byte[] origIncIndicatorErrorEncoded = null;

		//checking if collective element is requested in this case LIDB will return both ORIG IC and INC indicators 
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR) != null &&
				leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR) != null) {
			String origIcIndicators1 = (String) leg2Data
					.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR);
			String origIcIndicators2 = (String) leg2Data
					.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR);
			origIcIndicatorsEncoded1 = CommonUtils
					.formatIntToByte(Integer.parseInt(origIcIndicators1));
			origIcIndicatorsEncoded2 = CommonUtils
					.formatIntToByte(Integer.parseInt(origIcIndicators2));
			if(logger.isDebugEnabled()){
				logger.debug("origIcIndicators element is present, values:: "+origIcIndicators1 + ", " + origIcIndicators2);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATORS_ERR) != null) {
			origIcIndicatorsErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATORS_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origIcIndicators element Error is present, value:: "+origIcIndicatorsErr);
			}
		}

		if ( (origIcIndicatorsEncoded1 != null && origIcIndicatorsEncoded2 != null ) || origIcIndicatorsErr != 0) {
			encodeGetdataorigIcIndicatorsSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigIcIndicatorsSeqLen = GetDataConstants.GETDATA_ORIG_IC_INDICATORS_SEQ_LEN;
			encodeGetdataorigIcIndicatorsElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigIcIndicatorsElementIdLen = GetDataConstants.GETDATA_ORIG_IC_INDICATORS_TAG_LEN;
			encodedorigIcIndicatorsTag = GetDataConstants.GETDATA_ORIG_IC_INDICATORS_TAG;
			if ( origIcIndicatorsEncoded1 != null && origIcIndicatorsEncoded2 != null ) {
				encodeGetdataorigIcIndicatorsElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_OCTET;
				encodeGetdataorigIcIndicatorsElementDataLen = GetDataConstants.GETDATA_ORIG_IC_INDICATORS_LENGTH;
				totalLength += GetDataConstants.GETDATA_ORIG_IC_INDICATORS_LEN_OCTET;
			}
			else if (origIcIndicatorsErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigIcIndicatorsElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origIcIndicatorsErr != 6 ) {
					encodeGetdataorigIcIndicatorsElementErrorLen = 0x02;
					encodeGetdataorigIcIndicatorsSeqLen = 0x07;
					//LIDBElementError VALUE
					origIcIndicatorsErrorEncoded = CommonUtils.formatIntToByte(origIcIndicatorsErr);
					if(logger.isDebugEnabled()){
						logger.debug("origIcIndicators Error::"+origIcIndicatorsErr+" encoded as::"+origIcIndicatorsErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_IC_INDICATORS_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigIcIndicatorsElementErrorLen = 0x01;
					encodeGetdataorigIcIndicatorsSeqLen = 0x06;
					origIcIndicatorsErrorEncoded = CommonUtils.formatIntToByte(origIcIndicatorsErr);
					if(logger.isDebugEnabled()){
						logger.debug("origIcIndicators Error::"+origIcIndicatorsErr+" encoded as::"+origIcIndicatorsErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_IC_INDICATORS_ERR_LEN + 1;
				}				
			}
		}

		//if individual indicators requested
		else {
			//origIcIndicator

			if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR) != null) {
				String ic_ind= (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR);

				origIcIndicator=Integer.parseInt(ic_ind);
				if(logger.isDebugEnabled()){
					logger.debug("origIcIndicator element is present, value:: "+origIcIndicator);
				}
			}
			else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR_ERR) != null) {
				origIcIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR_ERR);
				if(logger.isDebugEnabled()){
					logger.debug("origIcIndicator element Error is present, value:: "+origIcIndicatorErr);
				}
			}
			if (origIcIndicator != -1  || origIcIndicatorErr != 0) {

				encodeGetdataorigIcIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
				encodeGetdataorigIcIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_IC_INDICATOR_SEQ_LEN;
				encodeGetdataorigIcIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
				encodeGetdataorigIcIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_IC_INDICATOR_TAG_LEN;
				encodedorigIcIndicatorTag1 = GetDataConstants.GETDATA_ORIG_IC_INDICATOR_TAG1;
				encodedorigIcIndicatorTag2 = GetDataConstants.GETDATA_ORIG_IC_INDICATOR_TAG2;
				if ( origIcIndicator != -1) {
					encodeGetdataorigIcIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
					encodeGetdataorigIcIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_IC_INDICATOR_LENGTH;
					origIcIndicatorEncoded = CommonUtils.formatIntToByte(origIcIndicator);
					logger.debug("origIcIndicator::"+origIcIndicator+" encoded as::"+origIcIndicatorEncoded);
					totalLength += GetDataConstants.GETDATA_ORIG_IC_INDICATOR_LEN_OCTET;
				}
				else if (origIcIndicatorErr != 0 ) {
					//LIDBElementError TAG 
					encodeGetdataorigIcIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
					//LIDBElementError LENGTH (length of Error)
					if (origIcIndicatorErr != 6 ) {
						encodeGetdataorigIcIndicatorElementErrorLen = 0x02;
						encodeGetdataorigIcIndicatorSeqLen = 0x08;
						//LIDBElementError VALUE
						origIcIndicatorErrorEncoded = CommonUtils.formatIntToByte(origIcIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("origIcIndicator Error::"+origIcIndicatorErr+" encoded as::"+origIcIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_ORIG_IC_INDICATOR_ERR_LEN + 2;

					}
					else {
						encodeGetdataorigIcIndicatorElementErrorLen = 0x01;
						encodeGetdataorigIcIndicatorSeqLen = 0x07;
						origIcIndicatorErrorEncoded = CommonUtils.formatIntToByte(origIcIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("origIcIndicator Error::"+origIcIndicatorErr+" encoded as::"+origIcIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_ORIG_IC_INDICATOR_ERR_LEN + 1;
					}				
				}
			}

			//origIncIndicator
			if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR) != null) {
				String origIncIndicatorStr = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR);

				origIncIndicator=Integer.parseInt(origIncIndicatorStr);
				if(logger.isDebugEnabled()){
					logger.debug("origIncIndicator element is present, value:: "+origIncIndicator);
				}
			}
			else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR_ERR) != null) {
				origIncIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR_ERR);
				if(logger.isDebugEnabled()){
					logger.debug("origIncIndicator element Error is present, value:: "+origIncIndicatorErr);
				}
			}
			if (origIncIndicator != -1  || origIncIndicatorErr != 0) {

				encodeGetdataorigIncIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
				encodeGetdataorigIncIndicatorSeqLen = GetDataConstants.GETDATA_ORIG_INC_INDICATOR_SEQ_LEN;
				encodeGetdataorigIncIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
				encodeGetdataorigIncIndicatorElementIdLen = GetDataConstants.GETDATA_ORIG_INC_INDICATOR_TAG_LEN;
				encodedorigIncIndicatorTag1 = GetDataConstants.GETDATA_ORIG_INC_INDICATOR_TAG1;
				encodedorigIncIndicatorTag2 = GetDataConstants.GETDATA_ORIG_INC_INDICATOR_TAG2;
				if ( origIncIndicator != -1) {
					encodeGetdataorigIncIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
					encodeGetdataorigIncIndicatorElementDataLen = GetDataConstants.GETDATA_ORIG_INC_INDICATOR_LENGTH;
					origIncIndicatorEncoded = CommonUtils.formatIntToByte(origIncIndicator);
					logger.debug("origIncIndicator::"+origIncIndicator+" encoded as::"+origIncIndicatorEncoded);
					totalLength += GetDataConstants.GETDATA_ORIG_INC_INDICATOR_LEN_OCTET;
				}
				else if (origIncIndicatorErr != 0 ) {
					//LIDBElementError TAG 
					encodeGetdataorigIncIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
					//LIDBElementError LENGTH (length of Error)
					if (origIncIndicatorErr != 6 ) {
						encodeGetdataorigIncIndicatorElementErrorLen = 0x02;
						encodeGetdataorigIncIndicatorSeqLen = 0x08;
						//LIDBElementError VALUE
						origIncIndicatorErrorEncoded = CommonUtils.formatIntToByte(origIncIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("origIncIndicator Error::"+origIncIndicatorErr+" encoded as::"+origIncIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_ORIG_INC_INDICATOR_ERR_LEN + 2;

					}
					else {
						encodeGetdataorigIncIndicatorElementErrorLen = 0x01;
						encodeGetdataorigIncIndicatorSeqLen = 0x07;
						origIncIndicatorErrorEncoded = CommonUtils.formatIntToByte(origIncIndicatorErr);
						if(logger.isDebugEnabled()){
							logger.debug("origIncIndicator Error::"+origIncIndicatorErr+" encoded as::"+origIncIndicatorErrorEncoded);
						}
						totalLength += GetDataConstants.GETDATA_ORIG_INC_INDICATOR_ERR_LEN + 1;
					}				
				}
			}
		}

		//origIc Element 
		String origIc = null;
		byte encodeGetdataorigIcSeq  = 0;
		byte encodeGetdataorigIcSeqLen  = 0;
		byte encodeGetdataorigIcElementIdTag  = 0;
		byte encodeGetdataorigIcElementIdLen  = 0;
		byte[] origIcEncoded  = null;
		byte encodedorigIcTag1  = 0;
		byte encodedorigIcTag2  = 0;
		byte encodeGetdataorigIcElementDataTag1  = 0;
		byte encodeGetdataorigIcElementDataTag2  = 0;
		byte encodeGetdataorigIcElementDataLen  = 0;
		int origIcErr = 0;
		byte encodeGetdataorigIcElementErrorTag = 0;
		byte encodeGetdataorigIcElementErrorLen = 0;
		byte[] origIcErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC) != null) {
			origIc = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC);
			if(logger.isDebugEnabled()){
				logger.debug("origIc element is present, value:: "+origIc);
			}

			if(!StringUtils.isNotBlank(origIc)){
				origIc = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_ERR) != null) {
			origIcErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origIc element Error is present, value:: "+origIcErr);
			}
		}

		if ( (origIc != null && !origIc.isEmpty()) || origIcErr != 0) {
			//SEQUENCE of origIc
			encodeGetdataorigIcSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof origIc
			encodeGetdataorigIcSeqLen = GetDataConstants.GETDATA_ORIG_IC_SEQ_LEN;

			//origIc LIDBElementIdentifier TAG
			encodeGetdataorigIcElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//origIc LIDBElementIdentifier LENGTH
			encodeGetdataorigIcElementIdLen = GetDataConstants.GETDATA_ORIG_IC_TAG_LEN;
			encodedorigIcTag1 = 0;
			encodedorigIcTag2 = 0;
			//origIc LIDBElementIdentifier VALUE. i.e origIc identifier
			encodedorigIcTag1 = GetDataConstants.GETDATA_ORIG_IC_TAG1;
			encodedorigIcTag2 = GetDataConstants.GETDATA_ORIG_IC_TAG2;
			if ( origIc != null && !origIc.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdataorigIcElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_1;
				encodeGetdataorigIcElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_2;
				//LIDBElementData LENGTH (length of origIc)
				encodeGetdataorigIcElementDataLen = GetDataConstants.GETDATA_LIDB_ELEMENT_DATA_BCD_DIG;
				//LIDBElementData VALUE . i.e Account Owner value
				origIcEncoded = encodeAdrsSignalForGetQuery(origIc);
				logger.debug("origIc::"+origIc+" encoded as::"+origIcEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_IC_LEN_OCTET;
			}
			else if (origIcErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigIcElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origIcErr != 6 ) {
					encodeGetdataorigIcElementErrorLen = 0x02;
					encodeGetdataorigIcSeqLen = 0x08;
					//LIDBElementError VALUE
					origIcErrorEncoded = CommonUtils.formatIntToByte(origIcErr);
					if(logger.isDebugEnabled()){
						logger.debug("origIc Error::"+origIcErr+" encoded as::"+origIcErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_IC_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigIcElementErrorLen = 0x01;
					encodeGetdataorigIcSeqLen = 0x07;
					origIcErrorEncoded = CommonUtils.formatIntToByte(origIcErr);
					if(logger.isDebugEnabled()){
						logger.debug("origIc Error::"+origIcErr+" encoded as::"+origIcErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_IC_ERR_LEN + 1;
				}				
			}
		}

		//origInc Element 
		String origInc = null;
		byte encodeGetdataorigIncSeq  = 0;
		byte encodeGetdataorigIncSeqLen  = 0;
		byte encodeGetdataorigIncElementIdTag  = 0;
		byte encodeGetdataorigIncElementIdLen  = 0;
		byte[] origIncEncoded  = null;
		byte encodedorigIncTag1  = 0;
		byte encodedorigIncTag2  = 0;
		byte encodeGetdataorigIncElementDataTag1  = 0;
		byte encodeGetdataorigIncElementDataTag2  = 0;
		byte encodeGetdataorigIncElementDataLen  = 0;
		int origIncErr = 0;
		byte encodeGetdataorigIncElementErrorTag = 0;
		byte encodeGetdataorigIncElementErrorLen = 0;
		byte[] origIncErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC) != null) {
			origInc = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC);
			if(logger.isDebugEnabled()){
				logger.debug("origInc element is present, value:: "+origInc);
			}

			if(!StringUtils.isNotBlank(origInc)){
				origInc = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_ERR) != null) {
			origIncErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origInc element Error is present, value:: "+origIncErr);
			}
		}

		if ( (origInc != null && !origInc.isEmpty()) || origIncErr != 0) {
			//SEQUENCE of origInc
			encodeGetdataorigIncSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof origInc
			encodeGetdataorigIncSeqLen = GetDataConstants.GETDATA_ORIG_INC_SEQ_LEN;

			//origInc LIDBElementIdentifier TAG
			encodeGetdataorigIncElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//origInc LIDBElementIdentifier LENGTH
			encodeGetdataorigIncElementIdLen = GetDataConstants.GETDATA_ORIG_INC_TAG_LEN;
			encodedorigIncTag1 = 0;
			encodedorigIncTag2 = 0;
			//origInc LIDBElementIdentifier VALUE. i.e origInc identifier
			encodedorigIncTag1 = GetDataConstants.GETDATA_ORIG_INC_TAG1;
			encodedorigIncTag2 = GetDataConstants.GETDATA_ORIG_INC_TAG2;
			if ( origInc != null && !origInc.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdataorigIncElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_1;
				encodeGetdataorigIncElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_2;
				//LIDBElementData LENGTH (length of origInc)
				encodeGetdataorigIncElementDataLen = GetDataConstants.GETDATA_LIDB_ELEMENT_DATA_BCD_DIG;
				//LIDBElementData VALUE . i.e Account Owner value
				origIncEncoded = encodeAdrsSignalForGetQuery(origInc);
				logger.debug("origInc::"+origInc+" encoded as::"+origIncEncoded);
				totalLength += GetDataConstants.GETDATA_ORIG_INC_LEN_OCTET;
			}
			else if (origIncErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigIncElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origIncErr != 6 ) {
					encodeGetdataorigIncElementErrorLen = 0x02;
					encodeGetdataorigIncSeqLen = 0x08;
					//LIDBElementError VALUE
					origIncErrorEncoded = CommonUtils.formatIntToByte(origIncErr);
					if(logger.isDebugEnabled()){
						logger.debug("origInc Error::"+origIncErr+" encoded as::"+origIncErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_INC_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigIncElementErrorLen = 0x01;
					encodeGetdataorigIncSeqLen = 0x07;
					origIncErrorEncoded = CommonUtils.formatIntToByte(origIncErr);
					if(logger.isDebugEnabled()){
						logger.debug("origInc Error::"+origIncErr+" encoded as::"+origIncErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_INC_ERR_LEN + 1;
				}				
			}
		}

		//origListingServicesIndicator1
		String origListingServicesIndicator1 = null;
		byte encodeGetdataorigListingServicesIndicator1Seq = 0;
		byte encodeGetdataorigListingServicesIndicator1SeqLen = 0;
		byte encodeGetdataorigListingServicesIndicator1ElementIdTag = 0;
		byte encodeGetdataorigListingServicesIndicator1ElementIdLen = 0;
		byte[] origListingServicesIndicator1Encoded = null;
		byte encodedorigListingServicesIndicator1Tag = 0;
		byte encodeGetdataorigListingServicesIndicator1ElementDataTag = 0;
		byte encodeGetdataorigListingServicesIndicator1ElementDataLen = 0;
		int origListingServicesIndicator1Err = 0;
		byte encodeGetdataorigListingServicesIndicator1ElementErrorTag = 0;
		byte encodeGetdataorigListingServicesIndicator1ElementErrorLen = 0;
		byte[] origListingServicesIndicator1ErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LISTING_SERVICES_INDICATOR_1) != null) {
			origListingServicesIndicator1 = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LISTING_SERVICES_INDICATOR_1);
			if(logger.isDebugEnabled()){
				logger.debug("origListingServicesIndicator1 element is present, value:: "+origListingServicesIndicator1);
			}

			if(!StringUtils.isNotBlank(origListingServicesIndicator1)){
				origListingServicesIndicator1 = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LISTING_SERVICES_INDICATOR_1_ERR) != null) {
			origListingServicesIndicator1Err = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LISTING_SERVICES_INDICATOR_1_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("origListingServicesIndicator1 element Error is present, value:: "+origListingServicesIndicator1Err);
			}
		}

		if ( (origListingServicesIndicator1 != null && !origListingServicesIndicator1.isEmpty()) || origListingServicesIndicator1Err != 0) {
			encodeGetdataorigListingServicesIndicator1Seq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataorigListingServicesIndicator1SeqLen = GetDataConstants.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_SEQ_LEN;
			encodeGetdataorigListingServicesIndicator1ElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataorigListingServicesIndicator1ElementIdLen = GetDataConstants.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_TAG_LEN;
			encodedorigListingServicesIndicator1Tag = GetDataConstants.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_TAG;
			if ( origListingServicesIndicator1 != null && !origListingServicesIndicator1.isEmpty()) {
				encodeGetdataorigListingServicesIndicator1ElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataorigListingServicesIndicator1ElementDataLen = GetDataConstants.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_LENGTH;
				//origListingServicesIndicator1Encoded = asciToHex(origListingServicesIndicator1);
				int origListingServicesIndicator1Int = Integer.parseInt(origListingServicesIndicator1);
				origListingServicesIndicator1Encoded = CommonUtils.formatIntToByte(origListingServicesIndicator1Int);
				logger.debug("origListingServicesIndicator1::"+origListingServicesIndicator1+" encoded as::"+origListingServicesIndicator1Encoded);
				totalLength += GetDataConstants.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_LEN_OCTET;
			}
			else if (origListingServicesIndicator1Err != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataorigListingServicesIndicator1ElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (origListingServicesIndicator1Err != 6 ) {
					encodeGetdataorigListingServicesIndicator1ElementErrorLen = 0x02;
					encodeGetdataorigListingServicesIndicator1SeqLen = 0x07;
					//LIDBElementError VALUE
					origListingServicesIndicator1ErrorEncoded = CommonUtils.formatIntToByte(origListingServicesIndicator1Err);
					if(logger.isDebugEnabled()){
						logger.debug("origListingServicesIndicator1 Error::"+origListingServicesIndicator1Err+" encoded as::"+origListingServicesIndicator1ErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_ERR_LEN + 2;

				}
				else {
					encodeGetdataorigListingServicesIndicator1ElementErrorLen = 0x01;
					encodeGetdataorigListingServicesIndicator1SeqLen = 0x06;
					origListingServicesIndicator1ErrorEncoded = CommonUtils.formatIntToByte(origListingServicesIndicator1Err);
					if(logger.isDebugEnabled()){
						logger.debug("origListingServicesIndicator1 Error::"+origListingServicesIndicator1Err+" encoded as::"+origListingServicesIndicator1ErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1_ERR_LEN + 1;
				}				
			}
		}

		//preferredInc Element 
		String preferredInc = null;
		byte encodeGetdatapreferredIncSeq  = 0;
		byte encodeGetdatapreferredIncSeqLen  = 0;
		byte encodeGetdatapreferredIncElementIdTag  = 0;
		byte encodeGetdatapreferredIncElementIdLen  = 0;
		byte[] preferredIncEncoded  = null;
		byte encodedpreferredIncTag1  = 0;
		byte encodedpreferredIncTag2  = 0;
		byte encodeGetdatapreferredIncElementDataTag1  = 0;
		byte encodeGetdatapreferredIncElementDataTag2  = 0;
		byte encodeGetdatapreferredIncElementDataLen  = 0;
		int preferredIncErr = 0;
		byte encodeGetdatapreferredIncElementErrorTag = 0;
		byte encodeGetdatapreferredIncElementErrorLen = 0;
		byte[] preferredIncErrorEncoded = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC) != null) {
			preferredInc = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC);
			if(logger.isDebugEnabled()){
				logger.debug("preferredInc element is present, value:: "+preferredInc);
			}

			if(!StringUtils.isNotBlank(preferredInc)){
				preferredInc = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_ERR) != null) {
			preferredIncErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("preferredInc element Error is present, value:: "+preferredIncErr);
			}
		}

		if ( (preferredInc != null && !preferredInc.isEmpty()) || preferredIncErr != 0) {
			//SEQUENCE of preferredInc
			encodeGetdatapreferredIncSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof preferredInc
			encodeGetdatapreferredIncSeqLen = GetDataConstants.GETDATA_PREFERRED_INC_SEQ_LEN;

			//preferredInc LIDBElementIdentifier TAG
			encodeGetdatapreferredIncElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//preferredInc LIDBElementIdentifier LENGTH
			encodeGetdatapreferredIncElementIdLen = GetDataConstants.GETDATA_PREFERRED_INC_TAG_LEN;
			encodedpreferredIncTag1 = 0;
			encodedpreferredIncTag2 = 0;
			//preferredInc LIDBElementIdentifier VALUE. i.e preferredInc identifier
			encodedpreferredIncTag1 = GetDataConstants.GETDATA_PREFERRED_INC_TAG1;
			encodedpreferredIncTag2 = GetDataConstants.GETDATA_PREFERRED_INC_TAG2;
			if ( preferredInc != null && !preferredInc.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdatapreferredIncElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_1;
				encodeGetdatapreferredIncElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_2;
				//LIDBElementData LENGTH (length of preferredInc)
				encodeGetdatapreferredIncElementDataLen = GetDataConstants.GETDATA_LIDB_ELEMENT_DATA_BCD_DIG;
				//LIDBElementData VALUE . i.e Account Owner value
				preferredIncEncoded = encodeAdrsSignalForGetQuery(preferredInc);
				logger.debug("preferredInc::"+preferredInc+" encoded as::"+preferredIncEncoded);
				totalLength += GetDataConstants.GETDATA_PREFERRED_INC_LEN_OCTET;
			}
			else if (preferredIncErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatapreferredIncElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (preferredIncErr != 6 ) {
					encodeGetdatapreferredIncElementErrorLen = 0x02;
					encodeGetdatapreferredIncSeqLen = 0x08;
					//LIDBElementError VALUE
					preferredIncErrorEncoded = CommonUtils.formatIntToByte(preferredIncErr);
					if(logger.isDebugEnabled()){
						logger.debug("preferredInc Error::"+preferredIncErr+" encoded as::"+preferredIncErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_PREFERRED_INC_ERR_LEN + 2;

				}
				else {
					encodeGetdatapreferredIncElementErrorLen = 0x01;
					encodeGetdatapreferredIncSeqLen = 0x07;
					preferredIncErrorEncoded = CommonUtils.formatIntToByte(preferredIncErr);
					if(logger.isDebugEnabled()){
						logger.debug("preferredInc Error::"+preferredIncErr+" encoded as::"+preferredIncErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_PREFERRED_INC_ERR_LEN + 1;
				}				
			}
		}

		//preferredCodeList 

		String preferredCodeList = null;
		byte encodeGetdatapreferredCodeListSeq = 0;
		byte encodeGetdatapreferredCodeListSeqLen = 0;
		byte encodeGetdatapreferredCodeListElementIdTag = 0;
		byte encodeGetdatapreferredCodeListElementIdLen = 0;
		byte[] preferredCodeListEncoded = null;
		byte encodedpreferredCodeListTag1 = 0;
		byte encodedpreferredCodeListTag2 = 0;
		byte encodeGetdatapreferredCodeListElementDataTag = 0;
		byte encodeGetdatapreferredCodeListElementDataLen = 0;

		int preferredCodeListErr = 0;
		byte encodeGetdatapreferredCodeListElementErrorTag = 0;
		byte encodeGetdatapreferredCodeListElementErrorLen = 0;
		byte[] preferredCodeListErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_CODE_LIST) != null) {
			preferredCodeList = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_CODE_LIST);
			if(logger.isDebugEnabled()){
				logger.debug("preferredCodeList element is present, value:: "+preferredCodeList);
			}

			if(!StringUtils.isNotBlank(preferredCodeList)){
				preferredCodeList = null;
			}
		}

		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_CODE_LIST_ERR) != null) {
			preferredCodeListErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_CODE_LIST_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("preferredCodeList element Error is present, value:: "+preferredCodeListErr);
			}
		}

		if ( (preferredCodeList != null && !preferredCodeList.isEmpty()) || preferredCodeListErr != 0) {
			encodeGetdatapreferredCodeListSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatapreferredCodeListElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatapreferredCodeListElementIdLen = GetDataConstants.GETDATA_PREFERRED_CODE_LIST_TAG_LEN;
			encodedpreferredCodeListTag1 = GetDataConstants.GETDATA_PREFERRED_CODE_LIST_TAG1;
			encodedpreferredCodeListTag2 = GetDataConstants.GETDATA_PREFERRED_CODE_LIST_TAG2;
			if ( preferredCodeList != null && !preferredCodeList.isEmpty()) {
				if (preferredCodeList.length() > 80) {
					logger.info("encodeGetDataQuery: throwing exception ");
					logger.error("encodeGetDataQuery: preferredCodeList length should be less than 80 char");
					throw new AINCodecException("preferredCodeList length should be less than 80 char");
				}
				//encodeGetdatapreferredCodeListSeqLen += (byte) (preferredCodeList.length());
				encodeGetdatapreferredCodeListElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_OCTET;

				preferredCodeListEncoded = encodeIa5(preferredCodeList);
				logger.debug("preferredCodeList::"+preferredCodeList+" encoded as::"+preferredCodeListEncoded);
				int preferredCodeListSeqLen =  preferredCodeListEncoded.length + 5; // LIDBElementIdentifier Tag,LIDBElementIdentifier Length,LIDBElementIdentifier Value,LIDBElementData Tag,LIDBElementData Length , preferredCodeList 
				encodeGetdatapreferredCodeListSeqLen += (byte) preferredCodeListSeqLen;
				encodeGetdatapreferredCodeListElementDataLen = (byte) preferredCodeListEncoded.length;
				totalLength += preferredCodeList.length() + 8; // SEQUENCE, SEQUENCE Length,  LIDBElementIdentifier Tag,LIDBElementIdentifier Length,LIDBElementIdentifier Value,LIDBElementData Tag,LIDBElementData Length , preferredCodeList
			}
			else if (preferredCodeListErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatapreferredCodeListElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (preferredCodeListErr != 6 ) {
					encodeGetdatapreferredCodeListElementErrorLen = 0x02;
					encodeGetdatapreferredCodeListSeqLen = 0x07;
					//LIDBElementError VALUE
					preferredCodeListErrorEncoded = CommonUtils.formatIntToByte(preferredCodeListErr);
					if(logger.isDebugEnabled()){
						logger.debug("preferredCodeList Error::"+preferredCodeListErr+" encoded as::"+preferredCodeListErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_PREFERRED_CODE_LIST_ERR_LEN + 2;

				}
				else {
					encodeGetdatapreferredCodeListElementErrorLen = 0x01;
					encodeGetdatapreferredCodeListSeqLen = 0x06;
					preferredCodeListErrorEncoded = CommonUtils.formatIntToByte(preferredCodeListErr);
					if(logger.isDebugEnabled()){
						logger.debug("preferredCodeList Error::"+preferredCodeListErr+" encoded as::"+preferredCodeListErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_PREFERRED_CODE_LIST_ERR_LEN + 1;
				}				
			}
		}

		//primaryPreferredIc Element 
		String primaryPreferredIc = null;
		byte encodeGetdataprimaryPreferredIcSeq  = 0;
		byte encodeGetdataprimaryPreferredIcSeqLen  = 0;
		byte encodeGetdataprimaryPreferredIcElementIdTag  = 0;
		byte encodeGetdataprimaryPreferredIcElementIdLen  = 0;
		byte[] primaryPreferredIcEncoded  = null;
		byte encodedprimaryPreferredIcTag1  = 0;
		byte encodedprimaryPreferredIcTag2  = 0;
		byte encodeGetdataprimaryPreferredIcElementDataTag1  = 0;
		byte encodeGetdataprimaryPreferredIcElementDataTag2  = 0;
		byte encodeGetdataprimaryPreferredIcElementDataLen  = 0;
		int primaryPreferredIcErr = 0;
		byte encodeGetdataprimaryPreferredIcElementErrorTag = 0;
		byte encodeGetdataprimaryPreferredIcElementErrorLen = 0;
		byte[] primaryPreferredIcErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC) != null) {
			primaryPreferredIc = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC);
			if(logger.isDebugEnabled()){
				logger.debug("primaryPreferredIc element is present, value:: "+primaryPreferredIc);
			}

			if(!StringUtils.isNotBlank(primaryPreferredIc)){
				primaryPreferredIc = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_ERR) != null) {
			primaryPreferredIcErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("primaryPreferredIc element Error is present, value:: "+primaryPreferredIcErr);
			}
		}
		if ( (primaryPreferredIc != null && !primaryPreferredIc.isEmpty()) || primaryPreferredIcErr != 0) {
			//SEQUENCE of primaryPreferredIc
			encodeGetdataprimaryPreferredIcSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof primaryPreferredIc
			encodeGetdataprimaryPreferredIcSeqLen = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_SEQ_LEN;

			//primaryPreferredIc LIDBElementIdentifier TAG
			encodeGetdataprimaryPreferredIcElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//primaryPreferredIc LIDBElementIdentifier LENGTH
			encodeGetdataprimaryPreferredIcElementIdLen = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_TAG_LEN;
			encodedprimaryPreferredIcTag1 = 0;
			encodedprimaryPreferredIcTag2 = 0;
			//primaryPreferredIc LIDBElementIdentifier VALUE. i.e primaryPreferredIc identifier
			encodedprimaryPreferredIcTag1 = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_TAG1;
			encodedprimaryPreferredIcTag2 = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_TAG2;
			if ( primaryPreferredIc != null && !primaryPreferredIc.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdataprimaryPreferredIcElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_1;
				encodeGetdataprimaryPreferredIcElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_2;
				//LIDBElementData LENGTH (length of primaryPreferredIc)
				encodeGetdataprimaryPreferredIcElementDataLen = GetDataConstants.GETDATA_LIDB_ELEMENT_DATA_BCD_DIG;
				//LIDBElementData VALUE . i.e Account Owner value
				primaryPreferredIcEncoded = encodeAdrsSignalForGetQuery(primaryPreferredIc);
				logger.debug("primaryPreferredIc::"+primaryPreferredIc+" encoded as::"+primaryPreferredIcEncoded);
				totalLength += GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_LEN_OCTET;
			}
			else if (primaryPreferredIcErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataprimaryPreferredIcElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (primaryPreferredIcErr != 6 ) {
					encodeGetdataprimaryPreferredIcElementErrorLen = 0x02;
					encodeGetdataprimaryPreferredIcSeqLen = 0x08;
					//LIDBElementError VALUE
					primaryPreferredIcErrorEncoded = CommonUtils.formatIntToByte(primaryPreferredIcErr);
					if(logger.isDebugEnabled()){
						logger.debug("primaryPreferredIc Error::"+primaryPreferredIcErr+" encoded as::"+primaryPreferredIcErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_ERR_LEN + 2;

				}
				else {
					encodeGetdataprimaryPreferredIcElementErrorLen = 0x01;
					encodeGetdataprimaryPreferredIcSeqLen = 0x07;
					primaryPreferredIcErrorEncoded = CommonUtils.formatIntToByte(primaryPreferredIcErr);
					if(logger.isDebugEnabled()){
						logger.debug("primaryPreferredIc Error::"+primaryPreferredIcErr+" encoded as::"+primaryPreferredIcErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_ERR_LEN + 1;
				}				
			}
		}


		//rao Element 
		String rao = null;
		byte encodeGetdataraoSeq  = 0;
		byte encodeGetdataraoSeqLen  = 0;
		byte encodeGetdataraoElementIdTag  = 0;
		byte encodeGetdataraoElementIdLen  = 0;
		byte[] raoEncoded  = null;
		byte encodedraoTag1  = 0;
		byte encodedraoTag2  = 0;
		byte encodeGetdataraoElementDataTag1  = 0;
		byte encodeGetdataraoElementDataTag2  = 0;
		byte encodeGetdataraoElementDataLen  = 0;
		int raoErr = 0;
		byte encodeGetdataraoElementErrorTag = 0;
		byte encodeGetdataraoElementErrorLen = 0;
		byte[] raoErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_RAO) != null) {
			rao = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_RAO);
			if(logger.isDebugEnabled()){
				logger.debug("rao element is present, value:: "+rao);
			}

			if(!StringUtils.isNotBlank(rao)){
				rao = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_RAO_ERR) != null) {
			raoErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_RAO_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("rao element Error is present, value:: "+raoErr);
			}
		}

		if ( (rao != null && !rao.isEmpty()) || raoErr != 0) {
			//SEQUENCE of rao
			encodeGetdataraoSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			//SEQUENCE LENGTHof rao
			encodeGetdataraoSeqLen = GetDataConstants.GETDATA_RAO_SEQ_LEN;

			//rao LIDBElementIdentifier TAG
			encodeGetdataraoElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			//rao LIDBElementIdentifier LENGTH
			encodeGetdataraoElementIdLen = GetDataConstants.GETDATA_RAO_TAG_LEN;
			encodedraoTag1 = 0;
			encodedraoTag2 = 0;
			//rao LIDBElementIdentifier VALUE. i.e rao identifier
			encodedraoTag1 = GetDataConstants.GETDATA_RAO_TAG1;
			encodedraoTag2 = GetDataConstants.GETDATA_RAO_TAG2;
			if ( rao != null && !rao.isEmpty()) {
				//LIDBElementData TAG 
				encodeGetdataraoElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_1;
				encodeGetdataraoElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_BCD_DIGITS_2;
				//LIDBElementData LENGTH (length of rao)
				encodeGetdataraoElementDataLen = GetDataConstants.GETDATA_LIDB_ELEMENT_DATA_BCD_DIG;
				//LIDBElementData VALUE . i.e Account Owner value
				raoEncoded = encodeAdrsSignalForGetQuery(rao);
				logger.debug("rao::"+rao+" encoded as::"+raoEncoded);
				int raolen = raoEncoded.length;
				totalLength += GetDataConstants.GETDATA_RAO_LEN_OCTET + raolen;
			}
			else if (raoErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataraoElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (raoErr != 6 ) {
					encodeGetdataraoElementErrorLen = 0x02;
					encodeGetdataraoSeqLen = 0x08;
					//LIDBElementError VALUE
					raoErrorEncoded = CommonUtils.formatIntToByte(raoErr);
					if(logger.isDebugEnabled()){
						logger.debug("rao Error::"+raoErr+" encoded as::"+raoErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_RAO_ERR_LEN + 2;

				}
				else {
					encodeGetdataraoElementErrorLen = 0x01;
					encodeGetdataraoSeqLen = 0x07;
					raoErrorEncoded = CommonUtils.formatIntToByte(raoErr);
					if(logger.isDebugEnabled()){
						logger.debug("rao Error::"+raoErr+" encoded as::"+raoErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_RAO_ERR_LEN + 1;
				}				
			}
		}




		//recordStatusIndicator
		String recordStatusIndicator = null;
		byte encodeGetdatarecordStatusIndicatorSeq = 0;
		byte encodeGetdatarecordStatusIndicatorSeqLen = 0;
		byte encodeGetdatarecordStatusIndicatorElementIdTag = 0;
		byte encodeGetdatarecordStatusIndicatorElementIdLen = 0;
		byte[] recordStatusIndicatorEncoded = null;
		byte encodedrecordStatusIndicatorTag = 0;
		byte encodeGetdatarecordStatusIndicatorElementDataTag = 0;
		byte encodeGetdatarecordStatusIndicatorElementDataLen = 0;
		int recordStatusIndicatorErr = 0;
		byte encodeGetdatarecordStatusIndicatorElementErrorTag = 0;
		byte encodeGetdatarecordStatusIndicatorElementErrorLen = 0;
		byte[] recordStatusIndicatorErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_RECORD_STATUS_INDICATOR) != null) {
			recordStatusIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_RECORD_STATUS_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("recordStatusIndicator element is present, value:: "+recordStatusIndicator);
			}

			if(!StringUtils.isNotBlank(recordStatusIndicator)){
				recordStatusIndicator = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_RECORD_STATUS_INDICATOR_ERR) != null) {
			if(logger.isDebugEnabled()){
				logger.debug("recordStatusIndicator element Error is present, value:: "+recordStatusIndicatorErr);
			}
			recordStatusIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_RECORD_STATUS_INDICATOR_ERR);
		}

		if ( (recordStatusIndicator != null && !recordStatusIndicator.isEmpty()) || recordStatusIndicatorErr != 0) {
			encodeGetdatarecordStatusIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatarecordStatusIndicatorSeqLen = GetDataConstants.GETDATA_RECORD_STATUS_INDICATOR_SEQ_LEN;
			encodeGetdatarecordStatusIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatarecordStatusIndicatorElementIdLen = GetDataConstants.GETDATA_RECORD_STATUS_INDICATOR_TAG_LEN;
			encodedrecordStatusIndicatorTag = GetDataConstants.GETDATA_RECORD_STATUS_INDICATOR_TAG;
			if ( recordStatusIndicator != null && !recordStatusIndicator.isEmpty()) {
				encodeGetdatarecordStatusIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdatarecordStatusIndicatorElementDataLen = GetDataConstants.GETDATA_RECORD_STATUS_INDICATOR_LENGTH;
				//recordStatusIndicatorEncoded = asciToHex(recordStatusIndicator);
				int recordStatusIndicatorInt = Integer.parseInt(recordStatusIndicator);
				recordStatusIndicatorEncoded = CommonUtils.formatIntToByte(recordStatusIndicatorInt);
				logger.debug("recordStatusIndicator::"+recordStatusIndicator+" encoded as::"+recordStatusIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_RECORD_STATUS_INDICATOR_LEN_OCTET;
			}
			else if (recordStatusIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatarecordStatusIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (recordStatusIndicatorErr != 6 ) {
					encodeGetdatarecordStatusIndicatorElementErrorLen = 0x02;
					encodeGetdatarecordStatusIndicatorSeqLen = 0x07;
					//LIDBElementError VALUE
					recordStatusIndicatorErrorEncoded = CommonUtils.formatIntToByte(recordStatusIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("recordStatusIndicator Error::"+recordStatusIndicatorErr+" encoded as::"+recordStatusIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_RECORD_STATUS_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdatarecordStatusIndicatorElementErrorLen = 0x01;
					encodeGetdatarecordStatusIndicatorSeqLen = 0x06;
					recordStatusIndicatorErrorEncoded = CommonUtils.formatIntToByte(recordStatusIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("recordStatusIndicator Error::"+recordStatusIndicatorErr+" encoded as::"+recordStatusIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_RECORD_STATUS_INDICATOR_ERR_LEN + 1;
				}				
			}
		}


		//serviceOrEquipmentIndicator
		String serviceOrEquipmentIndicator = null;
		byte encodeGetdataserviceOrEquipmentIndicatorSeq = 0;
		byte encodeGetdataserviceOrEquipmentIndicatorSeqLen = 0;
		byte encodeGetdataserviceOrEquipmentIndicatorElementIdTag = 0;
		byte encodeGetdataserviceOrEquipmentIndicatorElementIdLen = 0;
		byte[] serviceOrEquipmentIndicatorEncoded = null;
		byte encodedserviceOrEquipmentIndicatorTag = 0;
		byte encodeGetdataserviceOrEquipmentIndicatorElementDataTag = 0;
		byte encodeGetdataserviceOrEquipmentIndicatorElementDataLen = 0;
		int serviceOrEquipmentIndicatorErr = 0;
		byte encodeGetdataserviceOrEquipmentIndicatorElementErrorTag = 0;
		byte encodeGetdataserviceOrEquipmentIndicatorElementErrorLen = 0;
		byte[] serviceOrEquipmentIndicatorErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_OR_EQUIPMENT_INDICATOR) != null) {
			serviceOrEquipmentIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_OR_EQUIPMENT_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("serviceOrEquipmentIndicator element is present, value:: "+serviceOrEquipmentIndicator);
			}

			if(!StringUtils.isNotBlank(serviceOrEquipmentIndicator)){
				serviceOrEquipmentIndicator = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_OR_EQUIPMENT_INDICATOR_ERR) != null) {
			serviceOrEquipmentIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_OR_EQUIPMENT_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("serviceOrEquipmentIndicator element Error is present, value:: "+serviceOrEquipmentIndicatorErr);
			}
		}

		if ( (serviceOrEquipmentIndicator != null && !serviceOrEquipmentIndicator.isEmpty()) || serviceOrEquipmentIndicatorErr != 0) {
			encodeGetdataserviceOrEquipmentIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdataserviceOrEquipmentIndicatorSeqLen = GetDataConstants.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_SEQ_LEN;
			encodeGetdataserviceOrEquipmentIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdataserviceOrEquipmentIndicatorElementIdLen = GetDataConstants.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_TAG_LEN;
			encodedserviceOrEquipmentIndicatorTag = GetDataConstants.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_TAG;
			if ( serviceOrEquipmentIndicator != null && !serviceOrEquipmentIndicator.isEmpty()) {
				encodeGetdataserviceOrEquipmentIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdataserviceOrEquipmentIndicatorElementDataLen = GetDataConstants.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_LENGTH;
				//serviceOrEquipmentIndicatorEncoded = asciToHex(serviceOrEquipmentIndicator);
				int serviceOrEquipmentIndicatorInt = Integer.parseInt(serviceOrEquipmentIndicator);
				serviceOrEquipmentIndicatorEncoded = CommonUtils.formatIntToByte(serviceOrEquipmentIndicatorInt);
				logger.debug("serviceOrEquipmentIndicator::"+serviceOrEquipmentIndicator+" encoded as::"+serviceOrEquipmentIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_LEN_OCTET;
			}
			else if (serviceOrEquipmentIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdataserviceOrEquipmentIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (serviceOrEquipmentIndicatorErr != 6 ) {
					encodeGetdataserviceOrEquipmentIndicatorElementErrorLen = 0x02;
					encodeGetdataserviceOrEquipmentIndicatorSeqLen = 0x07;
					//LIDBElementError VALUE
					serviceOrEquipmentIndicatorErrorEncoded = CommonUtils.formatIntToByte(serviceOrEquipmentIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("serviceOrEquipmentIndicator Error::"+serviceOrEquipmentIndicatorErr+" encoded as::"+serviceOrEquipmentIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdataserviceOrEquipmentIndicatorElementErrorLen = 0x01;
					encodeGetdataserviceOrEquipmentIndicatorSeqLen = 0x06;
					serviceOrEquipmentIndicatorErrorEncoded = CommonUtils.formatIntToByte(serviceOrEquipmentIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("serviceOrEquipmentIndicator Error::"+serviceOrEquipmentIndicatorErr+" encoded as::"+serviceOrEquipmentIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR_ERR_LEN + 1;
				}				
			}
		}


		//thirdNumberAcceptanceIndicator
		String thirdNumberAcceptanceIndicator = null;
		byte encodeGetdatathirdNumberAcceptanceIndicatorSeq = 0;
		byte encodeGetdatathirdNumberAcceptanceIndicatorSeqLen = 0;
		byte encodeGetdatathirdNumberAcceptanceIndicatorElementIdTag = 0;
		byte encodeGetdatathirdNumberAcceptanceIndicatorElementIdLen = 0;
		byte[] thirdNumberAcceptanceIndicatorEncoded = null;
		byte encodedthirdNumberAcceptanceIndicatorTag = 0;
		byte encodeGetdatathirdNumberAcceptanceIndicatorElementDataTag = 0;
		byte encodeGetdatathirdNumberAcceptanceIndicatorElementDataLen = 0;
		int thirdNumberAcceptanceIndicatorErr = 0;
		byte encodeGetdatathirdNumberAcceptanceIndicatorElementErrorTag = 0;
		byte encodeGetdatathirdNumberAcceptanceIndicatorElementErrorLen = 0;
		byte[] thirdNumberAcceptanceIndicatorErrorEncoded = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_THIRD_NUMBER_ACCEPTANCE_INDICATOR) != null) {
			thirdNumberAcceptanceIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_THIRD_NUMBER_ACCEPTANCE_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("thirdNumberAcceptanceIndicator element is present, value:: "+thirdNumberAcceptanceIndicator);
			}

			if(!StringUtils.isNotBlank(thirdNumberAcceptanceIndicator)){
				thirdNumberAcceptanceIndicator = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_THIRD_NUMBER_ACCEPTANCE_INDICATOR_ERR) != null) {
			thirdNumberAcceptanceIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_THIRD_NUMBER_ACCEPTANCE_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("thirdNumberAcceptanceIndicator element Error is present, value:: "+thirdNumberAcceptanceIndicatorErr);
			}
		}
		if ( (thirdNumberAcceptanceIndicator != null && !thirdNumberAcceptanceIndicator.isEmpty()) || thirdNumberAcceptanceIndicatorErr != 0) {
			encodeGetdatathirdNumberAcceptanceIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatathirdNumberAcceptanceIndicatorSeqLen = GetDataConstants.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_SEQ_LEN;
			encodeGetdatathirdNumberAcceptanceIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatathirdNumberAcceptanceIndicatorElementIdLen = GetDataConstants.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_TAG_LEN;
			encodedthirdNumberAcceptanceIndicatorTag = GetDataConstants.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_TAG;
			if ( thirdNumberAcceptanceIndicator != null && !thirdNumberAcceptanceIndicator.isEmpty()) {
				encodeGetdatathirdNumberAcceptanceIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdatathirdNumberAcceptanceIndicatorElementDataLen = GetDataConstants.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_LENGTH;
				//thirdNumberAcceptanceIndicatorEncoded = asciToHex(thirdNumberAcceptanceIndicator);
				int thirdNumberAcceptanceIndicatorInt = Integer.parseInt(thirdNumberAcceptanceIndicator);
				thirdNumberAcceptanceIndicatorEncoded = CommonUtils.formatIntToByte(thirdNumberAcceptanceIndicatorInt);
				logger.debug("thirdNumberAcceptanceIndicator::"+thirdNumberAcceptanceIndicator+" encoded as::"+thirdNumberAcceptanceIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_LEN_OCTET;
			}
			else if (thirdNumberAcceptanceIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatathirdNumberAcceptanceIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (thirdNumberAcceptanceIndicatorErr != 6 ) {
					encodeGetdatathirdNumberAcceptanceIndicatorElementErrorLen = 0x02;
					encodeGetdatathirdNumberAcceptanceIndicatorSeqLen = 0x07;
					//LIDBElementError VALUE
					thirdNumberAcceptanceIndicatorErrorEncoded = CommonUtils.formatIntToByte(thirdNumberAcceptanceIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("thirdNumberAcceptanceIndicator Error::"+thirdNumberAcceptanceIndicatorErr+" encoded as::"+thirdNumberAcceptanceIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdatathirdNumberAcceptanceIndicatorElementErrorLen = 0x01;
					encodeGetdatathirdNumberAcceptanceIndicatorSeqLen = 0x06;
					thirdNumberAcceptanceIndicatorErrorEncoded = CommonUtils.formatIntToByte(thirdNumberAcceptanceIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("thirdNumberAcceptanceIndicator Error::"+thirdNumberAcceptanceIndicatorErr+" encoded as::"+thirdNumberAcceptanceIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		//treatmentIndicator
		String treatmentIndicator = null;
		byte encodeGetdatatreatmentIndicatorSeq = 0;
		byte encodeGetdatatreatmentIndicatorSeqLen = 0;
		byte encodeGetdatatreatmentIndicatorElementIdTag = 0;
		byte encodeGetdatatreatmentIndicatorElementIdLen = 0;
		byte[] treatmentIndicatorEncoded = null;
		byte encodedtreatmentIndicatorTag = 0;
		byte encodeGetdatatreatmentIndicatorElementDataTag = 0;
		byte encodeGetdatatreatmentIndicatorElementDataLen = 0;
		int treatmentIndicatorErr = 0;
		byte encodeGetdatatreatmentIndicatorElementErrorTag = 0;
		byte encodeGetdatatreatmentIndicatorElementErrorLen = 0;
		byte[] treatmentIndicatorErrorEncoded = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_TREATMENT_INDICATOR) != null) {
			treatmentIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_TREATMENT_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("treatmentIndicator element is present, value:: "+treatmentIndicator);
			}

			if(!StringUtils.isNotBlank(treatmentIndicator)){
				treatmentIndicator = null;
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_TREATMENT_INDICATOR_ERR) != null) {
			treatmentIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_TREATMENT_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("treatmentIndicator element Error is present, value:: "+treatmentIndicatorErr);
			}
		}
		if ( (treatmentIndicator != null && !treatmentIndicator.isEmpty()) || treatmentIndicatorErr != 0) {
			encodeGetdatatreatmentIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatatreatmentIndicatorSeqLen = GetDataConstants.GETDATA_TREATMENT_INDICATOR_SEQ_LEN;
			encodeGetdatatreatmentIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatatreatmentIndicatorElementIdLen = GetDataConstants.GETDATA_TREATMENT_INDICATOR_TAG_LEN;
			encodedtreatmentIndicatorTag = GetDataConstants.GETDATA_TREATMENT_INDICATOR_TAG;
			if ( treatmentIndicator != null && !treatmentIndicator.isEmpty()) {
				encodeGetdatatreatmentIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_INTEGER;
				encodeGetdatatreatmentIndicatorElementDataLen = GetDataConstants.GETDATA_TREATMENT_INDICATOR_LENGTH;
				//treatmentIndicatorEncoded = asciToHex(treatmentIndicator);
				int treatmentIndicatorInt = Integer.parseInt(treatmentIndicator);
				treatmentIndicatorEncoded = CommonUtils.formatIntToByte(treatmentIndicatorInt);
				logger.debug("treatmentIndicator::"+treatmentIndicator+" encoded as::"+treatmentIndicatorEncoded);
				totalLength += GetDataConstants.GETDATA_TREATMENT_INDICATOR_LEN_OCTET;
			}
			else if (treatmentIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatatreatmentIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (treatmentIndicatorErr != 6 ) {
					encodeGetdatatreatmentIndicatorElementErrorLen = 0x02;
					encodeGetdatatreatmentIndicatorSeqLen = 0x07;
					//LIDBElementError VALUE
					treatmentIndicatorErrorEncoded = CommonUtils.formatIntToByte(treatmentIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("treatmentIndicator Error::"+treatmentIndicatorErr+" encoded as::"+treatmentIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_TREATMENT_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdatatreatmentIndicatorElementErrorLen = 0x01;
					encodeGetdatatreatmentIndicatorSeqLen = 0x06;
					treatmentIndicatorErrorEncoded = CommonUtils.formatIntToByte(treatmentIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("treatmentIndicator Error::"+treatmentIndicatorErr+" encoded as::"+treatmentIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_TREATMENT_INDICATOR_ERR_LEN + 1;
				}				
			}
		}


		//referralNumber
		PhoneNumber referralNumber = null;
		byte encodeGetdatareferralNumberSeq =  0;
		byte encodeGetdatareferralNumberSeqLen =  0;
		byte encodeGetdatareferralNumberElementIdTag =  0;
		byte encodeGetdatareferralNumberElementIdLen =  0;
		byte encodedreferralNumberTag1 =  0;
		byte encodedreferralNumberTag2 =  0;
		byte encodeGetdatareferralNumberElementDataTag1 =  0;
		byte encodeGetdatareferralNumberElementDataTag2 =  0;
		byte encodeGetdatareferralNumberElementDataLen =  0;
		byte[] referralNumberEncoded =  null;
		int referralNumberErr = 0;
		byte encodeGetdatareferralNumberElementErrorTag = 0;
		byte encodeGetdatareferralNumberElementErrorLen = 0;
		byte[] referralNumberErrorEncoded = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_REFERRAL_NUMBER) != null) {
			referralNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GETDATA_O_REFERRAL_NUMBER);
			if(logger.isDebugEnabled()){
				logger.debug("referralNumber element is present, value:: "+referralNumber);
			}

			if(!StringUtils.isNotBlank(referralNumber.getAddress())){
				referralNumber = null;
			}else{
				if (leg2Data.get(LegDataAttributes.P_GETDATA_O_REF_NUM_TYPE) == null) {
					logger.error("encodeGetDataQuery:Referral number type is missing");
					throw new AINCodecException("parameter Referral number type is missing");
				}
				int refNumType = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_REF_NUM_TYPE);
				// National presentation allowed - 0
				// International presentation allowed - 1
				referralNumber.setNatureOfAddress(refNumType);
			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_REFERRAL_NUMBER_ERR) != null) {
			referralNumberErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_REFERRAL_NUMBER_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("referralNumber element Error is present, value:: "+referralNumberErr);
			}
		}
		if ( referralNumber != null || referralNumberErr != 0) {
			encodeGetdatareferralNumberSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatareferralNumberSeqLen = 0;
			encodeGetdatareferralNumberElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatareferralNumberElementIdLen = GetDataConstants.GETDATA_REFERRAL_NUMBER_TAG_LEN;
			encodedreferralNumberTag1 = GetDataConstants.GETDATA_REFERRAL_NUMBER_TAG1;
			encodedreferralNumberTag2 = GetDataConstants.GETDATA_REFERRAL_NUMBER_TAG2;
			if ( referralNumber != null ) {
				//				if(referralNumber.getAddress().length() > 10){
				//					referralNumber.setNatureOfAddress(1);
				//				}else if(referralNumber.getAddress().length() <= 10){
				//					referralNumber.setNatureOfAddress(0);
				//				}
				encodeGetdatareferralNumberElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_LIDB_DIGITS_1;
				encodeGetdatareferralNumberElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_LIDB_DIGITS_2;
				referralNumberEncoded = encodeAdrsSignalForGetQuery(referralNumber.getAddress());
				logger.debug("referralNumber::"+referralNumber+" encoded as::"+referralNumberEncoded);
				int referralNumberSeqLen = referralNumberEncoded.length + 11;
				encodeGetdatareferralNumberSeqLen = (byte) referralNumberSeqLen ;
				encodeGetdatareferralNumberElementDataLen = (byte) referralNumberEncoded.length;
				totalLength += referralNumberEncoded.length + 13;
			}
			else if (referralNumberErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatareferralNumberElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (referralNumberErr != 6 ) {
					encodeGetdatareferralNumberElementErrorLen = 0x02;
					encodeGetdatareferralNumberSeqLen = 0x08;
					//LIDBElementError VALUE
					referralNumberErrorEncoded = CommonUtils.formatIntToByte(referralNumberErr);
					if(logger.isDebugEnabled()){
						logger.debug("referralNumber Error::"+referralNumberErr+" encoded as::"+referralNumberErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_REFERRAL_NUMBER_ERR_LEN + 2;

				}
				else {
					encodeGetdatareferralNumberElementErrorLen = 0x01;
					encodeGetdatareferralNumberSeqLen = 0x07;
					referralNumberErrorEncoded = CommonUtils.formatIntToByte(referralNumberErr);
					if(logger.isDebugEnabled()){
						logger.debug("referralNumber Error::"+referralNumberErr+" encoded as::"+referralNumberErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_REFERRAL_NUMBER_ERR_LEN + 1;

				}	

			}
		}


		//trueBillingNumber
		String trueBillingNumber = null;
		byte encodeGetdatatrueBillingNumberSeq =  0;
		byte encodeGetdatatrueBillingNumberSeqLen =  0;
		byte encodeGetdatatrueBillingNumberElementIdTag =  0;
		byte encodeGetdatatrueBillingNumberElementIdLen =  0;
		byte encodedtrueBillingNumberTag =  0;
		byte encodeGetdatatrueBillingNumberElementDataTag1 =  0;
		byte encodeGetdatatrueBillingNumberElementDataTag2 =  0;
		byte encodeGetdatatrueBillingNumberElementDataLen =  0;
		byte[] trueBillingNumberEncoded =  null;
		int trueBillingNumberErr = 0;
		byte encodeGetdatatrueBillingNumberElementErrorTag = 0;
		byte encodeGetdatatrueBillingNumberElementErrorLen = 0;
		byte[] trueBillingNumberErrorEncoded = null;
		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_TRUE_BILLING_NUMBER) != null) {
			trueBillingNumber = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_TRUE_BILLING_NUMBER);
			if(logger.isDebugEnabled()){
				logger.debug("trueBillingNumber element is present, value:: "+trueBillingNumber);
			}

			//			if(!StringUtils.isNotBlank(trueBillingNumber.getAddress())){
			//				trueBillingNumber = null;
			//			}
		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_TRUE_BILLING_NUMBER_ERR) != null) {
			trueBillingNumberErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_TRUE_BILLING_NUMBER_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("trueBillingNumber element Error is present, value:: "+trueBillingNumberErr);
			}
		}
		int trueBillingNumberNature=0;
		if ( (trueBillingNumber != null && trueBillingNumber.isEmpty()) || trueBillingNumberErr != 0) {

			encodeGetdatatrueBillingNumberSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatatrueBillingNumberSeqLen = 0;
			encodeGetdatatrueBillingNumberElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatatrueBillingNumberElementIdLen = GetDataConstants.GETDATA_TRUE_BILLING_NUMBER_TAG_LEN;
			encodedtrueBillingNumberTag = GetDataConstants.GETDATA_TRUE_BILLING_NUMBER_TAG;
			if ( trueBillingNumber != null && !trueBillingNumber.isEmpty()) {
				if(trueBillingNumber.length() > 10){
					trueBillingNumberNature=1;
				}else if(trueBillingNumber.length() <= 10){
					trueBillingNumberNature=0;
				}
				encodeGetdatatrueBillingNumberElementDataTag1 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_LIDB_DIGITS_1;
				encodeGetdatatrueBillingNumberElementDataTag2 = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_LIDB_DIGITS_2;
				trueBillingNumberEncoded = encodeAdrsSignalForGetQuery(trueBillingNumber);//.getAddress());
				logger.debug("trueBillingNumber::"+trueBillingNumber+" encoded as::"+trueBillingNumberEncoded);
				int trueBillingNumberSeqLen = trueBillingNumberEncoded.length + 6;
				encodeGetdatatrueBillingNumberSeqLen = (byte) trueBillingNumberSeqLen ;
				encodeGetdatatrueBillingNumberElementDataLen = (byte) trueBillingNumberEncoded.length;
				totalLength += trueBillingNumberEncoded.length + 12;
			}
			else if (trueBillingNumberErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatatrueBillingNumberElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (trueBillingNumberErr != 6 ) {
					encodeGetdatatrueBillingNumberElementErrorLen = 0x02;
					encodeGetdatatrueBillingNumberSeqLen = 0x07;
					//LIDBElementError VALUE
					trueBillingNumberErrorEncoded = CommonUtils.formatIntToByte(trueBillingNumberErr);
					if(logger.isDebugEnabled()){
						logger.debug("trueBillingNumber Error::"+trueBillingNumberErr+" encoded as::"+trueBillingNumberErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_TRUE_BILLING_NUMBER_ERR_LEN + 2;

				}
				else {
					encodeGetdatatrueBillingNumberElementErrorLen = 0x01;
					encodeGetdatatrueBillingNumberSeqLen = 0x06;
					trueBillingNumberErrorEncoded = CommonUtils.formatIntToByte(trueBillingNumberErr);
					if(logger.isDebugEnabled()){
						logger.debug("trueBillingNumber Error::"+trueBillingNumberErr+" encoded as::"+trueBillingNumberErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_TRUE_BILLING_NUMBER_ERR_LEN + 1;
				}				
			}
		}


		//wirelessServicesOrigIndicator
		String wirelessServicesOrigIndicator = null;
		byte encodeGetdatawirelessServicesOrigIndicatorSeq =  0;
		byte encodeGetdatawirelessServicesOrigIndicatorSeqLen =  0;
		byte encodeGetdatawirelessServicesOrigIndicatorElementIdTag =  0;
		byte encodeGetdatawirelessServicesOrigIndicatorElementIdLen =  0;
		byte encodedwirelessServicesOrigIndicatorTag1 =  0;
		byte encodedwirelessServicesOrigIndicatorTag2 =  0;
		byte encodeGetdatawirelessServicesOrigIndicatorElementDataTag =  0;
		byte encodeGetdatawirelessServicesOrigIndicatorElementDataLen =  0;
		byte[] wirelessServicesOrigIndicatorEncoded =  null;
		int wirelessServicesOrigIndicatorErr = 0;
		byte encodeGetdatawirelessServicesOrigIndicatorElementErrorTag = 0;
		byte encodeGetdatawirelessServicesOrigIndicatorElementErrorLen = 0;
		byte[] wirelessServicesOrigIndicatorErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_ORIG_INDICATOR) != null) {
			wirelessServicesOrigIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_ORIG_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("wirelessServicesOrigIndicator element is present, value:: "+wirelessServicesOrigIndicator);
			}

		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_ORIG_INDICATOR_ERR) != null) {
			wirelessServicesOrigIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_ORIG_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("wirelessServicesOrigIndicator element Error is present, value:: "+wirelessServicesOrigIndicatorErr);
			}
		}


		if ( (wirelessServicesOrigIndicator != null && !wirelessServicesOrigIndicator.isEmpty()) ||
				wirelessServicesOrigIndicatorErr != 0) {
			encodeGetdatawirelessServicesOrigIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatawirelessServicesOrigIndicatorSeqLen = GetDataConstants.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_SEQ_LEN;
			encodeGetdatawirelessServicesOrigIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatawirelessServicesOrigIndicatorElementIdLen = GetDataConstants.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_TAG_LEN;
			encodedwirelessServicesOrigIndicatorTag1 = GetDataConstants.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_TAG1;
			encodedwirelessServicesOrigIndicatorTag2 = GetDataConstants.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_TAG2;
			if ( wirelessServicesOrigIndicator != null && !wirelessServicesOrigIndicator.isEmpty()) {
				encodeGetdatawirelessServicesOrigIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_OCTET;
				wirelessServicesOrigIndicatorEncoded = asciToHex(wirelessServicesOrigIndicator);
				logger.debug("wirelessServicesOrigIndicator::"+wirelessServicesOrigIndicator+" encoded as::"+wirelessServicesOrigIndicatorEncoded);
				encodeGetdatawirelessServicesOrigIndicatorElementDataLen = (byte) wirelessServicesOrigIndicatorEncoded.length;
				totalLength += GetDataConstants.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_LEN_OCTET;
			}
			else if (wirelessServicesOrigIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatawirelessServicesOrigIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (wirelessServicesOrigIndicatorErr != 6 ) {
					encodeGetdatawirelessServicesOrigIndicatorElementErrorLen = 0x02;
					encodeGetdatawirelessServicesOrigIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					wirelessServicesOrigIndicatorErrorEncoded = CommonUtils.formatIntToByte(wirelessServicesOrigIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("wirelessServicesOrigIndicator Error::"+wirelessServicesOrigIndicatorErr+" encoded as::"+wirelessServicesOrigIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdatawirelessServicesOrigIndicatorElementErrorLen = 0x01;
					encodeGetdatawirelessServicesOrigIndicatorSeqLen = 0x07;
					wirelessServicesOrigIndicatorErrorEncoded = CommonUtils.formatIntToByte(wirelessServicesOrigIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("wirelessServicesOrigIndicator Error::"+wirelessServicesOrigIndicatorErr+" encoded as::"+wirelessServicesOrigIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR_ERR_LEN + 1;
				}				
			}
		}


		//wirelessServicesTerminatingIndicator
		String wirelessServicesTerminatingIndicator = null;
		byte encodeGetdatawirelessServicesTerminatingIndicatorSeq =  0;
		byte encodeGetdatawirelessServicesTerminatingIndicatorSeqLen =  0;
		byte encodeGetdatawirelessServicesTerminatingIndicatorElementIdTag =  0;
		byte encodeGetdatawirelessServicesTerminatingIndicatorElementIdLen =  0;
		byte encodedwirelessServicesTerminatingIndicatorTag1 =  0;
		byte encodedwirelessServicesTerminatingIndicatorTag2 =  0;
		byte encodeGetdatawirelessServicesTerminatingIndicatorElementDataTag =  0;
		byte encodeGetdatawirelessServicesTerminatingIndicatorElementDataLen =  0;
		byte[] wirelessServicesTerminatingIndicatorEncoded =  null;
		int wirelessServicesTerminatingIndicatorErr = 0;
		byte encodeGetdatawirelessServicesTerminatingIndicatorElementErrorTag = 0;
		byte encodeGetdatawirelessServicesTerminatingIndicatorElementErrorLen = 0;
		byte[] wirelessServicesTerminatingIndicatorErrorEncoded = null;

		if (leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_TERMINATING_INDICATOR) != null) {
			wirelessServicesTerminatingIndicator = (String) leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_TERMINATING_INDICATOR);
			if(logger.isDebugEnabled()){
				logger.debug("wirelessServicesTerminatingIndicator element is present, value:: "+wirelessServicesTerminatingIndicator);
			}

		}
		else if (leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_TERMINATING_INDICATOR_ERR) != null) {
			wirelessServicesTerminatingIndicatorErr = (Integer) leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_TERMINATING_INDICATOR_ERR);
			if(logger.isDebugEnabled()){
				logger.debug("wirelessServicesTerminatingIndicator element Error is present, value:: "+wirelessServicesTerminatingIndicatorErr);
			}
		}

		if ( (wirelessServicesTerminatingIndicator != null && !wirelessServicesTerminatingIndicator.isEmpty()) 
				|| wirelessServicesTerminatingIndicatorErr != 0) {
			encodeGetdatawirelessServicesTerminatingIndicatorSeq = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
			encodeGetdatawirelessServicesTerminatingIndicatorSeqLen = GetDataConstants.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_SEQ_LEN;
			encodeGetdatawirelessServicesTerminatingIndicatorElementIdTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
			encodeGetdatawirelessServicesTerminatingIndicatorElementIdLen = GetDataConstants.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_TAG_LEN;
			encodedwirelessServicesTerminatingIndicatorTag1 = GetDataConstants.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_TAG1;
			encodedwirelessServicesTerminatingIndicatorTag2 = GetDataConstants.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_TAG2;
			if ( wirelessServicesTerminatingIndicator != null && !wirelessServicesTerminatingIndicator.isEmpty()) {
				encodeGetdatawirelessServicesTerminatingIndicatorElementDataTag = GetDataConstants.GETDATA_ELEMENT_DATA_TAG_OCTET;
				wirelessServicesTerminatingIndicatorEncoded = asciToHex(wirelessServicesTerminatingIndicator);
				logger.debug("wirelessServicesTerminatingIndicator::"+wirelessServicesTerminatingIndicator+" encoded as::"+wirelessServicesTerminatingIndicatorEncoded);
				encodeGetdatawirelessServicesTerminatingIndicatorElementDataLen = (byte) wirelessServicesTerminatingIndicatorEncoded.length;
				totalLength += GetDataConstants.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_LEN_OCTET;
			}
			else if (wirelessServicesTerminatingIndicatorErr != 0 ) {
				//LIDBElementError TAG 
				encodeGetdatawirelessServicesTerminatingIndicatorElementErrorTag = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
				//LIDBElementError LENGTH (length of Error)
				if (wirelessServicesTerminatingIndicatorErr != 6 ) {
					encodeGetdatawirelessServicesTerminatingIndicatorElementErrorLen = 0x02;
					encodeGetdatawirelessServicesTerminatingIndicatorSeqLen = 0x08;
					//LIDBElementError VALUE
					wirelessServicesTerminatingIndicatorErrorEncoded = CommonUtils.formatIntToByte(wirelessServicesTerminatingIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("wirelessServicesTerminatingIndicator Error::"+wirelessServicesTerminatingIndicatorErr+" encoded as::"+wirelessServicesTerminatingIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_ERR_LEN + 2;

				}
				else {
					encodeGetdatawirelessServicesTerminatingIndicatorElementErrorLen = 0x01;
					encodeGetdatawirelessServicesTerminatingIndicatorSeqLen = 0x07;
					wirelessServicesTerminatingIndicatorErrorEncoded = CommonUtils.formatIntToByte(wirelessServicesTerminatingIndicatorErr);
					if(logger.isDebugEnabled()){
						logger.debug("wirelessServicesTerminatingIndicator Error::"+wirelessServicesTerminatingIndicatorErr+" encoded as::"+wirelessServicesTerminatingIndicatorErrorEncoded);
					}
					totalLength += GetDataConstants.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR_ERR_LEN + 1;
				}				
			}
		}

		// calculating the length of invalid TCAP IDs response bytes for the received invalid TCAP IDs
		if (legData.get(LegDataAttributes.GETDATA_INVALID_TCAPID_LIST) != null) {
			String tempInValidTcapIds = (String) legData.get(LegDataAttributes.GETDATA_INVALID_TCAPID_LIST);
			String[] strInvalidTcapIds = tempInValidTcapIds.split(" ");
			int invalidTcapIdIntValue = 0;
			for (String tcpId : strInvalidTcapIds) {
				invalidTcapIdIntValue = Integer.parseInt(tcpId);
				InvalidTcapIdsList.add(invalidTcapIdIntValue);
				if (invalidTcapIdIntValue < 128) {
					totalLength += 9;
				}
				else {
					totalLength += 10;
				}
					
			}

		}



		// OUTPUT ARRAY
		String[] parameterSetLength = convertHexRepersentaion(Integer.toHexString(totalLength));
		byte[] outputArray = new byte[totalLength + 2]; //total length + parameter set identifier and parameter set length

		outputArray[index++] = encodedParameterSetIdentifier;
		outputArray[index++] = (byte) Byte.decode(parameterSetLength[0]);

		outputArray[index++] = encodedDataElementBlockTag1;
		outputArray[index++] = encodedDataElementBlockTag2;
		lidbDataElementBlockLength = totalLength - 3; // total length - LIDBDataElementBlock, LIDBDataElementBlock length
		String[] DataElementBlockLength = convertHexRepersentaion(Integer.toHexString(lidbDataElementBlockLength));
		outputArray[index++] = (byte) Byte.decode(DataElementBlockLength[0]);

		// Account Owner
		if (accountOwnerEncoded != null) {
			outputArray[index++] = encodeGetdataAccountOwnerSeq;
			outputArray[index++] = encodeGetdataAccountOwnerSeqLen;
			outputArray[index++] = encodeGetdataAccountOwnerElementIdTag;
			outputArray[index++] = encodeGetdataAccountOwnerElementIdLen;
			outputArray[index++] = encodedAccountOwnerTag1;
			outputArray[index++] = encodedAccountOwnerTag2;
			outputArray[index++] = encodeGetdataAccountOwnerElementDataTag;
			outputArray[index++] = encodeGetdataAccountOwnerElementDataLen;
			for (byte encodedVal : accountOwnerEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		else if (accountOwnerErrorEncoded != null) {
			outputArray[index++] = encodeGetdataAccountOwnerSeq;
			outputArray[index++] = encodeGetdataAccountOwnerSeqLen;
			outputArray[index++] = encodeGetdataAccountOwnerElementIdTag;
			outputArray[index++] = encodeGetdataAccountOwnerElementIdLen;
			outputArray[index++] = encodedAccountOwnerTag1;
			outputArray[index++] = encodedAccountOwnerTag2;
			outputArray[index++] = encodeGetdataAccountOwnerElementErrorTag;
			outputArray[index++] = encodeGetdataAccountOwnerElementErrorLen;
			for (byte encodedVal : accountOwnerErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// additionalOrigBillingServiceIndicators collective
		if (additionalOrigBillingServiceIndicatorsEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsSeq;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsElementIdLen;
			outputArray[index++] = encodedadditionalOrigBillingServiceIndicatorsTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsElementDataTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsElementDataLen;
			for (byte encodedVal : additionalOrigBillingServiceIndicatorsEncoded) {
				outputArray[index++] = encodedVal;

			}
		}
		else if (additionalOrigBillingServiceIndicatorsErrorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsSeq;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsElementIdLen;
			outputArray[index++] = encodedadditionalOrigBillingServiceIndicatorsTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsElementErrorTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceIndicatorsElementErrorLen;
			for (byte encodedVal : additionalOrigBillingServiceIndicatorsErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// additionalOrigBillingServiceIndicators individual indicators
		// additionalOrigThirdNumberIndicator
		if (additionalOrigThirdNumberIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigThirdNumberIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigThirdNumberIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorElementDataLen;
			for (byte encodedVal : additionalOrigThirdNumberIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		else if (additionalOrigThirdNumberIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigThirdNumberIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigThirdNumberIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataadditionalOrigThirdNumberIndicatorElementErrorLen;
			for (byte encodedVal : additionalOrigThirdNumberIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// additionalOrigCreditCardIndicator
		if (additionalOrigCreditCardIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigCreditCardIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigCreditCardIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorElementDataLen;
			for (byte encodedVal : additionalOrigCreditCardIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		else if (additionalOrigCreditCardIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigCreditCardIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigCreditCardIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataadditionalOrigCreditCardIndicatorElementErrorLen;
			for (byte encodedVal : additionalOrigCreditCardIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// additionalOrigSpecialBnsIndicator
		if (additionalOrigSpecialBnsIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigSpecialBnsIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigSpecialBnsIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorElementDataLen;
			for (byte encodedVal : additionalOrigSpecialBnsIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		else if (additionalOrigSpecialBnsIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigSpecialBnsIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigSpecialBnsIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataadditionalOrigSpecialBnsIndicatorElementErrorLen;
			for (byte encodedVal : additionalOrigSpecialBnsIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// additionalOrigSentpaidIndicator
		if (additionalOrigSentpaidIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigSentpaidIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigSentpaidIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorElementDataLen;
			for (byte encodedVal : additionalOrigSentpaidIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//additionalOrigSentpaidIndicator Error
		else if (additionalOrigSentpaidIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigSentpaidIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigSentpaidIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataadditionalOrigSentpaidIndicatorElementErrorLen;
			for (byte encodedVal : additionalOrigSentpaidIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// additionalOrigBillingServiceSpareIndicator
		if (additionalOrigBillingServiceSpareIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigBillingServiceSpareIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigBillingServiceSpareIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementDataLen;
			for (byte encodedVal : additionalOrigBillingServiceSpareIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//additionalOrigBillingServiceSpareIndicator Error
		else if (additionalOrigBillingServiceSpareIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeq;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorSeqLen;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementIdLen;
			outputArray[index++] = encodedadditionalOrigBillingServiceSpareIndicatorTag1;
			outputArray[index++] = encodedadditionalOrigBillingServiceSpareIndicatorTag2;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataadditionalOrigBillingServiceSpareIndicatorElementErrorLen;
			for (byte encodedVal : additionalOrigBillingServiceSpareIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}



		// alphanumericString
		if (alphanumericString != null) {
			outputArray[index++] = encodeGetdataalphanumericStringSeq;
			outputArray[index++] = encodeGetdataalphanumericStringSeqLen;
			outputArray[index++] = encodeGetdataalphanumericStringElementIdTag;
			outputArray[index++] = encodeGetdataalphanumericStringElementIdLen;
			outputArray[index++] = encodedalphanumericStringTag;
			outputArray[index++] = encodeGetdataalphanumericStringElementDataTag;
			outputArray[index++] = encodeGetdataalphanumericStringElementDataLen;
			for (byte encodedVal : alphanumericStringEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//alphanumericString Error
		else if (alphanumericStringErrorEncoded != null) {
			outputArray[index++] = encodeGetdataalphanumericStringSeq;
			outputArray[index++] = encodeGetdataalphanumericStringSeqLen;
			outputArray[index++] = encodeGetdataalphanumericStringElementIdTag;
			outputArray[index++] = encodeGetdataalphanumericStringElementIdLen;
			outputArray[index++] = encodedalphanumericStringTag;
			outputArray[index++] = encodeGetdataalphanumericStringElementErrorTag;
			outputArray[index++] = encodeGetdataalphanumericStringElementErrorLen;
			for (byte encodedVal : alphanumericStringErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// alternatePreferredIc
		if (alternatePreferredIc != null) {
			outputArray[index++] = encodeGetdataalternatePreferredIcSeq;
			outputArray[index++] = encodeGetdataalternatePreferredIcSeqLen;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementIdTag;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementIdLen;
			outputArray[index++] = encodedalternatePreferredIcTag1;
			outputArray[index++] = encodedalternatePreferredIcTag2;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementDataTag1;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementDataTag2;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementDataLen;
			outputArray[index++] = GetDataConstants.GETDATA_ALTERNATE_PREFERRED_IC_LENGTH;
			for (byte encodedVal : alternatePreferredIcEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//alternatePreferredIc Error
		else if (alternatePreferredIcErrorEncoded != null) {
			outputArray[index++] = encodeGetdataalternatePreferredIcSeq;
			outputArray[index++] = encodeGetdataalternatePreferredIcSeqLen;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementIdTag;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementIdLen;
			outputArray[index++] = encodedalternatePreferredIcTag1;
			outputArray[index++] = encodedalternatePreferredIcTag2;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementErrorTag;
			outputArray[index++] = encodeGetdataalternatePreferredIcElementErrorLen;
			for (byte encodedVal : alternatePreferredIcErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// billingServiceProvider
		if (billingServiceProvider != null) {
			outputArray[index++] = encodeGetdatabillingServiceProviderSeq;
			outputArray[index++] = encodeGetdatabillingServiceProviderSeqLen;
			outputArray[index++] = encodeGetdatabillingServiceProviderElementIdTag;
			outputArray[index++] = encodeGetdatabillingServiceProviderElementIdLen;
			outputArray[index++] = encodedbillingServiceProviderTag1;
			outputArray[index++] = encodedbillingServiceProviderTag2;
			outputArray[index++] = encodeGetdatabillingServiceProviderElementDataTag;
			outputArray[index++] = encodeGetdatabillingServiceProviderElementDataLen;
			for (byte encodedVal : billingServiceProviderEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//billingServiceProvider Error
		else if (billingServiceProviderErrorEncoded != null) {
			outputArray[index++] = encodeGetdatabillingServiceProviderSeq;
			outputArray[index++] = encodeGetdatabillingServiceProviderSeqLen;
			outputArray[index++] = encodeGetdatabillingServiceProviderElementIdTag;
			outputArray[index++] = encodeGetdatabillingServiceProviderElementIdLen;
			outputArray[index++] = encodedbillingServiceProviderTag1;
			outputArray[index++] = encodedbillingServiceProviderTag2;
			outputArray[index++] = encodeGetdatabillingServiceProviderElementErrorTag;
			outputArray[index++] = encodeGetdatabillingServiceProviderElementErrorLen;
			for (byte encodedVal : billingServiceProviderErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// serviceDenialIndicator
		if (serviceDenialIndicator != null) {
			outputArray[index++] = encodeGetdataserviceDenialIndicatorSeq;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorSeqLen;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorElementIdLen;
			outputArray[index++] = encodedserviceDenialIndicatorTag;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorElementDataLen;
			for (byte encodedVal : serviceDenialIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//serviceDenialIndicator Error
		else if (serviceDenialIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataserviceDenialIndicatorSeq;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorSeqLen;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorElementIdLen;
			outputArray[index++] = encodedserviceDenialIndicatorTag;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataserviceDenialIndicatorElementErrorLen;
			for (byte encodedVal : serviceDenialIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// collectAcceptanceIndicator
		if (collectAcceptanceIndicator != null) {
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorSeq;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorSeqLen;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorElementIdLen;
			outputArray[index++] = encodedcollectAcceptanceIndicatorTag;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorElementDataLen;
			for (byte encodedVal : collectAcceptanceIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//collectAcceptanceIndicator Error
		else if (collectAcceptanceIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorSeq;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorSeqLen;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorElementIdLen;
			outputArray[index++] = encodedcollectAcceptanceIndicatorTag;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatacollectAcceptanceIndicatorElementErrorLen;
			for (byte encodedVal : collectAcceptanceIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// diversionRoutingNumber
		if (diversionRoutingNumber != null) {
			outputArray[index++] = encodeGetdatadiversionRoutingNumberSeq;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberSeqLen;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberElementIdTag;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberElementIdLen;
			outputArray[index++] = encodeddiversionRoutingNumberTag1;
			outputArray[index++] = encodeddiversionRoutingNumberTag2;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberElementDataTag1;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberElementDataTag2;
			outputArray[index++] = (byte) (4 + encodeGetdatadiversionRoutingNumberElementDataLen);
			outputArray[index++] = (byte) GetDataConstants.DIVERSION_ROUTING_NUMBER_TYPE_OF_DIGITS;
			outputArray[index++] = (byte) diversionRoutingNumber.getNatureOfAddress();
			outputArray[index++] = (byte) 0x11;
			outputArray[index++] = (byte) diversionRoutingNumber.getAddress().length();
			for (byte encodedVal : diversionRoutingNumberEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//diversionRoutingNumber Error
		else if (diversionRoutingNumberErrorEncoded != null) {
			outputArray[index++] = encodeGetdatadiversionRoutingNumberSeq;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberSeqLen;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberElementIdTag;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberElementIdLen;
			outputArray[index++] = encodeddiversionRoutingNumberTag1;
			outputArray[index++] = encodeddiversionRoutingNumberTag2;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberElementErrorTag;
			outputArray[index++] = encodeGetdatadiversionRoutingNumberElementErrorLen;
			for (byte encodedVal : diversionRoutingNumberErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// foreignLanguageIdentifier
		if (foreignLanguageIdentifierEncoded != null) {
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierSeq;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierSeqLen;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierElementIdTag;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierElementIdLen;
			outputArray[index++] = encodedforeignLanguageIdentifierTag;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierElementDataTag;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierElementDataLen;
			for (byte encodedVal : foreignLanguageIdentifierEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//foreignLanguageIdentifier Error
		else if (foreignLanguageIdentifierErrorEncoded != null) {
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierSeq;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierSeqLen;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierElementIdTag;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierElementIdLen;
			outputArray[index++] = encodedforeignLanguageIdentifierTag;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierElementErrorTag;
			outputArray[index++] = encodeGetdataforeignLanguageIdentifierElementErrorLen;
			for (byte encodedVal : foreignLanguageIdentifierErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// genericName
		if (genericNameEncoded != null) {
			outputArray[index++] = encodeGetdatagenericNameSeq;
			outputArray[index++] = encodeGetdatagenericNameSeqLen;
			outputArray[index++] = encodeGetdatagenericNameElementIdTag;
			outputArray[index++] = encodeGetdatagenericNameElementIdLen;
			outputArray[index++] = encodedgenericNameTag;
			outputArray[index++] = encodeGetdatagenericNameElementDataTag;
			outputArray[index++] = encodeGetdatagenericNameElementDataLen;
			for (byte encodedVal : genericNameEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//genericName Error
		else if (genericNameErrorEncoded != null) {
			outputArray[index++] = encodeGetdatagenericNameSeq;
			outputArray[index++] = encodeGetdatagenericNameSeqLen;
			outputArray[index++] = encodeGetdatagenericNameElementIdTag;
			outputArray[index++] = encodeGetdatagenericNameElementIdLen;
			outputArray[index++] = encodedgenericNameTag;
			outputArray[index++] = encodeGetdatagenericNameElementErrorTag;
			outputArray[index++] = encodeGetdatagenericNameElementErrorLen;
			for (byte encodedVal : genericNameErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// icIndicators collective
		if (icIndicatorsEncoded != null ) {
			outputArray[index++] = encodeGetdataicIndicatorsSeq;
			outputArray[index++] = encodeGetdataicIndicatorsSeqLen;
			outputArray[index++] = encodeGetdataicIndicatorsElementIdTag;
			outputArray[index++] = encodeGetdataicIndicatorsElementIdLen;
			outputArray[index++] = encodedicIndicatorsTag;
			outputArray[index++] = encodeGetdataicIndicatorsElementDataTag;
			outputArray[index++] = encodeGetdataicIndicatorsElementDataLen;
			for (byte encodedVal : icIndicatorsEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//icindicators error
		else if (icIndicatorsErrorEncoded != null) {
			outputArray[index++] = encodeGetdataicIndicatorsSeq;
			outputArray[index++] = encodeGetdataicIndicatorsSeqLen;
			outputArray[index++] = encodeGetdataicIndicatorsElementIdTag;
			outputArray[index++] = encodeGetdataicIndicatorsElementIdLen;
			outputArray[index++] = encodedicIndicatorsTag;
			outputArray[index++] = encodeGetdataicIndicatorsElementErrorTag;
			outputArray[index++] = encodeGetdataicIndicatorsElementErrorLen;
			for (byte encodedVal : icIndicatorsErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		//icIndicators individual indicators
		// primaryPreferredIcIndicator
		if (primaryPreferredIcIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorSeq;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorSeqLen;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorElementIdLen;
			outputArray[index++] = encodedprimaryPreferredIcIndicatorTag1;
			outputArray[index++] = encodedprimaryPreferredIcIndicatorTag2;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorElementDataLen;
			for (byte encodedVal : primaryPreferredIcIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//primaryPreferredIcIndicator Error
		else if (primaryPreferredIcIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorSeq;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorSeqLen;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorElementIdLen;
			outputArray[index++] = encodedprimaryPreferredIcIndicatorTag1;
			outputArray[index++] = encodedprimaryPreferredIcIndicatorTag2;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataprimaryPreferredIcIndicatorElementErrorLen;
			for (byte encodedVal : primaryPreferredIcIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// alternatePreferredIcIndicator
		if (alternatePreferredIcIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorSeq;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorSeqLen;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorElementIdLen;
			outputArray[index++] = encodedalternatePreferredIcIndicatorTag1;
			outputArray[index++] = encodedalternatePreferredIcIndicatorTag2;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorElementDataLen;
			for (byte encodedVal : alternatePreferredIcIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//alternatePreferredIcIndicator Error
		else if (alternatePreferredIcIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorSeq;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorSeqLen;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorElementIdLen;
			outputArray[index++] = encodedalternatePreferredIcIndicatorTag1;
			outputArray[index++] = encodedalternatePreferredIcIndicatorTag2;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataalternatePreferredIcIndicatorElementErrorLen;
			for (byte encodedVal : alternatePreferredIcIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// preferredIncIndicator
		if (preferredIncIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdatapreferredIncIndicatorSeq;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorSeqLen;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorElementIdLen;
			outputArray[index++] = encodedpreferredIncIndicatorTag1;
			outputArray[index++] = encodedpreferredIncIndicatorTag2;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorElementDataLen;
			for (byte encodedVal : preferredIncIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//preferredIncIndicator Error
		else if (preferredIncIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatapreferredIncIndicatorSeq;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorSeqLen;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorElementIdLen;
			outputArray[index++] = encodedpreferredIncIndicatorTag1;
			outputArray[index++] = encodedpreferredIncIndicatorTag2;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatapreferredIncIndicatorElementErrorLen;
			for (byte encodedVal : preferredIncIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// ilpCicIndicator
		if (ilpCicIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdatailpCicIndicatorSeq;
			outputArray[index++] = encodeGetdatailpCicIndicatorSeqLen;
			outputArray[index++] = encodeGetdatailpCicIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatailpCicIndicatorElementIdLen;
			outputArray[index++] = encodedilpCicIndicatorTag;
			outputArray[index++] = encodeGetdatailpCicIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatailpCicIndicatorElementDataLen;
			for (byte encodedVal : ilpCicIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//ilpCicIndicator Error
		else if (ilpCicIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatailpCicIndicatorSeq;
			outputArray[index++] = encodeGetdatailpCicIndicatorSeqLen;
			outputArray[index++] = encodeGetdatailpCicIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatailpCicIndicatorElementIdLen;
			outputArray[index++] = encodedilpCicIndicatorTag;
			outputArray[index++] = encodeGetdatailpCicIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatailpCicIndicatorElementErrorLen;
			for (byte encodedVal : ilpCicIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// ilpCic
		if (ilpCicEncoded != null) {
			outputArray[index++] = encodeGetdatailpCicSeq;
			outputArray[index++] = encodeGetdatailpCicSeqLen;
			outputArray[index++] = encodeGetdatailpCicElementIdTag;
			outputArray[index++] = encodeGetdatailpCicElementIdLen;
			outputArray[index++] = encodedilpCicTag1;
			outputArray[index++] = encodedilpCicTag2;
			outputArray[index++] = encodeGetdatailpCicElementDataTag1;
			outputArray[index++] = encodeGetdatailpCicElementDataTag2;
			outputArray[index++] = encodeGetdatailpCicElementDataLen;
			outputArray[index++] = GetDataConstants.GETDATA_ILP_CIC_LENGTH;
			for (byte encodedVal : ilpCicEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//ilpCic Error
		else if (ilpCicErrorEncoded != null) {
			outputArray[index++] = encodeGetdatailpCicSeq;
			outputArray[index++] = encodeGetdatailpCicSeqLen;
			outputArray[index++] = encodeGetdatailpCicElementIdTag;
			outputArray[index++] = encodeGetdatailpCicElementIdLen;
			outputArray[index++] = encodedilpCicTag1;
			outputArray[index++] = encodedilpCicTag2;
			outputArray[index++] = encodeGetdatailpCicElementErrorTag;
			outputArray[index++] = encodeGetdatailpCicElementErrorLen;
			for (byte encodedVal : ilpCicErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}



		// interceptIndicator
		if (interceptIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdatainterceptIndicatorSeq;
			outputArray[index++] = encodeGetdatainterceptIndicatorSeqLen;
			outputArray[index++] = encodeGetdatainterceptIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatainterceptIndicatorElementIdLen;
			outputArray[index++] = encodedinterceptIndicatorTag;
			outputArray[index++] = encodeGetdatainterceptIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatainterceptIndicatorElementDataLen;
			for (byte encodedVal : interceptIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//interceptIndicator Error
		else if (interceptIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatainterceptIndicatorSeq;
			outputArray[index++] = encodeGetdatainterceptIndicatorSeqLen;
			outputArray[index++] = encodeGetdatainterceptIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatainterceptIndicatorElementIdLen;
			outputArray[index++] = encodedinterceptIndicatorTag;
			outputArray[index++] = encodeGetdatainterceptIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatainterceptIndicatorElementErrorLen;
			for (byte encodedVal : interceptIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// origBillingServiceIndicators
		//collective
		if (OrigBillingServiceIndicatorsEncoded != null ) {
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsSeq;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsSeqLen;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsElementIdTag;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsElementIdLen;
			outputArray[index++] = encodedorigBillingServiceIndicatorsTag;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsElementDataTag;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsElementDataLen;
			for (byte encodedVal : OrigBillingServiceIndicatorsEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		else if (origBillingServiceIndicatorsErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsSeq;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsSeqLen;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsElementIdTag;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsElementIdLen;
			outputArray[index++] = encodedorigBillingServiceIndicatorsTag;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsElementErrorTag;
			outputArray[index++] = encodeGetdataorigBillingServiceIndicatorsElementErrorLen;
			for (byte encodedVal : origBillingServiceIndicatorsErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		//individual orig billing indicators
		// origCollectBillingIndicator
		if (origCollectBillingIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorSeq;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorElementIdLen;
			outputArray[index++] = encodedorigCollectBillingIndicatorTag1;
			outputArray[index++] = encodedorigCollectBillingIndicatorTag2;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorElementDataLen;
			for (byte encodedVal : origCollectBillingIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origCollectBillingIndicator Error
		else if (origCollectBillingIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorSeq;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorElementIdLen;
			outputArray[index++] = encodedorigCollectBillingIndicatorTag1;
			outputArray[index++] = encodedorigCollectBillingIndicatorTag2;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigCollectBillingIndicatorElementErrorLen;
			for (byte encodedVal : origCollectBillingIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}


		//	origThirdNumberBillingIndicator
		if (origThirdNumberBillingIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorSeq;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorElementIdLen;
			outputArray[index++] = encodedorigThirdNumberBillingIndicatorTag1;
			outputArray[index++] = encodedorigThirdNumberBillingIndicatorTag2;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorElementDataLen;
			for (byte encodedVal : origThirdNumberBillingIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origThirdNumberBillingIndicator Error
		else if (origThirdNumberBillingIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorSeq;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorElementIdLen;
			outputArray[index++] = encodedorigThirdNumberBillingIndicatorTag1;
			outputArray[index++] = encodedorigThirdNumberBillingIndicatorTag2;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigThirdNumberBillingIndicatorElementErrorLen;
			for (byte encodedVal : origThirdNumberBillingIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//	origLocalNontollCallIndicator
		if (origLocalNontollCallIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorSeq;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorElementIdLen;
			outputArray[index++] = encodedorigLocalNontollCallIndicatorTag1;
			outputArray[index++] = encodedorigLocalNontollCallIndicatorTag2;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorElementDataLen;
			for (byte encodedVal : origLocalNontollCallIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origLocalNontollCallIndicator Error
		else if (origLocalNontollCallIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorSeq;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorElementIdLen;
			outputArray[index++] = encodedorigLocalNontollCallIndicatorTag1;
			outputArray[index++] = encodedorigLocalNontollCallIndicatorTag2;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigLocalNontollCallIndicatorElementErrorLen;
			for (byte encodedVal : origLocalNontollCallIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origCreditCardIndicator
		if (origCreditCardIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorSeq;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorElementIdLen;
			outputArray[index++] = encodedorigCreditCardIndicatorTag1;
			outputArray[index++] = encodedorigCreditCardIndicatorTag2;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorElementDataLen;
			for (byte encodedVal : origCreditCardIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origCreditCardIndicator Error
		else if (origCreditCardIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorSeq;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorElementIdLen;
			outputArray[index++] = encodedorigCreditCardIndicatorTag1;
			outputArray[index++] = encodedorigCreditCardIndicatorTag2;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigCreditCardIndicatorElementErrorLen;
			for (byte encodedVal : origCreditCardIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origFreeDaIndicator
		if (origFreeDaIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorSeq;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorElementIdLen;
			outputArray[index++] = encodedorigFreeDaIndicatorTag1;
			outputArray[index++] = encodedorigFreeDaIndicatorTag2;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorElementDataLen;
			for (byte encodedVal : origFreeDaIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origFreeDaIndicator Error
		else if (origFreeDaIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorSeq;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorElementIdLen;
			outputArray[index++] = encodedorigFreeDaIndicatorTag1;
			outputArray[index++] = encodedorigFreeDaIndicatorTag2;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigFreeDaIndicatorElementErrorLen;
			for (byte encodedVal : origFreeDaIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origSpecialBnsIndicator
		if (origSpecialBnsIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorSeq;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorElementIdLen;
			outputArray[index++] = encodedorigSpecialBnsIndicatorTag1;
			outputArray[index++] = encodedorigSpecialBnsIndicatorTag2;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorElementDataLen;
			for (byte encodedVal : origSpecialBnsIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origSpecialBnsIndicator Error
		else if (origSpecialBnsIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorSeq;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorElementIdLen;
			outputArray[index++] = encodedorigSpecialBnsIndicatorTag1;
			outputArray[index++] = encodedorigSpecialBnsIndicatorTag2;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigSpecialBnsIndicatorElementErrorLen;
			for (byte encodedVal : origSpecialBnsIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origSentpaidIndicator
		if (origSentpaidIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorSeq;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorElementIdLen;
			outputArray[index++] = encodedorigSentpaidIndicatorTag1;
			outputArray[index++] = encodedorigSentpaidIndicatorTag2;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorElementDataLen;
			for (byte encodedVal : origSentpaidIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origSentpaidIndicator Error
		else if (origSentpaidIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorSeq;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorElementIdLen;
			outputArray[index++] = encodedorigSentpaidIndicatorTag1;
			outputArray[index++] = encodedorigSentpaidIndicatorTag2;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigSentpaidIndicatorElementErrorLen;
			for (byte encodedVal : origSentpaidIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		//origDaccIndicator
		if (origDaccIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigDaccIndicatorSeq;
			outputArray[index++] = encodeGetdataorigDaccIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigDaccIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigDaccIndicatorElementIdLen;
			outputArray[index++] = encodedorigDaccIndicatorTag1;
			outputArray[index++] = encodedorigDaccIndicatorTag2;
			outputArray[index++] = encodeGetdataorigDaccIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigDaccIndicatorElementDataLen;
			for (byte encodedVal : origDaccIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origDaccIndicator Error
		else if (origDaccIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigDaccIndicatorSeq;
			outputArray[index++] = encodeGetdataorigDaccIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigDaccIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigDaccIndicatorElementIdLen;
			outputArray[index++] = encodedorigDaccIndicatorTag1;
			outputArray[index++] = encodedorigDaccIndicatorTag2;
			outputArray[index++] = encodeGetdataorigDaccIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigDaccIndicatorElementErrorLen;
			for (byte encodedVal : origDaccIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origBillingServiceSpareIndicator
		if (origBillingServiceSpareIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorSeq;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorElementIdLen;
			outputArray[index++] = encodedorigBillingServiceSpareIndicatorTag1;
			outputArray[index++] = encodedorigBillingServiceSpareIndicatorTag2;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorElementDataLen;
			for (byte encodedVal : origBillingServiceSpareIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origBillingServiceSpareIndicator Error
		else if (origBillingServiceSpareIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorSeq;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorElementIdLen;
			outputArray[index++] = encodedorigBillingServiceSpareIndicatorTag1;
			outputArray[index++] = encodedorigBillingServiceSpareIndicatorTag2;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigBillingServiceSpareIndicatorElementErrorLen;
			for (byte encodedVal : origBillingServiceSpareIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// origIcIndicators
		//collective
		if (origIcIndicatorsEncoded1 != null && origIcIndicatorsEncoded2 != null ) {
			outputArray[index++] = encodeGetdataorigIcIndicatorsSeq;
			outputArray[index++] = encodeGetdataorigIcIndicatorsSeqLen;
			outputArray[index++] = encodeGetdataorigIcIndicatorsElementIdTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorsElementIdLen;
			outputArray[index++] = encodedorigIcIndicatorsTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorsElementDataTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorsElementDataLen;
			for (byte encodedVal : origIcIndicatorsEncoded1) {
				outputArray[index++] = encodedVal;
			}
			for (byte encodedVal : origIcIndicatorsEncoded2) {
				outputArray[index++] = encodedVal;
			}
		}
		//	origIcIndicators Error	
		else if (origIcIndicatorsErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigIcIndicatorsSeq;
			outputArray[index++] = encodeGetdataorigIcIndicatorsSeqLen;
			outputArray[index++] = encodeGetdataorigIcIndicatorsElementIdTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorsElementIdLen;
			outputArray[index++] = encodedorigIcIndicatorsTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorsElementErrorTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorsElementErrorLen;
			for (byte encodedVal : origIcIndicatorsErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// origIcIndicators individual indicators
		// origIcIndicator
		if (origIcIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigIcIndicatorSeq;
			outputArray[index++] = encodeGetdataorigIcIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigIcIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorElementIdLen;
			outputArray[index++] = encodedorigIcIndicatorTag1;
			outputArray[index++] = encodedorigIcIndicatorTag2;
			outputArray[index++] = encodeGetdataorigIcIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorElementDataLen;
			for (byte encodedVal : origIcIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origIcIndicator Error
		else if (origIcIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigIcIndicatorSeq;
			outputArray[index++] = encodeGetdataorigIcIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigIcIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorElementIdLen;
			outputArray[index++] = encodedorigIcIndicatorTag1;
			outputArray[index++] = encodedorigIcIndicatorTag2;
			outputArray[index++] = encodeGetdataorigIcIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigIcIndicatorElementErrorLen;
			for (byte encodedVal : origIcIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// origIncIndicator
		if (origIncIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdataorigIncIndicatorSeq;
			outputArray[index++] = encodeGetdataorigIncIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigIncIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigIncIndicatorElementIdLen;
			outputArray[index++] = encodedorigIncIndicatorTag1;
			outputArray[index++] = encodedorigIncIndicatorTag2;
			outputArray[index++] = encodeGetdataorigIncIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataorigIncIndicatorElementDataLen;
			for (byte encodedVal : origIncIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origIncIndicator Error
		else if (origIncIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigIncIndicatorSeq;
			outputArray[index++] = encodeGetdataorigIncIndicatorSeqLen;
			outputArray[index++] = encodeGetdataorigIncIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataorigIncIndicatorElementIdLen;
			outputArray[index++] = encodedorigIncIndicatorTag1;
			outputArray[index++] = encodedorigIncIndicatorTag2;
			outputArray[index++] = encodeGetdataorigIncIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataorigIncIndicatorElementErrorLen;
			for (byte encodedVal : origIncIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}


		// origIc
		if (origIc != null) {
			outputArray[index++] = encodeGetdataorigIcSeq;
			outputArray[index++] = encodeGetdataorigIcSeqLen;
			outputArray[index++] = encodeGetdataorigIcElementIdTag;
			outputArray[index++] = encodeGetdataorigIcElementIdLen;
			outputArray[index++] = encodedorigIcTag1;
			outputArray[index++] = encodedorigIcTag2;
			outputArray[index++] = encodeGetdataorigIcElementDataTag1;
			outputArray[index++] = encodeGetdataorigIcElementDataTag2;
			outputArray[index++] = encodeGetdataorigIcElementDataLen;
			outputArray[index++] = GetDataConstants.GETDATA_ORIG_IC_LENGTH;
			for (byte encodedVal : origIcEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origIc Error
		else if (origIcErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigIcSeq;
			outputArray[index++] = encodeGetdataorigIcSeqLen;
			outputArray[index++] = encodeGetdataorigIcElementIdTag;
			outputArray[index++] = encodeGetdataorigIcElementIdLen;
			outputArray[index++] = encodedorigIcTag1;
			outputArray[index++] = encodedorigIcTag2;
			outputArray[index++] = encodeGetdataorigIcElementErrorTag;
			outputArray[index++] = encodeGetdataorigIcElementErrorLen;
			for (byte encodedVal : origIcErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// origInc
		if (origInc != null) {
			outputArray[index++] = encodeGetdataorigIncSeq;
			outputArray[index++] = encodeGetdataorigIncSeqLen;
			outputArray[index++] = encodeGetdataorigIncElementIdTag;
			outputArray[index++] = encodeGetdataorigIncElementIdLen;
			outputArray[index++] = encodedorigIncTag1;
			outputArray[index++] = encodedorigIncTag2;
			outputArray[index++] = encodeGetdataorigIncElementDataTag1;
			outputArray[index++] = encodeGetdataorigIncElementDataTag2;
			outputArray[index++] = encodeGetdataorigIncElementDataLen;
			outputArray[index++] = GetDataConstants.GETDATA_ORIG_INC_LENGTH;
			for (byte encodedVal : origIncEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origInc Error
		else if (origIncErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigIncSeq;
			outputArray[index++] = encodeGetdataorigIncSeqLen;
			outputArray[index++] = encodeGetdataorigIncElementIdTag;
			outputArray[index++] = encodeGetdataorigIncElementIdLen;
			outputArray[index++] = encodedorigIncTag1;
			outputArray[index++] = encodedorigIncTag2;
			outputArray[index++] = encodeGetdataorigIncElementErrorTag;
			outputArray[index++] = encodeGetdataorigIncElementErrorLen;
			for (byte encodedVal : origIncErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// origListingServicesIndicator1
		if (origListingServicesIndicator1 != null) {
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1Seq;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1SeqLen;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1ElementIdTag;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1ElementIdLen;
			outputArray[index++] = encodedorigListingServicesIndicator1Tag;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1ElementDataTag;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1ElementDataLen;
			for (byte encodedVal : origListingServicesIndicator1Encoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//origListingServicesIndicator1 Error
		else if (origListingServicesIndicator1ErrorEncoded != null) {
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1Seq;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1SeqLen;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1ElementIdTag;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1ElementIdLen;
			outputArray[index++] = encodedorigListingServicesIndicator1Tag;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1ElementErrorTag;
			outputArray[index++] = encodeGetdataorigListingServicesIndicator1ElementErrorLen;
			for (byte encodedVal : origListingServicesIndicator1ErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// preferredInc
		if (preferredInc != null) {
			outputArray[index++] = encodeGetdatapreferredIncSeq;
			outputArray[index++] = encodeGetdatapreferredIncSeqLen;
			outputArray[index++] = encodeGetdatapreferredIncElementIdTag;
			outputArray[index++] = encodeGetdatapreferredIncElementIdLen;
			outputArray[index++] = encodedpreferredIncTag1;
			outputArray[index++] = encodedpreferredIncTag2;
			outputArray[index++] = encodeGetdatapreferredIncElementDataTag1;
			outputArray[index++] = encodeGetdatapreferredIncElementDataTag2;
			outputArray[index++] = encodeGetdatapreferredIncElementDataLen;
			outputArray[index++] = GetDataConstants.GETDATA_PREFERRED_INC_LENGTH;
			for (byte encodedVal : preferredIncEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//preferredInc Error
		else if (preferredIncErrorEncoded != null) {
			outputArray[index++] = encodeGetdatapreferredIncSeq;
			outputArray[index++] = encodeGetdatapreferredIncSeqLen;
			outputArray[index++] = encodeGetdatapreferredIncElementIdTag;
			outputArray[index++] = encodeGetdatapreferredIncElementIdLen;
			outputArray[index++] = encodedpreferredIncTag1;
			outputArray[index++] = encodedpreferredIncTag2;
			outputArray[index++] = encodeGetdatapreferredIncElementErrorTag;
			outputArray[index++] = encodeGetdatapreferredIncElementErrorLen;
			for (byte encodedVal : preferredIncErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}


		//preferredCodeList
		if (preferredCodeList != null) {
			outputArray[index++] = encodeGetdatapreferredCodeListSeq;
			outputArray[index++] = encodeGetdatapreferredCodeListSeqLen;
			outputArray[index++] = encodeGetdatapreferredCodeListElementIdTag;
			outputArray[index++] = encodeGetdatapreferredCodeListElementIdLen;
			outputArray[index++] = encodedpreferredCodeListTag1;
			outputArray[index++] = encodedpreferredCodeListTag2;
			outputArray[index++] = encodeGetdatapreferredCodeListElementDataTag;
			outputArray[index++] = encodeGetdatapreferredCodeListElementDataLen;
			for (byte encodedVal : preferredCodeListEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//preferredCodeList Error
		else if (preferredCodeListErrorEncoded != null) {
			outputArray[index++] = encodeGetdatapreferredCodeListSeq;
			outputArray[index++] = encodeGetdatapreferredCodeListSeqLen;
			outputArray[index++] = encodeGetdatapreferredCodeListElementIdTag;
			outputArray[index++] = encodeGetdatapreferredCodeListElementIdLen;
			outputArray[index++] = encodedpreferredCodeListTag1;
			outputArray[index++] = encodedpreferredCodeListTag2;
			outputArray[index++] = encodeGetdatapreferredCodeListElementErrorTag;
			outputArray[index++] = encodeGetdatapreferredCodeListElementErrorLen;
			for (byte encodedVal : preferredCodeListErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// primaryPreferredIc
		if (primaryPreferredIc != null) {
			outputArray[index++] = encodeGetdataprimaryPreferredIcSeq;
			outputArray[index++] = encodeGetdataprimaryPreferredIcSeqLen;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementIdTag;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementIdLen;
			outputArray[index++] = encodedprimaryPreferredIcTag1;
			outputArray[index++] = encodedprimaryPreferredIcTag2;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementDataTag1;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementDataTag2;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementDataLen;
			outputArray[index++] = GetDataConstants.GETDATA_PRIMARY_PREFERRED_IC_LENGTH;
			for (byte encodedVal : primaryPreferredIcEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//primaryPreferredIc Error
		else if (primaryPreferredIcErrorEncoded != null) {
			outputArray[index++] = encodeGetdataprimaryPreferredIcSeq;
			outputArray[index++] = encodeGetdataprimaryPreferredIcSeqLen;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementIdTag;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementIdLen;
			outputArray[index++] = encodedprimaryPreferredIcTag1;
			outputArray[index++] = encodedprimaryPreferredIcTag2;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementErrorTag;
			outputArray[index++] = encodeGetdataprimaryPreferredIcElementErrorLen;
			for (byte encodedVal : primaryPreferredIcErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}


		// rao
		if (rao != null) {
			outputArray[index++] = encodeGetdataraoSeq;
			outputArray[index++] = encodeGetdataraoSeqLen;
			outputArray[index++] = encodeGetdataraoElementIdTag;
			outputArray[index++] = encodeGetdataraoElementIdLen;
			outputArray[index++] = encodedraoTag1;
			outputArray[index++] = encodedraoTag2;
			outputArray[index++] = encodeGetdataraoElementDataTag1;
			outputArray[index++] = encodeGetdataraoElementDataTag2;
			outputArray[index++] = encodeGetdataraoElementDataLen;
			outputArray[index++] = GetDataConstants.GETDATA_RAO_LENGTH;
			for (byte encodedVal : raoEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//rao Error
		else if (raoErrorEncoded != null) {
			outputArray[index++] = encodeGetdataraoSeq;
			outputArray[index++] = encodeGetdataraoSeqLen;
			outputArray[index++] = encodeGetdataraoElementIdTag;
			outputArray[index++] = encodeGetdataraoElementIdLen;
			outputArray[index++] = encodedraoTag1;
			outputArray[index++] = encodedraoTag2;
			outputArray[index++] = encodeGetdataraoElementErrorTag;
			outputArray[index++] = encodeGetdataraoElementErrorLen;
			for (byte encodedVal : raoErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}


		// recordStatusIndicator
		if (recordStatusIndicator != null) {
			outputArray[index++] = encodeGetdatarecordStatusIndicatorSeq;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorSeqLen;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorElementIdLen;
			outputArray[index++] = encodedrecordStatusIndicatorTag;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorElementDataLen;
			for (byte encodedVal : recordStatusIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//recordStatusIndicator Error
		else if (recordStatusIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatarecordStatusIndicatorSeq;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorSeqLen;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorElementIdLen;
			outputArray[index++] = encodedrecordStatusIndicatorTag;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatarecordStatusIndicatorElementErrorLen;
			for (byte encodedVal : recordStatusIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// referralNumber
		if (referralNumber != null) {
			outputArray[index++] = encodeGetdatareferralNumberSeq;
			outputArray[index++] = encodeGetdatareferralNumberSeqLen;
			outputArray[index++] = encodeGetdatareferralNumberElementIdTag;
			outputArray[index++] = encodeGetdatareferralNumberElementIdLen;
			outputArray[index++] = encodedreferralNumberTag1;
			outputArray[index++] = encodedreferralNumberTag2;
			outputArray[index++] = encodeGetdatareferralNumberElementDataTag1;
			outputArray[index++] = encodeGetdatareferralNumberElementDataTag2;
			outputArray[index++] = (byte) (4 + encodeGetdatareferralNumberElementDataLen);
			outputArray[index++] = (byte) GetDataConstants.REFERRAL_NUMBER_TYPE_OF_DIGITS;
			outputArray[index++] = (byte) referralNumber.getNatureOfAddress();
			outputArray[index++] = (byte) 0x11;
			outputArray[index++] = (byte) referralNumber.getAddress().length();
			for (byte encodedVal : referralNumberEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//referralNumber Error
		else if (referralNumberErrorEncoded != null) {
			outputArray[index++] = encodeGetdatareferralNumberSeq;
			outputArray[index++] = encodeGetdatareferralNumberSeqLen;
			outputArray[index++] = encodeGetdatareferralNumberElementIdTag;
			outputArray[index++] = encodeGetdatareferralNumberElementIdLen;
			outputArray[index++] = encodedreferralNumberTag1;
			outputArray[index++] = encodedreferralNumberTag2;
			outputArray[index++] = encodeGetdatareferralNumberElementErrorTag;
			outputArray[index++] = encodeGetdatareferralNumberElementErrorLen;
			for (byte encodedVal : referralNumberErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// serviceOrEquipmentIndicator
		if (serviceOrEquipmentIndicator != null) {
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorSeq;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorSeqLen;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorElementIdLen;
			outputArray[index++] = encodedserviceOrEquipmentIndicatorTag;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorElementDataTag;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorElementDataLen;
			for (byte encodedVal : serviceOrEquipmentIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//serviceOrEquipmentIndicator Error
		else if (serviceOrEquipmentIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorSeq;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorSeqLen;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorElementIdTag;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorElementIdLen;
			outputArray[index++] = encodedserviceOrEquipmentIndicatorTag;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdataserviceOrEquipmentIndicatorElementErrorLen;
			for (byte encodedVal : serviceOrEquipmentIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// thirdNumberAcceptanceIndicator
		if (thirdNumberAcceptanceIndicator != null) {
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorSeq;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorSeqLen;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorElementIdLen;
			outputArray[index++] = encodedthirdNumberAcceptanceIndicatorTag;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorElementDataLen;
			for (byte encodedVal : thirdNumberAcceptanceIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//thirdNumberAcceptanceIndicator Error
		else if (thirdNumberAcceptanceIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorSeq;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorSeqLen;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorElementIdLen;
			outputArray[index++] = encodedthirdNumberAcceptanceIndicatorTag;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatathirdNumberAcceptanceIndicatorElementErrorLen;
			for (byte encodedVal : thirdNumberAcceptanceIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		// treatmentIndicator
		if (treatmentIndicator != null) {
			outputArray[index++] = encodeGetdatatreatmentIndicatorSeq;
			outputArray[index++] = encodeGetdatatreatmentIndicatorSeqLen;
			outputArray[index++] = encodeGetdatatreatmentIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatatreatmentIndicatorElementIdLen;
			outputArray[index++] = encodedtreatmentIndicatorTag;
			outputArray[index++] = encodeGetdatatreatmentIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatatreatmentIndicatorElementDataLen;
			for (byte encodedVal : treatmentIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//treatmentIndicator Error
		else if (treatmentIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatatreatmentIndicatorSeq;
			outputArray[index++] = encodeGetdatatreatmentIndicatorSeqLen;
			outputArray[index++] = encodeGetdatatreatmentIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatatreatmentIndicatorElementIdLen;
			outputArray[index++] = encodedtreatmentIndicatorTag;
			outputArray[index++] = encodeGetdatatreatmentIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatatreatmentIndicatorElementErrorLen;
			for (byte encodedVal : treatmentIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// trueBillingNumber
		if (trueBillingNumber != null) {
			outputArray[index++] = encodeGetdatatrueBillingNumberSeq;
			outputArray[index++] = encodeGetdatatrueBillingNumberSeqLen;
			outputArray[index++] = encodeGetdatatrueBillingNumberElementIdTag;
			outputArray[index++] = encodeGetdatatrueBillingNumberElementIdLen;
			outputArray[index++] = encodedtrueBillingNumberTag;
			outputArray[index++] = encodeGetdatatrueBillingNumberElementDataTag1;
			outputArray[index++] = encodeGetdatatrueBillingNumberElementDataTag2;
			outputArray[index++] = (byte) (4 + encodeGetdatatrueBillingNumberElementDataLen);
			outputArray[index++] = (byte) GetDataConstants.TRUE_BILLING_NUMBER_TYPE_OF_DIGITS;
			outputArray[index++] = (byte) trueBillingNumberNature;
			outputArray[index++] = (byte) 0x11;
			outputArray[index++] = (byte) trueBillingNumber.length();
			for (byte encodedVal : trueBillingNumberEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//trueBillingNumber Error
		else if (trueBillingNumberErrorEncoded != null) {
			outputArray[index++] = encodeGetdatatrueBillingNumberSeq;
			outputArray[index++] = encodeGetdatatrueBillingNumberSeqLen;
			outputArray[index++] = encodeGetdatatrueBillingNumberElementIdTag;
			outputArray[index++] = encodeGetdatatrueBillingNumberElementIdLen;
			outputArray[index++] = encodedtrueBillingNumberTag;
			outputArray[index++] = encodeGetdatatrueBillingNumberElementErrorTag;
			outputArray[index++] = encodeGetdatatrueBillingNumberElementErrorLen;
			for (byte encodedVal : trueBillingNumberErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// wirelessServicesOrigIndicator
		if (wirelessServicesOrigIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorSeq;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorSeqLen;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorElementIdLen;
			outputArray[index++] = encodedwirelessServicesOrigIndicatorTag1;
			outputArray[index++] = encodedwirelessServicesOrigIndicatorTag2;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorElementDataLen;
			for (byte encodedVal : wirelessServicesOrigIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//wirelessServicesOrigIndicator Error
		else if (wirelessServicesOrigIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorSeq;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorSeqLen;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorElementIdLen;
			outputArray[index++] = encodedwirelessServicesOrigIndicatorTag1;
			outputArray[index++] = encodedwirelessServicesOrigIndicatorTag2;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatawirelessServicesOrigIndicatorElementErrorLen;
			for (byte encodedVal : wirelessServicesOrigIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		// wirelessServicesTerminatingIndicator
		if (wirelessServicesTerminatingIndicatorEncoded != null) {
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorSeq;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorSeqLen;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorElementIdLen;
			outputArray[index++] = encodedwirelessServicesTerminatingIndicatorTag1;
			outputArray[index++] = encodedwirelessServicesTerminatingIndicatorTag2;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorElementDataTag;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorElementDataLen;
			for (byte encodedVal : wirelessServicesTerminatingIndicatorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}
		//wirelessServicesTerminatingIndicator Error
		else if (wirelessServicesTerminatingIndicatorErrorEncoded != null) {
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorSeq;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorSeqLen;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorElementIdTag;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorElementIdLen;
			outputArray[index++] = encodedwirelessServicesTerminatingIndicatorTag1;
			outputArray[index++] = encodedwirelessServicesTerminatingIndicatorTag2;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorElementErrorTag;
			outputArray[index++] = encodeGetdatawirelessServicesTerminatingIndicatorElementErrorLen;
			for (byte encodedVal : wirelessServicesTerminatingIndicatorErrorEncoded) {
				outputArray[index++] = encodedVal;
			}
		}

		if (!InvalidTcapIdsList.isEmpty()) {
			for(int tcapId : InvalidTcapIdsList) {
				if (logger.isInfoEnabled()) {
					logger.info("encodeGetDataQuery :: Encoding response for invaild Tcap ID " + tcapId);
				}
				if (tcapId < 128) {
					outputArray[index++] = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
					outputArray[index++] = (byte) 0x08;
					outputArray[index++] = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
					outputArray[index++] = (byte) 0x01;
					outputArray[index++] = (byte) tcapId;
					outputArray[index++] = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
					outputArray[index++] = (byte) 0x02;
					outputArray[index++] = GetDataConstants.GETDATA_INVALID_TCAPID_ERROR_VALUE1;
					outputArray[index++] = GetDataConstants.GETDATA_INVALID_TCAPID_ERROR_VALUE2;
				}
				else {
					outputArray[index++] = GetDataConstants.GETDATA_STD_ELEMENT_SEQUENCE_TAG;
					outputArray[index++] = (byte) 0x09;
					outputArray[index++] = GetDataConstants.GETDATA_LIDB_ELEMENT_ID_TAG;
					outputArray[index++] = (byte) 0x02;
					outputArray[index++] = (byte) 0x00;
					outputArray[index++] = (byte) tcapId;
					outputArray[index++] = GetDataConstants.GETDATA_LIDB_ELEMENT_ERROR_TAG;
					outputArray[index++] = (byte) 0x02;
					outputArray[index++] = GetDataConstants.GETDATA_INVALID_TCAPID_ERROR_VALUE1;
					outputArray[index++] = GetDataConstants.GETDATA_INVALID_TCAPID_ERROR_VALUE2;
				}
			}
		}


		if (logger.isInfoEnabled()) {
			logger.info("Exit: encodeGetDataQuery ::" + CommonUtils.formatBytes(outputArray));
		}
		return outputArray; 
	}



	/**
	 * @param callData
	 * @return
	 */
	public static byte[] getApplicationErrorForGetDataQuery(CallData callData) {
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
		if(logger.isDebugEnabled()){
			logger.debug("Application error for GetData:" +
					CommonUtils.formatBytes(buffer));
		}
		return buffer;
	}

	/**
	 * @param asciiVal
	 * @return
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
	 * @param asciiVal
	 * @return
	 */
	private static byte[] encodeIa5(String asciiVal) {
		return asciiVal.getBytes(StandardCharsets.US_ASCII);
	}

	/**
	 * method to decode address signal from byte array
	 * 
	 * @param data
	 * @throws InvalidInputException
	 */
	private static String decodeAdrsSignalForGetQuery(byte[] data, int offset, int parity) throws InvalidInputException {
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
	private static byte[] encodeAdrsSignalForGetQuery(String addrSignal) throws InvalidInputException {

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
	public static void resetGetDataQuery(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetGetDataQuery:Enter");
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
		
		if(legData.get(LegDataAttributes.GETDATA_QUERIED_NUM) != null) {
			legData.remove(LegDataAttributes.GETDATA_QUERIED_NUM);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ACCOUNT_OWNER) != null) {
			legData.remove(LegDataAttributes.GETDATA_ACCOUNT_OWNER);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS) != null) {
			legData.remove(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_SENTPAID_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ALPHANUMERIC_STRING) != null) {
			legData.remove(LegDataAttributes.GETDATA_ALPHANUMERIC_STRING);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ALTERNATE_PREFERRED_IC) != null) {
			legData.remove(LegDataAttributes.GETDATA_ALTERNATE_PREFERRED_IC);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_BILLING_SERVICE_PROVIDER) != null) {
			legData.remove(LegDataAttributes.GETDATA_BILLING_SERVICE_PROVIDER);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_SERVICE_DENIAL_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_SERVICE_DENIAL_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_COLLECT_ACCEPTANCE_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_COLLECT_ACCEPTANCE_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_DIVERSION_ROUTING_NUMBER) != null) {
			legData.remove(LegDataAttributes.GETDATA_DIVERSION_ROUTING_NUMBER);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER) != null) {
			legData.remove(LegDataAttributes.GETDATA_FOREIGN_LANGUAGE_IDENTIFIER);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_GENERIC_NAME) != null) {
			legData.remove(LegDataAttributes.GETDATA_GENERIC_NAME);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_IC_INDICATORS) != null) {
			legData.remove(LegDataAttributes.GETDATA_IC_INDICATORS);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_PRIMARY_PREFERRED_IC_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ALTERNATE_PREFERRED_IC_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_PREFERRED_INC_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_PREFERRED_INC_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ILP_CIC_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ILP_CIC_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ILP_CIC) != null) {
			legData.remove(LegDataAttributes.GETDATA_ILP_CIC);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_INTERCEPT_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_INTERCEPT_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_BILLING_SERVICE_INDICATORS) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_BILLING_SERVICE_INDICATORS);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_COLLECT_BILLING_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_COLLECT_BILLING_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_THIRD_NUMBER_BILLING_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_LOCAL_NONTOLL_CALL_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_CREDIT_CARD_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_CREDIT_CARD_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_FREE_DA_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_FREE_DA_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_SPECIAL_BNS_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_SPECIAL_BNS_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_SENTPAID_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_SENTPAID_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_DACC_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_DACC_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_BILLING_SERVICE_SPARE_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_IC_INDICATORS) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_IC_INDICATORS);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_IC_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_IC_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_INC_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_INC_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_IC) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_IC);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_INC) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_INC);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1) != null) {
			legData.remove(LegDataAttributes.GETDATA_ORIG_LISTING_SERVICES_INDICATOR_1);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_PREFERRED_INC) != null) {
			legData.remove(LegDataAttributes.GETDATA_PREFERRED_INC);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_PREFERRED_CODE_LIST) != null) {
			legData.remove(LegDataAttributes.GETDATA_PREFERRED_CODE_LIST);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_PRIMARY_PREFERRED_IC) != null) {
			legData.remove(LegDataAttributes.GETDATA_PRIMARY_PREFERRED_IC);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_RAO) != null) {
			legData.remove(LegDataAttributes.GETDATA_RAO);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_RECORD_STATUS_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_RECORD_STATUS_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_REFERRAL_NUMBER) != null) {
			legData.remove(LegDataAttributes.GETDATA_REFERRAL_NUMBER);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_SERVICE_OR_EQUIPMENT_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_THIRD_NUMBER_ACCEPTANCE_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_TREATMENT_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_TREATMENT_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_TRUE_BILLING_NUMBER) != null) {
			legData.remove(LegDataAttributes.GETDATA_TRUE_BILLING_NUMBER);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_WIRELESS_SERVICES_ORIG_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR) != null) {
			legData.remove(LegDataAttributes.GETDATA_WIRELESS_SERVICES_TERMINATING_INDICATOR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_INVALID_TCAPID_LIST) != null) {
			legData.remove(LegDataAttributes.GETDATA_INVALID_TCAPID_LIST);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_RECEIVED_TCAPID_LIST) != null) {
			legData.remove(LegDataAttributes.GETDATA_RECEIVED_TCAPID_LIST);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_PSRID_TYPE) != null) {
			legData.remove(LegDataAttributes.GETDATA_PSRID_TYPE);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_PRIMARY_SERVICE_REQUESTER) != null) {
			legData.remove(LegDataAttributes.GETDATA_PRIMARY_SERVICE_REQUESTER);
		}
		
		//resetting leg2Data 
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
		if(leg2Data.get(LegDataAttributes.P_LIDB_QUERY_TYPE) != null) {
			leg2Data.remove(LegDataAttributes.P_LIDB_QUERY_TYPE);
		}
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ACCOUNT_OWNER) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ACCOUNT_OWNER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ACCOUNT_OWNER_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ACCOUNT_OWNER_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_INDICATORS_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_THIRD_NUMBER_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_CREDIT_CARD_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SPECIAL_BNS_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SENTPAID_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SENTPAID_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SENTPAID_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_SENTPAID_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ADDITIONAL_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ALPHANUMERIC_STRING) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ALPHANUMERIC_STRING);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ALPHANUMERIC_STRING_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ALPHANUMERIC_STRING_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_BILLING_SERVICE_PROVIDER) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_BILLING_SERVICE_PROVIDER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_BILLING_SERVICE_PROVIDER_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_BILLING_SERVICE_PROVIDER_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_DENIAL_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_SERVICE_DENIAL_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_DENIAL_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_SERVICE_DENIAL_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_COLLECT_ACCEPTANCE_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_COLLECT_ACCEPTANCE_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_COLLECT_ACCEPTANCE_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_COLLECT_ACCEPTANCE_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_DIVERSION_ROUTING_NUMBER) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_DIVERSION_ROUTING_NUMBER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_DIVERSION_ROUTING_NUMBER_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_DIVERSION_ROUTING_NUMBER_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_FOREIGN_LANGUAGE_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_FOREIGN_LANGUAGE_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_FOREIGN_LANGUAGE_IDENTIFIER_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_FOREIGN_LANGUAGE_IDENTIFIER_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_GENERIC_NAME) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_GENERIC_NAME);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_GENERIC_NAME_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_GENERIC_NAME_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_IC_INDICATORS_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_IC_INDICATORS_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ALTERNATE_PREFERRED_IC_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ILP_CIC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ILP_CIC_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ILP_CIC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ILP_CIC_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ILP_CIC_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_INTERCEPT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_INTERCEPT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_INTERCEPT_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_INTERCEPT_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_INDICATORS) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_INDICATORS);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_INDICATORS_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_INDICATORS_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_COLLECT_BILLING_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_COLLECT_BILLING_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_COLLECT_BILLING_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_COLLECT_BILLING_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_THIRD_NUMBER_BILLING_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_THIRD_NUMBER_BILLING_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_THIRD_NUMBER_BILLING_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_THIRD_NUMBER_BILLING_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LOCAL_NONTOLL_CALL_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_LOCAL_NONTOLL_CALL_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_LOCAL_NONTOLL_CALL_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_CREDIT_CARD_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_CREDIT_CARD_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_CREDIT_CARD_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_CREDIT_CARD_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_FREE_DA_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_FREE_DA_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_FREE_DA_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_FREE_DA_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SPECIAL_BNS_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_SPECIAL_BNS_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SPECIAL_BNS_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_SPECIAL_BNS_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SENTPAID_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_SENTPAID_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_SENTPAID_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_SENTPAID_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_DACC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_DACC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_DACC_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_DACC_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_SPARE_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_SPARE_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_BILLING_SERVICE_SPARE_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATORS_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATORS_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_IC_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_INC_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_IC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_IC_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_IC_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_INC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_INC_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_INC_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LISTING_SERVICES_INDICATOR_1) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_LISTING_SERVICES_INDICATOR_1);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_ORIG_LISTING_SERVICES_INDICATOR_1_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_ORIG_LISTING_SERVICES_INDICATOR_1_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PREFERRED_INC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PREFERRED_INC_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_CODE_LIST) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PREFERRED_CODE_LIST);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PREFERRED_CODE_LIST_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PREFERRED_CODE_LIST_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_PRIMARY_PREFERRED_IC_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_RAO) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_RAO);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_RAO_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_RAO_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_RECORD_STATUS_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_RECORD_STATUS_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_RECORD_STATUS_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_RECORD_STATUS_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_OR_EQUIPMENT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_SERVICE_OR_EQUIPMENT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_SERVICE_OR_EQUIPMENT_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_SERVICE_OR_EQUIPMENT_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_THIRD_NUMBER_ACCEPTANCE_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_THIRD_NUMBER_ACCEPTANCE_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_THIRD_NUMBER_ACCEPTANCE_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_THIRD_NUMBER_ACCEPTANCE_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_TREATMENT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_TREATMENT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_TREATMENT_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_TREATMENT_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_REFERRAL_NUMBER) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_REFERRAL_NUMBER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_REF_NUM_TYPE) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_REF_NUM_TYPE);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_REFERRAL_NUMBER_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_REFERRAL_NUMBER_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_TRUE_BILLING_NUMBER) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_TRUE_BILLING_NUMBER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_TRUE_BILLING_NUMBER_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_TRUE_BILLING_NUMBER_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_ORIG_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_ORIG_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_ORIG_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_ORIG_INDICATOR_ERR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_TERMINATING_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_TERMINATING_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_TERMINATING_INDICATOR_ERR) != null) {
			leg2Data.remove(LegDataAttributes.P_GETDATA_O_WIRELESS_SERVICES_TERMINATING_INDICATOR_ERR);
		}
		
		if(legData.get(LegDataAttributes.GETDATA_INVALID_TCAPID_LIST) != null) {
			legData.remove(LegDataAttributes.GETDATA_INVALID_TCAPID_LIST);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE);
		}
		if (logger.isDebugEnabled()) {
			logger.info("resetGetDataQuery:Exit");
		}
	}
}
