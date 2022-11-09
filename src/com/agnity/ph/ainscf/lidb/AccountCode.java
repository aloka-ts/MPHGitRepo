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

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.util.Util;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.ainscf.AinScfProtocolConfig;
import com.agnity.ph.ainscf.AinScfProtocolParser;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;

/**
 * This class handle Account code received in ProvideValue. Since it has nothing to do with 
 * LIDB however all ProvideValue is being handled as part of LIDB Package. 
 * @author rarya
 *
 */
public class AccountCode {


	private static Logger logger = Logger.getLogger(AccountCode.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
	'f' };

	/**
	 * Method is used to decode account code buffer. In case of any parsing error it will 
	 * follow lidb protocol error route. 
	 * @param callData
	 * @param input
	 * @throws InvalidInputException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void decodeAccountCodeQuery(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {
		if (logger.isDebugEnabled()) {
			logger.debug("decodeAccountCodeQuery: Enter::" + Util.formatBytes(input));
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		int index = 0;
		// Parameter Set ID 
		// (byte) 0xF2
		if (input[index] != (byte) 0xF2 && input[index] != (byte)0x31) {
			logger.error("decodeBuffer:parameter set ID is not correct. rxed:" + CommonUtils.formatBytes(input));
			throw new AINCodecException("parameter set ID is not correct");
		}

		int totalLen = input[++index]; // index = 1

		// operation perform to set--> parameter set id
		if (input.length < AccountCodeConstants.ACCOUNT_CODE_LENGTH) {
			logger.error("decodeBuffer:input buffer can not be less than 22" + CommonUtils.formatBytes(input));
			throw new ParameterOutOfRangeException("Length of the buffer must be Greater >= 22");
		}

		if (input[++index] != (byte) 0xDF || input[++index] != (byte) 0x49) {
			logger.error("decodeBuffer:Calling party Tag not as 0xdf 0x49 " + CommonUtils.formatBytes(input));
			throw new AINCodecException("Calling party tag in Account code is not correct");
		}

		// check Calling Party Length 
		if (input[++index] != (byte) 0x09) {
			logger.error("decodeBuffer:Calling party length should be 9 " + CommonUtils.formatBytes(input));
			throw new AINCodecException("CallingParty info length should be 9");
		}

		// Type of digits should be calling party Number , pos - 5
		// For DAL the type of digit should be Billing Number 
		if (input[++index] != (byte) 0x02 && input[index] != (byte) 0x05) {
			logger.error("decodeBuffer:digit type is not for calling party 0x02 " + CommonUtils.formatBytes(input));
			throw new AINCodecException("Digit Type is not for calling party [0x02]");
		}

		if(input[index] == 0x05){
			legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.AC_DAL.name());
		}else{
			legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.AC.name());
		}

		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:set P_LIDB_QUERY_TYPE: " + legData.get(LegDataAttributes.P_LIDB_QUERY_TYPE));
		}

		// Set Calling party Number at offset 8 
		int noa = input[++index];
		int np  = (input[++index] >> 4 ) & 0x0F;

		if(logger.isDebugEnabled()){
			logger.debug("Calling Party: NOA:" + noa + ", NP:" + np);
		}

		int numdigilen = input[++index] & 0xFF;
		int arrsize;
		if (numdigilen % 2 == 0) {
			arrsize = (numdigilen / 2);
		} else {
			arrsize = (numdigilen + 1) / 2;
		}
		AinDigits ainDigitForCC1 = new AinDigits();
		byte[] inputfrDecodeAin = new byte[arrsize];
		for (int i = 0; i < inputfrDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAin[i] = input[++index];
		}
		if(numdigilen%2 == 0)
			ainDigitForCC1.setAddrSignal(decodeAdrsSignalForAccountCode(inputfrDecodeAin, 0, 0));
		else 
			ainDigitForCC1.setAddrSignal(decodeAdrsSignalForAccountCode(inputfrDecodeAin, 0, 1));
		
		PhoneNumber callingParty = AinScfProtocolParser.parseAinDigits(ainDigitForCC1, 0);
		callingParty.setNumberingPlan(np);
		
		String isAinAccountCodeStripEnabled = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.AIN_ACCOUNT_CODE_STRIP_99_ENABLED);
		
		if (PhConstants.TRUE.equals(isAinAccountCodeStripEnabled) && callingParty.getAddress().startsWith("99")) {
			String address = callingParty.getAddress();
			address = address.substring(2,address.length());
			callingParty.setAddress(address);
		}

		legData.set(LegDataAttributes.P_CALLING_PARTY, callingParty);

		if(logger.isDebugEnabled()){
			logger.debug("Account code: CallingParty: " + callingParty);
		}

		// Check Billing Number
		if (input[++index] != (byte) 0xDF || input[++index] != (byte) 0x49 ) {
			logger.error(
					"decodeBuffer:Octet not match 0xdf 0x49 at index:" + index  
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("Billing tag 0xdf 0x49 is not correct ");
		}

		// length of billing buffer
		int billingBuf = input[++index];

		// check type of digits should be 0x05 for billing number
		if(input[++index] != 0x05){
			logger.error(
					"decodeBuffer:digit at pos index:" + index +" is not billing digit " 
							+ CommonUtils.formatBytes(input));
			throw new AINCodecException("Digit Type is not Billing number:0x05");
		}

		// Set Calling party Number at offset 8 
		noa = input[++index];
		np  = (input[++index] >> 4 ) & 0x0F;

		if(logger.isDebugEnabled()){
			logger.debug("Billing Number: NOA:" + noa + ", NP:" + np);
		}

		// length of Billing Number
		int billingNumLen = input[++index];
		logger.debug("billingNumLen:" + billingNumLen);
		arrsize = 0;
		if (billingNumLen % 2 == 0) {
			arrsize = (billingNumLen / 2);
		} else {
			arrsize = (billingNumLen + 1) / 2;
		}

		AinDigits billingNumDigits = new AinDigits();
		byte[] billingNumDigitsArray = new byte[arrsize];

		for (int i = 0; i < billingNumDigitsArray.length; i++) {
			// adding num of digit and bcd format of digit
			billingNumDigitsArray[i] = input[++index];
		}
		
		if(billingNumLen%2 ==0)
			billingNumDigits.setAddrSignal(decodeAdrsSignalForAccountCode(billingNumDigitsArray, 0, 0));
		else
			billingNumDigits.setAddrSignal(decodeAdrsSignalForAccountCode(billingNumDigitsArray, 0, 1));
		
		PhoneNumber billingNum = AinScfProtocolParser.parseAinDigits(billingNumDigits, 0);
		billingNum.setNumberingPlan(np);
		legData.set(LegDataAttributes.ACCOUNT_CODE, billingNum);

		logger.debug("BillingNumber:" + billingNum);

//		if(totalLen - index  != -1){
//			logger.error(
//					"total Len: " + totalLen + ", index:" + index +" ,"
//							+ CommonUtils.formatBytes(input));
//		//	throw new AINCodecException("total Length is not matching with receivedbuffer length");
//		}
	}

	/**
	 * Method is used to encode account code response. Application need to send 
	 * Account code result as 1 or 0, in case found or not found and accordingly 
	 * will be sent in ReturnResult. 
	 * 
	 * @param callData
	 * @return byte[]
	 * @throws InvalidInputException, AINCodecException
	 */
	public static byte[] encodeAccountCodeResponse(CallData callData) throws InvalidInputException, AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeAccountCodeResponse:Enter");
		}

		int index = 0;
		byte[] respBuf = new byte[AccountCodeConstants.AC_SUCCESS_RESP_LENGTH];
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);

		String acResult = (String)legData.get(LegDataAttributes.ACCOUNT_CODE_RESULT);

		// respBuf
		respBuf[index++] = AccountCodeConstants.AC_PARAMETER_SET_ID;
		respBuf[index++] = AccountCodeConstants.AC_PARAMETER_SET_LEN;
		respBuf[index++] = AccountCodeConstants.AC_PARAMETER_TAG1;
		respBuf[index++] = AccountCodeConstants.AC_PARAMETER_TAG2;
		respBuf[index++] = AccountCodeConstants.AC_VALID_LEN;

		if(StringUtils.equalsIgnoreCase(acResult, AccountCodeConstants.AC_FOUND)){
			respBuf[index++] = AccountCodeConstants.AC_VALID_FLAG;
		}else{
			respBuf[index++] = AccountCodeConstants.AC_INVALID_FLAG;
		}

		if (logger.isDebugEnabled()) {
			logger.info("encodeAccountCodeResponse:Exit, acResult:" + acResult + ", " +
					CommonUtils.formatBytes(respBuf));
		}
		return respBuf;
	}

	/**
	 * This method is used for decoding billing Number Address Signal
	 * 
	 * @param data   represents 5 octet BCD input
	 * @param offcet and @param parity are set to 0 for billing Number
	 * @return a String
	 */
	private static String decodeAdrsSignalForAccountCode(byte[] data, 
			int offset, int parity) throws InvalidInputException {
		if (logger.isDebugEnabled()) {
			logger.debug("Enter: decodeAdrsSignal:Input--> data:" + Util.formatBytes(data) +
					" ,offset:" + offset + " ,parity" + parity);
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
		tmpStr=tmpStr.replaceAll("a", "0");
		if (logger.isDebugEnabled()) {
			logger.debug("Exit: decodeAdrsSignal:Output<-- adrssignal:" + tmpStr);
		}
		return tmpStr;
	}

}
