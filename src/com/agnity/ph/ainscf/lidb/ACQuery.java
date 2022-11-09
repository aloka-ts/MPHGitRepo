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

import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.util.Constant;
import com.agnity.ain.util.Util;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.ainscf.AinScfProtocolParser;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;

/**
 * Automatic Callback Query
 * 
 * @author Vikas
 */
public class ACQuery {
	private static Logger logger = Logger.getLogger(BNSQuery.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
			'f' };

	// Testing buffer for Ac Query decodeAcQuery()
	/**
	 * byte[] testBuf = { (byte) 0xF2, //Parameter Set ID
	 * (byte) 0x26, //Parameter Set Length 
	 * (byte) 0xAA, //Service Key ID 
	 * (byte) 0x21, //Service Key Length
	 * (byte) 0x84, //Digits Id [Billing Number] 
	 * (byte) 0x09, //Digits Length (byte) 0x05, (byte) 0x00, (byte) 0x11, (byte) 0x0A, 0x21, 0x43, 0x65, 
	 * (byte) 0x87, 0x09, //Digits 
	 * (byte) 0x84, //Digits Id [Called Party] 
	 * (byte) 0x09, //Digits Length 
	 * (byte) 0x01, 0x00, 
	 * (byte) 0x11, 0x0A, 0x18, 0x17, 0x21, 0x00, 0x76, //Digits 
	 * (byte) 0x84, //Digits Id [Calling Party] (byte) 0x09, //Digits Length 
	 * (byte) 0x02, 0x00, (byte) 0x11, 0x0A, 0x79, 0x12, 0x70, 0x76, 0x24 //Digits };
	 */
	public static void decodeAcQuery(CallData callData, byte[] input) throws AINCodecException, InvalidInputException {
		if (logger.isInfoEnabled()) {
			logger.info("decodeAcbQuery:Enter");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.ACB);

		if (logger.isInfoEnabled()) {
			logger.info("decodeAcbQuery:set P_LIDB_QUERY_TYPE == ACB");
		}

		// Parameter Set ID
		if (input[0] != (byte) 0xF2) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:parameter set ID is not correct");
			throw new AINCodecException("parameter set ID is not correct");
		}
		// Parameter set id

		// check for parameter set length.
		if (input.length <= 28) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:input buffer can not be less then 28 ");
			throw new AINCodecException("Length of the buffer must be Greater then 28");
		}

		// Service Key Id
		if (input[2] != (byte) 0xAA) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:Service key ID is not correct ");
			throw new AINCodecException("Service key ID is not correct");
		}

		// check for Digits Id [Billing Number]
		if (input[4] != (byte) 0x84) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:Digits Id [Destination Number] is not correct ");
			throw new AINCodecException("Digits Id [Destination Number] is not correct ");
		}

		// check for Digits length.
		if (input[5] != (byte) 0x09) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:Digits  length[Destination Number] should be correct. ");
			throw new AINCodecException("Digits  length[Destination Number] should be correct.");
		}
		// operation to perform on --> digits length.

		// check for digits
		if (input[6] != (byte) 0x05 || input[7] != (byte) 0x00 || input[8] != (byte) 0x11 || input[9] != (byte) 0x0A) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:Digits[Destination Number] should be correct. ");
			throw new AINCodecException("Digits[Destination Number] should be correct.");
		}

		int DestinationNumDigitLen = input[5] & 0xFF;// it will always be 9
		if (DestinationNumDigitLen != 9) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:DestinationNumDigitLen should be correct. ");
			throw new AINCodecException("DestinationNumDigitLen should be correct.");
		}
		/*
		 * format for digits: 0x05 0x00 0x11 0x0A [5 octet BCD Format 10 digits]
		 */
		AinDigits ainDigitForDestinationNum = new AinDigits();
		// Creating format for AIN digit decoder
		int arraysize = 5; // 5 octet BCD format 10 digits
		byte[] inputforDecodeAin = new byte[arraysize];
		int natureOfBns = input[10] & 0xFF;
		for (int i = 0; i < inputforDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputforDecodeAin[i] = input[10 + i];
		}
		ainDigitForDestinationNum.setAddrSignal(decodeAdrsSignalForAc(inputforDecodeAin, 0, 0));
		PhoneNumber destinationNumber = AinScfProtocolParser.parseAinDigits(ainDigitForDestinationNum, natureOfBns);
		legData.set(LegDataAttributes.ACB_DESTINATION_NUM, destinationNumber);

		// check for Digits Id [CallingDN]
		if (input[15] != (byte) 0x84) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:Digits Id [CallingDN ] should be correct. ");
			throw new AINCodecException("Digits Id [CallingDN] should be correct.");
		}
		// operation to perform on Digits Id [CallingDN]
		// get the Digits[CallingDN] Length
		int callingDNDigitLen = input[16] & 0xFF;
		/**
		 * format for Digits[CallingDN]--> d0x01 [NOA] 0x11 [Num of Dig] [BCD Format]
		 * [Num of Dig] 10 - National Max 15 - International check for Digit[CallingDN]
		 */
		if (input[17] != (byte) 0x01) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:Digit[CallingDN] should be correct. ");
			throw new AINCodecException("Digit[CallingDN] should be correct.");
		}

		// check for Digit[CallingDN] -->NOA
		if (input[18] != (byte) 0x00 && input[18] != (byte) 0x01 && input[18] != (byte) 0x02
				&& input[18] != (byte) 0x03) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:Digit[CallingDN] -->NOA should be correct. ");
			throw new AINCodecException("Digit[CallingDN] -->NOA should be correct.");
		}

		// num digit length
		int numdigilen = input[20] & 0xFF;
		if (numdigilen > 15 || numdigilen < 10) {
			logger.info("decodeAcbQuery:throwing exception ");
			logger.error("decodeAcbQuery:Digit[CallingDN] -->num digit length should be correct. ");
			throw new AINCodecException("Digit[CallingDN] -->num digit length should be correct.");
		}

		// set CallingDN digit
		AinDigits aindigitcallingDn = new AinDigits();
		// Creating format for AIN digit decoder
		int arrsize;
		if (numdigilen % 2 == 0) {
			arrsize = (numdigilen / 2);
		} else {
			arrsize = (numdigilen + 1) / 2;
		}
		byte[] inputfrDecodeAin = new byte[arrsize + 2];
		inputfrDecodeAin[0] = input[18]; // adding NOA-->called
		for (int i = 1; i < inputfrDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAin[i] = input[19 + i];
		}
		// getting CallingDN num
		AinDigits callingDnNum = aindigitcallingDn.decodeAinDigits(inputfrDecodeAin, Constant.CALLED);
		// perform operation callingDnNum :vikas singh
		PhoneNumber callingDnNumber = AinScfProtocolParser.parseCalledPartyNum(callingDnNum);
		legData.set(LegDataAttributes.P_CALLING_DN, callingDnNumber);

		// check for Digits Id [Calling Party]
		if (input[arrsize + 21] != (byte) 0x84) {
			logger.info("decodeBuffer:throwing exception ");
			logger.error("decodeBuffer:Digits Id [Calling Party] should be correct. ");
			throw new AINCodecException("Digits Id [Calling Party] should be correct.");
		}

		// [Calling Party] -->Digits Length
		int clgPartyDigitLen = input[arrsize + 22] & 0xFF;

		/*
		 * check for [Calling Party] -->Digits format --> 0x02 [NOA] 0x11 [Num of Dig]
		 * [BCD Format]
		 * 
		 */
		if (input[arrsize + 23] != (byte) 0x02) {
			logger.info("decodeBuffer:throwing exception ");
			logger.error("decodeBuffer:Digit[Calling Party] should be correct. ");
			throw new AINCodecException("Digit[Calling Party] should be correct.");
		}

		// check for Digit[CallingDN] -->NOA
		if (input[arrsize + 24] != (byte) 0x00 && input[arrsize + 24] != (byte) 0x01
				&& input[arrsize + 24] != (byte) 0x02 && input[arrsize + 24] != (byte) 0x03) {
			logger.info("decodeBuffer:throwing exception ");
			logger.error("decodeBuffer:Digit[Calling Party] -->NOA should be correct. ");
			throw new AINCodecException("Digit[Calling Party] -->NOA should be correct.");
		}

		// num digit length
		int numdigilenclg = input[arrsize + 26] & 0xFF;

		if (numdigilenclg > 15 || numdigilenclg < 10) {
			logger.info("decodeBuffer:throwing exception ");
			logger.error("decodeBuffer:Digit[Calling Party] -->num digit length should be correct. ");
			throw new AINCodecException("Digit[Calling Party] -->num digit length should be correct.");
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
		inputfrDecodeAinclg[0] = input[arrsize + 24]; // adding NOA-->called
		for (int i = 1; i < inputfrDecodeAinclg.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAinclg[i] = input[arrsize + 25 + i];
		}
		// getting calling party num
		AinDigits callingPartyNum = aindigitcalling.decodeAinDigits(inputfrDecodeAinclg, Constant.CALLING);
		System.out.println(callingPartyNum);

		// perform operation callingPartyNum :vikas singh
		PhoneNumber callingNumber = AinScfProtocolParser.parseCallingPartyNum(callingPartyNum);
		legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
		if (logger.isInfoEnabled()) {
			logger.info("decodeAcbQuery:Exit");
		}

	}

	private static String decodeAdrsSignalForAc(byte[] data, int offset, int parity) throws InvalidInputException {
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
	 * method here used to reset all Global Variables, leg1Data & leg2Data attributes 
	 * @param callData
	 */
	public static void resetACQuery(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetACQuery:Enter");
		}
		//resetting leg1Data
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		if(legData.get(LegDataAttributes.P_LIDB_QUERY_TYPE) != null) {
			legData.remove(LegDataAttributes.P_LIDB_QUERY_TYPE);
		}
		if(legData.get(LegDataAttributes.ACB_DESTINATION_NUM) != null) {
			legData.remove(LegDataAttributes.ACB_DESTINATION_NUM);
		}
		
		if(legData.get(LegDataAttributes.P_CALLING_DN) != null) {
			legData.remove(LegDataAttributes.P_CALLING_DN);
		}
		
		if(legData.get(LegDataAttributes.P_CALLING_PARTY) != null) {
			legData.remove(LegDataAttributes.P_CALLING_PARTY);
		}
				
		if (logger.isDebugEnabled()) {
			logger.info("resetACQuery:Exit");
		}
	}
}
