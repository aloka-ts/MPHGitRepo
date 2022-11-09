package com.agnity.ph.ainscf.lidb;

import static com.agnity.ph.ainscf.lidb.IcdcConstants.ICDR_IDENTIFIER_TAG1;
import static com.agnity.ph.ainscf.lidb.IcdcConstants.ICDR_IDENTIFIER_TAG2;
import static com.agnity.ph.ainscf.lidb.IcdcConstants.ICDR_LENGTH;
import static com.agnity.ph.ainscf.lidb.IcdcConstants.PARAMETER_SET_ID;

import java.nio.charset.StandardCharsets;

import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.enumdata.CalledNatOfNumEnum;
import com.agnity.ain.enumdata.NumPlanEnum;
import com.agnity.ain.exceptions.InvalidInputException;
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

public class ICDCQuery {

	private static Logger logger = Logger.getLogger(ICDCQuery.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
	'f' };

	private ICDCQuery() {

	}

	// Testing buffer for Icdc Query decodeBuffer()
	/**
	
	 * 
	 * @param callData
	 * @param input
	 * @throws InvalidInputException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void decodeICDCQuery(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {
		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:Enter::" + Util.formatBytes(input));
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.ICDC.name());

		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:set P_LIDB_QUERY_TYPE == ICDC");
		}
		// Parameter Set ID - Row I Table 8-23 GR 1149
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
		if (input.length < IcdcConstants.ICDC_MANDATORY_LEN) {
			logger.error("decodeBuffer:input buffer can not be less than 22" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new ParameterOutOfRangeException("Length of the buffer must be Greater >= 22");
		}
		// check for parameter set length - Row J - length is variable, not checking
		
		// check for ICDC Info ID - Row K
		currentIndex = 2;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[2] != (byte) 0xDF || input[3] != (byte) 0x56) {
			logger.error("decodeBuffer:ICDC info identifier is not correct at pos2 & 3 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("ICDC info identifier is not correct");
		}
		// operation to perform to set--->ICDC Info ID

		// check for ICDC Info length. - Row L
		if (input[4] != (byte) 0x00) {
			logger.error("decodeBuffer:ICDC info length should be zero at pos4. " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("ICDC info length should be zero");
		}
		// operation to perform on--->ICDC Info length.

		// check for Service key ID - Row M
		currentIndex = 5;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[5] != (byte) 0xAA) {
			logger.error("decodeBuffer:ICDC Service key ID is not correct at pos5 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("ICDC Service key ID is not correct ");
		}

		// From GR: Service Key Length - Row N variable length
		int serviceKeyLen = input[6] & 0xFF;
		
		// check for Digits Identifier- Row O
		currentIndex = 7;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[7] != (byte) 0x84) {
			logger.error(
					"decodeBuffer:Digits Id [Called Number] is not correct at pos7 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Id [Called Number] is not correct ");
		}

		// check for Digits length. - Row P variable length
//		if (input[8] != (byte) 0x09) {
//			logger.error("decodeBuffer:Digits length[Called Number] should be correct at pos8. "
//					+ CommonUtils.formatBytes(input));
//			throw new AINCodecException("Digits  length[Called Number] should be correct.");
//		}

		// check for Digits Id [ANI Calling or billing number] - Row Q -1
		if (input[9] != (byte) 0x02 && input[9] != (byte) 0x05 ) {
			logger.error(
					"decodeBuffer:Type of Digits should be correct at pos9" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Type of Digits should be correct.");
		}
		int digitIdType = input[9] & 0xFF;	

		// check for Digit Identifier -->NOA - Row Q-2
		if (input[10] != (byte) 0x00 ) {
			logger.error("decodeBuffer:Digit Identifier  -->NOA should be correct at pos 10"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit Identifier  -->NOA should be correct.");
		}

		// check for Digit Identifier -->Encoding,NP - Row Q-3
		if (input[11] != (byte) 0x11 && input[11] != (byte) 0x01) {
			logger.error("decodeBuffer:Digit Identifier  -->Encoding should be correct at pos 11"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit Identifier  -->Encoding should be correct.");
		}

		// check for Digit Identifier -->Length - Row Q-4 variable length		
		int numdigilen = input[12] & 0xFF;
		
		// Check if digit type is ANI calling and checking the length, encoding, NP
		if (digitIdType == (byte) 0x02) { // ANI calling
			// check if Encoding/NP are correct for calling number type- Row Q-3
			if (input[11] != (byte) 0x11) {
				logger.error("decodeBuffer:Digit Identifier  -->Encoding/NP for Calling number should be correct at pos 11"
						+ CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digit Identifier  -->Encoding/NP for calling number should be correct.");
			}
			
			if (numdigilen != (byte) 0x0A) {
				logger.error("decodeBuffer:Digit Identifier  -->Length should be 10 for ANI Calling at pos 12"
						+ CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digit Identifier  -->Length should be 10 for ANI Calling.");
				
			}
		}
		else { //Billing Number type		
			if (numdigilen > (byte) 0x13 || numdigilen < (byte) 0x0A) {
				logger.error("decodeBuffer:Digit Identifier  -->Length should be >=10 and <19 for Billing number at pos 12"
						+ CommonUtils.formatBytes(input));
				AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
				throw new AINCodecException("Digit Identifier  -->Length should be >=10 and <19 for Billing number.");
				
			}			
		}

		// set called party digit
		int arrsize;
		if (numdigilen % 2 == 0) {
			arrsize = (numdigilen / 2);
		} else {
			arrsize = (numdigilen + 1) / 2;
		}
		AinDigits ainDigitForIcdc = new AinDigits();
		byte[] inputfrDecodeAin = new byte[arrsize];
		int natureOfIcdcDigits = input[11] & 0xFF;
		for (int i = 0; i < inputfrDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAin[i] = input[13 + i];
		}
		ainDigitForIcdc.setAddrSignal(decodeAdrsSignalForIcdc(inputfrDecodeAin, 0, 0));
		PhoneNumber icdcSrvcKey = AinScfProtocolParser.parseAinDigits(ainDigitForIcdc, natureOfIcdcDigits);
		if (digitIdType == (byte) 0x02) { // ANI calling) {
			legData.set(LegDataAttributes.P_CALLING_PARTY, icdcSrvcKey);

			//Setting the service key to ICDC_BILLING_NUM also for convenience in using same variable from ADE 
			legData.set(LegDataAttributes.ICDC_M_BILLING_NUM, icdcSrvcKey);
		}
		else { //Billing Number Type
			legData.set(LegDataAttributes.ICDC_M_BILLING_NUM, icdcSrvcKey);
		}

		// check for Digits Identifier- Row R
		int indexnum = 13 + arrsize; 
		currentIndex = indexnum;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[indexnum] != (byte) 0x84) {
			logger.error(
					"decodeBuffer:Digits Identifier is not correct at pos " + indexnum + ". " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits Identifier is not correct ");
		}
		indexnum++;
		
		// check for Digits length. - Row S 
		if (input[indexnum] != (byte) 0x06) {
			logger.error("decodeBuffer:Digits length[Carrier identifier] should be correct at pos " + indexnum + ". "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digits  length[Carrier identifier] should be correct.");
		}
		indexnum++;

		// check for type of Digits - Row T -1
		if (input[indexnum] != (byte) 0x08) {
			logger.error("decodeBuffer:Type of Digits [Carrier identifier] should be correct at pos " + indexnum + ". "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Type of Digits [Carrier identifier] should be correct.");
		}	
		indexnum++;
		
		// check for NOA - Row T -2
		if (input[indexnum] != (byte) 0x00) {
			logger.error("decodeBuffer:Nature of Number [Carrier identifier] should be correct at pos " + indexnum + ". "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Nature of Number [Carrier identifier] should be correct.");
		}			
		indexnum++;
		
		// check for NOA - Row T -3
		if (input[indexnum] != (byte) 0x01) {
			logger.error("decodeBuffer:Encoding/NP [Carrier identifier] should be correct at pos " + indexnum + ". "
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Encoding/NP [Carrier identifier] should be correct.");
		}
		indexnum++;
		
		// check for Digit Identifier length - Row R-4 variable length		
		int carrieridlen = input[indexnum] & 0xFF;		

		// set carrier id digit
		int arrsize2;
		if (carrieridlen % 2 == 0) {
			arrsize2 = (carrieridlen / 2);
		} else {
			arrsize2 = (carrieridlen + 1) / 2;
		}
		AinDigits ainDigitForCarrierId = new AinDigits();
		byte[] inputfrDecodeAinCarrier = new byte[arrsize2];
		int natureOfDigits = input[11] & 0xFF;
		for (int i = 0; i < inputfrDecodeAinCarrier.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAinCarrier[i] = input[13 + i];
		}
		ainDigitForCarrierId.setAddrSignal(decodeAdrsSignalForIcdc(inputfrDecodeAinCarrier, 0, 0));
		PhoneNumber icdcCarrier = AinScfProtocolParser.parseAinDigits(ainDigitForCarrierId, natureOfDigits);
		legData.set(LegDataAttributes.ICDC_M_CARRIER_ID, icdcCarrier);
	
	}

	/**
	 * method to encode the ICDC Query
	 * 
	 * @param callData
	 * @return byte[]
	 * @throws InvalidInputException, AINCodecException
	 */
	public static byte[] encodeIcdcsQuery(CallData callData) throws InvalidInputException, AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeICDCQuery:Enter");
		}

		// encode and set the data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int index = 0;
		// total length of output array
		int totalLength = 0;
		// totalLength += IcdcConstants.ICDC_PARAM_SET_ID_LEN;
		
		// IC denial check response
		// Row O, P Q
		if (leg2Data.get(LegDataAttributes.P_ICDC_ICDR) == null) {
			logger.error("encodeICDCQuery:parameter IC Denial Check Response is Mandatory");
			throw new AINCodecException("parameter IC Denial Check Response is Mandatory");
		}
		
		byte[] encodedIcdcIcdr = null;
		int icdrIcdr = (Integer) leg2Data.get(LegDataAttributes.P_ICDC_ICDR);
		encodedIcdcIcdr = CommonUtils.formatIntToByte(icdrIcdr);
		if (encodedIcdcIcdr.length != 1) {
			logger.error("encodeIcdcQuery : IC Denial Check Response length should be 1");
			throw new AINCodecException("IC Denial Check Response length should be 1");
		}

		// increment length by 4
		totalLength += 4;


		//output array
		String[] parameterSetLength = convertHexRepersentaion(Integer.toHexString(totalLength));
		byte[] outputArray = new byte[totalLength + 2];

		outputArray[index++] = PARAMETER_SET_ID;
		outputArray[index++] = (byte) Byte.decode(parameterSetLength[0]);
		outputArray[index++] = ICDR_IDENTIFIER_TAG1;
		outputArray[index++] = ICDR_IDENTIFIER_TAG2;
		outputArray[index++] = ICDR_LENGTH;
		for (byte encodedVal : encodedIcdcIcdr) {
			outputArray[index++] = encodedVal;
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
	private static String decodeAdrsSignalForIcdc(byte[] data, int offset, int parity) throws InvalidInputException {
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
	private static byte[] encodeAdrsSignalForIcdc(String addrSignal) throws InvalidInputException {

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
	public static byte[] getApplicationErrorForIcdc(CallData callData) {
		// error code will be in leg2
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int totalLength = 0;
		int index = 0;
		totalLength += IcdcConstants.TOTAL_FIXED_LEN_APP_ERR;

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
			logger.debug("Application Error for ICDC:" + CommonUtils.formatBytes(buffer));
		}
		return buffer;
	}


	
	/**
	 * This method is used for encode PhoneNumber into byte array
	 * 
	 * @param PhoneNumber PhoneNumber ph
	 * @return a byte array
	 */
	private static byte[] encodeIcdcAINDigits(PhoneNumber ph) throws InvalidInputException {

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
	public static byte[] decimalToBcdForIcdc(String input) {
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
	public static void resetICDCQuery(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetICDCQuery:Enter");
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
	
		if(legData.get(LegDataAttributes.ICDC_M_BILLING_NUM) != null) {
			legData.remove(LegDataAttributes.ICDC_M_BILLING_NUM);
		}
	
		if(legData.get(LegDataAttributes.ICDC_M_CARRIER_ID) != null) {
			legData.remove(LegDataAttributes.ICDC_M_CARRIER_ID);
		}

		//resetting leg2Data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
			
		if(leg2Data.get(LegDataAttributes.P_ICDC_ICDR) != null) {
			leg2Data.remove(LegDataAttributes.P_ICDC_ICDR);
		}
			
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
		}
			
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE);
		}
		if (logger.isDebugEnabled()) {
			logger.info("resetICDCQuery:Exit");
		}
	}

}

