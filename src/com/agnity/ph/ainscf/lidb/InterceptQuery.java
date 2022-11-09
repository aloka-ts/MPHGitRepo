package com.agnity.ph.ainscf.lidb;

import static com.agnity.ph.ainscf.lidb.InterceptConstants.COMPANY_ID_IDENTIFIER_TAG1;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.COMPANY_ID_IDENTIFIER_TAG2;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.COMPANY_ID_LENGTH;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG1;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.DIGITS_IDENTIFIER_ID_REFERRAL_NUMBER_TAG2;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.INTERCEPT_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.INTERCEPT_INDICATOR_ID_TAG2;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.INTERCEPT_INDICATOR_LENGTH;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.PARAMETER_SET_ID;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.RECORD_STATUS_INDICATOR_ID_LENGTH;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.RECORD_STATUS_INDICATOR_ID_TAG1;
import static com.agnity.ph.ainscf.lidb.InterceptConstants.RECORD_STATUS_INDICATOR_ID_TAG2;

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
public class InterceptQuery {


	private static Logger logger = Logger.getLogger(InterceptQuery.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
	'f' };

	private InterceptQuery() {

	}

	/**
	
	 * 
	 * @param callData
	 * @param input
	 * @throws InvalidInputException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void decodeInterceptQuery(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {
		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:Enter::" + Util.formatBytes(input));
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.INTERCEPT.name());

		if (logger.isDebugEnabled()) {
			logger.debug("decodeBuffer:set P_LIDB_QUERY_TYPE == INTERCEPT");
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
		if (input.length < InterceptConstants.INTRCPT_MANDATORY_LEN) {
			logger.error("decodeBuffer:input buffer cannot be less than 14" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new ParameterOutOfRangeException("Length of the buffer must be Greater >= 14");
		}

		// check for INTERCEPT Info ID - Row R
		currentIndex = 2;
		currentElementIdLen = 2; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[2] != (byte) 0xDF || input[3] != (byte) 0x6E) {
			logger.error("decodeBuffer:INTERCEPT info identifier is not correct at pos2 & 3 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("INTERCEPT info identifier is not correct");
		}
		// operation to perform to set--->INTERCEPT Info ID

		// check for INTERCEPT Info length. - Row S
		if (input[4] != (byte) 0x00) {
			logger.error("decodeBuffer:INTERCEPT info length should be zero at pos4. " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("INTERCEPT info length should be zero");
		}
		// operation to perform on--->INTERCEPT Info length.

		// check for Service key ID - Row T
		currentIndex = 5;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[5] != (byte) 0xAA) {
			logger.error("decodeBuffer:INTERCEPT Service key ID is not correct at pos5 " + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("INTERCEPT Service key ID is not correct ");
		}

		// From GR: Service Key Length - The service key has a length of 6 octets plus
		// the length of the
		// called number digits field.
		int serviceKeyLen = input[6] & 0xFF;
		//		if (serviceKeyLen != 11) {
		//			logger.error("decodeBuffer:INTERCEPT Service key Length is not correct at pos 6 " + CommonUtils.formatBytes(input));
		//			throw new AINCodecException("INTERCEPT Service key Length is not correct ");
		//		}
		
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
		//		if (input[8] != (byte) 0x09) {
		//			logger.error("decodeBuffer:Digits length[Called Number] should be correct at pos8. "
		//					+ CommonUtils.formatBytes(input));
		//			throw new AINCodecException("Digits  length[Called Number] should be correct.");
		//		}
		// operation to perform on --> digits length.

		// check for Digits Id [Called Party] - Row X -1
		if (input[9] != (byte) 0x01) {
			logger.error(
					"decodeBuffer:Type of Digit[Called Party] should be correct at pos20" + CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Type of Digit[Called Party] should be correct.");
		}

		// check for Digit[Called Party] -->NOA - Row X (2)
		if (input[10] != (byte) 0x00 && input[10] != (byte) 0x01) {
			logger.error("decodeBuffer:Digit[Called Party] -->NOAshould be correct at pos 10"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] -->NOA, Encoding, length should be correct.");
		}

		// check for Digit[Called Party] --> Encoding/NP - Row X (3)
		if (input[11] != (byte) 0x11) {
			logger.error("decodeBuffer:Digit[Called Party] -->Encoding, NP should be correct at pos 11"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] -->Encoding, NP length should be correct.");
		}

		
		// set called party digit
		int numdigilen = input[12] & 0xFF;
		// check for Digit[Called Party] --> length - Row X (4)
		if (numdigilen > 15 || numdigilen <= 0 ) {
			logger.error("decodeBuffer:Digit[Called Party] -->Called party digits should be >0 and <=15"
					+ CommonUtils.formatBytes(input));
			AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			throw new AINCodecException("Digit[Called Party] -->Called party digits should be >0 and <=15.");
		}
		
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
		ainDigitForCC1.setAddrSignal(decodeAdrsSignalForIntercept(inputfrDecodeAin, 0, 0));
		PhoneNumber interceptCalledNumber = AinScfProtocolParser.parseAinDigits(ainDigitForCC1, natureOfCC1Digits);
		legData.set(LegDataAttributes.P_CALLED_PARTY, interceptCalledNumber);	
		
	}

	/**
	 * method to encode the INTERCEPT Query
	 * 
	 * @param callData
	 * @return byte[]
	 * @throws InvalidInputException, AINCodecException
	 */
	public static byte[] encodeInterceptsQuery(CallData callData) throws InvalidInputException, AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeInterceptQuery:Enter");
		}

		// encode and set the data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int index = 0;
		// total length of output array
		int totalLength = 0;
		// totalLength += InterceptConstants.INTRCPT_PARAM_SET_ID_LEN;

		// company id should be long value in string format
		// Row O, P Q
		if (leg2Data.get(LegDataAttributes.P_INTRCPT_M_COMPANY_ID) == null) {
			logger.error("encodeInterceptQuery:parameter companyId is Mandatory");
			throw new AINCodecException("parameter companyId is Mandatory");
		}

		String InterceptCompId = (String) leg2Data.get(LegDataAttributes.P_INTRCPT_M_COMPANY_ID);
		//byte[] companyId = decimalToBcdForIntercept(InterceptCompId);
		byte[] companyId = AinScfProtocolFieldCodec.encodeDigitsInBcdFormat(InterceptCompId);
		if (companyId.length != 2) {
			logger.error("encodeInterceptQuery : company id length should be 2");
			throw new AINCodecException("company id length should be 2");
		}

		// increment length by 5
		totalLength += InterceptConstants.INTRCPT_COMPANY_ID_LEN;

		// Record Status Indicator, Row R, S, T
		if (leg2Data.get(LegDataAttributes.P_INTRCPT_M_RECORD_STATUS_INDICATOR) == null) {
			logger.error("encodeInterceptQuery:parameter record status indicator is Mandatory");
			throw new AINCodecException("parameter record status indicator is Mandatory");
		}

		int recStatInd = (Integer) leg2Data.get(LegDataAttributes.P_INTRCPT_M_RECORD_STATUS_INDICATOR);
		//int recStatIndInt = Integer.parseInt(recStatInd);
		//byte[] encodedRecStatInd = asciToHex(recStatInd);
		byte[] encodedRecStatInd = CommonUtils.formatIntToByte(recStatInd);
		if (encodedRecStatInd.length != 1) {
			logger.info("encodeInterceptQuery:throwing exception ");
			logger.error("encodeInterceptQuery:parameter record status indicator length should be 1");
			throw new AINCodecException("parameter record status indicatorlength should be 1");
		}

		// Increment length by 4
		totalLength += InterceptConstants.INTRCPT_REC_STATUS_IND_LEN;

		int inrcptInd = (Integer) leg2Data.get(LegDataAttributes.P_INTRCPT_M_INTERCEPT_INDICATOR);
		//int inrcptIndInt = Integer.parseInt(inrcptInd);
		byte[] encodedinrcptInd = CommonUtils.formatIntToByte(inrcptInd);
		//byte[] encodedinrcptInd = asciToHex(inrcptInd);
		totalLength += InterceptConstants.INTRCPT_INTERCPT_IND_LEN;


		// Reference Number Row AY, AZ, BA
		PhoneNumber refNum = null;
		if (leg2Data.get(LegDataAttributes.P_INTRCPT_O_REF_NUM) != null) {
			refNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_INTRCPT_O_REF_NUM);
			
			if(!StringUtils.isNotBlank(refNum.getAddress())){
				refNum = null;
			}else{
				if (leg2Data.get(LegDataAttributes.P_INTRCPT_O_REF_NUM_TYPE) == null) {
					logger.error("encodeInterceptQuery:Referral number type is missing");
					throw new AINCodecException("parameter Referral number type is missing");
				}
				int refNumType = (Integer) leg2Data.get(LegDataAttributes.P_INTRCPT_O_REF_NUM_TYPE);
				// National presentation allowed - 0
				// International presentation allowed - 1
					refNum.setNatureOfAddress(refNumType);
				}
		}



		byte encodedRefNumNumDigit = 0;
		byte[] encodedRefNum = null;

		if (refNum != null) {
			encodedRefNum = encodeAdrsSignalForIntercept(refNum.getAddress());
			encodedRefNumNumDigit = (byte) encodedRefNum.length;
			totalLength += encodedRefNum.length;
			totalLength += InterceptConstants.INTRCPT_REF_DIFIT_IDENTIFIER_LEN;
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

		outputArray[index++] = INTERCEPT_INDICATOR_ID_TAG1;
		outputArray[index++] = INTERCEPT_INDICATOR_ID_TAG2;
		outputArray[index++] = INTERCEPT_INDICATOR_LENGTH;
		for (byte encodedVal : encodedinrcptInd) {
			outputArray[index++] = encodedVal;
		}

		outputArray[index++] = RECORD_STATUS_INDICATOR_ID_TAG1;
		outputArray[index++] = RECORD_STATUS_INDICATOR_ID_TAG2;
		outputArray[index++] = RECORD_STATUS_INDICATOR_ID_LENGTH;
		for (byte encodedVal : encodedRecStatInd) {
			outputArray[index++] = encodedVal;
		}
		
		// Reference Number
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
	private static String decodeAdrsSignalForIntercept(byte[] data, int offset, int parity) throws InvalidInputException {
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
	private static byte[] encodeAdrsSignalForIntercept(String addrSignal) throws InvalidInputException {

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
	public static byte[] getApplicationErrorForIntercept(CallData callData) {
		// error code will be in leg2
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int totalLength = 0;
		int index = 0;
		totalLength += InterceptConstants.TOTAL_FIXED_LEN_APP_ERR;

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
			logger.debug("Application Error for INTERCEPT:" + CommonUtils.formatBytes(buffer));
		}
		return buffer;
	}

	/**
	 * This method is used for encode PhoneNumber into byte array
	 * 
	 * @param PhoneNumber PhoneNumber ph
	 * @return a byte array
	 */
	private static byte[] encodeInterceptAINDigits(PhoneNumber ph) throws InvalidInputException {

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
	public static byte[] decimalToBcdForIntercept(String input) {
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
	public static void resetInterceptQuery(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetInterceptQuery:Enter");
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

		//resetting leg2Data
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);	
		
		if(leg2Data.get(LegDataAttributes.P_INTRCPT_M_COMPANY_ID) != null) {
			leg2Data.remove(LegDataAttributes.P_INTRCPT_M_COMPANY_ID);
		}
		
		if(leg2Data.get(LegDataAttributes.P_INTRCPT_M_RECORD_STATUS_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_INTRCPT_M_RECORD_STATUS_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_INTRCPT_M_INTERCEPT_INDICATOR) != null) {
			leg2Data.remove(LegDataAttributes.P_INTRCPT_M_INTERCEPT_INDICATOR);
		}
		
		if(leg2Data.get(LegDataAttributes.P_INTRCPT_O_REF_NUM) != null) {
			leg2Data.remove(LegDataAttributes.P_INTRCPT_O_REF_NUM);
		}
		
		if(leg2Data.get(LegDataAttributes.P_INTRCPT_O_REF_NUM_TYPE) != null) {
			leg2Data.remove(LegDataAttributes.P_INTRCPT_O_REF_NUM_TYPE);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE);
		}
		
		//resetting leg1Data
		if (logger.isDebugEnabled()) {
			logger.info("resetInterceptQuery:Exit");
		}
	}

}
