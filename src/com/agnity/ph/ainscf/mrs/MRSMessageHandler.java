/****
Copyright (c) 2015 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.


Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.ainscf.mrs;

import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AddressSignal;
import com.agnity.ain.datatypes.AinDigits;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.PhoneNumber;
import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.tcap.GlobalTitle;
import com.agnity.ph.ainscf.AinScfProtocolParser;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;

import jain.protocol.ss7.tcap.component.InvokeIndEvent;

/**
 * @author manish.kumar
 *
 */
public class MRSMessageHandler {

	private static Logger logger = Logger.getLogger(MRSMessageHandler.class);

	/**
	 * method used to check SCCP Digits, if number of digits is equals to 6 only then decode
	 * @param callData
	 * @throws Exception 
	 */
	public static boolean isSixDigitsSCCP(CallData callData) throws Exception {
		SccpUserAddress addr = null;
		GlobalTitle gtt = null;
		String receivedDigits = null;
		try
		{
			if (callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS) != null)
			{
				addr = (SccpUserAddress)callData.get(CallDataAttribute.P_SCCP_REMOTE_USER_ADDRESS);
				if (addr.isGlobalTitlePresent())
				{
					gtt = addr.getGlobalTitle();
					if (gtt.getAddressInformation() != null)
					{
						if (logger.isDebugEnabled()) {
							logger.debug("checkSCCPDigits:: Address signal received: ");
						}
						receivedDigits = convertDigitsToString(gtt.getAddressInformation());
					}
				}
				else if (logger.isDebugEnabled())
				{
					logger.debug("checkSCCPDigits: GTT not enabled");
				}
			}
		}
		catch (Exception ex)
		{
			logger.warn("checkSCCPDigits: exception: " + ex);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("checkSCCPDigits: digits recevied:" + receivedDigits);
		}
		if(receivedDigits == null)
			return false;
		//if called party number is 6 digits
		return receivedDigits.length() == 6;
	}
	/**
	 * This method is used to convert SCCP digits to Number. 
	 * @param data
	 * @return
	 */
	private static String convertDigitsToString(byte[] data)
	{
		String value = "";
		String retVal = "";
		for (int i = 0; i < data.length; i++) {
			value = value + Integer.toString((data[i] & 0xFF) + 256, 16).substring(1);
		}
		int i = 0;
		for (int j = 1; (i < value.length()) && (j < value.length()); i++)
		{
			retVal = retVal + value.charAt(j);
			retVal = retVal + value.charAt(j - 1);

			j += 2;
		}
		if (logger.isDebugEnabled()) {
			logger.debug("convertDigitsToString:: ByteString: " + value + ", ReturnedString:" + retVal);
		}
		return retVal;
	}
	
	/**
	 * method used to parse Tcap Initial Query
	 * 
	 * @param invokeIndEvent
	 * @param callData
	 * @throws Exception
	 */
	public static void parseACtcapInitialQuery(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseACtcapInitialQuery");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			decodeTcapInitialQuery(callData, input);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseACtcapInitialQuery " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: No Translation for the specific address.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] parseACTtcapInitialQuery parsed successfully");
		}
	}

	/**
	 * method used to decode Tcap query & set destination number to callData
	 * attribute
	 * 
	 * @param callData
	 * @param input
	 * @throws Exception
	 */
	public static void decodeTcapInitialQuery(CallData callData, byte[] input) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("Inside decodeTcapInitialQuery, serviceId");
			logger.debug("decodeTcapInitialQuery: input buffer " + CommonUtils.formatBytes(input));
		}

		int bufferIndex = 0;
		if (input[bufferIndex++] != MRSConstants.IQM_PARAMETER_SET_ID) {// 0xF2
			logger.error("decodeTcapInitialQuery: Parameter Set Identifier is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeTcapInitialQuery: Parameter Set Identifier is not correct");
		}

		if (input[bufferIndex++] != MRSConstants.IQM_PARAMETER_SET_ID_LEN) {// 0x18
			logger.error("decodeTcapInitialQuery: The length of Parameter Set Identifier must be 24 octets :  "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of the Parameter Set Identifier must be 24 octets ");
		}

		// service key identifier
		if (input[bufferIndex++] != MRSConstants.IQM_SERVICE_KEY_ID) {// 0xAA
			logger.error(
					"decodeTcapInitialQuery: Service Key Identifier is not correct: " + CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeTcapInitialQuery: Service Key Identifier is not correct");
		}

		if (input[bufferIndex++] != MRSConstants.IQM_SERVICE_KEY_ID_LEN) {// 0x16
			logger.error("decodeTcapInitialQuery: The length of Service Key Identifier must be 22 octets :  "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of the Service Key Identifier must be 22 octets ");
		}

		// Destination Number Digits parsing
		if (input[bufferIndex++] != MRSConstants.IQM_DEST_NUM_DIGITS_ID) {// 0x84
			logger.error("decodeTcapInitialQuery: Destination Number Digits Identifier is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeTcapInitialQuery: Destination Number Digits Identifier is not correct");
		}

		if (input[bufferIndex++] != MRSConstants.IQM_DEST_NUM_DIGITS_ID_LEN) {// 0x09
			logger.error(
					"decodeTcapInitialQuery: The length of Destination Number Digits Identifier must be 9 octets :  "
							+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of the Destination Number Digits Identifier must be 9 octets ");
		}

		if (input[bufferIndex++] != MRSConstants.IQM_DEST_NUM_TYPE_OF_DIGITS) {// 0x06
			logger.error("decodeTcapInitialQuery:(Destination Number) Type of Digits is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeTcapInitialQuery: (Destination Number) Types of Digits is not correct");
		}

		int natureOfDigits = input[bufferIndex++] & 0xFF;
		if (natureOfDigits != MRSConstants.IQM_DEST_NUM_NATURE_OF_NUM_P || natureOfDigits != MRSConstants.IQM_DEST_NUM_NATURE_OF_NUM_NP) {// 0x00 or 0x01
			logger.error("decodeTcapInitialQuery:(Destination Number) Nature of Number is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeTcapInitialQuery: (Destination Number) Nature of Number is not correct");
		}

		if (input[bufferIndex++] != MRSConstants.IQM_DEST_NUM_NUMBERING_PLAN) {// 0x21
			logger.error("decodeTcapInitialQuery:(Destination Number) Numbering Plan is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeTcapInitialQuery: (Destination Number) Numbering Plan is not correct");
		}

		int digitsLen = input[bufferIndex++] & 0xFF;
		int arrSize;
		if (digitsLen % 2 == 0) {
			arrSize = (digitsLen / 2);
		} else {
			arrSize = (digitsLen + 1) / 2;
		}

		AinDigits ainDigits = new AinDigits();
		byte[] destNumberByte = new byte[arrSize];
		
		for (int i = 0; i < destNumberByte.length; i++) {
			destNumberByte[i] = input[bufferIndex++];
		}
		ainDigits.setAddrSignal( AddressSignal.decodeAdrsSignal(destNumberByte, 0, 0));
		PhoneNumber phNumber = AinScfProtocolParser.parseAinDigits(ainDigits, natureOfDigits);
		callData.set(CallDataAttribute.P_DESTINATION_NUMBER, phNumber);//set it in global variable
		
		if (logger.isDebugEnabled()) {
			logger.debug("exit decodeTcapInitialQuery");
		}
	}
	
	/**
	 * method used to parse ISVM tcap initial query
	 * @param invokeIndEvent
	 * @param callData
	 * @throws Exception
	 */
	public static void parseISVMtcapInitialQuery(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseISVMtcapInitialQuery");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			decodeISVMtcapInitialQuery(callData, input);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseISVMtcapInitialQuery " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: No Translation for the specific address.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] parseISVMtcapInitialQuery parsed successfully");
		}
	}

	
	/**
	 * method used to decode Tcap query & set destination number to callData
	 * attribute
	 * 
	 * @param callData
	 * @param input
	 * @throws Exception
	 */
	public static void decodeISVMtcapInitialQuery(CallData callData, byte[] input) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("Inside decodeISVMtcapInitialQuery, serviceId");
			logger.debug("decodeISVMtcapInitialQuery: input buffer " + CommonUtils.formatBytes(input));
		}

		int bufferIndex = 0;
		if (input[bufferIndex++] != MRSConstants.IQM_PARAMETER_SET_ID) {// 0xF2
			logger.error("decodeISVMtcapInitialQuery: Parameter Set Identifier is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeISVMtcapInitialQuery: Parameter Set Identifier is not correct");
		}
		//skip parameter_set_id_len
        bufferIndex++;

		// service key identifier
		if (input[bufferIndex++] != MRSConstants.IQM_SERVICE_KEY_ID) {// 0xAA
			logger.error(
					"decodeISVMtcapInitialQuery: Service Key Identifier is not correct: " + CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeISVMtcapInitialQuery: Service Key Identifier is not correct");
		}
		//skip service key len
		bufferIndex++;

		// Destination Number Digits parsing
		if (input[bufferIndex++] != MRSConstants.IQM_DEST_NUM_DIGITS_ID) {// 0x84
			logger.error("decodeISVMtcapInitialQuery: Destination Number Digits Identifier is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeISVMtcapInitialQuery: Destination Number Digits Identifier is not correct");
		}

		if (input[bufferIndex++] != MRSConstants.IQM_DEST_NUM_DIGITS_ID_LEN) {// 0x09
			logger.error(
					"decodeISVMtcapInitialQuery: The length of Destination Number Digits Identifier must be 9 octets :  "
							+ CommonUtils.formatBytes(input));
			throw new AINCodecException("The length of the Destination Number Digits Identifier must be 9 octets ");
		}

		if (input[bufferIndex++] != MRSConstants.IQM_DEST_NUM_TYPE_OF_DIGITS) {// 0x06
			logger.error("decodeISVMtcapInitialQuery:(Destination Number) Type of Digits is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeISVMtcapInitialQuery: (Destination Number) Types of Digits is not correct");
		}

		int natureOfDigits = input[bufferIndex++] & 0xFF;
		if (natureOfDigits != MRSConstants.IQM_DEST_NUM_NATURE_OF_NUM_NP) {// 0x00
			logger.error("decodeISVMtcapInitialQuery:(Destination Number) Nature of Number is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeISVMtcapInitialQuery: (Destination Number) Nature of Number is not correct");
		}

		if (input[bufferIndex++] != MRSConstants.ISVM_DEST_NUM_NUMBERING_PLAN) {// 0x11
			logger.error("decodeISVMtcapInitialQuery:(Destination Number) Numbering Plan is not correct: "
					+ CommonUtils.formatBytes(input));
			throw new AINCodecException("decodeISVMtcapInitialQuery: (Destination Number) Numbering Plan is not correct");
		}

		int digitsLen = input[bufferIndex++] & 0xFF;
		int arrSize;
		if (digitsLen % 2 == 0) {
			arrSize = (digitsLen / 2);
		} else {
			arrSize = (digitsLen + 1) / 2;
		}

		AinDigits ainDigits = new AinDigits();
		byte[] destNumberByte = new byte[arrSize];
		
		for (int i = 0; i < destNumberByte.length; i++) {
			destNumberByte[i] = input[bufferIndex++];
		}
		ainDigits.setAddrSignal( AddressSignal.decodeAdrsSignal(destNumberByte, 0, 0));
		PhoneNumber phNumber = AinScfProtocolParser.parseAinDigits(ainDigits, natureOfDigits);
		callData.set(CallDataAttribute.P_DESTINATION_NUMBER, phNumber);
		
		if (logger.isDebugEnabled()) {
			logger.debug("exit decodeISVMtcapInitialQuery");
		}
	}
	
	
	/**
	 * method used to parse Dequeue Call Unidirectional Message
	 * 
	 * @param invokeIndEvent
	 * @param callData
	 * @throws Exception
	 */
	public static void parseDequeueCallMessage(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseDequeueCallMessage");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			decodeTcapInitialQuery(callData, input);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseDequeueCallMessage " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: No Translation for the specific address.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] parseDequeueCallMessage parsed successfully");
		}
	}

}
