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
import java.util.HashMap;

import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.util.Util;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.ainscf.AinScfProtocolConfig;
import com.agnity.ph.ainscf.AinScfProtocolParser;
import com.agnity.ph.ainscf.AinScfProtocolUtil;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import java.math.BigInteger;

/**
 * @author rarya
 * @author Vikas
 *
 */
public class GenericNameQuery {
	private static Logger logger = Logger.getLogger(GenericNameQuery.class);
	private static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
			'f' };

	public static final HashMap<String, Integer> extendedUnicodeHexMap = new HashMap<String, Integer>() {{
		   put("00c7",128);
		   put("00fc",129);
		   put("00e9",130);
		   put("00e2",131);
		   put("00e4",132);
		   put("00e0",133);
		   put("00e5",134);
		   put("00e7",135);
		   put("00ea",136);
		   put("00eb",137);
		   put("00e8",138);
		   put("00ef",139);
		   put("00ee",140);
		   put("00ec",141);
		   put("00c4",142);
		   put("00c5",143);
		   put("00c9",144);
		   put("00e6",145);
		   put("00c6",146);
		   put("00f4",147);
		   put("00f6",148);
		   put("00f2",149);
		   put("00fb",150);
		   put("00f9",151);
		   put("00ff",152);
		   put("00d6",153);
		   put("00dc",154);
		   //put("00a2",155);
		   put("00f8",155);//ascii table
		   put("00a3",156);
		   //put("00a5",157);
		   put("00d8",157);//ascii table
		   put("20a7",158);
		   put("00d7",158);//ascii table
		   put("0192",159);
		   put("00e1",160);
		   put("00ed",161);
		   put("00f3",162);
		   put("00fa",163);
		   put("00f1",164);
		   put("00d1",165);
		   put("00aa",166);
		   put("00ba",167);
		   put("00bf",168);
		   put("2310",169);
		   put("00ae",169);//ascii table
		   put("00ac",170);
		   put("00bd",171);
		   put("00bc",172);
		   put("00a1",173);
		   put("00ab",174);
		   put("00bb",175);
		   put("2591",176);
		   put("2292",177);
		   put("2593",178);
		   put("2502",179);
		   put("2524",180);
		   put("2561",181);
		   put("00c1",181);//ascii table
		   put("2562",182);
		   put("00c2",182);//ascii table
		   put("2556",183);
		   put("00c0",183);//ascii table
		   put("2555",184);
		   put("00a9",184);
		   put("2563",185);
		   put("2551",186);
		   put("2557",187);
		   put("255d",188);
		   put("255c",189);
		   put("00a2",189);//ascii table
		   put("255b",190);
		   put("00a5",190);//ascii table
		   put("2510",191);
		   put("2514",192);
		   put("2534",193);
		   put("252c",194);
		   put("251c",195);
		   put("2500",196);
		   put("253c",197);
		   put("255e",198);
		   put("00e3",198);//ascii table
		   put("255f",199);
		   put("00c3",199);//ascii table
		   put("255a",200);
		   put("2554",201);
		   put("2569",202);
		   put("2566",203);
		   put("2560",204);
		   put("2550",205);
		   put("256c",206);
		   put("2567",207);
		   put("00a4",207);//ascii table
		   put("2568",208);
		   put("00f0",208);//ascii table
		   put("2564",209);
		   put("00d0",209);//ascii table
		   put("2565",210);
		   put("00ca",210);//ascii table
		   put("2559",211);
		   put("00cb",211);//ascii table
		   put("2558",212);
		   put("00c8",212);//ascii table
		   put("2552",213);
		   put("0131",213);//ascii table
		   put("2553",214);
		   put("00cd",214);//ascii table
		   put("256b",215);
		   put("00ce",215);//ascii table
		   put("256a",216);
		   put("00cf",216);//ascii table
		   put("2518",217);
		   put("250c",218);
		   put("2588",219);
		   put("2584",220);
		   put("258c",221);
		   put("00a6",221);//ascii table
		   put("2590",222);
		   put("00cc",222);//ascii table
		   put("2580",223);
		   put("03b1",224);
		   put("00d3",224);//ascii table
		   put("00df",225);
		   put("0393",226);
		   put("00d4",226);//ascii table
		   put("03c0",227);
		   put("00d2",227);//ascii table
		   put("03a3",228);
		   put("00f5",228);//ascii table
		   put("03c3",229);
		   put("00d5",229);
		   put("00b5",230);
		   put("03c4",231);
		   put("00fe",231);//ascii table
		   put("03a6",232);
		   put("00de",232);//ascii table
		   put("0398",233);
		   put("00da",233);//ascii table
		   put("03a9",234);
		   put("00db",234);//ascii table
		   put("03b4",235);
		   put("00d9",235);//ascii table
		   put("221e",236);
		   put("00fd",236);//ascii table
		   put("03c6",237);
		   put("00dd",237);//ascii table
		   put("03b5",238);
		   put("00af",238);//ascii table
		   put("2229",239);
		   put("00b4",239);//ascii table
		   put("2261",240);
		   put("00b1",241);
		   put("2265",242);
		   put("2017",242);//ascii table
		   put("2264",243);
		   put("00be",243);//ascii table
		   put("2320",244);
		   put("00b6",244);//ascii table
		   put("2321",245);
		   put("00a7",245);//ascii table
		   put("00f7",246);
		   put("2248",247);
		   put("00b8",247);//ascii table
		   put("00b0",248);
		   put("2219",249);
		   put("00a8",249);//ascii table
		   put("00b7",250);
		   put("221a",251);
		   put("00b9",251);//ascii table
		   put("207f",252);
		   put("00b3",252);//ascii table
		   put("00b2",253);
		   put("25a0",254);
		   put("00a0",255);
		}};
	private GenericNameQuery() {

	}

	/*
	 * test data for decodeGenericNameQuery
	 * 
	 * byte[] testBuffer = { (byte) 0xF2, //0 (byte) 0xF2, //1 (byte) 0x97, 0x00,
	 * (byte) 0xAA, (byte) 0x84, (byte) 0x84, 0x09, (byte) 0x0B,0x02, 0x11 , 0x0A,
	 * 0x18, 0x17, 0x21, (byte) 0x81, 0x76, (byte) 0xDF ,(byte) 0xC1 ,0x25, 0x05,
	 * 0x00, (byte) 0x61,(byte) 0x46,(byte) 0x62,(byte) 0x64, (byte)0x06, (byte)
	 * 0xFF, (byte) 0xDF, 0x01, 0x01 };
	 * 
	 * test data for encodeGenericNameQuery CallData callData = new CallData();
	 * LegData legData = new LegData();
	 * legData.set(LegDataAttributes.P_GN_M_GENERIC_NAME, "VikasSingh" );
	 * legData.set(LegDataAttributes.P_GN_M_GENERIC_NAME_AVAILABILITY,0 );
	 * legData.set(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION, 1 );
	 * legData.set(LegDataAttributes.P_GN_M_ACG_GAP, 128 );
	 * legData.set(LegDataAttributes.P_GN_M_ACG_DURATION, 256); PhoneNumber
	 * phCalling = new PhoneNumber(); phCalling.setAddress("123456");
	 * legData.set(LegDataAttributes.P_GN_M_CALLING_PARTY, phCalling);
	 * callData.set(CallDataAttribute.P_LEG2, legData);
	 * System.out.println(CommonUtils.formatBytes(encodeGenericName(callData)));
	 * 
	 */

	/**
	 * method to decode GenericNameQuery
	 * 
	 * @param CallData callData, byte[] input
	 * @throws AINCodecException, InvalidInputException,
	 *                            ParameterOutOfRangeException
	 */
	public static void decodeGenericNameQuery(CallData callData, byte[] input)
			throws AINCodecException, InvalidInputException, ParameterOutOfRangeException {

		if (logger.isInfoEnabled()) {
			logger.info("decodeGenericNameQuery:Enter::" + Util.formatBytes(input));
			logger.info("input buffer length::" + input.length);

		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		legData.set(LegDataAttributes.P_LIDB_QUERY_TYPE, LidBQueryType.GN.name());

		if (logger.isInfoEnabled()) {
			logger.info("GenericNameQuery:set P_LIDB_QUERY_TYPE ==GN");
		}

		// Parameter Set ID
		int currentIndex = 0;
		int currentElementIdLen = 1; // number of bytes of current element Identifier
		int currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[0] != (byte) 0xF2 && input[0] != (byte) 0x31) {
			logger.info("decodeGenericNameQuery:throwing exception ");
			logger.error("decodeGenericNameQuery:parameter set ID is not correct");
		//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//	throw new AINCodecException("parameter set ID is not correct");
		}

		
		
		// Generic Name ID
		currentIndex = 2;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

		if (input[2] != (byte) 0x97) {
			logger.info("decodeGenericNameQuery:throwing exception ");
			logger.error("decodeGenericNameQuery:Generic Name ID is not correct");
		//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
	//		throw new AINCodecException("Generic Name ID is not correct");
		}

		// Generic Name Length
		if (input[3] != (byte) 0x00) {
			logger.info("decodeGenericNameQuery:throwing exception ");
			logger.error("decodeGenericNameQuery:Generic Name Length is not correct");
		//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//	throw new AINCodecException("Generic Name Length is not correct");
		}
		// Service Key Id
		currentIndex = 4;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[4] != (byte) 0xAA) {
			logger.info("decodeGenericNameQuery:throwing exception ");
			logger.error("decodeGenericNameQuery:Service key ID is not correct");
		//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//	throw new AINCodecException("Service key ID is not correct");
		}

		if(input.length>7 && input.length<12) {
		// Digits ID [Calling Directory Number]
		currentIndex = 6;
		currentElementIdLen = 1; // number of bytes of current element Identifier
		currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
		if (input[6] != (byte) 0x84) {
			logger.info("decodeGenericNameQuery:throwing exception ");
			logger.error("decodeGenericNameQuery:Digits ID is not correct");
		//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//	throw new AINCodecException("Digits ID is not correct");
		}
		// Digits Length
		if (input[7] != (byte) 0x09) {
			logger.info("decodeGenericNameQuery:throwing exception ");
			logger.error("decodeGenericNameQuery:Digits Length is not correct");
		//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//	throw new AINCodecException("Digits Length is not correct");
		}

		if (input[8] != (byte) 0x0B || input[10] != (byte) 0x11) {
			logger.info("decodeGenericNameQuery:throwing exception ");
			logger.error("decodeGenericNameQuery:Type of Digits Or Numbering Plan/Encoding Scheme not correct");
	//		AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
	//		throw new AINCodecException("Type of Digits Or Numbering Plan/Encoding Scheme not correct");
		}

		// check forNOA
		if (input[9] != (byte) 0x00 && input[9] != (byte) 0x02) {
			logger.info("decodeBuffer:throwing exception ");
			logger.error("decodeBuffer:Digit[Calling Directory Number] -->NOA should be correct. ");
	//		AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
	//		throw new AINCodecException("Digit[Calling Directory Number] -->NOA should be correct.");
		}

		// check number of digits
		if (input[11] != (byte) 0x0A) {
			logger.info("decodeBuffer:throwing exception ");
			logger.error("decodeBuffer:Digit[Calling Directory Number] -->number of digits should be 10. ");
		//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//	throw new AINCodecException("Digit[Calling Directory Number] -->number of digits should be 10.");
		}
		}
		if(input.length>=12) {
		int numdigilen = input[11] & 0xFF;
		int arrsize;
		if (numdigilen % 2 == 0) {
			arrsize = (numdigilen / 2);
		} else {
			arrsize = (numdigilen + 1) / 2;
		}
		AinDigits ainDigitForGN = new AinDigits();
		byte[] inputfrDecodeAin = new byte[arrsize];
		int natureOfGNDigits = input[9] & 0xFF;
		for (int i = 0; i < inputfrDecodeAin.length; i++) {
			// adding num of digit and bcd format of digit
			inputfrDecodeAin[i] = input[12 + i];
		}
		ainDigitForGN.setAddrSignal(decodeAdrsSignalForGN(inputfrDecodeAin, 0, 0));
		PhoneNumber phNumber = AinScfProtocolParser.parseAinDigits(ainDigitForGN, natureOfGNDigits);
		legData.set(LegDataAttributes.P_CALLING_PARTY, phNumber);
		}
		if (input.length > 17 && input[17] == (byte) 0xDF) {
			logger.info("DIAL TONE SERVICE REQUESTER ID IS PRESENT");
			// Dial Tone Service Requester ID
			currentIndex = 17;
			currentElementIdLen = 3; // number of bytes of current element Identifier
			currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
			if (input[17] != (byte) 0xDF || input[18] != (byte) 0xC1 || input[19] != (byte) 0x25) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:Dial Tone Service Requester ID is not correct");
			//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//	throw new AINCodecException("Dial Tone Service Requester ID is not correct");
			}
			// Dial Tone Service Requester ID Length
			if (input[20] != (byte) 0x05) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:Dial Tone Service Requester ID Length is not correct");
			//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//	throw new ParameterOutOfRangeException("Dial Tone Service Requester ID Length is not correct");
			}

			if (input[21] != (byte) 0x09 && input[21] != (byte) 0x0A) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:Dial Tone Service Requester ID spare is not correct");
			//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//	throw new AINCodecException("Dial Tone Service Requester ID spare is not correct");
			}
			
			int dtsridContext = input[21] & 0xFF;
			int dtsridType = dtsridContext & 0x07; // extract lower 3 digits
			legData.set(LegDataAttributes.P_GN_O_DTSRID_TYPE, dtsridType);
			
			byte[] dialToneServiceRequesterIdtable = { input[22], input[23], input[24], input[25] };
			String dialToneServiceRequesterId = new String(dialToneServiceRequesterIdtable);
			legData.set(LegDataAttributes.P_GN_O_DIALTONE_SERVICE, dialToneServiceRequesterId);
		} else if (input.length > 17 && input[17] == (byte) 0x06) {
			// NEXT PARAMS ARE ACG
			legData.set(LegDataAttributes.P_GN_IS_ACG_PARAM_PRESENT, true);
			logger.info("ACG parameters present. setting P_GN_IS_PRESENT_ACG_PARAMETERS :: TRUE");
			// ACG Encountered ID
			currentIndex = 17;
			currentElementIdLen = 3; // number of bytes of current element Identifier
			currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
			if (input[17] != (byte) 0x06 || input[18] != (byte) 0xFF || input[19] != (byte) 0xDF) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:ACG Encountered ID is not correct");
		//		AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//		throw new AINCodecException("ACG Encountered ID is not correct");
			}
			// ACG Encountered Length
			if (input[20] != (byte) 0x01) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:ACG Encountered Length is not correct");
		//		AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//		throw new AINCodecException("ACG Encountered Length is not correct");
			}

			if (input[21] != (byte) 0x86) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:ACG Encountered value is not correct");
		//		AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//		throw new AINCodecException("ACG Encountered value is not correct");
			}
		}  else if (input.length > 17 && input[17] == (byte) 0x95) { //GR 1188 - Table D2
			logger.info("BUSINESS GROUP ID IS PRESENT");
			// Business Group ID
			currentIndex = 17;
			currentElementIdLen = 1; // number of bytes of current element Identifier
			currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;
			// Dial Tone Service Requester ID Length
			if (input[18] != (byte) 0x07) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:Business Group ID Length is not correct");
			//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//	throw new ParameterOutOfRangeException("Business Group ID Length is not correct");
			}
			if (input[19] != (byte) 0x01 || input[23] != (byte) 0x00 || input[24] != (byte) 0x00 || input[25] != (byte) 0x00) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:Business group parameter subfields are not correct at pos 19 or 23 or 24");
			//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
		//		throw new AINCodecException("Business group parameter subfields are not correct");
			}
			byte[] businessGroupIdentifierBytes = { input[20], input[21], input[22] };
			BigInteger tempBusGrpId = new BigInteger(businessGroupIdentifierBytes);
			int businessGroupIdTemp = tempBusGrpId.intValue() & 0xffffff;
		    String businessGroupIdentifier = new String();
		    businessGroupIdentifier = Integer.toString(businessGroupIdTemp);
			legData.set(LegDataAttributes.P_GN_O_BUSINESS_GROUP_ID, businessGroupIdentifier);

		}
		if (input.length > 26 && input[26] == (byte) 0x06) {
			// ACG PARAMETERS ARE PRESENT
			legData.set(LegDataAttributes.P_GN_IS_ACG_PARAM_PRESENT, true);
			logger.info("ACG parameters present. setting P_GN_IS_PRESENT_ACG_PARAMETERS :: TRUE");
			// ACG Encountered ID
			currentIndex = 26;
			currentElementIdLen = 3; // number of bytes of current element Identifier
			currentElementLen = input[currentIndex + currentElementIdLen] & 0xFF;

			if (input[26] != (byte) 0x06 || input[27] != (byte) 0xFF || input[28] != (byte) 0xDF) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:ACG Encountered ID is not correct");
			//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//	throw new AINCodecException("ACG Encountered ID is not correct");
			}
			// ACG Encountered Length
			if (input[29] != (byte) 0x01) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:ACG Encountered Length is not correct");
			//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//	throw new AINCodecException("ACG Encountered Length is not correct");
			}

			if (input[30] != (byte) 0x86) {
				logger.info("decodeGenericNameQuery:throwing exception ");
				logger.error("decodeGenericNameQuery:ACG Encountered value is not correct");
			//	AinScfProtocolUtil.setApplicationErrorProblemData(callData, input, currentIndex, currentElementIdLen, currentElementLen);
			//	throw new AINCodecException("ACG Encountered value is not correct");
			}
		}

		if (logger.isInfoEnabled()) {
			logger.info("decodeGenericNameQuery:Exit");
		}
	}

	/**
	 * method to encode GenericName for GenericNameQuery
	 * 
	 * @param callData
	 * @throws AINCodecException
	 */
	public static byte[] encodeGenericName(CallData callData) throws AINCodecException {
		if (logger.isDebugEnabled()) {
			logger.info("encodeGenericName:Enter");
		}

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int index = 0;
		int totalLength = 0;

		// ROW :13 14
		byte encodedParameterSetId = GnConstants.GN_PARAMETER_SET_ID;
		if (leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME) == null) {
			logger.info("encodeGNQuery: throwing exception ");
			logger.error("encodeGNQuery: generic name parameter 'char subfields' is Mandatory");
			throw new AINCodecException("generic name parameter 'char subfields' is Mandatory");
		}
		if (leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME_AVAILABILITY) == null) {
			logger.info("encodeGNQuery: throwing exception ");
			logger.error("encodeGNQuery: generic name parameter 'availability' is Mandatory");
			throw new AINCodecException("generic name parameter 'availability' is Mandatory");
		}
		if (leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION) == null) {
			logger.info("encodeGNQuery: throwing exception ");
			logger.error("encodeGNQuery: generic name parameter 'presentation' is Mandatory");
			throw new AINCodecException("generic name parameter 'presentation' is Mandatory");
		}

		// Row 15, 16, 17
		byte encodedGenericNameId = GnConstants.GN_GENERIC_NAME_ID;
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		byte encodedGenericNameLen = 1;
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		// For GN type , presentation and availability
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		String genericNameString = (String) leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME);
		if (genericNameString.length() > 15) {
			logger.info("encodeGNQuery: throwing exception ");
			logger.error("encodeGNQuery: generic name length should be less than 15");
			throw new AINCodecException("generic name length should be less than 15");
		}
		String charSubfields = genericNameString;
		if(!charSubfields.equals("0000")) {
			totalLength += charSubfields.length();
			encodedGenericNameLen += (byte) (charSubfields.length());
		}

		String[] parameterSetLength = convertHexRepersentaion(Integer.toHexString(totalLength));
		byte[] outputArray = new byte[totalLength + 2];

		outputArray[index++] = encodedParameterSetId;
		outputArray[index++] = (byte) Byte.decode(parameterSetLength[0]);
		outputArray[index++] = encodedGenericNameId;
		outputArray[index++] = encodedGenericNameLen;

		String genericName = (String) leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME);
		int gnAvailability = (Integer) leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME_AVAILABILITY);

		int presentationType = (Integer) leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION);
		int encodedGNTag = ((gnAvailability & 0x01) | 0x02) << 4;
		encodedGNTag |= (presentationType & 0x03);
		outputArray[index++] = (byte) encodedGNTag;
		String charSubfilds = genericName;
		if (charSubfilds != null && !charSubfields.equals("0000")) {
			byte[] encodedCharSubfilds = encodeIa5(charSubfilds);
			for (byte val : encodedCharSubfilds) {
				outputArray[index++] = val;
			}
		}
		if (logger.isInfoEnabled()) {
			logger.info("Exit: encodeGenericName ::" + CommonUtils.formatBytes(outputArray));
		}
		return outputArray;
	}
	/**
	 * method to encode ACG parameters for GenericNameQuery
	 * 
	 * @param callData
	 * @throws AINCodecException
	 */
	public static byte[] encodeAcgParameters(CallData callData) throws AINCodecException, InvalidInputException {

		if (logger.isDebugEnabled()) {
			logger.info("encodeAcgParameters:Enter");
		}

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		int index = 0;
		int totalLength = 0;

		// ROW 27, 28
		byte encodedParameterSetIdentifier = GnConstants.GN_PARAMETER_SET_ID;
		// Row 29, 30, 31
		byte encodedDigitIdentifierAniCAllingPartyTag1 = GnConstants.GN_DIGITS_IDENTIFIER_ANI_CALLING_PARTY_TAG1;
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		byte encodedDigitIdentifierAniCAllingPartyTag2 = GnConstants.GN_DIGITS_IDENTIFIER_ANI_CALLING_PARTY_TAG2;
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		byte encodeGnDigitsLength = GnConstants.GN_DIGITS_LENGTH;
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		PhoneNumber callingNum = null;
		if (leg2Data.get(LegDataAttributes.P_GN_M_CALLING_PARTY) != null) {
			callingNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GN_M_CALLING_PARTY);
			totalLength += 7;
		}
		byte[] encodedCallingPartyNum = null;
		if (callingNum != null) {
			if (callingNum.getAddress().length() != 6) {
				logger.info("digits [AIN calling party] length should be 6");
				throw new AINCodecException("digits [AIN calling party] length should be 6");
			}
			encodedCallingPartyNum = encodeAdrsSignalForGn(callingNum.getAddress());
		}

		
		// Row 32, 33, 34
		byte encodedGnAcgIndicatorIdTag1 = GnConstants.GN_ACG_INDICATOR_ID_TAG1;
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		byte encodedGnAcgIndicatorIdTag2 = GnConstants.GN_ACG_INDICATOR_ID_TAG2;
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		byte encodedGnAcgIndicatorLength = GnConstants.GN_ACG_INDICATOR_LENGTH;
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_ONE;

		if (leg2Data.get(LegDataAttributes.ACG_GAP_DURATION) == null
				|| leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE) == null) {
			logger.info("encodeGNQuery: throwing exception ");
			logger.error("encodeGNQuery: duration and gap values are Mandatory");
			throw new AINCodecException("duration and gap values are Mandatory");
		}

		if (logger.isDebugEnabled()) {
			logger.info("encodeAcgParameters:ACG duration and gap values are " +leg2Data.get(LegDataAttributes.ACG_GAP_DURATION) + ", " + leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
		}
		
		int duration = 0;

		if (leg2Data.get(LegDataAttributes.ACG_GAP_DURATION) != null) {
			duration = Integer.parseInt((String) leg2Data.get(LegDataAttributes.ACG_GAP_DURATION));
		}
		totalLength += GnConstants.GN_FIXED_OCTET_OF_LENGTH_THREE;

		int gap = 0;

		if (leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE) != null) {
			gap = Integer.parseInt((String) leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE));
		}

//		byte[] encodedDuration = CommonUtils.formatIntToByte((getPowerForGnACG(duration)));
//		byte[] encodedGap = CommonUtils.formatIntToByte((getPowerForGnACG(gap)));
		byte[] encodedDuration = CommonUtils.formatIntToByte(duration);
		byte[] encodedGap = CommonUtils.formatIntToByte(gap);

		String[] parameterSetLength = convertHexRepersentaion(Integer.toHexString(totalLength));
		byte[] outputArray = new byte[totalLength + 2];

		outputArray[index++] = encodedParameterSetIdentifier;
		outputArray[index++] = (byte) Byte.decode(parameterSetLength[0]);

		outputArray[index++] = encodedDigitIdentifierAniCAllingPartyTag1;
		outputArray[index++] = encodedDigitIdentifierAniCAllingPartyTag2;
		outputArray[index++] = encodeGnDigitsLength;
		outputArray[index++] = (byte) 0x02;
		outputArray[index++] = (byte) 0x02;
		outputArray[index++] = (byte) 0x12;
		outputArray[index++] = (byte) 0x06;

		if (encodedCallingPartyNum != null) {
			for (byte val : encodedCallingPartyNum) {
				outputArray[index++] = val;
			}
		}
		outputArray[index++] = encodedGnAcgIndicatorIdTag1;
		outputArray[index++] = encodedGnAcgIndicatorIdTag2;
		outputArray[index++] = encodedGnAcgIndicatorLength;

		outputArray[index++] = 0x03;
		for (byte val : encodedDuration) {
			outputArray[index++] = val;
		}
		for (byte val : encodedGap) {
			outputArray[index++] = val;
		}
		if (logger.isInfoEnabled()) {
			logger.info("Exit: encodeAcgParameters ::" + CommonUtils.formatBytes(outputArray));
		}
		return outputArray;
	}

	/**
	 * method to encode application error for GenericNameQuery
	 * 
	 * @param callData
	 */
	public static byte[] getErrorResponseForGnQuery(CallData callData) {
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
			logger.debug("Application error for GN:" +
					CommonUtils.formatBytes(buffer));
		}
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

	private static byte[] encodeIa5(String asciiVal) {
		String isExtendedASCIIEnabled= AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.ENABLE_EXTENDED_ASCII_CNAM);
		if(isExtendedASCIIEnabled.equalsIgnoreCase("1")) {
			byte con[] = asciiVal.getBytes(StandardCharsets.ISO_8859_1);
			
			for(int i=0;i< con.length;i++) {
				String hexString = Integer.toHexString(asciiVal.charAt(i) | 0x10000).substring(1);
				if(extendedUnicodeHexMap.get(hexString) != null) {
					con[i] = extendedUnicodeHexMap.get(hexString).byteValue();
				}
			}
			return con;
		}
		return asciiVal.getBytes(StandardCharsets.US_ASCII);
	}

	/**
	 * this method converts decimal number to hex byte array
	 * 
	 * @param input
	 * @return byte []
	 */
	public static byte[] decimalToBcdForGn(int num) {
		if (num < 0)
			throw new IllegalArgumentException(
					"The method decimalToBcdForGn doesn't support negative numbers." + " Invalid argument: " + num);

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
	 * this method converts string decimal number to hex byte array
	 * 
	 * @param input
	 * @return byte []
	 */
	public static byte[] decimalToBcdForGn(String input) {
		long num = Long.parseLong(input);
		if (num < 0) {
			throw new IllegalArgumentException(
					"The method decimalToBcdForGn doesn't support negative numbers." + " Invalid argument: " + num);
		}

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

	private static String decodeAdrsSignalForGN(byte[] data, int offset, int parity) throws InvalidInputException {
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

	public static char[] byteArrayToChar(byte[] data) {

		int l = data.length;

		char[] out = new char[l << 1];

		// two characters form the hex value.
		for (int i = 0, j = 0; i < l; i++) {
			out[j++] = hexcodes[(0xF0 & data[i]) >>> 4];
			out[j++] = hexcodes[0x0F & data[i]];
		}

		return out;
	}

	public static int getPowerForGnACG(int input) {
		int power = 1;
		int base = 2;
		if (input == 1) {
			return power;
		}
		while (base != input) {
			base *= 2;
			power++;
		}
		return power;
	}

	/**
	 * This method is used for encode Address Signal into byte array
	 * 
	 * @param addrSignal String addrSignal
	 * @return a byte array
	 */
	private static byte[] encodeAdrsSignalForGn(String addrSignal) throws InvalidInputException {

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
	public static void resetGenericNameQuery(CallData callData) {
		if (logger.isDebugEnabled()) {
			logger.info("resetGenericNameQuery:Enter");
		}
		
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
		
		if(legData.get(LegDataAttributes.P_GN_O_DTSRID_TYPE) != null) {
			legData.remove(LegDataAttributes.P_GN_O_DTSRID_TYPE);
		}
		
		if(legData.get(LegDataAttributes.P_GN_O_DIALTONE_SERVICE) != null) {
			legData.remove(LegDataAttributes.P_GN_O_DIALTONE_SERVICE);
		}
		
		if(legData.get(LegDataAttributes.P_GN_IS_ACG_PARAM_PRESENT) != null) {
			legData.remove(LegDataAttributes.P_GN_IS_ACG_PARAM_PRESENT);
		}
		
		if(legData.get(LegDataAttributes.P_GN_O_BUSINESS_GROUP_ID) != null) {
			legData.remove(LegDataAttributes.P_GN_O_BUSINESS_GROUP_ID);
		}
				
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		
		if(leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME) != null) {
			leg2Data.remove(LegDataAttributes.P_GN_M_GENERIC_NAME);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME_AVAILABILITY) != null) {
			leg2Data.remove(LegDataAttributes.P_GN_M_GENERIC_NAME_AVAILABILITY);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION) != null) {
			leg2Data.remove(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION);
		}
		
		if(leg2Data.get(LegDataAttributes.P_GN_M_CALLING_PARTY) != null) {
			leg2Data.remove(LegDataAttributes.P_GN_M_CALLING_PARTY);
		}
		
		if(leg2Data.get(LegDataAttributes.ACG_GAP_DURATION) != null) {
			leg2Data.remove(LegDataAttributes.ACG_GAP_DURATION);
		}
		
		if(leg2Data.get(LegDataAttributes.ACG_GAP_INTERVAL_VALUE) != null) {
			leg2Data.remove(LegDataAttributes.ACG_GAP_INTERVAL_VALUE);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE_IDENTIFIER);
		}
		
		if(leg2Data.get(LegDataAttributes.P_APP_ERR_CODE) != null) {
			leg2Data.remove(LegDataAttributes.P_APP_ERR_CODE);
		}
		
		if (logger.isDebugEnabled()) {
			logger.info("resetGenericNameQuery:Exit");
		}
	}
}
