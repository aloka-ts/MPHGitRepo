package com.agnity.ph.ainscf.cain;

import com.agnity.ph.ainscf.cain.datatype.CainConstants;
import com.agnity.ain.datatypes.AddressSignal;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * This class is responsible for encoding and decoding of CAIN parameters
 * as per the doc NetworkBuilder Carrier AIN (CAIN). 
 * This class shall provide framework for encoding CAIN parameters which 
 * are sent in extension Parameter field of AnalyzeRoute. The assignment 
 * authority for CAIN is nortel and shall use following object identifier 
 *  iso member-body usa(840) nt(113533) iec(8) cain02(65) protocolDefn(16) }
 *
 */
public class CAINExtensionParameterHelper {
	private static Logger logger = Logger.getLogger(CAINExtensionParameterHelper.class);

	/**
	 * Method is used to encode CAIN parameters. Application need to provide desired 
	 * field in LEG2 parameter if to be encoded. 
	 * The 
	 * @param callData
	 * @return
	 */
	public static byte[] encodeCainParameters(CallData callData, boolean sendTreatmentcode) {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		byte[] encodedCainParam = null;
		ByteArrayOutputStream outputStream = null;

		if(logger.isDebugEnabled()){
			logger.debug(dialogueId + " Inside encodeCainParameters");
		}

		try{

			// Tag - 0x81
			// type Implicit Integer 
			byte [] finalSrvTranslationSch = null;
			if(leg2Data.get(LegDataAttributes.CAIN_SERV_TRANSLATION_SCHEME) != null){

				Integer value = (Integer) leg2Data.get(LegDataAttributes.CAIN_SERV_TRANSLATION_SCHEME);
				byte[] srvTranslationSch = CommonUtils.convertHexStringToByteArray(Integer.toHexString(value));

				outputStream = new ByteArrayOutputStream();
				outputStream.write(CainConstants.servTranslationScheme);
				outputStream.write(srvTranslationSch);
				finalSrvTranslationSch = outputStream.toByteArray();

				// replace length
				finalSrvTranslationSch[1] = (byte) srvTranslationSch.length;

				if(logger.isDebugEnabled()){
					logger.debug(dialogueId + " CAIN SrvTranslationScheme: rxed:" + value + " byte[]:" +
							CommonUtils.formatBytes(finalSrvTranslationSch));
				}
			}

			// Tag - 0x93
			// By defult the value shall be 0x0a and will be send along with RRBCSM 
			// Possible value 
			// H G F | E   D        | C        | B    A
			// Spare | TERMRTE_GNCT | RTESDONE | RTEAVAI
			// (RTEAVAIL) field shall be encoded as
			//             00 Request, 01 Ignore, 10 Next Route, 11 Reserved
			//  (RTESDONE) field shall be encoded as
			//             0 Request, 1 Ignore
			// (TERMRTE_GNCT) field shall be encoded as follows:
			//             00 Request, 01 Ignore, 10 Next route, 11 Next CAIN route
			byte [] finalNetworkBusyAction = null;
			if(leg2Data.get(LegDataAttributes.CAIN_NETWORK_BUSY_ACTION) != null){
				finalNetworkBusyAction = new byte[3];
				finalNetworkBusyAction[0] = CainConstants.networkBusyActionTag;
				finalNetworkBusyAction[1] = 0x01;
				finalNetworkBusyAction[2] = 0x0a;
			}

			// Tag - 9f 2a
			// Type - AINDigits
			byte [] billingNumber = null;
			if(leg2Data.get(LegDataAttributes.CAIN_BILLING_NUMBER) != null){
				PhoneNumber billNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.CAIN_BILLING_NUMBER);

				if (billNumber != null){
					billingNumber = encodeBillingNumber(billNumber);
				}

				if(logger.isDebugEnabled()){
					logger.debug(dialogueId + " Billing Number: " + billNumber + 
							", ByteArray:" + CommonUtils.formatBytes(billingNumber));
				}
			}
			
			// Treatment Code 
			byte [] treatmentCode = null;
			if(sendTreatmentcode && leg2Data.get(LegDataAttributes.CAIN_TREATMENT_CODE) != null){
				String tc = (String) leg2Data.get(LegDataAttributes.CAIN_TREATMENT_CODE);
				
				if(StringUtils.isNotBlank(tc)){
					treatmentCode = new byte[3];
					treatmentCode[0] = CainConstants.treatmentTag;
					treatmentCode[1] = 0x01;
					treatmentCode[2] = (byte) (Integer.parseInt(tc));
				}
			}

			// If either of the field is present then start encoding complete string
			if(finalSrvTranslationSch != null || finalNetworkBusyAction != null || billingNumber != null
					|| treatmentCode != null){
				byte[] header = CainConstants.cainExtnHeaderBytes;

				int len = 0;

				if(outputStream == null){
					outputStream = new ByteArrayOutputStream( );	
				}else{
					outputStream.reset();
				}

				outputStream.write(header);

				if(finalSrvTranslationSch != null){
					outputStream.write(finalSrvTranslationSch);
					len += finalSrvTranslationSch.length;
				}

				if(finalNetworkBusyAction != null){
					outputStream.write(finalNetworkBusyAction);
					len += finalNetworkBusyAction.length;
				}

				if(billingNumber != null){
					outputStream.write(billingNumber);
					len += billingNumber.length;
				}

				if(treatmentCode != null){
					outputStream.write(treatmentCode);
					len += treatmentCode.length;
				}
				
				encodedCainParam = outputStream.toByteArray();
				encodedCainParam[1] = (byte) len;
			}

			if(logger.isDebugEnabled()){
				logger.debug(dialogueId + " : Encoded Extension Parameters:"+ 
						((encodedCainParam != null)?(CommonUtils.formatBytes(encodedCainParam)):"Null"));
			}
		} catch(Exception exception){
			logger.error(dialogueId + ": Exception while encoding CAIN " + exception.getMessage());
		}
		return encodedCainParam;
	}

	/**
	 * Method is used to encode CAIN Billing Number 
	 * @param billNumber
	 * @return
	 * @throws InvalidInputException
	 * @throws IOException
	 */
	private static byte [] encodeBillingNumber(PhoneNumber billNumber) throws InvalidInputException, IOException{

		if(logger.isDebugEnabled()){
			logger.debug("Inside CAIN encode Billing Number" + billNumber.getAddress());
		}

		byte[] encodedBillingNum = null;

		ByteArrayOutputStream outputStream = new ByteArrayOutputStream( );		

		// encode billing number
		byte[] number = AddressSignal.encodeAdrsSignal(billNumber.getAddress());
		int len = 2 + number.length;

		if(billNumber.getAddress().length()%2 == 0){
			outputStream.write(CainConstants.billingNumEvenHead);
		}else{
			outputStream.write(CainConstants.billingNumOddHead);
		}

		// add number
		outputStream.write(number);
		encodedBillingNum = outputStream.toByteArray();

		// update length 
		encodedBillingNum[2] = (byte)len;

		if(logger.isDebugEnabled()){
			logger.debug("CAIN encode Billing Number Byte Array:" + 
					CommonUtils.formatBytes(encodedBillingNum));
		}
		return encodedBillingNum;
	}
}