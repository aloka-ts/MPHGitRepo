/*******************************************************************************
 *   Copyright (c) 2011 Agnity, Inc. All rights reserved.
 *   
 *   This is proprietary source code of Agnity, Inc. 
 *   
 *   Agnity, Inc. retains all intellectual property rights associated 
 *   with this source code. Use is subject to license terms.
 *   
 *   This source code contains trade secrets owned by Agnity, Inc.
 *   Confidentiality of this computer program must be maintained at 
 *   all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.ph.common;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.math.BigInteger;
import java.nio.ByteBuffer;

import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import com.agnity.ain.datatypes.AddressSignal;
import com.agnity.ain.exceptions.InvalidInputException;
import com.baypackets.ase.spi.container.SasApplicationSession;

import jain.protocol.ss7.tcap.dialogue.DialogueConstants;
import jain.protocol.ss7.tcap.dialogue.DialoguePortion;

/**
 * This class is common utlity class used by all the protocol handlers to perform
 * utility operations 
 *
 */
public class CommonUtils {
	private static int		IVR_MAGIC_NUMBER		= 23;

	private static int		IVR_MIN_GREETING_LEN	= 6;

	private static Logger	logger					= Logger.getLogger(CommonUtils.class);
	/**
	 * This method generates the hash code corresponding to the announcement id passed.
	 * @param apcGreeting represents an instance of String
	 * @return long value of hashcode
	 */
	public static long getHashCode(String apcGreeting) {
		int liGreetingLen = 0;
		if (logger.isDebugEnabled()) {
			logger.debug("Inside getHashCode()\n.Received AnnID = " + apcGreeting);
		}

		if (apcGreeting == null) {
			logger.warn("Greeting Id is NULL");
			return 0l;
		}

		liGreetingLen = apcGreeting.length();
		if (liGreetingLen > IVR_MIN_GREETING_LEN) {
			int liHashCode = 0;
			int liTmp1 = 0;
			int liTmp2 = 0;
			int liMidValue = (liGreetingLen - 6) / 2 + 6;

			for (int liCount = 6; liCount < liGreetingLen; liCount++) {
				if (liCount < liMidValue) {
					liTmp1 = ((int) apcGreeting.charAt(liCount) - '0');
					liTmp2 = liTmp1 + ((int) apcGreeting.charAt(liGreetingLen - liCount + 5) - '0');
					// liHashCode = mix (liTmp1, liTmp2, liHashCode);
					liTmp1 -= liTmp2;
					liTmp1 -= liHashCode;
					liTmp1 ^= (liHashCode >> 13);
					liTmp2 -= liHashCode;
					liTmp2 -= liTmp1;
					liTmp2 ^= (liTmp1 << 8);
					liHashCode -= liTmp1;
					liHashCode -= liTmp2;
					liHashCode ^= (liTmp2 >> 13);
					liTmp1 -= liTmp2;
					liTmp1 -= liHashCode;
					liTmp1 ^= (liHashCode >> 12);
					liTmp2 -= liHashCode;
					liTmp2 -= liTmp1;
					liTmp2 ^= (liTmp1 << 16);
					liHashCode -= liTmp1;
					liHashCode -= liTmp2;
					liHashCode ^= (liTmp2 >> 5);
					liTmp1 -= liTmp2;
					liTmp1 -= liHashCode;
					liTmp1 ^= (liHashCode >> 3);
					liTmp2 -= liHashCode;
					liTmp2 -= liTmp1;
					liTmp2 ^= (liTmp1 << 10);
					liHashCode -= liTmp1;
					liHashCode -= liTmp2;
					liHashCode ^= (liTmp2 >> 15);
				} else{
					break;
				}
			}

			liHashCode = 0;
			liHashCode += (((int) apcGreeting.charAt(2) - '0') << 28);
			liHashCode += (((int) apcGreeting.charAt(3) - '0') << 24);
			liHashCode += (((int) apcGreeting.charAt(4) - '0') << 20);
			liHashCode += (((int) apcGreeting.charAt(5) - '0') << 16);
			liHashCode += (liTmp1 % IVR_MAGIC_NUMBER) << 8;
			liHashCode += (liTmp2 % IVR_MAGIC_NUMBER);
			if (logger.isDebugEnabled()) {
				logger.debug("Final Hash Code = " + liHashCode);
			}
			return Math.abs(liHashCode);
		}

		return 0;
	}
	/**
	 * This utility method converts the  byte value to binary.
	 * @param n represents an instance of byte
	 * @return an instance od String
	 */
	public static String conversiontoBinary(byte n) {
		StringBuilder sb = new StringBuilder("00000000");

		for (int bit = 0; bit < 8; bit++) {
			if (((n >> bit) & 1) > 0) {
				sb.setCharAt(7 - bit, '1');
			}
		}
		return sb.toString();
	}

	public static final char	hexcodes[]	= { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
			'A', 'B', 'C', 'D', 'E', 'F' };
	/**
	 * Formats the byte array passed.
	 * @param data represents an instance of byte[]
	 * @return instance of String
	 */
	public static String formatBytes(byte data[]) {
		char output[] = new char[5 * (data.length)];
		int top = 0;

		for (int i = 0; i < data.length; i++) {
			output[top++] = '0';
			output[top++] = 'x';
			output[top++] = hexcodes[(data[i] >> 4) & 0xf];
			output[top++] = hexcodes[data[i] & 0xf];
			output[top++] = ' ';
		}
		return (new String(output).trim());
	}


	/**
	 * This method is used to convert byte to int
	 * @param rno
	 * @return
	 */
	public static  int formatBytesToInt(byte[] rno){

		ByteBuffer bb=ByteBuffer.wrap(rno);
		return (int)bb.getShort();
	}


	/**
	 * This method is used to convert byte to int
	 * @param rno
	 * @return
	 */
	public static  byte[] formatIntToByte(int hexOpcode){

		BigInteger bigIn=BigInteger.valueOf(hexOpcode);
		return bigIn.toByteArray();
	}
	/**
	 * This utility method converts String ASCII value to hex byte array.
	 * @param asciiVal represents an instance of String
	 * @return a byte array of hex values
	 * @throws Exception
	 */
	public static byte[] asciToHex(String asciiVal) throws Exception {
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
			if (len >= 2){
				out[j] = (byte) ((b1 << 4) | b2);
			}
			else{
				out[j] = (byte) b1;
			}
			logger
			.debug("b1: " + b1 + " b2: " + b2 + " i: " + i + " j:" + j + " out[j]: " + out[j]);
		}
		return out;
	}
	/**
	 * This utility method calculates the initial delay from the date instance passed.
	 * @param date represents an instance of Date
	 * @return long equivalent of the delay calculated
	 */
	public static long getInitialDelay(Date date) {
		SimpleDateFormat sdf = new SimpleDateFormat("HH-mm-ss-S");
		String timeStr = sdf.format(date);
		String[] timeStrArr = timeStr.split("-");
		int hours = Integer.parseInt(timeStrArr[0]);
		int minutes = Integer.parseInt(timeStrArr[1]);
		int seconds = Integer.parseInt(timeStrArr[2]);
		int msecs = Integer.parseInt(timeStrArr[3]);

		if(logger.isDebugEnabled()){
			logger.debug("Hours:Minutes:Seconds:Msecs = " + hours + ":" + minutes + ":" + seconds+ ":" + msecs);
		}

		long initialDelay = 0L;

		if (hours > 0 && hours < 4) {
			int pendingHours = 3 - hours;
			int pendingMinutes = 59 - minutes;
			int pendingSeconds = 59 - seconds;
			int pendingMsecs = 1000 - msecs;
			long delayTillFourMsecs = (pendingHours * 60 * 60 * 1000)
					+ (pendingMinutes * 60 * 1000) + (pendingSeconds * 1000) + pendingMsecs;
			initialDelay = delayTillFourMsecs + (24 * 60 * 60 * 1000);
		} else {
			int pendingHours = 23 - hours;
			int pendingMinutes = 59 - minutes;
			int pendingSeconds = 59 - seconds;
			int pendingMsecs = 1000 - msecs;
			long delayTillMornMsecs = (pendingHours * 60 * 60 * 1000)
					+ (pendingMinutes * 60 * 1000) + (pendingSeconds * 1000) + pendingMsecs;
			initialDelay = delayTillMornMsecs + (4 * 60 * 60 * 1000);
		}
		return initialDelay;
	}
	/**
	 * This is an utility method which deletes announcement file from specified location.
	 * @param filePath represents an instance of String
	 * @return boolean value of operation status
	 */	
	public static boolean deleteAnnouncement(String filePath) {
		File srcFile = null;
		if (null == filePath || filePath.isEmpty()) {
			if (logger.isDebugEnabled()) {
				logger.debug("deleteAnnouncement:: announcement location is null or blank");
			}
			return false;
		}
		try {
			srcFile = new File(filePath);
			if (!srcFile.exists()) {
				logger.warn("deleteAnnouncement:: File " + srcFile + " does not exist");
				return false;
			}
			return srcFile.delete();
		} catch (Exception e) {
			logger.warn("deleteAnnouncement:: Error in file delete", e);
			return false;
		}
	}
	/**
	 * This is an utility method which has methods to copy announcement files from source to destination location.
	 * @param srcF represents an instance of String
	 * @param dstF represents an instance of String
	 * @return boolean value of operation status
	 */
	public static boolean copyAnnouncementFile(String srcF, String dstF) {
		File srcFile = null;
		File dstFile = null;
		BufferedInputStream bis = null;
		BufferedOutputStream bos = null;

		if (null == srcF || srcF.isEmpty() || null == dstF || dstF.isEmpty()) {
			if (logger.isDebugEnabled()) {
				logger.debug("copyAnnouncementFile:: source Or destination "
						+ "announcement location is null or blank");
			}
			return false;
		}

		try {

			String srcFilePath = srcF;
			if (srcF.startsWith("file:")) {
				srcFilePath = srcF.substring(5);
			}

			String dstFilePath = dstF;
			if (dstF.startsWith("file:")) {
				dstFilePath = dstF.substring(5);
			}

			srcFile = new File(srcFilePath);
			dstFile = new File(dstFilePath);
			//Check file existence
			if (!srcFile.exists()) {
				if (logger.isDebugEnabled()) {
					logger.debug("copyAnnouncementFile:: File " + srcFile + " does not exist");
				}
				return false;
			}
			if (!(dstFile.exists())) {
				if (logger.isDebugEnabled()) {
					logger.debug("copyAnnouncementFile:: File " + dstFile + " does not exist");
				}
				return false;
			}

			bis = new BufferedInputStream(new FileInputStream(srcFile));
			bos = new BufferedOutputStream(new FileOutputStream(dstFile));

			byte[] buf = new byte[1024];
			int length;
			while ((length = bis.read(buf)) > 0) {
				bos.write(buf, 0, length);
			}
			if (logger.isDebugEnabled()) {
				logger.debug("copyAnnouncementFile:: File copied");
			}
		} catch (Exception e) {
			logger.error("Exception in copy announcement.", e);
			return false;
		} finally {
			try {
				if (null != bis){
					bis.close();
				}
			} catch (Exception ex) {
				logger.error("Error closing input streams. Error is " + ex.getMessage());
				if (logger.isInfoEnabled()) {
					logger.error("Error closing input streams.", ex);
				}
			}

			try {
				if (null != bos){
					bos.close();
				}
			} catch (Exception ex) {
				logger.error("Error closing output streams. Error is " + ex.getMessage());
				if (logger.isInfoEnabled()) {
					logger.error("Error closing output streams.", ex);
				}
			}
		}
		return true;
	}

	public static void main(String args[]) {
		long initialDelay = CommonUtils.getInitialDelay(new Date());
		System.out.println("Initial Delay = " + initialDelay);
	}


	/**
	 * This method resets the appsession idle timeout. This is required to change the appsession
	 * timeout from default 5 to some other non-zero value.
	 * @param appSession represents an instance of SipApplicationSession
	 * @param timeoutInMinutes represents integer value of timeout 
	 * @param origLegCallId represents an instance of String
	 */
	public static void setAppSessionTimeout(SipApplicationSession appSession, int timeoutInMinutes, Object origLegCallId) {
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + " :: Inside setAppSessionTimeout with value " + timeoutInMinutes);
		}
		if (timeoutInMinutes >= 0) {
			//Set session expires in minutes
			((SasApplicationSession) appSession).setTimeout(timeoutInMinutes);
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + " :: AppSession expiry time in minutes " + timeoutInMinutes);
			}
		}
	}


	/**
	 * This method returns current time in GMT format
	 * 
	 * @return String GMT format
	 */
	public String getCurrentGMTFormatTime() {
		DateFormat formatter = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
		TimeZone obj = TimeZone.getTimeZone("GMT");
		formatter.setTimeZone(obj);
		String theResult = formatter.format(new Date());
		if (logger.isDebugEnabled()) {
			logger.debug(" :: Returning current time zone is GMT format " + theResult);
		}
		return theResult;
	}

	/**
	 * Method converts byte array from the given offset to integer
	 * @param rno
	 * @param offset
	 * @return
	 */
	public static  int formatBytesToInt(byte[] rno, int offset){

		System.out.println(offset);
		ByteBuffer bb=ByteBuffer.wrap(rno, offset, rno.length-offset);
		return (int)bb.getInt();
	}

	/**
	 * Method convert Hex String to Byte array 
	 * Example  String:730000000001: Byte array:0x73 0x00 0x00 0x00 0x00 0x01
	 * @param echoDataFromLeg
	 */
	public static byte[] convertHexStringToByteArray(String echoDataFromLeg) {
		int len = echoDataFromLeg.length();
		byte[] data = null;
		if(len%2 != 0){
			echoDataFromLeg = "0" + echoDataFromLeg;
			len += 1;
		}

		data = new byte[len/2];

		for (int i = 0; i < len; i += 2) {
			data[i / 2] = (byte) ((Character.digit(echoDataFromLeg.charAt(i), 16) << 4)
					+ Character.digit(echoDataFromLeg.charAt(i+1), 16));
		}
		return data;
	}

	/**
	 * Method convert byte array to string
	 * Example Byte array:0x73 0x00 0x00 0x00 0x00 0x01 String:730000000001:
	 * @param data
	 * @return
	 */
	public static String convertByteArrayToString(byte data[]) {
		char output[] = new char[2 * (data.length)];
		int top = 0;

		for (int i = 0; i < data.length; i++) {
			output[top++] = hexcodes[(data[i] >> 4) & 0xf];
			output[top++] = hexcodes[data[i] & 0xf];
		}
		return (new String(output).trim());
	}

	/**
	 * Method is used to convert string to byte array
	 * @param s
	 * @return
	 */
	public static byte[] hexStringToByteArray(String s) {
		int len = s.length();
		byte[] data = null;
		if(len%2 != 0){
			s = "0" + s;
			len += 1;
		}

		data = new byte[len/2];

		for (int i = 0; i < len; i += 2) {
			data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4)
					+ Character.digit(s.charAt(i+1), 16));
		}
		return data;
	}

	/**
	 * This method is used to create a dialogue portion 
	 * An ITU 1992 or ITU 1996 implementation of the JAIN 
	 * TCAP API should only use the Application Context and 
	 * User Information parameters within this class. 
	 * An ANSI 1992 implementation does not include a Dialogue 
	 * Portion therefore should not use this class and an ANSI 1996 
	 * implementation may use all the parameters within this class.
	 * "AppContextName=0607040001001D03|SecurityContextInfo=998877|ConfidentialityInfo=1100|UserInfomation=3300|AppContextId=OID";
	 * Possible values for APpCOntextId = OID|INT
	 * 
	 * @param input
	 * @return
	 */
	public static DialoguePortion createDaloguePortion( String input){
		DialoguePortion dp = null;

		// if input string is null then return.
		if(!StringUtils.isNotBlank(input)){
			logger.debug("DialoguePortion: input is null");
			return dp;
		}

		String[] dialoguePart = StringUtils.split(input, "|");
		if(dialoguePart.length == 0){
			logger.debug("DialoguePortion: Input not properly delimited by |" + input);
			return dp;
		}

		byte[] appContextNameArray = null;
		byte[] userInfoArray = null;
		int appContextId = DialogueConstants.APPLICATION_CONTEXT_INTEGER;

		try {
			for(int i=0; i < dialoguePart.length; ++i){
				String [] dialogueFields = StringUtils.split(dialoguePart[i], "=");

				if(dialogueFields.length == 2){
					switch(StringUtils.lowerCase(dialogueFields[0])){
					case "appcontextname":{
						// convert hex string to byte array 
						appContextNameArray = CommonUtils.convertHexStringToByteArray(dialogueFields[1]);
						logger.debug("appContextName:" + CommonUtils.formatBytes(appContextNameArray) +
								", actual String:" + dialogueFields[1]);
					}	
					break;
					case "userinformation":{
						// convert hex string to byte array 
						userInfoArray = CommonUtils.convertHexStringToByteArray(dialogueFields[1]);
						logger.debug("userInfomation:" + CommonUtils.formatBytes(userInfoArray) + 
								", actual String:" + dialogueFields[1]);
					}
					break;
					case "appcontextid":{
						// convert hex string to byte array 
						if(StringUtils.equalsIgnoreCase(dialogueFields[1], "OID")){
							appContextId = DialogueConstants.APPLICATION_CONTEXT_OBJECT;
						}
					}
					break;
					default:{
						//logger.error("unsupported Key:" + dialogueFields[0]);
						logger.debug("unsupported Key:" + dialogueFields[0] + 
								"," + StringUtils.lowerCase(dialogueFields[0]));
					}
					break;
					}
				}else{
					logger.debug("Wrongly provided value :" + dialoguePart[i] + ", dialogueFields length:" +
							dialogueFields.length);
				}
			}
		} catch(Exception ex){
			logger.error("Exception in parsing key value" + input + "," + ex);

		}

		if(appContextNameArray != null && userInfoArray != null){
			dp = new DialoguePortion();
			dp.setAppContextName(appContextNameArray);
			dp.setUserInformation(userInfoArray);
			dp.setAppContextIdentifier(appContextId);

			logger.debug("appContextNameArray:[" + CommonUtils.formatBytes(appContextNameArray) +
					"], userinformation:[" + CommonUtils.formatBytes(appContextNameArray) + "]" +
					", appContextIdentifier:" + appContextId);
		}else{
			logger.debug("Either appContextName is null or userInfo");
		}

		logger.debug("createDaloguePortion DP:" + dp);
		return dp;
	}
	
	
	/**
	 * Method to encode Digits in BCD format
	 * @param digits
	 * @return
	 */
	public static byte[] encodeDigitsInBcdFormat(String digits){
		byte [] retVal = null;
		if(StringUtils.isNotBlank(digits)){
			try {
				retVal = AddressSignal.encodeAdrsSignal(digits);
			} catch (InvalidInputException e) {
				logger.error("encodeDigitsBcdForamt: exception in encoding:" + digits);
			}
		}

		return retVal;
	}
}
