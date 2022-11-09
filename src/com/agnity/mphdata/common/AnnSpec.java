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
package com.agnity.mphdata.common;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

/**
 * 
 * This Class is used by application to specify announcemnt specs . This
 * is used to play announcemnt via MS
 *
 */

public class AnnSpec implements Serializable, Cloneable{

	private static Logger logger = Logger.getLogger(AnnSpec.class);
	//Available Announcement types
	public static enum ANN_TYPE {
		ANN, VAR
	};

	public static final String	ANN_LANG_EN_US		= "en_US".intern();
	public static final String	ANN_LANG_JA_JP		= "ja_JP".intern();
	
	public static final String	ANN_LANG_JA_JP1		= "ja_JP1".intern();
	public static final String	ANN_LANG_JA_JP2		= "ja_JP2".intern();

	// Possible announcements TYPES and SUBTYPES
	//Date
	public static final String	ANN_TYPE_DAT		= "date";
	public static final String	ANN_SUBTYPE_MDY		= "mdy";
	public static final String	ANN_SUBTYPE_DMY		= "dmy";
	public static final String	ANN_SUBTYPE_YMD		= "ymd";

	//Digit
	public static final String	ANN_TYPE_DIG		= "digit";
	public static final String	ANN_SUBTYPE_GEN		= "gen";
	public static final String	ANN_SUBTYPE_NDN		= "ndn";

	//Duration
	public static final String	ANN_TYPE_DUR		= "duration";

	//Money
	public static final String	ANN_TYPE_MNY		= "money";
	public static final String	ANN_SUBTYPE_USD		= "USD";

	//Month
	public static final String	ANN_TYPE_MONTH		= "month";

	//Number
	public static final String	ANN_TYPE_NUM		= "number";
	public static final String	ANN_SUBTYPE_CRD		= "crd";
	public static final String	ANN_SUBTYPE_ORD		= "ord";

	//Silence
	public static final String	ANN_TYPE_SILENCE	= "silence";

	//String
	public static final String	ANN_TYPE_STRING		= "string";

	//Time
	public static final String	ANN_TYPE_TIME		= "time";
	public static final String	ANN_SUBTYPE_12HR	= "t12";
	public static final String	ANN_SUBTYPE_24HR	= "t24";

	//Weekday
	public static final String	ANN_TYPE_WKD		= "weekday";

	//Play and Play-Collect related parameters
	private List<PlayMessage>	playMsgList;

	private String				annLanguage;
	private int					minDigits;
	private int					maxDigits;
	private int					firstDigitTimer;
	private int					interDigitTimer;
	private int                 extraDigitTimer;
	private boolean				clearDigitBuffer;
	private String				terminationKey;
	private String				esacpeKey;
	private int					annIteration;
	private int					annLength;

	//Record announcement property
	private int					maxRecordingTime;
	private String				recordAnnPath;

	private boolean barge =true;
	
	//Flag for FlexParameterBlock choice type in STR.
	private boolean flex = false;
	
	/**
	 * Will be set true if user wants to play Infinite Ann.
	 * Attribute is added to send event to service to notify about announcement.
	 */
	private boolean isInfiniteAnn; 
	
	
	/**
	 * DTMF key mapping for AIN
	 * IgnoreDigits    -->ignoreDigits
	 * TerminateDigits -->terminationKey
	 * ResetDigits     -->esacpeKey
	 * BackspaceDigits -->bkspcDigits
	 */
	
	private String				ignoreDigits;
	private String				backspaceDigits;


	public boolean isBarge() {
		return barge;
	}

	public void setBarge(boolean barge) {
		this.barge = barge;
	}
	
	/**
	 * method will return true or false for flex flag
	 * default value set as false.
	 * @return
	 */

	public boolean isFlex() {
		return flex;
	}

	/**
	 * setter method for flex flag
	 * @param flex
	 */
	public void setFlex(boolean flex) {
		this.flex = flex;
	}



	public class PlayMessage implements Serializable {
		private static final long	serialVersionUID	= -1148662808946669796L;

		private String				messageId;
		private ANN_TYPE			annType;
		//Variable announcement properties
		private String				varAnnType;
		private String				varAnnSubType;
		private String				varAnnLang;

		public PlayMessage(String messageId, ANN_TYPE annType) {
			super();
			this.setMessageId(messageId);
			this.setAnnType(annType);
		}
		
		public String getVarAnnLang() {
			return varAnnLang;
		}

		public void setVarAnnLang(String varAnnLang) {
			this.varAnnLang = varAnnLang;
		}

		public String getVarAnnSubType() {
			return varAnnSubType;
		}

		public void setVarAnnSubType(String varAnnSubType) {
			this.varAnnSubType = varAnnSubType;
		}

		public String getVarAnnType() {
			return varAnnType;
		}

		public void setVarAnnType(String varAnnType) {
			this.varAnnType = varAnnType;
		}

		/**
		 * @param messageId
		 *            the messageId to set
		 */
		public void setMessageId(String messageId) {
			this.messageId = messageId;
		}

		/**
		 * @return the messageId
		 */
		public String getMessageId() {
			return messageId;
		}

		/**
		 * @param annType
		 *            the annType to set
		 */
		public void setAnnType(ANN_TYPE annType) {
			this.annType = annType;
		}

		/**
		 * @return the annType
		 */
		public ANN_TYPE getAnnType() {
			return annType;
		}

	}

	public AnnSpec() {
		clearDigitBuffer = true;
		playMsgList = new ArrayList<PlayMessage>();
	}

	public void addMessage(String messageId, ANN_TYPE annType) {
		if (messageId != null && !messageId.isEmpty()) {
			PlayMessage playMessage = new PlayMessage(messageId, annType);
			playMsgList.add(playMessage);
		}
	}

	public void addVariableMessage(String message, String varAnnType, String varAnnSubType) {
		if (message != null && !message.isEmpty() && varAnnType != null && !varAnnType.isEmpty()) {
			PlayMessage playMessage = new PlayMessage(message, ANN_TYPE.VAR);
			playMessage.setVarAnnType(varAnnType);
			playMessage.setVarAnnSubType(varAnnSubType);
			playMsgList.add(playMessage);
		}
	}
	
	public void addVariableMessage(String message, String varAnnType, String varAnnSubType, String varAnnLang) {
		if (message != null && !message.isEmpty() && varAnnType != null && !varAnnType.isEmpty()) {
			PlayMessage playMessage = new PlayMessage(message, ANN_TYPE.VAR);
			playMessage.setVarAnnType(varAnnType);
			playMessage.setVarAnnSubType(varAnnSubType);
			playMessage.setVarAnnLang(varAnnLang);
			playMsgList.add(playMessage);
		}
	}

	public String getAnnLanguage() {
		return annLanguage;
	}

	public void setAnnLanguage(String annLanguage) {
		this.annLanguage = annLanguage;
	}

	public int getMinDigits() {
		return minDigits;
	}

	public void setMinDigits(int minDigits) {
		this.minDigits = minDigits;
	}

	public int getMaxDigits() {
		return maxDigits;
	}

	public void setMaxDigits(int maxDigits) {
		this.maxDigits = maxDigits;
	}

	public int getFirstDigitTimer() {
		return firstDigitTimer;
	}

	public void setFirstDigitTimer(int firstDigitTimer) {
		this.firstDigitTimer = firstDigitTimer;
	}

	public int getInterDigitTimer() {
		return interDigitTimer;
	}

	public void setInterDigitTimer(int interDigitTimer) {
		this.interDigitTimer = interDigitTimer;
	}

	public boolean isClearDigitBuffer() {
		return clearDigitBuffer;
	}

	public void setClearDigitBuffer(boolean clearDigitBuffer) {
		this.clearDigitBuffer = clearDigitBuffer;
	}

	public String getTerminationKey() {
		return terminationKey;
	}

	public void setTerminationKey(String terminationKey) {
		this.terminationKey = terminationKey;
	}

	public String getEsacpeKey() {
		return esacpeKey;
	}

	public void setEsacpeKey(String esacpeKey) {
		this.esacpeKey = esacpeKey;
	}

	public int getAnnIteration() {
		return annIteration;
	}

	public void setAnnIteration(int annIteration) {
		this.annIteration = annIteration;
	}

	public int getAnnLength() {
		return annLength;
	}

	public void setAnnLength(int annLength) {
		this.annLength = annLength;
	}

	/**
	 * @return the maxRecordingTime
	 */
	public int getMaxRecordingTime() {
		return maxRecordingTime;
	}

	/**
	 * @param maxRecordingTime
	 *            the maxRecordingTime to set
	 */
	public void setMaxRecordingTime(int maxRecordingTime) {
		this.maxRecordingTime = maxRecordingTime;
	}

	/**
	 * @return the recordAnnPath
	 */
	public String getRecordAnnPath() {
		return recordAnnPath;
	}

	/**
	 * @param recordAnnPath
	 *            the recordAnnPath to set
	 */
	public void setRecordAnnPath(String recordAnnPath) {
		this.recordAnnPath = recordAnnPath;
	}

	public List<PlayMessage> getPlayMsgList() {
		return playMsgList;
	}

	public boolean isInfiniteAnn() {
		return isInfiniteAnn;
	}
	
	public void setInfiniteAnn(boolean isInfiniteAnn) {
		this.isInfiniteAnn = isInfiniteAnn;
	}
	
	/**
	 * Getters method for ignoreDigits
	 * @return
	 */
	public String getIgnoreDigits() {
		return ignoreDigits;
	}

	/**
	 * Setters for ignoreDigits
	 * @param ignoreDigits
	 */
	public void setIgnoreDigits(String ignoreDigits) {
		this.ignoreDigits = ignoreDigits;
	}
	
	/**
	 * Getters method for backspaceDigits
	 * @return
	 */
	public String getBackspaceDigits() {
		return backspaceDigits;
	}
	
	/**
	 *  Setters for backspaceDigits.
	 * @param backspaceDigits
	 */
	public void setBackspaceDigits(String backspaceDigits) {
		this.backspaceDigits = backspaceDigits;
	}


	public void setPlayMsgList(List<PlayMessage> playMsgList) {
		this.playMsgList = playMsgList;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("AnnSpec [annIteration=");
		builder.append(annIteration);
		builder.append(", annLanguage=");
		builder.append(annLanguage);
		builder.append(", annLength=");
		builder.append(annLength);
		builder.append(", clearDigitBuffer=");
		builder.append(clearDigitBuffer);
		builder.append(", esacpeKey=");
		builder.append(esacpeKey);
		builder.append(", firstDigitTimer=");
		builder.append(firstDigitTimer);
		builder.append(", interDigitTimer=");
		builder.append(interDigitTimer);
		builder.append(", extraDigitTimer=");
		builder.append(extraDigitTimer);
		builder.append(", maxDigits=");
		builder.append(maxDigits);
		builder.append(", maxRecordingTime=");
		builder.append(maxRecordingTime);
		builder.append(", minDigits=");
		builder.append(minDigits);
		builder.append(", playMsgList=");
		builder.append(playMsgList);
		builder.append(", recordAnnPath=");
		builder.append(recordAnnPath);
		builder.append(", terminationKey=");
		builder.append(terminationKey);
		builder.append(", isInfiniteAnn");
		builder.append(isInfiniteAnn);
		builder.append(", ignoreDigits=");
		builder.append(ignoreDigits);
		builder.append(", backspaceDigits=");
		builder.append(backspaceDigits);
		builder.append("]");
		return builder.toString();
	}

	public int getExtraDigitTimer() {
		return extraDigitTimer;
	}

	public void setExtraDigitTimer(int extraDigitTimer) {
		this.extraDigitTimer = extraDigitTimer;
	}
	
	@Override
	public AnnSpec clone() {
		//AnnSpec as = new AnnSpec();
		AnnSpec as = null;
		try{
			as = (AnnSpec) super.clone();
			as.playMsgList = new ArrayList<AnnSpec.PlayMessage>(this.playMsgList); 
			
		}catch(CloneNotSupportedException ex){
			logger.error("Failed to clone AnnSpec :" + ex);
		}
		return as;
	}

}
