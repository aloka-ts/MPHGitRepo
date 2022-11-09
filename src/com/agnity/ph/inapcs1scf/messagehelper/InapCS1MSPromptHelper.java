package com.agnity.ph.inapcs1scf.messagehelper;

import com.agnity.inapitutcs2.asngenerated.*;
import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.inapitutcs2.operations.InapOperationsCoding;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolConfig;
import com.google.common.base.Predicate;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import static com.agnity.mphdata.common.AnnSpec.*;
import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.getNumberInRange;
import static com.google.common.collect.Collections2.filter;

/**
 * Created by ankitsinghal on 24/10/16.
 */
public class InapCS1MSPromptHelper {
	private static Logger logger = Logger.getLogger(InapCS1MSPromptHelper.class);

	public static byte[] createPlayAnnouncement(AnnSpec annSpec) throws Exception {
		logger.debug("Input AnnSpec: " + annSpec);
		PlayAnnouncementArg playAnnouncementArg = getPlayAnnouncementArgFromAnnSpec(annSpec);
		return createMessageByteArray(InapOpCodes.PA, playAnnouncementArg);
	}

	private static PlayAnnouncementArg getPlayAnnouncementArgFromAnnSpec(AnnSpec annSpec) throws ASNParsingException {
		PlayAnnouncementArg playAnnouncementArg = new PlayAnnouncementArg();
		playAnnouncementArg.setDisconnectFromIPForbidden(true);
		playAnnouncementArg.setRequestAnnouncementComplete(true);
		try {
			if (null != annSpec) {
				playAnnouncementArg.setInformationToSend(getInformationToSend(annSpec));
			} else {
				throw new IllegalArgumentException("AnnSpec cannot be null!");
			}
		} catch (ASNParsingException asne) {
			throw new ASNParsingException(asne.getMessage(), asne.getCause(), ASNParsingException.MESSAGE.PA);
		}
		return playAnnouncementArg;
	}

	private static byte[] createMessageByteArray(String opCode, Object operationObject) throws Exception {
		LinkedList<String> opCodeList = new LinkedList<String>();
		opCodeList.add(opCode);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(operationObject);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCodeList);
		return encodeList.getFirst();
	}

	private static InformationToSend getInformationToSend(AnnSpec annSpec) throws ASNParsingException {
		InformationToSend informationToSend = new InformationToSend();

		InbandInfo inbandInfo = new InbandInfo();
		inbandInfo.setMessageID(getMessageId(annSpec));

		String sendOptionalParam = InapCS1ScfProtocolConfig.
				getConfigData(InapCS1ScfProtocolConfig.SEND_OPTIONAL_PARAMS_PA);

		if(PhConstants.TRUE.equals(sendOptionalParam)) {
			inbandInfo.setNumberOfRepetitions(getNumberInRange(1, 127, annSpec.getAnnIteration()));
			inbandInfo.setDuration(getNumberInRange(0, 32767, annSpec.getAnnLength()));
		}
		
		if(annSpec.getAnnIteration() != 0){
			inbandInfo.setNumberOfRepetitions(annSpec.getAnnIteration());
		}

		if(annSpec.getAnnLength() != 0){
			inbandInfo.setDuration(annSpec.getAnnLength());
		}

		if(logger.isDebugEnabled()){
			logger.debug("getInformationToSend: PhConstants.TRUE.equals(sendOptionalParam):" + PhConstants.TRUE.equals(sendOptionalParam)
			   + ", AnnIteraction:" + annSpec.getAnnIteration() + ", annlength:" + annSpec.getAnnLength() );
		}

		informationToSend.selectInbandInfo(inbandInfo);

		return informationToSend;
	}

	private static MessageID getMessageId(AnnSpec annSpec) throws ASNParsingException {
		MessageID messageID = new MessageID();
		Collection<AnnSpec.PlayMessage> fixedAnnouncements = getListOfAnnouncementsByType(annSpec, AnnSpec.ANN_TYPE.ANN);
		Collection<AnnSpec.PlayMessage> variableAnnouncements = getListOfAnnouncementsByType(annSpec, AnnSpec.ANN_TYPE.VAR);

		if (CollectionUtils.isNotEmpty(fixedAnnouncements)) {
			logger.debug("Found non-empty fixed announcements list.");
			if (fixedAnnouncements.size() == 1) {
				messageID.selectElementaryMessageID(getElementaryMessageIdsCollection(fixedAnnouncements).get(0));
			} else {
				messageID.selectElementaryMessageIDs(getElementaryMessageIdsCollection(fixedAnnouncements));
			}
		} else if (CollectionUtils.isNotEmpty(variableAnnouncements)) {
			logger.debug("Found non-empty variable announcements list.");
			//INAP-CS1 supports only 1 variable announcement. So getting the first argument
			setVariableMessage(messageID, variableAnnouncements.iterator().next());
		} else {
			logger.error("Neither of fix or variable announcments are set..");
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, Neither of fix or variable announcments are set.", ASNParsingException.MESSAGE.PA);
		}
		return messageID;
	}

	private static Collection<AnnSpec.PlayMessage> getListOfAnnouncementsByType(AnnSpec annSpec, final AnnSpec.ANN_TYPE annType) {
		return filter(annSpec.getPlayMsgList(), new Predicate<AnnSpec.PlayMessage>() {
			@Override
			public boolean apply(AnnSpec.PlayMessage playMessage) {
				return (playMessage.getAnnType() == annType);
			}
		});
	}

	private static LinkedList<Integer4> getElementaryMessageIdsCollection(Collection<AnnSpec.PlayMessage> playMessages) throws ASNParsingException {
		LinkedList<Integer4> messageIds = new LinkedList<Integer4>();
		try {
			for (AnnSpec.PlayMessage playMessage : playMessages) {
				messageIds.add(getElementaryMessageId(playMessage));
			}
		} catch (Exception e) {
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, PA/PAC parsing failure occured.", e, ASNParsingException.MESSAGE.PA);
		}
		return messageIds;
	}

	private static void setVariableMessage(MessageID messageID, AnnSpec.PlayMessage playMessage) throws ASNParsingException {
		MessageID.VariableMessageSequenceType variableMessageSequenceType = new MessageID.VariableMessageSequenceType();
		variableMessageSequenceType.setElementaryMessageID(getElementaryMessageId(playMessage));
		VariablePart variablePart = new VariablePart();
		String variableAnnType = playMessage.getVarAnnType();

		//TODO: Currently AnnSpec is not supporting variable announcements properly. So adding dummy data.
		if (variableAnnType.equals(ANN_TYPE_DAT)) {
			//1993 September 30th
			String testDate = "930930";
			byte[] encodedDate = BCDEncoderHelper.getBCDEncodedArray(testDate, 3);
			variablePart.selectDate(encodedDate);
		} else if (variableAnnType.equals(ANN_TYPE_MNY)) {
			//$249.50
			String sampleMoney = "24950";
			byte[] encodedMoney = BCDEncoderHelper.getBCDEncodedArray(sampleMoney, 4);
			variablePart.selectPrice(encodedMoney);
		} else if (variableAnnType.equals(ANN_TYPE_NUM)) {
			logger.debug("ANN_TYPE_NUM");
		} else if (variableAnnType.equals(ANN_TYPE_TIME)) {
			//12:15
			String sampleTime = "1215";
			byte[] encodedTime = BCDEncoderHelper.getBCDEncodedArray(sampleTime, 2);
			variablePart.selectTime(encodedTime);
		} else {
			throw new IllegalArgumentException("Unsupported variable ann type! : " + variableAnnType);
		}

		//Creating a single-value collection
		List<VariablePart> variablePartList = new LinkedList<VariablePart>();
		variablePartList.add(variablePart);
		variableMessageSequenceType.setVariableParts(variablePartList);
		messageID.selectVariableMessage(variableMessageSequenceType);
	}

	private static Integer4 getElementaryMessageId(AnnSpec.PlayMessage playMessage) throws ASNParsingException {
		Integer4 messageId = new Integer4();
		try {
			//FIXME : Currently hardcoding the announcemnets as integers. THIS Needs to be fixed.

			logger.debug("getElementaryMessageId: " + playMessage.getMessageId());
			messageId.setValue(Integer.parseInt(playMessage.getMessageId()));
			//messageId.setValue(1234234);
		} catch (Exception e) {
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, PA/PAC parsing failure occured.", e, ASNParsingException.MESSAGE.PA);
		}
		return messageId;
	}

	public static byte[] createPromptAndCollectUserInformation(AnnSpec annSpec) throws Exception {
		logger.debug("Input AnnSpec: " + annSpec);
		PromptAndCollectUserInformationArg promptAndCollectUserInformationArg = getPromptAndCollectAnnouncementArgFromAnnSpec(annSpec);
		return createMessageByteArray(InapOpCodes.PAC, promptAndCollectUserInformationArg);
	}

	private static PromptAndCollectUserInformationArg getPromptAndCollectAnnouncementArgFromAnnSpec(AnnSpec annSpec) throws ASNParsingException {
		PromptAndCollectUserInformationArg promptAndCollectUserInformationArg = new PromptAndCollectUserInformationArg();
		promptAndCollectUserInformationArg.setDisconnectFromIPForbidden(true);
		try {
			if (null != annSpec) {
				promptAndCollectUserInformationArg.setInformationToSend(getInformationToSend(annSpec));
				promptAndCollectUserInformationArg.setCollectedInfo(getCollectedInfo(annSpec));
			} else {
				throw new IllegalArgumentException("AnnSpec cannot be null!");
			}
		} catch (ASNParsingException asne) {
			throw new ASNParsingException(asne.getMessage(), asne.getCause(), ASNParsingException.MESSAGE.PA);
		}
		return promptAndCollectUserInformationArg;
	}

	private static CollectedInfo getCollectedInfo(AnnSpec annSpec) throws ASNParsingException {
		CollectedInfo collectedInfo = new CollectedInfo();

		CollectedDigits collectedDigits = new CollectedDigits();
		int maxDigits = annSpec.getMaxDigits();
		maxDigits = maxDigits > 0 ? maxDigits : PhConstants.DEFAULT_PNC_MAX_DIGIT;
		collectedDigits.setMaximumNbOfDigits(maxDigits);

		int minDigits = annSpec.getMinDigits();
		minDigits = minDigits > 0 ? minDigits : PhConstants.DEFAULT_PNC_MIN_DIGIT;
		collectedDigits.setMinimumNbOfDigits(minDigits);

		if(logger.isDebugEnabled()){
			logger.debug("setting maxDigits as :" + maxDigits + "and minDigits as :" + minDigits + "in annSpec");
		}

		if (StringUtils.isNotEmpty(annSpec.getTerminationKey())) {
			collectedDigits.setEndOfReplyDigit(annSpec.getTerminationKey().getBytes());
		}
		if (StringUtils.isNotEmpty(annSpec.getEsacpeKey())) {
			collectedDigits.setCancelDigit(annSpec.getEsacpeKey().getBytes());
		}
		collectedDigits.setFirstDigitTimeOut(annSpec.getFirstDigitTimer());
		collectedDigits.setInterDigitTimeOut(annSpec.getInterDigitTimer());
		collectedDigits.setInterruptableAnnInd(annSpec.isBarge());
		
		ErrorTreatment et = new ErrorTreatment();
		et.setValue(ErrorTreatment.EnumType.reportErrorToScf);
		collectedDigits.setErrorTreatment(et);

		collectedInfo.selectCollectedDigits(collectedDigits);
		return collectedInfo;
	}
}
