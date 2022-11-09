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

package com.agnity.ph.ainscf;

import static com.agnity.ph.common.enums.PersistanceType.PERSISTABLE;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.bn.exceptions.EnumParamOutOfRangeException;
import org.bn.types.BitString;
import org.bn.types.ObjectIdentifier;

import com.agnity.ain.T1_660.CICExpansion;
import com.agnity.ain.T1_660.ConnectionControlArg;
import com.agnity.ain.T1_660.Digits;
import com.agnity.ain.T1_660.OriginatingStationType;
import com.agnity.ain.T1_660.ProviderInstructionArg;
import com.agnity.ain.asngenerated.ACGEncountered;
import com.agnity.ain.asngenerated.AINDigits;
import com.agnity.ain.asngenerated.AMAAlternateBillingNumber;
import com.agnity.ain.asngenerated.AMABusinessCustomerID;
import com.agnity.ain.asngenerated.AMADigitsDialedWC;
import com.agnity.ain.asngenerated.AMALineNumber;
import com.agnity.ain.asngenerated.AMAslpID;
import com.agnity.ain.asngenerated.AccessCode;
import com.agnity.ain.asngenerated.AlternateCarrier;
import com.agnity.ain.asngenerated.AlternateTrunkGroup;
import com.agnity.ain.asngenerated.Amp2.Amp2SequenceType;
import com.agnity.ain.asngenerated.AmpAINNodeID;
import com.agnity.ain.asngenerated.AnalyzeRouteArg;
import com.agnity.ain.asngenerated.AnnounceElement;
import com.agnity.ain.asngenerated.AnnouncementBlock;
import com.agnity.ain.asngenerated.AnnouncementDigitBlock;
import com.agnity.ain.asngenerated.AnswerIndicator;
import com.agnity.ain.asngenerated.AuthorizeTerminationArg;
import com.agnity.ain.asngenerated.BearerCapability.BearerCapabilityEnumType;
import com.agnity.ain.asngenerated.BillingIndicator;
import com.agnity.ain.asngenerated.CallInfoFromResourceArg;
import com.agnity.ain.asngenerated.CalledPartyID;
import com.agnity.ain.asngenerated.CallingPartyID;
import com.agnity.ain.asngenerated.Carrier;
import com.agnity.ain.asngenerated.ChargeNumber;
import com.agnity.ain.asngenerated.ChargePartyStationType;
import com.agnity.ain.asngenerated.CloseArg;
import com.agnity.ain.asngenerated.CloseCause.CloseCauseEnumType;
import com.agnity.ain.asngenerated.ExtensionParameter.ExtensionParameterSequenceType;
import com.agnity.ain.asngenerated.CollectedAddressInfo;
import com.agnity.ain.asngenerated.CollectedDigits;
import com.agnity.ain.asngenerated.ContinueArg;
import com.agnity.ain.asngenerated.DestinationAddress;
import com.agnity.ain.asngenerated.DisconnectArg;
import com.agnity.ain.asngenerated.DisconnectFlag;
import com.agnity.ain.asngenerated.DisplayInformation;
import com.agnity.ain.asngenerated.DisplayText;
import com.agnity.ain.asngenerated.EDPNotification;
import com.agnity.ain.asngenerated.EDPRequest;
import com.agnity.ain.asngenerated.EchoData;
import com.agnity.ain.asngenerated.ExtensionParameter;
import com.agnity.ain.asngenerated.FlexParameterBlock;
import com.agnity.ain.asngenerated.FlexParameterBlockContent;
import com.agnity.ain.asngenerated.FlexParameterBlockContent.FlexParameterBlockContentSequenceType;
import com.agnity.ain.asngenerated.ForwardCallArg;
import com.agnity.ain.asngenerated.GenericAddress;
import com.agnity.ain.asngenerated.GenericAddressList;
import com.agnity.ain.asngenerated.IPResourceType;
import com.agnity.ain.asngenerated.IPReturnBlockContent;
import com.agnity.ain.asngenerated.IPReturnBlockContent.IPReturnBlockContentSequenceType.ResultChoiceType;
//import com.agnity.ain.asngenerated.IPReturnBlockContent.IPReturnBlockContentSequenceType.ResultChoiceType;
import com.agnity.ain.asngenerated.IPStrParameterBlock;
import com.agnity.ain.asngenerated.InfoAnalyzedArg;
import com.agnity.ain.asngenerated.InfoCollectedArg;
import com.agnity.ain.asngenerated.InterruptionStatus;
import com.agnity.ain.asngenerated.Lata;
import com.agnity.ain.asngenerated.LegID;
import com.agnity.ain.asngenerated.NetworkBusyArg;
import com.agnity.ain.asngenerated.OAbandonArg;
import com.agnity.ain.asngenerated.OAnswerArg;
import com.agnity.ain.asngenerated.OCalledPartyBusyArg;
import com.agnity.ain.asngenerated.ODisconnectArg;
import com.agnity.ain.asngenerated.ONoAnswerArg;
import com.agnity.ain.asngenerated.ONoAnswerTimer;
import com.agnity.ain.asngenerated.OTermSeizedArg;
import com.agnity.ain.asngenerated.OriginalCalledPartyID;
import com.agnity.ain.asngenerated.OutpulseNumber;
import com.agnity.ain.asngenerated.OverflowBillingIndicator;
import com.agnity.ain.asngenerated.PrimaryBillingIndicator;
import com.agnity.ain.asngenerated.PrimaryTrunkGroup;
import com.agnity.ain.asngenerated.RedirectingPartyID;
import com.agnity.ain.asngenerated.RequestReportBCMEventArg;
import com.agnity.ain.asngenerated.ResourceClearArg;
import com.agnity.ain.asngenerated.ResourceEncodingAuthority;
import com.agnity.ain.asngenerated.ResourceType;
import com.agnity.ain.asngenerated.SecondAlternateCarrier;
import com.agnity.ain.asngenerated.SecondAlternateTrunkGroup;
import com.agnity.ain.asngenerated.SendNotificationArg;
import com.agnity.ain.asngenerated.SendToResourceArg;
import com.agnity.ain.asngenerated.StrParameterBlock;
import com.agnity.ain.asngenerated.StrParameterBlock.StrParameterBlockChoiceType;
import com.agnity.ain.asngenerated.TerminationAttemptArg;
import com.agnity.ain.asngenerated.TerminationIndicator;
import com.agnity.ain.asngenerated.TerminationNotificationArg;
import com.agnity.ain.asngenerated.TimeOutTimer;
import com.agnity.ain.asngenerated.TriggerCriteriaType.TriggerCriteriaTypeEnumType;
import com.agnity.ain.asngenerated.UninterAnnounceBlock;
import com.agnity.ain.asngenerated.UserID;
import com.agnity.ain.datatypes.ACGEncounteredNonAsn;
import com.agnity.ain.datatypes.AddressSignal;
import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.datatypes.CarrierFormat;
import com.agnity.ain.datatypes.ChargeNum;
import com.agnity.ain.datatypes.EDPNotificationNonAsn;
import com.agnity.ain.datatypes.EDPRequestNonAsn;
import com.agnity.ain.datatypes.GenericAddrs;
import com.agnity.ain.datatypes.T1Digits;
import com.agnity.ain.datatypes.TrunkGroup;
import com.agnity.ain.enumdata.AddPrsntRestEnum;
import com.agnity.ain.enumdata.CalgNatOfNumEnum;
import com.agnity.ain.enumdata.CallTreatIndicatorEnum;
import com.agnity.ain.enumdata.CalledNatOfNumEnum;
import com.agnity.ain.enumdata.CarrierFormatNatEnum;
import com.agnity.ain.enumdata.CarrierFormatSelectionEnum;
import com.agnity.ain.enumdata.ClgPrsntRestIndEnum;
import com.agnity.ain.enumdata.EncodingSchemeEnum;
import com.agnity.ain.enumdata.NatureOfAddCallingEnum;
import com.agnity.ain.enumdata.NatureOfNumEnum;
import com.agnity.ain.enumdata.NumPlanEnum;
import com.agnity.ain.enumdata.NumToOutpulseEnum;
import com.agnity.ain.enumdata.TestIndEnum;
import com.agnity.ain.enumdata.TypeOfAddrsEnum;
import com.agnity.ain.enumdata.TypeOfDigitEnum;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.operations.AinOpCodes;
import com.agnity.ain.operations.AinOperationsCoding;
import com.agnity.ain.util.Constant;
import com.agnity.ain.util.Util;
import com.agnity.dbnode.dialledpatternnode.DialledPatternNode;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AinCallStates;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.BillingIndicatorInfo;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CarrierInfo;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.TrunkGroupInfo;
import com.agnity.mphdata.common.Action.SEND_MODE;
import com.agnity.ph.ainscf.cain.CAINExtensionParameterHelper;
import com.agnity.ph.ainscf.lidb.AccountCode;
import com.agnity.ph.ainscf.lidb.BNSQuery;
import com.agnity.ph.ainscf.lidb.BnsConstants;
import com.agnity.ph.ainscf.lidb.CC1Query;
import com.agnity.ph.ainscf.lidb.CC2Query;
import com.agnity.ph.ainscf.lidb.TLNSQuery;
import com.agnity.ph.ainscf.lidb.GenericNameQuery;
import com.agnity.ph.ainscf.lidb.GetDataQuery;
import com.agnity.ph.ainscf.lidb.OLNSQuery;
import com.agnity.ph.ainscf.lidb.InterceptQuery;
import com.agnity.ph.ainscf.lidb.ICDCQuery;
import com.agnity.ph.ainscf.lidb.exception.AINCodecException;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
import com.agnity.ph.common.exception.CriticalityTypeException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.Parameters;

/**
 * This class provides utilities to parse and create SS7 messages
 * that are received and sent to network. It provides parsing and creation of
 * following messages and fields - InfoAnalyze - InfoCollected - Called Party
 * Number - Calling Party Number - RRBCSM - Analyze Route - Term Seize - Release
 * Cause - Busy - NoAnswer - oAnswer - Network Busy - OAbandon - ResourceClear -
 * Close - Send to Resource - CallInfoToResource - CallInfoFromResource
 * 
 * This class provides utilities to parse and create SS7 messages that
 * are received and sent to network. It provides parsing and creation of
 * following messages and fields - InfoAnalyze - InfoCollected - Called Party
 * Number - Calling Party Number - RRBCSM - Analyze Route - Term Seize - Release
 * Cause - Busy - NoAnswer - oAnswer - Network Busy - OAbandon - ResourceClear -
 * Close - Send to Resource
 * 
 * 
 * @author reeta
 *
 */
public class AinScfProtocolParser {

	private static Logger logger = Logger.getLogger(AinScfProtocolParser.class);

	/**
	 * This method is called by protocol handler to parse the infoAnalyze received
	 * and populate the callData object from the received parameters.
	 * 
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param callData       represents an instance of CallData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseInfoAnalyzed(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse InfoAnalyzed");
		}

		InfoAnalyzedArg infoAnalyzeArg;
		try {
			infoAnalyzeArg = (InfoAnalyzedArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Info analyze " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] IA parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in Info analyze " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse IA", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, Info analyze parsing failure occured.", e,
					MESSAGE.INFO_ANALYZE);
		}

		try {
			/*
			 * infoAnalyze.callingPartyNumber
			 */
			if (infoAnalyzeArg.isCallingPartyIDPresent()) {
				AinDigits callingPartyNumdigit = new AinDigits();
				AinDigits callingPartyNum = callingPartyNumdigit
						.decodeAinDigits(infoAnalyzeArg.getCallingPartyID().getValue().getValue(), Constant.CALLING);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Calling party number from Info analyze is "
							+ callingPartyNum);
				}
				if (callingPartyNum == null || callingPartyNum.getAddrSignal() == null
						|| "".equals(callingPartyNum.getAddrSignal().trim())) {
					/*
					 * call should be handled as ASn parse failure as address signal is missing
					 */
					logger.error(dialogueId + ":: [PH] Calling party num address signal missing");
					throw new ASNParsingException(dialogueId + ":: [PH] Calling party num address signal missing",
							MESSAGE.INFO_ANALYZE);
				}
				PhoneNumber callingNumber = parseCallingPartyNum(dialogueId, callingPartyNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parseCallingPartyNum, CallingNumber:" + callingNumber);
				}
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("CallingPartyNumber not present in InfoAnalyzed");
				}
			}

			/*
			 * infoAnalyze.calledPartyNumber
			 */
			if (infoAnalyzeArg.isCalledPartyIDPresent()) {

				AinDigits calliedPartyNumdigit = new AinDigits();
				AinDigits calledPartyNum = calliedPartyNumdigit
						.decodeAinDigits(infoAnalyzeArg.getCalledPartyID().getValue().getValue(), Constant.CALLED);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted called number from Info analyze is " + calledPartyNum);
				}

				PhoneNumber calledNumber = parseCalledPartyNum(dialogueId, calledPartyNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::[PH] Exit parseCalledPartyNum, calledNumber:" + calledNumber);
				}
				legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);

				if(calledPartyNum.getCalledNatOfNumEnum() != null){
					legData.set(LegDataAttributes.P_CALLED_PARTY_NOA, calledPartyNum.getCalledNatOfNumEnum().getCode());
				}
			}

			/*
			 * infoAnalyze.chargeNumber
			 */
			if (infoAnalyzeArg.isChargeNumberPresent()) {
				ChargeNumber chargeNumber = infoAnalyzeArg.getChargeNumber();

				ChargeNum chargeNum = new ChargeNum();
				chargeNum.decodeChargeNum(infoAnalyzeArg.getChargeNumber().getValue().getValue());
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Charge number from Info analyze is " + chargeNum);
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::[PH] Exit parseCharging number");
				}

				PhoneNumber chargePN = new PhoneNumber();
				chargePN.setAddress(chargeNum.getAddrSignal());
				
				if(chargeNum.getNumPlanEnum()!=null){
				chargePN.setNumberingPlan(chargeNum.getNumPlanEnum().getCode());
				}
				
				if(chargeNum.getChargeNumEnum()!=null){
				chargePN.setNatureOfAddress(chargeNum.getChargeNumEnum().getCode());
				}

				legData.set(LegDataAttributes.P_CHARGE_NUMBER, chargePN);
			}

			/*
			 * infoAnalyze.userid
			 */
			UserID uid = infoAnalyzeArg.getUserID();

			String dn = null;
			Integer facilityId = null;
			Integer trunkGroup = null;
			String adsstr = null;

			if (uid.getValue().getDn() != null) {
				dn = AddressSignal.decodeAdrsSignal(uid.getValue().getDn().getValue(), 0, 0);
			}

			if (uid.getValue().getPrivateFacilityGID() != null) {
				facilityId = uid.getValue().getPrivateFacilityGID().getValue();
			}

			if (uid.getValue().getTrunkGroupID() != null) {
				trunkGroup = uid.getValue().getTrunkGroupID().getValue();
			}

			if (uid.getValue().getADSIcpeID() != null) {
				adsstr = AddressSignal.decodeAdrsSignal(uid.getValue().getADSIcpeID().getValue(), 0, 0);
			}

			com.agnity.mphdata.common.UserID commonUser = new com.agnity.mphdata.common.UserID(dn, facilityId,
					trunkGroup, adsstr);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: [PH] Extracted user id  " + commonUser);
			}
			legData.set(LegDataAttributes.P_USER_ID, commonUser);

			/*
			 * infoAnalyze .carrier
			 */
			if (infoAnalyzeArg.isCarrierPresent()) {
				Carrier carrier = infoAnalyzeArg.getCarrier();

				CarrierFormat cf = new CarrierFormat();
				cf.decodeCarrierFormat(carrier.getValue().getValue());

				CarrierInfo ci = new CarrierInfo();
				ci.setAddress(cf.getAddrSignal());
				ci.setCarrierSelection(cf.getCarrierFormatSelectionEnum().getCode());
				ci.setNatureOfCarrier(cf.getCarrierFormatNatEnum().getCode());

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted carrier id from InfoAnalyzed " + ci);
				}

				legData.set(LegDataAttributes.P_CARRIER, ci);
			}

			/*
			 * infoAnalyze .collected address
			 */
			if (infoAnalyzeArg.isCollectedAddressInfoPresent()) {
				CollectedAddressInfo collectedAddr = infoAnalyzeArg.getCollectedAddressInfo();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(collectedAddr.getValue().getValue(), Constant.CALLED);

				PhoneNumber collectedPn = new PhoneNumber();
				collectedPn.setAddress(ainDigits.getAddrSignal());
				collectedPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted CollectedAddressInfo from InfoAnalyzed: "
							+ collectedPn.getAddress());
				}

				legData.set(LegDataAttributes.P_COLLECTED_ADDRESS_INFO, collectedPn);
			}

			/*
			 * Extract collected digits
			 */

			if (infoAnalyzeArg.isCollectedDigitsPresent()) {

				CollectedDigits collectedDigits = infoAnalyzeArg.getCollectedDigits();

				AinDigits collecDigits = AinDigits.getInstance().decodeAinDigits(collectedDigits.getValue().getValue(),
						Constant.CALLED);

				PhoneNumber collectedDigitsPn = parseCalledPartyNum(dialogueId, collecDigits);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted collectedDigits from InfoAnalyzed: "
							+ collectedDigitsPn.getAddress());
				}

				legData.set(LegDataAttributes.P_COLLECTED_DIGITS, collectedDigitsPn);
			}

			/*
			 * Extract Access Code
			 */

			if (infoAnalyzeArg.isAccessCodePresent()) {

				AccessCode accessCode = infoAnalyzeArg.getAccessCode();

				AinDigits collectedDigits = AinDigits.getInstance().decodeAinDigits(accessCode.getValue().getValue(),
						Constant.CALLED);

				PhoneNumber accessCodePn = parseCalledPartyNum(dialogueId, collectedDigits);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted AccessCode from InfoAnalyzed: "
							+ accessCodePn.getAddress());
				}

				legData.set(LegDataAttributes.P_ACCESS_CODE, accessCodePn);
			}

			/*
			 * infoAnalyze . orig called party number
			 */
			if (infoAnalyzeArg.isOriginalCalledPartyIDPresent()) {
				OriginalCalledPartyID origCallAddr = infoAnalyzeArg.getOriginalCalledPartyID();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(origCallAddr.getValue().getValue(), Constant.CALLING);

				PhoneNumber origCalledPn = new PhoneNumber();
				origCalledPn.setAddress(ainDigits.getAddrSignal());
				origCalledPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

				if(ainDigits.getCalgNatOfNumEnum() != null){
					origCalledPn.setNatureOfAddress(ainDigits.getCalgNatOfNumEnum().getCode());
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted OriginalCalledPartyID from InfoAnalyzed: "
							+ origCalledPn);
				}

				legData.set(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER, origCalledPn);
			}

			/*
			 * infoAnalyze . charge party station type
			 */
			if (infoAnalyzeArg.isChargePartyStationTypePresent()) {
				ChargePartyStationType chrgStation = infoAnalyzeArg.getChargePartyStationType();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted ChargePartyStationType from InfoAnalyzed: "
							+ chrgStation.getValue());
				}

				legData.set(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE, chrgStation.getValue());
			}

			/*
			 * infoAnalyze.redirectingPartyID
			 */
			if (infoAnalyzeArg.isRedirectingPartyIDPresent()) {
				RedirectingPartyID redirectParty = infoAnalyzeArg.getRedirectingPartyID();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(redirectParty.getValue().getValue(), Constant.CALLED);

				PhoneNumber redirectPn = new PhoneNumber();
				redirectPn.setAddress(ainDigits.getAddrSignal());
				redirectPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

				if(ainDigits.getCalledNatOfNumEnum() != null){
					redirectPn.setNatureOfAddress(ainDigits.getCalledNatOfNumEnum().getCode());
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted REDIRECTING_PARTY_ID from InfoAnalyzed: "
							+ redirectPn);
				}

				legData.set(LegDataAttributes.P_REDIRECTING_PARTY_ID, redirectPn);

			}

			/*
			 * infoAnalyze . acg encountered code
			 */
			if (infoAnalyzeArg.isACGEncounteredPresent()) {
				ACGEncountered acgEn = infoAnalyzeArg.getACGEncountered();
				ACGEncounteredNonAsn acgNonAsn = new ACGEncounteredNonAsn();
				ACGEncounteredNonAsn nonasn = acgNonAsn.decodeACGEncounteredNonAsn(acgEn.getValue());
				int acgcode = nonasn.getAcgEncounteredEnum().getCode();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted ACGEncountered from Info Analyse " + acgEn);
				}
				callData.set(CallDataAttribute.P_ACG_ENCOUNTERED, acgcode);
			}

			/*
			 * infoAnalyze - trigger criteria
			 */
			if (infoAnalyzeArg.isTriggerCriteriaTypePresent()) {
				TriggerCriteriaTypeEnumType triggerCriteriaType = infoAnalyzeArg.getTriggerCriteriaType().getValue();
				legData.set(LegDataAttributes.P_TRIGGER_CRITERIA, triggerCriteriaType.getValue().ordinal());
			}

			/*
			 * LATA
			 */
			if (infoAnalyzeArg.isLataPresent()) {
				Lata lata = infoAnalyzeArg.getLata();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(lata.getValue().getValue(), Constant.CALLED);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted LATA from Info Analyse " + ainDigits.getAddrSignal());
				}

				legData.set(LegDataAttributes.P_LATA, ainDigits.getAddrSignal());
			}

			// Vertical code 
			if(infoAnalyzeArg.isVerticalServiceCodePresent()){				
				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(infoAnalyzeArg.getVerticalServiceCode().getValue().getValue(), Constant.CALLED);

				PhoneNumber verticalCode = new PhoneNumber();

				// replace D with *
				String digits = StringUtils.replace(ainDigits.getAddrSignal(), "D", "*");
				digits = StringUtils.replace(digits, "d", "*");

				verticalCode.setAddress(digits);
				verticalCode.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::[PH] Exit parseVerticalCode, :" + verticalCode);
				}

				legData.set(LegDataAttributes.P_CALLED_PARTY, verticalCode);
			}

			// Bearer Capability 
			BearerCapabilityEnumType bearerEnumType = infoAnalyzeArg.getBearerCapability().getValue();
			String bearerValue = bearerEnumType.getValue().name();

			legData.set(LegDataAttributes.P_BEAR_CAP, bearerValue);
			if(logger.isDebugEnabled()){
				logger.debug("InfoAnalyze: BearerCapability:" + bearerValue);
			}

			// JurisdictionInformation 
			if(infoAnalyzeArg.isJurisdictionInformationPresent()){
				String jip = AddressSignal.decodeAdrsSignal(
						infoAnalyzeArg.getJurisdictionInformation().getValue(), 0, 0);
				legData.set(LegDataAttributes.P_JIP, jip);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::[PH] JurisdictionInfomation :" + jip);
				}
			}

			//AMP1
			if(infoAnalyzeArg.isAmp1Present()){
				byte[] amp1Buffer = infoAnalyzeArg.getAmp1().getValue();
				setAMP1Parameters(amp1Buffer, callData);
			}
			
			//AMP2
			if(infoAnalyzeArg.isAmp2Present()){
				Amp2SequenceType amp2Seq = infoAnalyzeArg.getAmp2().getValue();
				setAMP2Parameters(amp2Seq,callData);
			}
			
		} catch (InvalidInputException e) {
			logger.error(dialogueId + ":: Error in parseInfoAnalyze " + e.getMessage());
			throw new ASNParsingException(dialogueId + "::ASN Parsing Failure: Info analyze parsing failure occured.:",
					e, MESSAGE.INFO_ANALYZE);
		}

		catch (ASNParsingException e) {
			logger.error(dialogueId + "::[PH] Error in parseInfoAnalyze " + e.getMessage());
			throw e;
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseInfoAnalyze " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: Info analyze parsing failure occured.", e,
					MESSAGE.INFO_ANALYZE);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Info analyze parsed successfully");
		}
	}
	
	
	private static void setAMP1Parameters(byte[] amp1Buffer, CallData callData){
		
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		int index = 0;
		
		int alertCallValue = amp1Buffer[index] & 0x01;
		legData.set(LegDataAttributes.P_AMP1_ALERT_CALL, alertCallValue);
		
		if(logger.isDebugEnabled()){
			logger.debug("[PH] setAMP1Parameters  P_AMP1_ALERT_CALL :" + alertCallValue);
		}
		
		int amp1AMATreament = amp1Buffer[index] >>1 & 0x01;
		legData.set(LegDataAttributes.P_AMP1_AMA_TREATMENT, amp1AMATreament);
		
		if(logger.isDebugEnabled()){
			logger.debug("[PH] setAMP1Parameters  P_AMP1_AMA_TREATMENT :" + amp1AMATreament);
		}
		
		int cLogLevel =  amp1Buffer[index] >>2 & 0x03;
		legData.set(LegDataAttributes.P_AMP1_CLOG_LEVEL,cLogLevel);
		
		if(logger.isDebugEnabled()){
			logger.debug("[PH] setAMP1Parameters  P_AMP1_CLOG_LEVEL :" + cLogLevel);
		}
		
		index++;
		
		int msbCLogSerialNumber = amp1Buffer[index];
		index++;
		int lsbCLogSerialNumber = amp1Buffer[index];
		index++;
		//need to check 
		legData.set(LegDataAttributes.P_AMP1_CLOG_SN, msbCLogSerialNumber+ lsbCLogSerialNumber);
		if(logger.isDebugEnabled()){
			logger.debug("[PH] setAMP1Parameters  P_AMP1_CLOG_SN :" + (msbCLogSerialNumber+ lsbCLogSerialNumber));
		}
		
		//staring index contains year,month and null indicator
		//GBL_AMP1_TIME (YYYYMMDDHHMM format) 
		//GBL_AMP1_NULL_IND [0, 1]
		// year bits
		int year = Calendar. getInstance(). get(Calendar. YEAR);
		int resYearBufferValue = amp1Buffer[index] & 0x03;
		switch(resYearBufferValue){
		case 0:
			year--;
			break;
		case 2:
			year++;
			break;
		}
		//month
		int month = amp1Buffer[index] >>2 & 0x0F;
		
		//null indicator
		int nullIndicator = amp1Buffer[index] >>6 & 0x03;
		legData.set(LegDataAttributes.P_AMP1_NULL_IND, nullIndicator);
		
		if(logger.isDebugEnabled()){
			logger.debug("[PH] setAMP1Parameters  P_AMP1_NULL_IND :" + nullIndicator);
		}
		
		index++;
		//get date
		
		int date = amp1Buffer[index] & 0x1F;
		
		index++;
		//hour
		int hour = amp1Buffer[index] & 0x1F;
		//min
		int bufferMinValue = amp1Buffer[index] >> 5 & 0x03;
		
		int minutes = 0;
		switch(bufferMinValue){
		case 1:
			minutes = 15;
			break;
		case 2:
			minutes = 30;
			break;
		case 3:
			minutes = 45;
			break;
		default:
			minutes = 0;
			break;
		}
		//YYYYMMDDHHMM
		String amp1Time = String.valueOf(year) + month + date + hour + minutes;
		
		legData.set(LegDataAttributes.P_AMP1_TIME, amp1Time);
		if(logger.isDebugEnabled()){
			logger.debug("[PH] setAMP1Parameters  P_AMP1_TIME :" + amp1Time);
		}
	}
	
	public static void setAMP2Parameters(Amp2SequenceType amp2Seq, CallData callData) throws InvalidInputException{

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		
		//GBL_AMP2_AINNODEID
		if(amp2Seq.getAmpAINNodeID() != null){
			AmpAINNodeID ampAinNodeID = amp2Seq.getAmpAINNodeID();
			
			if(ampAinNodeID.isSpcIDSelected()){
				String spcID = new String(ampAinNodeID.getSpcID().getValue());
				
				if(logger.isDebugEnabled()){
					logger.debug("[PH] setAMP2Parameters  P_AMP2_AIN_SPEC_ID :" + spcID);
				}
				
				legData.set(LegDataAttributes.P_AMP2_AIN_SPEC_ID, spcID);
			}
			
			if(ampAinNodeID.isISDNDeviceIDSelected()){
				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(ampAinNodeID.getISDNDeviceID().getValue().getValue(), Constant.CALLED);
				
				if(logger.isDebugEnabled()){
					logger.debug("[PH] setAMP2Parameters  P_AMP2_AIN_ISDN_DEVICE_ID :" + ainDigits.getAddrSignal());
				}
				
				legData.set(LegDataAttributes.P_AMP2_AIN_ISDN_DEVICE_ID, ainDigits.getAddrSignal());
			}
		}
		
		//CLOG SQN Number
		if(amp2Seq.isAmpCLogSeqNoPresent()){
			if(logger.isDebugEnabled()){
				logger.debug("[PH] setAMP2Parameters  P_AMP2_CLOG_SQNNO :" + amp2Seq.getAmpCLogSeqNo().getValue());
			}
			
			legData.set(LegDataAttributes.P_AMP2_CLOG_SQNNO, amp2Seq.getAmpCLogSeqNo().getValue());
		}
		
		//CLOG Rep Ind
		if(amp2Seq.isAmpCLogRepIndPresent()){
			
			if(logger.isDebugEnabled()){
				logger.debug("[PH] setAMP2Parameters  P_AMP2_CLOG_REP_IND :" + amp2Seq.getAmpCLogRepInd().getValue());
			}
			
			legData.set(LegDataAttributes.P_AMP2_CLOG_REP_IND, amp2Seq.getAmpCLogRepInd().getValue());
		}
		
		//CLOG Call Prog Ind
		if(amp2Seq.isAmpCallProgIndPresent()){
			
			if(logger.isDebugEnabled()){
				logger.debug("[PH] setAMP2Parameters  P_AMP2_CALL_PROG_IND :" + amp2Seq.getAmpCallProgInd().getValue());
			}
			
			legData.set(LegDataAttributes.P_AMP2_CALL_PROG_IND, amp2Seq.getAmpCallProgInd().getValue());
		}
		
		//AmpTestReqInd
		if(amp2Seq.isAmpTestReqIndPresent()){
			
			if(logger.isDebugEnabled()){
				logger.debug("[PH] setAMP2Parameters  P_AMP2_TEST_REQ_IND :" + amp2Seq.getAmpTestReqInd().getValue());
			}
			
			legData.set(LegDataAttributes.P_AMP2_TEST_REQ_IND,amp2Seq.getAmpTestReqInd().getValue());
		}
		
		//CLog NAME 
		if(amp2Seq.isAmpCLogNamePresent()){
			String cLogName = new String(amp2Seq.getAmpCLogName().getValue());
			
			if(logger.isDebugEnabled()){
				logger.debug("[PH] setAMP2Parameters  P_AMP2_CLOG_NAME :" + cLogName);
			}
			
			legData.set(LegDataAttributes.P_AMP2_CLOG_NAME, cLogName);
		}
		
		if(amp2Seq.isAmpSvcProvIDPresent() && amp2Seq.getAmpSvcProvID().isOcnSelected()){
			
			if(logger.isDebugEnabled()){
				logger.debug("[PH] setAMP2Parameters  P_AMP2_SVC_PROV_ID :" + amp2Seq.getAmpSvcProvID().getOcn());
			}
			
			legData.set(LegDataAttributes.P_AMP2_SVC_PROV_ID, amp2Seq.getAmpSvcProvID().getOcn());
		}
	
	}
	/**
	 * This method is called by protocol handler to parse the Provide Instruction
	 * Operation.
	 * 
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param callData       represents an instance of CallData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseProvideInstruction(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH]  Enter :: parseProviderInstruction");
		}
		ProviderInstructionArg providerInstructionArg;
		if (logger.isInfoEnabled()) {
			logger.info(invokeIndEvent.toString());
		}
		try {
			if (logger.isInfoEnabled()) {
				logger.info(invokeIndEvent.toString());
			}
			// To parse the parameter set through BinaryNotes Need to convert F2 to 31.
			Parameters parameters = invokeIndEvent.getParameters();
			byte[] parameter = parameters.getParameter();
			if (logger.isInfoEnabled()) {
				logger.info("original parameters from spectra:" + CommonUtils.formatBytes(parameter));
			}
			parameter[0] = 0x31;
			if(parameter.length>=35) {
				parameter[35] = 0x25;	
			}

			parameters.setParameter(parameter);
			if (logger.isInfoEnabled()) {
				logger.info("after modifying parameter Buffer:" + CommonUtils.formatBytes(parameter));
			}
			invokeIndEvent.setParameters(parameters);

			providerInstructionArg = (ProviderInstructionArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
			if (logger.isInfoEnabled()) {
				logger.info("decoded ProviderInstructionArg Object:" + providerInstructionArg.toString());
			}
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Provider Instruction " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] ProviderInstruction parsing failure occured, due to paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in Provider Instruction (Exception)" + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse ProvidInstruction throwing ASNParsingException",
						e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, ProvideInstruction parsing failure occured.", e,
					MESSAGE.PROVIDE_INSTRUCTION);
		}

		try {
			// Called Party ANI
			if (providerInstructionArg.getEndUserNumber() != null
					&& providerInstructionArg.getEndUserNumber().getDigits() != null) {
				T1Digits t1Digit = new T1Digits();

				T1Digits calledAin = t1Digit.decodeDigits(
						providerInstructionArg.getEndUserNumber().getDigits().getValue(), Constant.CALLED);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Called party number from Provider Instruction arg is "
							+ calledAin);
				}
				if (calledAin == null || calledAin.getAddrSignal() == null
						|| "".equals(calledAin.getAddrSignal().trim())) {
					/*
					 * call should be handled as ASn parse failure as address signal is missing
					 */
					logger.error(dialogueId + ":: [PH] called party num address signal missing in provide instruction");
					throw new ASNParsingException(
							dialogueId + ":: [PH] called party num address signal missing in provide instruction",
							MESSAGE.PROVIDE_INSTRUCTION);
				}
				PhoneNumber calledNumber = parseT1PhoneNumber(dialogueId, calledAin);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parseCalledPartyNum, CalledNumber:" + calledAin);
				}
				legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("calledPartyNumber not present in Provide Instruction");
				}
			}

			// Calling Party ANI
			if (providerInstructionArg.getCallingPartyAni() != null) {
				T1Digits t1Digits = new T1Digits();
				T1Digits callingNum = t1Digits.decodeDigits(providerInstructionArg.getCallingPartyAni().getValue(),
						Constant.CALLING);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted calling party number from Provider Instruction arg is "
							+ callingNum);
				}
				if (callingNum == null || callingNum.getAddrSignal() == null
						|| "".equals(callingNum.getAddrSignal().trim())) {
					logger.error(
							dialogueId + ":: [PH] calling party num address signal missing in provide instruction");
					throw new ASNParsingException(
							dialogueId + ":: [PH] calling party num address signal missing in provide instruction",
							MESSAGE.PROVIDE_INSTRUCTION);
				}
				PhoneNumber callingNumber = parseT1PhoneNumber(dialogueId, callingNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parsecallingPartyNum, callingNumber:" + callingNum);
				}
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
			} else {
				// fetch the offset 
				Parameters parameters = invokeIndEvent.getParameters();
				byte[] parameter = parameters.getParameter();
				int offset = 6 + providerInstructionArg.getEndUserNumber().getDigits().getValue().length;
				int len = parameter[offset+1];
				byte[] callingPartyBuf = Arrays.copyOfRange(parameter, offset+2, offset+2+len);
				if(logger.isInfoEnabled()) {
					logger.info("Calling party null: offset:"+offset + ": len:" + len 
							+ ": " +CommonUtils.formatBytes(callingPartyBuf));
				}
				//Calling party null: offset:15: len:9: 0x02 0x00 0x21 0x0a 0x21 0x43 0x65 0x87 0x09
				T1Digits callingPartyT1Digits = new T1Digits();
				T1Digits t1Digits = new T1Digits();

				callingPartyT1Digits = t1Digits.decodeDigits(callingPartyBuf, "CALLING");
				PhoneNumber callingPartyPhoneNumber = parseT1PhoneNumber(dialogueId, callingPartyT1Digits);
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingPartyPhoneNumber);
			}

			// LATA
			if (providerInstructionArg.getCgpLata() != null) {
				T1Digits t1Digit = new T1Digits();

				T1Digits lataNum = t1Digit.decodeDigits(providerInstructionArg.getCgpLata().getValue(),
						Constant.CALLING);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted lata from Provider Instruction arg is " + lataNum);
				}
				if (lataNum == null || lataNum.getAddrSignal() == null || "".equals(lataNum.getAddrSignal().trim())) {
					logger.error(dialogueId + ":: [PH] lata address signal missing in provide instruction");
					throw new ASNParsingException(
							dialogueId + ":: [PH] lata address signal missing in provide instruction",
							MESSAGE.PROVIDE_INSTRUCTION);
				}
				PhoneNumber lataNumber = parseT1PhoneNumber(dialogueId, lataNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parsecallingPartyNum, Lata:" + lataNum);
				}

				if(StringUtils.isNoneBlank(lataNumber.getAddress())){
					legData.set(LegDataAttributes.P_LATA, lataNumber.getAddress());
				}else{
					logger.debug(dialogueId + ":: lata number not present");
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("LATA not present in Provider Instruction");
				}
			}

			/*
			 * provideInstruction . originating station type
			 */
			if (providerInstructionArg.getOli() != null) {
				OriginatingStationType originatingStationType = providerInstructionArg.getOli();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted OriginatingStationType from provideInstruction: "
							+ originatingStationType.getValue());
				}

				int cpc = -1;
				if(originatingStationType != null && originatingStationType.getValue() != null){
					byte[] val = originatingStationType.getValue();

					if(val.length >= 1){
						cpc = val[0] & 0x0F;
					}
				}

				if(cpc != -1){
					legData.set(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE, cpc);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: [PH] Setting OriginatingStationType: "
								+ cpc);
					}
				}else{
					logger.debug(dialogueId + ":: not setting cpc");
				}
			} 

			/*
			 * provideInstruction . cic support
			 */
			if (providerInstructionArg.getCicSupport() != null) {
				CICExpansion cicSupport = providerInstructionArg.getCicSupport();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted cicSupport from provide Instruction: "
							+ cicSupport.getValue());
				}

				if(cicSupport.getValue() != null){
					legData.set(LegDataAttributes.P_PI_O_CIC_SUPPORT, cicSupport.getValue().ordinal());
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("cicSupport not present in Provider Instruction");
				}
			}
		} catch (InvalidInputException e) {
			logger.error(dialogueId + ":: Error in parseProvideInstruction (InvalidInputException)" + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::ASN Parsing Failure: Provide Instruction parsing failure occured.:", e,
					MESSAGE.PROVIDE_INSTRUCTION);
		} catch (ASNParsingException e) {
			logger.error(dialogueId + "::[PH] Error in parseProvideInstruction (ASNParsingException)" + e.getMessage());
			throw e;
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseProvideInstruction (Exception)" + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure:  ProvideInstruction parsing failure occured.", e,
					MESSAGE.PROVIDE_INSTRUCTION);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Provide Instruction parsed successfully");
		}
	}

	public static void parseBnsQuery(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse BnsQuery");

		}
		if (logger.isInfoEnabled()) {
			logger.info("[invokeIndEvent] :" + invokeIndEvent.toString());
		}

		byte[] input = invokeIndEvent.getParameters().getParameter();
		if (input.length >= BnsConstants.BNS_MANDATORY_LEN) {
			try {
				BNSQuery.decodeBuffer(callData, input);
			} catch (AINCodecException ainExp) {
				logger.error("AinCodecException ::");

				throw new ASNParsingException(
						dialogueId + "::[PH] ASN Parsing Failure: Bns Query parsing failure occured.", ainExp,
						MESSAGE.LIDB_PROTOCOL_ERR);
			} catch (InvalidInputException invIpExp) {
				logger.error("InvalidInputException ::", invIpExp);
				throw new ASNParsingException(
						dialogueId + "::[PH] ASN Parsing Failure: Bns Query parsing failure occured.", invIpExp,
						MESSAGE.LIDB_PROTOCOL_ERR);
			} catch (Exception e) {
				logger.error(dialogueId + "::[PH] Error in parseBnsQuery " + e.getMessage());
				throw new ASNParsingException(
						dialogueId + "::[PH] ASN Parsing Failure: Bns Query parsing failure occured.", e,
						MESSAGE.LIDB_PROTOCOL_ERR);
			}
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "::[PH] Bns Query parsed successfully");
			}
		} else {
			logger.error(dialogueId + "::BNS input is not of expected length.(query length should be >= 28)");
			throw new EnumParamOutOfRangeException("input is not of expected length.(query length should be >= 28)");
		}

	}

	public static void parseGnQuery(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse GnQuery");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			GenericNameQuery.decodeGenericNameQuery(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(dialogueId + "::[PH] ASN Parsing Failure: GN Query parsing failure occured.",
					ainExp, MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			logger.error(dialogueId + "::[PH] Error in parseGNQuery " + invIpExp.getMessage());
			throw new ASNParsingException(dialogueId + "::[PH] ASN Parsing Failure: GN Query parsing failure occured.",
					invIpExp, MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseGNQuery " + e.getMessage());
			throw new ASNParsingException(dialogueId + "::[PH] ASN Parsing Failure: GN Query parsing failure occured.",
					e, MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Gn Query parsed successfully");
		}
	}

	public static void parseOlnsQuery(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse OlnsQuery");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			OLNSQuery.decodeOLNSQuery(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: OLNS Query parsing failure occured.", ainExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: OLNS Query parsing failure occured.", invIpExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseOLNSQuery " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: OLNS Query parsing failure occured.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] OLNS Query parsed successfully");
		}
	}

	/**
	 * This method is called by protocol handler for parsing Called Party number.
	 * 
	 * @param dialogueId     represents integer value of dialogue Id.
	 * @param calledPartyNum represents an instance of CalledPartyNum
	 * @return an instance of PhoneNumber
	 */
	private static PhoneNumber parseCalledPartyNum(int dialogueId, AinDigits calledPartyNum) {
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside parseCalledPartyNum");
		}

		PhoneNumber calledNumber = new PhoneNumber();

		/*
		 * Nature Of Address
		 */
		CalledNatOfNumEnum natureOfAddrEnum = calledPartyNum.getCalledNatOfNumEnum();
		if ((natureOfAddrEnum == CalledNatOfNumEnum.SPARE) || (natureOfAddrEnum == CalledNatOfNumEnum.NAT_NUM)
				|| (natureOfAddrEnum == CalledNatOfNumEnum.CALL_LOCAL_EXCHANGE)
				// || (natureOfAddrEnum == CalledNatOfNumEnum.NAT_NUM_OPERTR_REQ)
				|| (natureOfAddrEnum == CalledNatOfNumEnum.SUBS_NUM)
				|| (natureOfAddrEnum == CalledNatOfNumEnum.SUBS_NUM_OPERTR_REQ)) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == CalledNatOfNumEnum.NAT_NUM_OPERTR_REQ) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL_OPERATOR);
		} else if (natureOfAddrEnum == CalledNatOfNumEnum.NOT_APPLICABLE) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if ((natureOfAddrEnum == CalledNatOfNumEnum.INTER_NAT_NUM)
				|| natureOfAddrEnum == CalledNatOfNumEnum.INTER_NAT_NUM_OPERTR_REQ) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}
		
		if(natureOfAddrEnum!=null){
		calledNumber.setNatureOfAddressActual(natureOfAddrEnum.getCode());

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Nature of Address is " + natureOfAddrEnum.getCode());
		}
		}

		/*
		 * Numbering Plan Indicator
		 */
		NumPlanEnum numPlanIndEnum = calledPartyNum.getNumPlanEnum();
		if (numPlanIndEnum == NumPlanEnum.PRIVATE_NP) {
			calledNumber.setNumberingPlan(PhoneNumber.NP_PRIVATE);
		} else if (numPlanIndEnum == NumPlanEnum.ISDN_NP) {
			calledNumber.setNumberingPlan(PhoneNumber.NP_ISDN);
		} else {
			calledNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Numbering Plan is " + numPlanIndEnum.getCode());
		}

		/*
		 * Address
		 */
		String addrSignal = calledPartyNum.getAddrSignal();
		calledNumber.setAddress(addrSignal);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Address Signal is " + addrSignal);
		}

		return calledNumber;
	}

	/**
	 * This method is called by protocol handler for parsing calling party number
	 * and return a phoneNumber instance.
	 * 
	 * @param dialogueId      represents integer value of dialogue Id.
	 * @param callingPartyNum represents an instance of CallingPartyNum
	 * @return an instance of PhoneNumber
	 */
	private static PhoneNumber parseCallingPartyNum(int dialogueId, AinDigits callingPartyNum) {
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside parseCallingPartyNum");
		}

		PhoneNumber callingNumber = new PhoneNumber();

		/*
		 * Nature Of Address
		 */
		CalgNatOfNumEnum natureOfAddrEnum = callingPartyNum.getCalgNatOfNumEnum();
		if ((natureOfAddrEnum == CalgNatOfNumEnum.SPARE) || (natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_NAT_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_NAT_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_SUBS_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_SUBS_NUM)) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == CalgNatOfNumEnum.NOT_APPLICABLE) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if ((natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_INTER_NAT_NUM)
				|| natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_INTER_NAT_NUM) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}
		if (natureOfAddrEnum != null) {
			callingNumber.setNatureOfAddressActual(natureOfAddrEnum.getCode());
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId
						+ "::[PH] Extracted Nature of Address is "
						+ natureOfAddrEnum.getCode());
			}
		}

		/*
		 * Numbering Plan Indicator
		 */
		com.agnity.ain.enumdata.NumPlanEnum numPlanIndEnum = callingPartyNum.getNumPlanEnum();
		if (numPlanIndEnum == NumPlanEnum.PRIVATE_NP) {
			callingNumber.setNumberingPlan(PhoneNumber.NP_PRIVATE);
		} else if (numPlanIndEnum == NumPlanEnum.ISDN_NP) {
			callingNumber.setNumberingPlan(PhoneNumber.NP_ISDN);
		} else {
			callingNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Numbering Plan is " + numPlanIndEnum.getCode());
		}

		// Presentation and screening indicator
		callingNumber.setPresentationIndicator(callingPartyNum.getClgPrsntRestIndEnum().getCode());
		callingNumber.setScreeningIndicator(callingPartyNum.getScreeningIndEnum().getCode());

		/*
		 * Address
		 */
		String addrSignal = callingPartyNum.getAddrSignal();
		callingNumber.setAddress(addrSignal);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Address Signal is " + addrSignal);
		}

		return callingNumber;
	}

	private static PhoneNumber parseT1PhoneNumber(int dialogueId, T1Digits callingPartyNum) {
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside parseCallingPartyNum");
		}

		PhoneNumber callingNumber = new PhoneNumber();

		/*
		 * Nature Of Address
		 */
		NatureOfNumEnum natureOfAddrEnum = callingPartyNum.getNoa();
		callingNumber.setNatureOfAddress(natureOfAddrEnum.getCode());

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Nature of Address is " + natureOfAddrEnum.getCode());
		}

		/*
		 * Numbering Plan Indicator
		 */
		if(callingPartyNum.getNumPlanEnum() != null) {
			callingNumber.setNumberingPlan(callingPartyNum.getNumPlanEnum().getCode());
			if (logger.isDebugEnabled()) {
				logger.debug(
						dialogueId + "::[PH] Extracted Numbering Plan is " + callingPartyNum.getNumPlanEnum().getCode());
			}
		}

		/*
		 * Address
		 */
		String addrSignal = callingPartyNum.getAddrSignal();
		callingNumber.setAddress(addrSignal);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Address Signal is " + addrSignal);
		}

		return callingNumber;
	}

	/**
	 * This method is called used for parsing called party number and return a
	 * phoneNumber instance.
	 * 
	 * @param calledPartyNum represents an instance of calledPartyNum
	 * @return an instance of PhoneNumber
	 */
	public static PhoneNumber parseCalledPartyNum(AinDigits calledPartyNum) {
		PhoneNumber calledNumber = new PhoneNumber();
		/*
		 * Nature Of Address
		 */
		CalledNatOfNumEnum natureOfAddrEnum = calledPartyNum.getCalledNatOfNumEnum();
		if ((natureOfAddrEnum == CalledNatOfNumEnum.SPARE) || (natureOfAddrEnum == CalledNatOfNumEnum.NAT_NUM)
				|| (natureOfAddrEnum == CalledNatOfNumEnum.CALL_LOCAL_EXCHANGE)
				|| (natureOfAddrEnum == CalledNatOfNumEnum.SUBS_NUM)
				|| (natureOfAddrEnum == CalledNatOfNumEnum.SUBS_NUM_OPERTR_REQ)) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == CalledNatOfNumEnum.NAT_NUM_OPERTR_REQ) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL_OPERATOR);
		} else if (natureOfAddrEnum == CalledNatOfNumEnum.NOT_APPLICABLE) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if ((natureOfAddrEnum == CalledNatOfNumEnum.INTER_NAT_NUM)
				|| natureOfAddrEnum == CalledNatOfNumEnum.INTER_NAT_NUM_OPERTR_REQ) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}
		if (natureOfAddrEnum != null) {
		  calledNumber.setNatureOfAddressActual(natureOfAddrEnum.getCode());
		}
		/*
		 * Numbering Plan Indicator
		 */
		NumPlanEnum numPlanIndEnum = calledPartyNum.getNumPlanEnum();
		if (numPlanIndEnum == NumPlanEnum.PRIVATE_NP) {
			calledNumber.setNumberingPlan(PhoneNumber.NP_PRIVATE);
		} else if (numPlanIndEnum == NumPlanEnum.ISDN_NP) {
			calledNumber.setNumberingPlan(PhoneNumber.NP_ISDN);
		} else {
			calledNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
		}

		/*
		 * Address
		 */
		String addrSignal = calledPartyNum.getAddrSignal();
		calledNumber.setAddress(addrSignal);
		return calledNumber;
	}

	/**
	 * This method is used for parsing calling party number and return a phoneNumber
	 * instance.
	 * 
	 * @param callingPartyNum represents an instance of ainDigits
	 * @return an instance of PhoneNumber
	 */
	public static PhoneNumber parseCallingPartyNum(AinDigits callingPartyNum) {
		PhoneNumber callingNumber = new PhoneNumber();
		/*
		 * Nature Of Address
		 */
		CalgNatOfNumEnum natureOfAddrEnum = callingPartyNum.getCalgNatOfNumEnum();
		if ((natureOfAddrEnum == CalgNatOfNumEnum.SPARE) || (natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_NAT_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_NAT_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_SUBS_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_SUBS_NUM)) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == CalgNatOfNumEnum.NOT_APPLICABLE) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if ((natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_INTER_NAT_NUM)
				|| natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_INTER_NAT_NUM) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}
		if (natureOfAddrEnum != null) {
		callingNumber.setNatureOfAddressActual(natureOfAddrEnum.getCode());
		}
		
		com.agnity.ain.enumdata.NumPlanEnum numPlanIndEnum = callingPartyNum.getNumPlanEnum();
		if (numPlanIndEnum == NumPlanEnum.PRIVATE_NP) {
			callingNumber.setNumberingPlan(PhoneNumber.NP_PRIVATE);
		} else if (numPlanIndEnum == NumPlanEnum.ISDN_NP) {
			callingNumber.setNumberingPlan(PhoneNumber.NP_ISDN);
		} else {
			callingNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
		}
		// Presentation and screening indicator
		callingNumber.setPresentationIndicator(callingPartyNum.getClgPrsntRestIndEnum().getCode());
		callingNumber.setScreeningIndicator(callingPartyNum.getScreeningIndEnum().getCode());

		String addrSignal = callingPartyNum.getAddrSignal();
		callingNumber.setAddress(addrSignal);
		return callingNumber;
	}

	/**
	 * This method is called by protocol handler for parsing Billing number and
	 * return a phoneNumber instance.
	 *
	 * @param ainDigit represents an instance of billingPartyNum
	 * @return an instance of PhoneNumber
	 */
	public static PhoneNumber parseAinDigits(AinDigits ainDigit, int natureOfNum) {
		PhoneNumber phoneNumber = new PhoneNumber();
		phoneNumber.setNatureOfAddress(natureOfNum);
		String addrSignal = ainDigit.getAddrSignal();
		phoneNumber.setAddress(addrSignal);
		return phoneNumber;
	}

	/**
	 * This method is called by protocol handler for creating RRBCSM for Disarming
	 * of signals.
	 * 
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createRRBCSMForDisarming(CallData callData, Action action) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(action.getLeg()));

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside createRRBCSMForDisarming");
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Disarm oDisconnect");
		}

		EDPRequest edpNoti = new EDPRequest();
		EDPRequestNonAsn nonAsn = new EDPRequestNonAsn();
		nonAsn.disableEdp(EDPRequestNonAsn.O_DISCONNECT);

		/*
		 * oAbandon
		 */

		nonAsn.disableEdp(EDPRequestNonAsn.O_ABANDON);
		byte[] byteArr = nonAsn.encodeEDPRequestNonAsn();

		BitString bitS = new BitString(byteArr);

		edpNoti.setValue(bitS);
		RequestReportBCMEventArg rrbcsmEventArg = new RequestReportBCMEventArg();
		rrbcsmEventArg.setEDPRequest(edpNoti);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Disarm oAbandon");
		}

		legData.set(LegDataAttributes.P_ERB_SET, null);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(rrbcsmEventArg);
		opCode.add(AinOpCodes.RRBE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Exiting createRRBCSMForDisarming");
		}

		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler for creating RRBCSM for Arming of
	 * signals.
	 * 
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static byte[] createRRBCSMForArming(CallData callData, Action action) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.valueOf(action.getLeg()));

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside createRRBCSMForArming");
		}

		Object erbTypeSetObj = AinScfProtocolUtil.getErbSetByApplication(callData, action.getLeg());

		Object noAnswerTimeObj = legData.get(LegDataAttributes.NP_NO_ANSWER_TIMER_DURATION);

		Object erbTimoutTimerObj = legData.get(LegDataAttributes.NP_ERB_TIMEOUT_TIMER_DURATION);

		Set<Action.ERB_TYPE> erbTypeSet = null;

		if (erbTypeSetObj != null) {
			erbTypeSet = (Set<Action.ERB_TYPE>) (erbTypeSetObj);
		} else {

			throw new Exception("No ERB set provided to createRRBCSMForArming");
		}

		// Encode RRBCSM event
		RequestReportBCMEventArg rrbcsmEventArg = new RequestReportBCMEventArg();

		// O_CalledPartyBusy, Network Busy, O NoAnswer shall be part of EDPRequest
		EDPRequest edpRequest = new EDPRequest();
		EDPRequestNonAsn edpReqNonAsn = new EDPRequestNonAsn();

		// Only oAnswer would EDPNotification
		EDPNotification edpNotificationAsn = new EDPNotification();
		EDPNotificationNonAsn edpNotifNonAsn = new EDPNotificationNonAsn();

		// Encode Notification First.
		// OAnswer is set in Notification by Default
		edpNotifNonAsn.enableEdp(EDPNotificationNonAsn.O_ANSWER);

		BitString bs = new BitString();
		bs.setValue(edpNotifNonAsn.encodeEDPNotificationNonAsn(), 4); // 4 refers to no of bit unused
		edpNotificationAsn.setValue(bs);
		rrbcsmEventArg.setEDPNotification(edpNotificationAsn);

		// Now encode EDP Request based on the SET type.
		// oCalledPartyBusy
		if (erbTypeSet.contains(Action.ERB_TYPE.ERB_BUSY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Arm oCalledPartyBusy");
			}

			edpReqNonAsn.enableEdp(EDPRequestNonAsn.O_CALLEDPARTY_BUSY);
		}

		// oNoAnswer
		if (erbTypeSet.contains(Action.ERB_TYPE.ERB_NO_ANSWER)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Arm oNoAnswer");
			}

			edpReqNonAsn.enableEdp(EDPRequestNonAsn.O_NOANSWER);

			// Encode NoAnswer only if NoAnswer is armed and provided by
			// application. As per GR1299 the value may range from 1..120 sec
			if (noAnswerTimeObj != null) {

				int noAnswerTimer = (Integer) noAnswerTimeObj;
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: noAnswerTimer from service[ms] = " + noAnswerTimer);
				}

				// Time value comes as milli-sec
				if (noAnswerTimer < 1000) {
					noAnswerTimer = 1;
				} else if (noAnswerTimer > 120000) {
					noAnswerTimer = 120;
				}

				// change to seconds
				noAnswerTimer /= 1000;

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: AIN RRBCSM - Set no answer time " + noAnswerTimer);
				}

				ONoAnswerTimer timer = new ONoAnswerTimer();
				timer.setValue(noAnswerTimer);
				rrbcsmEventArg.setONoAnswerTimer(timer);
			}
		}

		// Set ERB Timeout value
		if (erbTimoutTimerObj != null) {

			int erbTimoutTimer = (Integer) erbTimoutTimerObj;
			TimeOutTimer tot = new TimeOutTimer();
			tot.setValue(erbTimoutTimer);
			rrbcsmEventArg.setTimeoutTimer(tot);
		}

		// Arm ODisconnect
		if (erbTypeSet.contains(Action.ERB_TYPE.ERB_DISCONNECT)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Arm oDisconnect");
			}
			edpReqNonAsn.enableEdp(EDPRequestNonAsn.O_DISCONNECT);
		}

		// Arm Network Busy
		if (erbTypeSet.contains(Action.ERB_TYPE.ERB_NETWORK_BUSY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Arm Network Busy");
			}
			edpReqNonAsn.enableEdp(EDPRequestNonAsn.NETWORK_BUSY);
		}

		// Arm oAbandon
		if (erbTypeSet.contains(Action.ERB_TYPE.ERB_ABANDON)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Arm OAbandon");
			}
			edpReqNonAsn.enableEdp(EDPRequestNonAsn.O_ABANDON);
		}

		// Encode EDPRequest
		bs = new BitString();
		bs.setValue(edpReqNonAsn.encodeEDPRequestNonAsn(), 4); // setting value 4 which refers to
		// no. of bits unused
		edpRequest.setValue(bs);
		rrbcsmEventArg.setEDPRequest(edpRequest);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(rrbcsmEventArg);
		opCode.add(AinOpCodes.REQUEST_REPORT_BCSM_EVENT);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Exiting createRRBCSMForArming");
		}

		return encodeList.getFirst();
	}

	/**
	 * This method is called by the protocol handler for creating AnalyzeRoute to
	 * connect Media server AnalyzeRoute is used for handoff and shall contain
	 * correlation ID
	 * 
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createAnalyzeRouteForMs(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createAnalyzeRouteForMs");
		}

		AnalyzeRouteArg connectArg = new AnalyzeRouteArg();
		AinDigits ainDigits;

		String calledPartyId = null;
		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
			calledPartyId = callData.get(CallDataAttribute.P_CORRELATION_ID).toString();
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::Called Party ID: " + calledPartyId);
		}

		AinDigits sd = new AinDigits();
		sd.setAddrSignal(calledPartyId);
		sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
		sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

		AINDigits calledPartyAddrDigits = new AINDigits(sd.encodeAinDigits());

		CalledPartyID calledParty = new CalledPartyID();
		calledParty.setValue(calledPartyAddrDigits);

		connectArg.setCalledPartyID(calledParty);

		if (logger.isDebugEnabled()) {
			logger.debug("Setting CalledPartyId as:[" + calledPartyId + "], byte: "
					+ CommonUtils.formatBytes(calledPartyAddrDigits.getValue()));
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.ANALYZE_ROUTE);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectArg);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by the protocol handler for creating AnalyzeRoute
	 * 
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	/**
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createAnalyzeRouteForTerm(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createAnalyzeRouteForTerm");
		}

		AnalyzeRouteArg connectArg = new AnalyzeRouteArg();
		AinDigits ainDigits;

		// Destination number in called Party ID
		PhoneNumber destinationNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);

		if (destinationNumber != null) {

			CalledNatOfNumEnum natureOfAddrEnum = CalledNatOfNumEnum.fromInt(destinationNumber.getNatureOfAddress());
			String noaInt = (String) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER_NOA_INT);
			if(StringUtils.isNotBlank(noaInt)){
				try {
					int noa = Integer.parseInt(noaInt);
					natureOfAddrEnum = CalledNatOfNumEnum.fromInt(noa);
				} catch (NumberFormatException ex) {
					logger.error("Failed to parse noa as integer from: " + noaInt);
				}

				if(logger.isDebugEnabled()){
					logger.debug("NOA of Destination number is set to: " + natureOfAddrEnum);
				}
			}
			if (natureOfAddrEnum == null) {
				natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
			}

			NumPlanEnum numberPlan = NumPlanEnum.fromInt(destinationNumber.getNumberingPlan());
			String numPlanInt = (String) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER_NUM_PLAN_INT);
			if(StringUtils.isNotBlank(numPlanInt)){
				try {
					int numPlan = Integer.parseInt(numPlanInt);
					numberPlan = NumPlanEnum.fromInt(numPlan);
				} catch (NumberFormatException ex) {
					logger.error("Failed to parse number plan as integer from: " + numPlanInt);
				}

				if(logger.isDebugEnabled()){
					logger.debug("Number Plan of Destination number is set to: " + numberPlan);
				}
			}
			if (numberPlan == null) {
				numberPlan = NumPlanEnum.ISDN_NP;
			}

			ainDigits = new AinDigits();
			ainDigits.setAddrSignal(destinationNumber.getAddress());
			ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
			ainDigits.setNumPlanEnum(numberPlan);

			byte[] calledPartyNum = ainDigits.encodeAinDigits();

			CalledPartyID calledPartyNumber = new CalledPartyID();
			calledPartyNumber.setValue(new AINDigits(calledPartyNum));

			connectArg.setCalledPartyID(calledPartyNumber);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting CalledPartyId as:[" + destinationNumber + "], byte: "
						+ CommonUtils.formatBytes(calledPartyNum));
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug("Destination number not set in call data");
			}
		}

		// Encode Calling Party ID
		PhoneNumber callingPartyId = null;
		if (leg2Data.get(LegDataAttributes.P_CALLING_PARTY) != null) {
			callingPartyId = (PhoneNumber) leg1Data.get(LegDataAttributes.P_CALLING_PARTY);

			if (callingPartyId.getAddress() != null
					&& !callingPartyId.getAddress().isEmpty()) {
				// check if presentation restriction is enable or not
				if(leg2Data.get(LegDataAttributes.P_CLG_PRESENTATION_RESTRICTED) != null){
					callingPartyId.setPresentationIndicator(1); // 1 - restricted
				}

				AINDigits asnAinDigits = encodeAINDigits(callingPartyId);

				CallingPartyID asnCallingPartyId = new CallingPartyID();
				asnCallingPartyId.setValue(asnAinDigits);

				connectArg.setCallingPartyID(asnCallingPartyId);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting CallingPartyID as:[" + callingPartyId + "], byte: "
							+ CommonUtils.formatBytes(asnAinDigits.getValue()));
				}
			}
		}
		
		if (leg2Data.get(LegDataAttributes.P_LATA) != null) {
			String lata = (String) leg2Data.get(LegDataAttributes.P_LATA);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting lata as:[" + lata + "]");
			}

			Lata leg2Lata = new Lata();
			AinDigits digits = new AinDigits();
			digits.setAddrSignal(lata);

			// Setting NOA as SPARE (2)
			digits.setCalgNatOfNumEnum(CalgNatOfNumEnum.SPARE);

			// Setting NumPlan as TELEPHONY_NP (2) -> There is no value RESERVED in NumPlan
			digits.setNumPlanEnum(NumPlanEnum.TELEPHONY_NP);

			leg2Lata.setValue(new AINDigits(digits.encodeAinDigits()));

			if (logger.isDebugEnabled()) {
				logger.debug("Setting lata encoded as:[" + digits+ "]");
			}
			connectArg.setLata(leg2Lata);
		}

		// Encode carrier
		byte[] carrierByte = null;
		CarrierInfo ci = null;
		if (leg2Data.get(LegDataAttributes.P_CARRIER) != null) {
			ci = (CarrierInfo) leg2Data.get(LegDataAttributes.P_CARRIER);

			// check if application has send Selection carrier information too. 
			// if yes then replace it
			if(leg2Data.get(LegDataAttributes.P_CARRIER_SELECTION_CODE) !=null){
				Object val = leg2Data.get(LegDataAttributes.P_CARRIER_SELECTION_CODE);
				try{
					int cicInt = Integer.parseInt(val.toString());
					ci.setCarrierSelection(cicInt);
					if(logger.isDebugEnabled()){
						logger.debug("Overriding CIC Selection Code:"+val + ": int"
								+cicInt);
					}
				}catch(Exception e){

				}
			}

			carrierByte = getCarrierEncodedBuffer(ci);
			Carrier ansgenCarrier = new Carrier();
			ansgenCarrier.setValue(new com.agnity.ain.asngenerated.CarrierFormat(carrierByte));
			connectArg.setCarrier(ansgenCarrier);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting Carrier as:[" + ci + "], byte: " + CommonUtils.formatBytes(carrierByte));
			}
		}

		// Encode alternate Carrier
		if (leg2Data.get(LegDataAttributes.P_ALT_CARRIER) != null) {
			ci = (CarrierInfo) leg2Data.get(LegDataAttributes.P_ALT_CARRIER);

			carrierByte = getCarrierEncodedBuffer(ci);
			AlternateCarrier ansgenCarrier = new AlternateCarrier();
			ansgenCarrier.setValue(new com.agnity.ain.asngenerated.CarrierFormat(carrierByte));
			connectArg.setAlternateCarrier(ansgenCarrier);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting AlternateCarrier as:[" + ci + "], byte: " + CommonUtils.formatBytes(carrierByte));
			}
		}

		// ENcode second alternate carrier
		if (leg2Data.get(LegDataAttributes.P_SECOND_ALT_CARRIER) != null) {
			ci = (CarrierInfo) leg2Data.get(LegDataAttributes.P_SECOND_ALT_CARRIER);

			carrierByte = getCarrierEncodedBuffer(ci);
			SecondAlternateCarrier ansgenCarrier = new SecondAlternateCarrier();
			ansgenCarrier.setValue(new com.agnity.ain.asngenerated.CarrierFormat(carrierByte));
			connectArg.setSecondAlternateCarrier(ansgenCarrier);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting SecondAlternateCarrier as:[" + ci + "], byte: "
						+ CommonUtils.formatBytes(carrierByte));
			}
		}

		// Encode AMASLPId. It should be of 9 digits

		String  amaspid=(String)leg2Data.get(LegDataAttributes.P_AMASLPID) ;

		if (amaspid != null && !amaspid.isEmpty()) {
			String digits = (String) leg2Data.get(LegDataAttributes.P_AMASLPID);

			if (StringUtils.isNotBlank(digits) && StringUtils.length(digits) != 9) {
				logger.error("Not encoding AMASlpID in AnalyzeRoute as length is not equal to 9, [" + digits + "]");
			} else {
				byte[] slpId = AddressSignal.encodeAdrsSignal(digits);

				AMAslpID asnSlpId = new AMAslpID();
				asnSlpId.setValue(slpId);

				connectArg.setAMAslpID(asnSlpId);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting AAMASlpID as:[" + digits + "], byte: " + CommonUtils.formatBytes(slpId));
				}
			}
		}

		// Encode Charge Number
		if (leg2Data.get(LegDataAttributes.P_CHARGE_NUMBER) != null) {
			PhoneNumber chargeNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_CHARGE_NUMBER);

			if (chargeNum.getAddress() != null
					&& !chargeNum.getAddress().isEmpty()) {
				AINDigits asnAinDigits = encodeAINDigits(chargeNum);

				ChargeNumber asnChargeNum = new ChargeNumber();
				asnChargeNum.setValue(asnAinDigits);

				connectArg.setChargeNumber(asnChargeNum);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting ChargeNumber as:[" + chargeNum
							+ "], byte: "
							+ CommonUtils.formatBytes(asnAinDigits.getValue()));
				}
			}
		}

		// Encode AMALineNumber. There could be upto 2 AMALineNumber as per standard.
		List<AMALineNumber> asnAMALineNumCollection = null;
		for (int i = 0; i < 2; ++i) {
			PhoneNumber lineNum = null;
			if (i == 0 && (leg2Data.get(LegDataAttributes.P_AMALINENUMBER) != null)) {
				lineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMALINENUMBER);

				if (logger.isDebugEnabled()) {
					logger.debug("AMALineNumber1 - Encoding value:" + lineNum);
				}
			} else if (i == 1 && (leg2Data.get(LegDataAttributes.P_AMALINENUMBER2) != null)) {
				lineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMALINENUMBER2);

				if (logger.isDebugEnabled()) {
					logger.debug("AMALineNumber2 - Encoding value:" + lineNum);
				}
			}

			if (lineNum != null && lineNum.getAddress()!=null &&!lineNum.getAddress().isEmpty()) {
				// In case of AMALineNumber the Nature of Number and number Plan is not
				// applicable
				lineNum.setNatureOfAddress(0);
				lineNum.setNumberingPlan(0);
				AINDigits asnAinDigits = encodeAINDigits(lineNum);

				AMALineNumber asnAmaLineNum = new AMALineNumber();
				asnAmaLineNum.setValue(asnAinDigits);

				if (asnAMALineNumCollection == null)
					asnAMALineNumCollection = new ArrayList<AMALineNumber>();

				asnAMALineNumCollection.add(asnAmaLineNum);
			}
		}
		if (asnAMALineNumCollection != null) {
			connectArg.setAMALineNumber(asnAMALineNumCollection);

			if (logger.isDebugEnabled()) {
				logger.debug(" Encoded AMALineNumber to AnalyzeRoute");
			}
		}

		// Encode AMAAlternateBillingIndicator
		if (leg2Data.get(LegDataAttributes.P_AMA_ALT_BILLING_NUMBER) != null) {
			PhoneNumber altLineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMA_ALT_BILLING_NUMBER);

			if (altLineNum.getAddress() != null
					&& !altLineNum.getAddress().isEmpty()) {
				AINDigits asnAinDigits = encodeAINDigits(altLineNum);

				AMAAlternateBillingNumber asnAmaAltBillNum = new AMAAlternateBillingNumber();
				asnAmaAltBillNum.setValue(asnAinDigits);

				connectArg.setAMAAlternateBillingNumber(asnAmaAltBillNum);
			}
		}

		// Encode PrimaryBillingIndicator
		if (leg2Data.get(LegDataAttributes.P_PRIMARY_BILLING_IND) != null) {
			BillingIndicatorInfo billInfo = (BillingIndicatorInfo) leg2Data
					.get(LegDataAttributes.P_PRIMARY_BILLING_IND);

			byte[] callType = AddressSignal.encodeAdrsSignal(billInfo.getCallType());
			byte[] srvFeatureId = AddressSignal.encodeAdrsSignal(billInfo.getServiceFeatureIdentifiation());

			byte[] billInd = new byte[4];

			if (callType.length == 1) {
				billInd[0] = 0x00;
				billInd[1] = callType[0];
			} else {
				billInd[0] = callType[0];
				billInd[1] = callType[1];
			}

			if (srvFeatureId.length == 1) {
				billInd[2] = 0x00;
				billInd[3] = srvFeatureId[0];
			} else {
				billInd[2] = srvFeatureId[0];
				billInd[3] = srvFeatureId[1];
			}

			BillingIndicator asnBillInd = new BillingIndicator();
			asnBillInd.setValue(billInd);

			PrimaryBillingIndicator primBillingInd = new PrimaryBillingIndicator();
			primBillingInd.setValue(asnBillInd);

			connectArg.setPrimaryBillingIndicator(primBillingInd);

			if (logger.isDebugEnabled()) {
				logger.debug(
						"Setting PrimaryBillingInd as:[" + billInfo + "], byte: " + CommonUtils.formatBytes(billInd));
			}
		}

		// Encode OverflowBillingIndicator
		if (leg2Data.get(LegDataAttributes.P_OVERFLOW_BILLING_IND) != null) {
			BillingIndicatorInfo billInfo = (BillingIndicatorInfo) leg2Data
					.get(LegDataAttributes.P_OVERFLOW_BILLING_IND);

			byte[] callType = AddressSignal.encodeAdrsSignal(billInfo.getCallType());
			byte[] srvFeatureId = AddressSignal.encodeAdrsSignal(billInfo.getServiceFeatureIdentifiation());

			byte[] billInd = new byte[4];

			if (callType.length == 1) {
				billInd[0] = 0x00;
				billInd[1] = callType[0];
			} else {
				billInd[0] = callType[0];
				billInd[1] = callType[1];
			}

			if (srvFeatureId.length == 1) {
				billInd[2] = 0x00;
				billInd[3] = srvFeatureId[0];
			} else {
				billInd[2] = srvFeatureId[0];
				billInd[3] = srvFeatureId[1];
			}

			BillingIndicator asnBillInd = new BillingIndicator();
			asnBillInd.setValue(billInd);

			OverflowBillingIndicator overflowBillInd = new OverflowBillingIndicator();
			overflowBillInd.setValue(asnBillInd);

			connectArg.setOverflowBillingIndicator(overflowBillInd);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting OverflowBillingIndicator as:[" + billInfo + "], byte: "
						+ CommonUtils.formatBytes(billInd));
			}
		}

		// Encode RedirectingPartyId
		if (leg2Data.get(LegDataAttributes.P_REDIRECTING_PARTY_ID) != null) {
			PhoneNumber redirPartyId = (PhoneNumber) leg2Data.get(LegDataAttributes.P_REDIRECTING_PARTY_ID);

			if (redirPartyId.getAddress() != null
					&& !redirPartyId.getAddress().isEmpty()) {
				CalledNatOfNumEnum natureOfAddrEnum = CalledNatOfNumEnum.fromInt(redirPartyId.getNatureOfAddress());

				// check if NOA is set to be inserted
				String rpNoa = (String) leg2Data.get(LegDataAttributes.P_REDIRECTING_PARTY_ID_NOA);

				// Nature of address
				if(StringUtils.isNotBlank(rpNoa)){
					int rpNoaInt = -1;
					try{
						rpNoaInt = Integer.parseInt(rpNoa);
						natureOfAddrEnum = CalledNatOfNumEnum.fromInt(rpNoaInt);
					}catch(Exception ex){ }

					if(logger.isDebugEnabled()){
						logger.debug("Redirecting Party ID: NOA: "+ natureOfAddrEnum);
					}
				}

				if (natureOfAddrEnum == null) {
					natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
				}

				// Numbering Plan 
				String rpNp = (String) leg2Data.get(LegDataAttributes.P_REDIRECTING_PARTY_ID_NP);

				NumPlanEnum numberPlan = NumPlanEnum.fromInt(redirPartyId.getNumberingPlan());

				if(StringUtils.isNotBlank(rpNp)){
					int rpNpInt = -1;
					try{
						rpNpInt = Integer.parseInt(rpNp);
						numberPlan = NumPlanEnum.fromInt(rpNpInt);
					}catch(Exception ex){ }

					if(logger.isDebugEnabled()){
						logger.debug("Redirecting Party ID: NP: "+ numberPlan);
					}
				}

				if (numberPlan == null) {
					numberPlan = NumPlanEnum.ISDN_NP;
				}

				ainDigits = new AinDigits();
				ainDigits.setAddrSignal(redirPartyId.getAddress());
				ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
				ainDigits.setNumPlanEnum(numberPlan);

				byte[] redirPtyId = ainDigits.encodeAinDigits();

				RedirectingPartyID asnRedirPartyId = new RedirectingPartyID();
				asnRedirPartyId.setValue(new AINDigits(redirPtyId));

				connectArg.setRedirectingPartyID(asnRedirPartyId);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting RedirectingPartyID as:[" + redirPartyId + "], byte: "
							+ CommonUtils.formatBytes(redirPtyId));
				}
			}
		}

		// Encode PrimaryTrunkGroup
		if (leg2Data.get(LegDataAttributes.P_PRIMARY_TRUNK_GROUP) != null) {
			TrunkGroupInfo trgInf = (TrunkGroupInfo) leg2Data.get(LegDataAttributes.P_PRIMARY_TRUNK_GROUP);

			byte[] trunkGroupByte = TrunkGroup.encodeAlternateTrunkGrp(
					CallTreatIndicatorEnum.fromInt(trgInf.getCallTreatmentInd()),
					NumToOutpulseEnum.fromInt(trgInf.getNumToOutpulse()), trgInf.getSfg(),
					trgInf.getTrunkGroupAddress());

			PrimaryTrunkGroup asnPriTrunkGrp = new PrimaryTrunkGroup();
			asnPriTrunkGrp.setValue(trunkGroupByte);

			connectArg.setPrimaryTrunkGroup(asnPriTrunkGrp);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting PrimaryTrunkGroup as:[" + trgInf + "], byte: "
						+ CommonUtils.formatBytes(trunkGroupByte));
			}
		}

		// Alternate Trunk Group 
		if (leg2Data.get(LegDataAttributes.P_ALT_TRUNK_GROUP) != null) {
			TrunkGroupInfo trgInf = (TrunkGroupInfo) leg2Data.get(LegDataAttributes.P_ALT_TRUNK_GROUP);

			byte[] trunkGroupByte = TrunkGroup.encodeAlternateTrunkGrp(
					CallTreatIndicatorEnum.fromInt(trgInf.getCallTreatmentInd()),
					NumToOutpulseEnum.fromInt(trgInf.getNumToOutpulse()), trgInf.getSfg(),
					trgInf.getTrunkGroupAddress());

			AlternateTrunkGroup asnAltTrunkGrp = new AlternateTrunkGroup();
			asnAltTrunkGrp.setValue(trunkGroupByte);

			connectArg.setAlternateTrunkGroup(asnAltTrunkGrp);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting Alternate TrunkGroup as:[" + trgInf + "], byte: "
						+ CommonUtils.formatBytes(trunkGroupByte));
			}
		}

		// Second Alternate Trunk Group 
		if (leg2Data.get(LegDataAttributes.P_SECOND_ALT_TRUNK_GROUP) != null) {
			TrunkGroupInfo trgInf = (TrunkGroupInfo) leg2Data.get(LegDataAttributes.P_SECOND_ALT_TRUNK_GROUP);

			byte[] trunkGroupByte = TrunkGroup.encodeAlternateTrunkGrp(
					CallTreatIndicatorEnum.fromInt(trgInf.getCallTreatmentInd()),
					NumToOutpulseEnum.fromInt(trgInf.getNumToOutpulse()), trgInf.getSfg(),
					trgInf.getTrunkGroupAddress());

			SecondAlternateTrunkGroup asnAltTrunkGrp = new SecondAlternateTrunkGroup();
			asnAltTrunkGrp.setValue(trunkGroupByte);

			connectArg.setSecondAlternateTrunkGroup(asnAltTrunkGrp);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting Second Alternate TrunkGroup as:[" + trgInf + "], byte: "
						+ CommonUtils.formatBytes(trunkGroupByte));
			}
		}

		// AMADialedDigitsWC contains that customer dialed along wiht context ID to name
		// the digit
		// string like Access Code, Auth code, Customer Dialed Account Recording.
		// There could be upto 5 AMADialedDigitWC in AnalyzeRoute
		List dialedDigitWCList = null;
		for (int i = 0; i < 5; ++i) {
			PhoneNumber collectedDigits = null;

			switch (i) {
			case 0:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC1) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC1);
				}
				break;
			case 1:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC2) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC2);
				}
				break;
			case 2:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC3) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC3);
				}
				break;
			case 3:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC4) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC4);
				}
				break;
			case 4:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC5) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC5);
				}
				break;
			}

			if (collectedDigits != null && StringUtils.isNotBlank(collectedDigits.getAddress())) {
				collectedDigits.setNatureOfAddress(0);
				collectedDigits.setNumberingPlan(0);

				AINDigits asnAinDigits = encodeAINDigits(collectedDigits);
				AMADigitsDialedWC amaDigitWc = new AMADigitsDialedWC();
				amaDigitWc.setValue(asnAinDigits);

				if (dialedDigitWCList == null)
					dialedDigitWCList = new ArrayList<AMADigitsDialedWC>();

				dialedDigitWCList.add(amaDigitWc);
			}
		}
		if (dialedDigitWCList != null) {
			connectArg.setAMADigitsDialedWC(dialedDigitWCList);
		}

		// ChargePartyStattionType
		if (leg2Data.get(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE) != null) {
			String value = (String) leg2Data.get(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE);
			ChargePartyStationType ch = new ChargePartyStationType();
			ch.setValue(Integer.parseInt(value));
			connectArg.setChargePartyStationType(ch);
		}

		// Outpulse Number
		if (leg2Data.get(LegDataAttributes.P_OUTPULSE_NUM) != null) {
			PhoneNumber outpulseNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_OUTPULSE_NUM);

			if (outpulseNum.getAddress() != null
					&& !outpulseNum.getAddress().isEmpty()) {
				// outpulse number should have nature of number and numbering plan as 0
				outpulseNum.setNumberingPlan(1);
				outpulseNum.setNatureOfAddress(3);

				AINDigits asnAinDigits = encodeAINDigits(outpulseNum);

				OutpulseNumber asnOutpulseNum = new OutpulseNumber();
				asnOutpulseNum.setValue(asnAinDigits);

				connectArg.setOutpulseNumber(asnOutpulseNum);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting Outpulse Number as:[" + outpulseNum + "], byte: "
							+ CommonUtils.formatBytes(asnAinDigits.getValue()));
				}
			}
		}

		// Generic Address List.. There could be upto 5 addresses which be
		// encoded as Generic Address in a list. Currently adding support for 2
		// destination Address - Dialed Number and Destination Number
		GenericAddressList genericAddressList = null;
		List gaList = null;
		for (int i = 0; i < 5; ++i) {
			PhoneNumber destinationAddr = null;
			TypeOfAddrsEnum typeOfAddrs = null;

			switch (i) {
			case 0:
				// Check for Dialed Number
				if (leg2Data.get(LegDataAttributes.P_GENERIC_ADDR_DIALED_NUM) != null) {
					destinationAddr = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GENERIC_ADDR_DIALED_NUM);
					typeOfAddrs = TypeOfAddrsEnum.DAILED_NUM;
					destinationAddr.setNatureOfAddress(3);
					destinationAddr.setNumberingPlan(1);
				}
				break;
			case 1:
				// Check for Destination Number
				if (leg2Data.get(LegDataAttributes.P_GENERIC_ADDR_DESTINATION_NUM) != null) {
					destinationAddr = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GENERIC_ADDR_DESTINATION_NUM);
					typeOfAddrs = TypeOfAddrsEnum.DESTINATION_NO;
				}
				break;
			case 2:
				// Alternate Outpulse Number - as per CAIN alternate Outpulse shall be encoded as 
				// Generic Address. A.2.6 of CAIN doc
				if (leg2Data.get(LegDataAttributes.P_ALT_OUTPULSE_NUM) != null) {
					destinationAddr = (PhoneNumber) leg2Data.get(LegDataAttributes.P_ALT_OUTPULSE_NUM);
					typeOfAddrs = TypeOfAddrsEnum.CAIN_ALTERNATE_OUTPULSE_NO;
					destinationAddr.setNatureOfAddress(3);
					destinationAddr.setNumberingPlan(1);
				}
				break;
			case 3:
				// Second Alternate Outpulse Number - as per CAIN alternate Outpulse shall be encoded as 
				// Generic Address. 
				if (leg2Data.get(LegDataAttributes.P_SECOND_ALT_OUTPULSE_NUM) != null) {
					destinationAddr = (PhoneNumber) leg2Data.get(LegDataAttributes.P_SECOND_ALT_OUTPULSE_NUM);
					typeOfAddrs = TypeOfAddrsEnum.CAIN_SECOND_ALT_OUTPULSE_NO;
					destinationAddr.setNatureOfAddress(3);
					destinationAddr.setNumberingPlan(1);
				}
				break;
			case 4:
				break;
			}

			if (destinationAddr != null &&destinationAddr.getAddress() != null
					&& !destinationAddr.getAddress().isEmpty()) {
				byte[] value = GenericAddrs.encodeGenericAddrs(typeOfAddrs,
						NatureOfAddCallingEnum.fromInt(destinationAddr.getNatureOfAddress()),
						NumPlanEnum.fromInt(destinationAddr.getNumberingPlan()), TestIndEnum.fromInt(0),
						AddPrsntRestEnum.fromInt(destinationAddr.getAddressPresentationRestrictedIndicator()),
						destinationAddr.getAddress());
				GenericAddress ga = new GenericAddress();
				ga.setValue(value);

				if (gaList == null) {
					gaList = new ArrayList<GenericAddressList>();
				}

				gaList.add(ga);
			}
		}
		if (gaList != null) {
			genericAddressList = new GenericAddressList();
			genericAddressList.setValue(gaList);
			connectArg.setGenericAddressList(genericAddressList);
		}

		// AMA Business Customer ID
		if (leg2Data.get(LegDataAttributes.P_AMA_BUSINESS_CUSTOMER_ID) != null) {
			PhoneNumber amaBusCustId = (PhoneNumber) leg2Data.get
					(LegDataAttributes.P_AMA_BUSINESS_CUSTOMER_ID);

			if (amaBusCustId.getAddress() != null
					&& !amaBusCustId.getAddress().isEmpty()) {

				AinDigits sd = new AinDigits();
				sd.setAddrSignal(amaBusCustId.getAddress());
				sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
				sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

				AINDigits amaAinDigits = new AINDigits(sd.encodeAinDigits());

				AMABusinessCustomerID amaCustIdArg = new AMABusinessCustomerID();
				amaCustIdArg.setValue(amaAinDigits);

				connectArg.setAMABusinessCustomerID(amaCustIdArg);
			}
		}

		// Display Text 
		DisplayText dt = new DisplayText();
		List<DisplayInformation> displayInfoList = null;
		DisplayInformation disInfo = null;
		String displayTextLegData = null;
		byte [] displayValByte = null;
		int count=0;

		// Display Text Calling Party Name 
		displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_NAME);
		if(StringUtils.isNotBlank(displayTextLegData)){
			displayInfoList = new ArrayList<DisplayInformation>();
			disInfo = new DisplayInformation();
			displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

			disInfo.selectCallingPartyName(displayValByte);
			displayInfoList.add(disInfo);

			if(logger.isDebugEnabled()){
				logger.debug("AnalyzeRoute: DisplayText Calling Name:"+ displayTextLegData + " byte:"
						+ CommonUtils.formatBytes(displayValByte));
			}
			count++;
		}

		// Display Text Calling Party Address
		displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_ADDR);
		if(StringUtils.isNotBlank(displayTextLegData)){
			if(count == 0){
				displayInfoList = new ArrayList<DisplayInformation>();
			}
			disInfo = new DisplayInformation();
			displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

			disInfo.selectCallingAddress(displayValByte);
			displayInfoList.add(disInfo);

			if(logger.isDebugEnabled()){
				logger.debug("AnalyzeRoute: DisplayText Calling Address:"+ displayTextLegData + " byte:"
						+ CommonUtils.formatBytes(displayValByte));
			}
			count++;
		}

		// Display Text Called Party Name
		displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_NAME);
		if(StringUtils.isNotBlank(displayTextLegData)){
			if(count == 0){
				displayInfoList = new ArrayList<DisplayInformation>();
			}
			disInfo = new DisplayInformation();
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_NAME);
			displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

			disInfo.selectCalledPartyName(displayValByte);
			displayInfoList.add(disInfo);

			if(logger.isDebugEnabled()){
				logger.debug("AnalyzeRoute: DisplayText Called Name:"+ displayTextLegData + " byte:"
						+ CommonUtils.formatBytes(displayValByte));
			}
			count++;
		}

		// Display Text Called Party Address 
		displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_ADDR);
		if(StringUtils.isNotBlank(displayTextLegData)){
			if(count == 0){
				displayInfoList = new ArrayList<DisplayInformation>();
			}
			disInfo = new DisplayInformation();
			displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

			disInfo.selectCalledAddress(displayValByte);
			displayInfoList.add(disInfo);

			if(logger.isDebugEnabled()){
				logger.debug("AnalyzeRoute: DisplayText Called Address:"+ displayTextLegData + " byte:" 
						+ CommonUtils.formatBytes(displayValByte));
			}
			count++;
		}

		if(count > 0){
			dt.setValue(displayInfoList);
			connectArg.setDisplayText(dt);

			if(logger.isDebugEnabled()){
				logger.debug("AnalyzeRoute setting DisplayText");
			}
		}

		// CAIN Extension parameters
		String isCainEnabled = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.CAIN_ENABLED);

		if (PhConstants.TRUE.equals(isCainEnabled)) {
			// Get the encoded params for CAIN
			byte [] extnParams = CAINExtensionParameterHelper.encodeCainParameters(callData,false);

			if(extnParams != null){
				if(logger.isDebugEnabled()){
					logger.debug("AnalyzeRoute: CAIN is enabled, encoded Bytes:" + 
							CommonUtils.formatBytes(extnParams));
				}

				ExtensionParameter extParam = new ExtensionParameter();
				ExtensionParameterSequenceType extParamSeqType = new 
						ExtensionParameterSequenceType();

				ObjectIdentifier objId = new ObjectIdentifier();
				objId.setValue("1.2.840.113533.8.65.16");

				extParamSeqType.setAssignmentAuthority(objId);
				extParamSeqType.setParameters(extnParams);

				extParam.setValue(extParamSeqType);
				connectArg.setExtensionParameter(extParam);
			}else{
				if(logger.isDebugEnabled()){
					logger.debug("AnalyzeRoute: CAIN is enabled, however no value set by application");
				}
			}
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.ANALYZE_ROUTE);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectArg);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

//	/**
//	 * This method is called by the protocol handler for creating connection control
//	 * response
//	 * 
//	 * @param callData represents an instance of CallData
//	 * @return an instance of byte[]
//	 * @throws Exception
//	 */
//	public static byte[] createConnectionControlTerm(CallData callData) throws Exception {
//		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
//		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
//		byte[] returnBuffer = null;
//		if (logger.isDebugEnabled()) {
//			logger.debug("[PH]:: Inside createConnectionControlTerm");
//		}
//		ConnectionControlArg connectionControlArg = new ConnectionControlArg();
//		//if (leg2Data.get(LegDataAttributes.P_NETWORK_ROUTING_NUMBER) != null) {
//
//		if (leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER) != null) {
//			PhoneNumber routingNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);
//
//			// encode ROuting Number
//			Digits routingNumberDigits = new Digits();
//			routingNumberDigits.setValue(encodeT1RoutingDigits(
//					TypeOfDigitEnum.ROUTING_NUMBER, routingNumber.getAddress(),
//					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.ISDN_NP, 0));
//
//			connectionControlArg.setCarrierId(routingNumberDigits);
//
//			if(logger.isDebugEnabled()){
//				logger.debug("ConnectionControl: RoutingNumber:" +
//						CommonUtils.formatBytes(routingNumberDigits.getValue()));
//			}
//		}else{
//			// throw end error
//			if(logger.isDebugEnabled()){
//				logger.debug("ConnectionControl: RoutingNumber not set");
//			}
//		}
//
//		// Encode Carrier ID type-8
//		CarrierInfo ci = null;
//		if (leg2Data.get(LegDataAttributes.P_CARRIER) != null) {
//			ci = (CarrierInfo) leg2Data.get(LegDataAttributes.P_CARRIER);
//		}else if(leg2Data.get(LegDataAttributes.PI_CARRIER) != null){
//			String cic = (String) leg2Data.get(LegDataAttributes.PI_CARRIER);
//
//			if(StringUtils.isNotBlank(cic)){
//				ci = new CarrierInfo();
//				ci.setAddress(cic);
//
//				logger.debug("createConnectionControlTerm: CIC for PI_CARRIER is " + cic);
//			}else{
//				logger.debug("createConnectionControlTerm: CIC for PI_CARRIER is null");
//			}
//		}
//
//		if(ci != null){
//			Digits cic = new Digits();
//
//			String cicDigits = ci.getAddress();
//			cicDigits = StringUtils.rightPad(cicDigits, 4, '0');
//
//			if(leg1Data.get(LegDataAttributes.P_PI_O_CIC_SUPPORT) != null){
//				cicDigits = StringUtils.rightPad(cicDigits, 4, '0');
//			}else{
//				cicDigits = StringUtils.rightPad(cicDigits, 4, '0');
//				cicDigits = cicDigits.substring(0, 3);
//			}
//
//			// as per T1.TRQ3 standard, if provide instruction contains CIC extension then 
//			// we need to send 3 digits of carrier else 4 digits
//			byte [] cicByte = encodeT1RoutingDigits(
//					TypeOfDigitEnum.CARRIER_IDENTIFICATION, cicDigits,
//					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.ISDN_NP, 0);
//
//			if(leg1Data.get(LegDataAttributes.P_PI_O_CIC_SUPPORT) == null){
//				int len = cicByte.length;
//				cicByte[len-1] |= 0xF0;
//			}
//
//			cic.setValue(encodeT1RoutingDigits(
//					TypeOfDigitEnum.CARRIER_IDENTIFICATION, cicDigits,
//					NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.ISDN_NP, 0));
//
//			connectionControlArg.setNetworkRoutingNumber(cic);
//
//			if (logger.isDebugEnabled()) {
//				logger.debug("Setting Carrier as:[" + ci + "], byte: " + CommonUtils.formatBytes(cic.getValue()));
//			}
//		}else{
//			if(logger.isDebugEnabled()){
//				logger.debug("ConnectionControl: Carrier not set");
//			}			
//		}
//
//		// Encode BillingIndicator
//		if (leg2Data.get(LegDataAttributes.PI_AMA_CALLTYPE) != null && 
//				leg2Data.get(LegDataAttributes.PI_SERVICE_FEATURE_ID) != null) {
//
//			String callTypeStr = (String)leg2Data.get(LegDataAttributes.PI_AMA_CALLTYPE);
//			String srvFeatureStr = (String)leg2Data.get(LegDataAttributes.PI_SERVICE_FEATURE_ID);
//
//			if(StringUtils.isNotBlank(callTypeStr)  && StringUtils.isNotBlank(srvFeatureStr)){
//				// right pad with 0
//				callTypeStr = StringUtils.rightPad(callTypeStr, 4, '0');
//				srvFeatureStr = StringUtils.rightPad(srvFeatureStr, 4, '0');
//
//				Digits billIndDigits = new Digits();
//				billIndDigits.setValue(encodeT1RoutingDigits(
//						TypeOfDigitEnum.BILLING_NUMBER, (callTypeStr+srvFeatureStr),
//						NatureOfNumEnum.NATIONAL_NPR, NumPlanEnum.ISDN_NP, 0));
//
//				if (logger.isDebugEnabled()) {
//					logger.debug("Provide Instruction: Billing Num: " + CommonUtils.formatBytes(billIndDigits.getValue()));
//				}
//
//				connectionControlArg.setBillingIndicators(billIndDigits);
//			}
//		}else{
//			if(logger.isDebugEnabled()){
//				logger.debug("ConnectionControl: Billing number not set");
//			}	
//		}
//
//		LinkedList<String> opCode = new LinkedList<String>();
//		opCode.add(AinOpCodes.CONNECTION_CONTROL);
//		LinkedList<Object> operationObjs = new LinkedList<Object>();
//		operationObjs.add(connectionControlArg);
//		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
//		returnBuffer = encodeList.getFirst();
//		//changing zero'th octet to 0xF2 for parameter set.
//		returnBuffer[0] = (byte) 0xF2;
//		//changing second octet to 0x84 for TCAP digits.
//		returnBuffer[2] = (byte) 0x84;
//		return returnBuffer;
//	}

	/**
	 * This method is called on receiving Term Sieze
	 * 
	 * @param callData       Call specific Data
	 * @param invokeIndEvent Invoked Indication Event
	 * @param tcapSession    TcapSession
	 * @return actions to be performed
	 * @throws ASNParsingException
	 * @throws ParameterOutOfRangeException
	 */
	protected static Action[] parseTermSiezed(CallData callData, InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws ASNParsingException, ParameterOutOfRangeException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside parseTermSiezed");
		}
		OTermSeizedArg oTermSz;
		try {
			oTermSz = (OTermSeizedArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding OTermSeizedArg " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId + ":: [PH] OTermSeizedArg parsing failure occured, due to Enum paramter out of range."
							+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding OTermSeizedArg " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse OTermSeizedArg", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, OTermSeizedArg parsing failure occured.", e,
					MESSAGE.ERB_TERMSIEZED);
		}

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.TERM_SEIZED_RECIEVED);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Exiting parseTermSiezed");
		}
		return null;

	}

	/**
	 * This method parse the release cause
	 * 
	 * @param releaseCause
	 * @param callData
	 * @throws InvalidInputException
	 * @throws EnumParamOutOfRangeException
	 */
	public static void parseReleaseCause(com.agnity.ain.asngenerated.DisconnectCause releaseCause, CallData callData)
			throws InvalidInputException, EnumParamOutOfRangeException {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside parseReleaseCause");
		}

		if (releaseCause != null) {
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, releaseCause.getValue().getIntegerForm());
			callData.set(CallDataAttribute.P_CAUSE_VALUE_FROM_MSG, PhConstants.TRUE);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Exiting parseReleaseCause");
		}

	}

	/**
	 * This method is called by the Protocol handler whenever an ErbBusy event is
	 * received.
	 * @param tcapSession represents the instance of TcapSession
	 * @return an array of Action Objects
	 * @throws ParameterOutOfRangeException
	 */
	static Action[] parseBusy(CallData callData, InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws ASNParsingException, ParameterOutOfRangeException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside parseBusy");
		}
		OCalledPartyBusyArg oCalledPartyBusy;
		try {
			oCalledPartyBusy = (OCalledPartyBusyArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding OCalledPartyBusyArg " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] OCalledPartyBusyArg parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding OCalledPartyBusyArg " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse OCalledPartyBusyArg", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, OCalledPartyBusyArg parsing failure occured.", e,
					MESSAGE.ERB_BUSY);
		}

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.BUSY_RCVD_FROM_TERM);

		legData.set(LegDataAttributes.P_CAUSE_CODE, AinScfRelReasonCode.CAUSE_CODE_BUSY);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Exiting parseBusy");
		}

		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever an No Answer is
	 * received event is received.
	 * 
	 * @param callData
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws ASNParsingException
	 * @throws ParameterOutOfRangeException
	 */
	static Action[] parseNoAns(CallData callData, InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws ASNParsingException, ParameterOutOfRangeException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside parseNoAns");
		}

		ONoAnswerArg oNoAns;
		try {
			oNoAns = (ONoAnswerArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding  ONoAnswerArg" + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId + ":: [PH] ONoAnswerArg parsing failure occured, due to Enum paramter out of range."
							+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding ONoAnswerArg " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse ONoAnswerArg", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, ONoAnswerArg parsing failure occured.", e,
					MESSAGE.ERB_NOANS);
		}

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.NOANS_RCVD_FROM_TERM);

		// Set cause code for alternate routing
		legData.set(LegDataAttributes.P_CAUSE_CODE, AinScfRelReasonCode.CAUSE_CODE_NOANSWER);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Exiting parseNoAns");
		}

		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever an ErbAnswer message
	 * is received.
	 * 
	 * @param callData
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws ASNParsingException
	 * @throws ParameterOutOfRangeException
	 */
	static Action[] parseAns(CallData callData, InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws ASNParsingException, ParameterOutOfRangeException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside parseAns");
		}

		OAnswerArg oAns;
		try {
			oAns = (OAnswerArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding OAnswerArg " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] ERB parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding OAnswerArg " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse OAnswerArg", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, OAnswerArg parsing failure occured.", e,
					MESSAGE.ERB_ANS);
		}

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERM_CONNECTED);
		callData.set(CallDataAttribute.P_CALL_CONNECT_TIME, new Date());

		Date startTime = (Date) callData.get(CallDataAttribute.P_CALL_START_TIME);
		Date answerTime = (Date) callData.get(CallDataAttribute.P_CALL_CONNECT_TIME);
		long diffSeconds = (answerTime.getTime() - startTime.getTime())/ 1000 ;
		callData.set(CallDataAttribute.P_ALERTING_DURATION, Long.toString(diffSeconds));

		AinScfProtocolUtil.startCdrTimer(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Set attempted indicator to 0");
		}

		callData.set(CallDataAttribute.P_ATTEMPTED_IND, 0);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Exiting parseAns");
		}

		return null;

	}

	/**
	 * This method is called by the Protocol handler whenever an Disconnect message
	 * is received.
	 * 
	 * @param callData
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws ASNParsingException
	 * @throws ParameterOutOfRangeException
	 * @throws InvalidInputException
	 * @throws EnumParamOutOfRangeException
	 */
	static Action[] parseDisconnect(CallData callData, InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws ASNParsingException, ParameterOutOfRangeException, InvalidInputException,
			EnumParamOutOfRangeException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside parseDisconnect");
		}

		ODisconnectArg odisc;
		try {
			odisc = (ODisconnectArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding ODisconnectArg " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId + ":: [PH] ODisconnectArg parsing failure occured, due to Enum paramter out of range."
							+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding ODisconnectArg " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse ODisconnectArg", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, ODisconnectArg parsing failure occured.", e,
					MESSAGE.ERB_DISCONNECT);
		}
		if (odisc.isLegIDPresent()) {
			LegID legid = odisc.getLegID();
			callData.set(CallDataAttribute.P_SS7_LEG_ID, legid.getValue());
		}

		parseReleaseCause(odisc.getDisconnectCause(), callData);
		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERMINATED);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.ERB_ODISC_RCVD);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Exiting parseDisconnect");
		}

		return null;

	}

	/**
	 * This method is called by the Protocol handler whenever an ErbAnswer event is
	 * received.
	 * 
	 * @param callData
	 * @param invokeIndEvent
	 * @param tcapSession
	 * @return
	 * @throws ASNParsingException
	 * @throws ParameterOutOfRangeException
	 * @throws InvalidInputException
	 * @throws EnumParamOutOfRangeException
	 */
	static Action[] parseAbandon(CallData callData, InvokeIndEvent invokeIndEvent, TcapSession tcapSession)
			throws ASNParsingException, ParameterOutOfRangeException, InvalidInputException,
			EnumParamOutOfRangeException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside parseAbandon");
		}

		OAbandonArg oAbandon;
		try {
			oAbandon = (OAbandonArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding OAbandonArg " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId + ":: [PH] OAbandonArg parsing failure occured, due to Enum paramter out of range."
							+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding OAbandonArg " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse OAbandonArg", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, OAbandonArg parsing failure occured.", e,
					MESSAGE.ERB_ABANDON);
		}

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, AinCallStates.TERMINATED);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, AinScfRelReasonCode.ERB_OABANDON_RCVD);

		legData.set(LegDataAttributes.P_CAUSE_CODE, AinScfRelReasonCode.ERB_OABANDON_RCVD);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside parseAbandon");
		}

		return null;

	}

	/**
	 * This method is used to create disconnect
	 * 
	 * @param callData Call Data object
	 * @param action
	 * @return
	 * @throws Exception
	 */
	public static byte[] createDisconnect(CallData callData, Action action) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Inside createDisconnect");
		}

		DisconnectArg disConnectArg = new DisconnectArg();

		// CAIN Extension parameters
		String isCainEnabled = AinScfProtocolConfig.getConfigData(AinScfProtocolConfig.CAIN_ENABLED);

		if (PhConstants.TRUE.equals(isCainEnabled)) {
			// Get the encoded params for CAIN
			byte [] extnParams = CAINExtensionParameterHelper.encodeCainParameters(callData,true);

			if(extnParams != null){
				if(logger.isDebugEnabled()){
					logger.debug("Disconnect: CAIN is enabled, encoded Bytes:" + 
							CommonUtils.formatBytes(extnParams));
				}

				ExtensionParameter extParam = new ExtensionParameter();
				ExtensionParameterSequenceType extParamSeqType = new 
						ExtensionParameterSequenceType();

				ObjectIdentifier objId = new ObjectIdentifier();
				objId.setValue("1.2.840.113533.8.65.16");

				extParamSeqType.setAssignmentAuthority(objId);
				extParamSeqType.setParameters(extnParams);

				extParam.setValue(extParamSeqType);
				disConnectArg.setExtensionParameter(extParam);
			}else{
				if(logger.isDebugEnabled()){
					logger.debug("Disconnect: CAIN is enabled, however no value set by application");
				}
			}
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.DISCONNECT);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(disConnectArg);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Exiting createDisconnect");
		}

		return encodeList.getFirst();
	}

	/**
	 * This method is used to parse network busy
	 * 
	 * @param invokeIndEvent   Network Busy message
	 * @param callData         Call Data object
	 * @param componentInBegin
	 * @throws ParameterOutOfRangeException
	 * @throws ASNParsingException
	 */
	public static void parseNetworkBusy(InvokeIndEvent invokeIndEvent, CallData callData, boolean componentInBegin)
			throws ParameterOutOfRangeException, ASNParsingException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside parseNetworkBusy rxed as "
					+ ((componentInBegin == true) ? "BEGIN" : "CWP"));
		}

		NetworkBusyArg nwBusy;
		try {
			nwBusy = (NetworkBusyArg) AinOperationsCoding.decodeOperation(invokeIndEvent);

			// Parse network Busy as it is received as Begin
			if (componentInBegin) {
				// User ID
				UserID uid = nwBusy.getUserID();

				String dn = null;
				Integer facilityId = null;
				Integer trunkGroup = null;
				String adsstr = null;

				if (uid.getValue().getDn() != null) {
					dn = AddressSignal.decodeAdrsSignal(uid.getValue().getDn().getValue(), 0, 0);
				}

				if (uid.getValue().getPrivateFacilityGID() != null) {
					facilityId = uid.getValue().getPrivateFacilityGID().getValue();
				}

				if (uid.getValue().getTrunkGroupID() != null) {
					trunkGroup = uid.getValue().getTrunkGroupID().getValue();
				}

				if (uid.getValue().getADSIcpeID() != null) {
					adsstr = AddressSignal.decodeAdrsSignal(uid.getValue().getADSIcpeID().getValue(), 0, 0);
				}

				com.agnity.mphdata.common.UserID commonUser = new com.agnity.mphdata.common.UserID(dn, facilityId,
						trunkGroup, adsstr);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted user id  " + commonUser);
				}
				legData.set(LegDataAttributes.P_USER_ID, commonUser);

				// Bearer Capability

				// Lata
				if (nwBusy.isLataPresent()) {
					Lata lata = nwBusy.getLata();

					AinDigits ainDigits = new AinDigits();
					ainDigits.decodeAinDigits(lata.getValue().getValue(), Constant.CALLED);

					if (logger.isDebugEnabled()) {
						logger.debug(
								dialogueId + ":: [PH] Extracted LATA from NetworkBusy " + ainDigits.getAddrSignal());
					}

					legData.set(LegDataAttributes.P_LATA, ainDigits.getAddrSignal());
				}

				// ChargeNumber
				if (nwBusy.isChargeNumberPresent()) {
					ChargeNumber chargeNumber = nwBusy.getChargeNumber();

					ChargeNum chargeNum = new ChargeNum();
					chargeNum.decodeChargeNum(nwBusy.getChargeNumber().getValue().getValue());
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: [PH] Extracted Charge number from NetworkBusy is " + chargeNum);
					}

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Exit parseCharging number");
					}

					PhoneNumber chargePN = new PhoneNumber();
					chargePN.setAddress(chargeNum.getAddrSignal());
					chargePN.setNumberingPlan(chargeNum.getNumPlanEnum().getCode());
					chargePN.setNatureOfAddress(chargeNum.getChargeNumEnum().getCode());

					legData.set(LegDataAttributes.P_CHARGE_NUMBER, chargePN);
				}

				// Trigger Criteria
				if (nwBusy.isTriggerCriteriaTypePresent()) {
					TriggerCriteriaTypeEnumType triggerCriteriaType = nwBusy.getTriggerCriteriaType().getValue();
					legData.set(LegDataAttributes.P_TRIGGER_CRITERIA, triggerCriteriaType.getValue().ordinal());
				}

				// Calling Party ID
				if (nwBusy.isCallingPartyIDPresent()) {
					AinDigits callingPartyNumdigit = new AinDigits();
					AinDigits callingPartyNum = callingPartyNumdigit
							.decodeAinDigits(nwBusy.getCallingPartyID().getValue().getValue(), Constant.CALLING);

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: [PH] Extracted Calling party number from NetworkBusy is "
								+ callingPartyNum);
					}
					if (callingPartyNum == null || callingPartyNum.getAddrSignal() == null
							|| "".equals(callingPartyNum.getAddrSignal().trim())) {
						/*
						 * call should be handled as ASn parse failure as address signal is missing
						 */
						logger.error(dialogueId + ":: [PH] Calling party num address signal missing");
						throw new ASNParsingException(dialogueId + ":: [PH] Calling party num address signal missing",
								MESSAGE.NTWK_BUSY);
					}
					PhoneNumber callingNumber = parseCallingPartyNum(dialogueId, callingPartyNum);
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: [PH] Exit parseCallingPartyNum, CallingNumber:" + callingNumber);
					}
					legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
				} else {
					if (logger.isDebugEnabled()) {
						logger.debug("CallingPartyNumber not present in NetworkBusy");
					}
				}

				// Charge Party Station Type
				if (nwBusy.isChargePartyStationTypePresent()) {
					ChargePartyStationType chrgStation = nwBusy.getChargePartyStationType();

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: [PH] Extracted ChargePartyStationType from NetworkBusy: "
								+ chrgStation.getValue());
					}

					legData.set(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE, chrgStation.getValue());
				}

				// Called Party ID
				if (nwBusy.isCalledPartyIDPresent()) {

					AinDigits calliedPartyNumdigit = new AinDigits();
					AinDigits calledPartyNum = calliedPartyNumdigit
							.decodeAinDigits(nwBusy.getCalledPartyID().getValue().getValue(), Constant.CALLED);

					if (logger.isDebugEnabled()) {
						logger.debug(
								dialogueId + ":: [PH] Extracted called number from NetworkBusy is " + calledPartyNum);
					}

					PhoneNumber calledNumber = parseCalledPartyNum(dialogueId, calledPartyNum);
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + "::[PH] Exit parseCalledPartyNum, calledNumber:" + calledNumber);
					}
					legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);

					if(calledPartyNum.getCalledNatOfNumEnum() != null){
						legData.set(LegDataAttributes.P_CALLED_PARTY_NOA, calledPartyNum.getCalledNatOfNumEnum().getCode());
					}
				}

				// Carrier
				if (nwBusy.isCarrierPresent()) {
					Carrier carrier = nwBusy.getCarrier();

					CarrierFormat cf = new CarrierFormat();
					cf.decodeCarrierFormat(carrier.getValue().getValue());

					CarrierInfo ci = new CarrierInfo();
					ci.setAddress(cf.getAddrSignal());
					ci.setCarrierSelection(cf.getCarrierFormatSelectionEnum().getCode());
					ci.setNatureOfCarrier(cf.getCarrierFormatNatEnum().getCode());

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: [PH] Extracted carrier id from NetworkBusy " + ci);
					}

					legData.set(LegDataAttributes.P_CARRIER, ci);
				}

				// Bearer Capability 
				BearerCapabilityEnumType bearerEnumType = nwBusy.getBearerCapability().getValue();
				String bearerValue = bearerEnumType.getValue().name();

				legData.set(LegDataAttributes.P_BEAR_CAP, bearerValue);

				if(logger.isDebugEnabled()){
					logger.debug("InfoAnalyze: BearerCapability:" + bearerValue);
				}
			} else {
				legData.set(LegDataAttributes.P_CAUSE_CODE, AinScfRelReasonCode.CAUSE_CODE_BUSY);
			}

			/*
			 * NetworkBusy . orig called party number
			 */
			if (nwBusy.isOriginalCalledPartyIDPresent()) {
				OriginalCalledPartyID origCallAddr = nwBusy.getOriginalCalledPartyID();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(origCallAddr.getValue().getValue(), Constant.CALLING);

				PhoneNumber origCalledPn = new PhoneNumber();
				origCalledPn.setAddress(ainDigits.getAddrSignal());
				origCalledPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

				if(ainDigits.getCalgNatOfNumEnum() != null){
					origCalledPn.setNatureOfAddress(ainDigits.getCalgNatOfNumEnum().getCode());
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted OriginalCalledPartyID from NetworkBusy: "
							+ origCalledPn);
				}

				legData.set(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER, origCalledPn);
			}

			/*
			 * NetworkBusy .redirectingPartyID
			 */
			if (nwBusy.isRedirectingPartyIDPresent()) {
				RedirectingPartyID redirectParty = nwBusy.getRedirectingPartyID();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(redirectParty.getValue().getValue(), Constant.CALLED);

				PhoneNumber redirectPn = new PhoneNumber();
				redirectPn.setAddress(ainDigits.getAddrSignal());
				redirectPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

				if(ainDigits.getCalledNatOfNumEnum() != null){
					redirectPn.setNatureOfAddress(ainDigits.getCalledNatOfNumEnum().getCode());
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted REDIRECTING_PARTY_ID from InfoAnalyzed: "
							+ redirectPn);
				}

				legData.set(LegDataAttributes.P_REDIRECTING_PARTY_ID, redirectPn);

			}

			//AMP1
			if(nwBusy.isAmp1Present()){
				byte[] amp1Buffer = nwBusy.getAmp1().getValue();
				setAMP1Parameters(amp1Buffer, callData);
			}
			
			//AMP2
			if(nwBusy.isAmp2Present()){
				Amp2SequenceType amp2Seq = nwBusy.getAmp2().getValue();
				setAMP2Parameters(amp2Seq,callData);
			}
			
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Network busy " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId + ":: [PH]  Network busy parsing failure occured, due to Enum paramter out of range."
							+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding Network Busy " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse Network busy", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, Network Busy parsing failure occured.", e,
					MESSAGE.NTWK_BUSY);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Exiting parseNetworkBusy");
		}

	}

	/**
	 * This method is used to parse Resource Clear messaged received from network
	 * 
	 * @param invokeIndEvent Resource clear message
	 * @param callData       Call Data object
	 * @throws ParameterOutOfRangeException
	 * @throws ASNParsingException
	 */
	public static void parseResourceClear(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ParameterOutOfRangeException, ASNParsingException {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AinCallStates currCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside parseResourceClear in state:" + currCallState);
		}

		ResourceClearArg rcArg;
		try {
			rcArg = (ResourceClearArg) AinOperationsCoding.decodeOperation(invokeIndEvent);

			// Clear Cause
			int rcClearCause = rcArg.getClearCause().getValue();
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "RC: Clear Cuase:" + rcClearCause);
			}

			legData.set(LegDataAttributes.P_RC_CAUSE, rcClearCause);

			// CollectedDigits - Should be present in case rcClearCause is normal
			// and DIRECT_MS and PlayAndCollect in STR
			if (rcArg.isCollectedDigitsPresent()) {
				String digitsCollected = AinScfProtocolFieldCodec.decodeCollectedDigits(dialogueId,
						rcArg.getCollectedDigits().getValue().getValue(), "ResourceClear");

				legData.set(LegDataAttributes.P_COLLECTED_DIGITS, digitsCollected);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "RC: Digit Collected:" + digitsCollected);
				}
			}

			// IPReturn Block - this should be present in case rcClearCause is normal
			// and Flex Parameter is sent in STR
			if (rcArg.isIPReturnBlockPresent()) {

			}
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Resource clear " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] RC parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding Resource clear " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse RC", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, Network Busy parsing failure occured.", e,
					MESSAGE.RES_CLR);
		}
	}

	/**
	 * This method is used to parse Close message received from network. It extract
	 * Cause value and set in callData for key NP_RELEASE_REASON_VALUE
	 * 
	 * @param invokeIndEvent Close message
	 * @param callData       Call Data object
	 * @throws ParameterOutOfRangeException
	 * @throws ASNParsingException
	 */
	public static void parseClose(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ParameterOutOfRangeException, ASNParsingException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside parseClose");
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] Extract Close  Argument from Close");
		}
		CloseArg cArg;
		try {
			cArg = (CloseArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Close " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] Close parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding Close " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parseClose", e);
			}
			throw new ASNParsingException(dialogueId + ":: [PH] ASN Parsing Failure,Close parsing failure occured.", e,
					MESSAGE.CLOSE);
		}

		CloseCauseEnumType closeEnum = cArg.getCloseCause().getValue();
		callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, closeEnum.getValue().ordinal());
	}

	/**
	 * This method is used to send_to_resource message to network for creating ivr
	 * connection. It encodes following fields - DestinationAddress, CalledPartyId,
	 * PartyID, ResourceType, StrParamBlock
	 * 
	 * @param callData  Call Data Object
	 * @param localAddr SCCP user address
	 * @param leg       Identifies the leg to be conencetd for IVR.
	 * @return
	 * @throws Exception
	 */
	public static byte[] createSTR(CallData callData, SccpUserAddress localAddr, String leg) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside createSTR");
		}

		/*
		 * destinationAddress
		 */
		String assistSspIpRoutAddrStr = null;
		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
			assistSspIpRoutAddrStr = callData.get(CallDataAttribute.P_CORRELATION_ID).toString();
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Assisting SSP IP Routing Address formed as " + assistSspIpRoutAddrStr);
		}

		AinDigits sd = new AinDigits();
		sd.setAddrSignal(assistSspIpRoutAddrStr);
		sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
		sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

		AINDigits assistSspIpRoutAddrDigits = new AINDigits(sd.encodeAinDigits());

		DestinationAddress dA = new DestinationAddress();
		dA.setValue(assistSspIpRoutAddrDigits);
		SendToResourceArg sendToResourceArg = new SendToResourceArg();
		sendToResourceArg.setDestinationAddress(dA);

		/*
		 * CalledPartyId coding scheme should be odd or even decreateEtcpending on
		 * length of corrID
		 */
		// No need to set called party. Identified during interop with DMS100
		// String corrId=
		// String.valueOf(callData.get(CallDataAttribute.P_CORRELATION_ID));
		// EncodingSchemeEnum encodeSchemeEnum =null;
		// if(corrId.length()%2 == 0){
		// //for even lenth
		// encodeSchemeEnum=EncodingSchemeEnum.BCD_EVEN;
		// }else{
		// encodeSchemeEnum=EncodingSchemeEnum.BCD_ODD;
		// }
		//
		// byte[] correlationIdByteArr = GenericDigits.encodeGenericDigits(
		// encodeSchemeEnum, DigitCatEnum.BILL_TO_NUMBER,
		// corrId);
		// AINDigits correlationIdDigits = new AINDigits(correlationIdByteArr);
		//
		// CalledPartyID calledParty=new CalledPartyID();
		// calledParty.setValue(correlationIdDigits);
		//
		// sendToResourceArg.setCalledPartyID(calledParty);
		//
		//
		// if (logger.isDebugEnabled()) {
		// logger.debug(dialogueId + "::[PH] correlationId is " + corrId);
		// }

		/*
		 * partyToConnect
		 */
		// commenting as not required during interop with DMS100

		// PartyID pid= new PartyID();
		// PartyIDEnumType enumType= new PartyIDEnumType();
		// enumType.setValue(PartyIDEnumType.EnumType.callingParty);
		//
		// if (leg != null && leg.equals(CallDataAttribute.P_LEG2)) {
		// enumType.setValue(PartyIDEnumType.EnumType.calledParty);
		// }
		//
		// pid.setValue(enumType);
		// sendToResourceArg.setPartyID(pid);

		/*
		 * set Resource type
		 */
		ResourceType rstype = new ResourceType();
		rstype.setValue(4);
		sendToResourceArg.setResourceType(rstype);

		/*
		 * set Param block
		 */
		StrParameterBlock block = new StrParameterBlock();
		StrParameterBlockChoiceType type = new StrParameterBlockChoiceType();

		// Setting Flex Parameter Block
		FlexParameterBlock fpb = new FlexParameterBlock();
		byte[] flexByte = { (byte) 0xA3, 0x1E, (byte) 0x81, 0x04, 0x2B, 0x11, 0x69, 0x03, (byte) 0x82, 0x01, 0x00,
				(byte) 0xA3, 0x13, (byte) 0xA0, 0x11, (byte) 0xA1, 0x04, (byte) 0x80, 0x02, 0x0B, (byte) 0xB8,
				(byte) 0xA1, 0x09, (byte) 0x82, 0x07, 0x0B, (byte) 0x83, (byte) 0x80, 0x67, 0x38, 0x02, 0x00 };

		fpb.setValue(flexByte);
		type.selectFlexParameterBlock(fpb);
		block.setValue(type);
		sendToResourceArg.setStrParameterBlock(block);

		// AnnouncementDigitBlock adb= new AnnouncementDigitBlock();
		// MaximumDigits md= new MaximumDigits(15);
		// adb.setMaximumDigits(md);
		// type.selectAnnouncementDigitBlock(adb);
		// block.setValue(type);
		// sendToResourceArg.setStrParameterBlock(block);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(sendToResourceArg);
		opCode.add(AinOpCodes.SEND_TO_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This STR is Hawaiian specific switch which is sent in TC_END so that switch
	 * can play an announcement
	 * 
	 * @param callData
	 * @param localAddr
	 * @param leg
	 * @return
	 * @throws Exception
	 */
	public static byte[] createSTRForCallCompletion(CallData callData, SccpUserAddress localAddr, String leg)
			throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside createSTRForCallCompletion");
		}

		SendToResourceArg sendToResourceArg = new SendToResourceArg();

		// Resource Type as 0
		ResourceType rstype = new ResourceType();
		rstype.setValue(0);
		sendToResourceArg.setResourceType(rstype);

		/*
		 * set Param block StrParameterBlock StrParameterBlock ::= [50] CHOICE { [0]
		 * IMPLICIT AnnouncementBlock, [1] IMPLICIT AnnouncementDigitBlock, [2] IMPLICIT
		 * FlexParameterBlock }
		 *
		 * AnnouncementBlock ::= SEQUENCE { [1] IMPLICIT UninterAnnounceBlock OPTIONAL,
		 * [2] IMPLICIT InterAnnounceBlock OPTIONAL }
		 *
		 * UninterAnnounceBlock ::= SEQUENCE SIZE(1..10) OF AnnounceElement
		 * InterAnnounceBlock ::= SEQUENCE SIZE(1..10) OF AnnounceElement
		 * AnnounceElement ::= OCTET STRING (SIZE(3..131))
		 *
		 */
		StrParameterBlock block = new StrParameterBlock();

		// Create Announcement Element
		AnnounceElement annElmt = new AnnounceElement();
		byte[] item = { 0x00, 0x69, 0x00 };

		annElmt.setValue(item);

		// Set announcement element in Uninterrupted Announcement Block
		List<AnnounceElement> annList = new ArrayList<AnnounceElement>();
		annList.add(annElmt);

		UninterAnnounceBlock uniAnnBlk = new UninterAnnounceBlock(annList);

		// Set Uninterrupted announcement Block in Announcement Block
		AnnouncementBlock annBlock = new AnnouncementBlock();
		annBlock.setUninterAnnounceBlock(uniAnnBlk);

		// Set Announcement Block in Str Parameter Block
		StrParameterBlockChoiceType type = new StrParameterBlockChoiceType();
		type.selectAnnouncementBlock(annBlock);

		// Set Str Parameter Choice Type in Str Parameter Block
		block.setValue(type);

		// Set Str parameter in SendToResource message
		sendToResourceArg.setStrParameterBlock(block);

		// Set Disconnect Flag
		DisconnectFlag discFlag = new DisconnectFlag();
		sendToResourceArg.setDisconnectFlag(discFlag);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(sendToResourceArg);
		opCode.add(AinOpCodes.SEND_TO_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		// For testing with customer buffer
		// LinkedList<byte[]> encodeList = new LinkedList<byte[]>();
		//
		// byte[] buf = {0x30, 0x12, (byte) 0x9F, 0x2D, 0x01, 0x00, (byte) 0xBF, 0x32,
		// 0x09, (byte) 0xA0,
		// 0x07, (byte) 0xA1, 0x05, 0x04, 0x03, 0x00, 0x69, 0x00, (byte) 0x99, 0x00};
		// encodeList.add(buf);

		return encodeList.getFirst();
	}

	/**
	 * Method create buffer for Forward Call. This message is sent instead of STR to
	 * conenct to SoftSwtich (CS2k) to connect to media server.
	 * 
	 * @param callData
	 * @param leg
	 * @return
	 * @throws Exception
	 */
	public static byte[] createForwardCall(CallData callData, String leg) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		boolean encodeFullBuffer = true;
		AinDigits ainDigits = null;

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside createForwardCall");
		}

		ForwardCallArg forwardCallArg = new ForwardCallArg();

		/*
		 * Forward Call is sent in case of connecting to media server as well as end
		 * party Check if correlation ID is set. If yes then Forward call is for media
		 * server connection. Else send whatever parameters coming from application
		 */
		String calledPartyId = null;
		if (callData.get(CallDataAttribute.P_CORRELATION_ID) != null) {
			calledPartyId = callData.get(CallDataAttribute.P_CORRELATION_ID).toString();
			encodeFullBuffer = false;

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + "::Called Party ID: " + calledPartyId);
			}

			AinDigits sd = new AinDigits();
			sd.setAddrSignal(calledPartyId);
			sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
			sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

			AINDigits calledPartyAddrDigits = new AINDigits(sd.encodeAinDigits());

			CalledPartyID calledParty = new CalledPartyID();
			calledParty.setValue(calledPartyAddrDigits);

			// setting called party in forward call
			forwardCallArg.setCalledPartyID(calledParty);
		}

		// need to send Calling party Id in case of Forward Call. Else INVITE received
		// has NULL calling party ID
		// if(leg2Data.get(LegDataAttributes.P_CALLING_PARTY) != null &&
		// !encodeFullBuffer){
		// PhoneNumber callingParty = (PhoneNumber)
		// leg2Data.get(LegDataAttributes.P_CALLING_PARTY);
		//
		// if(callingParty != null){
		// CalledNatOfNumEnum natureOfAddrEnum =
		// CalledNatOfNumEnum.fromInt(callingParty.getNatureOfAddress());
		//
		// if (natureOfAddrEnum == null) {
		// natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
		// }
		//
		// NumPlanEnum numberPlan =
		// NumPlanEnum.fromInt(callingParty.getNumberingPlan());
		// if (numberPlan == null) {
		// numberPlan = NumPlanEnum.ISDN_NP;
		// }
		//
		// ainDigits= new AinDigits();
		// ainDigits.setAddrSignal(callingParty.getAddress());
		// ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
		// ainDigits.setNumPlanEnum(numberPlan);
		//
		// byte[] callingPartyBytes = ainDigits.encodeAinDigits();
		//
		// CallingPartyID callingPartyID = new CallingPartyID();
		// callingPartyID.setValue(new AINDigits(callingPartyBytes));
		//
		// forwardCallArg.setCallingPartyID(callingPartyID);
		//
		// if (logger.isDebugEnabled()) {
		// logger.debug("Setting Calling Party in ForwardCall as:[" + callingParty + "],
		// byte: " +
		// CommonUtils.formatBytes(callingPartyBytes));
		// }
		// }
		// }

		if (encodeFullBuffer) {

			if (logger.isDebugEnabled()) {
				logger.debug("Encoding Full buffer for Forward Call");
			}
			PhoneNumber destinationNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);

			if (destinationNumber != null) {

				CalledNatOfNumEnum natureOfAddrEnum = CalledNatOfNumEnum
						.fromInt(destinationNumber.getNatureOfAddress());

				if (natureOfAddrEnum == null) {
					natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
				}

				NumPlanEnum numberPlan = NumPlanEnum.fromInt(destinationNumber.getNumberingPlan());
				if (numberPlan == null) {
					numberPlan = NumPlanEnum.ISDN_NP;
				}

				ainDigits = new AinDigits();
				ainDigits.setAddrSignal(destinationNumber.getAddress());
				ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
				ainDigits.setNumPlanEnum(numberPlan);

				byte[] calledPartyNum = ainDigits.encodeAinDigits();

				CalledPartyID calledPartyNumber = new CalledPartyID();
				calledPartyNumber.setValue(new AINDigits(calledPartyNum));

				forwardCallArg.setCalledPartyID(calledPartyNumber);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting CalledPartyId in ForwardCall as:[" + destinationNumber + "], byte: "
							+ CommonUtils.formatBytes(calledPartyNum));
				}
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("Destination number not set in call data");
				}
			}

			// Encode carrier
			byte[] carrierByte = null;
			CarrierInfo ci = null;
			if (leg2Data.get(LegDataAttributes.P_CARRIER) != null) {
				ci = (CarrierInfo) leg2Data.get(LegDataAttributes.P_CARRIER);

				carrierByte = getCarrierEncodedBuffer(ci);
				Carrier ansgenCarrier = new Carrier();
				ansgenCarrier.setValue(new com.agnity.ain.asngenerated.CarrierFormat(carrierByte));
				forwardCallArg.setCarrier(ansgenCarrier);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting Carrier as:[" + ci + "], byte: " + CommonUtils.formatBytes(carrierByte));
				}
			}

			// Encode Charge Number
			if (leg2Data.get(LegDataAttributes.P_CHARGE_NUMBER) != null) {
				PhoneNumber chargeNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_CHARGE_NUMBER);

				AINDigits asnAinDigits = encodeAINDigits(chargeNum);

				ChargeNumber asnChargeNum = new ChargeNumber();
				asnChargeNum.setValue(asnAinDigits);

				forwardCallArg.setChargeNumber(asnChargeNum);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting ChargeNumber as:[" + chargeNum + "], byte: "
							+ CommonUtils.formatBytes(asnAinDigits.getValue()));
				}
			}

			// ChargePartyStattionType
			if (leg2Data.get(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE) != null) {
				String value = (String) leg2Data.get(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE);
				ChargePartyStationType ch = new ChargePartyStationType();
				ch.setValue(Integer.parseInt(value));
				forwardCallArg.setChargePartyStationType(ch);
			}

			// Display Text 
			DisplayText dt = new DisplayText();
			List<DisplayInformation> displayInfoList = null;
			DisplayInformation disInfo = null;
			String displayTextLegData = null;
			byte [] displayValByte = null;
			int count=0;

			// Display Text Calling Party Name 
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_NAME);
			if(StringUtils.isNotBlank(displayTextLegData)){
				displayInfoList = new ArrayList<DisplayInformation>();
				disInfo = new DisplayInformation();
				displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_NAME);
				displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

				disInfo.selectCallingPartyName(displayValByte);
				displayInfoList.add(disInfo);

				if(logger.isDebugEnabled()){
					logger.debug("ForwardCall: DisplayText Calling Name:"+ displayTextLegData + " byte:"
							+ CommonUtils.formatBytes(displayValByte));
				}
				count++;
			}

			// Display Text Calling Party Address
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_ADDR);
			if(StringUtils.isNotBlank(displayTextLegData)){
				if(count == 0){
					displayInfoList = new ArrayList<DisplayInformation>();
				}
				disInfo = new DisplayInformation();
				displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_ADDR);
				displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

				disInfo.selectCallingAddress(displayValByte);
				displayInfoList.add(disInfo);

				if(logger.isDebugEnabled()){
					logger.debug("ForwardCall: DisplayText Calling Address:"+ displayTextLegData + " byte:"
							+ CommonUtils.formatBytes(displayValByte));
				}
				count++;
			}

			// Display Text Called Party Name
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_NAME);
			if(StringUtils.isNotBlank(displayTextLegData)){
				if(count == 0){
					displayInfoList = new ArrayList<DisplayInformation>();
				}
				disInfo = new DisplayInformation();
				displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

				disInfo.selectCalledPartyName(displayValByte);
				displayInfoList.add(disInfo);

				if(logger.isDebugEnabled()){
					logger.debug("ForwardCall: DisplayText Called Name:"+ displayTextLegData + " byte:"
							+ CommonUtils.formatBytes(displayValByte));
				}
				count++;
			}

			// Display Text Called Party Address 
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_ADDR);
			if(StringUtils.isNotBlank(displayTextLegData)){
				if(count == 0){
					displayInfoList = new ArrayList<DisplayInformation>();
				}
				disInfo = new DisplayInformation();
				displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_ADDR);
				displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

				disInfo.selectCalledAddress(displayValByte);
				displayInfoList.add(disInfo);

				if(logger.isDebugEnabled()){
					logger.debug("ForwardCall: DisplayText Called Address:"+ displayTextLegData + " byte:" 
							+ CommonUtils.formatBytes(displayValByte));
				}
				count++;
			}

			if(count > 0){
				dt.setValue(displayInfoList);
				forwardCallArg.setDisplayText(dt);

				if(logger.isDebugEnabled()){
					logger.debug("ForwardCall setting DisplayText");
				}
			}

		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(forwardCallArg);

		opCode.add(AinOpCodes.FORWARD_CALL);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called to parse InfoCollected message received from the
	 * network. It extracts - callingPartyNumber, chargeNumber, CollectedDigits,
	 * userid, carrier, CollectedAddress, - OriginalCalledPartyId,
	 * ChargePartyStationType, RedirectingPartyId, ACGEncountered
	 * 
	 * @param invokeIndEvent InfoCollected message
	 * @param callData       Call Data object
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseInfoCollected(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {
		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] Extract INFO Collected Argument from IC");
		}
		InfoCollectedArg infoCollectArg;
		try {
			infoCollectArg = (InfoCollectedArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Info collected " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] IC parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in Info collected " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse IC", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, Info collected parsing failure occured.", e,
					MESSAGE.INFO_COLLECT);
		}

		try {
			// Bearer Capability 
			BearerCapabilityEnumType bearerEnumType = infoCollectArg.getBearerCapability().getValue();
			String bearerValue = bearerEnumType.getValue().name();

			legData.set(LegDataAttributes.P_BEAR_CAP, bearerValue);

			if(logger.isDebugEnabled()){
				logger.debug("infoCollectArg: BearerCapability:" + bearerValue);
			}

			/*
			 * InfoCollectedArg.callingPartyNumber
			 */

			if (infoCollectArg.isCallingPartyIDPresent()) {

				AinDigits aindigit = new AinDigits();
				AinDigits callingPartyNum = aindigit
						.decodeAinDigits(infoCollectArg.getCallingPartyID().getValue().getValue(), Constant.CALLING);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Calling party number from Info analyze is "
							+ callingPartyNum);
				}
				if (callingPartyNum == null || callingPartyNum.getAddrSignal() == null
						|| "".equals(callingPartyNum.getAddrSignal().trim())) {
					/*
					 * call should be handled as ASn parse failure as address signal is missing
					 */
					logger.error(dialogueId + ":: [PH] Calling party num address signal missing");
					throw new ASNParsingException(dialogueId + ":: [PH] Calling party num address signal missing",
							MESSAGE.INFO_COLLECT);
				}
				PhoneNumber callingNumber = parseCallingPartyNum(dialogueId, callingPartyNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parseCallingPartyNum");
				}
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
			}

			/*
			 * InfoCollectedArg.chargeNumber
			 */
			if (infoCollectArg.isChargeNumberPresent()) {
				ChargeNumber chargeNumber = infoCollectArg.getChargeNumber();

				ChargeNum chargeNum = new ChargeNum();
				chargeNum.decodeChargeNum(infoCollectArg.getChargeNumber().getValue().getValue());
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Charge number from Info collected is " + chargeNum);
				}

				PhoneNumber chargePN = new PhoneNumber();
				chargePN.setAddress(chargeNum.getAddrSignal());
				chargePN.setNumberingPlan(chargeNum.getNumPlanEnum().getCode());
				chargePN.setNatureOfAddress(chargeNum.getChargeNumEnum().getCode());

				legData.set(LegDataAttributes.P_CHARGE_NUMBER, chargePN);
			}

			/*
			 * Extract collected digits
			 */

			if (infoCollectArg.isCollectedDigitsPresent()) {

				CollectedDigits collectedDigits = infoCollectArg.getCollectedDigits();

				AinDigits collecDigits = AinDigits.getInstance().decodeAinDigits(collectedDigits.getValue().getValue(),
						Constant.CALLED);

				PhoneNumber collectedDigitsPn = parseCalledPartyNum(dialogueId, collecDigits);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted collectedDigits from Info Collected "
							+ collectedDigitsPn.getAddress());
				}

				legData.set(LegDataAttributes.P_COLLECTED_DIGITS, collectedDigitsPn);
			}

			/*
			 * InfoCollectedArg.userid
			 */
			UserID uid = infoCollectArg.getUserID();

			String dn = null;
			Integer facilityId = null;
			Integer trunkGroup = null;
			String adsstr = null;

			if (uid.getValue().getDn() != null) {
				dn = AddressSignal.decodeAdrsSignal(uid.getValue().getDn().getValue(), 0, 0);
			}

			if (uid.getValue().getPrivateFacilityGID() != null) {
				facilityId = uid.getValue().getPrivateFacilityGID().getValue();
			}

			if (uid.getValue().getTrunkGroupID() != null) {
				trunkGroup = uid.getValue().getTrunkGroupID().getValue();
			}

			if (uid.getValue().getADSIcpeID() != null) {
				adsstr = AddressSignal.decodeAdrsSignal(uid.getValue().getADSIcpeID().getValue(), 0, 0);
			}

			com.agnity.mphdata.common.UserID commonUser = new com.agnity.mphdata.common.UserID(dn, facilityId,
					trunkGroup, adsstr);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: [PH] Extracted user id  " + uid);
			}
			legData.set(LegDataAttributes.P_USER_ID, commonUser);

			/*
			 * InfoCollectedArg .carrier
			 */
			if (infoCollectArg.isCarrierPresent()) {
				Carrier carrier = infoCollectArg.getCarrier();

				CarrierFormat cf = new CarrierFormat();
				cf.decodeCarrierFormat(carrier.getValue().getValue());

				CarrierInfo ci = new CarrierInfo();
				ci.setAddress(cf.getAddrSignal());
				ci.setCarrierSelection(cf.getCarrierFormatSelectionEnum().getCode());
				ci.setNatureOfCarrier(cf.getCarrierFormatNatEnum().getCode());

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted carrier id from InfoCollected " + ci);
				}

				legData.set(LegDataAttributes.P_CARRIER, ci);
			}

			/*
			 * InfoCollectedArg .collected address
			 */
			if (infoCollectArg.isCollectedAddressInfoPresent()) {
				CollectedAddressInfo collectedAddr = infoCollectArg.getCollectedAddressInfo();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(collectedAddr.getValue().getValue(), Constant.CALLED);

				PhoneNumber collectedPn = new PhoneNumber();
				collectedPn.setAddress(ainDigits.getAddrSignal());
				collectedPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted CollectedAddressInfo from Info collected "
							+ collectedPn.getAddress());
				}

				legData.set(LegDataAttributes.P_COLLECTED_ADDRESS_INFO, collectedPn);

				if(ainDigits.getCalledNatOfNumEnum() != null){
					legData.set(LegDataAttributes.P_CALLED_PARTY_NOA, ainDigits.getCalledNatOfNumEnum().getCode());
				}

			}

			/*
			 * InfoCollectedArg . orig called party number
			 */
			if (infoCollectArg.isOriginalCalledPartyIDPresent()) {
				OriginalCalledPartyID origCallAddr = infoCollectArg.getOriginalCalledPartyID();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(origCallAddr.getValue().getValue(), Constant.CALLING);

				PhoneNumber origCalledPn = new PhoneNumber();
				origCalledPn.setAddress(ainDigits.getAddrSignal());
				origCalledPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

				if(ainDigits.getCalgNatOfNumEnum() != null){
					origCalledPn.setNatureOfAddress(ainDigits.getCalgNatOfNumEnum().getCode());
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted OriginalCalledPartyID from Info collected "
							+ origCalledPn);
				}

				legData.set(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER, origCalledPn);

			}

			/*
			 * InfoCollectedArg . charge party station type
			 */
			if (infoCollectArg.isChargePartyStationTypePresent()) {
				ChargePartyStationType chrgStation = infoCollectArg.getChargePartyStationType();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted ChargePartyStationType from Info collected "
							+ chrgStation.getValue());
				}

				legData.set(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE, chrgStation.getValue());

			}

			/*
			 * InfoCollectedArg. redirecting Party ID
			 */
			if (infoCollectArg.isRedirectingPartyIDPresent()) {
				RedirectingPartyID redirectParty = infoCollectArg.getRedirectingPartyID();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(redirectParty.getValue().getValue(), Constant.CALLED);

				PhoneNumber redirectPn = new PhoneNumber();
				redirectPn.setAddress(ainDigits.getAddrSignal());
				redirectPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

				if(ainDigits.getCalledNatOfNumEnum() != null){
					redirectPn.setNatureOfAddress(ainDigits.getCalledNatOfNumEnum().getCode());
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted REDIRECTING_PARTY_ID from Info collected "
							+ redirectPn.getAddress());
				}

				legData.set(LegDataAttributes.P_REDIRECTING_PARTY_ID, redirectPn);

			}

			/*
			 * InfoCollectedArg. ACG Encountered
			 */
			if (infoCollectArg.isACGEncounteredPresent()) {
				ACGEncountered acgEn = infoCollectArg.getACGEncountered();
				ACGEncounteredNonAsn acgNonAsn = new ACGEncounteredNonAsn();
				ACGEncounteredNonAsn nonasn = acgNonAsn.decodeACGEncounteredNonAsn(acgEn.getValue());
				int acgcode = nonasn.getAcgEncounteredEnum().getCode();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted ACGEncountered from Info collect " + acgEn);
				}

				callData.set(CallDataAttribute.P_ACG_ENCOUNTERED, acgcode);

			}

			/*
			 * LATA
			 */
			if (infoCollectArg.isLataPresent()) {
				Lata lata = infoCollectArg.getLata();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(lata.getValue().getValue(), Constant.CALLED);

				if (logger.isDebugEnabled()) {
					logger.debug(
							dialogueId + ":: [PH] Extracted LATA from Info Collected " + ainDigits.getAddrSignal());
				}

				legData.set(LegDataAttributes.P_LATA, ainDigits.getAddrSignal());
			}

			if (infoCollectArg.isTriggerCriteriaTypePresent()) {
				TriggerCriteriaTypeEnumType triggerCriteriaType = infoCollectArg.getTriggerCriteriaType().getValue();
				legData.set(LegDataAttributes.P_TRIGGER_CRITERIA, triggerCriteriaType.getValue().ordinal());
			}

			// Vertical code 
			if(infoCollectArg.isVerticalServiceCodePresent()){
				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(infoCollectArg.getVerticalServiceCode().getValue().getValue(), Constant.CALLED);

				PhoneNumber verticalCode = new PhoneNumber();

				// replace D with *
				String digits = StringUtils.replace(ainDigits.getAddrSignal(), "D", "*");
				digits = StringUtils.replace(digits, "d", "*");

				verticalCode.setAddress(digits);
				verticalCode.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::[PH] Exit parseVerticalCode, :" + verticalCode);
				}

				legData.set(LegDataAttributes.P_CALLED_PARTY, verticalCode);
			}

			//AMP1
			if(infoCollectArg.isAmp1Present()){
				byte[] amp1Buffer = infoCollectArg.getAmp1().getValue();
				setAMP1Parameters(amp1Buffer, callData);
			}
			
			//AMP2
			if(infoCollectArg.isAmp2Present()){
				Amp2SequenceType amp2Seq = infoCollectArg.getAmp2().getValue();
				setAMP2Parameters(amp2Seq,callData);
			}
			
		} catch (InvalidInputException e) {
			logger.error(dialogueId + ":: Error in parseInfoCollected " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::ASN Parsing Failure: Info collected parsing failure occured.:", e,
					MESSAGE.INFO_COLLECT);
		}

		catch (ASNParsingException e) {
			logger.error(dialogueId + "::[PH] Error in parseInfoCollected " + e.getMessage());
			throw e;
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseInfoCollected " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: parseInfoCollectedparsing failure occured.", e,
					MESSAGE.INFO_COLLECT);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] parseInfoCollected successfully");
		}
	}

	/**
	 * This method is used to encode the carrier, alternate carrier and second
	 * alternate carrier. It returns encoded byte array.
	 * 
	 * @param ci
	 * @return
	 * @throws InvalidInputException
	 */
	static byte[] getCarrierEncodedBuffer(CarrierInfo ci) throws InvalidInputException {
		CarrierFormat cf = new CarrierFormat();
		cf.setAddrSignal(ci.getAddress());
		cf.setCarrierFormatNatEnum(CarrierFormatNatEnum.fromInt(ci.getNatureOfCarrier()));
		cf.setCarrierFormatSelectionEnum(CarrierFormatSelectionEnum.fromInt(ci.getCarrierSelection()));

		return cf.encodeCarrierFormat();
	}

	/**
	 * This calss takes PhoneNumber as input and generate ASNDigits class.
	 * 
	 * @param ph
	 * @return
	 * @throws InvalidInputException
	 */
	static com.agnity.ain.asngenerated.AINDigits encodeAINDigits(PhoneNumber ph) throws InvalidInputException {

		CalledNatOfNumEnum natureOfAddrEnum = CalledNatOfNumEnum.fromInt(ph.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
		}

		NumPlanEnum numberPlan = NumPlanEnum.fromInt(ph.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		ClgPrsntRestIndEnum screeningStatus = ClgPrsntRestIndEnum.fromInt(ph.getPresentationIndicator());
		if(screeningStatus == null){
			screeningStatus = ClgPrsntRestIndEnum.PRESENT_ALLWD;
		}

		AinDigits ainDigits = new AinDigits();
		ainDigits.setAddrSignal(ph.getAddress());
		ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
		ainDigits.setNumPlanEnum(numberPlan);
		ainDigits.setClgPrsntRestIndEnum(screeningStatus);


		byte[] buffer = ainDigits.encodeAinDigits();

		AINDigits asnAinDigits = new AINDigits();
		asnAinDigits.setValue(buffer);

		return asnAinDigits;
	}

//	static byte[] encodeT1Digits(PhoneNumber ph) throws InvalidInputException {
//
//		NatureOfNumEnum noa = NatureOfNumEnum.fromInt(ph.getNatureOfAddress());
//
//		if (noa == null) {
//			noa = NatureOfNumEnum.NATIONAL_NPR;
//		}
//
//		NumPlanEnum numberPlan = NumPlanEnum.fromInt(ph.getNumberingPlan());
//		if (numberPlan == null) {
//			numberPlan = NumPlanEnum.ISDN_NP;
//		}
//
//		T1Digits ainDigits = new T1Digits();
//		ainDigits.setAddrSignal(ph.getAddress());
//		ainDigits.setNoa(NatureOfNumEnum.fromInt(ph.getNatureOfAddress()));
//		ainDigits.setNumPlanEnum(numberPlan);
//		ainDigits.setEncodingSchemeEnum(EncodingSchemeEnum.BCD_EVEN);
//		ainDigits.setNumOfDigits(ph.getAddress().length());
//		return  ainDigits.encodeDigits();
//
//	}

//	/**
//	 * This method encodes digits as per T.660 and T1.114 standard. 
//	 * @param typeOfDig
//	 * @param addressSig
//	 * @param noa
//	 * @param np
//	 * @param presRestricted
//	 * @return
//	 * @throws InvalidInputException
//	 */
//	static byte[] encodeT1RoutingDigits(TypeOfDigitEnum typeOfDig, String addressSig, 
//			NatureOfNumEnum noa, NumPlanEnum np, int presRestricted) throws InvalidInputException {
//
//		T1Digits ainDigits = new T1Digits();
//		ainDigits.setTypeOfDigit(typeOfDig);
//		ainDigits.setAddrSignal(addressSig);
//		ainDigits.setNoa(noa);
//		ainDigits.setNumPlanEnum(np);
//		ainDigits.setPresentationRestricted(0);
//		ainDigits.setEncodingSchemeEnum(EncodingSchemeEnum.BCD_ODD); // has to be 1 so setting BCD_ODD
//		ainDigits.setNumOfDigits(addressSig.length());
//		return  ainDigits.encodeDigits();
//
//	}

	/**
	 * This method is called by protocol handler to parse the termination Attempt
	 * received and populate the callData object from the received parameters.
	 * 
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param callData       represents an instance of CallData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseTerminationAttempt(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {
		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseTerminationAttempt");
		}

		TerminationAttemptArg termAttemptArg;
		try {
			termAttemptArg = (TerminationAttemptArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Info analyze " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] IA parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in Info analyze " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse IA", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, Info analyze parsing failure occured.", e,
					MESSAGE.INFO_ANALYZE);
		}

		try {

			// Bearer Capability 
			BearerCapabilityEnumType bearerEnumType = termAttemptArg.getBearerCapability().getValue();
			String bearerValue = bearerEnumType.getValue().name();

			legData.set(LegDataAttributes.P_BEAR_CAP, bearerValue);

			if(logger.isDebugEnabled()){
				logger.debug("TerminationAttempt: BearerCapability:" + bearerValue);
			}

			/*
			 * terminationAttempt.callingPartyNumber
			 */
			if (termAttemptArg.isCallingPartyIDPresent()) {
				AinDigits callingPartyNumdigit = new AinDigits();
				AinDigits callingPartyNum = callingPartyNumdigit
						.decodeAinDigits(termAttemptArg.getCallingPartyID().getValue().getValue(), Constant.CALLING);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Calling party number from Termination Attempt is "
							+ callingPartyNum);
				}
				if (callingPartyNum == null || callingPartyNum.getAddrSignal() == null
						|| "".equals(callingPartyNum.getAddrSignal().trim())) {
					/*
					 * call should be handled as ASn parse failure as address signal is missing
					 */
					logger.error(dialogueId + ":: [PH] Calling party num address signal missing");
					throw new ASNParsingException(dialogueId + ":: [PH] Calling party num address signal missing",
							MESSAGE.TERM_ATTEMPT);
				}
				PhoneNumber callingNumber = parseCallingPartyNum(dialogueId, callingPartyNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parseCallingPartyNum, CallingNumber:" + callingNumber);
				}
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
			} else {
				if (logger.isDebugEnabled()) {
					logger.debug("CallingPartyNumber not present in TerminationAttempt");
				}
			}

			/*
			 * termAttemptArg.calledPartyNumber
			 */
			if (termAttemptArg.isCalledPartyIDPresent()) {

				AinDigits calliedPartyNumdigit = new AinDigits();
				AinDigits calledPartyNum = calliedPartyNumdigit
						.decodeAinDigits(termAttemptArg.getCalledPartyID().getValue().getValue(), Constant.CALLED);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted called number from Termination Attempt is "
							+ calledPartyNum);
				}

				PhoneNumber calledNumber = parseCalledPartyNum(dialogueId, calledPartyNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::[PH] Exit parseCalledPartyNum, calledNumber:" + calledNumber);
				}
				legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);

				if(calledPartyNum.getCalledNatOfNumEnum() != null){
					legData.set(LegDataAttributes.P_CALLED_PARTY_NOA, calledPartyNum.getCalledNatOfNumEnum().getCode());
				}
			}

			/*
			 * infoAnalyze.chargeNumber
			 */
			if (termAttemptArg.isChargeNumberPresent()) {
				ChargeNumber chargeNumber = termAttemptArg.getChargeNumber();

				ChargeNum chargeNum = new ChargeNum();
				chargeNum.decodeChargeNum(termAttemptArg.getChargeNumber().getValue().getValue());
				if (logger.isDebugEnabled()) {
					logger.debug(
							dialogueId + ":: [PH] Extracted Charge number from Termination Attempt is " + chargeNum);
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + "::[PH] Exit parseCharging number");
				}

				PhoneNumber chargePN = new PhoneNumber();
				chargePN.setAddress(chargeNum.getAddrSignal());
				chargePN.setNumberingPlan(chargeNum.getNumPlanEnum().getCode());

				legData.set(LegDataAttributes.P_CHARGE_NUMBER, chargePN);
			}

			/*
			 * TerminationAttempt . charge party station type
			 */
			if (termAttemptArg.isChargePartyStationTypePresent()) {
				ChargePartyStationType chrgStation = termAttemptArg.getChargePartyStationType();

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted ChargePartyStationType from TerminationAttempt: "
							+ chrgStation.getValue());
				}

				legData.set(LegDataAttributes.P_CHARGE_PARTY_STATION_TYPE, chrgStation.getValue());
			}

			/*
			 * termAttempt - trigger criteria
			 */
			if (termAttemptArg.isTriggerCriteriaTypePresent()) {
				TriggerCriteriaTypeEnumType triggerCriteriaType = termAttemptArg.getTriggerCriteriaType().getValue();
				legData.set(LegDataAttributes.P_TRIGGER_CRITERIA, triggerCriteriaType.getValue().ordinal());
			}

			/*
			 * LATA
			 */
			if (termAttemptArg.isLataPresent()) {
				Lata lata = termAttemptArg.getLata();

				AinDigits ainDigits = new AinDigits();
				ainDigits.decodeAinDigits(lata.getValue().getValue(), Constant.CALLED);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted LATA from Termination Attempt "
							+ ainDigits.getAddrSignal());
				}

				legData.set(LegDataAttributes.P_LATA, ainDigits.getAddrSignal());
			}

			// Generic Name 
			if(termAttemptArg.isGenericNamePresent()){
				byte[] gnBuf = termAttemptArg.getGenericName().getValue();
				AinScfProtocolFieldCodec.decodeGenericName(dialogueId, gnBuf, "Termiantion Attempt", legData);
			}
			
			//AMP1
			if(termAttemptArg.isAmp1Present()){
				byte[] amp1Buffer = termAttemptArg.getAmp1().getValue();
				setAMP1Parameters(amp1Buffer, callData);
			}
			
			//AMP2
			if(termAttemptArg.isAmp2Present()){
				Amp2SequenceType amp2Seq = termAttemptArg.getAmp2().getValue();
				setAMP2Parameters(amp2Seq,callData);
			}

		} catch (InvalidInputException e) {
			logger.error(dialogueId + ":: Error in parseTerminationAttempt " + e.getMessage());
			throw new ASNParsingException(dialogueId + "::ASN Parsing Failure:Term ttempt parsing failure occured.:", e,
					MESSAGE.INFO_ANALYZE);
		}

		catch (ASNParsingException e) {
			logger.error(dialogueId + "::[PH] Error in parseTerminationAttempt " + e.getMessage());
			throw e;
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseTerminationAttempt " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: Info analyze parsing failure occured.", e,
					MESSAGE.TERM_ATTEMPT);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Term Attempt parsed successfully");
		}
	}

	/**
	 * This method is used to create Authorize Termination
	 * 
	 * @param callData Call Data object
	 * @param action
	 * @return
	 * @throws Exception
	 */
	public static byte[] createAuthorizeTermination(CallData callData, Action action) 
			throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Inside createAuthorizeTermination");
		}

		AuthorizeTerminationArg authTermArg = new AuthorizeTerminationArg();

		// Encode AMAAlternateBillingIndicator
		if (leg2Data.get(LegDataAttributes.P_AMA_ALT_BILLING_NUMBER) != null) {
			PhoneNumber altLineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMA_ALT_BILLING_NUMBER);

			AINDigits asnAinDigits = encodeAINDigits(altLineNum);

			AMAAlternateBillingNumber asnAmaAltBillNum = new AMAAlternateBillingNumber();
			asnAmaAltBillNum.setValue(asnAinDigits);

			authTermArg.setAMAAlternateBillingNumber(asnAmaAltBillNum);
		}

		// AMA Business Customer ID
		if (leg2Data.get(LegDataAttributes.P_AMA_BUSINESS_CUSTOMER_ID) != null) {
			PhoneNumber amaBusCustId = (PhoneNumber) leg2Data.get
					(LegDataAttributes.P_AMA_BUSINESS_CUSTOMER_ID);

			AinDigits sd = new AinDigits();
			sd.setAddrSignal(amaBusCustId.getAddress());
			sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
			sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

			AINDigits amaAinDigits = new AINDigits(sd.encodeAinDigits());

			AMABusinessCustomerID amaCustIdArg = new AMABusinessCustomerID();
			amaCustIdArg.setValue(amaAinDigits);

			authTermArg.setAMABusinessCustomerID(amaCustIdArg);
		}

		// Encode AMASLPId. It should be of 9 digits
		if (leg2Data.get(LegDataAttributes.P_AMASLPID) != null) {
			String digits = (String) leg2Data.get(LegDataAttributes.P_AMASLPID);

			if (StringUtils.isNotBlank(digits) && StringUtils.length(digits) != 9) {
				logger.error("Not encoding AMASlpID in AuthorizationTermination as length is not equal to 9, [" + digits + "]");
			} else {
				byte[] slpId = AddressSignal.encodeAdrsSignal(digits);

				AMAslpID asnSlpId = new AMAslpID();
				asnSlpId.setValue(slpId);

				authTermArg.setAMAslpID(asnSlpId);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting AAMASlpID as:[" + digits + "], byte: " + CommonUtils.formatBytes(slpId));
				}
			}
		}

		// Encode AMALineNumber. There could be upto 2 AMALineNumber as per standard.
		List<AMALineNumber> asnAMALineNumCollection = null;
		for (int i = 0; i < 2; ++i) {
			PhoneNumber lineNum = null;
			if (i == 0 && (leg2Data.get(LegDataAttributes.P_AMALINENUMBER) != null)) {
				lineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMALINENUMBER);

				if (logger.isDebugEnabled()) {
					logger.debug("AMALineNumber1 - Encoding value:" + lineNum);
				}
			} else if (i == 1 && (leg2Data.get(LegDataAttributes.P_AMALINENUMBER2) != null)) {
				lineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMALINENUMBER2);

				if (logger.isDebugEnabled()) {
					logger.debug("AMALineNumber2 - Encoding value:" + lineNum);
				}
			}

			if (lineNum != null) {
				// In case of AMALineNumber the Nature of Number and number Plan is not
				// applicable
				lineNum.setNatureOfAddress(0);
				lineNum.setNumberingPlan(0);
				AINDigits asnAinDigits = encodeAINDigits(lineNum);

				AMALineNumber asnAmaLineNum = new AMALineNumber();
				asnAmaLineNum.setValue(asnAinDigits);

				if (asnAMALineNumCollection == null)
					asnAMALineNumCollection = new ArrayList<AMALineNumber>();

				asnAMALineNumCollection.add(asnAmaLineNum);
			}
		}
		if (asnAMALineNumCollection != null) {
			authTermArg.setAMALineNumber(asnAMALineNumCollection);

			if (logger.isDebugEnabled()) {
				logger.debug(" Encoded AMALineNumber to AuthorizeTermination");
			}
		}

		// AMADialedDigitsWC contains that customer dialed along wiht context ID to name
		// the digit
		// string like Access Code, Auth code, Customer Dialed Account Recording.
		// There could be upto 5 AMADialedDigitWC in AnalyzeRoute
		List<AMADigitsDialedWC> dialedDigitWCList = null;
		for (int i = 0; i < 5; ++i) {
			PhoneNumber collectedDigits = null;

			switch (i) {
			case 0:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC1) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC1);
				}
				break;
			case 1:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC2) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC2);
				}
				break;
			case 2:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC3) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC3);
				}
				break;
			case 3:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC4) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC4);
				}
				break;
			case 4:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC5) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC5);
				}
				break;
			}

			if (collectedDigits != null && StringUtils.isNotBlank(collectedDigits.getAddress())) {
				collectedDigits.setNatureOfAddress(0);
				collectedDigits.setNumberingPlan(0);

				AINDigits asnAinDigits = encodeAINDigits(collectedDigits);
				AMADigitsDialedWC amaDigitWc = new AMADigitsDialedWC();
				amaDigitWc.setValue(asnAinDigits);

				if (dialedDigitWCList == null)
					dialedDigitWCList = new ArrayList<AMADigitsDialedWC>();

				dialedDigitWCList.add(amaDigitWc);
			}
		}
		if (dialedDigitWCList != null) {
			authTermArg.setAMADigitsDialedWC(dialedDigitWCList);
		}

		// Display Text 
		DisplayText dt = new DisplayText();
		List<DisplayInformation> displayInfoList = null;
		DisplayInformation disInfo = null;
		String displayTextLegData = null;
		byte [] displayValByte = null;
		int count=0;

		// Display Text Calling Party Name 
		displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_NAME);
		if(StringUtils.isNotBlank(displayTextLegData)){
			displayInfoList = new ArrayList<DisplayInformation>();
			disInfo = new DisplayInformation();
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_NAME);
			displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

			disInfo.selectCallingPartyName(displayValByte);
			displayInfoList.add(disInfo);

			if(logger.isDebugEnabled()){
				logger.debug("AuthorizeTermination: DisplayText Calling Name:"+ displayTextLegData + " byte:"
						+ CommonUtils.formatBytes(displayValByte));
			}
			count++;
		}

		// Display Text Calling Party Address
		displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_ADDR);
		if(StringUtils.isNotBlank(displayTextLegData)){
			if(count == 0){
				displayInfoList = new ArrayList<DisplayInformation>();
			}
			disInfo = new DisplayInformation();
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLG_ADDR);
			displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

			disInfo.selectCallingAddress(displayValByte);
			displayInfoList.add(disInfo);

			if(logger.isDebugEnabled()){
				logger.debug("AuthorizeTermination: DisplayText Calling Address:"+ displayTextLegData + " byte:"
						+ CommonUtils.formatBytes(displayValByte));
			}
			count++;
		}

		// Display Text Called Party Name
		displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_NAME);
		if(StringUtils.isNotBlank(displayTextLegData)){
			if(count == 0){
				displayInfoList = new ArrayList<DisplayInformation>();
			}
			disInfo = new DisplayInformation();
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_NAME);
			displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

			disInfo.selectCalledPartyName(displayValByte);
			displayInfoList.add(disInfo);

			if(logger.isDebugEnabled()){
				logger.debug("AuthorizeTermination: DisplayText Called Name:"+ displayTextLegData + " byte:"
						+ CommonUtils.formatBytes(displayValByte));
			}
			count++;
		}

		// Display Text Called Party Address 
		displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_ADDR);
		if(StringUtils.isNotBlank(displayTextLegData)){
			if(count == 0){
				displayInfoList = new ArrayList<DisplayInformation>();
			}
			disInfo = new DisplayInformation();
			displayTextLegData = (String) leg2Data.get(LegDataAttributes.P_DISPLAY_TEXT_CLD_ADDR);
			displayValByte = AinScfProtocolFieldCodec.encodeIa5(displayTextLegData);

			disInfo.selectCalledAddress(displayValByte);
			displayInfoList.add(disInfo);

			if(logger.isDebugEnabled()){
				logger.debug("AuthorizeTermination: DisplayText Called Address:"+ displayTextLegData + " byte:" 
						+ CommonUtils.formatBytes(displayValByte));
			}
			count++;
		}

		if(count > 0){
			dt.setValue(displayInfoList);
			authTermArg.setDisplayText(dt);

			if(logger.isDebugEnabled()){
				logger.debug("AuthorizeTermination setting DisplayText");
			}
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.AUTHORIZED_TERMINATION);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(authTermArg);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Exiting createAuthorizeTermination");
		}

		return encodeList.getFirst();
	}

	/**
	 * Method to create Continue without argument. ======= Method to
	 * create Continue wihtout argument.
	 * 
	 * @param callData
	 * @param action
	 * @return
	 * @throws Exception
	 */
	public static byte[] createContinue(CallData callData, Action action) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Inside createContinue");
		}

		ContinueArg continuearg = new ContinueArg();

		// AMA Business Customer ID
		if (leg2Data.get(LegDataAttributes.P_AMA_BUSINESS_CUSTOMER_ID) != null) {
			PhoneNumber amaBusCustId = (PhoneNumber) leg2Data.get
					(LegDataAttributes.P_AMA_BUSINESS_CUSTOMER_ID);

			AinDigits sd = new AinDigits();
			sd.setAddrSignal(amaBusCustId.getAddress());
			sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
			sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

			AINDigits amaAinDigits = new AINDigits(sd.encodeAinDigits());

			AMABusinessCustomerID amaCustIdArg = new AMABusinessCustomerID();
			amaCustIdArg.setValue(amaAinDigits);

			continuearg.setAMABusinessCustomerID(amaCustIdArg);
		}


		// AMADialedDigitsWC contains that customer dialed along wiht context ID to name
		// the digit
		// string like Access Code, Auth code, Customer Dialed Account Recording.
		// There could be upto 5 AMADialedDigitWC in AnalyzeRoute
		List dialedDigitWCList = null;
		for (int i = 0; i < 5; ++i) {
			PhoneNumber collectedDigits = null;

			switch (i) {
			case 0:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC1) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC1);
				}
				break;
			case 1:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC2) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC2);
				}
				break;
			case 2:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC3) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC3);
				}
				break;
			case 3:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC4) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC4);
				}
				break;
			case 4:
				if (leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC5) != null) {
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC5);
				}
				break;
			}

			if (collectedDigits != null && StringUtils.isNotBlank(collectedDigits.getAddress())) {
				collectedDigits.setNatureOfAddress(0);
				collectedDigits.setNumberingPlan(0);

				AINDigits asnAinDigits = encodeAINDigits(collectedDigits);
				AMADigitsDialedWC amaDigitWc = new AMADigitsDialedWC();
				amaDigitWc.setValue(asnAinDigits);

				if (dialedDigitWCList == null)
					dialedDigitWCList = new ArrayList<AMADigitsDialedWC>();

				dialedDigitWCList.add(amaDigitWc);
			}
		}
		if (dialedDigitWCList != null) {
			continuearg.setAMADigitsDialedWC(dialedDigitWCList);
		}

		// Encode AMAAlternateBillingIndicator
		if (leg2Data.get(LegDataAttributes.P_AMA_ALT_BILLING_NUMBER) != null) {
			PhoneNumber altLineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMA_ALT_BILLING_NUMBER);

			AINDigits asnAinDigits = encodeAINDigits(altLineNum);

			AMAAlternateBillingNumber asnAmaAltBillNum = new AMAAlternateBillingNumber();
			asnAmaAltBillNum.setValue(asnAinDigits);

			continuearg.setAMAAlternateBillingNumber(asnAmaAltBillNum);
		}


		// Encode AMALineNumber. There could be upto 2 AMALineNumber as per standard.
		List<AMALineNumber> asnAMALineNumCollection = null;
		for (int i = 0; i < 2; ++i) {
			PhoneNumber lineNum = null;
			if (i == 0 && (leg2Data.get(LegDataAttributes.P_AMALINENUMBER) != null)) {
				lineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMALINENUMBER);

				if (logger.isDebugEnabled()) {
					logger.debug("AMALineNumber1 - Encoding value:" + lineNum);
				}
			} else if (i == 1 && (leg2Data.get(LegDataAttributes.P_AMALINENUMBER2) != null)) {
				lineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMALINENUMBER2);

				if (logger.isDebugEnabled()) {
					logger.debug("AMALineNumber2 - Encoding value:" + lineNum);
				}
			}

			if (lineNum != null) {
				// In case of AMALineNumber the Nature of Number and number Plan is not
				// applicable
				lineNum.setNatureOfAddress(0);
				lineNum.setNumberingPlan(0);
				AINDigits asnAinDigits = encodeAINDigits(lineNum);

				AMALineNumber asnAmaLineNum = new AMALineNumber();
				asnAmaLineNum.setValue(asnAinDigits);

				if (asnAMALineNumCollection == null)
					asnAMALineNumCollection = new ArrayList<AMALineNumber>();

				asnAMALineNumCollection.add(asnAmaLineNum);
			}
		}
		if (asnAMALineNumCollection != null) {
			continuearg.setAMALineNumber(asnAMALineNumCollection);

			if (logger.isDebugEnabled()) {
				logger.debug(" Encoded AMALineNumber to Continue");
			}
		}

		// Encode AMASLPId. It should be of 9 digits
		if (leg2Data.get(LegDataAttributes.P_AMASLPID) != null) {
			String digits = (String) leg2Data.get(LegDataAttributes.P_AMASLPID);

			if (StringUtils.isNotBlank(digits) && StringUtils.length(digits) != 9) {
				logger.error("Not encoding AMASlpID in Continue as length is not equal to 9, [" + digits + "]");
			} else {
				byte[] slpId = AddressSignal.encodeAdrsSignal(digits);

				AMAslpID asnSlpId = new AMAslpID();
				asnSlpId.setValue(slpId);

				continuearg.setAMAslpID(asnSlpId);

				if (logger.isDebugEnabled()) {
					logger.debug("Setting AAMASlpID as:[" + digits + "], byte: " + CommonUtils.formatBytes(slpId));
				}
			}
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.CONTINUE);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(continuearg);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Exiting createContinue");
		}

		return encodeList.getFirst();
	}

	/**
	 * Method to create SendNotification component
	 * 
	 * @param callData
	 * @param action
	 * @return
	 */
	public static byte[] createSendNotification(CallData callData, Action action) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Inside createSendNotification");
		}

		SendNotificationArg sendNotifArg = new SendNotificationArg();

		// Fetch Echo Data sent by an applicaiton. It is assumed that echo data 
		// shall be encoded in following format 
		// AppId (1 Octet) + Running Number (5 Octets) 
		// AppId shall be used for triggerign an application once TerminationNotification is rxed
		// Since it is possible that TA may be ahndled by any other CAS which 
		// shall be correlated through this echo data. 
		// In case application did not set Echo data then dialogue ID is used as echo data 
		// an error will be printed and encode dialogue ID
		// Since Echo Data is of 6 Octets, Right padding it with 0x00
		byte[] combined = null;
		String echoDataFromLeg = null;
		if(leg2Data.get(LegDataAttributes.P_ECHO_DATA) != null){
			echoDataFromLeg = (String) leg2Data.get(LegDataAttributes.P_ECHO_DATA);

			if(StringUtils.isNotBlank(echoDataFromLeg)){
				combined = CommonUtils.convertHexStringToByteArray(echoDataFromLeg);

				if (logger.isDebugEnabled()) {
					logger.debug(":: Dialogue ID: " + dialogueId + 
							", added as Echo Data:" + CommonUtils.formatBytes(combined) +
							", StringValue:" + echoDataFromLeg);
				}
			}else{
				logger.error(dialogueId + "P_ECHO_DATA is null");
			}
		}else{
			logger.error(dialogueId + ": Application Id:" + callData.get(CallDataAttribute.SERVICE_ID) +
					" did not pass EchoData, using dialogue ID");

			byte[] dlg = CommonUtils.formatIntToByte(dialogueId);

			byte[] allByteArray = new byte[dlg.length + (6 - dlg.length)];
			byte[] pad = new byte[(6 - dlg.length)];
			ByteBuffer buff = ByteBuffer.wrap(allByteArray);
			buff.put(pad);
			buff.put(dlg);

			combined = buff.array();

			if (logger.isDebugEnabled()) {
				logger.debug(":: Dialogue ID: " + dialogueId + ", added as Echo Data:" + CommonUtils.formatBytes(combined));
			}
		}

		// add buffer
		EchoData data = new EchoData();
		data.setValue(combined);

		sendNotifArg.setEchoData(data);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(sendNotifArg);
		opCode.add(AinOpCodes.SEND_NOTIFICATION);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Exiting createSendNotification");
		}

		return encodeList.getFirst();
	}

	/**
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static CallData parseTerminationNotification(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		String serviceId = null;
		if (callData.get(CallDataAttribute.SERVICE_ID) != null) {
			serviceId = (String) callData.get(CallDataAttribute.SERVICE_ID);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside ParseTerminationNotification, serviceId:" + serviceId);
		}

		TerminationNotificationArg termAttemptArg;

		try {
			termAttemptArg = (TerminationNotificationArg) AinOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding TerminationNotification" + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] IA parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in Termination Notification " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse IA", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, Termination Notification parsing failure occured.", e,
					MESSAGE.TERMINATION_NOTIFICATION);
		}

		try {
			// Echo Data
			EchoData echoData = termAttemptArg.getEchoData();
			byte[] echoDataByte = echoData.getValue();
			String echoDataString = CommonUtils.convertByteArrayToString(echoDataByte);

			if(logger.isDebugEnabled()){
				logger.debug(dialogueId + ": TerminationAttempt rxed with echo Data: "+ 
						echoDataString + " for appId: " + echoDataByte[0] + " ServiceId:" + serviceId);
			}

			callData.set(CallDataAttribute.P_AIN_ECHO_DATA, echoDataString);

			// Termination Notification is UNI message and may be received by ANY CAS/INC
			// Therefore we need to send to application as new event which application 
			// need to handle by looking for data corresponding to echo data.  

			// set to clean up call through prearranged null i.e., call shall be
			// cleaned by stack and not to send any message to network
			callData.set(CallDataAttribute.P_SEND_PREARRANGED_END, "1");

			// Terminator Indicator
			TerminationIndicator termInd = termAttemptArg.getTerminationIndicator();
			byte[] termIndByteValue = termInd.getValue();

			int terminationIndicator = (termIndByteValue[0] & 0x0F);

			// possible value
			// 1 bit - NM Ctrl List Overflow Indicator
			// 2 bit - Answer Indication
			// 3 bit - Cause Indication
			// 4 bit - Unrelated error
			switch (terminationIndicator) {
			case 1: {
				if (logger.isDebugEnabled()) {
					logger.debug(
							"AIN TerminationIndicator received: NM Ctrl List Overflow Ind" + terminationIndicator);
				}
				callData.set(CallDataAttribute.P_AIN_TERMINATION_IND, "1");
				break;
			}
			case 2: {
				if (logger.isDebugEnabled()) {
					logger.debug("AIN TerminationIndicator received: Answer Indication" + terminationIndicator);
				}
				callData.set(CallDataAttribute.P_AIN_TERMINATION_IND, "2");

				if (termAttemptArg.isConnectTimePresent()) {
					byte[] connectTimeByte = termAttemptArg.getConnectTime().getValue();

					// length MUST be 5 octet
					// |4bits |4 bits|8bits|8bits|8bits|4 bits |4bits|
					// |-filler-|---minutes--------|-sec-|-tenth of sec -|spare|

					int minutes = ((connectTimeByte[0] >> 4) | connectTimeByte[1] | connectTimeByte[2]);
					int seconds = connectTimeByte[3];
					int tenthOfSec = (connectTimeByte[4] & 0x0F);

					String conTime = minutes + ":" + seconds + ":" + tenthOfSec;

					callData.set(CallDataAttribute.P_AIN_CONNECT_TIME, conTime);

					if (logger.isDebugEnabled()) {
						logger.debug("AIN Connect Time in Termination Notification [min:sec:tenthofSec]" + minutes
								+ ":" + seconds + ":" + tenthOfSec);
					}
				}
				break;
			}
			case 4: {
				if (logger.isDebugEnabled()) {
					logger.debug("AIN TerminationIndicator received: Cause Indication" + terminationIndicator);
				}
				callData.set(CallDataAttribute.P_AIN_TERMINATION_IND, "4");
				break;
			}
			case 8: {
				if (logger.isDebugEnabled()) {
					logger.debug("AIN TerminationIndicator received: Unrrelated Error" + terminationIndicator);
				}
				callData.set(CallDataAttribute.P_AIN_TERMINATION_IND, "8");
				break;
			}
			default:
				logger.error("Unknown AIN TerminationIndicator received:" + terminationIndicator);
			}

			// Connect time
			if(termAttemptArg.isConnectTimePresent()){
				byte[] conTimeByte = termAttemptArg.getConnectTime().getValue();
				String connectTime = AddressSignal.decodeAdrsSignal(conTimeByte, 0, 1);
				// First Digit is filler. 
				connectTime = connectTime.substring(1);

				// First 5 digits are minutes
				String min = connectTime.substring(0, 5);

				// Next 2 digits are seconds 
				String sec = connectTime.substring(5, 7);

				// Last digit is thenth of seconds. 
				String tenthOfSec = connectTime.substring(7,8);

				if(logger.isDebugEnabled()){
					logger.debug("TerminationNotifcation:ConnectTime:" + connectTime +
							" Min:" + min + ":Sec: "+ sec + ":tenthofSec:" + tenthOfSec);
				}

				callData.set(CallDataAttribute.P_AIN_TN_CONNECT_TIME_MIN, min);
				callData.set(CallDataAttribute.P_AIN_TN_CONNECT_TIME_SEC, sec);
				callData.set(CallDataAttribute.P_AIN_TN_CONNECT_TIME_TENTH_SEC, tenthOfSec);
			}

			if(termAttemptArg.isBusyCausePresent()){
				byte[] busyByte = termAttemptArg.getBusyCause().getValue();

				if(busyByte != null && busyByte.length >= 2){
					int value = busyByte[1]&0x7f;

					callData.set(CallDataAttribute.P_AIN_TN_BUSY_CAUSE, Integer.toString(value));

					if(logger.isDebugEnabled()){
						logger.debug("TerminationNotifcation: BusyCause:" + value);
					}
				}
			}

		} catch (Exception ex) {
			logger.error("Exception in parsing Termination Notification");
		}

		return callData;
	}

	/**
	 * Method is used creating STR to send Play Information in case of DIRECT_MS
	 * Mode
	 * 
	 * @param callData
	 * @param action 
	 * @return
	 * @throws Exception
	 */
	public static byte[] createSTRForPlay(CallData callData, Action action) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] Inside createSTRForPlay");
		}

		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) leg1.get(LegDataAttributes.NP_ANN_SPEC);

		AnnouncementBlock annblk = AinScfProtocolFieldCodec.encodeAnnouncementBlock(annSpec, dialogId);

		StrParameterBlockChoiceType strParamChoiceType = new StrParameterBlockChoiceType();
		strParamChoiceType.selectAnnouncementBlock(annblk);

		StrParameterBlock strParam = new StrParameterBlock();
		strParam.setValue(strParamChoiceType);

		SendToResourceArg sendToResourceArg = new SendToResourceArg();

		// Set Destination Address
		if (leg1.get(LegDataAttributes.P_STR_PRI_NUMBER) != null) {
			String strPriNum = (String) leg1.get(LegDataAttributes.P_STR_PRI_NUMBER);

			AinDigits sd = new AinDigits();
			sd.setAddrSignal(strPriNum);
			sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
			sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

			AINDigits strWithPriDigits = new AINDigits(sd.encodeAinDigits());

			DestinationAddress destAddr = new DestinationAddress();
			destAddr.setValue(strWithPriDigits);
			sendToResourceArg.setDestinationAddress(destAddr);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Setting Destination Address as " + strPriNum + " in createSTRForPlay");
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Not Setting Destination Address as in createSTRForPlay");
			}
		}

		// Resource Type as 0 for Play Announcement
		ResourceType rstype = new ResourceType();
		rstype.setValue(0);
		sendToResourceArg.setResourceType(rstype);

		sendToResourceArg.setStrParameterBlock(strParam);

		// Set Disconnect Flag id send mode is END
		if(action.getSendMode() == Action.SEND_MODE.END){
			DisconnectFlag discFlag = new DisconnectFlag();
			sendToResourceArg.setDisconnectFlag(discFlag);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(sendToResourceArg);
		opCode.add(AinOpCodes.SEND_TO_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: STR created with Ann Block");
		}
		return encodeList.getFirst();
	}

	/**
	 * method used to encode STR with announcement for digit collection in case
	 * DIRECT_MS mode
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createSTRForPNC(CallData callData, boolean isEndReq) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] Inside createSTRForPNC");
		}

		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) leg1.get(LegDataAttributes.NP_ANN_SPEC);

		SendToResourceArg sendToResourceArg = new SendToResourceArg();

		// Set Destination Address
		if (leg1.get(LegDataAttributes.P_STR_PRI_NUMBER) != null) {
			String strPriNum = (String) leg1.get(LegDataAttributes.P_STR_PRI_NUMBER);

			AinDigits sd = new AinDigits();
			sd.setAddrSignal(strPriNum);
			sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
			sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

			AINDigits strWithPriDigits = new AINDigits(sd.encodeAinDigits());

			DestinationAddress destAddr = new DestinationAddress();
			destAddr.setValue(strWithPriDigits);
			sendToResourceArg.setDestinationAddress(destAddr);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Setting Destination Address as " + strPriNum + " in createSTRForPNC");
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Not Setting Destination Address as in createSTRForPNC");
			}
		}

		// Set Answer Indicator
		if(!isEndReq){
			AnswerIndicator answerIndicator = new AnswerIndicator();
			answerIndicator.initWithDefaults();
			sendToResourceArg.setAnswerIndicator(answerIndicator);
		}

		// Resource Type as 1 - PlayCollect
		ResourceType rstype = new ResourceType();
		rstype.setValue(1);
		sendToResourceArg.setResourceType(rstype);

		AnnouncementDigitBlock annDigiblk = AinScfProtocolFieldCodec.encodeAnnouncementDigitBlock(annSpec, dialogId);
		StrParameterBlockChoiceType strParamChoiceType = new StrParameterBlockChoiceType();
		strParamChoiceType.selectAnnouncementDigitBlock(annDigiblk);

		StrParameterBlock strParam = new StrParameterBlock();
		strParam.setValue(strParamChoiceType);

		// set StrParameterBlock in sendToIPResource
		sendToResourceArg.setStrParameterBlock(strParam);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(sendToResourceArg);
		opCode.add(AinOpCodes.SEND_TO_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Exiting createSTRForPNC");
		}
		return encodeList.getFirst();
	}

	/**
	 * method used to encode STR for Flex-Play in case DIRECT_MS mode
	 * 
	 * @param callData
	 * @param action 
	 * @return
	 * @throws Exception
	 */
	public static byte[] createSTRForFlexPlay(CallData callData, Action action) throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] createSTRForFlexPlay:enter");
		}
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) leg1.get(LegDataAttributes.NP_ANN_SPEC);

		AnnouncementBlock annBlk = AinScfProtocolFieldCodec.encodeAnnouncementBlock(annSpec, dialogId);
		IPStrParameterBlock ipStrParamBlk = new IPStrParameterBlock();
		ipStrParamBlk.selectAnnouncementBlock(annBlk);

		byte[] rea = { 0x2b, 0x11, 0x69, 0x03 };
		// encoding of ResourceEncodingAuthority
		ResourceEncodingAuthority resourceEncodingAuthority = new ResourceEncodingAuthority();
		resourceEncodingAuthority.setValue(rea);

		// encoding of IPResourceType
		IPResourceType iPResourceType = new IPResourceType();

		// IPResourceType = 0 ("Play Announcement")
		byte[] play = { 0x00 };
		iPResourceType.setValue(play);
		/**
		 * FlexParameterBlockContent::= [3] IMPLICIT SEQUENCE { [1] IMPLICIT
		 * ResourceEncodingAuthority, [2] IMPLICIT IPResourceType, [3]
		 * IPStrParameterBlock, [4] IMPLICIT FlashIndication OPTIONAL }
		 */
		FlexParameterBlockContentSequenceType flexParaBlkContSeqType = new FlexParameterBlockContentSequenceType();
		flexParaBlkContSeqType.setResourceEncodingAuthority(resourceEncodingAuthority);
		flexParaBlkContSeqType.setIPResourceType(iPResourceType);
		flexParaBlkContSeqType.setIPStrParameterBlock(ipStrParamBlk);

		FlexParameterBlockContent flexParaBlkCont = new FlexParameterBlockContent();
		flexParaBlkCont.setValue(flexParaBlkContSeqType);

		byte[] flexParaBlkContbyte = AinOperationsCoding.encodeFieldsValue(flexParaBlkCont,
				"FlexParameterBlockContent");
		if (logger.isDebugEnabled()) {
			logger.info("the length of flexParaBlkContbyte::" + flexParaBlkContbyte.length);
		}
		// FlexParameterBlock ::= OCTET STRING (SIZE(1..120))
		FlexParameterBlock flexparBlk = new FlexParameterBlock(flexParaBlkContbyte);
		StrParameterBlockChoiceType strParamChoiceType = new StrParameterBlockChoiceType();
		strParamChoiceType.selectFlexParameterBlock(flexparBlk);

		StrParameterBlock strParam = new StrParameterBlock();
		strParam.setValue(strParamChoiceType);

		SendToResourceArg sendToResourceArg = new SendToResourceArg();

		// Set Destination Address
		if (leg1.get(LegDataAttributes.P_STR_PRI_NUMBER) != null) {
			String strPriNum = (String) leg1.get(LegDataAttributes.P_STR_PRI_NUMBER);

			AinDigits sd = new AinDigits();
			sd.setAddrSignal(strPriNum);
			sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
			sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

			AINDigits strWithPriDigits = new AINDigits(sd.encodeAinDigits());

			DestinationAddress destAddr = new DestinationAddress();
			destAddr.setValue(strWithPriDigits);
			sendToResourceArg.setDestinationAddress(destAddr);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Setting Destination Address as " + strPriNum + " in createSTRForFlexPlay");
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Not Setting Destination Address as in createSTRForFlexPlay");
			}
		}

		// to do create a Flag for Answer Indicator.
		// set Answer Indicator
		if (logger.isDebugEnabled()) {
			logger.debug("Going to set Answer Indicator");
		}
		AnswerIndicator answerIndicator = new AnswerIndicator();
		answerIndicator.initWithDefaults();
		sendToResourceArg.setAnswerIndicator(answerIndicator);

		// Resource Type as 4 - Flex
		ResourceType rstype = new ResourceType();
		rstype.setValue(4);
		sendToResourceArg.setResourceType(rstype);
		sendToResourceArg.setStrParameterBlock(strParam);

		// Set Disconnect Flag id send mode is END
		if(action.getSendMode() == Action.SEND_MODE.END){
			DisconnectFlag discFlag = new DisconnectFlag();
			sendToResourceArg.setDisconnectFlag(discFlag);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(sendToResourceArg);
		opCode.add(AinOpCodes.SEND_TO_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] createSTRForFlexPlay:exit");
		}

		return encodeList.getFirst();

	}

	/**
	 * method used to encode STR for Flex-PlayCollect in case DIRECT_MS mode
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createSTRForFlexPNC(CallData callData) throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] createSTRForFlexPNC:enter");
		}
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) leg1.get(LegDataAttributes.NP_ANN_SPEC);

		// encoding of AnnouncementDigitBlock.
		AnnouncementDigitBlock annDigitBlk = AinScfProtocolFieldCodec.encodeAnnouncementDigitBlock(annSpec, dialogId);

		// encoding of IPStrParameterBlock.
		IPStrParameterBlock iPStrParamBlk = new IPStrParameterBlock();
		iPStrParamBlk.selectAnnouncementDigitBlock(annDigitBlk);
		byte[] rea = { 0x2b, 0x11, 0x69, 0x03 };
		// encoding of ResourceEncodingAuthority
		ResourceEncodingAuthority resourceEncodingAuthority = new ResourceEncodingAuthority();
		resourceEncodingAuthority.setValue(rea);

		// encoding of IPResourceType
		IPResourceType iPResourceType = new IPResourceType();

		// IPResourceType = 1 ("Play Announcement & Collect Digits")
		byte[] playAndCollect = { 0x01 };
		iPResourceType.setValue(playAndCollect);
		/**
		 * FlexParameterBlockContent::= [3] IMPLICIT SEQUENCE { [1] IMPLICIT
		 * ResourceEncodingAuthority, [2] IMPLICIT IPResourceType, [3]
		 * IPStrParameterBlock, [4] IMPLICIT FlashIndication OPTIONAL }
		 */
		FlexParameterBlockContentSequenceType flexParaBlkContSeqType = new FlexParameterBlockContentSequenceType();
		flexParaBlkContSeqType.setResourceEncodingAuthority(resourceEncodingAuthority);
		flexParaBlkContSeqType.setIPResourceType(iPResourceType);
		flexParaBlkContSeqType.setIPStrParameterBlock(iPStrParamBlk);

		FlexParameterBlockContent flexParaBlkCont = new FlexParameterBlockContent();
		flexParaBlkCont.setValue(flexParaBlkContSeqType);

		// encoding the FlexParameterBlockContent into byte[]
		byte[] flexParaBlkContByte = AinOperationsCoding.encodeFieldsValue(flexParaBlkCont,
				"FlexParameterBlockContent");

		// FlexParameterBlock ::= OCTET STRING (SIZE(1..120))
		if (logger.isDebugEnabled()) {
			logger.debug("length of FlexParameterBlockContent in byte" + flexParaBlkContByte);
		}
		FlexParameterBlock flexparBlk = new FlexParameterBlock(flexParaBlkContByte);

		StrParameterBlockChoiceType strParamChoiceType = new StrParameterBlockChoiceType();
		strParamChoiceType.selectFlexParameterBlock(flexparBlk);

		StrParameterBlock strParam = new StrParameterBlock();
		strParam.setValue(strParamChoiceType);

		SendToResourceArg sendToResourceArg = new SendToResourceArg();

		// Set Destination Address

		if (leg1.get(LegDataAttributes.P_STR_PRI_NUMBER) != null) {
			String strPriNum = (String) leg1.get(LegDataAttributes.P_STR_PRI_NUMBER);

			AinDigits sd = new AinDigits();
			sd.setAddrSignal(strPriNum);
			sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
			sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);

			AINDigits strWithPriDigits = new AINDigits(sd.encodeAinDigits());

			DestinationAddress destAddr = new DestinationAddress();
			destAddr.setValue(strWithPriDigits);
			sendToResourceArg.setDestinationAddress(destAddr);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Setting Destination Address as " + strPriNum + " in createSTRForFlexPNC");
			}
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: Not Setting Destination Address as in createSTRForFlexPNC");
			}
		}

		// to do create a Flag for Answer Indicator.
		// set Answer Indicator
		if (logger.isDebugEnabled()) {
			logger.debug("Going to set Answer Indicator");
		}
		AnswerIndicator answerIndicator = new AnswerIndicator();
		answerIndicator.initWithDefaults();
		sendToResourceArg.setAnswerIndicator(answerIndicator);

		// Resource Type as 4 - Flex
		ResourceType rstype = new ResourceType();
		rstype.setValue(4);
		sendToResourceArg.setResourceType(rstype);
		sendToResourceArg.setStrParameterBlock(strParam);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(sendToResourceArg);
		opCode.add(AinOpCodes.SEND_TO_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH]createSTRForFlexPNC:exit");
		}

		return encodeList.getFirst();

	}

	/**
	 * method use to parse the CALL_INFO_FROM_RESOURCE.
	 * 
	 * @param invokeIndEvent
	 * @param callData
	 * @throws ParameterOutOfRangeException
	 * @throws ASNParsingException
	 */
	public static void parseCallInfoFrmResource(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ParameterOutOfRangeException, ASNParsingException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AinCallStates currCallState = (AinCallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside parseCallInfoFrmResource in state:" + currCallState);
		}

		CallInfoFromResourceArg callInfoFrmResrcArg;
		try {
			callInfoFrmResrcArg = (CallInfoFromResourceArg) AinOperationsCoding.decodeOperation(invokeIndEvent);

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Inside parseCallInfoFrmResource get CallInfoFromResourceArg as:"
						+ callInfoFrmResrcArg);
			}
			byte[] ipRturnBlk = callInfoFrmResrcArg.getIPReturnBlock().getValue();

			// decoding IPReturnBlock into IPReturnBlockContent
			IPReturnBlockContent ipRtnBlkCont = (IPReturnBlockContent) AinOperationsCoding.decodeFieldsValue(ipRturnBlk,
					"IPReturnBlockContent");

			if (logger.isDebugEnabled()) {
				logger.debug(
						dialogueId + ":: Inside parseCallInfoFrmResource get IPReturnBlockContent as::" + ipRtnBlkCont);
			}
			ResultChoiceType resultChoiceType = ipRtnBlkCont.getValue().getResult();

			if (logger.isDebugEnabled()) {
				logger.debug(
						dialogueId + ":: Inside parseCallInfoFrmResource get ResultChoiceType as::" + resultChoiceType);
			}

			if (resultChoiceType != null) {

				if (resultChoiceType.isAnnouncementResultSelected()) {
					if (logger.isDebugEnabled()) {
						logger.debug(
								dialogueId + ":: Inside parseCallInfoFrmResource for ChoiceType::AnnouncementResult");
					}
					// Interruption Status 0-->voice and 1-->keypad
					InterruptionStatus interruptionStatus = ipRtnBlkCont.getValue().getResult().getAnnouncementResult()
							.getValue();

				} else if (resultChoiceType.isAnnouncementDigitResultSelected()) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ ":: Inside parseCallInfoFrmResource for ChoiceType::AnnouncementDigitResult");
					}
					CollectedDigits collectedDigit = ipRtnBlkCont.getValue().getResult().getAnnouncementDigitResult()
							.getIPCollectedDigits().getValue();
					String digitsCollected = AddressSignal.decodeAdrsSignal(collectedDigit.getValue().getValue(), 0, 0);

					legData.set(LegDataAttributes.P_COLLECTED_DIGITS, digitsCollected);
				} else if (resultChoiceType.isCollectedInformationResultSelected()) {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ ":: Inside parseCallInfoFrmResource for ChoiceType::CollectedInformationResult");
					}

					CollectedDigits collectedDigit = ipRtnBlkCont.getValue().getResult().getCollectedInformationResult()
							.getIPCollectedDigits().getValue();
					String digitsCollected = AddressSignal.decodeAdrsSignal(collectedDigit.getValue().getValue(), 0, 0);

					legData.set(LegDataAttributes.P_COLLECTED_DIGITS, digitsCollected);
				} else if (resultChoiceType.isTextSpeechDigitResultSelected()) {

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ ":: Inside parseCallInfoFrmResource for ChoiceType::isTextSpeechDigitResultSelected");
					}
					CollectedDigits collectedDigit = ipRtnBlkCont.getValue().getResult().getTextSpeechDigitResult()
							.getValue().getIPCollectedDigits().getValue();
					String digitsCollected = AddressSignal.decodeAdrsSignal(collectedDigit.getValue().getValue(), 0, 0);
					if (logger.isDebugEnabled()) {
						logger.debug("collectedDigit:: " + digitsCollected);
					}
					legData.set(LegDataAttributes.P_COLLECTED_DIGITS, digitsCollected);

				} else if (resultChoiceType.isTextSpeechResultSelected()) {

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId
								+ ":: Inside parseCallInfoFrmResource for ChoiceType::isTextSpeechResultSelected");
					}
					InterruptionStatus interruptionStatus = ipRtnBlkCont.getValue().getResult().getTextSpeechResult()
							.getValue().getValue();

				} else {
					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: Inside parseCallInfoFrmResource for ChoiceType::Undefined");
					}
				}

			} else {
				logger.error(dialogueId + ":: Inside parseCallInfoFrmResource for ChoiceType::null");
			}

		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Call Info from Resource " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in AinOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] Call Info from Resource parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding Call Info from Resource " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse Call Info from Resource", e);
			}
			throw new ASNParsingException(
					dialogueId + ":: [PH] ASN Parsing Failure, Network Busy parsing failure occured.", e,
					MESSAGE.CALL_INFO_FRM_RESRC);
		}

	}

	/**
	 * method use for encoding CALL_INFO_TO_RESOURCE.
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createCITR(CallData callData) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: Inside createCITR");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC);

		if (annSpec == null) {
			logger.error(dialogId + ":: No announmcement spec specified by Service");
			throw new Exception(dialogId + ":: :: No announmcement spec specified by Service");
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] Inside createCITR");
		}

		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);

		AnnouncementBlock annblk = AinScfProtocolFieldCodec.encodeAnnouncementBlock(annSpec, dialogId);

		StrParameterBlockChoiceType strParamChoiceType = new StrParameterBlockChoiceType();
		strParamChoiceType.selectAnnouncementBlock(annblk);

		StrParameterBlock strParam = new StrParameterBlock();
		strParam.setValue(strParamChoiceType);

		SendToResourceArg sendToResourceArg = new SendToResourceArg();

		// Resource Type as 0 for Play Announcement
		ResourceType rstype = new ResourceType();
		rstype.setValue(0);
		sendToResourceArg.setResourceType(rstype);

		sendToResourceArg.setStrParameterBlock(strParam);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(sendToResourceArg);
		opCode.add(AinOpCodes.SEND_TO_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: CITR created with Ann Block");
		}
		return encodeList.getFirst();
	}

	/**
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */

	public static byte[] createCITRForPlayFlex(CallData callData) throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] createCITRForPlayFlex:enter");
		}
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) leg1.get(LegDataAttributes.NP_ANN_SPEC);

		AnnouncementBlock annBlk = AinScfProtocolFieldCodec.encodeAnnouncementBlock(annSpec, dialogId);
		IPStrParameterBlock ipStrParamBlk = new IPStrParameterBlock();
		ipStrParamBlk.selectAnnouncementBlock(annBlk);

		byte[] rea = { 0x2b, 0x11, 0x69, 0x03 };
		// encoding of ResourceEncodingAuthority
		ResourceEncodingAuthority resourceEncodingAuthority = new ResourceEncodingAuthority();
		resourceEncodingAuthority.setValue(rea);

		// encoding of IPResourceType
		IPResourceType iPResourceType = new IPResourceType();

		// IPResourceType = 0 ("Play Announcement")
		byte[] play = { 0x00 };
		iPResourceType.setValue(play);
		/**
		 * FlexParameterBlockContent::= [3] IMPLICIT SEQUENCE { [1] IMPLICIT
		 * ResourceEncodingAuthority, [2] IMPLICIT IPResourceType, [3]
		 * IPStrParameterBlock, [4] IMPLICIT FlashIndication OPTIONAL }
		 */
		FlexParameterBlockContentSequenceType flexParaBlkContSeqType = new FlexParameterBlockContentSequenceType();
		flexParaBlkContSeqType.setResourceEncodingAuthority(resourceEncodingAuthority);
		flexParaBlkContSeqType.setIPResourceType(iPResourceType);
		flexParaBlkContSeqType.setIPStrParameterBlock(ipStrParamBlk);

		FlexParameterBlockContent flexParaBlkCont = new FlexParameterBlockContent();
		flexParaBlkCont.setValue(flexParaBlkContSeqType);

		byte[] flexParaBlkContbyte = AinOperationsCoding.encodeFieldsValue(flexParaBlkCont,
				"FlexParameterBlockContent");
		if (logger.isDebugEnabled()) {
			logger.info("the length of flexParaBlkContbyte::" + flexParaBlkContbyte.length);
		}
		// FlexParameterBlock ::= OCTET STRING (SIZE(1..120))
		FlexParameterBlock flexparBlk = new FlexParameterBlock(flexParaBlkContbyte);
		StrParameterBlockChoiceType strParamChoiceType = new StrParameterBlockChoiceType();
		strParamChoiceType.selectFlexParameterBlock(flexparBlk);

		StrParameterBlock strParam = new StrParameterBlock();
		strParam.setValue(strParamChoiceType);

		// SendToResourceArg sendToResourceArg =new SendToResourceArg();

		CallInfoFromResourceArg callInfoToResRetResult = new CallInfoFromResourceArg();

		// Resource Type as 4 - Flex
		ResourceType rstype = new ResourceType();
		rstype.setValue(4);
		callInfoToResRetResult.setResourceType(rstype);
		callInfoToResRetResult.setStrParameterBlock(strParam);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(callInfoToResRetResult);
		opCode.add(AinOpCodes.CALL_INFO_FROM_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] createCITRForPlayFlex:exit");
		}

		return encodeList.getFirst();

	}

	/**
	 * method that is use to create str parameter block for Call_Info_To_Resource.
	 * i.e. return result of Call_Info_From_Resource.
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createCITRForPlay(CallData callData) throws Exception {

		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + "::[PH] Inside createCITRForPlay");
		}

		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) leg1.get(LegDataAttributes.NP_ANN_SPEC);

		AnnouncementBlock annblk = AinScfProtocolFieldCodec.encodeAnnouncementBlock(annSpec, dialogId);

		StrParameterBlockChoiceType strParamChoiceType = new StrParameterBlockChoiceType();
		strParamChoiceType.selectAnnouncementBlock(annblk);

		StrParameterBlock strParam = new StrParameterBlock();
		strParam.setValue(strParamChoiceType);

		// SendToResourceArg sendToResourceArg =new SendToResourceArg();

		CallInfoFromResourceArg callInfoRetRes = new CallInfoFromResourceArg();

		// Resource Type as 0 for Play Announcement
		ResourceType rstype = new ResourceType();
		rstype.setValue(0);
		callInfoRetRes.setResourceType(rstype);

		callInfoRetRes.setStrParameterBlock(strParam);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(callInfoRetRes);
		opCode.add(AinOpCodes.CALL_INFO_FROM_RESOURCE);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogId + ":: CITR created with Ann Block");
		}
		return encodeList.getFirst();
	}

	/**
	 * method is used to create blank Call_Info_To_Resource.
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createDisconnectCITR(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Inside createDisconnectCITR");
		}

		CallInfoFromResourceArg citrarg = new CallInfoFromResourceArg();

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(AinOpCodes.CALL_INFO_FROM_RESOURCE);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(citrarg);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId + " : Exiting createDisconnectCITR");
		}

		return encodeList.getFirst();
	}

	/**
	 * Method for parsing GetDataQuery fields 
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static void parseGetDataQuery(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseGetDataQuery");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			GetDataQuery.decodeGetDataQuery(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: GetDATA Query parsing failure occured.", ainExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: GetData Query parsing failure occured.", invIpExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseGetDataQuery " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: GetData Query parsing failure occured.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] GetData Query parsed successfully");
		}
	}



	/**
	 * Method for parsing CC1 query fields 
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static void parseCc1Query(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseCc1Query");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			CC1Query.decodeCc1Query(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: CC1 Query parsing failure occured.", ainExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: CC1 Query parsing failure occured.", invIpExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseCc1Query " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: CC1 Query parsing failure occured.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] CC1 Query parsed successfully");
		}
	}

	/**
	 * Method for parsing CC2 query fields 
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static void parseCc2Query(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseCc2Query");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			CC2Query.decodeCc2Query(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: CC2 Query parsing failure occured.", ainExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: CC2 Query parsing failure occured.", invIpExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseCc2Query " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: CC2 Query parsing failure occured.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] CC2 Query parsed successfully");
		}
	}	

	/**
	 * Method for parsing TLNS query fields 
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static void parseTlnsQuery(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseTlnsQuery");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			TLNSQuery.decodeTlnsQuery(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: TLNS Query parsing failure occured.", ainExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: TLNS Query parsing failure occured.", invIpExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseTlnsQuery " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: TLNS Query parsing failure occured.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] TLNS Query parsed successfully");
		}
	}	

	/**
	 * Method for parsing Intercept query fields 
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static void parseInterceptQuery(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseInterceptQuery");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			InterceptQuery.decodeInterceptQuery(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: Intercept Query parsing failure occured.", ainExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: Intercept Query parsing failure occured.", invIpExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseInterceptQuery " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: Intercept Query parsing failure occured.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Intercept Query parsed successfully");
		}
	}	

	/**
	 * Method for parsing ICDC query fields 
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static void parseICDCQuery(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseICDCQuery");
		}
		byte[] input = invokeIndEvent.getParameters().getParameter();
		try {
			ICDCQuery.decodeICDCQuery(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: ICDC Query parsing failure occured.", ainExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: ICDC Query parsing failure occured.", invIpExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseICDCQuery " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: ICDC Query parsing failure occured.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] ICDC Query parsed successfully");
		}
	}		


	/**
	 * This method is called by protocol handler for creating RRBCSM for Arming of
	 * DP for CAIN network. In this case only Network Busy need to be armed along 
	 * with CAIN extension. Required for Roggers. 
	 * 
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static byte[] createRRBCSMForArmingForCain(CallData callData, Action action) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside createRRBCSMForArming");
		}

		// Encode RRBCSM event
		RequestReportBCMEventArg rrbcsmEventArg = new RequestReportBCMEventArg();

		//only Network Busy will be part of EDP request
		EDPRequest edpRequest = new EDPRequest();
		EDPRequestNonAsn edpReqNonAsn = new EDPRequestNonAsn();

		BitString bs = new BitString();

		// Now encode EDP Request based on the SET type.
		// Arm Network Busy
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: CAIN Arm Network Busy");
		}
		edpReqNonAsn.enableEdp(EDPRequestNonAsn.NETWORK_BUSY);

		// Encode EDPRequest
		bs = new BitString();
		bs.setValue(edpReqNonAsn.encodeEDPRequestNonAsn(), 3); // setting value 3 which refers to
		// no. of bits unused
		edpRequest.setValue(bs);
		rrbcsmEventArg.setEDPRequest(edpRequest);

		// Extension Parameter
		byte [] extnParams = {0x31, 0x03, (byte) 0x93, 0x01, 0x0a};

		if(logger.isDebugEnabled()){
			logger.debug("RRBCSM: CAIN is enabled, encoded Bytes:" + 
					CommonUtils.formatBytes(extnParams));
		}

		ExtensionParameter extParam = new ExtensionParameter();
		ExtensionParameterSequenceType extParamSeqType = new 
				ExtensionParameterSequenceType();

		ObjectIdentifier objId = new ObjectIdentifier();
		objId.setValue("1.2.840.113533.8.65.16");

		extParamSeqType.setAssignmentAuthority(objId);
		extParamSeqType.setParameters(extnParams);

		extParam.setValue(extParamSeqType);
		rrbcsmEventArg.setExtensionParameter(extParam);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(rrbcsmEventArg);
		opCode.add(AinOpCodes.REQUEST_REPORT_BCSM_EVENT);

		LinkedList<byte[]> encodeList = AinOperationsCoding.encodeOperations(operationObjs, opCode);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Exiting createRRBCSMForArming");
		}

		return encodeList.getFirst();
	}

	/**
	 * Method for parsing Account code query fields 
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static void parseAccountCodeQuery(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {

		int dialogueId = invokeIndEvent.getDialogueId();
		byte[] input = invokeIndEvent.getParameters().getParameter();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] parse parseAccountCodeQuery, bytes:" +
					CommonUtils.formatBytes(input));
		}
		try {
			AccountCode.decodeAccountCodeQuery(callData, input);
		} catch (AINCodecException ainExp) {
			logger.error("AinCodecException ::", ainExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: TLNS Query parsing failure occured.", ainExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (InvalidInputException invIpExp) {
			logger.error("InvalidInputException ::", invIpExp);
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: TLNS Query parsing failure occured.", invIpExp,
					MESSAGE.LIDB_PROTOCOL_ERR);
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseTlnsQuery " + e.getMessage());
			throw new ASNParsingException(
					dialogueId + "::[PH] ASN Parsing Failure: TLNS Query parsing failure occured.", e,
					MESSAGE.LIDB_PROTOCOL_ERR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] TLNS Query parsed successfully");
		}
	}	
}
