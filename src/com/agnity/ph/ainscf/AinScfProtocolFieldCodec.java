/****
Copyright (c) 2020 Agnity, Inc. All rights reserved.


This is proprietary source code of Agnity, Inc.


Agnity, Inc. retains all intellectual property rights associated 
with this source code. Use is subject to license terms.

This source code contains trade secrets owned by Agnity, Inc.
Confidentiality of this computer program must be maintained at
 all times, unless explicitly authorized by Agnity, Inc.
 ****/
package com.agnity.ph.ainscf;

import static com.agnity.ph.common.enums.PersistanceType.PERSISTABLE;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.bn.exceptions.EnumParamOutOfRangeException;
import org.bn.types.BitString;

import com.agnity.ain.asngenerated.AINDigits;
import com.agnity.ain.asngenerated.AMAAlternateBillingNumber;
import com.agnity.ain.asngenerated.AMADigitsDialedWC;
import com.agnity.ain.asngenerated.AMALineNumber;
import com.agnity.ain.asngenerated.AMAslpID;
import com.agnity.ain.asngenerated.AlternateCarrier;
import com.agnity.ain.asngenerated.AnnounceElement;
import com.agnity.ain.asngenerated.AnnouncementBlock;
import com.agnity.ain.asngenerated.AnnouncementDigitBlock;
import com.agnity.ain.asngenerated.BackspaceDigits;
import com.agnity.ain.asngenerated.BillingIndicator;
import com.agnity.ain.asngenerated.CalledPartyID;
import com.agnity.ain.asngenerated.CallingPartyID;
import com.agnity.ain.asngenerated.Carrier;
import com.agnity.ain.asngenerated.ChargeNumber;
import com.agnity.ain.asngenerated.ChargePartyStationType;
import com.agnity.ain.asngenerated.DTMFKeyMap;
import com.agnity.ain.asngenerated.EDPNotification;
import com.agnity.ain.asngenerated.EDPRequest;
import com.agnity.ain.asngenerated.EchoData;
import com.agnity.ain.asngenerated.GenericAddress;
import com.agnity.ain.asngenerated.GenericAddressList;
import com.agnity.ain.asngenerated.IgnoreDigits;
import com.agnity.ain.asngenerated.InterAnnounceBlock;
import com.agnity.ain.asngenerated.Lata;
import com.agnity.ain.asngenerated.ONoAnswerTimer;
import com.agnity.ain.asngenerated.OutpulseNumber;
import com.agnity.ain.asngenerated.OverflowBillingIndicator;
import com.agnity.ain.asngenerated.PrimaryBillingIndicator;
import com.agnity.ain.asngenerated.PrimaryTrunkGroup;
import com.agnity.ain.asngenerated.RedirectingPartyID;
import com.agnity.ain.asngenerated.ResetDigits;
import com.agnity.ain.asngenerated.ResourceType;
import com.agnity.ain.asngenerated.SecondAlternateCarrier;
import com.agnity.ain.asngenerated.TerminateDigits;
import com.agnity.ain.asngenerated.TimeOutTimer;
import com.agnity.ain.asngenerated.UninterAnnounceBlock;
import com.agnity.ain.asngenerated.DestinationAddress;
import com.agnity.ain.datatypes.ACGEncounteredNonAsn;
import com.agnity.ain.datatypes.AddressSignal;
import com.agnity.ain.datatypes.AinDigits;
import com.agnity.ain.datatypes.AnnouncementElement;
import com.agnity.ain.datatypes.CarrierFormat;
import com.agnity.ain.datatypes.ChargeNum;
import com.agnity.ain.datatypes.EDPNotificationNonAsn;
import com.agnity.ain.datatypes.EDPRequestNonAsn;
import com.agnity.ain.datatypes.GenericAddrs;
import com.agnity.ain.datatypes.KeyCodes;
import com.agnity.ain.datatypes.MaximumDigits;
import com.agnity.ain.datatypes.TrunkGroup;
import com.agnity.ain.enumdata.AddPrsntRestEnum;
import com.agnity.ain.enumdata.CalgNatOfNumEnum;
import com.agnity.ain.enumdata.CallTreatIndicatorEnum;
import com.agnity.ain.enumdata.CalledNatOfNumEnum;
import com.agnity.ain.enumdata.CarrierFormatNatEnum;
import com.agnity.ain.enumdata.CarrierFormatSelectionEnum;
import com.agnity.ain.enumdata.NatureOfAddCallingEnum;
import com.agnity.ain.enumdata.NumPlanEnum;
import com.agnity.ain.enumdata.NumToOutpulseEnum;
import com.agnity.ain.enumdata.TestIndEnum;
import com.agnity.ain.enumdata.TypeOfAddrsEnum;
import com.agnity.ain.exceptions.InvalidInputException;
import com.agnity.ain.operations.AinOperationsCoding;
import com.agnity.ain.util.Constant;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.BillingIndicatorInfo;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CarrierInfo;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.TrunkGroupInfo;
import com.agnity.mphdata.common.AnnSpec.PlayMessage;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;

import jain.protocol.ss7.tcap.component.InvokeIndEvent;

/**
 * This class contains methods used to decode/encode different fields received in incoming 
 * message like InfoCollected, InfoAnalyze etc. Some of the fields are common to most of the 
 * incoming messages, therefore this class provides common plan for parsing messges.
 * 
 * @author rarya
 *
 */
public class AinScfProtocolFieldCodec {


	private static Logger logger = Logger.getLogger(AinScfProtocolFieldCodec.class);


	/**
	 * Method used to decode UerId. User ID may contain 
	 * 	- DN
	 * 	- Private Facility GID
	 * 	- Trunk Group
	 * @param dlgId
	 * @param uid
	 * @param rxMsgId
	 * @return
	 * @throws Exception 
	 */
	public static com.agnity.mphdata.common.UserID decodeUserId
	(int dlgId, com.agnity.ain.asngenerated.UserID uid, String rxMsgId) throws Exception{
		String dn=null;
		Integer facilityId=0;
		Integer trunkGroup=0;
		String adsstr=null;
		com.agnity.mphdata.common.UserID commonUser = null;

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + ":: Inside decodeUserId");
		}

		try {
			// UserId is of choice type. Only one of the fields - 
			// DN, FacilityGID, TrunkGroupId or ADIscpeID could be present
			if(uid.getValue().getDn()!=null){
				dn=AddressSignal.decodeAdrsSignal(uid.getValue().getDn().getValue(), 0, 0);
			}

			if(uid.getValue().getPrivateFacilityGID()!=null){
				facilityId=uid.getValue().getPrivateFacilityGID().getValue();
			}

			if(uid.getValue().getTrunkGroupID()!=null){
				trunkGroup=uid.getValue().getTrunkGroupID().getValue();
			}

			if(uid.getValue().getADSIcpeID()!=null){
				adsstr=AddressSignal.decodeAdrsSignal(uid.getValue().getADSIcpeID().getValue(), 0, 0);
			}

			// userId 
			commonUser= new com.agnity.mphdata.common.UserID(dn, facilityId,  trunkGroup, adsstr);


			if (logger.isDebugEnabled()) {
				logger.debug(dlgId + ":: [PH] Extracted user id  " + commonUser);
			}
		}catch(Exception ex){
			logger.error(dlgId + ":: Exception in parsing UserId");
			throw (ex);
		}
		return commonUser;
	}

	/**
	 * Method to decode calling party 
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws ASNParsingException 
	 * @throws InvalidInputException 
	 */
	public static PhoneNumber decodeCallingParty(int dlgId,  byte[] rxBuf, String rxMsgId) throws ASNParsingException, InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeCallingParty for " + rxMsgId);
		}

		PhoneNumber callingNumber =null;
		AinDigits callingPartyNumdigit=new AinDigits();
		AinDigits callingPartyNum = callingPartyNumdigit.decodeAinDigits(rxBuf, Constant.CALLING);

		// check if decoded calling party is null or incoming digits are not present
		if (callingPartyNum == null || callingPartyNum.getAddrSignal() == null
				|| "".equals(callingPartyNum.getAddrSignal().trim())) {
			/*
			 * call should be handled as ASn parse failure as address signal is missing
			 */
			logger.error(dlgId + ":: Calling party num address signal missing");
			throw new ASNParsingException(dlgId
					+ ":: Calling party num address signal missing", MESSAGE.valueOf(rxMsgId));
		}

		// Creating Non ASN PhoneNumber object
		callingNumber = nonAsnParseCallingPartyNum(dlgId, callingPartyNum);
		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeCallingParty, CallingNumber:[" + callingNumber + "]");
		}

		return callingNumber;

	}

	/**
	 * Method to decode Called Party
	 * @param dlgId 
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static PhoneNumber decodeCalledParty(int dlgId,byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeCalledParty for " + rxMsgId);
		}
		PhoneNumber calledNumber= null;
		AinDigits calledPartyNumdigit =new AinDigits();
		AinDigits calledPartyNum = calledPartyNumdigit.decodeAinDigits(rxBuf, Constant.CALLING);

		//Creating Non ASN PhoneNumber object
		calledNumber = nonAsnParseCallingPartyNum(dlgId, calledPartyNum);
		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit parseCalledPartyNum, calledNumber:[" + calledNumber+"]");
		}

		return calledNumber;
	}

	/**
	 * Method to decode Charge Number
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static PhoneNumber decodeChargeNum(int dlgId,byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeChargeNum for " + rxMsgId);
		}

		ChargeNum chargeNum=new ChargeNum();
		chargeNum.decodeChargeNum(rxBuf);
		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: ChargeNum:[" + chargeNum+"]");
		}

		PhoneNumber chargePN=new PhoneNumber();
		chargePN.setAddress(chargeNum.getAddrSignal());
		chargePN.setNumberingPlan(chargeNum.getNumPlanEnum().getCode());
		chargePN.setNatureOfAddress(chargeNum.getChargeNumEnum().getCode());
		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeChargeNum number, chargePN :["+chargePN+"]");
		}
		return chargePN;
	}

	/**
	 * Method to decode Collected Address Info
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static PhoneNumber decodeCollectedAddressInfo(int dlgId, byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeCollectedAddressInfo for " + rxMsgId);
		}

		AinDigits ainDigits = new AinDigits();

		//this method decodes aindigits
		ainDigits.decodeAinDigits(rxBuf, Constant.CALLED);

		PhoneNumber collectedPn=new PhoneNumber();
		collectedPn.setAddress(ainDigits.getAddrSignal());
		collectedPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeChargeNum number, collected phone number :["+collectedPn+"]");
		}

		return collectedPn;
	}

	/**
	 * Method to decode Collected Digits 
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static String decodeCollectedDigits(int dlgId, byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeCollectedDigits for " + rxMsgId);
		}

		AinDigits collecDigits = AinDigits.getInstance().decodeAinDigits(rxBuf,Constant.CALLED);

		if(collecDigits == null){
			throw new InvalidInputException("Collected digit is null");
		}
		//		//Creating Non ASN PhoneNumber object
		//		PhoneNumber collectedDigitsPn = nonAsnParseCallingPartyNum(dlgId,
		//				collecDigits);

		// d is * and e is #.
		String digits = StringUtils.replace(collecDigits.getAddrSignal(), "e", "#");
		digits = StringUtils.replace(digits, "d", "*");

		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeCollectedDigits, CollectedDigits :["+collecDigits.getAddrSignal()+"]" +
					" translated:" + digits);
		}

		return digits;
	}

	/**
	 * Method to decode Access Code 
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static PhoneNumber decodeAccessCode(int dlgId,byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeAccessCode for " + rxMsgId);
		}

		//this method decodes aindigits
		AinDigits collectedDigits = AinDigits.getInstance().decodeAinDigits(rxBuf,
				Constant.CALLED);

		PhoneNumber accessCodePn = nonAsnParseCallingPartyNum(dlgId,
				collectedDigits);

		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeAccessCode, accessCodePn :["+accessCodePn+"]");
		}

		return accessCodePn;
	}

	/**
	 * Method to decode Original Called Party Id
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static PhoneNumber decodeOriginalCalledPartyId(int dlgId,byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeOriginalCalledPartyId for " + rxMsgId);
		}

		AinDigits ainDigits = new AinDigits();

		//decoding ain digits
		ainDigits.decodeAinDigits(rxBuf, Constant.CALLING);
		PhoneNumber origCalledPn=new PhoneNumber();
		origCalledPn.setAddress(ainDigits.getAddrSignal());
		origCalledPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());


		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeOriginalCalledPartyId, origCalledPn :["+origCalledPn+"]");
		}

		return origCalledPn;
	}

	/**
	 * Method to deocde Redirecting Party Id
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static PhoneNumber decodeRedirectingPartyId(int dlgId,byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeRedirectingPartyId for " + rxMsgId);
		}

		AinDigits ainDigits = new AinDigits();

		//decoding ain digits
		ainDigits.decodeAinDigits(rxBuf, Constant.CALLED);
		PhoneNumber redirectPn=new PhoneNumber();
		redirectPn.setAddress(ainDigits.getAddrSignal());
		redirectPn.setNumberingPlan(ainDigits.getNumPlanEnum().getCode());

		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeRedirectingPartyId, redirectPn :["+redirectPn+"]");
		}

		return redirectPn;
	}

	/**
	 * Method to decode Carrier 
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static CarrierInfo decodeCarrier(int dlgId,byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeCarrier for " + rxMsgId);
		}

		CarrierFormat cf= new CarrierFormat();

		//decoding carrierformat object
		cf.decodeCarrierFormat(rxBuf);
		CarrierInfo ci = new CarrierInfo();
		ci.setAddress(cf.getAddrSignal());
		ci.setCarrierSelection(cf.getCarrierFormatSelectionEnum().getCode());
		ci.setNatureOfCarrier(cf.getCarrierFormatNatEnum().getCode());

		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeCarrier, CarrierInfo :["+ci+"]");
		}

		return ci;
	}

	/**
	 * Method to decode Automatic Call Gaping 
	 * @param dlgId
	 * @param rxAcg
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */
	public static int isACGEncountered(int dlgId,byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside isACGEncountered for " + rxMsgId);
		}

		ACGEncounteredNonAsn acgNonAsn=new ACGEncounteredNonAsn();

		//decoding ACGEncounteredNonAsn object
		ACGEncounteredNonAsn nonasn=acgNonAsn.decodeACGEncounteredNonAsn(rxBuf);
		int acgcode=nonasn.getAcgEncounteredEnum().getCode();

		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit isACGEncountered, ACGEncountered number :["+acgcode+"]");
		}

		return acgcode;
	}

	/**
	 * Method to decode Generic Name 
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 * @throws InvalidInputException 
	 */

	//-------------------------------------------------------------------------------

	public static String decodeLata(int dlgId, byte[] rxBuf, String rxMsgId) throws InvalidInputException{

		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " :: Inside decodeLata for " + rxMsgId);
		}

		Lata lata = new Lata();
		AinDigits ainDigits = new AinDigits();
		ainDigits.decodeAinDigits(rxBuf, Constant.CALLED);

		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + ":: Exit decodeLata, ainDigits :["+ainDigits+"]");
		}

		return ainDigits.getAddrSignal();
	}



	/**
	 * Method to decode generic name. Refer to GR1188 A.1
	 * @param dlgId
	 * @param rxBuf
	 * @param rxMsgId
	 * @return
	 */
	public static String decodeGenericName(int dlgId, byte[] rxBuf, String rxMsgId, 
			LegData legData){
		if(legData == null || rxBuf.length == 0){
			logger.error(dlgId +" decodeGenericName: legData or length is not correct in rxMsgId: "
					+ rxMsgId);
		}
		
		if(rxBuf.length == 1){
			if(logger.isDebugEnabled()){
				logger.debug(dlgId +"rxMsgId: " + rxMsgId + " length is 1, not doing anything");
			}
			return null;
		}
		
		int clgPres = rxBuf[0] & 0x03;
		
		// allowed
		switch(clgPres){
		case 0: // allowed 
			legData.set(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION, "0");
			break;
		case 1: // rstrcited
			legData.set(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION, "1");
			break;
		case 2: // Blocking toggle
			legData.set(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION, "2");
			break;
		case 3: // No indication
			legData.set(LegDataAttributes.P_GN_M_GENERIC_NAME_PRESENTATION, "0");
			break;
		}
		
		// name decoding
		String name = null;
		try {
			// rx buffer, offset (ignore 1 byte), total length (minus first byte) and charset
			name = new String(rxBuf, 0, rxBuf.length-1, "UTF-8");
			legData.set(LegDataAttributes.P_GN_M_GENERIC_NAME, name);
		} catch (UnsupportedEncodingException e) {
			logger.error(dlgId + " eror in decoding decoding name: "+ CommonUtils.formatBytes(rxBuf));
		}
		
		if(logger.isDebugEnabled()){
			logger.debug(dlgId + " Generic name decoded: presentation:" + clgPres + " :name: "+ name);
		}
		
		return name;
	}

	/**
	 * Non-ASN method to decode calling party from ASN
	 * @param dialogueId
	 * @param callingPartyNum
	 * @return
	 */
	private static PhoneNumber nonAsnParseCallingPartyNum(int dlgId, AinDigits number) {
		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + "[PH]:: Inside nonAsnParseCallingPartyNum");
		}

		PhoneNumber callingNumber = new PhoneNumber();

		/*
		 *  Nature Of Address
		 */
		CalgNatOfNumEnum natureOfAddrEnum = number.getCalgNatOfNumEnum();
		if (logger.isDebugEnabled()) {
			logger.info("natureOfAddrEnum::"+natureOfAddrEnum);
		}

		if ((natureOfAddrEnum == CalgNatOfNumEnum.SPARE)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_NAT_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_NAT_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_SUBS_NUM)
				|| (natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_SUBS_NUM)) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == CalgNatOfNumEnum.NOT_APPLICABLE) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if ((natureOfAddrEnum == CalgNatOfNumEnum.NON_UNIQUE_INTER_NAT_NUM)||natureOfAddrEnum == CalgNatOfNumEnum.UNIQUE_INTER_NAT_NUM) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}

		/*
		 *  Numbering Plan Indicator
		 */
		com.agnity.ain.enumdata.NumPlanEnum numPlanIndEnum = number.getNumPlanEnum();
		if (numPlanIndEnum == NumPlanEnum.PRIVATE_NP) {
			callingNumber.setNumberingPlan(PhoneNumber.NP_PRIVATE);
		} else if (numPlanIndEnum == NumPlanEnum.ISDN_NP) {
			callingNumber.setNumberingPlan(PhoneNumber.NP_ISDN);
		} else {
			callingNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);
		}

		// Presentation and screening indicator
		callingNumber.setPresentationIndicator(number.getClgPrsntRestIndEnum().getCode());
		callingNumber.setScreeningIndicator(number.getScreeningIndEnum().getCode());

		/*
		 *  Address
		 */
		String addrSignal = number.getAddrSignal();
		callingNumber.setAddress(addrSignal);
		if (logger.isDebugEnabled()) {
			logger.debug(dlgId + "::[PH] Extracted Address Signal is " + addrSignal + 
					", NOA:"+natureOfAddrEnum.getCode()+", NumPlan:"+numPlanIndEnum.getCode()+
					", presentationInd:"+number.getClgPrsntRestIndEnum().getCode()+
					", screeningInd:"+number.getScreeningIndEnum().getCode());
		}
		return callingNumber;
	}





	/**
	 * @param destinationNumber
	 * @return
	 * @throws InvalidInputException
	 */
	public static CalledPartyID encodeDestinationNumber(PhoneNumber destinationNumber) throws InvalidInputException{
		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeDestinationNumber");
		}
		AinDigits ainDigits;

		CalledNatOfNumEnum natureOfAddrEnum = CalledNatOfNumEnum.fromInt(destinationNumber.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
		}

		NumPlanEnum numberPlan = NumPlanEnum.fromInt(destinationNumber.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		ainDigits= new AinDigits();
		ainDigits.setAddrSignal(destinationNumber.getAddress());
		ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
		ainDigits.setNumPlanEnum(numberPlan);

		byte[] calledPartyNum = ainDigits.encodeAinDigits();

		CalledPartyID calledPartyNumber = new CalledPartyID();
		calledPartyNumber.setValue(new AINDigits(calledPartyNum));
		return calledPartyNumber;

	}

	/**
	 * @param ci
	 * @return
	 * @throws InvalidInputException
	 */
	public static Carrier encodeCarrier(CarrierInfo ci) throws InvalidInputException{

		// Encode carrier
		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeCarrier");
		}
		byte [] carrierByte = null;
		carrierByte = getCarrierEncodedBuffer(ci);
		Carrier ansgenCarrier = new Carrier();
		ansgenCarrier.setValue(new com.agnity.ain.asngenerated.CarrierFormat(carrierByte));


		if (logger.isDebugEnabled()) {
			logger.debug("Setting Carrier as:[" +  ci + "], byte: " + 
					CommonUtils.formatBytes(carrierByte));

		}
		return ansgenCarrier;
	}


	/**
	 * @param callingPartyId
	 * @return
	 * @throws InvalidInputException
	 */

	public static CallingPartyID encodeCallingPartyId(PhoneNumber callingPartyId) throws InvalidInputException{
		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeCallingPartyId");
		}
		AINDigits asnAinDigits = encodeAINDigits(callingPartyId);

		CallingPartyID asnCallingPartyId = new CallingPartyID();
		asnCallingPartyId.setValue(asnAinDigits);
		if (logger.isDebugEnabled()) {
			logger.debug("Setting CallingPartyID as:[" +  callingPartyId + "], byte: " + 
					CommonUtils.formatBytes(asnAinDigits.getValue()));
		}

		return  asnCallingPartyId;

	}


	/**
	 * @param ci
	 * @return
	 * @throws InvalidInputException
	 */
	public static AlternateCarrier encodeAlternateCarrier(CarrierInfo ci) throws InvalidInputException{
		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeAlternateCarrier");
		}
		byte [] carrierByte = null;

		carrierByte = getCarrierEncodedBuffer(ci);
		AlternateCarrier ansgenCarrier = new AlternateCarrier();
		ansgenCarrier.setValue(new com.agnity.ain.asngenerated.CarrierFormat(carrierByte));


		if (logger.isDebugEnabled()) {
			logger.debug("Setting AlternateCarrier as:[" +  ci + "], byte: " + 
					CommonUtils.formatBytes(carrierByte));
		}
		return ansgenCarrier;
	}


	/**
	 * @param ci
	 * @return
	 * @throws InvalidInputException
	 */
	public static SecondAlternateCarrier encodeSecondCarrier(CarrierInfo ci) throws InvalidInputException{
		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeSecondCarrier");
		}
		byte [] carrierByte = null;

		carrierByte = getCarrierEncodedBuffer(ci);
		SecondAlternateCarrier ansgenCarrier = new SecondAlternateCarrier();
		ansgenCarrier.setValue(new com.agnity.ain.asngenerated.CarrierFormat(carrierByte));

		if (logger.isDebugEnabled()) {
			logger.debug("Setting SecondAlternateCarrier as:[" +  ci + "], byte: " + 
					CommonUtils.formatBytes(carrierByte));
		}
		return ansgenCarrier;
	}



	/**
	 * @param digits
	 * @return
	 * @throws InvalidInputException
	 */
	public static AMAslpID encodeAMASlpId(String digits) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeSecondCarrier");
		}

		byte[] slpId=null;
		AMAslpID asnSlpId = new AMAslpID();
		if(StringUtils.isNotBlank(digits) && StringUtils.length(digits) != 9) {
			logger.error("Not encoding AMASlpID in AnalyzeRoute as length is not equal to 9, [" + digits + "]"); 
		} else {
			slpId = AddressSignal.encodeAdrsSignal(digits);


			asnSlpId.setValue(slpId);

			if (logger.isDebugEnabled()) {
				logger.debug("Setting AAMASlpID as:[" +  digits + "], byte: " + 
						CommonUtils.formatBytes(slpId));
			}
		}

		return asnSlpId;
	}


	/**
	 * @param chargeNum
	 * @return
	 * @throws InvalidInputException
	 */
	public static ChargeNumber encodeChargeNumber(PhoneNumber chargeNum) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeChargeNumber");
		}

		AINDigits asnAinDigits = encodeAINDigits(chargeNum);

		ChargeNumber asnChargeNum = new ChargeNumber();
		asnChargeNum.setValue(asnAinDigits);

		if (logger.isDebugEnabled()) {
			logger.debug("Setting ChargeNumber as:[" +  chargeNum + "], byte: " + 
					CommonUtils.formatBytes(asnAinDigits.getValue()));
		}

		return asnChargeNum;
	}


	/**
	 * @param altLineNum
	 * @return
	 * @throws InvalidInputException
	 */
	public static AMAAlternateBillingNumber encodeAMAAlternateBilling(PhoneNumber altLineNum) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeAMAAlternateBilling");
		}

		AINDigits asnAinDigits = encodeAINDigits(altLineNum);

		AMAAlternateBillingNumber asnAmaAltBillNum = new AMAAlternateBillingNumber();
		asnAmaAltBillNum.setValue(asnAinDigits);

		return asnAmaAltBillNum;
	}


	/**
	 * @param billInfo
	 * @return
	 * @throws InvalidInputException
	 */
	public static PrimaryBillingIndicator encodeAMAPrimaryBillingIndicator(BillingIndicatorInfo billInfo) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeAMAPrimaryBilling");
		}

		byte[] callType = AddressSignal.encodeAdrsSignal(billInfo.getCallType());
		byte[] srvFeatureId = AddressSignal.encodeAdrsSignal(billInfo.getServiceFeatureIdentifiation());

		byte[] billInd = new byte[4];

		if(callType.length == 1){
			billInd[0] = 0x00;
			billInd[1] = callType[0];
		} else {
			billInd[0] = callType[0];
			billInd[1] = callType[1];
		}

		if(srvFeatureId.length == 1){
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



		if (logger.isDebugEnabled()) {
			logger.debug("Setting PrimaryBillingInd as:[" +  billInfo + "], byte: " + 
					CommonUtils.formatBytes(billInd));
		}
		return primBillingInd;
	}


	/**
	 * @param billInfo
	 * @return
	 * @throws InvalidInputException
	 */
	public static OverflowBillingIndicator encodeOverflowBillingIndicator(BillingIndicatorInfo billInfo) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeOverflowBillingIndicator");
		}

		byte[] callType = AddressSignal.encodeAdrsSignal(billInfo.getCallType());
		byte[] srvFeatureId = AddressSignal.encodeAdrsSignal(billInfo.getServiceFeatureIdentifiation());

		byte[] billInd = new byte[4];

		if(callType.length == 1){
			billInd[0] = 0x00;
			billInd[1] = callType[0];
		} else {
			billInd[0] = callType[0];
			billInd[1] = callType[1];
		}

		if(srvFeatureId.length == 1){
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


		if (logger.isDebugEnabled()) {
			logger.debug("Setting OverflowBillingIndicator as:[" +  billInfo + "], byte: " + 
					CommonUtils.formatBytes(billInd));
		}

		return overflowBillInd;
	}


	/**
	 * @param redirPartyId
	 * @return
	 * @throws InvalidInputException
	 */
	public static RedirectingPartyID encodeRedirectingPartyId(PhoneNumber redirPartyId) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside redirectingPartyId");
		}

		AinDigits ainDigits;
		CalledNatOfNumEnum natureOfAddrEnum = CalledNatOfNumEnum.fromInt(redirPartyId.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
		}

		NumPlanEnum numberPlan = NumPlanEnum.fromInt(redirPartyId.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		ainDigits= new AinDigits();
		ainDigits.setAddrSignal(redirPartyId.getAddress());
		ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
		ainDigits.setNumPlanEnum(numberPlan);

		byte[] redirPtyId = ainDigits.encodeAinDigits();

		RedirectingPartyID asnRedirPartyId = new RedirectingPartyID();
		asnRedirPartyId.setValue(new AINDigits(redirPtyId));

		if (logger.isDebugEnabled()) {
			logger.debug("Setting RedirectingPartyID as:[" +  redirPartyId + "], byte: " + 
					CommonUtils.formatBytes(redirPtyId));
		}

		return asnRedirPartyId;
	}


	/**
	 * @param trgInf
	 * @return
	 * @throws InvalidInputException
	 */
	public static PrimaryTrunkGroup encodePrimaryTrunkGroup(TrunkGroupInfo trgInf) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodePrimaryTrunkGroup");
		}

		byte[] trunkGroupByte = TrunkGroup.encodeAlternateTrunkGrp(
				CallTreatIndicatorEnum.fromInt(trgInf.getCallTreatmentInd()),
				NumToOutpulseEnum.fromInt(trgInf.getNumToOutpulse()),
				trgInf.getSfg(),
				trgInf.getTrunkGroupAddress());

		PrimaryTrunkGroup asnPriTrunkGrp = new PrimaryTrunkGroup(); 
		asnPriTrunkGrp.setValue(trunkGroupByte);

		if (logger.isDebugEnabled()) {
			logger.debug("Setting PrimaryTrunkGroup as:[" +  trgInf + "], byte: " + 
					CommonUtils.formatBytes(trunkGroupByte));
		}
		return asnPriTrunkGrp;
	}


	/**
	 * @param value
	 * @return
	 */
	public static ChargePartyStationType encodeChargePartyStattionType(String value){

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeChargePartyStattionType");
		}

		ChargePartyStationType ch = new ChargePartyStationType();
		ch.setValue(Integer.parseInt(value));

		return ch;
	}


	/**
	 * @param leg2Data
	 * @return
	 * @throws InvalidInputException
	 */
	public static GenericAddressList encodeGenericAddressList(LegData leg2Data) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeGenericAddressList");
		}

		GenericAddressList genericAddressList= null;
		List gaList = null;
		for(int i=0; i < 5; ++i){
			PhoneNumber destinationAddr = null;
			TypeOfAddrsEnum typeOfAddrs = null;

			switch (i){
			case 0: 
				// Check for Dialed Number 
				if(leg2Data.get(LegDataAttributes.P_GENERIC_ADDR_DIALED_NUM) != null){
					destinationAddr = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GENERIC_ADDR_DIALED_NUM);
					typeOfAddrs = TypeOfAddrsEnum.DAILED_NUM;
					destinationAddr.setNatureOfAddress(3);
					destinationAddr.setNumberingPlan(1);
				}
				break;
			case 1:
				// Check for Destination Number 
				if(leg2Data.get(LegDataAttributes.P_GENERIC_ADDR_DESTINATION_NUM) != null){
					destinationAddr = (PhoneNumber) leg2Data.get(LegDataAttributes.P_GENERIC_ADDR_DESTINATION_NUM);
					typeOfAddrs = TypeOfAddrsEnum.DESTINATION_NO;
				}
				break;
			case 2:
			case 3:
			case 4:
				break;
			}

			if(destinationAddr != null){
				byte[] value = GenericAddrs.encodeGenericAddrs(typeOfAddrs,
						NatureOfAddCallingEnum.fromInt(destinationAddr.getNatureOfAddress()),
						NumPlanEnum.fromInt(destinationAddr.getNumberingPlan()),
						TestIndEnum.fromInt(0),
						AddPrsntRestEnum.fromInt(destinationAddr.getAddressPresentationRestrictedIndicator()),
						destinationAddr.getAddress());
				GenericAddress ga = new GenericAddress();
				ga.setValue(value);

				if(gaList == null) {
					gaList = new ArrayList<GenericAddressList>();
				}

				gaList.add(ga);
			}
		}
		if(gaList != null){
			genericAddressList = new GenericAddressList();
			genericAddressList.setValue(gaList);
		}
		return genericAddressList;
	}

	/**
	 * @param outpulseNum
	 * @return
	 * @throws InvalidInputException
	 */
	public static OutpulseNumber encodeOutpulseNumber(PhoneNumber outpulseNum) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeOutpulseNumber");
		}
		// outpulse number should have nature of number and numbering plan as 0

		outpulseNum.setNumberingPlan(0);
		outpulseNum.setNatureOfAddress(0);

		AINDigits asnAinDigits = encodeAINDigits(outpulseNum);

		OutpulseNumber asnOutpulseNum = new OutpulseNumber();
		asnOutpulseNum.setValue(asnAinDigits);



		if (logger.isDebugEnabled()) {
			logger.debug("Setting Outpulse Number as:[" +  outpulseNum + "], byte: " + 
					CommonUtils.formatBytes(asnAinDigits.getValue()));
		}
		return asnOutpulseNum;
	}


	/**
	 * @param leg2Data
	 * @return
	 * @throws InvalidInputException
	 */
	public static List encodeAMADialedDigitsWCList(LegData leg2Data) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside createGenericAddressList");
		}

		// AMADialedDigitsWC contains that customer dialed along wiht context ID to name the digit 
		// string like Access Code, Auth code, Customer Dialed Account Recording. 
		// There could be upto 5 AMADialedDigitWC in AnalyzeRoute 
		List dialedDigitWCList= null;
		for(int i =0; i < 5; ++i){
			PhoneNumber collectedDigits = null;

			switch(i){
			case 0: 
				if(leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC1) != null){
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC1);
				}
				break;
			case 1:
				if(leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC2) != null){
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC2);
				}
				break;
			case 2:
				if(leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC3) != null){
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC3);
				}
				break;
			case 3:
				if(leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC4) != null){
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC4);
				}
				break;
			case 4:
				if(leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC5) != null){
					collectedDigits = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMADIGITSDIALEDWC5);
				}
				break;		
			}

			if(collectedDigits != null && 
					StringUtils.isNotBlank(collectedDigits.getAddress())){
				collectedDigits.setNatureOfAddress(0);
				collectedDigits.setNumberingPlan(0);

				AINDigits asnAinDigits = encodeAINDigits(collectedDigits);
				AMADigitsDialedWC amaDigitWc= new AMADigitsDialedWC();
				amaDigitWc.setValue(asnAinDigits);

				if(dialedDigitWCList == null)
					dialedDigitWCList = new ArrayList<AMADigitsDialedWC>();

				dialedDigitWCList.add(amaDigitWc);
			}
		}
		return dialedDigitWCList;
	}

	/**
	 * @param leg2Data
	 * @return
	 * @throws InvalidInputException
	 */
	public static List encodeAMALineNumber(LegData leg2Data) throws InvalidInputException{
		// Encode AMALineNumber. There could be upto 2 AMALineNumber as per standard. 

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeAMALineNumber");
		}

		List<AMALineNumber> asnAMALineNumCollection = null;
		for(int i=0; i < 2; ++i){
			PhoneNumber lineNum = null;
			if(i==0 && (leg2Data.get(LegDataAttributes.P_AMALINENUMBER) != null)){ 
				lineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMALINENUMBER);

				if(logger.isDebugEnabled()){
					logger.debug("AMALineNumber1 - Encoding value:"+ lineNum);
				}
			}else if(i==1 && (leg2Data.get(LegDataAttributes.P_AMALINENUMBER2) != null)){ 
				lineNum = (PhoneNumber) leg2Data.get(LegDataAttributes.P_AMALINENUMBER2);

				if(logger.isDebugEnabled()){
					logger.debug("AMALineNumber2 - Encoding value:"+ lineNum);
				}
			}

			if(lineNum != null){
				// In case of AMALineNumber the Nature of Number and number Plan is not applicable
				lineNum.setNatureOfAddress(0);
				lineNum.setNumberingPlan(0);
				AINDigits asnAinDigits = encodeAINDigits(lineNum);

				AMALineNumber asnAmaLineNum = new AMALineNumber();
				asnAmaLineNum.setValue(asnAinDigits);

				if(asnAMALineNumCollection == null)
					asnAMALineNumCollection = new ArrayList<AMALineNumber>();

				asnAMALineNumCollection.add(asnAmaLineNum);
			}
		}
		if(asnAMALineNumCollection != null){

			if(logger.isDebugEnabled()){
				logger.debug(" Encoded AMALineNumber to AnalyzeRoute");
			}
		}

		return asnAMALineNumCollection;
	}

	/**
	 * @param dialogueId
	 * @return
	 */
	// methods for Send Notificatiion and Termination Notification

	public static EchoData encodeEcho(int dialogueId){

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeEcho");
		}

		// Echo Data - encode dialogue ID 
		// Since Echo Datat is of 6 Octets, Right paddign it with 0x00
		byte[] dlg = CommonUtils.formatIntToByte(dialogueId);	

		byte[] allByteArray = new byte[dlg.length + (6-dlg.length)];
		byte[] pad = new byte[(6-dlg.length)];
		ByteBuffer buff = ByteBuffer.wrap(allByteArray);
		buff.put(pad);
		buff.put(dlg);

		byte[] combined = buff.array();

		if(logger.isDebugEnabled()){
			logger.debug(":: Dialogue ID: "+dialogueId+ ", added as Echo Data:"+CommonUtils.formatBytes(combined));
		}

		// add buffer
		EchoData data = new EchoData();
		data.setValue(combined);

		return data;
	}


	/**
	 * @param callData
	 * @param invokeIndEvent
	 * @return
	 * @throws ParameterOutOfRangeException, ASNParsingException
	 */
	public static Object encodeCloseCause(CallData callData,InvokeIndEvent invokeIndEvent) throws ParameterOutOfRangeException, ASNParsingException
	{
		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeCloseCause");
		}

		int dialogueId = (Integer) callData
				.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside parseClose");
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] Extract Close  Argument from Close");
		}

		Object obj;
		try {
			obj =  AinOperationsCoding
					.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding Close "
					+ epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId+ ":: [PH] Error in AinOperationsCoding.decodeOperation",epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId
					+ ":: [PH] Close parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding Close "
					+ e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId
						+ ":: [PH] Exception in parseClose", e);
			}
			throw new ASNParsingException(
					dialogueId
					+ ":: [PH] ASN Parsing Failure,Close parsing failure occured.",
					e, MESSAGE.CLOSE);
		}

		return obj;
	}


	public static EDPNotification encodeEDPNotification(){

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeEDPNotification");
		}

		EDPNotification edpNotificationAsn= new EDPNotification();
		EDPNotificationNonAsn edpNotifNonAsn=new EDPNotificationNonAsn();
		edpNotifNonAsn.enableEdp(EDPNotificationNonAsn.O_ANSWER);
		BitString bs= new BitString();
		bs.setValue(edpNotifNonAsn.encodeEDPNotificationNonAsn(), 4); // 4 refers to no of bit unused
		edpNotificationAsn.setValue(bs);
		return edpNotificationAsn;
	}


	/**
	 * @param erbTypeSet
	 * @param dialogueId
	 * @return
	 */
	EDPRequest encodeEDPRequest(Set<Action.ERB_TYPE> erbTypeSet,int dialogueId){

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeEDPRequest");
		}
		//BitString bs= new BitString();
		EDPRequest edpRequest= new EDPRequest();
		EDPRequestNonAsn edpReqNonAsn = new EDPRequestNonAsn();
		// Now encode EDP Request based on the SET type. 
		// oCalledPartyBusy
		if (erbTypeSet.contains(Action.ERB_TYPE.ERB_BUSY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Arm oCalledPartyBusy");
			}

			edpReqNonAsn.enableEdp(EDPRequestNonAsn.O_CALLEDPARTY_BUSY);
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
		BitString bs=  new BitString();
		bs.setValue(edpReqNonAsn.encodeEDPRequestNonAsn(), 4); // setting value 4 which refers to 
		// no. of bits unused
		edpRequest.setValue(bs);
		return edpRequest;
	}

	/**
	 * @param noAnswerTimeObj
	 * @return
	 */
	public static ONoAnswerTimer encodeNoAnswerTime(Object noAnswerTimeObj){

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeNoAnswerTime");
		}

		int noAnswerTimer = (Integer) noAnswerTimeObj;
		if (logger.isDebugEnabled()) {
			logger.debug(" noAnswerTimer from service[ms] = " + noAnswerTimer);
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
			logger.debug("AIN RRBCSM - Set no answer time " + noAnswerTimer);
		}

		ONoAnswerTimer timer= new ONoAnswerTimer();
		timer.setValue(noAnswerTimer);
		return timer;
	}


	/**
	 * @param erbTimoutTimerObj
	 * @return
	 */
	public static TimeOutTimer encodeTimeOutTimer(Object erbTimoutTimerObj){

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeTimeOutTimer");
		}

		int erbTimoutTimer = (Integer) erbTimoutTimerObj;
		TimeOutTimer tot = new TimeOutTimer();
		tot.setValue(erbTimoutTimer);
		return tot;
	}

	/**
	 * @param assistSspIpRoutAddrStr
	 * @return
	 * @throws InvalidInputException
	 */
	public static DestinationAddress encodeDestinationAddress(String assistSspIpRoutAddrStr) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeDestinationAddress");
		}


		AinDigits sd= new AinDigits();
		sd.setAddrSignal(assistSspIpRoutAddrStr);
		sd.setNumPlanEnum(NumPlanEnum.ISDN_NP);
		sd.setCalledNatOfNumEnum(CalledNatOfNumEnum.NAT_NUM);


		AINDigits assistSspIpRoutAddrDigits = new AINDigits(sd.encodeAinDigits());

		DestinationAddress  dA=new DestinationAddress();
		dA.setValue(assistSspIpRoutAddrDigits);

		return dA;
	}

	/*
	 * set Resource type
	 */
	public static ResourceType encodeResourceType(){

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeResourceType");
		}

		ResourceType rstype= new ResourceType();
		rstype.setValue(4);
		return rstype;
	}

	/**
	 * This method is used to encode the carrier, alternate carrier and second
	 * alternate carrier. It returns encoded byte array. 
	 * @param ci
	 * @return
	 * @throws InvalidInputException
	 */
	static byte [] getCarrierEncodedBuffer(CarrierInfo ci) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside getCarrierEncodedBuffer");
		}

		CarrierFormat cf = new CarrierFormat();
		cf.setAddrSignal(ci.getAddress());
		cf.setCarrierFormatNatEnum(CarrierFormatNatEnum.fromInt(ci.getNatureOfCarrier()));
		cf.setCarrierFormatSelectionEnum(CarrierFormatSelectionEnum.fromInt(ci.getCarrierSelection()));

		return cf.encodeCarrierFormat();
	}

	/**
	 * This calss takes PhoneNumber as input and generate ASNDigits class.
	 * @param ph
	 * @return
	 * @throws InvalidInputException
	 */
	static com.agnity.ain.asngenerated.AINDigits encodeAINDigits(PhoneNumber ph) throws InvalidInputException{

		if (logger.isDebugEnabled()) {
			logger.debug("Inside encodeAINDigits");
		}

		CalledNatOfNumEnum natureOfAddrEnum = CalledNatOfNumEnum.fromInt(ph.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = CalledNatOfNumEnum.NAT_NUM;
		}

		NumPlanEnum  numberPlan = NumPlanEnum.fromInt(ph.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		AinDigits ainDigits= new AinDigits();
		ainDigits.setAddrSignal(ph.getAddress());
		ainDigits.setCalledNatOfNumEnum(natureOfAddrEnum);
		ainDigits.setNumPlanEnum(numberPlan);

		byte[] buffer = ainDigits.encodeAinDigits();

		AINDigits asnAinDigits = new AINDigits();
		asnAinDigits.setValue(buffer);

		return asnAinDigits;
	}

	/**
	 * 
	 * @return
	 * @throws Exception 
	 */
	public static com.agnity.ain.asngenerated.AnnouncementBlock encodeAnnouncementBlock(AnnSpec annSpec,int dialogId) throws Exception{
		AnnouncementBlock annblk = new AnnouncementBlock();

		if (annSpec == null) {
			logger.error(dialogId
					+ ":: No announmcement spec specified by Service");
			throw new Exception(dialogId
					+ ":: :: No announmcement spec specified by Service");
		}

		ArrayList<PlayMessage> annList = (ArrayList<PlayMessage>) annSpec
				.getPlayMsgList();
		Iterator<PlayMessage> annEntry = annList.iterator();
		PlayMessage playMessage = null;


		if ((annList == null) || (annList.isEmpty())) {
			logger.error(dialogId
					+ ":: No announmcement specified by Service");
			throw new Exception(dialogId
					+ "::  No announmcement specified by Service");
		}

		/**
		 * noOfInfo digit set as zero.
		 * In the case of noOfInfo digit is not zero
		 *  digitInfo also have to set...not handling till now.
		 */
		int noOfDigi = 0;
		int digitInfo = 0;
		String getSs7AnnouncementId = null;

		List <AnnounceElement> annlist = new ArrayList<AnnounceElement>();
		while (annEntry.hasNext()) {
			playMessage = annEntry.next();
			AnnSpec.ANN_TYPE annType = playMessage.getAnnType();
			getSs7AnnouncementId = playMessage.getMessageId();
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: annType is " + annType
						+ " msgToPlay is " + getSs7AnnouncementId);
			}
			if(getSs7AnnouncementId != null ){
				byte[] announceElement = AnnouncementElement.encodeAnnouncementElement(Integer.parseInt(getSs7AnnouncementId), noOfDigi, digitInfo);
				annlist.add(new AnnounceElement(announceElement));
			}
		}
		//if barge not true then encode it as uninterrupted Ann block.
		if(!annSpec.isBarge()){
			UninterAnnounceBlock notbarge = new UninterAnnounceBlock();
			notbarge.setValue(annlist);
			annblk.setUninterAnnounceBlock(notbarge);
		}else{
			InterAnnounceBlock bargetrue = new InterAnnounceBlock();
			bargetrue.setValue(annlist);
			annblk.setInterAnnounceBlock(bargetrue);
		}
		return annblk;
	}

	/**
	 * 
	 * @return
	 * @throws Exception 
	 */
	public static com.agnity.ain.asngenerated.AnnouncementDigitBlock encodeAnnouncementDigitBlock(AnnSpec annSpec,int dialogId) throws Exception{
		AnnouncementDigitBlock annDigiblk = new AnnouncementDigitBlock();


		if (annSpec == null) {
			logger.error(dialogId
					+ ":: No announmcement spec specified by Service");
			throw new Exception(dialogId
					+ ":: :: No announmcement spec specified by Service");
		}

		int maximumdigit = annSpec.getMaxDigits();

		// check maximum digit is 0 then set to default 254
		if(maximumdigit <= 0){
			maximumdigit = 254;
		}

		if(logger.isDebugEnabled()){
			logger.debug(dialogId + " :: Maximm Digit :" + maximumdigit );
		}

		ArrayList<PlayMessage> annList = (ArrayList<PlayMessage>) annSpec
				.getPlayMsgList();


		if ((annList == null) || (annList.isEmpty())) {
			logger.error(dialogId
					+ ":: No announmcement specified by Service");
			throw new Exception(dialogId
					+ ":: Malformed Ann List for Announcement");
		}

		Iterator<PlayMessage> annEntry = annList.iterator();
		PlayMessage playMessage = null;

		/**
		 * noOfInfo digit set as zero.
		 * In the case of noOfInfo digit is not zero
		 *  digitInfo also have to set...not handling till now.
		 */
		int noOfDigi = 0;
		int digitInfo = 0;
		String getSs7AnnouncementId = null;

		List <AnnounceElement> annlist = new ArrayList<AnnounceElement>();
		while (annEntry.hasNext()) {
			playMessage = annEntry.next();
			AnnSpec.ANN_TYPE annType = playMessage.getAnnType();
			getSs7AnnouncementId = playMessage.getMessageId();
			if (logger.isDebugEnabled()) {
				logger.debug(dialogId + ":: annType is " + annType
						+ " msgToPlay is " + getSs7AnnouncementId);
			}
			if(getSs7AnnouncementId != null ){
				byte[] announceElement = AnnouncementElement.
						encodeAnnouncementElement(Integer.parseInt(getSs7AnnouncementId), noOfDigi, digitInfo);
				annlist.add(new AnnounceElement(announceElement));
			}
		}

		Integer maximumDigi = MaximumDigits.encodeMaximumDigits(maximumdigit);
		//MaximumDigits
		com.agnity.ain.asngenerated.MaximumDigits maxdigi = 
				new com.agnity.ain.asngenerated.MaximumDigits(maximumDigi);

		annDigiblk.setMaximumDigits(maxdigi);
		//if barge not true then encode it as uninterrupted Ann block.
		if(!annSpec.isBarge()){
			UninterAnnounceBlock notbarge = new UninterAnnounceBlock();
			//notbarge.add(annEle);
			notbarge.setValue(annlist);
			annDigiblk.setUninterAnnounceBlock(notbarge);

		}else{
			InterAnnounceBlock bargetrue = new InterAnnounceBlock();
			//	bargetrue.add(annEle);
			bargetrue.setValue(annlist);
			annDigiblk.setInterAnnounceBlock(bargetrue);
		}


		/**
		 * DTMFKeyMap ::= SEQUENCE {
			[0] IMPLICIT IgnoreDigits OPTIONAL,
			[1] IMPLICIT TerminateDigits OPTIONAL,
			[2] IMPLICIT ResetDigits OPTIONAL,
			[3] IMPLICIT BackspaceDigits OPTIONAL
			}
		 */ 

		// terminateDigits - These are digits that can cause the digit entry to 
		//                   terminate prior to the collection of MaximumDigits digits.
		// mapped to rtk
		TerminateDigits termDigit=null;
		String terminateDigits = annSpec.getTerminationKey();
		if(StringUtils.isNotBlank(terminateDigits)){
			com.agnity.ain.asngenerated.KeyCodes asnKeyCodesTerm = 
					new com.agnity.ain.asngenerated.KeyCodes(KeyCodes.encodeKeyCodes(terminateDigits));
			termDigit = new TerminateDigits();
			termDigit.setValue(asnKeyCodesTerm);

			if(logger.isDebugEnabled()){
				logger.debug(dialogId + ":: Setting terminateDigits:" + terminateDigits);
			}
		}

		//resetDigits - These are digits that can cause collection to start over as if no digits had
		//              been collected.
		// mapped to escapekey
		String resetDigits = annSpec.getEsacpeKey();
		ResetDigits resDigit = null;
		if(StringUtils.isNotBlank(resetDigits)){
			com.agnity.ain.asngenerated.KeyCodes asnKeyCodesReset = 
					new com.agnity.ain.asngenerated.KeyCodes(KeyCodes.encodeKeyCodes(resetDigits));
			resDigit = new ResetDigits();
			resDigit.setValue(asnKeyCodesReset);

			if(logger.isDebugEnabled()){
				logger.debug(dialogId + ":: Setting reset digits:" + resetDigits);
			}
		}

		//ignoreDigits- These are digits that should be ignored by the IP when pressed by a user
		// ToDo 
		String ignoreDigits =   annSpec.getIgnoreDigits();
		IgnoreDigits ignrDigit = null;
		if(StringUtils.isNotBlank(ignoreDigits)){
			com.agnity.ain.asngenerated.KeyCodes asnKeyCodesIgnore = 
					new com.agnity.ain.asngenerated.KeyCodes(KeyCodes.encodeKeyCodes(ignoreDigits));
			ignrDigit = new IgnoreDigits();
			ignrDigit.setValue(asnKeyCodesIgnore);

			if(logger.isDebugEnabled()){
				logger.debug(dialogId + ":: Setting Ignore digits:" + ignoreDigits);
			}
		}

		// Backspace. These are keys that will cause the previously entered digit (if one
		// or more digits have already been entered) to be removed from the list of
		// collected digits and allow collection to continue.
		// ToDo
		String backspaceDigits =   annSpec.getBackspaceDigits();
		BackspaceDigits bkspcDigits = null;
		if(StringUtils.isNotBlank(backspaceDigits)){
			com.agnity.ain.asngenerated.KeyCodes asnKeyCodesBkspac = 
					new com.agnity.ain.asngenerated.KeyCodes(KeyCodes.encodeKeyCodes(backspaceDigits));
			bkspcDigits = new BackspaceDigits();
			bkspcDigits.setValue(asnKeyCodesBkspac);

			if(logger.isDebugEnabled()){
				logger.debug(dialogId + ":: Setting backspace digits:" + backspaceDigits);
			}
		}		

		// set DTMFKey only if any of the above is define 
		DTMFKeyMap dtmfKey = null;
		if(termDigit != null || resDigit != null || 
				ignrDigit != null || bkspcDigits != null){
			dtmfKey = new DTMFKeyMap();

			if(termDigit != null){
				dtmfKey.setTerminateDigits(termDigit);
			}

			if(resDigit != null) {
				dtmfKey.setResetDigits(resDigit);
			}

			if(ignrDigit != null){
				dtmfKey.setIgnoreDigits(ignrDigit);
			}

			if(bkspcDigits != null){
				dtmfKey.setBackspaceDigits(bkspcDigits);
			}

			// set DTMFKy 
			annDigiblk.setDtmfKeyMap(dtmfKey);

			if(logger.isDebugEnabled()){
				logger.debug(dialogId + " :: setting DTMFKey");
			}
		}

		return annDigiblk;
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

	/**
	 * Method returns ASCII value for given string
	 * @param displayTextLegData
	 * @return
	 */
	public static byte[] encodeIa5(String displayTextLegData) {
		return displayTextLegData.getBytes(StandardCharsets.US_ASCII);
	}
}
