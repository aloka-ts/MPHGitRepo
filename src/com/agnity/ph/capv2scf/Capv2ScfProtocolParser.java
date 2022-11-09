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
package com.agnity.ph.capv2scf;

import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolHelper.sendComponentReq;
import static com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil.src;

import java.util.ArrayList;
import java.util.Date;
import java.util.EventObject;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.bn.exceptions.EnumParamOutOfRangeException;

import com.agnity.ain.asngenerated.AnnounceElement;
import com.agnity.camelv2.asngenerated.AChBillingChargingCharacteristics;
import com.agnity.camelv2.asngenerated.ApplicationTimer;
import com.agnity.camelv2.asngenerated.ApplyChargingArg;
import com.agnity.camelv2.asngenerated.ApplyChargingReportArg;
import com.agnity.camelv2.asngenerated.AssistingSSPIPRoutingAddress;
import com.agnity.camelv2.asngenerated.BCSMEvent;
import com.agnity.camelv2.asngenerated.BothwayThroughConnectionInd;
import com.agnity.camelv2.asngenerated.CAMEL_AChBillingChargingCharacteristics;
import com.agnity.camelv2.asngenerated.CAMEL_CallResult;
import com.agnity.camelv2.asngenerated.CallResult;
import com.agnity.camelv2.asngenerated.CalledPartyNumber;
import com.agnity.camelv2.asngenerated.CancelArg;
import com.agnity.camelv2.asngenerated.ConnectArg;
import com.agnity.camelv2.asngenerated.ConnectToResourceArg;
import com.agnity.camelv2.asngenerated.ConnectToResourceArg.ResourceAddressChoiceType;
import com.agnity.camelv2.asngenerated.ContinueWithArgumentArg;
import com.agnity.camelv2.asngenerated.CorrelationID;
import com.agnity.camelv2.asngenerated.DPSpecificCriteria;
import com.agnity.camelv2.asngenerated.DestinationRoutingAddress;
import com.agnity.camelv2.asngenerated.Digits;
import com.agnity.camelv2.asngenerated.EstablishTemporaryConnectionArg;
import com.agnity.camelv2.asngenerated.EventReportBCSMArg;
import com.agnity.camelv2.asngenerated.EventSpecificInformationBCSM;
import com.agnity.camelv2.asngenerated.EventTypeBCSM;
import com.agnity.camelv2.asngenerated.FCIBillingChargingCharacteristics;
import com.agnity.camelv2.asngenerated.FurnishChargingInformationArg;
import com.agnity.camelv2.asngenerated.InbandInfo;
import com.agnity.camelv2.asngenerated.InformationToSend;
import com.agnity.camelv2.asngenerated.InitialDPArg;
import com.agnity.camelv2.asngenerated.Integer4;
import com.agnity.camelv2.asngenerated.LegID;
import com.agnity.camelv2.asngenerated.LegType;
import com.agnity.camelv2.asngenerated.LocationInformation;
import com.agnity.camelv2.asngenerated.MessageID;
import com.agnity.camelv2.asngenerated.MonitorMode;
import com.agnity.camelv2.asngenerated.PlayAnnouncementArg;
import com.agnity.camelv2.asngenerated.ReceivedInformationArg;
import com.agnity.camelv2.asngenerated.ReceivingSideID;
import com.agnity.camelv2.asngenerated.RedirectingPartyID;
import com.agnity.camelv2.asngenerated.RedirectionInformation;
import com.agnity.camelv2.asngenerated.ReleaseCallArg;
import com.agnity.camelv2.asngenerated.RequestReportBCSMEventArg;
import com.agnity.camelv2.asngenerated.ResetTimerArg;
import com.agnity.camelv2.asngenerated.ScfID;
import com.agnity.camelv2.asngenerated.SendingSideID;
import com.agnity.camelv2.asngenerated.ServiceInteractionIndicatorsTwo;
import com.agnity.camelv2.asngenerated.SpecializedResourceReportArg;
import com.agnity.camelv2.asngenerated.TimeInformation;
import com.agnity.camelv2.asngenerated.TimerID;
import com.agnity.camelv2.asngenerated.CAMEL_CallResult.TimeDurationChargingResultSequenceType;
import com.agnity.camelv2.asngenerated.TimerID.EnumType;
import com.agnity.camelv2.datatypes.AdrsStringDataType;
import com.agnity.camelv2.datatypes.BearerCapabilityDataType;
import com.agnity.camelv2.datatypes.CalledPartyNum;
import com.agnity.camelv2.datatypes.CallingPartyNum;
import com.agnity.camelv2.datatypes.CauseDataType;
import com.agnity.camelv2.datatypes.GenericDigitsDataType;
import com.agnity.camelv2.datatypes.ImsiDataType;
import com.agnity.camelv2.datatypes.LocationNum;
import com.agnity.camelv2.enumdata.AdrsPrsntRestdEnum;
import com.agnity.camelv2.enumdata.CauseValEnum;
import com.agnity.camelv2.enumdata.CodingStndEnum;
import com.agnity.camelv2.enumdata.EncodingSchemeEnum;
import com.agnity.camelv2.enumdata.GTIndicatorEnum;
import com.agnity.camelv2.enumdata.IntNtwrkNumEnum;
import com.agnity.camelv2.enumdata.LocationEnum;
import com.agnity.camelv2.enumdata.NatureOfAdrsEnum;
import com.agnity.camelv2.enumdata.NumINcomplteEnum;
import com.agnity.camelv2.enumdata.NumPlanEnum;
import com.agnity.camelv2.enumdata.NumQualifierIndEnum;
import com.agnity.camelv2.enumdata.RoutingIndicatorEnum;
import com.agnity.camelv2.enumdata.SPCIndicatorEnum;
import com.agnity.camelv2.enumdata.SSNIndicatorEnum;
import com.agnity.camelv2.enumdata.ScreeningIndEnum;
import com.agnity.camelv2.exceptions.InvalidInputException;
import com.agnity.camelv2.operations.CapV2OpCodes;
import com.agnity.camelv2.operations.CapV2OperationsCoding;
import com.agnity.camelv2.util.NonAsnArg;
import com.agnity.inapitutcs2.datatypes.GenericDigits;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.CapV2CallStates;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.mphdata.common.AnnSpec.PlayMessage;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ASNParsingException.FAILTYPE;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolHelper;
import com.agnity.ph.inapcs1scf.InapCS1ScfProtocolUtil;
import com.agnity.ph.common.exception.CriticalityTypeException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.tcap.ComponentIndEvent;
import jain.protocol.ss7.tcap.TcapConstants;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.InvokeReqEvent;
import jain.protocol.ss7.tcap.component.Operation;
import jain.protocol.ss7.tcap.component.Parameters;
import jain.protocol.ss7.tcap.component.ResultIndEvent;

/**
 * This class is used to do CAP protocol messages parsing
 *
 */
public class Capv2ScfProtocolParser {

	private static Logger logger = Logger.getLogger(Capv2ScfProtocolParser.class);
	// Leg Types
	public static LegType leg1Type = new LegType(new byte[] { 0x01 });
	public static LegType leg2Type = new LegType(new byte[] { 0x02 });

	/**
	 * This method is called by protocol handler to parse the IDP received and
	 * populate the callData object from the received parameters.
	 * 
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param callData       represents an instance of CallData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseIdp(InvokeIndEvent invokeIndEvent, CallData callData)
			throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {
		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] Extract InitialDP Argument from IDP:" + invokeIndEvent);

		}
		InitialDPArg idpArg = null;
		try {
			// check if CAPV2 flow is for OCS then we
			String capv2Flow = Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW);

			if (capv2Flow != null && StringUtils.equalsIgnoreCase(capv2Flow, "OCS")) {
				idpArg = replaceExtensionOffset(invokeIndEvent);
			}else{
				idpArg = (InitialDPArg) CapV2OperationsCoding.decodeOperation(invokeIndEvent, true);
			}
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding IDP " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in CapV2OperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] IDP parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding IDP " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse IDP", e);
			}
			throw new ASNParsingException(dialogueId + ":: [PH] ASN Parsing Failure, IDP parsing failure occured.", e,
					MESSAGE.IDP);
		}

		/*
		 * IDP.serviceKey
		 */
		Integer serviceKey = idpArg.getServiceKey().getValue().getValue();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] Extracted Service Key from IDP is " + serviceKey);
		}

		// Service returned will be negative incase if it is greater than
		// 128. So either add 256 to negative value or logical AND with FF
		//serviceKey &= 0xFF;

		callData.set(CallDataAttribute.P_SERVICE_KEY, serviceKey.intValue());

		try {
			// Calling Party Number
			if (idpArg.isCallingPartyNumberPresent()) {
				CallingPartyNum callingPartyNum = CallingPartyNum
						.decodeCalgParty(idpArg.getCallingPartyNumber().getValue());

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Calling party number from IDP is " + callingPartyNum);
				}
				if (callingPartyNum == null || callingPartyNum.getAddrSignal() == null
						|| "".equals(callingPartyNum.getAddrSignal().trim())) {
					/*
					 * call should be handled as ASn parse failure as address signal is missing
					 */
					logger.error(dialogueId + ":: [PH] Calling party num address signal missing");
					throw new ASNParsingException(dialogueId + ":: [PH] Calling party num address signal missing",
							MESSAGE.IDP);
				}
				PhoneNumber callingNumber = parseCallingPartyNum(dialogueId, callingPartyNum);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Exit parseCallingPartyNum");
				}
				legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);
			}

			// Event Type BCSM
			//			if (idpArg.isEventTypeBCSMPresent()) {
			//
			//				EventTypeBCSM eventTypeBCSM = idpArg.getEventTypeBCSM();
			//
			//				if (logger.isDebugEnabled()) {
			//					logger.debug(dialogueId + ":: [PH] Extracted eventTypeBCSM from IDP is " + eventTypeBCSM);
			//				}
			//
			//				// legData.set(LegDataAttributes.NP_EVENT_TYPE_BCSM, eventTypeBCSM);
			//			}

			/*
			 * IDP.bearerCapability
			 */
			if (idpArg.isBearerCapabilityPresent()) {

				if(logger.isDebugEnabled()){
					logger.debug("IDP Bearar Capability Buffer:"+ 
							CommonUtils.formatBytes(idpArg.getBearerCapability().getBearerCap()));
				}

				BearerCapabilityDataType bearCap = BearerCapabilityDataType
						.decodeBearerCapability(idpArg.getBearerCapability().getBearerCap());

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted bearCap from IDP is " + bearCap);
				}
				int infoTransferCap = bearCap.getInfoTrnsfrCap().getCode();
				int infoTransferRate = bearCap.getInfoTrfrRate().getCode();
				int infoLayer1Protocol = -1;
				int infoLayer2Protocol = -1;
				int infoLayer3Protocol = -1;

				if(bearCap.getUserInfoLayer1Protocol() != null){
					infoLayer1Protocol = bearCap.getUserInfoLayer1Protocol().getCode();
				}
				if(bearCap.getUserInfoLayer2Protocol() != null){
					infoLayer2Protocol = bearCap.getUserInfoLayer2Protocol().getCode();
				}

				if(bearCap.getUserInfoLayer3Protocol() != null) {
					infoLayer3Protocol = bearCap.getUserInfoLayer3Protocol().getCode();
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted bearCap from IDP is infoTransferCap = "
							+ infoTransferCap + " infoTransferRate= " + infoTransferRate + " infoLayer1Protocol= "
							+ infoLayer1Protocol + " infoLayer2Protocol=" + infoLayer2Protocol + " infoLayer3Protocol="
							+ infoLayer3Protocol);
				}
				legData.set(LegDataAttributes.CAP_BEARER_CAP_INFO_TRANSFER_CAP, String.valueOf(infoTransferCap));
				legData.set(LegDataAttributes.CAP_BEARER_CAP_INFO_TRANSFER_RATE, String.valueOf(infoTransferRate));

				if(infoLayer1Protocol != -1){
					legData.set(LegDataAttributes.CAP_BEARER_CAP_USER_INFO_LAYER1, String.valueOf(infoLayer1Protocol));
				}
				if(infoLayer2Protocol != -1){
					legData.set(LegDataAttributes.CAP_BEARER_CAP_USER_INFO_LAYER2, String.valueOf(infoLayer2Protocol));
				}
				if(infoLayer3Protocol != -1){
					legData.set(LegDataAttributes.CAP_BEARER_CAP_USER_INFO_LAYER3, String.valueOf(infoLayer3Protocol));
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Bearer Cap is set properly");
				}
			}

			// IDP Location Number
			if (idpArg.isLocationNumberPresent()) {

				LocationNum locNum = LocationNum.decodeLocationNum(idpArg.getLocationNumber().getValue());

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted LocationNumber from IDP is " + locNum);
				}

				int natureOfAddress = locNum.getNatureOfAdrs().getCode();
				int numPlan = locNum.getNumPlan().getCode();
				int addresPresentRst = locNum.getAdrsPresntRestd().getCode();
				int screeningInumber = locNum.getScreening().getCode();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted location number  from IDP is natureOfAddress = "
							+ natureOfAddress + " numPlan= " + numPlan + " addresPresentRst= " + addresPresentRst
							+ " screeningInumber=" + screeningInumber);
				}
				PhoneNumber phNumber = new PhoneNumber();
				phNumber.setNatureOfAddress(natureOfAddress);
				phNumber.setNumberingPlan(numPlan);
				phNumber.setAddress(String.valueOf(addresPresentRst));
				phNumber.setScreeningIndicator(screeningInumber);

				legData.set(LegDataAttributes.CAP_LOCATION_NUM, phNumber);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Location number  is set properly");
				}
			}

			// Location Information 
			if (idpArg.isLocationInformationPresent()) {

				LocationInformation locInfo = idpArg.getLocationInformation();

				if (locInfo.isAgeOfLocationInformationPresent()) {
					int ageOfLocation = locInfo.getAgeOfLocationInformation().getValue();
				}

				if (locInfo.isVlr_numberPresent()) {
					byte[] vlrInfo = locInfo.getVlr_number().getValue().getValue();
					AdrsStringDataType vlrDataBlock = AdrsStringDataType.decodeAdrsString(vlrInfo);

					String msisdn = vlrDataBlock.getMscAdrs();
					String vlrCountryCode = msisdn.substring(0, 2); // first 2 digits are CC
					int vlrNoa = vlrDataBlock.getNatureOfAdrs().getCode();
					int vlrNumPlan = vlrDataBlock.getNumPlan().getCode();

					// remove this CAP_LOCATION_INFO_VLR_NUMBER
					legData.set(LegDataAttributes.CAP_LOCATION_INFO_VLR_NOA, Integer.toString(vlrNoa));
					legData.set(LegDataAttributes.CAP_LOCATION_INFO_MSISDN, msisdn);
					legData.set(LegDataAttributes.CAP_LOCATION_INFO_COUNTRY_CODE, vlrCountryCode);

					if (logger.isDebugEnabled()) {
						logger.debug("parseIDP: LocationInformation: VLR - MSISND:" + msisdn + ", CC:" + vlrCountryCode
								+ ", VlrNoa:" + vlrNoa + ", VlrNumPlan:" + vlrNumPlan);
					}
				}

				// CellGlobalId
				if (locInfo.isCellGlobalIdOrServiceAreaIdOrLAIPresent()) {
					if (locInfo.getCellGlobalIdOrServiceAreaIdOrLAI()
							.isCellGlobalIdOrServiceAreaIdFixedLengthSelected()) {
						byte[] cellInfo = locInfo.getCellGlobalIdOrServiceAreaIdOrLAI()
								.getCellGlobalIdOrServiceAreaIdFixedLength().getValue();
						String cellId = Capv2ScfProtocolHelper.encodeHexString(cellInfo);

						legData.set(LegDataAttributes.CAP_CELL_GLOBAL_ID, cellId);

						if(logger.isDebugEnabled()){
							logger.debug("parseID: CellGlobalId: "+ cellId);
						}
					} else if (locInfo.getCellGlobalIdOrServiceAreaIdOrLAI().isLaiFixedLengthSelected()) {
						// ToDO- need basis
					}
				}
			}

			// Calling party Category
			if (idpArg.isCallingPartysCategoryPresent()) {

				com.agnity.camelv2.enumdata.CalgPartyCatgEnum category = NonAsnArg.decodeCalgPartyCatg
						(idpArg.getCallingPartysCategory().getValue());

				if (category == null) {
					logger.error(dialogueId + ":: [PH] Calling party Category missing");
				}else{
					int callingPartyCategory = category.getCode();
					legData.set(LegDataAttributes.P_CPC, Integer.toString(callingPartyCategory));

					if (logger.isDebugEnabled()) {
						logger.debug(dialogueId + ":: [PH] Extracted CPC from IDP is " + callingPartyCategory);
					}
				}
			}

			// Call Reference Number
			if (idpArg.isCallReferenceNumberPresent()) {
				byte [] val= idpArg.getCallReferenceNumber().getValue();

				String callRefNum = Capv2ScfProtocolHelper.encodeHexString(val);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Call Reference Number from IDP is "
							+ callRefNum);
				}

				legData.set(LegDataAttributes.CAP_CALL_REFERENCE_NUM, callRefNum);
			}

			// IMSI
			if (idpArg.isIMSIPresent()) {
				ImsiDataType imsi = ImsiDataType.decodeImsi(idpArg.getIMSI().getValue().getValue());

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted IMSI from IDP is " + imsi);
				}
				if (imsi == null || "".equals(imsi.getMobileCountryCode()) || "".equals(imsi.getMobileNetworkCode())
						|| "".equals(imsi.getLocationAreaCode())) {

					// call should be handled as ASn parse failure as IMSI is missing

					logger.error(dialogueId + ":: [PH] IMSI missing");
					throw new ASNParsingException(dialogueId + ":: [PH] IMSI missing", MESSAGE.IDP);
				}

				String mobileCountryCode = imsi.getMobileCountryCode();
				String mobileNetworkCode = imsi.getMobileNetworkCode();
				String locationAreaCode = imsi.getLocationAreaCode();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted IMSI from IDP is mobileCountryCode = "
							+ mobileCountryCode + " mobileNetworkCode= " + mobileNetworkCode + " locationAreaCode= "
							+ locationAreaCode);
				}
				legData.set(LegDataAttributes.CAP_IMSI_COUNTRY_CODE, mobileCountryCode);
				legData.set(LegDataAttributes.CAP_IMSI_NETWORK_CODE, mobileNetworkCode);
				legData.set(LegDataAttributes.CAP_IMSI_AREA_CODE, locationAreaCode);
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] IMSI  is set properly");
				}
			}

			// MSC Address
			if (idpArg.isMscAddressPresent()) {
				AdrsStringDataType mscAddr = AdrsStringDataType.decodeAdrsString(
						idpArg.getMscAddress().getValue().getValue());

				String msisdn = mscAddr.getMscAdrs();
				String mscCountryCode = msisdn.substring(0, 2); // first 2 digits are CC
				int mscNoa = mscAddr.getNatureOfAdrs().getCode();
				int mscNumPlan = mscAddr.getNumPlan().getCode();

				legData.set(LegDataAttributes.CAP_MSC_ADDRESS_NOA, Integer.toString(mscNoa));
				legData.set(LegDataAttributes.CAP_MSC_ADDRESS_MSISDN, msisdn);
				legData.set(LegDataAttributes.CAP_MSC_ADDRESS_COUNTRY_CODE, mscCountryCode);

				if(logger.isDebugEnabled()){
					logger.debug("parseIDP: MSC Address: VLR - MSISND:" + msisdn 
							+ ", CC:" + mscCountryCode + ", mscNoa:" + mscNoa + ", mscNumPlan:" + mscNumPlan);
				}
			}

			// Timezone 
			if (idpArg.isTimeAndTimezonePresent()) {
				byte[] val = idpArg.getTimeAndTimezone().getValue();
				String timeAndTz = Capv2ScfProtocolHelper.encodeHexString(val);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted Time and TimeZone from IDP is "
							+ timeAndTz);
				}
				legData.set(LegDataAttributes.CAP_TIME_AND_TIMEZONE, timeAndTz);
			}

			// Called Party Number
			// give priority to Called Party Number. If not present then check for CalledPartyBCDNumber
			if(idpArg.isCalledPartyNumberPresent()){
				CalledPartyNum calledPartyNum = CalledPartyNum
						.decodeCaldParty(idpArg.getCalledPartyNumber().getValue(), false);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted called number from CalledPartyNumber is " + calledPartyNum);
				}

				PhoneNumber calledNumber = parseCalledPartyBCDNum(dialogueId, calledPartyNum);
				// extra check if called party contains f as filler then remove it. 
				// coming in Telus
				if(StringUtils.containsIgnoreCase(calledNumber.getAddress(), "f")){
					String modifiedCalledParty =  StringUtils.removeEndIgnoreCase(calledNumber.getAddress(), "f");
					calledNumber.setAddress(modifiedCalledParty);					
				}


				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted called number from IDP is " + calledNumber);
				}

				legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);

				if(calledPartyNum.getNatureOfAdrs() != null){
					legData.set(LegDataAttributes.P_CALLED_PARTY_NOA, calledPartyNum.getNatureOfAdrs().getCode());
				}

				/*
				 * Require to match trace criteria
				 */
				legData.set(LegDataAttributes.P_IDP_CALLED_PARTY, calledNumber);	
			}

			if (idpArg.isCalledPartyBCDNumberPresent()) {
				CalledPartyNum calledPartyNum = CalledPartyNum
						.decodeCaldParty(idpArg.getCalledPartyBCDNumber().getValue(), true);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted called number from CalledPartyBCDNumber is " + calledPartyNum);
				}

				PhoneNumber calledNumber = parseCalledPartyBCDNum(dialogueId, calledPartyNum);

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: [PH] Extracted called number from IDP is " + calledNumber);
				}

				// extra check if called party contains f as filler then remove it. 
				// coming in Telus
				if(StringUtils.containsIgnoreCase(calledNumber.getAddress(), "f")){
					String modifiedCalledParty =  StringUtils.removeEndIgnoreCase(calledNumber.getAddress(), "f");
					calledNumber.setAddress(modifiedCalledParty);					
				}
				legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);

				if(calledPartyNum.getNatureOfAdrs() != null){
					legData.set(LegDataAttributes.P_CALLED_PARTY_NOA, calledPartyNum.getNatureOfAdrs().getCode());
				}

				/*
				 * Require to match trace criteria
				 */
				legData.set(LegDataAttributes.P_IDP_CALLED_PARTY, calledNumber);
			}

		} catch (InvalidInputException e) {
			logger.error(dialogueId + ":: Error in parseIdp " + e.getMessage());
			throw new ASNParsingException(dialogueId + "::ASN Parsing Failure: IDP parsing failure occured.:", e,
					MESSAGE.IDP);
		}

		catch (ASNParsingException e) {
			logger.error(dialogueId + "::[PH] Error in parseIdp " + e.getMessage());
			throw e;
		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseIdp " + e.getMessage());
			throw new ASNParsingException(dialogueId + "::[PH] ASN Parsing Failure: IDP parsing failure occured.", e,
					MESSAGE.IDP);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Initial DP parsed successfully");
		}
	}

	/**
	 * This method is called by protocol handler for parsing Called Party BCD
	 * Number.
	 * 
	 * @param dialogueId     represents integer value of dialogue Id.
	 * @param calledPartyNum represents an instance of CalledPartyNum
	 * @return an instance of PhoneNumber
	 */
	private static PhoneNumber parseCalledPartyBCDNum(int dialogueId, CalledPartyNum calledPartyNum) {
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside parseCalledPartyBCDNum");
		}

		PhoneNumber calledNumber = new PhoneNumber();

		/*
		 * Nature Of Address
		 */
		calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);

		/*
		 * Numbering Plan Indicator
		 */
		NumPlanEnum numPlanIndEnum = calledPartyNum.getNumPlan();
		calledNumber.setNumberingPlan(PhoneNumber.NP_UNKNOWN);

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
	 * This method is called by protocol handler for parsing Called Party number.
	 * 
	 * @param dialogueId     represents integer value of dialogue Id.
	 * @param calledPartyNum represents an instance of CalledPartyNum
	 * @return an instance of PhoneNumber
	 */
	private static PhoneNumber parseCalledPartyNum(int dialogueId, CalledPartyNum calledPartyNum) {
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside parseCalledPartyNum");
		}

		PhoneNumber calledNumber = new PhoneNumber();

		/*
		 * Nature Of Address
		 */
		NatureOfAdrsEnum natureOfAddrEnum = calledPartyNum.getNatureOfAdrs();
		if ((natureOfAddrEnum == NatureOfAdrsEnum.SPARE) || (natureOfAddrEnum == NatureOfAdrsEnum.SUBS_NO)
				|| (natureOfAddrEnum == NatureOfAdrsEnum.NETWORK_NO)
				|| (natureOfAddrEnum == NatureOfAdrsEnum.NATIONAL_NO)) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == NatureOfAdrsEnum.UNKNOWN) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if (natureOfAddrEnum == NatureOfAdrsEnum.INTER_NO) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Nature of Address is " + natureOfAddrEnum.getCode());
		}

		/*
		 * Numbering Plan Indicator
		 */
		NumPlanEnum numPlanIndEnum = calledPartyNum.getNumPlan();
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
	private static PhoneNumber parseCallingPartyNum(int dialogueId, CallingPartyNum callingPartyNum) {
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside parseCallingPartyNum");
		}

		PhoneNumber callingNumber = new PhoneNumber();

		/*
		 * Nature Of Address
		 */
		NatureOfAdrsEnum natureOfAddrEnum = callingPartyNum.getNatureOfAdrs();
		if ((natureOfAddrEnum == NatureOfAdrsEnum.SPARE) || (natureOfAddrEnum == NatureOfAdrsEnum.SUBS_NO)
				|| (natureOfAddrEnum == NatureOfAdrsEnum.NETWORK_NO)
				|| (natureOfAddrEnum == NatureOfAdrsEnum.NATIONAL_NO)) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == NatureOfAdrsEnum.UNKNOWN) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if (natureOfAddrEnum == NatureOfAdrsEnum.INTER_NO) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}

		if(StringUtils.equalsIgnoreCase(Capv2ScfProtocolConfig
				.getConfigData(Capv2ScfProtocolConfig.SEND_TERMINATING_RRBCSM), "TRUE")){
			if(StringUtils.isNotBlank(callingPartyNum.getAddrSignal()) && 
					callingPartyNum.getAddrSignal().length() == 11  && 
					StringUtils.startsWith(callingPartyNum.getAddrSignal(), "1")){
				callingNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);

				if(logger.isDebugEnabled()){
					logger.debug("Overriding NOA for calling based on legth and start digit");
				}
			}
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Extracted Nature of Address is " + natureOfAddrEnum.getCode());
		}



		/*
		 * Numbering Plan Indicator
		 */
		NumPlanEnum numPlanIndEnum = callingPartyNum.getNumPlan();
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

		callingNumber.setPresentationIndicator(callingPartyNum.getAdrsPresntRestd().getCode());
		callingNumber.setScreeningIndicator(callingPartyNum.getScreening().getCode());

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

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside createRRBCSMForDisarming");
		}

		/*
		 * Monitor Mode - Transparent
		 */
		MonitorMode monitorTransparent = new MonitorMode();
		monitorTransparent.setValue(MonitorMode.EnumType.transparent);

		/*
		 * Leg Id
		 */
		LegID leg1Id = new LegID();
		leg1Id.selectSendingSideID(leg1Type);

		LinkedList<BCSMEvent> bcsmEventList = new LinkedList<BCSMEvent>();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Disarm oDisconnect");
		}

		/*
		 * oDisconnect-Leg1
		 */
		EventTypeBCSM eventTypeBCSMDisconnect = new EventTypeBCSM();
		eventTypeBCSMDisconnect.setValue(EventTypeBCSM.EnumType.oDisconnect);
		BCSMEvent bcsmEventDisconnectLeg1 = new BCSMEvent();
		bcsmEventDisconnectLeg1.setEventTypeBCSM(eventTypeBCSMDisconnect);
		bcsmEventDisconnectLeg1.setMonitorMode(monitorTransparent);
		bcsmEventDisconnectLeg1.setLegID(leg1Id);
		bcsmEventList.add(bcsmEventDisconnectLeg1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Disarm oAbandon");
		}

		/*
		 * oAbandon
		 */
		EventTypeBCSM eventTypeBCSMAbandon = new EventTypeBCSM();
		eventTypeBCSMAbandon.setValue(EventTypeBCSM.EnumType.oAbandon);
		BCSMEvent bcsmEventAbandon = new BCSMEvent();
		bcsmEventAbandon.setEventTypeBCSM(eventTypeBCSMAbandon);
		bcsmEventAbandon.setMonitorMode(monitorTransparent);
		bcsmEventList.add(bcsmEventAbandon);

		RequestReportBCSMEventArg rrbcsmEventArg = new RequestReportBCSMEventArg();
		rrbcsmEventArg.setBcsmEvents(bcsmEventList);

		callData.set(CallDataAttribute.P_ERB_SET, null);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(rrbcsmEventArg);
		opCode.add(CapV2OpCodes.REQUEST_REPORT);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
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

		Object erbTypeSetObj = Capv2CS1ScfProtocolUtil.getErbSetByApplication(callData, action.getLeg());

		Object noAnswerTimeObj = legData.get(LegDataAttributes.NP_NO_ANSWER_TIMER_DURATION);

		Set<Action.ERB_TYPE> erbTypeSet = null;

		if (erbTypeSetObj != null) {
			erbTypeSet = (Set<Action.ERB_TYPE>) (erbTypeSetObj);
		} else {

			throw new Exception("No ERB set provided to createRRBCSMForArming");
		}

		/*
		 * Monitor Mode - Notify and Continue
		 */
		MonitorMode monitorNotifyAndContinue = new MonitorMode();
		monitorNotifyAndContinue.setValue(MonitorMode.EnumType.notifyAndContinue);

		/*
		 * Monitor Mode - Interrupted
		 */
		MonitorMode monitorInterrupted = new MonitorMode();
		monitorInterrupted.setValue(MonitorMode.EnumType.interrupted);

		/*
		 * Leg Id
		 */
		LegID leg1Id = new LegID();
		leg1Id.selectSendingSideID(leg1Type);
		LegID leg2Id = new LegID();
		leg2Id.selectSendingSideID(leg2Type);

		LinkedList<BCSMEvent> bcsmEventList = new LinkedList<BCSMEvent>();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Arm oAnswer");
		}

		/*
		 * oAnswer
		 */
		EventTypeBCSM eventTypeBCSMAnswer = new EventTypeBCSM();
		eventTypeBCSMAnswer.setValue(EventTypeBCSM.EnumType.oAnswer);
		BCSMEvent bcsmEventAnswer = new BCSMEvent();
		bcsmEventAnswer.setEventTypeBCSM(eventTypeBCSMAnswer);
		bcsmEventAnswer.setMonitorMode(monitorNotifyAndContinue);
		bcsmEventList.add(bcsmEventAnswer);
		// 10406 populating set as per events in message

		erbTypeSet.add(Action.ERB_TYPE.ERB_ANSWER);

		/*
		 * oCalledPartyBusy
		 */
		if (erbTypeSet.contains(Action.ERB_TYPE.ERB_BUSY)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Arm oCalledPartyBusy");
			}

			EventTypeBCSM eventTypeBCSMBusy = new EventTypeBCSM();
			eventTypeBCSMBusy.setValue(EventTypeBCSM.EnumType.oCalledPartyBusy);
			BCSMEvent bcsmEventBusy = new BCSMEvent();
			bcsmEventBusy.setEventTypeBCSM(eventTypeBCSMBusy);
			bcsmEventBusy.setMonitorMode(monitorInterrupted);
			bcsmEventList.add(bcsmEventBusy);
			erbTypeSet.add(Action.ERB_TYPE.ERB_BUSY);
		}

		/*
		 * oNoAnswer
		 */
		if (erbTypeSet.contains(Action.ERB_TYPE.ERB_NO_ANSWER)) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Arm oNoAnswer");
			}

			EventTypeBCSM eventTypeBCSMNoAnswer = new EventTypeBCSM();
			eventTypeBCSMNoAnswer.setValue(EventTypeBCSM.EnumType.oNoAnswer);
			BCSMEvent bcsmEventNoAnswer = new BCSMEvent();
			bcsmEventNoAnswer.setEventTypeBCSM(eventTypeBCSMNoAnswer);
			bcsmEventNoAnswer.setMonitorMode(monitorInterrupted);

			if (noAnswerTimeObj != null) {

				int noAnswerTimer = (Integer) noAnswerTimeObj;

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: noAnswerTimer from service[seconds] = " + noAnswerTimer);
				}

				if (noAnswerTimer < 3) {
					noAnswerTimer = 3;
				} else if (noAnswerTimer > 175) {
					noAnswerTimer = 175;
				}

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Set no answer time " + noAnswerTimer);
				}
				DPSpecificCriteria noAnswerCriteria = new DPSpecificCriteria();
				noAnswerCriteria.selectApplicationTimer(new ApplicationTimer(noAnswerTimer));
				bcsmEventNoAnswer.setDPSpecificCriteria(noAnswerCriteria);
				bcsmEventList.add(bcsmEventNoAnswer);
				erbTypeSet.add(Action.ERB_TYPE.ERB_NO_ANSWER);
			}

		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Arm oDisconnect on leg 1");
		}

		/*
		 * oDisconnect-Leg1
		 */
		EventTypeBCSM eventTypeBCSMDisconnect = new EventTypeBCSM();
		eventTypeBCSMDisconnect.setValue(EventTypeBCSM.EnumType.oDisconnect);
		BCSMEvent bcsmEventDisconnectLeg1 = new BCSMEvent();
		bcsmEventDisconnectLeg1.setEventTypeBCSM(eventTypeBCSMDisconnect);
		bcsmEventDisconnectLeg1.setMonitorMode(monitorNotifyAndContinue);
		bcsmEventDisconnectLeg1.setLegID(leg1Id);
		bcsmEventList.add(bcsmEventDisconnectLeg1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Arm oDisconnect on leg 2");
		}

		/*
		 * oDisconnect-Leg2
		 */
		BCSMEvent bcsmEventDisconnectLeg2 = new BCSMEvent();
		bcsmEventDisconnectLeg2.setEventTypeBCSM(eventTypeBCSMDisconnect);
		bcsmEventDisconnectLeg2.setMonitorMode(monitorNotifyAndContinue);
		bcsmEventDisconnectLeg2.setLegID(leg2Id);
		bcsmEventList.add(bcsmEventDisconnectLeg2);

		erbTypeSet.add(Action.ERB_TYPE.ERB_DISCONNECT);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Arm oAbandon");
		}

		/*
		 * oAbandon
		 */
		EventTypeBCSM eventTypeBCSMAbandon = new EventTypeBCSM();
		eventTypeBCSMAbandon.setValue(EventTypeBCSM.EnumType.oAbandon);
		BCSMEvent bcsmEventAbandon = new BCSMEvent();
		bcsmEventAbandon.setEventTypeBCSM(eventTypeBCSMAbandon);
		bcsmEventAbandon.setMonitorMode(monitorNotifyAndContinue);
		bcsmEventList.add(bcsmEventAbandon);
		erbTypeSet.add(Action.ERB_TYPE.ERB_ABANDON);

		RequestReportBCSMEventArg rrbcsmEventArg = new RequestReportBCSMEventArg();
		rrbcsmEventArg.setBcsmEvents(bcsmEventList);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(rrbcsmEventArg);
		opCode.add(CapV2OpCodes.REQUEST_REPORT);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler for creating CAP CONNECT message
	 * for HandOff.
	 * 
	 * @param callData  represents an instance of CallData
	 * @param localAddr represents an instance of SccpUserAddress
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createConnectForHandoff(CallData callData, SccpUserAddress localAddr) throws Exception {

		/*
		 * int dialogueId = (Integer)
		 * callData.getPersistableData(CallDataAttribute.P_DIALOG_ID);
		 * 
		 * if (logger.isDebugEnabled()) {
		 * logger.debug("[PH]:: Inside createConnectForTerm"); }
		 * 
		 * PhoneNumber destinationNumber = (PhoneNumber)
		 * callData.getPersistableData(CallDataAttribute.P_DESTINATION_NUMBER); int
		 * corrId = (Integer)
		 * callData.getPersistableData(CallDataAttribute.P_CORRELATION_ID); int
		 * clusterNum = (Integer)
		 * callData.getPersistableData(CallDataAttribute.P_CLUSTER_NUMBER);
		 * 
		 * if (logger.isDebugEnabled()) { logger.debug(dialogueId +
		 * ":: Inside createConnectForHandoff"); }
		 * 
		 * String correlationNumber = PhConstants.CORRELATION_NO_FLEX_CHARGING;
		 * 
		 * // Called Party Number String addressDigits = PhConstants.CORRELATION_PREFIX
		 * + PhConstants.CORRELATION_HANDOFF + clusterNum + correlationNumber + corrId;
		 * 
		 * if (logger.isDebugEnabled()) { logger.debug(dialogueId + ":: addressDigits "
		 * + addressDigits); }
		 * 
		 * byte[] calledPartyNum = CalledPartyNum.encodeCaldParty(addressDigits,
		 * NatureOfAddEnum.ASSIST_SSPIP_ROUTE_ADDR, NumPlanEnum.ISDN_NP,
		 * IntNtwrkNumEnum.ROUTING_ALLWD);
		 * 
		 * CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
		 * calledPartyNumber.setValue(calledPartyNum);
		 * 
		 * // Destination Routing Address DestinationRoutingAddress destRoutingAddress =
		 * new DestinationRoutingAddress(); destRoutingAddress.initValue();
		 * destRoutingAddress.add(calledPartyNumber);
		 * 
		 * ConnectArg connectArg = new ConnectArg();
		 * connectArg.setDestinationRoutingAddress(destRoutingAddress);
		 * 
		 * /* correlationID coding scheme should be odd or even depending on length of
		 * corrID
		 */
		/*
		 * String corrIdStr= String.valueOf(corrId); EncodingSchemeEnum encodeSchemeEnum
		 * =null; if(corrIdStr.length()%2 == 0){ //for even lenth
		 * encodeSchemeEnum=EncodingSchemeEnum.BCD_EVEN; }else{
		 * encodeSchemeEnum=EncodingSchemeEnum.BCD_ODD; }
		 * 
		 * byte[] correlationIdByteArr = GenericDigits.encodeGenericDigits(
		 * encodeSchemeEnum, DigitCatEnum.CORRELATION_ID, corrIdStr); Digits
		 * correlationIdDigits = new Digits(correlationIdByteArr);
		 * 
		 * CorrelationID correlationId = new CorrelationID();
		 * correlationId.setValue(correlationIdDigits);
		 * connectArg.setCorrelationID(correlationId);
		 * 
		 * if (logger.isDebugEnabled()) { logger.debug(dialogueId +
		 * ":: correlationId is " + correlationId); }
		 * 
		 * /* scfID
		 */
		/*
		 * byte[] scfIdByteArr = ScfId.encodeScfId(SPCIndicatorEnum.SPC_PRESENT,
		 * SSNIndicatorEnum.SSN_PRESENT, GTIndicatorEnum.NO_GT,
		 * RoutingIndicatorEnum.ROUTING_PC_SSN, localAddr.getSubSystemAddress()
		 * .getSignalingPointCode().getZone(), localAddr.getSubSystemAddress()
		 * .getSignalingPointCode().getCluster(), localAddr.getSubSystemAddress()
		 * .getSignalingPointCode().getMember(), localAddr.getSubSystemAddress()
		 * .getSubSystemNumber()); com.agnity.inapitutcs2.asngenerated.ScfID scfId = new
		 * com.agnity.inapitutcs2.asngenerated.ScfID( scfIdByteArr);
		 * connectArg.setScfID(scfId);
		 * 
		 * if (logger.isDebugEnabled()) { logger.debug(dialogueId + ":: scfId is " +
		 * scfId); }
		 * 
		 * 
		 * boolean allowedCdlNoPresent = false; Boolean calledNoOverriding = null; if
		 * (destinationNumber != null) { allowedCdlNoPresent =
		 * (destinationNumber.getPresentationIndicator() == 1) ? true : false;
		 * calledNoOverriding = (destinationNumber.getNumberOverriding() == 0) ? false :
		 * null; }
		 * 
		 * ServiceInteractionIndicatorsTwo serviceInterIndTwo = new
		 * ServiceInteractionIndicatorsTwo();
		 * serviceInterIndTwo.setAllowCdINNoPresentationInd(allowedCdlNoPresent);
		 * connectArg.setServiceInteractionIndicatorsTwo(serviceInterIndTwo); if
		 * (logger.isDebugEnabled()) { logger.debug(dialogueId +
		 * " :: allowedCdlNoPresent=" + allowedCdlNoPresent + ", calledNoOverriding=" +
		 * calledNoOverriding); }
		 * 
		 * LinkedList<Object> operationObjs = new LinkedList<Object>();
		 * LinkedList<String> opCode = new LinkedList<String>();
		 * operationObjs.add(connectArg); opCode.add(CapV2OpCodes.CONNECT);
		 * 
		 * LinkedList<byte[]> encodeList = CapV2OperationsCoding
		 * .encodeOperations(operationObjs, opCode,true); return encodeList.getFirst();
		 */

		return null;
	}

	/**
	 * This method is called by protocol handler for creating CAP CONNECT message
	 * for Port.
	 * 
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createConnectForPort(CallData callData) throws Exception {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectForPort");
		}

		PhoneNumber destinationNumber = (PhoneNumber) callData.get(CallDataAttribute.P_DESTINATION_NUMBER);

		NatureOfAdrsEnum natureOfAddrEnum = NatureOfAdrsEnum.fromInt(destinationNumber.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = NatureOfAdrsEnum.NATIONAL_NO;
		}

		Object calledType = callData.get(CallDataAttribute.P_CALLED_PARTY_TYPE);

		byte[] calledPartyNum = CalledPartyNum.encodeCaldParty(destinationNumber.getAddress(), natureOfAddrEnum,
				NumPlanEnum.ISDN_NP, IntNtwrkNumEnum.ROUTING_ALLWD);

		CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
		calledPartyNumber.setValue(calledPartyNum);
		DestinationRoutingAddress destRoutingAddress = new DestinationRoutingAddress();
		destRoutingAddress.initValue();
		destRoutingAddress.add(calledPartyNumber);

		ConnectArg connectArg = new ConnectArg();
		connectArg.setDestinationRoutingAddress(destRoutingAddress);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(connectArg);
		opCode.add(CapV2OpCodes.CONNECT);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by the protocol handler for creating CAP CONNECT
	 * signal.
	 * 
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createConnectForTerm(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectForTerm");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		PhoneNumber destinationNumber = (PhoneNumber) legData.get(LegDataAttributes.P_DESTINATION_NUMBER);

		NatureOfAdrsEnum natureOfAddrEnum = NatureOfAdrsEnum.fromInt(destinationNumber.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = NatureOfAdrsEnum.NATIONAL_NO;
		}

		// check if NOA is set to be inserted, APplication may send 1 or 4 (actual value)
		String noaInt = (String) legData.get(LegDataAttributes.P_DESTINATION_NUMBER_NOA_INT);
		if(StringUtils.isNotBlank(noaInt) && (StringUtils.equalsIgnoreCase(noaInt, "1") 
				|| StringUtils.equalsIgnoreCase(noaInt, "4"))){
			natureOfAddrEnum = NatureOfAdrsEnum.INTER_NO;

			if(logger.isDebugEnabled()){
				logger.debug("NOA of Destination number is set to International:4");
			}
		}

		NumPlanEnum numberPlan = NumPlanEnum.fromInt(destinationNumber.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		byte[] calledPartyNum = CalledPartyNum.encodeCaldParty(destinationNumber.getAddress(), natureOfAddrEnum,
				numberPlan, IntNtwrkNumEnum.ROUTING_ALLWD);

		CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
		calledPartyNumber.setValue(calledPartyNum);
		DestinationRoutingAddress destRoutingAddress = new DestinationRoutingAddress();
		destRoutingAddress.initValue();
		destRoutingAddress.add(calledPartyNumber);

		ConnectArg connectArg = new ConnectArg();
		connectArg.setDestinationRoutingAddress(destRoutingAddress);

		if(legData.get(LegDataAttributes.P_REDIRECTING_PARTY_ID) != null){
			PhoneNumber redirPartyId = (PhoneNumber) legData.get(LegDataAttributes.P_REDIRECTING_PARTY_ID);

			byte[] redirByteArray = CalledPartyNum.encodeCaldParty(redirPartyId.getAddress(), natureOfAddrEnum,
					numberPlan, IntNtwrkNumEnum.ROUTING_ALLWD);

			RedirectingPartyID redirectingPartyId = new RedirectingPartyID();
			redirectingPartyId.setValue(redirByteArray);

			connectArg.setRedirectingPartyID(redirectingPartyId);

			if(logger.isDebugEnabled()){
				logger.debug(dialogueId + " setting RedirectingPartyID :" 
						+ CommonUtils.formatBytes(redirByteArray));
			}
		}

		// Redirection Info is 2 Octet defined in Q.763. 
		// First octet 
		// H  G  F  E  D  C  B   A
		// |---ORR--|  S  |--RI--|
		// |--RR----|     |--RC--|
		// where ORR - Original Redirection Reason, S - Spare, RI - Redirection Indicator
		// RC - Redirection Counter, RR - Redirecting Reason
		if(legData.get(LegDataAttributes.P_REDIRECTION_INFO) != null){
			String value = (String)legData.get(LegDataAttributes.P_REDIRECTION_INFO);

			// setting redirecting indicator as call divert - by default. 
			// and count as 1.
			byte[] redirectionByte = { 0x03, 0x01};

			RedirectionInformation reinfo = new RedirectionInformation();
			reinfo.setValue(redirectionByte);
			connectArg.setRedirectionInformation(reinfo);

			if(logger.isDebugEnabled()){
				logger.debug(dialogueId + " setting RedirectionInfo :" 
						+ CommonUtils.formatBytes(redirectionByte));
			}
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(CapV2OpCodes.CONNECT);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectArg);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler for parsing ERBCSM event.
	 * 
	 * @param eventReportBCSMArg represents an instance of EventReportBCSMArg
	 * @param callData           represents an instance of CallData
	 * @throws Exception
	 */
	public static void parseErbcsm(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		TcapSession tcapSession = PhUtilityServices.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID))
				.getTcapProvider().getTcapSession(dialogueId);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);	
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);	

		CapV2CallStates callstate = (CapV2CallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] Extract ERBCSM Argument from ERB");
		}
		EventReportBCSMArg eventReportBCSMArg;
		try {
			eventReportBCSMArg = (EventReportBCSMArg) CapV2OperationsCoding.decodeOperation(invokeIndEvent, true);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding ERB " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in CapV2OperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] IDP parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding ERB " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse ERB", e);
			}
			throw new ASNParsingException(dialogueId + ":: [PH] ASN Parsing Failure, ERB parsing failure occured.", e,
					MESSAGE.ERB);
		}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside parseErbcsm");
		}

		com.agnity.camelv2.asngenerated.Cause releaseCause = null;
		com.agnity.camelv2.asngenerated.Cause failureCause = null;

		Event event = null;
		ServiceInterface serviceInterface = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

		if (eventReportBCSMArg.isEventSpecificInformationBCSMPresent()
				|| eventReportBCSMArg.getEventTypeBCSM() != null) {

			EventTypeBCSM eventTypeBCSM = eventReportBCSMArg.getEventTypeBCSM();
			EventSpecificInformationBCSM eventSpecificInfo = eventReportBCSMArg.getEventSpecificInformationBCSM();

			switch (eventTypeBCSM.getValue()) {
			case oAbandon: 
			case tAbandon:{

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Inside parseErbcsm OAbandan Received");
				}

				// In case of OCS, it is possible to get ACR + ERBCSM in single dialogue. 
				// Check previous call state and handle it accordingly 
				String capv2Flow = Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW);

				if (capv2Flow != null && StringUtils.equalsIgnoreCase(capv2Flow, "OCS")) {
					if(callstate == CapV2CallStates.USER_INTREACTION_IN_PROGRESS){
						if(logger.isDebugEnabled()){
							logger.debug("ERBCAM - oAbandon: Last Operation was Play");
						}
						event = new Event(EventType.EVENT_PLAY_SUCCESS, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG1.name());
						legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.PLAY_SUCCSS);
					}
				}

				// If event is null then send disconnect to application
				if(event == null){
					event = new Event(EventType.EVENT_DISCONNECT, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());
					legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERMINATED);
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.ERB_OABANDON_RCVD);
				}
			}
			break;
			case oDisconnect: {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Inside parseErbcsm oDisconnect Received");
				}
				if (eventSpecificInfo != null
						&& eventSpecificInfo.getODisconnectSpecificInfo().isReleaseCausePresent()) {
					releaseCause = eventSpecificInfo.getODisconnectSpecificInfo().getReleaseCause();
					parseReleaseCause(releaseCause, callData);
				}

				legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERMINATED);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.ERB_ODISC_RCVD);


				// In case of OCS, it is possible to get ACR + ERBCSM in single dialogue. 
				// Check previous call state and handle it accordingly 
				String capv2Flow = Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW);

				if (capv2Flow != null && StringUtils.equalsIgnoreCase(capv2Flow, "OCS")) {
					if(callstate == CapV2CallStates.TERM_CONNECTED_ACR){
						if(logger.isDebugEnabled()){
							logger.debug("ERBCAM - oDisconnected- OCS: Last Operation was ACR, raising EVENT_APPLY_CHG_REPORT");
						}
						event = new Event(EventType.EVENT_APPLY_CHG_REPORT, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());
						legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERMINATED_ACR);
					}else{
						event = new Event(EventType.EVENT_DISCONNECT, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());
					}
				}else{
					event = new Event(EventType.EVENT_DISCONNECT, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());

					if(logger.isDebugEnabled()){
						logger.debug("ERBCAM - oDisconnected- EVENT_DISCONNECT");
					}
				}
			}
			break;

			case oCalledPartyBusy: {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Inside parseErbcsm oCalledPartyBusy Received");
				}

				event = new Event(EventType.EVENT_FAILURE, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());
				parseBusy(callData, eventReportBCSMArg, tcapSession);

			}
			break;

			case tAnswer:
			case oAnswer: {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Inside parseErbcsm oAnswer Received");
				}

				event = new Event(EventType.EVENT_SUCCESS, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());
				parseAns(callData, eventReportBCSMArg, tcapSession);

			}
			break;
			case tNoAnswer:
			case oNoAnswer: {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Inside parseErbcsm oNoAnswer Received");
				}

				event = new Event(EventType.EVENT_FAILURE, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());
				parseNoAns(callData, eventReportBCSMArg, tcapSession);
			}
			break;

			case routeSelectFailure: {

				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Inside parseErbcsm routeSelectFailure Received");
				}

				if (eventSpecificInfo != null) {
					failureCause = eventSpecificInfo.getRouteSelectFailureSpecificInfo().getFailureCause();
					parseReleaseCause(failureCause, callData);
				}

				event = new Event(EventType.EVENT_FAILURE, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());
				parseRouteSelectFailure(callData, eventReportBCSMArg, tcapSession);
			}
			break;
			case collectedInfo:
			case tBusy:
			case tDisconnect:
			case termAttemptAuthorized:
				leg2Data.set(LegDataAttributes.P_CAUSE_CODE, Capv2ScfRelReasonCode.UNEXP_ERB_ORIG_AUTH);
				event = new Event(EventType.EVENT_FAILURE, Protocol.CAPV2_SCF, CallDataAttribute.P_LEG2.name());
				break;
			}

			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Inside parseErbcsm Notify Application of ERB Event");
			}

			// Notify application of disconnect event	
			if(event!=null){
				ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
			}
		}
	}

	/**
	 * This method is called by the Protocol handler whenever an Erb Route Select
	 * failure event is received.
	 * 
	 * @param callData
	 * 
	 * @param erbcsmArg   represents the instance of EventReportBCSMArg
	 * @param tcapSession represents the instance of TcapSession
	 * @return represents the instance of TcapSession
	 * @throws ASNParsingException
	 */

	private static Action[] parseRouteSelectFailure(CallData callData, EventReportBCSMArg erbcsmArg,
			TcapSession tcapSession) throws ASNParsingException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside parseRouteSelectFailure");
		}

		if (erbcsmArg.isLegIDPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Leg Id Present");
			}

			/*
			 * checking Route Select leg ID is receiving side is and matches leg 2
			 */
			ReceivingSideID legId = erbcsmArg.getLegID();
			if (legId.isReceivingSideIDSelected()) {
				LegType legType = legId.getReceivingSideID();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: got recieving side ID::" + legType.getValue()[0]);
				}
				/*
				 * matching only byte at index 0 since valid values as per standard are 1 and 2
				 */

				if ((legType.getValue()[0]) != (Capv2ScfProtocolParser.leg2Type.getValue()[0])) {
					logger.error(dialogueId + ":: Invalid leg type in RouteSelectFailure ");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.ERB_INVALID_LEG_TYPE);
					return Capv2ScfProtocolHelper.getASNParsingFailureAction(tcapSession,
							MESSAGE.ERB_ROUTESELECTFAILURE, FAILTYPE.DEFAULT);

				} // end if legType value check
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Valid leg ID in RouteSelectFailure message");
				}
			} else {
				logger.error(dialogueId + ":: Invalid side Only ReceivingSideID expected in busy");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.ERB_INVALID_RECEIVING_SIDE);
				return Capv2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_ROUTESELECTFAILURE,
						FAILTYPE.DEFAULT);
			} // @end if isReceivingSideIDSelected
		} // @end if erbcsmArg.isLegIDPresent() no else as standard says assume default as
		// 2

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.ROUTESELECTFAILURE_RCVD_FROM_TERM);
		return null;

	}

	/**
	 * This method is called by protocol handler for parsing releaseCause.
	 * 
	 * @param releaseCause represents an instance of
	 *                     com.agnity.inapitutcs2.asngenerated.Cause
	 * @param callData     represents an instance of CallData
	 * @throws InvalidInputException
	 * @throws EnumParamOutOfRangeException
	 */

	public static void parseReleaseCause(com.agnity.camelv2.asngenerated.Cause releaseCause, CallData callData)
			throws InvalidInputException, EnumParamOutOfRangeException {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside parseReleaseCause");
		}

		if (releaseCause != null) {
			CauseDataType cause = CauseDataType.decodeCauseVal(releaseCause.getValue());
			if(cause.getCauseValEnum()!=null){
				callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, cause.getCauseValEnum().getCode());
			}
			callData.set(CallDataAttribute.P_CAUSE_VALUE_FROM_MSG, PhConstants.TRUE);
		}

	}

	/**
	 * This method is called by the Protocol handler whenever an ErbBusy event is
	 * received.
	 * 
	 * @param erbcsmArg   represents the instance of EventReportBCSMArg
	 * @param tcapSession represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] parseBusy(CallData callData, EventReportBCSMArg erbcsmArg, TcapSession tcapSession)
			throws ASNParsingException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside process ErbBusy");
		}

		if (erbcsmArg.isLegIDPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Leg Id Present");
			}

			/*
			 * checking oBusy leg ID is receiving side is and matches leg 2
			 */
			ReceivingSideID legId = erbcsmArg.getLegID();
			if (legId.isReceivingSideIDSelected()) {
				LegType legType = legId.getReceivingSideID();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: got recieving side ID::" + legType.getValue()[0]);
				}
				/*
				 * matching only byte at index 0 since valid values as per standard are 1 and 2
				 */
				if ((legType.getValue()[0]) != (Capv2ScfProtocolParser.leg2Type.getValue()[0])) {
					logger.error(dialogueId + ":: Invalid leg type in ERB busy");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.ERB_INVALID_LEG_TYPE);
					return Capv2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_BUSY,
							FAILTYPE.DEFAULT);

				} // end if legType value check
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Valid leg ID in oBusy message");
				}
			} else {
				logger.error(dialogueId + ":: Invalid side Only ReceivingSideID expected in busy");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.ERB_INVALID_RECEIVING_SIDE);
				return Capv2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_BUSY,
						FAILTYPE.DEFAULT);
			} // @end if isReceivingSideIDSelected
		} // @end if erbcsmArg.isLegIDPresent() no else as standard says assume default as
		// 2

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.BUSY_RCVD_FROM_TERM);
		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever an ErbNoAnswer event
	 * is received.
	 * 
	 * @param erbcsmArg   represents the instance of EventReportBCSMArg
	 * @param tcapSession represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] parseNoAns(CallData callData, EventReportBCSMArg erbcsmArg, TcapSession tcapSession)
			throws ASNParsingException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside process ErbNoAnswer");
		}
		if (erbcsmArg.isLegIDPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Leg Id Present");
			}

			/*
			 * checking no Ans leg ID is receiving side is and matches leg 2
			 */
			ReceivingSideID legId = erbcsmArg.getLegID();
			if (legId.isReceivingSideIDSelected()) {
				LegType legType = legId.getReceivingSideID();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: got recieving side ID::" + legType.getValue()[0]);
				}
				/*
				 * matching only byte at index 0 since valid values as per standard are 1 and 2
				 */

				if ((legType.getValue()[0]) != (Capv2ScfProtocolParser.leg2Type.getValue()[0])) {
					logger.error(dialogueId + ":: Invalid leg type in ERB NoAns");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.ERB_INVALID_LEG_TYPE);
					return Capv2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_NOANS,
							FAILTYPE.DEFAULT);

				} // end if legType value check
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Valid leg ID in oNoAns message");
				}
			} else {
				logger.error(dialogueId + ":: Invalid side Only ReceivingSideID expected in NoAns");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.ERB_INVALID_RECEIVING_SIDE);
				return Capv2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_NOANS,
						FAILTYPE.DEFAULT);
			} // @end if isReceivingSideIDSelected
		} // @end if erbcsmArg.isLegIDPresent() no else as standard says assume default as
		// 2

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.NOANS_RCVD_FROM_TERM);
		return null;

	}

	/**
	 * This method is called by the Protocol handler whenever an ErbAnswer event is
	 * received.
	 * 
	 * @param erbcsmArg   represents the instance of EventReportBCSMArg
	 * @param tcapSession represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] parseAns(CallData callData, EventReportBCSMArg erbcsmArg, TcapSession tcapSession)
			throws ASNParsingException {

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside process ErbAnswer");
		}

		if (erbcsmArg.isLegIDPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug(dialogueId + ":: Leg Id Present");
			}

			/*
			 * checking oAnswer leg ID is receiving side is and matches leg 2
			 */
			ReceivingSideID legId = erbcsmArg.getLegID();
			if (legId.isReceivingSideIDSelected()) {
				LegType legType = legId.getReceivingSideID();
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: got recieving side ID::" + legType.getValue()[0]);
				}
				/*
				 * matching only byte at index 0 since valid values as per standard are 1 and 2
				 */

				if ((legType.getValue()[0]) != (Capv2ScfProtocolParser.leg2Type.getValue()[0])) {
					logger.error(dialogueId + ":: Invalid leg type in ERB oAnswer");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, Capv2ScfRelReasonCode.ERB_INVALID_LEG_TYPE);
					return Capv2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_ANS,
							FAILTYPE.DEFAULT);

				} // end if legType value check
				if (logger.isDebugEnabled()) {
					logger.debug(dialogueId + ":: Valid leg ID in oAns message");
				}
			} else {
				logger.error(dialogueId + ":: Invalid side Only ReceivingSideID expected oAns");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE,
						Capv2ScfRelReasonCode.ERB_INVALID_RECEIVING_SIDE);
				return Capv2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_ANS,
						FAILTYPE.DEFAULT);
			} // @end if isReceivingSideIDSelected
		} // @end if erbcsmArg.isLegIDPresent() no else as standard says assume default as
		// 2

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERM_CONNECTED);
		callData.set(CallDataAttribute.P_CALL_CONNECT_TIME, new Date());

		// alterting Duration
		Date startTime = (Date) callData.get(CallDataAttribute.P_CALL_START_TIME);
		Date answerTime = (Date) callData.get(CallDataAttribute.P_CALL_CONNECT_TIME);
		long diffSeconds = (answerTime.getTime() - startTime.getTime())/ 1000 ;
		callData.set(CallDataAttribute.P_ALERTING_DURATION, Long.toString(diffSeconds));

		Capv2CS1ScfProtocolUtil.startCdrTimer(tcapSession);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Set attempted indicator to 0");
		}

		callData.set(CallDataAttribute.P_ATTEMPTED_IND, 0);

		return null;

	}

	/**
	 * This method is called by protocol handler for creating a release call
	 * message.
	 * 
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createReleaseCall(CallData callData, Action action) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		Object releaseCauseValueObj = callData.get(CallDataAttribute.NP_RELEASE_REASON_VALUE);

		int releaseCauseValue = 31;
		try{
			releaseCauseValue= Integer.parseInt(releaseCauseValueObj.toString());
		}catch(Exception ex){}

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside createReleaseCall");
			logger.debug(dialogueId + ":: Release Cause Value is " + releaseCauseValue);
		}

		byte[] causeByteArr = CauseDataType.encodeCauseVal(LocationEnum.TRANSIT_NETWORK,
				CodingStndEnum.ITUT_STANDARDIZED_CODING, CauseValEnum.fromInt(releaseCauseValue));

		com.agnity.camelv2.asngenerated.Cause causeValue = new com.agnity.camelv2.asngenerated.Cause(causeByteArr);

		ReleaseCallArg releaseCallArg = new ReleaseCallArg();
		releaseCallArg.setValue(causeValue);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(releaseCallArg);
		opCode.add(CapV2OpCodes.RELEASE_CALL);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler to parse ER signal received from
	 * switch.
	 * 
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param callData       represents an instance of CallData
	 * @throws Exception
	 */
	public static void parseEntityRelease(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {
		/*
		 * int dialogueId = (Integer)
		 * callData.getPersistableData(CallDataAttribute.P_DIALOG_ID);
		 * 
		 * if (logger.isDebugEnabled()) { logger.debug(dialogueId +
		 * ":: Inside parseEntity Release"); }
		 * 
		 * EntityReleasedArg erArg = null;
		 * 
		 * erArg = (EntityReleasedArg)
		 * CapV2OperationsCoding.decodeOperation(invokeIndEvent);
		 * 
		 * byte[] reason = null; if (erArg.isCSFailureSelected() &&
		 * erArg.getCSFailure().isReasonPresent()) { reason =
		 * erArg.getCSFailure().getReason().getValue();
		 * com.agnity.inapitutcs2.datatypes.Reason decodedReason=
		 * com.agnity.inapitutcs2.datatypes.Reason.decodeReason(reason);
		 * if(decodedReason.getReasonEnum() == ReasonEnum.NOT_USED){
		 * logger.error(dialogueId + ":: Invalid ER reason"); throw new
		 * ASNParsingException(dialogueId+":: Invalid ER Reason::"+decodedReason.
		 * getReasonEnum(),MESSAGE.DEFAULT); } }
		 * 
		 * com.agnity.inapitutcs2.asngenerated.Cause releaseCause = null; if
		 * (erArg.isCSFailureSelected() && erArg.getCSFailure().isCausePresent()) {
		 * releaseCause = erArg.getCSFailure().getCause();
		 * 
		 * } parseReleaseCause(releaseCause, callData);
		 */
	}

	/**
	 * This method is called by protocol handler for creating CAP ETC message.
	 * 
	 * @param callData  represents an instance of CallData
	 * @param localAddr represents an instance of SccpUserAddress
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createEtc(CallData callData, SccpUserAddress localAddr) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Inside createEtc");
		}

		/*
		 * assistingSSPIPRoutingAddress
		 */

		String assistSspIpRoutAddrStr = PhConstants.CORRELATION_PREFIX + PhConstants.CORRELATION_ASSIST
				+ Capv2ScfProtocolConfig.getConfigData(Capv2ScfProtocolConfig.CLUSTER_NUMBER)
				+ PhConstants.CORRELATION_NO_FLEX_CHARGING + callData.get(CallDataAttribute.P_CORRELATION_ID);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Assisting SSP IP Routing Address formed as " + assistSspIpRoutAddrStr);
		}

		/*
		 * byte[] assistSspIpRoutAddrByteArr =
		 * com.agnity.camelv2.datatypes.GenericNumDataType
		 * .encodeGenericNum(NumQualifierIndEnum.RESERVED, assistSspIpRoutAddrStr,
		 * NatureOfAddEnum.ASSIST_SSPIP_ROUTE_ADDR, NumPlanEnum.ISDN_NP,
		 * AdrsPrsntRestdEnum.PRSNT_RESTD, ScreeningIndEnum.NETWORK_PROVD,
		 * NumIncmpltEnum.COMPLETE);
		 */
		byte[] assistSspIpRoutAddrByteArr = com.agnity.camelv2.datatypes.GenericNumDataType.encodeGenericNum(
				NumQualifierIndEnum.SPARE, // RESERVED
				assistSspIpRoutAddrStr, NatureOfAdrsEnum.SPARE, NumPlanEnum.ISDN_NP, AdrsPrsntRestdEnum.PRSNT_RESTD,
				ScreeningIndEnum.NETWORK_PROVD, NumINcomplteEnum.COMPLETE);
		Digits assistSspIpRoutAddrDigits = new Digits(assistSspIpRoutAddrByteArr);
		AssistingSSPIPRoutingAddress assistSspIpRoutAddr = new AssistingSSPIPRoutingAddress();
		assistSspIpRoutAddr.setValue(assistSspIpRoutAddrDigits);

		EstablishTemporaryConnectionArg etcArg = new EstablishTemporaryConnectionArg();
		etcArg.setAssistingSSPIPRoutingAddress(assistSspIpRoutAddr);

		/*
		 * correlationID
		 */
		/*
		 * coding scheme should be odd or even decreateEtcpending on length of corrID
		 */
		String corrId = String.valueOf(callData.get(CallDataAttribute.P_CORRELATION_ID));
		EncodingSchemeEnum encodeSchemeEnum = null;
		if (corrId.length() % 2 == 0) {
			// for even lenth
			encodeSchemeEnum = EncodingSchemeEnum.BCD_EVEN;
		} else {
			encodeSchemeEnum = EncodingSchemeEnum.BCD_ODD;
		}

		byte[] correlationIdByteArr = GenericDigitsDataType.encodeGenericDigits(encodeSchemeEnum, corrId);
		Digits correlationIdDigits = new Digits(correlationIdByteArr);

		CorrelationID correlationId = new CorrelationID();
		correlationId.setValue(correlationIdDigits);
		etcArg.setCorrelationID(correlationId);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] correlationId is " + correlationId);
		}

		/*
		 * scfID
		 */
		byte[] scfIdByteArr = com.agnity.camelv2.datatypes.ScfId.encodeScfId(SPCIndicatorEnum.SPC_PRESENT,
				SSNIndicatorEnum.SSN_PRESENT, GTIndicatorEnum.NO_GT, RoutingIndicatorEnum.ROUTING_PC_SSN,
				localAddr.getSubSystemAddress().getSignalingPointCode().getZone(),
				localAddr.getSubSystemAddress().getSignalingPointCode().getCluster(),
				localAddr.getSubSystemAddress().getSignalingPointCode().getMember(),
				localAddr.getSubSystemAddress().getSubSystemNumber());
		ScfID scfId = new ScfID(scfIdByteArr);
		etcArg.setScfID(scfId);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] scfId is " + scfId);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(etcArg);
		opCode.add(CapV2OpCodes.ESTABLISH_TEMP_CONNECTION);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
		return encodeList.getFirst();
	}

	/**
	 * This Method is used to encode Furnish charging information
	 * 
	 * @param callData
	 * @return Byte array containing Furnish charging information
	 * @throws Exception
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static byte[] createFci(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside createFciOld");
		}

		FurnishChargingInformationArg furnishChargingInformationArg = new FurnishChargingInformationArg();
		FCIBillingChargingCharacteristics fCharacteristics = new FCIBillingChargingCharacteristics();

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		String fciChrInfoStr = (String) legData.get(LegDataAttributes.P_FCI_BILLING_CHARACTERISTICS);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "[PH]:: Inside createFciOld:: fci dump=" + fciChrInfoStr);
		}
		byte[] fciChrInfo = fciChrInfoStr.getBytes("UTF-8");
		fCharacteristics.setValue(fciChrInfo);
		furnishChargingInformationArg.setValue(fCharacteristics);

		LinkedList opObjects = new LinkedList();
		opObjects.add(furnishChargingInformationArg);
		LinkedList opCodes = new LinkedList();
		opCodes.add(CapV2OpCodes.FURNISH_CHARGING_INFORMATION);
		LinkedList<byte[]> fciLinkList = CapV2OperationsCoding.encodeOperations(opObjects, opCodes, true);
		return fciLinkList.getFirst();
	}

	/**
	 * This Method is used to parse ACR event received from network
	 * 
	 * @param invokeIndEvent
	 * @param callData
	 * @throws InvalidInputException
	 * @throws EnumParamOutOfRangeException
	 * @throws ASNParsingException
	 * @throws ParameterOutOfRangeException
	 */
	public static void parseAcr(InvokeIndEvent invokeIndEvent, CallData callData) throws InvalidInputException,
	EnumParamOutOfRangeException, ASNParsingException, ParameterOutOfRangeException {
		byte[] callResult = null;
		CAMEL_CallResult camelCallResult = null;
		ApplyChargingReportArg applyChargingReportArg = null;
		TimeInformation timeInformation = null;
		Integer timeIfNoTariffSwitch = null;
		Integer timeIfTariffSwitch = null;
		boolean isLegActive = false;
		LegType legType = null;

		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: Inside parseAcr");
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + ":: [PH] Extract ACR Argument from ACR");
		}

		ApplyChargingReportArg acr = null;

		try {
			acr = (ApplyChargingReportArg)CapV2OperationsCoding.decodeOperation(invokeIndEvent, true);
			if(acr != null){
				CallResult ar = acr.getValue();
				if(logger.isDebugEnabled()){
					logger.debug("CallResult Bytes:"+CommonUtils.formatBytes(ar.getValue()));
				}

				camelCallResult = (CAMEL_CallResult) CapV2OperationsCoding
						.decodeOperationsForOpCode(ar.getValue(), CapV2OpCodes.CAMEL_CALL_RESULT, true);

				// print value sof camelCallResult
				if(camelCallResult != null){
					TimeDurationChargingResultSequenceType timeDurChargeResult = null;
					if(camelCallResult.isTimeDurationChargingResultSelected()){
						timeDurChargeResult = camelCallResult.getTimeDurationChargingResult();
					}

					//					CAMEL-CallResult {PARAMETERS-BOUND : bound} ::= CHOICE {
					//						 timeDurationChargingResult [0] SEQUENCE {
					//						 partyToCharge [0] ReceivingSideID,
					//						 timeInformation [1] TimeInformation,
					//						 legActive [2] BOOLEAN DEFAULT TRUE,
					//						 callLegReleasedAtTcpExpiry [3] NULL OPTIONAL,
					//						 extensions [4] Extensions {bound} OPTIONAL,
					//						 aChChargingAddress [5] AChChargingAddress {bound}
					//						 DEFAULT legID:receivingSideID:leg1,
					//						 ...
					//						 }
					//						 } 

					//					TimeInformation ::= CHOICE {
					//						 timeIfNoTariffSwitch [0] TimeIfNoTariffSwitch,
					//						 timeIfTariffSwitch [1] TimeIfTariffSwitch
					//						 } 

					// party To Charge 

					if(timeDurChargeResult != null){
						if(timeDurChargeResult.getPartyToCharge().isReceivingSideIDSelected()){
							legType = timeDurChargeResult.getPartyToCharge().getReceivingSideID();
						}

						if(timeDurChargeResult.getTimeInformation() != null){
							timeInformation = timeDurChargeResult.getTimeInformation();

							if(timeInformation.isTimeIfNoTariffSwitchSelected()){
								timeIfNoTariffSwitch = timeInformation.getTimeIfNoTariffSwitch().getValue();

								legData.set(LegDataAttributes.P_CAP_ACR_TIME_IF_NO_TRAFFIC_SWITCH,
										Integer.toString(timeIfNoTariffSwitch));
							}

							if(timeInformation.isTimeIfTariffSwitchSelected()){
								timeIfTariffSwitch = timeInformation.getTimeIfTariffSwitch().getTariffSwitchInterval();
								legData.set(LegDataAttributes.P_CAP_ACR_TIME_IF_TRAFFIC_SWITCH,
										Integer.toString(timeIfTariffSwitch));
							}
						}

						if(timeDurChargeResult.getCallActive()){
							isLegActive = true;
							legData.set(LegDataAttributes.P_CAP_ACR_IS_LEG_ACTIVE, "1");
						}else{
							legData.set(LegDataAttributes.P_CAP_ACR_IS_LEG_ACTIVE, "0");
							
							callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());
						}
						
						if (legType.equals(leg1Type))
							legData.set(LegDataAttributes.P_CAP_PARTY_TO_CHARGE,
									"1");
						else {
							legData.set(LegDataAttributes.P_CAP_PARTY_TO_CHARGE,
									"2");
						}

					}
					
					if(logger.isDebugEnabled()){
						logger.debug("ACR: PartyToCharge:"+ CommonUtils.formatBytes(legType.getValue()) +
								", timeIfNoTariffSwitch:" +timeIfNoTariffSwitch + ", timeIfTariffSwitch:"+
								timeIfTariffSwitch + ", isLegActive:"+isLegActive);
					}
				}
			}

		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding ACR " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Error in CapV2OperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException(dialogueId
					+ ":: [PH] IDP parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding ACR " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse ACR", e);
			}
			throw new ASNParsingException(dialogueId + ":: [PH] ASN Parsing Failure, ACR parsing failure occured.", e,
					MESSAGE.ACR);
		}

		legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERM_CONNECTED_ACR);

//		legData.set(LegDataAttributes.P_CAP_PARTY_TO_CHARGE,
//				camelCallResult.getTimeDurationChargingResult().getPartyToCharge());/camel FT serializtion issues
	}

	/**
	 * This Methos is used to create Continue without arguments Message
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createContinue(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createContinue" + dialogueId);
		}

		ContinueWithArgumentArg connectArg = new ContinueWithArgumentArg();

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(CapV2OpCodes.CONTINUE_WITH_ARG);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectArg);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createContinue operationObjs :" + operationObjs + " opCode :" + opCode);
		}
		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: createContinue completed");
		}
		return encodeList.getFirst();
	}

	/**
	 * This Method is used to create apply charging message
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static byte[] createApplyCharging(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createApplyCharging" + dialogueId);
		}

		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		int maxCallPeriodDuration = 1080;
		Boolean releaseIdDurationExceeded = true;
		String partyToCharge = "1";
		SendingSideID sendingSideId  = new SendingSideID();
		sendingSideId.selectSendingSideID(leg2Type);

		if(leg1Data.get(LegDataAttributes.P_CAP_MAX_CALL_PERIOD_DURATION) != null){
			maxCallPeriodDuration = (Integer) leg1Data.get(LegDataAttributes.P_CAP_MAX_CALL_PERIOD_DURATION);
		}

		if(leg1Data.get(LegDataAttributes.P_CAP_RELEASE_IF_DURATION_EXCEED) != null){
			releaseIdDurationExceeded = (Boolean) leg1Data.get(LegDataAttributes.P_CAP_RELEASE_IF_DURATION_EXCEED);
		}

		if(leg1Data.get(LegDataAttributes.P_CAP_PARTY_TO_CHARGE) != null){
			partyToCharge = (String) leg1Data.get(LegDataAttributes.P_CAP_PARTY_TO_CHARGE);

			if(StringUtils.equalsIgnoreCase(partyToCharge, "1")){
				sendingSideId.selectSendingSideID(leg1Type);
			}	
		}

		LinkedList<byte[]> encodeList = null; 

		try {
			ApplyChargingArg ac = new ApplyChargingArg();

			CAMEL_AChBillingChargingCharacteristics cAchBillChrgChar = new CAMEL_AChBillingChargingCharacteristics();		
			com.agnity.camelv2.asngenerated.CAMEL_AChBillingChargingCharacteristics.TimeDurationChargingSequenceType timeChargingSeqType = new 
					com.agnity.camelv2.asngenerated.CAMEL_AChBillingChargingCharacteristics.TimeDurationChargingSequenceType();

			timeChargingSeqType.setMaxCallPeriodDuration(maxCallPeriodDuration);

			if(releaseIdDurationExceeded){
				byte[] buf = {0x01, 0x01, (byte) 0xff};
				//timeChargingSeqType.setReleaseIfdurationExceeded(releaseIdDurationExceeded);
				timeChargingSeqType.setReleaseIfdurationExceeded(buf);
			}

			cAchBillChrgChar.selectTimeDurationCharging(timeChargingSeqType);
			logger.debug("maxCallPeriodDuration: "+maxCallPeriodDuration + " releaseIdDurationExceeded:"+releaseIdDurationExceeded + " timeChargingSeqType:"+timeChargingSeqType  );

			// get Byte array from CAMEL_ACHBilling Charging Characteristics
			LinkedList<Object> operationObjs = new LinkedList<Object>();
			LinkedList<String> opCode = new LinkedList<String>();
			operationObjs.add(cAchBillChrgChar);
			opCode.add(CapV2OpCodes.CAMEL_BILL_CHARGE_CHAR);

			logger.debug("before encoding opcode "+opCode );
			encodeList = CapV2OperationsCoding
					.encodeOperations(operationObjs, opCode,true);

			// Hack, if releaseDurationExceed is set then need to change tag 
			// from 0x81 to 0xa1 (for OCS)
			if(releaseIdDurationExceeded){
				byte[] chargingChar = encodeList.getFirst();
				if(chargingChar != null && chargingChar.length > 9){
					// replace tag at 6th octet
					chargingChar[6] = (byte) 0xa1;
				}
			}
			
			if(logger.isDebugEnabled()){
				logger.debug("AC: Billing Characteristics: "+ CommonUtils.formatBytes(encodeList.getFirst()));
			}

			// set value in apply charging 
			AChBillingChargingCharacteristics achChargingChar = new AChBillingChargingCharacteristics();
			achChargingChar.setValue(encodeList.getFirst());

			// Set AchBilling Charging Characteristics
			ac.setAChBillingChargingCharacteristics(achChargingChar);

			// partytoCharge - 0x01
			ac.setPartyToCharge(sendingSideId);

			// encode ApplyCharging 
			operationObjs.clear();
			opCode.clear();

			operationObjs.add(ac);
			opCode.add(CapV2OpCodes.APPLY_CHARGING);

			encodeList = CapV2OperationsCoding
					.encodeOperations(operationObjs, opCode,true);

		}catch(Exception exp){
			logger.error("AC Encoding Failure " + exp);
		}

		if(logger.isDebugEnabled()){
			logger.debug("ApplyCharging final bytes:" + CommonUtils.formatBytes(encodeList.getFirst()));
		}

		return ((encodeList != null)?encodeList.getFirst():null);
	}

	/**
	 * This Method is used to create Reset Timer Message
	 * 
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static byte[] createResetTimer(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createResetTimer" + dialogueId);
		}

		ResetTimerArg resetTimerArg = new ResetTimerArg();

		TimerID tmr = new TimerID();
		tmr.setValue(EnumType.tssf);
		resetTimerArg.setTimerID(tmr);
		// resetTimerArg.setTimervalue(value);
		LinkedList opObjects = new LinkedList();
		opObjects.add(resetTimerArg);
		LinkedList opCodes = new LinkedList();
		opCodes.add(CapV2OpCodes.RESET_TIMER);
		LinkedList<byte[]> resetLinkList = CapV2OperationsCoding.encodeOperations(opObjects, opCodes, true);
		return resetLinkList.getFirst();

	}

	/**
	 * Method used to send RRBCSM for leg1 for OCS
	 * @return
	 * @throws Exception
	 */
	public static byte[] generateRRBCSMLeg1Parameters() throws Exception {
		BCSMEvent abandonBCSMEvent = getBCSMEvent(EventTypeBCSM.EnumType.oAbandon,
				MonitorMode.EnumType.notifyAndContinue);

		logger.info("inside generate generateRRBCSMLeg1Parameters");
		BCSMEvent disconnectBCSMEvent = getBCSMEvent(EventTypeBCSM.EnumType.oDisconnect,
				MonitorMode.EnumType.interrupted);
		LegID sendingLegId1 = new LegID();
		sendingLegId1.selectSendingSideID(leg1Type);
		disconnectBCSMEvent.setLegID(sendingLegId1);

		LinkedList<BCSMEvent> bcsmEventList = new LinkedList<BCSMEvent>();
		bcsmEventList.add(abandonBCSMEvent);
		bcsmEventList.add(disconnectBCSMEvent);

		RequestReportBCSMEventArg rrBCSMEventArg = new RequestReportBCSMEventArg();
		rrBCSMEventArg.setBcsmEvents(bcsmEventList);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(rrBCSMEventArg);
		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(CapV2OpCodes.REQUEST_REPORT);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
		return encodeList.getFirst();
	}

	/**
	 * Method used to send RRBCSM for Leg2 
	 * @return
	 * @throws Exception
	 */
	public static byte[] generateRRBCSMLeg2Parameters() throws Exception {
		logger.info("inside generate generateRRBCSMLeg2Parameters");
		BCSMEvent routeSelectFailureBCSMEvent = getBCSMEvent(EventTypeBCSM.EnumType.routeSelectFailure,
				MonitorMode.EnumType.interrupted);

		BCSMEvent oCalledPartyBusyBCSMEvent = getBCSMEvent(EventTypeBCSM.EnumType.oCalledPartyBusy,
				MonitorMode.EnumType.interrupted);

		BCSMEvent oAnswerBCSMEvent = getBCSMEvent(EventTypeBCSM.EnumType.oAnswer, MonitorMode.EnumType.interrupted);

		BCSMEvent oDisconnectBCSMEvent = getBCSMEvent(EventTypeBCSM.EnumType.oDisconnect,
				MonitorMode.EnumType.interrupted);

		LegID sendingLegId2 = new LegID();
		sendingLegId2.selectSendingSideID(leg2Type);
		oDisconnectBCSMEvent.setLegID(sendingLegId2);

		BCSMEvent oNoAnswerBCSMEvent = getBCSMEvent(EventTypeBCSM.EnumType.oNoAnswer, MonitorMode.EnumType.interrupted);

		LinkedList<BCSMEvent> bcsmEventList = new LinkedList<BCSMEvent>();
		bcsmEventList.add(routeSelectFailureBCSMEvent);
		bcsmEventList.add(oCalledPartyBusyBCSMEvent);
		bcsmEventList.add(oAnswerBCSMEvent);
		bcsmEventList.add(oDisconnectBCSMEvent);
		bcsmEventList.add(oNoAnswerBCSMEvent);

		RequestReportBCSMEventArg rrBCSMEventArg = new RequestReportBCSMEventArg();
		rrBCSMEventArg.setBcsmEvents(bcsmEventList);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(rrBCSMEventArg);
		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(CapV2OpCodes.REQUEST_REPORT);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);

		if(logger.isDebugEnabled()){
			logger.debug("encodeList.getFirst():" + CommonUtils.formatBytes(encodeList.getFirst()));
		}
		return encodeList.getFirst();
	}

	/**
	 * Method used to send RRBCSM for Terminating MO. currently encode tAbandon
	 * @return
	 * @throws Exception
	 */
	public static byte[] generateRRBCSMTerminatingMO() throws Exception {
		logger.info("inside generate generateRRBCSMTerminatingMO");

		BCSMEvent tAbandon = getBCSMEvent(EventTypeBCSM.EnumType.tAbandon,
				MonitorMode.EnumType.notifyAndContinue);

		LinkedList<BCSMEvent> bcsmEventList = new LinkedList<BCSMEvent>();
		bcsmEventList.add(tAbandon);

		RequestReportBCSMEventArg rrBCSMEventArg = new RequestReportBCSMEventArg();
		rrBCSMEventArg.setBcsmEvents(bcsmEventList);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(rrBCSMEventArg);
		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(CapV2OpCodes.REQUEST_REPORT);

		LinkedList<byte[]> encodeList = CapV2OperationsCoding.encodeOperations(operationObjs, opCode, true);
		if(logger.isDebugEnabled()){
			logger.debug("encodeList.getFirst():" + CommonUtils.formatBytes(encodeList.getFirst()));
		}
		return encodeList.getFirst();
	}

	private static BCSMEvent getBCSMEvent(com.agnity.camelv2.asngenerated.EventTypeBCSM.EnumType eventTypeEnum,
			com.agnity.camelv2.asngenerated.MonitorMode.EnumType monitorModeEnum) {
		EventTypeBCSM eventType = new EventTypeBCSM();
		eventType.setValue(eventTypeEnum);

		MonitorMode monitorMode = new MonitorMode();
		monitorMode.setValue(monitorModeEnum);

		BCSMEvent bcsmEvent = new BCSMEvent();
		bcsmEvent.setEventTypeBCSM(eventType);
		bcsmEvent.setMonitorMode(monitorMode);
		return bcsmEvent;
	}

	/**
	 * Method used to cerate Cancel Buffer
	 * @param callData
	 * @return
	 */
	public static byte[] createCancel(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createCancel" + dialogueId);
		}

		CancelArg cancelArg = new CancelArg();
		org.bn.types.NullObject nullObj = new org.bn.types.NullObject();
		cancelArg.selectAllRequests(nullObj);

		LinkedList opObjects = new LinkedList();
		opObjects.add(cancelArg);

		LinkedList opCodes = new LinkedList();
		opCodes.add(CapV2OpCodes.CANCEL);

		LinkedList<byte[]> linkListObj = CapV2OperationsCoding.encodeOperations(opObjects, opCodes, true);

		if(logger.isDebugEnabled()){
			logger.debug("Cancel Buffer:" + CommonUtils.formatBytes(linkListObj.getFirst()));
		}
		return linkListObj.getFirst();
	}

	/**
	 * Method to create CTR
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createConnectToResource(CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectToResource" + dialogueId);
		}

		ConnectToResourceArg ctrArg = new ConnectToResourceArg();

		// Resource Address
		ResourceAddressChoiceType resAddrChoiceType = new ResourceAddressChoiceType();
		resAddrChoiceType.selectNone(new org.bn.types.NullObject());

		// Resource Address Type
		ctrArg.setResourceAddress(resAddrChoiceType);

		// send Service interactionIndicatorTwo only incase of OCS		
		if(StringUtils.equalsIgnoreCase(Capv2ScfProtocolConfig
				.getConfigData(Capv2ScfProtocolConfig.CAPV2_FLOW), "OCS")){
			// ServiceInteractionIndicatorTwo
			ServiceInteractionIndicatorsTwo srvInd = new ServiceInteractionIndicatorsTwo();
			BothwayThroughConnectionInd bothway = new BothwayThroughConnectionInd();
			bothway.setValue(BothwayThroughConnectionInd.EnumType.bothwayPathNotRequired);
			srvInd.setBothwayThroughConnectionInd(bothway);

			// ServiceIndicator
			ctrArg.setServiceInteractionIndicatorsTwo(srvInd);
		}

		LinkedList opObjects = new LinkedList();
		opObjects.add(ctrArg);

		LinkedList opCodes = new LinkedList();
		opCodes.add(CapV2OpCodes.CONNECT_TO_RESOURCE);

		LinkedList<byte[]> linkListObj = CapV2OperationsCoding.encodeOperations(opObjects, opCodes, true);

		if(logger.isDebugEnabled()){
			logger.debug("ConnectorToResource Buffer:" + CommonUtils.formatBytes(linkListObj.getFirst()));
		}

		return linkListObj.getFirst();
	}

	/**
	 * Method to create Play Message
	 * @param callData
	 * @return
	 */
	public static byte[] createPlay(CallData callData) throws Exception {
		int dialogId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);		
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createPlay" + dialogId);
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		AnnSpec annSpec = (AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC);

		if (annSpec == null) {
			logger.error(dialogId + ":: No announmcement spec specified by Service");
			throw new Exception(dialogId + ":: :: No announmcement spec specified by Service");
		}

		// get Announcement 
		ArrayList<PlayMessage> annList = (ArrayList<PlayMessage>) annSpec.getPlayMsgList();
		Iterator<PlayMessage> annEntry = annList.iterator();
		PlayMessage playMessage = null;

		if ((annList == null) || (annList.isEmpty())) {
			logger.error(dialogId + ":: No announmcement specified by Service");
			throw new Exception(dialogId + "::  No announmcement specified by Service");
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
				// There could be multiple announcement. As per OCS there would be only one announcement 
				// therefore assuming only one announcement for timebeing 
				break;
			}
		}

		PlayAnnouncementArg playArg = new PlayAnnouncementArg();

		// Set announcement Id
		Integer4 int4 = new Integer4();
		int4.setValue(Integer.parseInt(getSs7AnnouncementId));

		// First create message ID 
		MessageID msgId = new MessageID();
		msgId.selectElementaryMessageID(int4);

		InbandInfo inbandInfo  = new InbandInfo();
		inbandInfo.setMessageID(msgId);

		InformationToSend infoToSend = new InformationToSend();
		infoToSend.selectInbandInfo(inbandInfo);

		// Information to send 
		playArg.setInformationToSend(infoToSend);

		// following fields need to be set in case OCS and not for Telus
		if(!StringUtils.equalsIgnoreCase(Capv2ScfProtocolConfig
				.getConfigData(Capv2ScfProtocolConfig.SEND_TERMINATING_RRBCSM), "TRUE")){

			// Disconnect from IP FOrbidden
			playArg.setDisconnectFromIPForbidden(true);

			// requestAnnouncementCompleteNotification
			playArg.setRequestAnnouncementComplete(true);
		}

		LinkedList opObjects = new LinkedList();
		opObjects.add(playArg);

		LinkedList opCodes = new LinkedList();
		opCodes.add(CapV2OpCodes.PLAY_ANNOUNCEMENT);

		LinkedList<byte[]> linkListObj = CapV2OperationsCoding.encodeOperations(opObjects, opCodes, true);

		if(logger.isDebugEnabled()){
			logger.debug("PlayAnnouncement Buffer:" + CommonUtils.formatBytes(linkListObj.getFirst()));
		}
		return linkListObj.getFirst();
	}

	/**
	 * @param invokeIndEvent
	 * @param callData
	 */
	public static void parseSRR(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		CapV2CallStates capCallState = (CapV2CallStates) legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		ServiceInterface serviceInterface = PhUtilityServices
				.getInstance((String) callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + " :: Inside process parseSRR");
		}

		Event event = null;
		SpecializedResourceReportArg srrArg;
		try {
			// SpecializedResourceReport is of null type. We need not decode it as it will throw an exception. 
			// Bypass decode. 
			//srrArg = (SpecializedResourceReportArg) CapV2OperationsCoding.decodeOperation(invokeIndEvent, true);

			if(capCallState == CapV2CallStates.USER_INTREACTION_IN_PROGRESS){
				if(logger.isDebugEnabled()){
					logger.debug("parseSRR - specializedResourceReport: Last Operation was UserInteraction");
				}

				event = new Event(EventType.EVENT_PLAY_SUCCESS, Protocol.CAPV2_SCF, null);
				legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.PLAY_SUCCESS_SRR);
			}
			if( capCallState==CapV2CallStates.TERM_CONNECT_IN_PROGRESS){

				event = new Event(EventType.EVENT_SUCCESS, Protocol.CAPV2_SCF, null);
				legData.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.TERM_CONNECTED);
			}

		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding SRR " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse SRR", e);
			}
			throw new ASNParsingException(dialogueId + ":: [PH] ASN Parsing Failure, SRR parsing failure occured.", e,
					MESSAGE.SRR);
		}

		// Notify application of disconnect event	
		ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
	}


	/**
	 * Method used to send Prompt & Collect
	 * @param tcapSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 */
	public static void sendPromptAndCollectAnnouncement(TcapSession tcapSession, CallData callData, Action action) 
			throws Exception { 
		LegData legData;
		String legSetInAction = action.getLeg();
		if (StringUtils.isNotEmpty(legSetInAction)) {
			legData = (LegData) callData.get(CallDataAttribute.valueOf(legSetInAction));
		} else {
			logger.warn("[PH]:: No leg set in action, using leg1 as default");
			legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside sendPromptAndCollectAnnouncement");
		}

		byte[] promptAndCollectAnnouncement = Capv2ScfProtocolHelper
				.createPromptAndCollectUserInformation((AnnSpec) legData.get(LegDataAttributes.NP_ANN_SPEC));

		byte[] playAnnOpCode = {CapV2OpCodes.PROMPT_COLLECT_USER_INFO_BYTE};

		Operation connectOperation = new Operation(Operation.OPERATIONTYPE_LOCAL, playAnnOpCode);

		InvokeReqEvent connectInvokeReqEvent = new InvokeReqEvent(src, tcapSession.getDialogueId(), connectOperation);
		connectInvokeReqEvent.setInvokeId(InapCS1ScfProtocolUtil.getNextInvokeId(callData));
		connectInvokeReqEvent.setParameters(new Parameters(Parameters.PARAMETERTYPE_SEQUENCE, promptAndCollectAnnouncement));
		connectInvokeReqEvent.setClassType(PhConstants.INVOKE_CLASS_TYPE);

		//Setting PromptAndCollect Invoke ID to be used while processing result
		callData.set(CallDataAttribute.PROMPT_COLLECT_INVOKE_ID, connectInvokeReqEvent.getInvokeId());

		sendComponentReq(connectInvokeReqEvent, callData);
		InapCS1ScfProtocolHelper.sendContinueRequestEvent(tcapSession);

		// Set Call state 
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
		leg1Data.set(LegDataAttributes.P_LEG_SS7_STATE, CapV2CallStates.USER_INTREACTION_IN_PROGRESS);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exiting sendPromptAndCollectAnnouncement() .....");
		}
	}

	/**
	 * @param resultIndEvent
	 * @param callData
	 */
	public static void parsePACResult(ResultIndEvent resultIndEvent,
			CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception{
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extract ReceivedInformation Argument from ResultIndEvent");
		}

		ReceivedInformationArg receivedInformationArg;
		try {
			receivedInformationArg = (ReceivedInformationArg) CapV2OperationsCoding.decodeOperation(resultIndEvent, true);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Error in decoding ReceivedInformation " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Error in InapOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: ReceivedInformation parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Error in decoding ReceivedInformation " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Exception in parse ReceivedInformation", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, ReceivedInformation parsing failure occured.", e, ASNParsingException.MESSAGE.IDP);
		}
		try {
			String digitsCollected = null;
			if (null != receivedInformationArg && null != receivedInformationArg.getDigitsResponse()) {
				digitsCollected = GenericDigits.decodeGenericDigits(receivedInformationArg.getDigitsResponse().getValue()).getDigits();
				if (logger.isDebugEnabled()) {
					logger.debug("Digits collected: " + digitsCollected);
				}
			} else {
				logger.warn("No digits found in the input!");
			}
			legData.set(LegDataAttributes.P_COLLECTED_DIGITS, digitsCollected);
		} catch (Exception e) {
			logger.error("[PH]:: Error in parsePACResult " + e.getMessage());
			throw new ASNParsingException("[PH]:: ASN Parsing Failure: ReceivedInformation parsing failure occured.", e, ASNParsingException.MESSAGE.IDP);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: ReceivedInformation parsed successfully");
		}
	}

	/**
	 * Method is used to replace extension tag coming in InitialDP. 
	 * This issue has occurred with OCS customer. InitialDPArg has
	 * been modified for extension Field. It has been replaced with 
	 * octet string. Therefore the incoming tag 0xaf need to be replaced
	 * with 0x8f. 
	 * @param input
	 * @return
	 * @throws Exception 
	 */
	@SuppressWarnings("unused")
	private static InitialDPArg replaceExtensionOffset(EventObject eventObject) throws Exception{

		if(logger.isDebugEnabled()){
			logger.debug("Inside: replaceExtensionOffset");
		}

		InitialDPArg idpArg = null;

		// take buffer from invoke
		ComponentIndEvent cmpReqEvent = (ComponentIndEvent)eventObject ;		
		InvokeIndEvent receivedInvoke = (InvokeIndEvent)cmpReqEvent; 
		byte[] opBuffer = receivedInvoke.getParameters().getParameter();

		if(logger.isDebugEnabled()){
			logger.debug("replaceExtensionOffset: IDP before repalcement:" + 
					CommonUtils.formatBytes(opBuffer));
		}

		try{
			int offset = 0;
			// check if buffer is long form. if yes then it shall be 0x81
			if(Byte.compare(opBuffer[1], (byte) 0x81) == 0){
				offset = 4;
			}else{
				offset = 3;
			}

			// New offset shall be next tag (current offset + SK len + 1)
			offset += ((int)opBuffer[offset++])+1;

			// check if next tag is 0x83
			if(Byte.compare(opBuffer[offset], (byte) 0x83)==0){
				offset += opBuffer[++offset] +2; // right shift by length of 0x83
			}

			// check if next tag is 0x85
			if(Byte.compare(opBuffer[offset], (byte) 0x85)==0){
				offset += opBuffer[++offset] +2; // right shift by length of 0x85
			}

			if(Byte.compare(opBuffer[offset], (byte) 0x8a)==0){
				offset += opBuffer[++offset] +2; // right shift by length 0f 0x8a
			}

			// reached tag for extension parameter, replace it bit 0x8f
			if(Byte.compare(opBuffer[offset], (byte) 0xaf)==0 ){
				opBuffer[offset] = (byte) 0x8f;
			}
			
			try{
				idpArg =(InitialDPArg) CapV2OperationsCoding.decodeOperationsForOpCode(opBuffer, CapV2OpCodes.IDP, true);
				if(logger.isDebugEnabled()){
					logger.debug("OCS IDP parsed successfully");
				}
			}catch(Exception exp){
				logger.error("OCS IDP Parsing exception" + exp.getMessage());
				throw exp;
			}
		}catch(Exception ex){
			logger.error("OCS Offset parsing issue" + ex.getMessage());
			throw ex;
		}

		if(logger.isDebugEnabled()){
			if(logger.isDebugEnabled()){
				logger.debug("Exit replaceExtensionOffset");
			}
		}
		return idpArg;
	}	
}