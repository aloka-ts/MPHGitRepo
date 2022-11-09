/*******************************************************************************
 * Copyright (c) 2011 Agnity, Inc. All rights reserved.
 * <p>
 * This is proprietary source code of Agnity, Inc.
 * <p>
 * Agnity, Inc. retains all intellectual property rights associated
 * with this source code. Use is subject to license terms.
 * <p>
 * This source code contains trade secrets owned by Agnity, Inc.
 * Confidentiality of this computer program must be maintained at
 * all times, unless explicitly authorized by Agnity, Inc.
 *******************************************************************************/
package com.agnity.ph.inapcs2scf;

import com.agnity.inapitutcs2.asngenerated.*;
import com.agnity.inapitutcs2.datatypes.*;
import com.agnity.inapitutcs2.datatypes.BearerCapability;
import com.agnity.inapitutcs2.datatypes.Cause;
import com.agnity.inapitutcs2.datatypes.fci.mexico.*;
import com.agnity.inapitutcs2.enumdata.*;
import com.agnity.inapitutcs2.exceptions.InvalidInputException;
import com.agnity.inapitutcs2.operations.InapOpCodes;
import com.agnity.inapitutcs2.operations.InapOperationsCoding;
import com.agnity.inapitutcs2.util.NonAsnArg;
import com.agnity.mphdata.common.*;
import com.agnity.mphdata.common.Event.EventType;
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
import com.agnity.ph.common.exception.CriticalityTypeException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.agnity.ph.sip.SipProtocolConfig;
import com.genband.tcap.provider.TcapSession;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.tcap.TcapConstants;
import jain.protocol.ss7.tcap.component.InvokeIndEvent;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.bn.exceptions.EnumParamOutOfRangeException;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import static com.agnity.ph.inapcs2scf.InapCS2ScfProtocolUtil.leg1Type;
import static com.agnity.ph.inapcs2scf.InapCS2ScfProtocolUtil.leg2Type;

/**
 * This class is used to do INAP protocol messages parsing
 */
public class InapCS2ScfProtocolParser {

	private static Logger logger = Logger.getLogger(InapCS2ScfProtocolParser.class);

	/**
	 * This method is called by protocol handler to parse the IDP received
	 * and populate the callData object from the received parameters.
	 *
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param callData       represents an instance of CallData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseIdp(InvokeIndEvent invokeIndEvent, CallData callData) throws ASNParsingException, CriticalityTypeException, ParameterOutOfRangeException, Exception {
		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extract InitialDP Argument from IDP");
		}
		InitialDPArg idpArg;
		try {
			idpArg = (InitialDPArg) InapOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Error in decoding IDP " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Error in InapOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: IDP parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Error in decoding IDP " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Exception in parse IDP", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, IDP parsing failure occured.", e, MESSAGE.IDP);
		}

		/*
		 *  IDP.serviceKey
		 */
		if (idpArg.isServiceKeyPresent()) {
			Integer serviceKey = idpArg.getServiceKey().getValue().getValue();

			// Service returned will be negative incase if it is greater than 
			// 128. So either add 256 to negative value or logical AND with FF
			serviceKey &= 0xFF;

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Extracted Service Key from IDP is " + serviceKey);
			}

			callData.set(CallDataAttribute.P_SERVICE_KEY, serviceKey.intValue());
		}
		try {
			/*
			 *  IDP.callingPartyNumber
			 */
			if (idpArg.isCallingPartyNumberPresent()) {
				CallingPartyNum callingPartyNum = CallingPartyNum.decodeCalgParty(idpArg.getCallingPartyNumber().getValue());

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Extracted Calling party number from IDP is " + callingPartyNum);
				}
				if (callingPartyNum == null || callingPartyNum.getAddrSignal() == null || "".equals(callingPartyNum.getAddrSignal().trim())) {
					/*
					 * call should be handled as ASn parse failure as address signal is missing
					 */
					logger.error("[PH]:: Calling party num address signal missing");
					//throw new ASNParsingException("[PH]:: Calling party num address signal missing", MESSAGE.IDP);
				} else {
					PhoneNumber callingNumber = parseCallingPartyNum(dialogueId, callingPartyNum);
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Exit parseCallingPartyNum");
					}
					legData.set(LegDataAttributes.P_CALLING_PARTY, callingNumber);

					//Service needs it in String
					String noaString = "";
					if (callingNumber.getNatureOfAddress() == PhoneNumber.NOA_INTERNATIONAL) {
						noaString = "INT";
					} else if (callingNumber.getNatureOfAddress() == PhoneNumber.NOA_NATIONAL) {
						noaString = "NAT";
					}
					legData.set(LegDataAttributes.P_NOA, noaString);
				}


			}

			/*
			 *  IDP.dialledDigits
			 */
			if (idpArg.isDialledDigitsPresent()) {
				CalledPartyNum calledPartyNum = CalledPartyNum.decodeCaldParty(idpArg.getDialledDigits().getValue());

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Extracted dialed digit from IDP is " + calledPartyNum);
				}

				PhoneNumber dialedDigit = parseCalledPartyNum(dialogueId, calledPartyNum);
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Exit parseCalledPartyNum");
				}
				legData.set(LegDataAttributes.P_DIALED_DIGITS, dialedDigit);
			}

			/*
			 *  IDP.calledPartyNumber
			 */
			if (idpArg.isCalledPartyNumberPresent()) {
				CalledPartyNum calledPartyNum = CalledPartyNum.decodeCaldParty(idpArg.getCalledPartyNumber().getValue());

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Extracted called number from IDP is " + calledPartyNum);
				}

				PhoneNumber calledNumber = parseCalledPartyNum(dialogueId, calledPartyNum);
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Exit parseCalledPartyNum");
				}
				legData.set(LegDataAttributes.P_CALLED_PARTY, calledNumber);
				/*
				 * Require to match trace criteria
				 */
				legData.set(LegDataAttributes.P_IDP_CALLED_PARTY, calledNumber);
			}

			/*
			 *Calling Party Category
			 */
			if (idpArg.isCallingPartysCategoryPresent()) {
				CalgPartyCatgEnum callingPartyCatgEnum = NonAsnArg.decodeCalgPartyCatg(idpArg.getCallingPartysCategory().getValue());
				if (callingPartyCatgEnum == null) {
					logger.error("[PH]:: Calling party category value is null");
					//throw new ASNParsingException("[PH]:: DECODED Calling party category is null in IDP", MESSAGE.IDP, FAILTYPE.CPC_MISSING);
				}else {
					int callingPartyCategory = callingPartyCatgEnum.getCode();
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Extracted CPC from IDP is " + callingPartyCategory);
					}
					legData.set(LegDataAttributes.P_CPC, String.valueOf(callingPartyCategory));
				}
			}

			/*
			 * Bearer Capability
			 */
			if (idpArg.isBearerCapabilityPresent()) {
				com.agnity.inapitutcs2.asngenerated.BearerCapability bearerCapability = idpArg.getBearerCapability();

				if(bearerCapability.isBearerCapSelected()){
					BearerCapability bcap =
							BearerCapability.decodeBearerCapability(bearerCapability.getBearerCap());

					int transRate = bcap.getInfoTrfrRate().getCode();
					int transCap = bcap.getInfoTrnsfrCap().getCode();

					legData.set(LegDataAttributes.P_TRANSFER_RATE, transRate);
					legData.set(LegDataAttributes.P_TRANSFER_CAPABILITY, transCap);


					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Extracted Transfer Rate from IDP is " + transRate + ", transfer capability:" + transCap);
					}

				}else if (bearerCapability.isTmrSelected()) {
					TransmissionMedReqEnum tmrEnum = NonAsnArg.decodeTmr(bearerCapability.getTmr());
					if (tmrEnum == null) {
						logger.error("[PH]:: TMR value is null");
						throw new ASNParsingException("[PH]:: DECODED TMR is null in IDP", MESSAGE.IDP, FAILTYPE.TMR_MISSING);
					}
					int tmr = tmrEnum.getCode();
					if (logger.isDebugEnabled()) {
						logger.debug("[PH]:: Extracted TMR from IDP is " + tmr);
					}
					legData.set(LegDataAttributes.P_BEAR_CAP, tmr);
				}
			}
			// Original Called Party ID
			if(idpArg.isOriginalCalledPartyIDPresent()){
				CalledPartyNum origCalledPartyID = CalledPartyNum.decodeCaldParty(idpArg.getOriginalCalledPartyID().getValue());

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Extracted Original Called Party ID from IDP is " + origCalledPartyID);
				}

				PhoneNumber origCalledPartyPhoneNumber = parseCalledPartyNum(dialogueId, origCalledPartyID);

				if(StringUtils.isNotBlank(origCalledPartyPhoneNumber.getAddress())){
					if(logger.isDebugEnabled()){
						logger.debug("Original Called Party ID from IDP: "+ origCalledPartyPhoneNumber);
					}

					legData.set(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER, origCalledPartyPhoneNumber);
				}else{
					if(logger.isDebugEnabled()){
						logger.debug("Original Called Party ID address is null");
					}
				}
			}

			// Redirecting Party ID
			if(idpArg.isRedirectingPartyIDPresent()){
				CalledPartyNum redirectingPartyID = CalledPartyNum.decodeCaldParty(idpArg.getRedirectingPartyID().getValue());

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Extracted Redirecting Party ID from IDP is " + redirectingPartyID);
				}

				PhoneNumber redirectingPartyIdPhoneNum = parseCalledPartyNum(dialogueId, redirectingPartyID);

				if(StringUtils.isNotBlank(redirectingPartyIdPhoneNum.getAddress())){
					if(logger.isDebugEnabled()){
						logger.debug("Redirecting Party ID from IDP: "+ redirectingPartyIdPhoneNum);
					}

					legData.set(LegDataAttributes.P_REDIRECTING_PARTY_ID, redirectingPartyIdPhoneNum);
				}else{
					if(logger.isDebugEnabled()){
						logger.debug("Redirecting Party ID address is null");
					}
				}
			}else{
				logger.debug("Redirecting Party ID not present");
			}
		} catch (InvalidInputException e) {
			logger.error("[PH]:: Error in parseIdp " + e.getMessage());
			throw new ASNParsingException("ASN Parsing Failure: IDP parsing failure occured.:", e, MESSAGE.IDP);
		} catch (ASNParsingException e) {
			logger.error("[PH]:: Error in parseIdp " + e.getMessage());
			throw e;
		} catch (Exception e) {
			logger.error("[PH]:: Error in parseIdp " + e.getMessage());
			throw new ASNParsingException("[PH]:: ASN Parsing Failure: IDP parsing failure occured.", e, MESSAGE.IDP);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Initial DP parsed successfully");
		}
	}

	/**
	 * This method is called by protocol handler for parsing calling party number and return
	 * a phoneNumber instance.
	 *
	 * @param dialogueId      represents integer value of dialogue Id.
	 * @param callingPartyNum represents an instance of CallingPartyNum
	 * @return an instance of PhoneNumber
	 */
	private static PhoneNumber parseCallingPartyNum(int dialogueId, CallingPartyNum callingPartyNum) {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside parseCallingPartyNum");
		}

		PhoneNumber callingNumber = new PhoneNumber();

		/*
		 *  Nature Of Address
		 */
		NatureOfAddEnum natureOfAddrEnum = callingPartyNum.getNatureOfAdrs();
		if ((natureOfAddrEnum == NatureOfAddEnum.SPARE) || (natureOfAddrEnum == NatureOfAddEnum.SUBS_NO) || (natureOfAddrEnum == NatureOfAddEnum.NETWORK_NO) || (natureOfAddrEnum == NatureOfAddEnum.NATIONAL_NO)) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == NatureOfAddEnum.UNKNOWN) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if (natureOfAddrEnum == NatureOfAddEnum.INTER_NO) {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			callingNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extracted Nature of Address is " + natureOfAddrEnum.getCode());
		}

		/*
		 *  Numbering Plan Indicator
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
			logger.debug("[PH]:: Extracted Numbering Plan is " + numPlanIndEnum.getCode());
		}

		/*
		 *  Address
		 */
		String addrSignal = callingPartyNum.getAddrSignal();
		callingNumber.setAddress(addrSignal);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extracted Address Signal is " + addrSignal);
		}

		callingNumber.setAddressPresentationRestrictedIndicator(callingPartyNum.getAdrsPresntRestd().getCode());
		callingNumber.setPresentationIndicator(callingPartyNum.getAdrsPresntRestd().getCode());
		callingNumber.setScreeningIndicator(callingPartyNum.getScreening().getCode());
		callingNumber.setNumberIncompleteIndicator(callingPartyNum.getNumIncomplte().getCode());

		return callingNumber;
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
			logger.debug("[PH]:: Inside parseCalledPartyNum");
		}

		PhoneNumber calledNumber = new PhoneNumber();

		/*
		 *  Nature Of Address
		 */
		NatureOfAddEnum natureOfAddrEnum = calledPartyNum.getNatureOfAdrs();
		if ((natureOfAddrEnum == NatureOfAddEnum.SPARE) || (natureOfAddrEnum == NatureOfAddEnum.SUBS_NO) || (natureOfAddrEnum == NatureOfAddEnum.NETWORK_NO) || (natureOfAddrEnum == NatureOfAddEnum.NATIONAL_NO)) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_NATIONAL);
		} else if (natureOfAddrEnum == NatureOfAddEnum.UNKNOWN) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		} else if (natureOfAddrEnum == NatureOfAddEnum.INTER_NO) {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_INTERNATIONAL);
		} else {
			calledNumber.setNatureOfAddress(PhoneNumber.NOA_UNKNOWN);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extracted Nature of Address is " + natureOfAddrEnum.getCode());
		}

		/*
		 *  Numbering Plan Indicator
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
			logger.debug("[PH]:: Extracted Numbering Plan is " + numPlanIndEnum.getCode());
		}

		/*
		 *  Address
		 */
		String addrSignal = calledPartyNum.getAddrSignal();
		calledNumber.setAddress(addrSignal);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extracted Address Signal is " + addrSignal);
		}

		return calledNumber;
	}


	/**
	 * This method is called by protocol handler for creating SCI event.
	 *
	 * @param noChargeInd represent boolean flag for noChargeInd
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createSendChargingInformation(boolean noChargeInd) throws Exception {
		return null;
	}

	/**
	 * This method is called by protocol handler for creating a RNCE event.
	 *
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createRequestNotificationChargingEvent() throws Exception {
		return null;
	}

	/**
	 * This method is called by protocol handler for creating RRBCSM for Disarming of signals.
	 *
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createRRBCSMForDisarming(CallData callData, Action action) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createRRBCSMForDisarming");
		}

		// Monitor Mode - Transparent
		MonitorMode monitorTransparent = new MonitorMode();
		monitorTransparent.setValue(MonitorMode.EnumType.transparent);

		// Leg Id
		LegID leg1Id = new LegID();
		leg1Id.selectSendingSideID(leg1Type);

		LinkedList<BCSMEvent> bcsmEventList = new LinkedList<BCSMEvent>();

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Disarm oDisconnect");
		}

		// oDisconnect-Leg1
		EventTypeBCSM eventTypeBCSMDisconnect = new EventTypeBCSM();
		eventTypeBCSMDisconnect.setValue(EventTypeBCSM.EnumType.oDisconnect);
		BCSMEvent bcsmEventDisconnectLeg1 = new BCSMEvent();
		bcsmEventDisconnectLeg1.setEventTypeBCSM(eventTypeBCSMDisconnect);
		bcsmEventDisconnectLeg1.setMonitorMode(monitorTransparent);
		bcsmEventDisconnectLeg1.setLegID(leg1Id);
		bcsmEventList.add(bcsmEventDisconnectLeg1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Disarm oAbandon");
		}

		// oAbandon
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
		opCode.add(InapOpCodes.RRBE);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}


	/**
	 * This method is called by protocol handler for creating INAP CONNECT message for HandOff.
	 *
	 * @param callData  represents an instance of CallData
	 * @param localAddr represents an instance of SccpUserAddress
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createConnectForHandoff(CallData callData, SccpUserAddress localAddr) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectForTerm");
		}
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		PhoneNumber destinationNumber = (PhoneNumber) legData.get(LegDataAttributes.P_DESTINATION_NUMBER);
		int corrId = (Integer) callData.get(CallDataAttribute.P_CORRELATION_ID);
		int clusterNum = (Integer) callData.get(CallDataAttribute.P_CLUSTER_NUMBER);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectForHandoff");
		}

		String correlationNumber = PhConstants.CORRELATION_NO_FLEX_CHARGING;

		// Called Party Number
		String addressDigits = PhConstants.CORRELATION_PREFIX + PhConstants.CORRELATION_HANDOFF + clusterNum + correlationNumber + corrId;

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: addressDigits " + addressDigits);
		}

		byte[] calledPartyNum = CalledPartyNum.encodeCaldParty(addressDigits, NatureOfAddEnum.ASSIST_SSPIP_ROUTE_ADDR, NumPlanEnum.ISDN_NP, IntNwNumEnum.ROUTING_ALLWD);

		CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
		calledPartyNumber.setValue(calledPartyNum);

		// Destination Routing Address
		DestinationRoutingAddress destRoutingAddress = new DestinationRoutingAddress();
		destRoutingAddress.initValue();
		destRoutingAddress.add(calledPartyNumber);

		ConnectArg connectArg = new ConnectArg();
		connectArg.setDestinationRoutingAddress(destRoutingAddress);

		/*
		 * correlationID coding scheme should be odd or even depending on length
		 * of corrID
		 */
		String corrIdStr = String.valueOf(corrId);
		EncodingSchemeEnum encodeSchemeEnum = null;
		if (corrIdStr.length() % 2 == 0) {
			encodeSchemeEnum = EncodingSchemeEnum.BCD_EVEN;
		} else {
			encodeSchemeEnum = EncodingSchemeEnum.BCD_ODD;
		}

		byte[] correlationIdByteArr = GenericDigits.encodeGenericDigits(encodeSchemeEnum, DigitCatEnum.CORRELATION_ID, corrIdStr);
		Digits correlationIdDigits = new Digits(correlationIdByteArr);

		CorrelationID correlationId = new CorrelationID();
		correlationId.setValue(correlationIdDigits);
		connectArg.setCorrelationID(correlationId);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: correlationId is " + correlationId);
		}

		// scfID
		byte[] scfIdByteArr = ScfId.encodeScfId(SPCIndicatorEnum.SPC_PRESENT, SSNIndicatorEnum.SSN_PRESENT, GTIndicatorEnum.NO_GT, RoutingIndicatorEnum.ROUTING_PC_SSN, localAddr.getSubSystemAddress().getSignalingPointCode().getZone(), localAddr.getSubSystemAddress().getSignalingPointCode().getCluster(), localAddr.getSubSystemAddress().getSignalingPointCode().getMember(), localAddr.getSubSystemAddress().getSubSystemNumber());
		com.agnity.inapitutcs2.asngenerated.ScfID scfId = new com.agnity.inapitutcs2.asngenerated.ScfID(scfIdByteArr);
		connectArg.setScfID(scfId);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: scfId is " + scfId);
		}

		boolean allowedCdlNoPresent = false;
		Boolean calledNoOverriding = null;
		if (destinationNumber != null) {
			allowedCdlNoPresent = (destinationNumber.getPresentationIndicator() == 1) ? true : false;
			calledNoOverriding = (destinationNumber.getNumberOverriding() == 0) ? false : null;
		}

		ServiceInteractionIndicatorsTwo serviceInterIndTwo = new ServiceInteractionIndicatorsTwo();
		serviceInterIndTwo.setAllowCdINNoPresentationInd(allowedCdlNoPresent);
		connectArg.setServiceInteractionIndicatorsTwo(serviceInterIndTwo);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: allowedCdlNoPresent=" + allowedCdlNoPresent + ", calledNoOverriding=" + calledNoOverriding);
		}

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(connectArg);
		opCode.add(InapOpCodes.CONNECT);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler for creating INAP CONNECT message for Port.
	 *
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createConnectForPort(CallData callData) throws Exception {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectForPort");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		PhoneNumber destinationNumber = (PhoneNumber) legData.get(LegDataAttributes.P_DESTINATION_NUMBER);

		NatureOfAddEnum natureOfAddrEnum = NatureOfAddEnum.fromInt(destinationNumber.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = NatureOfAddEnum.NATIONAL_NO;
		}

		Object calledType = callData.get(CallDataAttribute.P_CALLED_PARTY_TYPE);
		if (calledType != null && "1".equals(calledType)) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Change NOA to 126 for QuickLine subscribers");
			}
			natureOfAddrEnum = NatureOfAddEnum.ASSIST_SSPIP_ROUTE_ADDR;
		}

		byte[] calledPartyNum = CalledPartyNum.encodeCaldParty(destinationNumber.getAddress(), natureOfAddrEnum, NumPlanEnum.ISDN_NP, IntNwNumEnum.ROUTING_ALLWD);

		CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
		calledPartyNumber.setValue(calledPartyNum);
		DestinationRoutingAddress destRoutingAddress = new DestinationRoutingAddress();
		destRoutingAddress.initValue();
		destRoutingAddress.add(calledPartyNumber);

		ConnectArg connectArg = new ConnectArg();
		connectArg.setDestinationRoutingAddress(destRoutingAddress);

		ServiceInteractionIndicatorsTwo serviceInterIndTwo = new ServiceInteractionIndicatorsTwo();
		byte[] redirectReasonByteArr = RedirectReason.encodeRedirectionReason(RedirectionReasonEnum.RESERVERD);
		RedirectReason redirectReason = new RedirectReason();
		redirectReason.setRedirectionReasonEnum(RedirectionReasonEnum.RESERVERD);

		connectArg.setServiceInteractionIndicatorsTwo(serviceInterIndTwo);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(connectArg);
		opCode.add(InapOpCodes.CONNECT);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by the protocol handler for creating INAP CONNECT signal.
	 *
	 * @param callData represents an instance of CallData
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createConnectForTerm(CallData callData, Action action) throws Exception {
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);
		LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createConnectForTerm");
			logger.debug("[PH]:: Need to send CallingParty : " + action.isSendCallingParty());
		}

		String isSendOptionalParam = SipProtocolConfig.getConfigData(InapCS2ScfProtocolConfig.SEND_OPTIONAL_PARAMS_CONNECT);
		if(logger.isDebugEnabled()){
			logger.debug("[PH]:: Need to send OptionalParam in Connect : " + isSendOptionalParam);
		}
		PhoneNumber destinationNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_DESTINATION_NUMBER);

		NatureOfAddEnum natureOfAddrEnum = NatureOfAddEnum.fromInt(destinationNumber.getNatureOfAddress());

		if (natureOfAddrEnum == null) {
			natureOfAddrEnum = NatureOfAddEnum.NATIONAL_NO;
		}

		NumPlanEnum numberPlan = NumPlanEnum.fromInt(destinationNumber.getNumberingPlan());
		if (numberPlan == null) {
			numberPlan = NumPlanEnum.ISDN_NP;
		}

		byte[] calledPartyNum = CalledPartyNum.encodeCaldParty(destinationNumber.getAddress(),
				natureOfAddrEnum,
				numberPlan,
				IntNwNumEnum.ROUTING_ALLWD);

		CalledPartyNumber calledPartyNumber = new CalledPartyNumber();
		calledPartyNumber.setValue(calledPartyNum);
		DestinationRoutingAddress destRoutingAddress = new DestinationRoutingAddress();
		destRoutingAddress.initValue();
		destRoutingAddress.add(calledPartyNumber);

		ConnectArg connectArg = new ConnectArg();
		connectArg.setDestinationRoutingAddress(destRoutingAddress);

		boolean allowedCdlNoPresent = (destinationNumber.getPresentationIndicator() == 1);
		Boolean calledNoOverriding = !(destinationNumber.getNumberOverriding() == 0);

		if(PhConstants.TRUE.equals(isSendOptionalParam)){
			ServiceInteractionIndicatorsTwo serviceInterIndTwo = new ServiceInteractionIndicatorsTwo();
			serviceInterIndTwo.setAllowCdINNoPresentationInd(allowedCdlNoPresent);

			connectArg.setServiceInteractionIndicatorsTwo(serviceInterIndTwo);
		}

		String sendCalling=(String)leg2Data.get(LegDataAttributes.P_SEND_CALLING_PARTY);
		//Calling Party Number
		if(null != leg1Data.get(LegDataAttributes.P_CALLING_PARTY) && action.isSendCallingParty() && !PhConstants.FALSE.equals(sendCalling)) {
			logger.debug("[PH]:: Adding calling party details in Connect..");

			PhoneNumber callingPartyPhoneNumber = (PhoneNumber) leg1Data.get(LegDataAttributes.P_CALLING_PARTY);
			NatureOfAddEnum natureOfAddrEnumCallingParty = NatureOfAddEnum.fromInt(destinationNumber.getNatureOfAddress());

			NumPlanEnum numberPlanCallingParty = NumPlanEnum.fromInt(callingPartyPhoneNumber.getNumberingPlan());
			if (numberPlanCallingParty == null) {
				numberPlanCallingParty = NumPlanEnum.ISDN_NP;
			}

			AddPrsntRestEnum addressPresentationEnum = AddPrsntRestEnum.fromInt(callingPartyPhoneNumber.getAddressPresentationRestrictedIndicator());
			String isCallingpartyOverride = (String)leg2Data.get(LegDataAttributes.NP_CALLING_PARTY_OVERRIDE);
			if(StringUtils.isNotBlank(isCallingpartyOverride) && isCallingpartyOverride.equals(PhConstants.TRUE)){
				addressPresentationEnum = AddPrsntRestEnum.PRSNT_ALLWD;
				logger.debug("[PH]:: NP Calling party overrride is enabled by service. Setting AddPrsntRestEnum.PRSNT_ALLWD!");
			}

			byte[] callingPartyNum = CallingPartyNum.encodeCalgParty(callingPartyPhoneNumber.getAddress(),
					natureOfAddrEnumCallingParty,
					numberPlanCallingParty,
					addressPresentationEnum,
					ScreeningIndEnum.fromInt(callingPartyPhoneNumber.getScreeningIndicator()),
					NumIncmpltEnum.fromInt(callingPartyPhoneNumber.getNumberIncompleteIndicator()));
			CallingPartyNumber cgpn = new CallingPartyNumber();
			cgpn.setValue(callingPartyNum);
			connectArg.setCallingPartyNumber(cgpn);
		}

		//changes for OrigCalledPartyNumber

		PhoneNumber origDestinationNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.NP_ORIG_CALLED_PARTY_NUMBER);


		if(null != origDestinationNumber) {

			if(logger.isDebugEnabled()){
				logger.debug("[PH]:: Adding OrigCalledParty details in Connect..");
			}

			NatureOfAddEnum natureOfAddrForOrigCalledNumEnum = NatureOfAddEnum.fromInt(origDestinationNumber.getNatureOfAddress());
			if (natureOfAddrForOrigCalledNumEnum == null || natureOfAddrForOrigCalledNumEnum == NatureOfAddEnum.SPARE){
				natureOfAddrForOrigCalledNumEnum = NatureOfAddEnum.NATIONAL_NO;
			}

			NumPlanEnum numberPlanForOrigCalledNum = NumPlanEnum.fromInt(origDestinationNumber.getNumberingPlan());
			if (numberPlanForOrigCalledNum == null || numberPlanForOrigCalledNum == NumPlanEnum.SPARE){
				numberPlanForOrigCalledNum = NumPlanEnum.ISDN_NP;
			}

			if(logger.isDebugEnabled()){
				logger.debug("setting natureOfAddrForOrigCalledNumEnum value in ph :" + natureOfAddrForOrigCalledNumEnum);
				logger.debug("setting numberPlanForOrigCalledNum value in ph :" + numberPlanForOrigCalledNum);

			}
			byte[] origCalledPartyNum = OrigCalledPartyId.encodeOrigCalledPartyId(origDestinationNumber.getAddress(),
					natureOfAddrForOrigCalledNumEnum,
					numberPlanForOrigCalledNum,
					AddPrsntRestEnum.PRSNT_ALLWD); 

			OriginalCalledPartyID originalCalledPartyID = new OriginalCalledPartyID();
			originalCalledPartyID.setValue(origCalledPartyNum);

			connectArg.setOriginalCalledPartyID(originalCalledPartyID);	 

		}
		//end changes
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: allowedCdlNoPresent=" + allowedCdlNoPresent +
					", calledNoOverriding=" + calledNoOverriding);
		}

		// Ericsson specific parameters encoding for extension. 
		// This extension 4 parameter is used for carrying the charge number in 
		// the INAP CS1 CONNECT response
		String sendExtnField=InapCS2ScfProtocolConfig.getConfigData(InapCS2ScfProtocolConfig.EXTENSION4_ENABLED);

		if (PhConstants.TRUE.equals(sendExtnField)){
			byte[] encodedExt4 = encodeExtension4Parameter(callData);

			if(encodedExt4 != null){
				ExtensionField extField = new ExtensionField();

				// Criticality Type
				CriticalityType ct = new CriticalityType();
				ct.setValue(com.agnity.inapitutcs2.asngenerated.CriticalityType.EnumType.abort);
				extField.setCriticality(ct);

				extField.setValue(encodedExt4);

				// setting to 19 as per Ericson capture
				extField.setType(19L);

				List extFieldList= new ArrayList<ExtensionField>();
				extFieldList.add(extField);

				connectArg.setExtensions(extFieldList);		

				if(logger.isDebugEnabled()){
					logger.debug("Added Extension fields to COnenctArg");
				}
			}
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(InapOpCodes.CONNECT);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectArg);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler for parsing ERBCSM event.
	 *
	 * @param callData represents an instance of CallData
	 * @throws Exception
	 */
	public static void parseErbcsm(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {
		int dialogueId = (Integer) callData.get(CallDataAttribute.P_DIALOG_ID);
		TcapSession tcapSession = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getTcapProvider().getTcapSession(dialogueId);
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		leg2Data.set(LegDataAttributes.DISCONNECT_LEGID, "0");

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extract ERBCSM Argument from ERB");
		}
		EventReportBCSMArg eventReportBCSMArg;
		try {
			eventReportBCSMArg = (EventReportBCSMArg) InapOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Error in decoding ERB " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Error in InapOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: ERB parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Error in decoding ERB " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Exception in parse ERB", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, ERB parsing failure occured.", e, MESSAGE.ERB);
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside parseErbcsm");
		}

		com.agnity.inapitutcs2.asngenerated.Cause releaseCause = null;
		com.agnity.inapitutcs2.asngenerated.Cause failureCause = null;

		Event event = null;
		ServiceInterface serviceInterface = PhUtilityServices.getInstance((String)callData.get(CallDataAttribute.SERVICE_ID)).getServiceHandler();

		if (eventReportBCSMArg.isEventSpecificInformationBCSMPresent() || eventReportBCSMArg.getEventTypeBCSM() != null) {

			EventTypeBCSM eventTypeBCSM = eventReportBCSMArg.getEventTypeBCSM();
			EventSpecificInformationBCSM eventSpecificInfo = eventReportBCSMArg.getEventSpecificInformationBCSM();

			switch (eventTypeBCSM.getValue()) {
			case oAbandon: {

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Inside parseErbcsm OAbandan Received");
				}

				if (eventSpecificInfo != null && eventSpecificInfo.getOAbandon() != null && 
						eventSpecificInfo.getOAbandon().isAbandonCausePresent()) {
					releaseCause = eventSpecificInfo.getOAbandon().getAbandonCause();
				}
				InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_OABANDON_RCVD);
				parseReleaseCause(releaseCause, callData);
				event = new Event(EventType.EVENT_FAILURE, Protocol.ITUINAPCS1_SCF, CallDataAttribute.P_LEG2.name());
				leg2Data.set(LegDataAttributes.P_CAUSE_CODE, InapCS2ScfRelReasonCode.ERB_OABANDON_RCVD);
			}
			break;
			case oDisconnect: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Inside parseErbcsm oDisconnect Received");
				}
				if (eventSpecificInfo != null) {
					if (eventSpecificInfo.getODisconnectSpecificInfo().isReleaseCausePresent()) {
						releaseCause = eventSpecificInfo.getODisconnectSpecificInfo().getReleaseCause();
					}
				}
				parseReleaseCause(releaseCause, callData);

				// Check leg id to find if oDisconnect is received from A-party or B-Party
				// Application shall use this field to fill it in direction of release call in cdr.
				String legId=CallDataAttribute.P_LEG1.name();

				EventType evtType = EventType.EVENT_FAILURE;

				if (eventReportBCSMArg.isLegIDPresent()) {
					LegID legType = eventReportBCSMArg.getLegID();

					byte []legBuf = null;
					if(legType.isReceivingSideIDSelected()){
						legBuf = legType.getReceivingSideID().getValue();
					}else if (legType.isSendingSideIDSelected()){
						legBuf = legType.getSendingSideID().getValue();
					}

					if(logger.isDebugEnabled()){
						logger.debug("oDisconnect: isrxingside: " + legType.isReceivingSideIDSelected() + 
								", isSendingSide:" + legType.isSendingSideIDSelected() + 
								", id:" + CommonUtils.formatBytes(legBuf));
					}

					int legName= Byte.toUnsignedInt(legBuf[0]);//CommonUtils.formatBytesToInt(legBuf);
					
					if(logger.isDebugEnabled()){
						logger.debug("oDisconnect: Leg who sent it: " + legName); 
					}
					
					// receiving side leg is leg id = 2. Make sure in spectra the Receiving Side 
					// is defined for both leg and type
					if (legType.isReceivingSideIDSelected()) {
						// added for Telaverge 
						if(null != legData.get(LegDataAttributes.P_CALLING_PARTY) &&
								StringUtils.equalsIgnoreCase
								((String)InapCS2ScfProtocolConfig.getConfigData(InapCS2ScfProtocolConfig.INAPCS2_SMPP_SIMULATOR)
										, "TRUE")){
							PhoneNumber callingPartyPhoneNumber = (PhoneNumber) legData.get(LegDataAttributes.P_CALLING_PARTY);
							logger.error("[DACC_SMPP_CLIENT_SIMULATION_TRIGGER] " + callingPartyPhoneNumber.getAddress());
						}

						if (legName == 2) {
							legId = CallDataAttribute.P_LEG2.name();
							leg2Data.set(LegDataAttributes.DISCONNECT_LEGID,
									"2");
							leg2Data.set(
									LegDataAttributes.P_CAUSE_CODE,
									InapCS2ScfRelReasonCode.ODISCONNECT_RCVD_FROM_TERM);
						} else if (legName == 1) {
							legId = CallDataAttribute.P_LEG1.name();
							leg2Data.set(LegDataAttributes.DISCONNECT_LEGID,
									"1");
							leg2Data.set(
									LegDataAttributes.P_CAUSE_CODE,
									InapCS2ScfRelReasonCode.ODISCONNECT_RCVD_FROM_ORIG);
						}
						evtType = EventType.EVENT_FAILURE;

						// application may send next route in case of failure. 
						InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);

					}else{
						leg2Data.set(LegDataAttributes.DISCONNECT_LEGID, "1");
						leg2Data.set(LegDataAttributes.P_CAUSE_CODE, InapCS2ScfRelReasonCode.ODISCONNECT_RCVD_FROM_ORIG);
						InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);
					}
				}

				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_ODISC_RCVD);
				event = new Event(evtType, Protocol.ITUINAPCS2_SCF, legId);
			}
			break;

			case oCalledPartyBusy: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Inside parseErbcsm oCalledPartyBusy Received");
				}
				event = new Event(EventType.EVENT_FAILURE, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG2.name());
				parseBusy(callData, eventReportBCSMArg, tcapSession);
			}
			break;

			case oAnswer: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Inside parseErbcsm oAnswer Received");
				}
				InapCS2ScfProtocolUtil.startMaxCallDurationTimer(dialogueId, callData, tcapSession);
				InapCS2ScfProtocolUtil.startActivityTestTimer(dialogueId, callData, tcapSession);
				event = new Event(EventType.EVENT_SUCCESS, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG2.name());
				parseAns(callData, eventReportBCSMArg, tcapSession);
			}
			break;
			case oNoAnswer: {
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Inside parseErbcsm oNoAnswer Received");
				}

				event = new Event(EventType.EVENT_FAILURE, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG2.name());
				parseNoAns(callData, eventReportBCSMArg, tcapSession);
			}
			break;

			case routeSelectFailure: {
				if (logger.isDebugEnabled()) {
					logger.debug("Inside parseErbcsm routeSelectFailure Received");
				}

				failureCause = eventSpecificInfo.getRouteSelectFailureSpecificInfo().getFailureCause();
				parseReleaseCause(failureCause, callData);

				event = new Event(EventType.EVENT_FAILURE, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG2.name());
				parseRouteSelectFailure(callData, eventReportBCSMArg, tcapSession);
			}
			break;

			case  oMidCall: {
				if (logger.isDebugEnabled()) {
					logger.debug("Inside parseErbcsm oMidCall Received");
				}

				// Need to handle new event for for intimating MidCall
				event = new Event(EventType.EVENT_DTMF, Protocol.ITUINAPCS2_SCF, CallDataAttribute.P_LEG2.name());
				parseMidCall(callData, eventReportBCSMArg, tcapSession);
			}
			break;

			}

			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Inside parseErbcsm Notify Application of ERB Event");
			}

			// Check for Dialogure Primitive. If it has been received as TC_END then 
			// we should inform application for Drop Call
			int lastRxDialoguePrimitiveType = (Integer) callData.get(CallDataAttribute.P_DIALOG_PRIMITIVE_TYPE);
			if(lastRxDialoguePrimitiveType == TcapConstants.PRIMITIVE_END){

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Last Dialogue Rxed as TC_END, notifying Application");
				}

				InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.TERMINATED);
				InapCS2ScfProtocolHelper.notifyCallDropped(tcapSession, true);
			} else {
				ProtocolRouter.getInstance().execute(event, callData, serviceInterface);
			}

		}

	}

	/**
	 * @param callData
	 * @param eventReportBCSMArg
	 * @param tcapSession
	 */
	private static void parseMidCall(CallData callData,
			EventReportBCSMArg eventReportBCSMArg, TcapSession tcapSession) {

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData termLegData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside process parseoMidCall");
		}

		// setting up service state as SERVIC LOGIC as now application must send to disconnect leg 
		// followed by either release call or connect terminating end. 
		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);

	}

	/**
	 * This method is called by protocol handler for parsing releaseCause.
	 *
	 * @param releaseCause represents an instance of com.agnity.inapitutcs2.asngenerated.Cause
	 * @param callData     represents an instance of CallData
	 * @throws InvalidInputException
	 * @throws EnumParamOutOfRangeException
	 */
	public static void parseReleaseCause(com.agnity.inapitutcs2.asngenerated.Cause releaseCause, CallData callData) throws InvalidInputException, EnumParamOutOfRangeException {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside parseReleaseCause");
		}

		if (releaseCause != null) {
			Cause cause = Cause.decodeCauseVal(releaseCause.getValue());
			callData.set(CallDataAttribute.NP_RELEASE_REASON_VALUE, cause.getCauseValEnum().getCode());
			callData.set(CallDataAttribute.P_CAUSE_VALUE_FROM_MSG, PhConstants.TRUE);
		}
	}

	/**
	 * This method is called by the Protocol handler whenever an ErbBusy event
	 * is received.
	 *
	 * @param erbcsmArg   represents the instance of EventReportBCSMArg
	 * @param tcapSession represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] parseBusy(CallData callData, EventReportBCSMArg erbcsmArg, TcapSession tcapSession) throws ASNParsingException {

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData termLegData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside process ErbBusy");
		}

		if (erbcsmArg.isLegIDPresent()) {

			/*
			 * checking oBusy leg ID is receiving side is and matches leg 2
			 */
			LegID legId = erbcsmArg.getLegID();
			if (legId.isReceivingSideIDSelected()) {
				LegType legType = legId.getReceivingSideID();
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: got recieving side ID::" + legType.getValue()[0]);
				}
				/* matching only byte at index 0 since valid values as per
				 * standard are 1 and 2
				 */
				if ((legType.getValue()[0]) != (leg2Type.getValue()[0])) {
					logger.error("[PH]:: Invalid leg type in ERB busy");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_INVALID_LEG_TYPE);
					return InapCS2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_BUSY, FAILTYPE.DEFAULT);

				}// end if legType value check

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]::setting P_CAUSE_CODE :" + termLegData.get(LegDataAttributes.P_CAUSE_CODE));
					logger.debug("[PH]:: Valid leg ID in oBusy message");
				}//ends
			} else {
				logger.error("[PH]:: Invalid side Only ReceivingSideID expected in busy");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_INVALID_RECEIVING_SIDE);
				return InapCS2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_BUSY, FAILTYPE.DEFAULT);
			}
		}// @end if erbcsmArg.isLegIDPresent() no else as standard says assume default as 2

		//changes for alternate routing feature start
		termLegData.set(LegDataAttributes.P_CAUSE_CODE, InapCS2ScfRelReasonCode.CAUSE_CODE_BUSY);
		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.BUSY_RCVD_FROM_TERM);

		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever an ErbAnswer event
	 * is received.
	 *
	 * @param erbcsmArg   represents the instance of EventReportBCSMArg
	 * @param tcapSession represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] parseAns(CallData callData, EventReportBCSMArg erbcsmArg, TcapSession tcapSession) throws ASNParsingException {

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside process ErbAnswer");
		}

		if (erbcsmArg.isLegIDPresent()) {

			/*
			 *  checking oAnswer leg ID is receiving side is and matches leg 2
			 */
			LegID legId = erbcsmArg.getLegID();
			if (legId.isReceivingSideIDSelected()) {
				LegType legType = legId.getReceivingSideID();
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: got recieving side ID::" + legType.getValue()[0]);
				}
				/*
				 *  matching only byte at index 0 since valid values as per standard are 1 and 2
				 */

				if ((legType.getValue()[0]) != (leg2Type.getValue()[0])) {
					logger.error("[PH]:: Invalid leg type in ERB oAnswer");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_INVALID_LEG_TYPE);
					return InapCS2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_ANS, FAILTYPE.DEFAULT);

				}// end if legType value check
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Valid leg ID in oAns message");
				}
			} else {
				logger.error("[PH]:: Invalid side Only ReceivingSideID expected oAns");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_INVALID_RECEIVING_SIDE);
				return InapCS2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_ANS, FAILTYPE.DEFAULT);
			}
		}// @end if erbcsmArg.isLegIDPresent() no else as standard says assume default as 2

		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.TERM_CONNECTED);
		callData.set(CallDataAttribute.P_CALL_CONNECT_TIME, new Date());

		Date startTime = (Date) callData.get(CallDataAttribute.P_CALL_START_TIME);
		Date answerTime = (Date) callData.get(CallDataAttribute.P_CALL_CONNECT_TIME);
		long diffSeconds = (answerTime.getTime() - startTime.getTime())/ 1000 ;
		callData.set(CallDataAttribute.P_ALERTING_DURATION, Long.toString(diffSeconds));

		InapCS2ScfProtocolUtil.startCdrTimer(tcapSession);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Set attempted indicator to 0");
		}

		callData.set(CallDataAttribute.P_ATTEMPTED_IND, 0);
		return null;
	}

	/**
	 * This method is called by the Protocol handler whenever an ErbNoAnswer
	 * event is received.
	 *
	 * @param erbcsmArg   represents the instance of EventReportBCSMArg
	 * @param tcapSession represents the instance of TcapSession
	 * @return an array of Action Objects
	 */
	static Action[] parseNoAns(CallData callData, EventReportBCSMArg erbcsmArg, TcapSession tcapSession) throws ASNParsingException {

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData termLegData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside process ErbNoAnswer");
		}
		if (erbcsmArg.isLegIDPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Leg Id Present");
			}

			// checking oBusy leg ID is receiving side is and matches leg 2
			LegID legId = erbcsmArg.getLegID();
			if (legId.isReceivingSideIDSelected()) {
				LegType legType = legId.getReceivingSideID();
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: got recieving side ID::" + legType.getValue()[0]);
				}
				/*
				 * matching only byte at index 0 since valid values as per
				 * standard are 1 and 2
				 */
				if ((legType.getValue()[0]) != (leg2Type.getValue()[0])) {
					logger.error("[PH]:: Invalid leg type in ERB NoAns");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_INVALID_LEG_TYPE);
					return InapCS2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_NOANS, FAILTYPE.DEFAULT);

				}// end if legType value check

				//changes for alternate routing feature start
				termLegData.set(LegDataAttributes.P_CAUSE_CODE, InapCS2ScfRelReasonCode.CAUSE_CODE_NOANSWER);

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]::setting P_CAUSE_CODE :" + termLegData.get(LegDataAttributes.P_CAUSE_CODE));
					logger.debug("[PH]:: Valid leg ID in oNoAns message");
				}
			} else {
				logger.error("[PH]:: Invalid side Only ReceivingSideID expected in NoAns");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_INVALID_RECEIVING_SIDE);
				return InapCS2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_NOANS, FAILTYPE.DEFAULT);
			}// @end if isReceivingSideIDSelected
		}// @end if erbcsmArg.isLegIDPresent() no else as standard says assume default as 2

		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.NOANS_RCVD_FROM_TERM);
		return null;

	}

	/**
	 * This method is used to parse Route Select Failure ERB
	 *
	 * @param callData
	 * @param erbcsmArg
	 * @param tcapSession
	 * @return
	 * @throws ASNParsingException
	 */
	private static Action[] parseRouteSelectFailure(CallData callData, EventReportBCSMArg erbcsmArg, TcapSession tcapSession) throws ASNParsingException {

		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		LegData termLegData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside parseRouteSelectFailure");
		}

		if (erbcsmArg.isLegIDPresent()) {
			if (logger.isDebugEnabled()) {
				logger.debug("[PH]:: Leg Id Present");
			}

			/*
			 *  checking leg ID is receiving side is and matches leg 2
			 */
			LegID legId = erbcsmArg.getLegID();
			if (legId.isReceivingSideIDSelected()) {
				LegType legType = legId.getReceivingSideID();
				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: got recieving side ID::" + legType.getValue()[0]);
				}
				/*
				 *  matching only byte at index 0 since valid values as per
				 *  standard are 1 and 2
				 */

				if ((legType.getValue()[0]) != (leg2Type.getValue()[0])) {
					logger.error("[PH]:: Invalid leg type in RouteSelectFailure ");
					callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_INVALID_LEG_TYPE);
					return InapCS2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_ROUTESELECTFAILURE, FAILTYPE.DEFAULT);

				}// end if legType value check

				//Setting P_CAUSE_CODE for supporting Alternate Route
				termLegData.set(LegDataAttributes.P_CAUSE_CODE, InapCS2ScfRelReasonCode.ROUTESELECTFAILURE_RCVD_FROM_TERM);

				if (logger.isDebugEnabled()) {
					logger.debug("[PH]:: Setting P_CAUSE_CODE :" + termLegData.get(LegDataAttributes.P_CAUSE_CODE));
					logger.debug("[PH]:: Valid leg ID in RouteSelectFailure message");
				}
			} else {
				logger.error("[PH]:: Invalid side Only ReceivingSideID expected in Route Select Failure");
				callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ERB_INVALID_RECEIVING_SIDE);
				return InapCS2ScfProtocolHelper.getASNParsingFailureAction(tcapSession, MESSAGE.ERB_ROUTESELECTFAILURE, FAILTYPE.DEFAULT);
			}// @end if isReceivingSideIDSelected
		}// @end if erbcsmArg.isLegIDPresent() no else as standard says assume default as 2

		InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.SERVICE_LOGIC);
		callData.set(CallDataAttribute.NP_RELEASE_REASON_CODE, InapCS2ScfRelReasonCode.ROUTESELECTFAILURE_RCVD_FROM_TERM);
		return null;

	}

	/**
	 * This method is called by protocol handler for parsing ENC message received at SCP.
	 *
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param callData       represents an instance of CallData
	 * @throws ParameterOutOfRangeException
	 * @throws ASNParsingException
	 */
	public static void parseEnc(InvokeIndEvent invokeIndEvent, CallData callData) throws ParameterOutOfRangeException, ASNParsingException {

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside parseEnc");
			logger.debug("[PH]:: Extract ENC Argument from ENC");
		}
		EventNotificationChargingArg encArg = null;
		try {
			encArg = (EventNotificationChargingArg) InapOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Exception parsing ENC " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: ENC parsing exception", epore);
			}
			throw new ParameterOutOfRangeException("ENC parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Exception parsing ENC " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: ENC parsing exception", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure occured. in ENC", e, MESSAGE.ENC);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Leaving parse ENC");
		}
	}

	/**
	 * This method is called by protocol handler for creating a release call message.
	 *
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createReleaseCall(CallData callData, Action action) throws Exception {
		int releaseCauseValue = 31;
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG2);

		
		if(callData.get(CallDataAttribute.NP_RELEASE_REASON_VALUE) != null ){
			releaseCauseValue = (Integer) callData.get(CallDataAttribute.NP_RELEASE_REASON_VALUE);
		} else if (legData!=null &&  legData.get(LegDataAttributes.NP_RELEASE_REASON_VALUE) != null){
			try{
				releaseCauseValue = Integer.parseInt((String)legData
						.get(LegDataAttributes.NP_RELEASE_REASON_VALUE));
			}catch(Exception ex){ }
		}

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createReleaseCall");
			logger.debug("[PH]:: Release Cause Value is " + releaseCauseValue);
		}

		byte[] causeByteArr = Cause.encodeCauseVal(LocationEnum.TRANSIT_NETWORK, CodingStndEnum.ITUT_STANDARDIZED_CODING, CauseValEnum.fromInt(releaseCauseValue));

		com.agnity.inapitutcs2.asngenerated.Cause causeValue = new com.agnity.inapitutcs2.asngenerated.Cause(causeByteArr);

		ReleaseCallArg releaseCallArg = new ReleaseCallArg();
		releaseCallArg.selectInitialCallSegment(causeValue);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(releaseCallArg);
		opCode.add(InapOpCodes.RELEASE_CALL);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is called by protocol handler to parse
	 * ER signal received from switch.
	 *
	 * @param invokeIndEvent represents an instance of InvokeIndEvent
	 * @param callData       represents an instance of CallData
	 * @throws Exception
	 */
	public static void parseEntityRelease(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside parseEntity Release");
		}

		EntityReleasedArg erArg = null;

		erArg = (EntityReleasedArg) InapOperationsCoding.decodeOperation(invokeIndEvent);

		byte[] reason = null;
		if (erArg.isCSFailureSelected() && erArg.getCSFailure().isReasonPresent()) {
			reason = erArg.getCSFailure().getReason().getValue();
			com.agnity.inapitutcs2.datatypes.Reason decodedReason = com.agnity.inapitutcs2.datatypes.Reason.decodeReason(reason);
			if (decodedReason.getReasonEnum() == ReasonEnum.NOT_USED) {
				logger.error("[PH]:: Invalid ER reason");
				throw new ASNParsingException("[PH]:: Invalid ER Reason::" + decodedReason.getReasonEnum(), MESSAGE.DEFAULT);
			}
		}

		com.agnity.inapitutcs2.asngenerated.Cause releaseCause = null;
		if (erArg.isCSFailureSelected() && erArg.getCSFailure().isCausePresent()) {
			releaseCause = erArg.getCSFailure().getCause();

		}
		parseReleaseCause(releaseCause, callData);
	}


	/**
	 * This method is used to create Fci message
	 *
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] createFci(CallData callData) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createFci");
		}
		FurnishChargingMessage fciMsg = (FurnishChargingMessage)callData.get(CallDataAttribute.FCI);
		if(null != fciMsg) {
			FurnishChargingInformationArg furnishChargingInformationArg = new FurnishChargingInformationArg();
			FCIBillingChargingCharacteristics fCharacteristics = new FCIBillingChargingCharacteristics();
			byte[] fciChrInfo = fciMsg.encodeFurnishChargingInfo();

			fCharacteristics.setValue(fciChrInfo);
			furnishChargingInformationArg.setValue(fCharacteristics);

			LinkedList opObjects = new LinkedList();
			opObjects.add(furnishChargingInformationArg);
			LinkedList opCodes = new LinkedList();
			opCodes.add(InapOpCodes.FCI);
			LinkedList<byte[]> fciLinkList = InapOperationsCoding.encodeOperations(opObjects, opCodes);
			return fciLinkList.getFirst();
		} else {
			logger.error("[PH]:: Required FCI is null in calldata");
			throw new IllegalArgumentException("Required FCI is null in callData!");
		}
	}

	/**
	 * This method is used to parse Apply charging Report message
	 *
	 * @param invokeIndEvent
	 * @param callData
	 * @throws InvalidInputException
	 * @throws EnumParamOutOfRangeException
	 * @throws ASNParsingException
	 * @throws ParameterOutOfRangeException
	 */
	public static void parseAcr(InvokeIndEvent invokeIndEvent, CallData callData) throws InvalidInputException, EnumParamOutOfRangeException, ASNParsingException, ParameterOutOfRangeException {
		LegData legData = (LegData) callData.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Extract ACR Argument from ACR");
		}
		ApplyChargingReportArg applyChargingReportArg;
		try {
			applyChargingReportArg = (ApplyChargingReportArg) InapOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Error in decoding ACR " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Error in InapOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: IDP parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Error in decoding ACR " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Exception in parse ACR", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, ACR parsing failure occured.", e, MESSAGE.ACR);
		}


		if (logger.isDebugEnabled()) {
			logger.debug("Inside parseAcr");
		}
		Object legState = legData.get(LegDataAttributes.P_LEG_SS7_STATE);
		if (legState.equals(InapCallStates.TERM_CONNECTED)) {
			InapCS2ScfProtocolUtil.updateSS7CallState(legData, InapCallStates.TERM_CONNECTED_ACR);
		}
	}

	/**
	 * This method is used to parse the incoming SRR message from IPMS
	 * Currently this method is not used to commenting it for now
	 *
	 * @param invokeIndEvent
	 * @param callData
	 * @throws InvalidInputException
	 * @throws EnumParamOutOfRangeException
	 * @throws ASNParsingException
	 * @throws ParameterOutOfRangeException
	 */
	public static void parseSRR(InvokeIndEvent invokeIndEvent, CallData callData) throws InvalidInputException, EnumParamOutOfRangeException, ASNParsingException, ParameterOutOfRangeException {
		logger.debug("[PH]:: Extract SRR Argument from SRR");
		try {
			SpecializedResourceReportArg specializedResArg = (SpecializedResourceReportArg) InapOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Error in decoding SRR " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Error in InapOperationsCoding.decodeOperation", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: SRR parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Error in decoding SRR " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: Exception in parse SRR", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure, ACR parsing failure occured.", e, MESSAGE.SRR);
		}
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside parseSRR");
		}
	}


	/**
	 * This method is used to create continue component
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
		opCode.add(InapOpCodes.CONTINUE);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(connectArg);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}


	/**
	 * This method is used to parse Asist Resource Instructions request
	 *
	 * @param invokeIndEvent
	 * @param callData
	 * @throws Exception
	 */
	public static void parseAri(InvokeIndEvent invokeIndEvent, CallData callData) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside parseEnc");
			logger.debug("[PH]:: Extract ARI Argument from ARI");
		}
		AssistRequestInstructionsArg ariArg = null;
		try {
			ariArg = (AssistRequestInstructionsArg) InapOperationsCoding.decodeOperation(invokeIndEvent);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error("[PH]:: Exception parsing ARI " + epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: ARI parsing exception", epore);
			}
			throw new ParameterOutOfRangeException("[PH]:: ARI parsing failure occured, due to Enum paramter out of range." + epore.getMessage());
		} catch (Exception e) {
			logger.error("[PH]:: Exception parsing ARI " + e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error("[PH]:: ARI parsing exception", e);
			}
			throw new ASNParsingException("[PH]:: ASN Parsing Failure occured. in ARI", e, MESSAGE.ARI);
		}

		IPAvailable ipAva = ariArg.getIPAvailable();

	}


	/**
	 * This method is for creating INAP ER message.This method is not used as service necer sends ER....
	 * Creates ER messgae bytes tream and returns
	 *
	 * @param callData represents an instance of CallData
	 * @param action   represents an instance of Action
	 * @return an instance of byte[]
	 * @throws Exception
	 */
	public static byte[] createEntityRelease(CallData callData, Action action) throws Exception {

		int releasecause = (Integer) callData.get(CallDataAttribute.NP_RELEASE_REASON_VALUE);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createEntityRelease");
			logger.debug("[PH]:: Release Cause Value is " + releasecause);
		}

		byte[] causeByteArr = Cause.encodeCauseVal(LocationEnum.TRANSIT_NETWORK, CodingStndEnum.ITUT_STANDARDIZED_CODING, CauseValEnum.fromInt(releasecause));

		com.agnity.inapitutcs2.asngenerated.Cause causeValue = new com.agnity.inapitutcs2.asngenerated.Cause(causeByteArr);

		com.agnity.inapitutcs2.asngenerated.EntityReleasedArg.CSFailureSequenceType csFailureSeq = new com.agnity.inapitutcs2.asngenerated.EntityReleasedArg.CSFailureSequenceType();
		csFailureSeq.setCause(causeValue);

		com.agnity.inapitutcs2.asngenerated.Reason reason = new com.agnity.inapitutcs2.asngenerated.Reason();

		byte[] reasonByte = com.agnity.inapitutcs2.datatypes.Reason.encodeReason(ReasonEnum.OTHER_RESOURCE_RELEASED);
		reason.setValue(reasonByte);
		csFailureSeq.setReason(reason);

		com.agnity.inapitutcs2.asngenerated.CallSegmentID segId = new com.agnity.inapitutcs2.asngenerated.CallSegmentID();
		csFailureSeq.setCallSegmentID(segId);

		EntityReleasedArg erArg = new EntityReleasedArg();
		erArg.selectCSFailure(csFailureSeq);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		LinkedList<String> opCode = new LinkedList<String>();
		operationObjs.add(erArg);
		opCode.add(InapOpCodes.ER);

		LinkedList<byte[]> encodeList = InapOperationsCoding.encodeOperations(operationObjs, opCode);
		return encodeList.getFirst();
	}

	/**
	 * This method is used to create DisconenctLeg
	 * @param callData
	 * @param action
	 * @return
	 */
	public static byte[] createDisconnectLeg(CallData callData, Action action) throws Exception {
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Inside createDisconnectLeg");
		}
		DisconnectLegArg disconnectArg = new DisconnectLegArg();
		LegID legId = new LegID();
		legId.selectSendingSideID(InapCS2ScfProtocolUtil.leg2Type);

		disconnectArg.setLegToBeReleased(legId);

		LinkedList opObjects = new LinkedList();
		opObjects.add(disconnectArg);
		LinkedList opCodes = new LinkedList();
		opCodes.add(InapOpCodes.DISCONNECT_LEG);
		LinkedList<byte[]> disconnectLeg = InapOperationsCoding.encodeOperations(opObjects, opCodes);
		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:: Exit createDisconnectLeg");
		}
		return disconnectLeg.getFirst();
	}

	/**
	 * This method encodes Ericson exension parameter. The charge number is encoded in this 
	 * field
	 * @param callData
	 * @return
	 * @throws Exception
	 */
	public static byte[] encodeExtension4Parameter(CallData callData) throws Exception{
		if(logger.isDebugEnabled()){
			logger.debug("Inside encodeExtension4Parameter");
		}

		LegData leg2Data = (LegData) callData.get(CallDataAttribute.P_LEG2);

		// return if charge number is not set by an application
		if(leg2Data.get(LegDataAttributes.P_CHARGE_NUMBER) == null){
			if(logger.isDebugEnabled()){
				logger.debug("Exit encodeExtension4Parameter, charge number not set by app");
			}
			return null;
		}

		//get destination number properties
		PhoneNumber chargeNumber = (PhoneNumber) leg2Data.get(LegDataAttributes.P_CHARGE_NUMBER);

		// fixed NOA and NP. 
		byte[] chargeNumByteArray = CalledPartyNum.encodeCaldParty(chargeNumber.getAddress(),
				NatureOfAddEnum.NATIONAL_NO,
				NumPlanEnum.ISDN_NP,
				IntNwNumEnum.ROUTING_ALLWD);

		// Extension content shall be octet string as below
		// 
		byte [] retVal = new byte[chargeNumByteArray.length + 4];

		int index=0;
		retVal[index++] = (byte)0xa1; // Tag for value
		retVal[index++] = (byte) (chargeNumByteArray.length + 2); 
		retVal[index++] = 0x04; // Tag for charge number 
		retVal[index++] = (byte) chargeNumByteArray.length;

		for(int i=0; i< chargeNumByteArray.length; i++){
			retVal[index++] = chargeNumByteArray[i];
		}

		if(logger.isDebugEnabled()){
			logger.debug(" Encoded Extension:" + CommonUtils.formatBytes(retVal));
		}

		return retVal;
	}
}
