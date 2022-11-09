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
package com.agnity.ph.mapscf;

import jain.protocol.ss7.tcap.component.InvokeIndEvent;
import jain.protocol.ss7.tcap.component.ResultIndEvent;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.sip.SipApplicationSession;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.bn.exceptions.EnumParamOutOfRangeException;
import org.bn.types.NullObject;

import com.agnity.map.asngenerated.AddressString;
import com.agnity.map.asngenerated.AnyTimeInterrogationArg;
import com.agnity.map.asngenerated.AnyTimeInterrogationArgv1;
import com.agnity.map.asngenerated.AnyTimeInterrogationArgv2;
import com.agnity.map.asngenerated.AnyTimeInterrogationRes;
import com.agnity.map.asngenerated.AnyTimeModificationArg;
import com.agnity.map.asngenerated.AnyTimeModificationRes;
import com.agnity.map.asngenerated.AnyTimeSubscriptionInterrogationArg;
import com.agnity.map.asngenerated.AnyTimeSubscriptionInterrogationRes;
import com.agnity.map.asngenerated.CAMEL_SubscriptionInfo;
import com.agnity.map.asngenerated.DomainType;
import com.agnity.map.asngenerated.DomainType.EnumType;
import com.agnity.map.asngenerated.IMSI;
import com.agnity.map.asngenerated.ISDN_AddressString;
import com.agnity.map.asngenerated.InterrogationType;
import com.agnity.map.asngenerated.MT_smsCAMELTDP_CriteriaList;
import com.agnity.map.asngenerated.NoteSubscriberDataModifiedArg;
import com.agnity.map.asngenerated.RequestedInfo;
import com.agnity.map.asngenerated.RequestedInfov1;
import com.agnity.map.asngenerated.RequestedInfov2;
import com.agnity.map.asngenerated.RequestedSubscriptionInfo;
import com.agnity.map.asngenerated.SSInvocationNotificationArg;
import com.agnity.map.asngenerated.SendRoutingInfoArg;
import com.agnity.map.asngenerated.SendRoutingInfoRes;
import com.agnity.map.asngenerated.SubscriberIdentity;
import com.agnity.map.asngenerated.SubscriberState;
import com.agnity.map.asngenerated.TBCD_STRING;
import com.agnity.map.datatypes.AnyTimeInterrogationArgMap;
import com.agnity.map.datatypes.AnyTimeInterrogationResMap;
import com.agnity.map.datatypes.AnyTimeModificationArgMap;
import com.agnity.map.datatypes.AnyTimeModificationResMap;
import com.agnity.map.datatypes.AnyTimeSubscriptionInterrogationArgMap;
import com.agnity.map.datatypes.AnyTimeSubscriptionInterrogationResMap;
import com.agnity.map.datatypes.CellGidOrSaiOrLaiMap;
import com.agnity.map.datatypes.CellGlobalIdOrServiceAreaIdFixedLengthMap;
import com.agnity.map.datatypes.ISDNAddressStringMap;
import com.agnity.map.datatypes.LAIFixedLenDataType;
import com.agnity.map.datatypes.LocationInformationGprsMap;
import com.agnity.map.datatypes.LocationInformationMap;
import com.agnity.map.datatypes.LocationNumberMap;
import com.agnity.map.datatypes.NoteSubscriberDataModifiedArgMap;
import com.agnity.map.datatypes.PDPContextInfoListMap;
import com.agnity.map.datatypes.PDPContextInfoMap;
import com.agnity.map.datatypes.PsSubscriberStateMap;
import com.agnity.map.datatypes.SSInvocationNotificationArgMap;
import com.agnity.map.datatypes.RequestedInfoMap;
import com.agnity.map.datatypes.RequestedNodesMap;
import com.agnity.map.datatypes.SendRoutingInfoArgMap;
import com.agnity.map.datatypes.SendRoutingInfoResMap;
import com.agnity.map.datatypes.SubscriberIdentityMap;
import com.agnity.map.datatypes.SubscriberInfoMap;
import com.agnity.map.enumdata.DomainTypeMapEnum;
import com.agnity.map.enumdata.ExtentionMapEnum;
import com.agnity.map.enumdata.InterrogationTypeEnumMap;
import com.agnity.map.enumdata.NatureOfAddressMapEnum;
import com.agnity.map.enumdata.NumberPlanMapEnum;
import com.agnity.map.enumdata.RequestedNodesMapEnum;
import com.agnity.map.exceptions.InvalidInputException;
import com.agnity.map.operations.MapOpCodes;
import com.agnity.map.operations.MapOperationsCoding;
import com.agnity.map.parser.ParseAsnToUserType;
import com.agnity.map.parser.ParseUserToAsnType;
import com.agnity.map.util.MapFunctions;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.CommonUtils;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.common.exception.ASNParsingException;
import com.agnity.ph.common.exception.ASNParsingException.MESSAGE;
import com.agnity.ph.common.exception.CriticalityTypeException;
import com.agnity.ph.common.exception.ParameterOutOfRangeException;
import com.genband.tcap.provider.TcapSession;

/**
 * This class is used to do INAP protocol messages parsing
 *
 */
public class MapScfProtocolParser {

	private static Logger logger = Logger.getLogger(MapScfProtocolParser.class);

	/**
	 * This method is called by protocol handler to parse the NSDM received and
	 * populate the callData object from the received parameters.
	 *
	 * @param invokeIndEvent
	 *            represents an instance of InvokeIndEvent
	 * @param callData
	 *            represents an instance of CallData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseSSInvocationNotificationReq(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException,
	CriticalityTypeException, ParameterOutOfRangeException, Exception {
		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId
					+ ":: [PH] Extract SSInvocationNotificationArg Argument from SSIN");
		}

		SSInvocationNotificationArg  ssInvokeNotifyArg;
		try {

			ssInvokeNotifyArg = (SSInvocationNotificationArg) MapOperationsCoding
					.decodeOperation(invokeIndEvent.getParameters()
							.getParameter(), invokeIndEvent);

			SSInvocationNotificationArgMap result = ParseAsnToUserType
					.decodeAsnToSSINArg(ssInvokeNotifyArg);

			ISDNAddressStringMap  msisdn=result.getMsisdn();


			// extra check if called party contains f as filler then remove it. 
			// coming in Telus
			String modifiedMsisdn = msisdn.getAddressDigits();
			if(StringUtils.containsIgnoreCase(msisdn.getAddressDigits(), "f")){
				modifiedMsisdn =  StringUtils.removeEndIgnoreCase(msisdn.getAddressDigits(), "f");					
			}

			legData.set(LegDataAttributes.NP_MAP_MSISDN, modifiedMsisdn);
			legData.set(LegDataAttributes.NP_MAP_IMSI, result.getImsi());
			legData.set(LegDataAttributes.NP_MAP_SSEVENT_CODE, result.getSsCode());
			legData.set(LegDataAttributes.NP_MAP_QUERY_TYPE, MapOpCodes.MAP_SS_INVOCATION_NOTIFICATION);


			//legData.set(LegDataAttributes.SS_INVOKE_REQ, ssInvokeNotifyArg);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding SS_INVOKE_REQ "
					+ epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(
						dialogueId
						+ ":: [PH] Error in MapOperationsCoding.decodeOperation SSIN_REQ",
						epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId
					+ ":: [PH] SS_INVOKE_REQ parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding SSIN_REQ "
					+ e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse SSIN_REQ", e);
			}
			throw new ASNParsingException(
					dialogueId
					+ ":: [PH] ASN Parsing Failure, SSIN_REQ parsing failure occured.",
					e, MESSAGE.SS_INVOKE);
		}


		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] SS-Invoke parsed successfully");
		}
	}

	/**
	 * This method is called by protocol handler to parse the NSDM received and
	 * populate the callData object from the received parameters.
	 * 
	 * @param invokeIndEvent
	 *            represents an instance of InvokeIndEvent
	 * @param callData
	 *            represents an instance of CallData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseNSDM(InvokeIndEvent invokeIndEvent,
			CallData callData) throws ASNParsingException,
	CriticalityTypeException, ParameterOutOfRangeException, Exception {
		int dialogueId = invokeIndEvent.getDialogueId();
		LegData legData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);

		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId
					+ ":: [PH] Extract NoteSubscriberDataModified Argument from NSDM");
		}
		NoteSubscriberDataModifiedArg nsdmArg;
		try {

			nsdmArg = (NoteSubscriberDataModifiedArg) MapOperationsCoding
					.decodeOperation(invokeIndEvent.getParameters()
							.getParameter(), invokeIndEvent);

			NoteSubscriberDataModifiedArgMap result = ParseAsnToUserType
					.decodeAsnToNsdmArg(nsdmArg);

			legData.set(LegDataAttributes.NP_NSDM_REQ, nsdmArg);
		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding NSDM "
					+ epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(
						dialogueId
						+ ":: [PH] Error in MapOperationsCoding.decodeOperation",
						epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId
					+ ":: [PH] NSDM parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding NSDM "
					+ e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId + ":: [PH] Exception in parse NSDM", e);
			}
			throw new ASNParsingException(
					dialogueId
					+ ":: [PH] ASN Parsing Failure, IDP parsing failure occured.",
					e, MESSAGE.NSDM);
		}

		try {
			/*
			 * IDP.callingPartyNumber
			 */
			if (nsdmArg.isCamel_SubscriptionInfoPresent()) {

				CAMEL_SubscriptionInfo camelSubInfo = nsdmArg
						.getCamel_SubscriptionInfo();

				MT_smsCAMELTDP_CriteriaList criteriaList = camelSubInfo
						.getMt_smsCAMELTDP_CriteriaList();

			}

		} catch (Exception e) {
			logger.error(dialogueId + "::[PH] Error in parseNSDM "
					+ e.getMessage());
			throw new ASNParsingException(
					dialogueId
					+ "::[PH] ASN Parsing Failure: NSDM parsing failure occured.",
					e, MESSAGE.NSDM);
		}
		if (logger.isDebugEnabled()) {
			logger.debug(dialogueId + "::[PH] Initial NSDM parsed successfully");
		}
	}

	/**
	 * This method is used to create any time Interrogate Request
	 *
	 * @param callData
	 * @param action
	 * @return
	 * @throws Exception
	 */
	public static byte[] createAnyTimeInterrogate(CallData callData,
			Action action) throws Exception {

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		int dialogueId = (Integer) legData
				.get(LegDataAttributes.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId
					+ " : Inside createAnyTimeInterrogate");
		}

		Boolean currLocation=(Boolean)legData.get(LegDataAttributes.MAP_ATI_CURRENT_LOCATION);
		String msisdn=(String)legData.get(LegDataAttributes.MAP_ATI_MSISDN);
		String domainType= (String)legData.get(LegDataAttributes.MAP_ATI_DOMAIN_TYPE);
		String gsmScfAddr= (String)legData.get(LegDataAttributes.MAP_ATI_GSM_SCF_ADDRESS);

		if (logger.isDebugEnabled()) {
			logger.info("Action Leg Data Attributes values extracted are: ");
			logger.info("currLocation : "+currLocation);
			logger.info("msisdn : "+msisdn);
			logger.info("domainType : "+domainType);
			logger.info("domainType : "+domainType);
		}

		// Version instruct what all fields need to be set in
		// RequestedInfo in ATI. 
		// version1- LocationInformation, SubscriberState, requestedDomain, IMEI
		// version2 - LocationInformation, SubscriberState, currentLocation, requestedDomain, IMEI
		// Default  - all 
		// Due to an issue with BinaryNotes, encoded buffer is manipulated in 
		// CompatUtil.java in map/codec/src/com/agnity/map/util/CompatUtil.java
		int atiVersion = 1; 
		AnyTimeInterrogationArgMap request = (AnyTimeInterrogationArgMap) legData
				.get(LegDataAttributes.NP_ATI_REQ);
		request=defaultAnyTimeInterrogationArgMap(callData,msisdn,domainType,gsmScfAddr);

		if (request == null) {
			logger.error("Requested argument is null");
			throw new InvalidInputException("Requested object is null");
		}

		// Due to an issue with binary notes in encoding NULL Object in RequestedInfo 
		// different version of ATI have been created to meet customer requirmment. 
		// ATIv1 - this shall have LocationInformation, SubscriberState, Requested Domai and IMEI
		//         in RequestedInfo
		// ATIv2 - this shall have currentLocation additionaly to v1. 
		// ATI   - shall have all the parameters
		// ASN has been modified to generate ATIv1, V2 classes to handle customer need. 
		//Set current Location
		if(currLocation==true){
			atiVersion = 2;
		}

		// Get the subscriber Identity containing MS ISDN
		SubscriberIdentity subIdAsn = new SubscriberIdentity();
		ISDN_AddressString msisdnAddrAsn = new ISDN_AddressString();
		msisdnAddrAsn.setValue(new AddressString(request.getSubscriberIdentity().getMsisdn().encode()));
		subIdAsn.selectMsisdn(msisdnAddrAsn);

		// Get the gsmScfAddress
		ISDN_AddressString gsmScfAddrAsn = new ISDN_AddressString();
		gsmScfAddrAsn.setValue(new AddressString(request.getGsmScfAddress().encode()));

		AnyTimeInterrogationArgv1 atiArgAsnv1 = null;
		AnyTimeInterrogationArgv2 atiArgAsnv2 = null;
		AnyTimeInterrogationArg atiArgAsn     = null;

		if(atiVersion == 1){
			atiArgAsnv1 = new AnyTimeInterrogationArgv1();

			// RequestedInfo v1
			RequestedInfov1 reqInfoAsn = new RequestedInfov1();
			DomainType domain = new DomainType();
			domain.setValue(getRequestedDomain(domainType)); // domain type
			reqInfoAsn.setRequestedDomain(domain);
			atiArgAsnv1.setRequestedInfo(reqInfoAsn);

			// set Subscriber Identity and gsmScfAddress
			atiArgAsnv1.setSubscriberIdentity(subIdAsn);

			// Set GSMScfAddress
			atiArgAsnv1.setGsmSCF_Address(gsmScfAddrAsn);

		}else if(atiVersion == 2){
			atiArgAsnv2 = new AnyTimeInterrogationArgv2();

			// RequestedInfo v2
			RequestedInfov2 reqInfoAsn = new RequestedInfov2();
			DomainType domain = new DomainType();
			domain.setValue(getRequestedDomain(domainType)); // domain type
			reqInfoAsn.setRequestedDomain(domain);
			atiArgAsnv2.setRequestedInfo(reqInfoAsn);

			// set Subscriber Identity and gsmScfAddress
			atiArgAsnv2.setSubscriberIdentity(subIdAsn);

			// Set GSMScfAddress
			atiArgAsnv2.setGsmSCF_Address(gsmScfAddrAsn);
		}else{
			atiArgAsn = new AnyTimeInterrogationArg();

			// RequestedInfo
			RequestedInfo reqInfoAsn = new RequestedInfo();
			DomainType domain = new DomainType();
			domain.setValue(getRequestedDomain(domainType)); // domain type
			reqInfoAsn.setRequestedDomain(domain);

			// set Subscriber Identity and gsmScfAddress
			atiArgAsn.setSubscriberIdentity(subIdAsn);

			// Set GSMScfAddress
			atiArgAsn.setGsmSCF_Address(gsmScfAddrAsn);
		}

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(MapOpCodes.MAP_ANY_TIME_INTERROGATION);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		if(atiArgAsnv1 != null){
			operationObjs.add(atiArgAsnv1);
		}else if(atiArgAsnv2 != null){
			operationObjs.add(atiArgAsnv2);
		}else{
			operationObjs.add(atiArgAsn);
		}

		LinkedList<byte[]> encodeList = MapOperationsCoding.encodeOperations(
				operationObjs, opCode, true);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId+ 
					" : Exiting createAnyTimeInterrogate + encoded byte forversion:" +
					atiVersion + ", " +
					CommonUtils.formatBytes(encodeList.get(0)));
		}

		return encodeList.getFirst();
	}


	/**
	 * @param callData
	 * @param msisdn
	 * @param domainType
	 * @param gsmScfAddr2
	 * @return
	 */
	private static AnyTimeInterrogationArgMap defaultAnyTimeInterrogationArgMap(CallData callData,String msisdn, String domainType, String gsmScfAddr2) {
		logger.info("anyTimeInterrogationReq: Entr");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_INTERROGATE);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());

		LegData origLegData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));
		AnyTimeInterrogationArgMap atiArg =null;

		try {

			ISDNAddressStringMap msisdnAddr = new ISDNAddressStringMap();
			msisdnAddr.setAddressDigits(msisdn);
			msisdnAddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			msisdnAddr.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			msisdnAddr.setNumberPlan(NumberPlanMapEnum.ISDN_TELEPHONY_NUMBERING);

			SubscriberIdentityMap subId = new SubscriberIdentityMap(msisdnAddr);

			RequestedInfoMap reqInfo = new RequestedInfoMap();

			RequestedNodesMap reqNodes = new RequestedNodesMap();
			reqNodes.enableRequestedNodeAtIndex(RequestedNodesMapEnum.MME);

			if ("PSDomain".equalsIgnoreCase(domainType)) {
				reqInfo.setDomainType(DomainTypeMapEnum.PS_DOMAIN);
			} else if ("CSDomain".equalsIgnoreCase(domainType)) {
				reqInfo.setDomainType(DomainTypeMapEnum.CS_DOMAIN);
			}
			reqInfo.setRequestedNodes(reqNodes);

			ISDNAddressStringMap gsmScfAddr = new ISDNAddressStringMap();
			gsmScfAddr.setAddressDigits(gsmScfAddr2);
			gsmScfAddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmScfAddr.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmScfAddr.setNumberPlan(NumberPlanMapEnum.ISDN_TELEPHONY_NUMBERING);
			atiArg = new AnyTimeInterrogationArgMap(
					subId, reqInfo, gsmScfAddr);

		} catch (Exception ex) {
			logger.error("Error encoding ATI request: " + ex);
		}

		logger.info("anyTimeInterrogationReq: Exit");

		return atiArg;
	}

	/**
	 * @param domainType
	 * @return
	 * @throws Exception
	 */
	private static EnumType getRequestedDomain(String domainType) throws Exception {
		if(StringUtils.equalsIgnoreCase(domainType,"cs_Domain")) {
			if (logger.isDebugEnabled()) {
				logger.debug("cs_Domain is added as domainType in the Send Interrogation ATI Request ");
			}
			return DomainType.EnumType.cs_Domain;
		}
		else if (StringUtils.equalsIgnoreCase(domainType,"ps_Domain")) {
			if (logger.isDebugEnabled()) {
				logger.debug("ps_Domain is added as domainType in the Send Interrogation ATI Request ");
			}
			return DomainType.EnumType.ps_Domain;
		}
		else {
			logger.error("Invalid value of domainType found in request throwing exception for value :- "+domainType);
			throw new Exception("Invalid Domain type for Send Interrogation ATI Request");
		}
	}

	/**
	 * This method is used to create any time modification request
	 *
	 * @param callData
	 * @param action
	 * @return
	 * @throws Exception
	 */
	public static byte[] createAnyTimeModification(CallData callData,
			Action action) throws Exception {

		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);

		// Get the dialog data from Leg data
		int dialogueId = (Integer) origLegData
				.get(LegDataAttributes.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId
					+ " : Inside createAnyTimeModification");
		}


		AnyTimeModificationArgMap request =
				(AnyTimeModificationArgMap) origLegData
				.get(LegDataAttributes.NP_ATM_REQ);


		if (request == null) {
			logger.error("Requested argument is null");
			throw new InvalidInputException("Requested object is null");
		}

		AnyTimeModificationArg atmArgAsn = new AnyTimeModificationArg();

		// Get the subscriber Identity
		SubscriberIdentity subIdAsn = new SubscriberIdentity();
		IMSI imsiAsn = new IMSI();
		imsiAsn.setValue(new TBCD_STRING(request.getSubIdentity()
				.getImsi().encode()));
		subIdAsn.selectImsi(imsiAsn);

		// Get the gsmScfAddress
		ISDN_AddressString gsmScfAddrAsn = new ISDN_AddressString();
		gsmScfAddrAsn.setValue(new AddressString(request.getGsmScfAddr()
				.encode()));

		// RequestedSubscInfo
		RequestedSubscriptionInfo reqInfoAsn = new RequestedSubscriptionInfo();
		// Populate the request argument to be sent to network end point
		atmArgAsn.setSubscriberIdentity(subIdAsn);
		atmArgAsn.setGsmSCF_Address(gsmScfAddrAsn);



		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(MapOpCodes.MAP_ANY_TIME_MODIFICATION);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(atmArgAsn);

		LinkedList<byte[]> encodeList = MapOperationsCoding.encodeOperations(
				operationObjs, opCode, true);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId
					+ " : Exiting createAnyTimeSubsInterrogate");
		}

		return encodeList.getFirst();
	}



	/**
	 * This method is used to send routing information
	 *
	 * @param callData
	 * @param action
	 * @return
	 * @throws Exception
	 */
	public static byte[] createSendRoutingInformation(CallData callData,
			Action action) throws Exception {
		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);

		// Get the dialog data from Leg data
		int dialogueId = (Integer) origLegData
				.get(LegDataAttributes.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId
					+ " : Inside createSendRoutingInformation");
		}


		SendRoutingInfoArgMap request =
				(SendRoutingInfoArgMap) origLegData
				.get(LegDataAttributes.NP_SRI_REQ);

		if (request == null) {
			logger.error("Requested argument is null");
			throw new InvalidInputException("Requested object is null");
		}

		SendRoutingInfoArg sriArgAsn = new SendRoutingInfoArg();
		ISDN_AddressString msisdn = new ISDN_AddressString();
		msisdn.setValue(new AddressString(request.getMsisdn().encode()));
		sriArgAsn.setMsisdn(msisdn);

		InterrogationType intrAsn = new InterrogationType();
		if(request.getInterrogationType() == InterrogationTypeEnumMap.BASIC_CALL){
			logger.info("SRI Interrogation Type = BASIC_CALL");
			intrAsn.setValue(InterrogationType.EnumType.basicCall);
		}
		else if(request.getInterrogationType() == InterrogationTypeEnumMap.FORWARDING){
			logger.info("SRI Interrogation Type = FORWARDING");
			intrAsn.setValue(InterrogationType.EnumType.forwarding);
		}
		sriArgAsn.setInterrogationType(intrAsn);

		ISDN_AddressString gsmscfAddr = new ISDN_AddressString();
		gsmscfAddr.setValue(new AddressString(request.getGmscOrGsmScfAddress().encode()));
		sriArgAsn.setGmsc_OrGsmSCF_Address(gsmscfAddr);

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(MapOpCodes.MAP_SEND_ROUTING_INFO);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(sriArgAsn);

		LinkedList<byte[]> encodeList = MapOperationsCoding.encodeOperations(
				operationObjs, opCode, true);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId
					+ " : Exiting createAnyTimeSubsInterrogate");
		}

		return encodeList.getFirst();
	}

	/**
	 * This method is used to send any time subscriber interrogation request
	 *
	 * @param callData
	 * @param action
	 * @return
	 * @throws Exception
	 */
	public static byte[] createAnyTimeSubsInterrogate(CallData callData,
			Action action) throws Exception {

		LegData origLegData = (LegData) callData
				.get(CallDataAttribute.P_LEG1);


		// Get the dialog data from Leg data
		int dialogueId = (Integer) origLegData
				.get(LegDataAttributes.P_DIALOG_ID);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId
					+ " : Inside createAnyTimeSubsInterrogate");
		}


		AnyTimeSubscriptionInterrogationArgMap request =
				(AnyTimeSubscriptionInterrogationArgMap) origLegData
				.get(LegDataAttributes.NP_ATSI_REQ);

		if (request == null) {
			logger.error("Requested argument is null");
			throw new InvalidInputException("Requested object is null");
		}

		AnyTimeSubscriptionInterrogationArg atsiArgAsn = ParseUserToAsnType.encodeUserToAtsiArg(request);

		LinkedList<String> opCode = new LinkedList<String>();
		opCode.add(MapOpCodes.MAP_ANY_TIME_SUBSCRIPTION_INTERROGATION);

		LinkedList<Object> operationObjs = new LinkedList<Object>();
		operationObjs.add(atsiArgAsn);

		LinkedList<byte[]> encodeList = MapOperationsCoding.encodeOperations(
				operationObjs, opCode, true);

		if (logger.isDebugEnabled()) {
			logger.debug("[PH]:" + dialogueId
					+ " : Exiting createAnyTimeSubsInterrogate");
		}

		return encodeList.getFirst();
	}

	/**
	 * This method is used to parse ATI result
	 * @param tcapSession 
	 *
	 * @param resultIndEvent
	 * @param callData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseAnytimeInterrogateResult(
			TcapSession tcapSession, ResultIndEvent resultIndEvent, CallData callData)
					throws ASNParsingException, CriticalityTypeException,
					ParameterOutOfRangeException, Exception {

		AnyTimeInterrogationRes atiRes = null;
		int dialogueId = resultIndEvent.getDialogueId();
		LegData legData = null;
		String domainType="";

		if (callData.get(CallDataAttribute.P_LAST_CALL_ACTION) != null) {

			Action lastAction = (Action) callData
					.get(CallDataAttribute.P_LAST_CALL_ACTION);
			legData = (LegData) callData
					.get(CallDataAttribute.valueOf(lastAction.getLeg()));
			domainType= (String)legData.get(LegDataAttributes.MAP_ATI_DOMAIN_TYPE);
		} else {
			legData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
		}
		 SimpleDateFormat sdfDate = new SimpleDateFormat("yyyyMMddHHmmssSSS");//dd/MM/yyyy
		    Date now = new Date();
		    String strDate = sdfDate.format(now);
		    legData.set(LegDataAttributes.MAP_ATI_RES_RECEIVED_TIMESTAMP,strDate);
		
		  String sent= (String) legData.get(LegDataAttributes.MAP_ATI_REQ_SENT_TIMESTAMP);
		  
		  long sent1= Long.parseLong(sent);
		  long rec=Long.parseLong(strDate);
	        
			//SipApplicationSession appSession=MapScfProtocolUtil.getAppSession(tcapSession);
		//	HttpServletRequest req=(HttpServletRequest) callData.get(CallDataAttribute.NP_HTTP_REQ);
//			logger.error("ATI_RES:"+ tcapSession.getDialogueId()
//				+ "," + legData.get(LegDataAttributes.MAP_ATI_MSISDN) + ","+strDate+","
//				+ (rec - sent1));
      
		try {
			atiRes = (AnyTimeInterrogationRes) MapOperationsCoding
					.decodeOperation(resultIndEvent.getParameters()
							.getParameter(), resultIndEvent);
			if (logger.isDebugEnabled()) {
				logger.debug("ATI response is: "+atiRes);
			}

			AnyTimeInterrogationResMap result = ParseAsnToUserType
					.decodeAsnToAtiRes(atiRes);

			if (logger.isDebugEnabled()) {
				logger.debug("Obtained ATI Response as: "+result);
			}

			legData.set(LegDataAttributes.NP_ATI_RES, result);
			SubscriberInfoMap ssuser=null;
			
			if(atiRes.getSubscriberInfo()!=null){
			 ssuser = ParseAsnToUserType.decodeAsnToSubsInfo(atiRes.getSubscriberInfo());
			}

			Integer ageoflocation = null;
			ISDNAddressStringMap vlrnum = null;
			ISDNAddressStringMap mscnum = null;
			ISDNAddressStringMap sgsn = null;
			LocationInformationMap locationInformationMap = null;
			LocationInformationGprsMap locationInformationGprsMap = null;

			if(ssuser!=null &&ssuser.getLocationInfo() != null ){
				
				if(ssuser.getLocationInfo().getAgeOfLocation()!=null){
				ageoflocation = ssuser.getLocationInfo().getAgeOfLocation().getAgeOfLocation();
				}
				vlrnum = ssuser.getLocationInfo().getVlrNum();
				mscnum = ssuser.getLocationInfo().getMscNumber();
				locationInformationMap = ssuser.getLocationInfo();
			}

			if(ssuser!=null && ssuser.getLocationInformationGPRS() != null){
				sgsn = ssuser.getLocationInformationGPRS().getSgsnNumber();
			}

			CellGidOrSaiOrLaiMap cellGidOrSaiOrLaiMap = null;
			CellGlobalIdOrServiceAreaIdFixedLengthMap cellGidOrSaiFixedLen  = null;
			LAIFixedLenDataType laiFixedLen = null;
			if(locationInformationMap != null){
				cellGidOrSaiOrLaiMap = locationInformationMap.getCgidOrSaiOrLai();	
				if(cellGidOrSaiOrLaiMap != null){
					cellGidOrSaiFixedLen = cellGidOrSaiOrLaiMap.getCgidOrSaiFixedLen();
					laiFixedLen = cellGidOrSaiOrLaiMap.getLaiFixedLen();
				}
			}
			SubscriberState subscriberState = null;
			
			if(atiRes.getSubscriberInfo()!=null){
				subscriberState=atiRes.getSubscriberInfo().getSubscriberState();
			}
			PsSubscriberStateMap psSubscriberStateMap = null;

			if(ssuser!=null){
				psSubscriberStateMap = ssuser.getPsSubscriberState();
			if(psSubscriberStateMap != null){
				// apn-InUse
				if(psSubscriberStateMap.getPsPDPActiveNotReachableForPaging() != null){
					//System.out.println("Rajee1");
					PDPContextInfoListMap pdpContextInfoListMap = psSubscriberStateMap.getPsPDPActiveNotReachableForPaging();
					if(pdpContextInfoListMap.getValue() != null){
						java.util.Collection<PDPContextInfoMap> pdpContextInfoMapArray = pdpContextInfoListMap.getValue();
						for(PDPContextInfoMap elementInfo: pdpContextInfoMapArray){
							if(elementInfo.getApnInUse() != null){
								legData.set(LegDataAttributes.NP_ATI_APNU, elementInfo.getApnInUse().getAPN());
								System.out.println("NP_ATI_APNU: F" + elementInfo.getApnInUse().getAPN());
							}

							if(elementInfo.isPdpContextActive()){
								legData.set(LegDataAttributes.NP_ATI_PDPST, "T");
								System.out.println("NP_ATI_PDPST: T");
							}else{
								legData.set(LegDataAttributes.NP_ATI_PDPST, "F");
								System.out.println("NP_ATI_PDPST: F");
							}
							break;
						}
					}
				}
			}
			}

			com.agnity.map.asngenerated.LocationNumber locNum = null;
			boolean saiPresent = false;
			if(atiRes.getSubscriberInfo()!=null && atiRes.getSubscriberInfo().getLocationInformation() != null){
				locNum = atiRes
						.getSubscriberInfo().getLocationInformation()
						.getLocationNumber();

				// Hack added to check if sai is present. Since SAI_present is of type
				// NULL, even it is present (Tag 0x89) it is set to false. added a method
				// to check for tag 0x89 and if present then mark sai as present. 
				saiPresent = checkSaiPresentTag(resultIndEvent.getParameters().getParameter());
				//				saiPresent=atiRes
				//						.getSubscriberInfo().getLocationInformation().isSaiPresent();
			}

			LocationNumberMap locMap = null;
			if(locNum != null){
				locMap=LocationNumberMap.decode(locNum.getValue());
			}

			//LegData legData = new LegData();

			if(subscriberState != null){
				legData.set(LegDataAttributes.NP_ATI_SUBSCRIBER_STATE, getSubscribeState(subscriberState));
			}

			if(psSubscriberStateMap != null){
				legData.set(LegDataAttributes.NP_ATI_ET_PS_SUBSCRIBER_STATE_lOCATION_RESP, 
						getPsSubscribeState(psSubscriberStateMap));
				//System.out.println("NP_ATI_ET_PS_SUBSCRIBER_STATE_lOCATION_RESP: " + getPsSubscribeState(psSubscriberStateMap));
			}

			if(ageoflocation != null){
				legData.set(LegDataAttributes.NP_ATI_AGE_OF_LOCATION_INFORMATION_CS, ageoflocation);
			}

			if(vlrnum != null){
				String vlrNumber = StringUtils.removeEndIgnoreCase( vlrnum.getAddressDigits(), "f");
				legData.set(LegDataAttributes.NP_ATI_VLR_NUMBER, vlrNumber);
			}

			if(sgsn != null){
				String sgsnAddr = StringUtils.removeEndIgnoreCase(sgsn.getAddressDigits(), "f");
				legData.set(LegDataAttributes.NP_ATI_LOCATION_INFORMATION_GPRS, sgsnAddr);
				//System.out.println("NP_ATI_LOCATION_INFORMATION_GPRS: " + sgsnAddr);

			}

			if(mscnum != null){
				String MscNumber = StringUtils.removeEndIgnoreCase( mscnum.getAddressDigits(), "f");
				legData.set(LegDataAttributes.NP_ATI_MSC_NUMBER, MscNumber);	
			}

			if(locMap!= null){
				legData.set(LegDataAttributes.NP_ATI_LOCATION_NUMBER,locMap.getLocationNumber());
			}

			//for NP_ATI_CELLGIDORSAIORLAI_FLAG = 1 for CELL_ID
			//for NP_ATI_CELLGIDORSAIORLAI_FLAG = 2 for SAI
			//for NP_ATI_CELLGIDORSAIORLAI_FLAG = 3 for LAI
			if (cellGidOrSaiFixedLen != null) {
				String cellidSai=null;
				if(cellGidOrSaiFixedLen.getByteValue()!=null){
					cellidSai= 	(cellGidOrSaiFixedLen.getByteValue()).replaceAll("0x", "").replaceAll(" ", "");
				}
				legData.set(LegDataAttributes.NP_ATI_CELL_GLOB_ID_OR_SERV_AREA_OR_LAI_CONSTR, cellidSai);

				if (saiPresent) {
					if (logger.isDebugEnabled()) {
						logger.debug("SAI is present:");
					}
					legData.set(
							LegDataAttributes.NP_ATI_CELLGIDORSAIORLAI_FLAG, 2);
				} else {
					legData.set(
							LegDataAttributes.NP_ATI_CELLGIDORSAIORLAI_FLAG, 1);
				}

			} else if (laiFixedLen != null) {
				String laiFixed=null;
				if(laiFixedLen.getByteValue()!=null){
					laiFixed= 	(laiFixedLen.getByteValue()).replaceAll("0x", "").replaceAll(" ", "");
				}
				legData.set(LegDataAttributes.NP_ATI_CELL_GLOB_ID_OR_SERV_AREA_OR_LAI_CONSTR, laiFixed);
				legData.set(LegDataAttributes.NP_ATI_CELLGIDORSAIORLAI_FLAG,3);
			}

		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding ATI Result "
					+ epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(
						dialogueId
						+ ":: [PH] Error in MapOperationsCoding.decodeOperation",
						epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId
					+ ":: [PH]  parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding ATI Result "
					+ e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId
						+ ":: [PH] Exception in parse ATI Result", e);
			}
			throw new ASNParsingException(
					dialogueId
					+ ":: [PH] ASN Parsing Failure, ATI parsing failure occured.",
					e, MESSAGE.ATI);
		}
	}

	/**
	 * 
	 * @param subscriberState
	 * @return
	 */
	static Integer getSubscribeState(SubscriberState subscriberState){
		Integer state=-1;

		if(subscriberState.isAssumedIdleSelected()){
			state=0;
		}
		if(subscriberState.isCamelBusySelected()){
			state=1;
		}
		if(subscriberState.isNotProvidedFromVLRSelected()){
			state=2;
		}
		if(subscriberState.isNetDetNotReachableSelected()){
			state=3;
		}
		return state;
	}

	/**
	 * @param subscriberState
	 * @return
	 */
	static Integer getPsSubscribeState(PsSubscriberStateMap subscriberState){
		Integer state=-1;

		if(subscriberState.isNotProvidedFromSGSNorMME()){
			state = 1;
		}else if(subscriberState.isPsDetached()){
			state = 2;
		}else if(subscriberState.isPsAttachedNotReachableForPaging()){
			state = 3;
		}else if(subscriberState.isPsAttachedReachableForPaging()){
			state = 4;
		}else if(subscriberState.getPsPDPActiveNotReachableForPaging() != null){
			state = 5;
		}else if(subscriberState.getPsPDPActiveReachableForPaging() != null){
			state = 6;
		}else if(subscriberState.getNetDetNotReachable() != null){
			state = 7;
		}

		return state;
	}

	/**
	 * @param atiRes
	 * @return
	 */
	private static Integer getps_subscriberstate(AnyTimeInterrogationRes atiRes) {
		NullObject Ps_AttachedReachableForPaging = atiRes.getSubscriberInfo().getPs_SubscriberState()
				.getPs_AttachedReachableForPaging();
		NullObject NotProvidedFromSGSNorMME = atiRes.getSubscriberInfo().getPs_SubscriberState()
				.getNotProvidedFromSGSNorMME();
		NullObject Ps_Detached = atiRes.getSubscriberInfo().getPs_SubscriberState().getPs_Detached();
		NullObject Ps_AttachedNotReachableForPaging = atiRes.getSubscriberInfo().getPs_SubscriberState()
				.getPs_AttachedNotReachableForPaging();
		if (NotProvidedFromSGSNorMME != null) {
			return 0;
		}
		if (Ps_Detached != null) {
			return 1;
		}
		if (Ps_AttachedNotReachableForPaging != null) {
			return 2;
		}
		if (Ps_AttachedReachableForPaging != null) {
			return 3;
		}
		return null;
	}

	/**
	 * This method is used to parse ATSI result
	 *
	 * @param resultIndEvent
	 * @param callData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseAnytimeSubsInterrogateResult(
			ResultIndEvent resultIndEvent, CallData callData)
					throws ASNParsingException, CriticalityTypeException,
					ParameterOutOfRangeException, Exception {

		AnyTimeSubscriptionInterrogationRes atsiRes = null;
		int dialogueId = resultIndEvent.getDialogueId();
		LegData legData = null;

		if (callData.get(CallDataAttribute.P_LAST_CALL_ACTION) != null) {

			Action lastAction = (Action) callData
					.get(CallDataAttribute.P_LAST_CALL_ACTION);
			legData = (LegData) callData
					.get(CallDataAttribute.valueOf(lastAction.getLeg()));
		} else {
			legData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
		}

		try {

			atsiRes = (AnyTimeSubscriptionInterrogationRes) MapOperationsCoding
					.decodeOperation(resultIndEvent.getParameters()
							.getParameter(), resultIndEvent);

			AnyTimeSubscriptionInterrogationResMap result = ParseAsnToUserType
					.decodeAsnToAtsiRes(atsiRes);

			if (logger.isDebugEnabled()) {
				logger.debug("Obtained ATI Response as: "+result);
			}

			legData.set(LegDataAttributes.NP_ATSI_RES, result);

		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding ATSI Result "
					+ epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(
						dialogueId
						+ ":: [PH] Error in MapOperationsCoding.decodeOperation",
						epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId
					+ ":: [PH]  parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding ATSI Result "
					+ e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId
						+ ":: [PH] Exception in parse ATSI Result", e);
			}
			throw new ASNParsingException(
					dialogueId
					+ ":: [PH] ASN Parsing Failure, ATSI parsing failure occured.",
					e, MESSAGE.ATSI);
		}

	}

	/**
	 * This method is used to parse ATM result
	 *
	 * @param resultIndEvent
	 * @param callData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseAnytimeModificationResult(
			ResultIndEvent resultIndEvent, CallData callData)
					throws ASNParsingException, CriticalityTypeException,
					ParameterOutOfRangeException, Exception {

		AnyTimeModificationRes atmRes = null;
		int dialogueId = resultIndEvent.getDialogueId();
		LegData legData = null;

		if (callData.get(CallDataAttribute.P_LAST_CALL_ACTION) != null) {

			Action lastAction = (Action) callData
					.get(CallDataAttribute.P_LAST_CALL_ACTION);
			legData = (LegData) callData
					.get(CallDataAttribute.valueOf(lastAction.getLeg()));
		} else {
			legData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
		}

		try {
			atmRes = (AnyTimeModificationRes) MapOperationsCoding
					.decodeOperation(resultIndEvent.getParameters()
							.getParameter(), resultIndEvent);

			AnyTimeModificationResMap result = ParseAsnToUserType
					.decodeAsnToAtmRes(atmRes);

			if (logger.isDebugEnabled()) {
				logger.debug("Obtained ATM Response as: "+result);
			}

			legData.set(LegDataAttributes.NP_ATM_RES, result);

		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding ATM Result "
					+ epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(
						dialogueId
						+ ":: [PH] Error in MapOperationsCoding.decodeOperation",
						epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId
					+ ":: [PH] NSDM parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding ATM Result "
					+ e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId
						+ ":: [PH] Exception in parse ATM Result", e);
			}
			throw new ASNParsingException(
					dialogueId
					+ ":: [PH] ASN Parsing Failure, ATM parsing failure occured.",
					e, MESSAGE.ATM);
		}
	}

	/**
	 * This method is used to parse SRI result
	 *
	 * @param resultIndEvent
	 * @param callData
	 * @throws ASNParsingException
	 * @throws CriticalityTypeException
	 * @throws ParameterOutOfRangeException
	 * @throws Exception
	 */
	public static void parseSendRoutingInfoRes(ResultIndEvent resultIndEvent,
			CallData callData) throws ASNParsingException,
	CriticalityTypeException, ParameterOutOfRangeException, Exception {

		SendRoutingInfoRes sriRes = null;
		int dialogueId = resultIndEvent.getDialogueId();
		LegData legData = null;

		if (callData.get(CallDataAttribute.P_LAST_CALL_ACTION) != null) {

			Action lastAction = (Action) callData
					.get(CallDataAttribute.P_LAST_CALL_ACTION);
			legData = (LegData) callData
					.get(CallDataAttribute.valueOf(lastAction.getLeg()));
		} else {
			legData = (LegData) callData
					.get(CallDataAttribute.P_LEG1);
		}

		try {
			sriRes = (SendRoutingInfoRes) MapOperationsCoding.decodeOperation(
					resultIndEvent.getParameters().getParameter(),
					resultIndEvent);

			SendRoutingInfoResMap result = ParseAsnToUserType
					.decodeAsnToSriRes(sriRes);

			if (logger.isDebugEnabled()) {
				logger.debug("Obtained SRI Response as: "+result);
			}

			legData.set(LegDataAttributes.NP_SRI_RES, result);

		} catch (EnumParamOutOfRangeException epore) {
			logger.error(dialogueId + ":: [PH] Error in decoding SRI Result "
					+ epore.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(
						dialogueId
						+ ":: [PH] Error in MapOperationsCoding.decodeOperation",
						epore);
			}
			throw new ParameterOutOfRangeException(
					dialogueId
					+ ":: [PH] NSDM parsing failure occured, due to Enum paramter out of range."
					+ epore.getMessage());
		} catch (Exception e) {
			logger.error(dialogueId + ":: [PH] Error in decoding SRI Result "
					+ e.getMessage());
			if (logger.isInfoEnabled()) {
				logger.error(dialogueId
						+ ":: [PH] Exception in parse SRI Result", e);
			}
			throw new ASNParsingException(
					dialogueId
					+ ":: [PH] ASN Parsing Failure, SRI parsing failure occured.",
					e, MESSAGE.SRI);
		}
	}

	/**
	 * This method is used to check if Sai-present is set in LocationInformation()
	 * Its of type NULL and since due to BinaryNotes issue its not parsed properly. 
	 * Ths method is called if locationInforation has been received.
	 * This method checks for tag 0x89.Structure of Location Information is shown below
	 * LocationInformation ::= SEQUENCE {
	 * 		....
	 * 		sai-Present [9] NULL OPTIONAL,
	 *      ....
	 *  }  
	 * @param buffer
	 * @return
	 */
	private static boolean checkSaiPresentTag(byte[] buffer){
		boolean retVal = false;		
		if(logger.isDebugEnabled()){
			logger.debug("Inside checkSaiPresentTag: buffer:" + CommonUtils.formatBytes(buffer));
		}

		try {
			if(buffer[4] != (byte)0xa0){
				if(logger.isDebugEnabled()){
					logger.debug("Exit cehckSaiPresentTag: SubscriberInfo not present, returning: false");
				}
				return false;
			}

			int totalLen = buffer[5] & 0xFF;
			for (int i=6, j=2; j < totalLen; ++i){
				byte tag = buffer[i];
				if(tag == (byte)0x89){
					retVal = true;
					if(logger.isDebugEnabled()){
						logger.debug("cehckSaiPresentTag, SAI Tag found");
					}

					break;
				}

				j += buffer[i+1] + 1;
				i += buffer[i+1] + 1;
			}
			if(logger.isDebugEnabled()){
				logger.debug("Exit cehckSaiPresentTag, retVal:" + retVal);
			}
		}catch(Exception ex){

		}
		return retVal;
	}
	
	
//	public static void main(String[] args){
//		byte[] bytes=CommonUtils.formatIntToByte(3027206001719146);
//		
//		System.out.println("Exit checkSaiPresentTag, retVal:" + formatBytes(bytes));
//			
//	}
	
	public  static String formatBytes(byte data[]) {
		char output[] = new char[5 * (data.length)];
		int top = 0;

		for (int i = 0; i < data.length; i++) {
			
			output[top++] = hexcodes[(data[i] >> 4) & 0xf];
			output[top++] = hexcodes[data[i] & 0xf];
		}

		return (new String(output).trim());
	}
	
	public static final char hexcodes[] = { '0', '1', '2', '3', '4', '5', '6',
		'7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
}
