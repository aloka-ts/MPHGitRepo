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

package com.agnity.service.mphTestApp;

import jain.protocol.ss7.SccpUserAddress;
import jain.protocol.ss7.SignalingPointCode;
import jain.protocol.ss7.SubSystemAddress;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.Array;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.xbill.DNS.Message;
import org.xbill.DNS.NAPTRRecord;
import org.xbill.DNS.Name;
import org.xbill.DNS.RRset;
import org.xbill.DNS.Record;
import org.xbill.DNS.TextParseException;

import com.agnity.map.datatypes.AnyTimeInterrogationArgMap;
import com.agnity.map.datatypes.AnyTimeModificationArgMap;
import com.agnity.map.datatypes.AnyTimeSubscriptionInterrogationArgMap;
import com.agnity.map.datatypes.BasicServiceCodeMap;
import com.agnity.map.datatypes.ISDNAddressStringMap;
import com.agnity.map.datatypes.ImsiDataType;
import com.agnity.map.datatypes.RequestedInfoMap;
import com.agnity.map.datatypes.RequestedNodesMap;
import com.agnity.map.datatypes.RequestedSubscriptionInfoMap;
import com.agnity.map.datatypes.SendRoutingInfoArgMap;
import com.agnity.map.datatypes.SsCodeMap;
import com.agnity.map.datatypes.SsForBSCodeMap;
import com.agnity.map.datatypes.SubscriberIdentityMap;
import com.agnity.map.enumdata.AddlRequestedCAMELSubscriptionInfoMapEnum;
import com.agnity.map.enumdata.BearerServiceCodeMapEnum;
import com.agnity.map.enumdata.DomainTypeMapEnum;
import com.agnity.map.enumdata.ExtentionMapEnum;
import com.agnity.map.enumdata.InterrogationTypeEnumMap;
import com.agnity.map.enumdata.NatureOfAddressMapEnum;
import com.agnity.map.enumdata.NumberPlanMapEnum;
import com.agnity.map.enumdata.RequestedCAMELSubscriptionInfoMapEnum;
import com.agnity.map.enumdata.RequestedNodesMapEnum;
import com.agnity.map.enumdata.SupplementaryServicesMapEnum;
import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.AnnSpec;
import com.agnity.mphdata.common.AnnSpec.ANN_TYPE;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.ConnectionType;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.diameter.DiameterAVPAttribute;
import com.agnity.ph.diameter.DiameterAVPCodes;
import com.agnity.ph.diameter.DiameterAVPNames;
import com.agnity.ph.diameter.DiameterAVPType;
import com.agnity.ph.diameter.DiameterConstants;
import com.baypackets.ase.ra.diameter.ro.CreditControlAnswer;
import com.baypackets.ase.ra.diameter.ro.CreditControlRequest;
import com.baypackets.ase.ra.diameter.ro.RoResourceException;
import com.baypackets.ase.ra.diameter.ro.RoResourceFactory;
import com.baypackets.ase.ra.diameter.ro.enums.CCRequestTypeEnum;
import com.baypackets.ase.ra.diameter.ro.enums.RequestedActionEnum;
import com.baypackets.ase.util.AseAlarmUtil;

import fr.marben.diameter.DiameterAVP;
import fr.marben.diameter.DiameterException;
import fr.marben.diameter.DiameterGroupedAVP;
import fr.marben.diameter.DiameterInvalidArgumentException;
import fr.marben.diameter._3gpp.ro.DiameterRoMessageFactory;

public class MphTestAppMainImpl implements ServiceInterface {
	private static final MphTestAppMainImpl mphTestAppImplObj = new MphTestAppMainImpl();
	private static Logger logger = Logger.getLogger(MphTestAppMainImpl.class);

	private static final String DEST_INDEX = "DEST_INDEX";

	private static final String HLR_SSN = "HLR_SSN";
	private static final String HLR_SPC = "HLR_SPC";
	private static SccpUserAddress hlrSccpAddr;
	private static boolean isHlrInteractionReqd = false;
	private static String RBT_TIMER = "RBT_TIMER";
	private static String SSD_KEY = "SSD_KEY";
	private static long RBT_TIME_IN_MILLS = 4000;

	int callcount = 1;

	/**
	 * @return MphTestAppMainImpl instance
	 */
	public static MphTestAppMainImpl getInstance() {
		return mphTestAppImplObj;
	}

	public static MphTestAppMainImpl getInstance(AseAlarmUtil aseAlarmUtilRef) {
		return mphTestAppImplObj;
	}

	public static boolean initialize() {

		return true;
	}

	/**
	 * <p>
	 * To send 302 to orig:- 1. Create ACTION with ACTION_REDIRECT 2. Set LEG in
	 * ACTION to LEG1 which is orig leg 3. Set Destination phone number
	 * PhoneNumber in orig LegData 4. Set Protocol type in Action Refer method
	 * sendRedirect
	 * <p>
	 * To connect orig and term :- 1. Create Action ACTION_CONNECT 2. Create
	 * LegData object for destination and set in callData 3. Set destination
	 * LegData key in Action->Leg 4. Set destination Phone Number in destination
	 * LegData Refer connectTerm mPH will connect term leg and will send EVENT
	 * EVENT_SUCCESS with term leg in EVENT object. Then to connect orig and
	 * term 1. Create Action ACTION_RESYNC_CALL 2. Set Protocol in Action 3. Set
	 * destination/term leg name in Action mPH will connect orig and term and
	 * will return EVENT EVENT_RESYNC_SUCCESS with Leg->TERM leg
	 * <p>
	 * Disconnect/END the call 1.Create ACTION ACTION_END_CALL 2. Set Protocol
	 * in Action 3. Set SIP error code for all legs 4. No need to set Leg in
	 * action Refer dropCall() method
	 * <p>
	 * Play and disconnect. Play chargeable ann 1. Create action
	 * ACTION_CONNECT_MS 2. Set Action->Protocol = SIP, Action->Leg=LEG1 and
	 * return this action to mPH 3. Set LEG1
	 * LegData(PersistableData)->IS_CHARGEABLE_ANN attribute to TRUE 4. mPH will
	 * return EVENT_MS_SUCCESS event after connecting orig to MS 5. Create
	 * action ACTION_PLAY 6. mPH will return EVENT_PLAY_SUCCESS 7. Now disconect
	 * IVR connected to orig. Cretae action ACTION_DISCONNECT_MS 8. mPH will
	 * return EVENT_MS_DISCONNECT 9. Now drop the call. Refer
	 * connectOrigToIvrInConfirmed() and playAnnToOrig(), disconnectOrigIvr(),
	 * dropCall()
	 */
	@Override
	public Action[] processEvent(Event event, CallData callData) {

		if (logger.isDebugEnabled())
			logger.debug(" Entering :processEvent  handle new event" + event);
		Action[] actionArr = null;
		if (event != null && callData != null) {
			Protocol protocol = event.getProtocol();
			switch (protocol) {
			case SIP:
				actionArr = processSipEvent(event, callData);
				break;
			case ENUM:
				actionArr = processEnumEvent(event, callData);
				break;
			case DIAMETER:
				actionArr = processDiameterEvent(event, callData);
				break;
			case ITUINAPCS1_SCF:
				actionArr = MphTestAppInapCs1Impl.getInstance().processEvent(
						event, callData);
				break;
			case CAPV2_SCF:
				actionArr = MphTestAppCapV2Impl.getInstance().processEvent(
						event, callData);
				break;
			case AIN_SCF:
				actionArr = MphTestAppAinImpl.getInstance().processEvent(event,
						callData);
			case MAP_SCF:
				actionArr = MphTestAppCapV2Impl.getInstance().processEvent(
						event, callData);
				break;
			// case HTTP:
			// actionArr = processHttpEvent(event, callData);
			// break;
			default:
				logger.error("Unknown unsupported protocol event received returning NULL ... ");
				break;
			}
		}
		return actionArr;
	}

	// private Action[] processHttpEvent(Event event, CallData callData) {
	// // TODO Auto-generated method stub
	//
	// Action[] actionArr = new Action[1];
	//
	// /*
	// * Create LegData for term leg and set destination details on term
	// * legData
	// */
	//
	// Action action = new Action(Action.ActionType.ACTION_HTTP_RES);
	// callData.set(CallDataAttribute.NP_HTTP_CONTENT,
	// "This is my test PH cal");
	//
	// actionArr[0]=action;
	// return actionArr;
	// }

	private Action[] processDiameterEvent(Event event, CallData callData) {
		// TODO Auto-generated method stub
		Action[] actionArr = null;
		switch (event.getEventType()) {
		case EVENT_CCR_EVENT:
		case EVENT_CCR_INITIAL: {
			if (logger.isDebugEnabled())
				logger.debug(" Entering :processDiameterEvent  "
						+ event.getEventType().name());

			try {
				// return sendCCA("aaa", callData);

				return sendCCAUsingAttributes("1111", callData);
			} catch (DiameterInvalidArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (DiameterException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			// LegData leg1Data = (LegData)
			// callData.get(CallDataAttribute.P_LEG1);
			//
			// leg1Data.set(LegDataAttributes.DIAMETER_CCA_OUT_RETURN_CODE,
			// DIAMETER_USER_UNKNOWN);
			//
			// actionArr = new Action[1];
			//
			// Action action = new
			// Action(Action.ActionType.ACTION_SEND_CCR_ANSWER);
			// action.setProtocol(Protocol.DIAMETER);
			// action.setLeg(CallDataAttribute.P_LEG1.name());
			// actionArr[0] = action;
			// return actionArr;
			break;
		}
		case EVENT_CCA_RECEIVED: {
			if (logger.isDebugEnabled())
				logger.debug(" Entering :EVENT_CCA_RECEIVED ");
			// LegData leg1Data = (LegData)
			// callData.get(CallDataAttribute.P_LEG1);
			try {
				LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG2);
				CreditControlAnswer cca = (CreditControlAnswer) leg1
						.get(LegDataAttributes.DIAMETER_IN_CCR_RES);
				try {

					if (logger.isDebugEnabled())
						logger.debug(" Entering :EVENT_CCA_RECEIVED "
								+ cca.getCCRequestType());
					if (cca.getCCRequestType() == CCRequestTypeEnum
							.getCode(CCRequestTypeEnum.INITIAL_REQUEST)) {
						if (logger.isDebugEnabled())
							logger.debug(" sendCCRUpdate after  20 secs ---->");
						try {
							Thread.currentThread().sleep(20000);
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						return sendCCRUpdate("1234", callData);
					} else if (cca.getCCRequestType() == CCRequestTypeEnum
							.getCode(CCRequestTypeEnum.UPDATE_REQUEST)) {
						if (logger.isDebugEnabled())
							logger.debug(" sendCCRTerminate after 20 secs---->");
						try {
							Thread.currentThread().sleep(20000);
						} catch (InterruptedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
						return sendCCRTerminate("1234", callData);
					}
				} catch (RoResourceException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			} catch (DiameterInvalidArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (DiameterException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
		case EVENT_CCA_SENT: {
			if (logger.isDebugEnabled())
				logger.debug(" Entering :CCR Answer was sent successfully ");
			return actionArr;
		}
		}

		return actionArr;
	}

	/**
	 * Send CCR Terminate
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	private Action[] sendCCRTerminate(String origLegCallId, CallData callData) {
		// TODO Auto-generated method stub
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendCCRTerminate..");
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_SEND_CCR_TERMINATE);
		action.setProtocol(Protocol.DIAMETER);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		// leg1.set(LegDataAttributes.DIAMETER_CCR_REQUESTED_ACTION,RequestedActionEnum.DIRECT_DEBITING);

		if (callcount == 1) {
			leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
					"remoteRealm");
			callcount = 2;
		} else {
			callcount = 1;
			leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
					"remoteRealm");
		}

		leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
				"serverRealm");

		List<DiameterAVPAttribute> avpAttriList = new ArrayList<DiameterAVPAttribute>();

		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_INPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_CURRENCY_CODE, "2");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_EXPONENT, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_VALUE_DIGITS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_OUTPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_SERVICE_SPECIFIC_UNITS,
				"100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_TOTAL_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_TIME, "100");

		leg1.set(LegDataAttributes.DIAMETER_USU_REPORTING_REASON,
				"QUOTA_EXHAUSTED");
		leg1.set(LegDataAttributes.DIAMETER_USU_TARIFF_CHANGE_USAGE,
				"UNIT_BEFORE_TARIFF_CHANGE");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_TIME, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_INPUT_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_OUTPUT_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_TOTAL_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_SERVICE_SPECIFIC_UNITS,
				"1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_EVENT_CHARGING_TIMESTAMP, ""
				+ System.currentTimeMillis());

		List<String> avpCodes = new ArrayList<String>();
		avpCodes.add(DiameterAVPNames.Requested_Service_Unit);
		avpCodes.add(DiameterAVPNames.Used_Service_Unit);

		DiameterAVPAttribute avp1 = new DiameterAVPAttribute(
				DiameterAVPNames.Mutil_Service_Credit_Control,
				DiameterConstants.VENDOR_NAME_3GPP, DiameterAVPType.Grouped);
		avp1.setValue(avpCodes);

		avpAttriList.add(avp1);
		leg1.set(LegDataAttributes.DIAMETER_REQ_AVP_LIST, avpAttriList);

		actionArr[0] = action;
		return actionArr;
	}

	private Action[] processEnumEvent(Event event, CallData callData) {
		// TODO Auto-generated method stub

		if (logger.isDebugEnabled())
			logger.debug(" Entering :processEnumEvent ");
		Action[] actionArr = null;
		switch (event.getEventType()) {
		case EVENT_ENUM_RESPONSE_SENT: {
			if (logger.isDebugEnabled())
				logger.debug(" Entering :EVENT_ENUM_RESPONSE_SENT no further action needed ");
		}
			break;
		case EVENT_INITIAL: {
			if (logger.isDebugEnabled())
				logger.debug(" Entering :processInitial Enum request ");
			LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
			String aus = (String) leg1Data.get(LegDataAttributes.ENUM_AUS);
			String key = (String) leg1Data.get(LegDataAttributes.ENUM_KEY);

			List<NAPTRRecord> result = getResult(key);
			leg1Data.set(LegDataAttributes.ENUM_RESULT_RECORDS, result);

			actionArr = new Action[1];

			Action action = new Action(
					Action.ActionType.ACTION_SEND_ENUM_RESPONSE);
			action.setProtocol(Protocol.ENUM);
			action.setLeg(CallDataAttribute.P_LEG1.name());
			actionArr[0] = action;

			return actionArr;
		}
		case EVENT_ENUM_RESPONSE_RECEIVED: {
			if (logger.isDebugEnabled())
				logger.debug(" Entering :EVENT_ENUM_RESPONSE_RECEIVED no further action needed");

			LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
			List result = (List) leg1Data
					.get(LegDataAttributes.ENUM_RESULT_LIST);

			if (logger.isDebugEnabled())
				logger.debug(" Result list is " + result);
			return actionArr;
		}
		}

		return actionArr;

	}

	private List<NAPTRRecord> getResult(String key) {

		// NAPTR *****************start****************

		List<NAPTRRecord> records = new ArrayList<NAPTRRecord>();
		if (logger.isDebugEnabled())
			logger.debug(" Entering :getResult " + key);

		Name repl1 = null;
		Name repl2 = null;
		try {
			repl1 = new Name(key);// "0.0.0.0.5.6.7.8.7.6.1.cz.TOROONXN08.NGN.uslcr.telus.com"
			repl2 = new Name(".");

		} catch (TextParseException e) {
		}

		// !^.*$!sip:57@aarnet.edu.au!
		NAPTRRecord reco1 = new NAPTRRecord(repl1, 1, 12121, 1, 0, "u",
				"E2U+cic", "!^.*$!u.E2U+cic.0010!", repl2);
		NAPTRRecord reco2 = new NAPTRRecord(repl1, 1, 12121, 2, 0, "u",
				"E2U+cic", "!^.*$!u.E2U+cic.9997!", repl1);
		NAPTRRecord reco3 = new NAPTRRecord(repl1, 1, 12121, 2, 0, "u",
				"E2U+cic", "!^.*$!u.E2U+cic.9998!", repl1);
		NAPTRRecord reco4 = new NAPTRRecord(repl1, 1, 12121, 2, 0, "u",
				"E2U+cic", "!^.*$!u.E2U+cic.9999!", repl1);
		records.add(reco1);
		records.add(reco2);
		records.add(reco3);
		records.add(reco4);

		// ************ testing non Terminal NAPTR ends ********************

		if (logger.isDebugEnabled())
			logger.debug(" Returning  :getResult " + records);
		return records;

	}

	public Action[] anyTimeInterrogationReq(String origLegCallId,
			CallData callData) {
		logger.info("anyTimeInterrogationReq: Entr");
		if (logger.isDebugEnabled())
			logger.debug("Orig Call-Leg id = " + origLegCallId);

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_INTERROGATE);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());

		LegData origLegData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		try {
			logger.info("Setting the ATI Argument");
			ImsiDataType imsi = new ImsiDataType();
			imsi.setMobileCountryCode("460");
			imsi.setMobileNetworkCode("00");
			imsi.setMsin("13511078690");
			SubscriberIdentityMap subId = new SubscriberIdentityMap(imsi);

			RequestedInfoMap reqInfo = new RequestedInfoMap();

			RequestedNodesMap reqNodes = new RequestedNodesMap();
			reqNodes.enableRequestedNodeAtIndex(RequestedNodesMapEnum.MME);

			reqInfo.setDomainType(DomainTypeMapEnum.PS_DOMAIN);
			reqInfo.setRequestedNodes(reqNodes);

			ISDNAddressStringMap gsmScfAddr = new ISDNAddressStringMap();
			gsmScfAddr.setAddressDigits("9230001559");
			gsmScfAddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmScfAddr
					.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmScfAddr.setNumberPlan(NumberPlanMapEnum.DATA_NUMBERING_PLAN);

			AnyTimeInterrogationArgMap atiArg = new AnyTimeInterrogationArgMap(
					subId, reqInfo, gsmScfAddr);

			origLegData.set(LegDataAttributes.NP_ATI_REQ, atiArg);

			actionArr[0] = action;
		} catch (Exception ex) {
			logger.error("Error encoding ATI request: " + ex);
		}

		logger.info("anyTimeInterrogationReq: Exit");

		return actionArr;
	}

	public Action[] anyTimeSubsInterrogationReq(String origLegCallId,
			CallData callData) {
		logger.info("anyTimeSubsInterrogationReq: Entr");
		if (logger.isDebugEnabled())
			logger.debug("Orig Call-Leg id = " + origLegCallId);

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_INTERROGATE_SUBS);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());

		LegData origLegData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		try {
			// Subscriber Identity User object
			ImsiDataType imsi = new ImsiDataType();
			imsi.setMobileCountryCode("1227");
			imsi.setMobileNetworkCode("28");
			imsi.setMsin("0982321332");
			System.out.println("imsi  = " + imsi);
			SubscriberIdentityMap subId = new SubscriberIdentityMap(imsi);

			// Subscription Info User object
			RequestedNodesMap nodes = new RequestedNodesMap();
			nodes.enableRequestedNodeAtIndex(RequestedNodesMapEnum.SGSN);

			RequestedSubscriptionInfoMap reqInfo = new RequestedSubscriptionInfoMap();

			SsForBSCodeMap reqSsInfo = new SsForBSCodeMap(new SsCodeMap(
					SupplementaryServicesMapEnum.CALL_SESSION_RELATED_SS));

			reqSsInfo.setBasicServiceCode(new BasicServiceCodeMap(
					BearerServiceCodeMapEnum.ALLDATACIRCUITSYNCHRONOUS));

			reqInfo.setSsforBSCode(reqSsInfo);

			reqInfo.setReqCAMELSubsInfo(RequestedCAMELSubscriptionInfoMapEnum.D_CSI);
			reqInfo.setAddlReqCAMELSubsInfo(AddlRequestedCAMELSubscriptionInfoMapEnum.MT_SMS_CSI);

			// GSM ScfAddr User object
			ISDNAddressStringMap gsmScfAddr = new ISDNAddressStringMap();
			gsmScfAddr.setAddressDigits("3214793122");
			gsmScfAddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmScfAddr
					.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmScfAddr
					.setNumberPlan(NumberPlanMapEnum.ISDN_TELEPHONY_NUMBERING);

			AnyTimeSubscriptionInterrogationArgMap atsi = new AnyTimeSubscriptionInterrogationArgMap(
					subId, reqInfo, gsmScfAddr);

			origLegData.set(LegDataAttributes.NP_ATSI_REQ, atsi);
			callData.set(CallDataAttribute.P_LEG1, origLegData);
			actionArr[0] = action;
		} catch (Exception ex) {
			logger.error("Error encoding ATSI request: " + ex);
		}

		logger.info("anyTimeSubsInterrogationReq: Exit");
		return actionArr;
	}

	public Action[] anyTimeModificationReq(String origLegCallId,
			CallData callData) {
		logger.info("anyTimeModificationReq: Entr");
		if (logger.isDebugEnabled())
			logger.debug("Orig Call-Leg id = " + origLegCallId);

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_MODIFY_INFO);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		actionArr[0] = action;

		LegData origLegData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		try {
			ImsiDataType imsi = new ImsiDataType();
			imsi.setMobileCountryCode("460");
			imsi.setMobileNetworkCode("00");
			imsi.setMsin("13511078690");
			SubscriberIdentityMap subId = new SubscriberIdentityMap(imsi);

			ISDNAddressStringMap gsmScfAddr = new ISDNAddressStringMap();
			gsmScfAddr.setAddressDigits("9230001559"); // check for odd digits
			gsmScfAddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmScfAddr
					.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmScfAddr.setNumberPlan(NumberPlanMapEnum.DATA_NUMBERING_PLAN);

			AnyTimeModificationArgMap atmArg = new AnyTimeModificationArgMap(
					subId, gsmScfAddr);

			origLegData.set(LegDataAttributes.NP_ATM_REQ, atmArg);
			callData.set(CallDataAttribute.P_LEG1, origLegData);
			actionArr[0] = action;
		} catch (Exception ex) {
			logger.error("Error encoding ATM Request: " + ex);
		}

		logger.info("anyTimeModificationReq: Exit");
		return actionArr;
	}

	public Action[] sendRoutingInfoReq(String origLegCallId, CallData callData) {
		logger.info("sendRoutingInfoReq: Entr");

		if (logger.isDebugEnabled())
			logger.debug("Orig Call-Leg id = " + origLegCallId);

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_SEND_ROUTING_INFO);
		action.setProtocol(Protocol.MAP_SCF);
		action.setLeg(CallDataAttribute.P_LEG1.name());

		LegData origLegData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		try {
			ISDNAddressStringMap msisdn = new ISDNAddressStringMap();
			msisdn.setAddressDigits("9230001559"); // check for odd digits
			msisdn.setExtention(ExtentionMapEnum.NO_EXTENTION);
			msisdn.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			msisdn.setNumberPlan(NumberPlanMapEnum.DATA_NUMBERING_PLAN);

			// gsmorgsmscf address
			ISDNAddressStringMap gsmscfaddr = new ISDNAddressStringMap();
			gsmscfaddr.setAddressDigits("5234371559"); // check for odd digits
			gsmscfaddr.setExtention(ExtentionMapEnum.NO_EXTENTION);
			gsmscfaddr
					.setNatureOfNumber(NatureOfAddressMapEnum.INTERNATIONAL_NUMBER);
			gsmscfaddr.setNumberPlan(NumberPlanMapEnum.DATA_NUMBERING_PLAN);

			SendRoutingInfoArgMap sriArg = new SendRoutingInfoArgMap(msisdn,
					InterrogationTypeEnumMap.BASIC_CALL, gsmscfaddr);

			origLegData.set(LegDataAttributes.NP_SRI_REQ, sriArg);
			callData.set(CallDataAttribute.P_LEG1, origLegData);
			actionArr[0] = action;

		} catch (Exception ex) {
			logger.error("Error encoding SRI: " + ex);
		}
		logger.info("sendRoutingInfoReq: Exit");
		return actionArr;
	}

	private Action[] processSipEvent(Event event, CallData callData) {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		Action[] actionArr = null;
		logger.debug(origLegCallId + ":: Inside processEvent for SIP");
		logger.debug(origLegCallId + ":: Event is " + event);

		// LegData origLegData =
		// (LegData)callData.get(CallDataAttribute.valueOf(event.getLeg()));
		switch (event.getEventType()) {
		case EVENT_INITIAL: {
			logger.debug(origLegCallId + " Rceeived a new call for protocol "
					+ event.getProtocol());

			if (!isHlrInteractionReqd && hlrSccpAddr == null) {
				logger.info("Reading the HLR configuration");
				readHlrConfiguration();
			}

			MphTestAppServiceStateData serviceStateData = new MphTestAppServiceStateData();
			callData.set(CallDataAttribute.SSD_KEY, serviceStateData);
			LegData leg1Data = (LegData) callData.get(CallDataAttribute.P_LEG1);
			PhoneNumber dialedDigits = (PhoneNumber) leg1Data
					.get(LegDataAttributes.P_DIALED_DIGITS);

			if (isHlrInteractionReqd == true && hlrSccpAddr != null) {
				leg1Data.set(LegDataAttributes.P_HLR_SUA, hlrSccpAddr);
				logger.info("setting hlr in leg data to "
						+ (SccpUserAddress) leg1Data
								.get(LegDataAttributes.P_HLR_SUA));
				// Generating ATSI request
				logger.info("origCallId = " + origLegCallId);

				String dAddress = dialedDigits.getAddress();
				logger.info("Dialled Address is " + dAddress);
				if (dAddress.equals("123430")) {
					if (logger.isDebugEnabled())
						logger.debug("Sending ATSI request");
					actionArr = anyTimeSubsInterrogationReq(origLegCallId,
							callData);
				} else if (dAddress.equals("123431")) {
					if (logger.isDebugEnabled())
						logger.debug("Sending ATI request");
					actionArr = anyTimeInterrogationReq(origLegCallId, callData);
				} else if (dAddress.equals("123432")) {
					if (logger.isDebugEnabled())
						logger.debug("Sending ATM request");
					actionArr = anyTimeModificationReq(origLegCallId, callData);
				} else if (dAddress.equals("123433")) {
					if (logger.isDebugEnabled())
						logger.debug("Sending SRI request");
					actionArr = sendRoutingInfoReq(origLegCallId, callData);
				}
			} else {
				logger.info("HLR Not configured, interaction not required");
				// actionArr = sendRedirect(origLegCallId, callData);
				// actionArr = connectTerm(origLegCallId, callData,legData);
				// actionArr = connectOrigToIvrInConfirmed(origLegCallId,
				// callData);
				// actionArr = connectOrigToIvrInEarlyMedia(origLegCallId,
				// callData);

				try {
					// actionArr=sendCCRInitial(origLegCallId, callData);
					actionArr = sendCCRInitialUsingAttributes(origLegCallId,
							callData);

				} catch (DiameterInvalidArgumentException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (DiameterException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} // sendRedirect(origLegCallId, callData);
					// actionArr=sendEnumRequest(origLegCallId,callData);
			}

			break;
		}
		case EVENT_SUCCESS: {
			if (!CallDataAttribute.P_LEG1.equals(event.getLeg())) {
				LegData termLegData = (LegData) callData
						.get(CallDataAttribute.P_LEG2);
				logger.debug(origLegCallId + " 200 OK received from term "
						+ termLegData);

				return connectOrigAndTermAndDisconnectMs(origLegCallId,
						callData);
				// actionArr =connectOrigAndTerm(origLegCallId, callData);

				// logger.debug(origLegCallId
				// + " Creating action to connect orig and term "
				// + termLegData);
				// return connectOrigAndTerm(origLegCallId, callData);
				// logger.debug(origLegCallId
				// + " Creating action to connect term to MS "
				// + termLegData);
				// return connectTermToIvr(origLegCallId, callData);
			}
			break;
		}
		case EVENT_RESYNC_SUCCESS: {
			logger.debug(origLegCallId + " orig and term connected");
			// return dropCall(origLegCallId, callData, "503");

			// String provRespNotified = (String) callData
			// .getNonpersistableData(CallDataAttribute.NP_PROV_WO_SDP_NOTIFIED);

			// if(PhConstants.TRUE.equals(provRespNotified)){
			//
			// logger.debug(origLegCallId +
			// " provisional resp withut sdp was notified disconnect ms");
			// return disconnectOrigIvr(origLegCallId, callData, Protocol.SIP);
			// }
			return null;

		}

		case EVENT_PROVISIONAL_WO_SDP: {
			logger.debug(origLegCallId
					+ " provisional response without sdp received");

			actionArr = startNoSDPTimerForRBT(origLegCallId, callData,
					Protocol.SIP);
			return actionArr;

		}
		case EVENT_APP_TIMER_TIMEOUT: {
			logger.debug(origLegCallId
					+ " EVENT_APP_TIMER_TIMEOUT received for"
					+ event.getTimerName());

			if (event.getTimerName().equals(RBT_TIMER)) {

				logger.debug(origLegCallId
						+ " RBT timer timed out connect orig to ivr ");
				actionArr = connectOrigToIvrInConfirmed(origLegCallId, callData);
			}
			return actionArr;

		}
		case EVENT_SDP_RECEIVED: {
			logger.debug(origLegCallId + " Sdp is received");
			actionArr = disconnectOrigIvrAndStopRBTTimer(origLegCallId,
					callData, Protocol.SIP);

			return actionArr;

		}
		case EVENT_TRANSFER_INITIATED: {

			logger.debug(origLegCallId + "Transfer initiated by leg  "
					+ event.getLeg());

			// return rejectTransfer(origLegCallId,callData,event.getLeg());

			return performTransfer(origLegCallId, callData);

			// if (CallDataAttribute.P_LEG2.equals(event.getLeg())) {
			// logger.debug(origLegCallId
			// + "perofrm transfer  on on leg2 ");
			// return performTransfer(origLegCallId, callData);
			// } else{
			// logger.debug(origLegCallId
			// + "Do not process transfer received on unexpected leg ");
			// return rejectTransfer(origLegCallId,callData,event.getLeg());
			// }

		}
		case EVENT_TRANSFER_SUCCESS: {

			logger.debug(origLegCallId + "Call Transfer was success on   "
					+ event.getLeg());

			String transferedCallId = (String) callData
					.get(CallDataAttribute.P_TRANSFERED_CALL_ID);

			logger.debug(origLegCallId + " P_TRANSFERED_CALL_ID  is  "
					+ transferedCallId);

			break;
		}

		case EVENT_TRANSFER_FAILURE: {

			logger.debug(origLegCallId
					+ "Call Transfer was failed on drop call  "
					+ event.getLeg());

			return dropCall(origLegCallId, callData, 503, Protocol.SIP);

		}
		case EVENT_CALL_TRANSFER_DISCONNECT: {

			logger.debug(origLegCallId + "Call Transfer disconnect for   "
					+ event.getLeg());

			String transferedCallId = (String) callData
					.get(CallDataAttribute.P_TRANSFERED_CALL_ID);
			logger.debug(origLegCallId + " P_TRANSFERED_CALL_ID is  "
					+ transferedCallId);

			logger.debug(origLegCallId
					+ "call transfer disconnect received ... drop call  ");
			return dropCall(origLegCallId, callData, 503, Protocol.SIP);

		}
		case EVENT_DISCONNECT: {

			if (CallDataAttribute.P_LEG1.equals(event.getLeg())) {
				logger.debug(origLegCallId + " Call disconnected by Orig");
			} else {
				logger.debug(origLegCallId + " Call disconnected by Term");
			}
			return dropCall(origLegCallId, callData, 503, Protocol.SIP);
		}
		case EVENT_MS_SUCCESS: {

			MphTestAppServiceStateData serviceStateData = (MphTestAppServiceStateData) callData
					.get(CallDataAttribute.SSD_KEY);

			if (CallDataAttribute.P_LEG1.equals(event.getLeg())) {

				serviceStateData.setIsorigconnectedToIvr(true);
				logger.debug(origLegCallId
						+ "Orig connected to IVR. Now play ann to orig");
				return playAnnToOrig(origLegCallId, callData);
			} else {
				logger.debug(origLegCallId
						+ "Term connected to IVR. Now play ann to term");
				return playAnnToTerm(origLegCallId, callData);
			}

		}
		case EVENT_REDIRECT: {

			if (CallDataAttribute.P_LEG2.equals(event.getLeg())) {
				logger.debug(origLegCallId + "Redirect received on leg2 ");
				return tryRedirectContacts(origLegCallId, callData);
			} else {
				logger.debug(origLegCallId
						+ "Can not process Redirect received on unexpected leg ");
			}

		}
		case EVENT_PLAY_SUCCESS: {

			if (CallDataAttribute.P_LEG1.equals(event.getLeg())) {
				logger.debug(origLegCallId
						+ "Ann played to orig. Now disconnect IVR connected to orig");

				logger.debug(origLegCallId
						+ "Ann played to orig. Now disconnect SIP IVR connection ");
				return disconnectOrigIvr(origLegCallId, callData, Protocol.SIP);

			} else {
				logger.debug(origLegCallId
						+ "Ann played to term. Now disconnect IVR connected to term");
				return disconnectTermIvr(origLegCallId, callData);
			}
		}
		case EVENT_MS_DISCONNECT: {

			logger.debug(origLegCallId
					+ "Orig IVR connection disconnected do nothing ");

			// connectOrigAndTerm(origLegCallId, callData);

			// connectCRE(origLegCallId, callData, "1234");
			//
			// if (origLegCallId.equals(event.getLeg())) {
			// logger
			// .debug(origLegCallId +
			// "Orig IVR connection disconnected. Now drop call.");
			// return dropCall(origLegCallId, callData, "487");
			// } else {
			// logger
			// .debug(origLegCallId
			// +
			// "Term IVR connection disconnected. Now connect orig and term.");
			// return connectOrigAndTerm(origLegCallId, callData);
			// }
			break;

		}
		case EVENT_FAILURE: {

			if (CallDataAttribute.P_LEG2.equals(event.getLeg())) {
				logger.debug(origLegCallId + "Evet failure received on leg2 ");
			}

			// return tryRedirectContacts(origLegCallId,callData);
			//
			// if (MphTestAppSipServlet.getDestinationList() != null) {
			//
			// logger.debug(origLegCallId
			// + "trying to connect to another available destination !!!! ");
			// actionArr = connectTerm(origLegCallId, callData,legData);
			//
			// if (actionArr == null) {
			// logger.debug(origLegCallId
			// + "drop call now no action available ");
			// return dropCall(origLegCallId, callData, 503,
			// Protocol.SIP);
			// }
			// } else {
			//
			// logger.debug(origLegCallId
			// + "drop call now no other destination available ");
			// return dropCall(origLegCallId, callData, 503,
			// Protocol.SIP);
			// }
			//
			// } else {
			//
			// logger.debug(origLegCallId
			// + "drop call now failure received on orig leg");
			// return dropCall(origLegCallId, callData, 503, Protocol.SIP);
			// }
			break;
		}
		default:
			break;
		}
		return actionArr;

	}

	private Action connectCRE(String origLegCallId, CallData callData,
			String destinationNumber) {
		logger.debug(origLegCallId + " connectCRE");

		PhoneNumber destPhNumber = new PhoneNumber();
		destPhNumber.setAddress(destinationNumber);

		// Action[] actionArr = new Action[1];
		Action action = new Action(Action.ActionType.ACTION_CONNECT);
		action.setConnectionMode(Action.CONNECTIONMODE.EQSROUTING);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		LegData termLegData = new LegData();
		termLegData.set(LegDataAttributes.NP_NO_ANSWER_TIMER_DURATION, 25000);
		termLegData.set(LegDataAttributes.P_DESTINATION_NUMBER, destPhNumber);
		termLegData.set(LegDataAttributes.P_CONNECTION_TYPE,
				ConnectionType.TERM_CONNECTION);
		callData.set(CallDataAttribute.P_LEG2, termLegData);

		// actionArr[0] = action;

		/*
		 * Set Terminating IP and port. These will be set in 302 response
		 * contact header
		 */

		termLegData.set(LegDataAttributes.P_REMOTE_IP, "10.32.4.66");
		termLegData.set(LegDataAttributes.P_REMOTE_PORT, 6068);
		return action;

	}

	/**
	 * This method is used to reject transfer
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	private Action[] rejectTransfer(String origLegCallId, CallData callData,
			String leg) {

		logger.debug(origLegCallId + " rejectTransfer");
		Action[] actionArr = new Action[1];
		Action action = new Action(Action.ActionType.ACTION_REJECT_TRANSFER);
		action.setProtocol(Protocol.SIP);
		action.setLeg(leg);
		actionArr[0] = action;
		return actionArr;
	}

	/**
	 * This method is used to allow transfer
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	private Action[] performTransfer(String origLegCallId, CallData callData) {

		logger.debug(origLegCallId + " performTransfer");
		Action[] actionArr = new Action[1];
		Action action = new Action(Action.ActionType.ACTION_ALLOW_TRANSFER);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		actionArr[0] = action;
		return actionArr;
	}

	private Action[] tryRedirectContacts(String origLegCallId, CallData callData) {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " tryRedirectContacts");
		Action[] actionArr = null;
		actionArr = new Action[1];
		String destinationNumber = "123412";
		PhoneNumber destPhNumber = new PhoneNumber();
		destPhNumber.setAddress(destinationNumber);
		Action action = new Action(
				Action.ActionType.ACTION_TRY_REDIRECT_CONTACTS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		actionArr[0] = action;

		return actionArr;
	}

	/*
	 * This method creates and returns ACTION for mPH to send SIP 302 response
	 */
	public Action[] sendRedirect(String origLegCallId, CallData callData) {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendRedirect() Send 302 to orig");
		Action[] actionArr = null;
		actionArr = new Action[1];
		String destinationNumber = "123412";
		PhoneNumber destPhNumber = new PhoneNumber();
		destPhNumber.setAddress(destinationNumber);
		Action action = new Action(Action.ActionType.ACTION_REDIRECT);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		leg1.set(LegDataAttributes.P_DESTINATION_NUMBER, destPhNumber);
		actionArr[0] = action;

		/*
		 * Set Terminating IP and port. These will be set in 302 response
		 * contact header
		 */
		leg1.set(LegDataAttributes.P_REMOTE_IP, "10.32.5.137");
		leg1.set(LegDataAttributes.P_REMOTE_PORT, 5040);
		return actionArr;
	}

	/*
	 * This method creates and returns ACTION for mPH to send SIP 302 response
	 */
	public Action[] sendCCRInitial(String origLegCallId, CallData callData)
			throws DiameterInvalidArgumentException, DiameterException {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendCCR..");
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_SEND_CCR_INITIAL);
		action.setProtocol(Protocol.DIAMETER);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		leg1.set(LegDataAttributes.DIAMETER_CCR_REQUESTED_ACTION,
				RequestedActionEnum.CHECK_BALANCE);

		if (callcount == 1) {
			leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
					"remoteRealm");
			callcount = 2;
		} else {
			callcount = 1;
			leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
					"remoteRealm");
		}

		leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
				"serverRealm");

		List<DiameterAVPAttribute> avpAttriList = new ArrayList<DiameterAVPAttribute>();

		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				getApplicationName()).getDiameterRoMesageFactory();
		List<DiameterAVP> uvAVP = roMsgfactory.createUnitValueAVP(1000L);
		List<DiameterAVP> ccmAVP = roMsgfactory.createCCMoneyAVP(uvAVP, 1000L);

		// createRequestedServiceUnitAVP(java.lang.Long cCTime,
		// java.util.List<DiameterAVP> cCMoney, java.math.BigInteger
		// cCTotalOctets, java.math.BigInteger cCInputOctets,
		// java.math.BigInteger cCOutputOctets, java.math.BigInteger
		// cCServiceSpecificUnits)

		BigInteger bigInt = new BigInteger("1000");
		List<DiameterAVP> reqSvcUnitAVP = roMsgfactory
				.createRequestedServiceUnitAVP(System.currentTimeMillis(),
						ccmAVP, bigInt, bigInt, bigInt, bigInt);

		// createGrantedServiceUnitAVP(java.lang.String tariffChangeUsage,
		// int CCTime,
		// int valueDigits,
		// int exponent,
		// int currencyCode,
		// int CCTotalOctets,
		// int CCInputOctets,
		// int CCOutputOctets,
		// int CCServiceSpecificUnits)

		List<DiameterAVP> reqGrantedSVCUnitAVP = roMsgfactory
				.createGrantedServiceUnitAVP("" + System.currentTimeMillis(),
						100L, ccmAVP, bigInt, bigInt, bigInt, bigInt);

		// createUsedServiceUnitAVP(java.lang.String reportingReason,
		// java.lang.String tariffChangeUsage, java.lang.Long cCTime,
		// java.math.BigInteger cCTotalOctets,
		// java.math.BigInteger cCInputOctets, java.math.BigInteger
		// cCOutputOctets, java.math.BigInteger cCServiceSpecificUnits,
		// java.lang.String[] eventChargingTimeStamp)
		String[] timestamps = new String[] { "" + System.currentTimeMillis() };
		List<DiameterAVP> usedSVCUnitAVP = roMsgfactory
				.createUsedServiceUnitAVP("QUOTA_EXHAUSTED",
						"UNIT_BEFORE_TARIFF_CHANGE", (long) 100, bigInt,
						bigInt, bigInt, bigInt, timestamps);
		List<DiameterAVP>[] usuarrray = new List[1];
		usuarrray[0] = usedSVCUnitAVP;

		List<DiameterAVP> finalUIUnitAVP = roMsgfactory
				.createFinalUnitIndicationAVP("TERMINATE");
		//
		// // DiameterAVP[] avparr= new DiameterAVP[1];
		// // avparr[0]=usedSVCUnitAVP.get(0);
		//
		// Array[] istarrya= new Array[1];
		// ArrayList<DiameterAVP>[] arr=new ArrayList<DiameterAVP>[1];
		// //
		// istarrya[0]=usedSVCUnitAVP;
		//

		// Used-Service-Unit

		// <!-- Ref = [base - RFC4006] 8.19 -->
		// <avp name="Used-Service-Unit" code="446" mandatory="must"
		// may-encrypt="yes"
		// vendor-specific="no" >
		// <grouped>
		// <gavp name="Reporting-Reason"/>
		// <gavp name="Tariff-Change-Usage"/>
		// <gavp name="CC-Time"/>
		// <gavp name="CC-Total-Octets"/>
		// <gavp name="CC-Input-Octets"/>
		// <gavp name="CC-Output-Octets"/>
		// <gavp name="CC-Service-Specific-Units"/>
		// <gavp name="Event-Charging-TimeStamp"/>
		// </grouped>
		// </avp>

		List<DiameterAVP> multiSCCUnitAVP = roMsgfactory
				.createMultipleServicesCreditControlAVP(reqGrantedSVCUnitAVP,
						reqSvcUnitAVP, usuarrray, null, null, null, null, null,
						finalUIUnitAVP, null, null, null, null, null, null,
						null, null, null, null, null, null, null, null, null);
		//
		DiameterAVPAttribute avp1 = new DiameterAVPAttribute(
				DiameterAVPNames.Mutil_Service_Credit_Control,
				DiameterConstants.VENDOR_NAME_3GPP, DiameterAVPType.Grouped);
		avp1.setValue(multiSCCUnitAVP);

		avpAttriList.add(avp1);
		leg1.set(LegDataAttributes.DIAMETER_REQ_AVP_LIST, avpAttriList);

		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * This method creates and returns ACTION for mPH to send SIP 302 response
	 */
	public Action[] sendCCRInitialUsingAttributes(String origLegCallId,
			CallData callData) throws DiameterInvalidArgumentException,
			DiameterException {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendCCRInitial. first");
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_SEND_CCR_INITIAL);
		action.setProtocol(Protocol.DIAMETER);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
//		leg1.set(LegDataAttributes.DIAMETER_CCR_REQUESTED_ACTION,
//				RequestedActionEnum.CHECK_BALANCE);

		if (callcount == 1) {
			leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
					"remoteRealm");
			callcount = 2;
		} else {
			callcount = 1;
			leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
					"remoteRealm");
		}

		leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
				"serverRealm");

		List<DiameterAVPAttribute> avpAttriList = new ArrayList<DiameterAVPAttribute>();

		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_INPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_CURRENCY_CODE, "2");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_EXPONENT, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_VALUE_DIGITS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_OUTPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_SERVICE_SPECIFIC_UNITS,
				"100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_TOTAL_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_TIME, "100");

		List<String> avpCodes = new ArrayList<String>();
		avpCodes.add(DiameterAVPNames.Requested_Service_Unit);

		DiameterAVPAttribute avp1 = new DiameterAVPAttribute(
				DiameterAVPNames.Mutil_Service_Credit_Control,
				DiameterConstants.VENDOR_NAME_3GPP, DiameterAVPType.Grouped);
		avp1.setValue(avpCodes);

		avpAttriList.add(avp1);
		leg1.set(LegDataAttributes.DIAMETER_REQ_AVP_LIST, avpAttriList);

		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * This method creates and returns ACTION for mPH to send SIP 302 response
	 */
	public Action[] sendCCRUpdate(String origLegCallId, CallData callData)
			throws DiameterInvalidArgumentException, DiameterException {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendCCRUpdate..");
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_SEND_CCR_UPDATE);
		action.setProtocol(Protocol.DIAMETER);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		// leg1.set(LegDataAttributes.DIAMETER_CCR_REQUESTED_ACTION,RequestedActionEnum.DIRECT_DEBITING);

		if (callcount == 1) {
			leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
					"remoteRealm");
			callcount = 2;
		} else {
			callcount = 1;
			leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
					"remoteRealm");
		}

		leg1.set(LegDataAttributes.DIAMETER_RO_ROUTE_REMOTE_REALM,
				"serverRealm");

		List<DiameterAVPAttribute> avpAttriList = new ArrayList<DiameterAVPAttribute>();

		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_INPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_CURRENCY_CODE, "2");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_EXPONENT, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_VALUE_DIGITS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_OUTPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_SERVICE_SPECIFIC_UNITS,
				"100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_TOTAL_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_TIME, "100");

		leg1.set(LegDataAttributes.DIAMETER_USU_REPORTING_REASON,
				"QUOTA_EXHAUSTED");
		leg1.set(LegDataAttributes.DIAMETER_USU_TARIFF_CHANGE_USAGE,
				"UNIT_BEFORE_TARIFF_CHANGE");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_TIME, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_INPUT_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_OUTPUT_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_TOTAL_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_SERVICE_SPECIFIC_UNITS,
				"1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_EVENT_CHARGING_TIMESTAMP, ""
				+ System.currentTimeMillis());

		List<String> avpCodes = new ArrayList<String>();
		avpCodes.add(DiameterAVPNames.Requested_Service_Unit);
		avpCodes.add(DiameterAVPNames.Used_Service_Unit);

		DiameterAVPAttribute avp1 = new DiameterAVPAttribute(
				DiameterAVPNames.Mutil_Service_Credit_Control,
				DiameterConstants.VENDOR_NAME_3GPP, DiameterAVPType.Grouped);
		avp1.setValue(avpCodes);

		avpAttriList.add(avp1);
		leg1.set(LegDataAttributes.DIAMETER_REQ_AVP_LIST, avpAttriList);

		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * This method creates and returns ACTION for mPH to send SIP 302 response
	 */
	public Action[] sendCCAUsingAttributes(String origLegCallId,
			CallData callData) throws DiameterInvalidArgumentException,
			DiameterException {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendCCAUsingAttributes..");
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_SEND_CCR_ANSWER);
		action.setProtocol(Protocol.DIAMETER);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		leg1.set(LegDataAttributes.DIAMETER_CCA_OUT_RETURN_CODE,
				DIAMETER_SUCCESS);

		List<DiameterAVPAttribute> avpAttriList = new ArrayList<DiameterAVPAttribute>();

		leg1.set(LegDataAttributes.DIAMETER_COST_INFO_COST_UNIT, "100");
		leg1.set(LegDataAttributes.DIAMETER_COST_INFO_CURRENCY_CODE, "2");
		leg1.set(LegDataAttributes.DIAMETER_COST_INFO_EXPONENT, "10");
		leg1.set(LegDataAttributes.DIAMETER_COST_INFO_VALUE_DIGITS, "100");

		DiameterAVPAttribute avp0 = new DiameterAVPAttribute(
				DiameterAVPNames.Cost_Information,
				DiameterConstants.VENDOR_NAME_BASE, DiameterAVPType.Grouped);
		// avp0.setValue(costInfoAvp);

		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_INPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_CURRENCY_CODE, "2");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_EXPONENT, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_MONEY_VALUE_DIGITS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_OUTPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_SERVICE_SPECIFIC_UNITS,
				"100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_TOTAL_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_RSU_CC_TIME, "233453");

		leg1.set(LegDataAttributes.DIAMETER_GSU_CC_INPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_CURRENCY_CODE, "2");
		leg1.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_EXPONENT, "100");
		leg1.set(LegDataAttributes.DIAMETER_GSU_CC_OUTPUT_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_GSU_CC_MONEY_VALUE_DIGITS, "100");
		leg1.set(LegDataAttributes.DIAMETER_GSU_CC_SERVICE_SPECIFIC_UNITS,
				"100");
		leg1.set(LegDataAttributes.DIAMETER_GSU_CC_TIME, "100");
		leg1.set(LegDataAttributes.DIAMETER_GSU_CC_TOTAL_OCTETS, "100");
		leg1.set(LegDataAttributes.DIAMETER_GSU_TARIFF_TIME_CHANGE,
				"" + System.currentTimeMillis());

		// createUsedServiceUnitAVP("QUOTA_EXHAUSTED","UNIT_BEFORE_TARIFF_CHANGE",(long)
		// 100,bigInt,bigInt,bigInt,bigInt,timestamps);

		leg1.set(LegDataAttributes.DIAMETER_USU_REPORTING_REASON,
				"QUOTA_EXHAUSTED");
		leg1.set(LegDataAttributes.DIAMETER_USU_TARIFF_CHANGE_USAGE,
				"UNIT_BEFORE_TARIFF_CHANGE");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_TIME, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_INPUT_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_OUTPUT_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_TOTAL_OCTETS, "1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_CC_SERVICE_SPECIFIC_UNITS,
				"1000");
		leg1.set(LegDataAttributes.DIAMETER_USU_EVENT_CHARGING_TIMESTAMP, ""
				+ System.currentTimeMillis());

		leg1.set(LegDataAttributes.DIAMETER_FUI_FINAL_UNIT_ACTION, "TERMINATE");
		//
		// List<DiameterAVP>
		// AnnInfo=roMsgfactory.createAnnouncementInformationAVP(1L, vparrray,
		// 1000L, "QUOTA_IS_NOT_USED_DURING_PLAYBACK", 1L, "SERVED_PARTY",
		// "PRIVATE", "English");

		leg1.set(LegDataAttributes.DIAMETER_ANN_IDENTIFIER, "1");
		leg1.set(LegDataAttributes.DIAMETER_ANN_ORDER, "2");
		leg1.set(LegDataAttributes.DIAMETER_ANN_PLANGUAGE, "English");
		//

		List<String> avpcodes = new ArrayList<String>();
		avpcodes.add(DiameterAVPNames.Requested_Service_Unit);
		avpcodes.add(DiameterAVPNames.Granted_Service_Unit);
		avpcodes.add(DiameterAVPNames.Used_Service_Unit);
		avpcodes.add(DiameterAVPNames.Final_Unit_Indication);

		DiameterAVPAttribute avp1 = new DiameterAVPAttribute(
				DiameterAVPNames.Mutil_Service_Credit_Control,
				DiameterConstants.VENDOR_NAME_BASE, DiameterAVPType.Grouped);
		avp1.setValue(avpcodes);

		DiameterAVPAttribute avp2 = new DiameterAVPAttribute(
				DiameterAVPNames.Credit_Control_Failure_Handling,
				DiameterConstants.VENDOR_NAME_BASE, DiameterAVPType.OctetString);
		String abc = "TERMINATE";
		avp2.setValue(abc);

		avpAttriList.add(avp0);
		avpAttriList.add(avp1);
		avpAttriList.add(avp2);

		logger.debug(origLegCallId + " set attribute map.." + avpAttriList);
		leg1.set(LegDataAttributes.DIAMETER_RES_AVP_LIST, avpAttriList);

		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * This method creates and returns ACTION for mPH to send SIP 302 response
	 */
	public Action[] sendCCA(String origLegCallId, CallData callData)
			throws DiameterInvalidArgumentException, DiameterException {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendCCA..");
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_SEND_CCR_ANSWER);
		action.setProtocol(Protocol.DIAMETER);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		leg1.set(LegDataAttributes.DIAMETER_CCA_OUT_RETURN_CODE,
				DIAMETER_SUCCESS);

		List<DiameterAVPAttribute> avpAttriList = new ArrayList<DiameterAVPAttribute>();

		DiameterRoMessageFactory roMsgfactory = PhUtilityServices.getInstance(
				getApplicationName()).getDiameterRoMesageFactory();
		List<DiameterAVP> uvAVP = roMsgfactory.createUnitValueAVP(1000L);
		List<DiameterAVP> ccmAVP = roMsgfactory.createCCMoneyAVP(uvAVP, 1000L);

		// createRequestedServiceUnitAVP(java.lang.Long cCTime,
		// java.util.List<DiameterAVP> cCMoney, java.math.BigInteger
		// cCTotalOctets, java.math.BigInteger cCInputOctets,
		// java.math.BigInteger cCOutputOctets, java.math.BigInteger
		// cCServiceSpecificUnits)

		BigInteger bigInt = new BigInteger("1000");
		List<DiameterAVP> reqSvcUnitAVP = roMsgfactory
				.createRequestedServiceUnitAVP(System.currentTimeMillis(),
						ccmAVP, bigInt, bigInt, bigInt, bigInt);

		// createGrantedServiceUnitAVP(java.lang.String tariffTimeChanges,
		// int CCTime,
		// int valueDigits,
		// int exponent,
		// int currencyCode,
		// int CCTotalOctets,
		// int CCInputOctets,
		// int CCOutputOctets,
		// int CCServiceSpecificUnits)

		List<DiameterAVP> reqGrantedSVCUnitAVP = roMsgfactory
				.createGrantedServiceUnitAVP("" + System.currentTimeMillis(),
						100L, ccmAVP, bigInt, bigInt, bigInt, bigInt);

		// createUsedServiceUnitAVP(java.lang.String reportingReason,
		// java.lang.String tariffChangeUsage, java.lang.Long cCTime,
		// java.math.BigInteger cCTotalOctets,
		// java.math.BigInteger cCInputOctets, java.math.BigInteger
		// cCOutputOctets, java.math.BigInteger cCServiceSpecificUnits,
		// java.lang.String[] eventChargingTimeStamp)
		String[] timestamps = new String[] { "" + System.currentTimeMillis() };
		List<DiameterAVP> usedSVCUnitAVP = roMsgfactory
				.createUsedServiceUnitAVP("QUOTA_EXHAUSTED",
						"UNIT_BEFORE_TARIFF_CHANGE", (long) 100, bigInt,
						bigInt, bigInt, bigInt, timestamps);
		List<DiameterAVP>[] usuarrray = new List[1];
		usuarrray[0] = usedSVCUnitAVP;

		List<DiameterAVP> finalUIUnitAVP = roMsgfactory
				.createFinalUnitIndicationAVP("TERMINATE");

		// Used-Service-Unit

		// <!-- Ref = [base - RFC4006] 8.19 -->
		// <avp name="Used-Service-Unit" code="446" mandatory="must"
		// may-encrypt="yes"
		// vendor-specific="no" >
		// <grouped>
		// <gavp name="Reporting-Reason"/>
		// <gavp name="Tariff-Change-Usage"/>
		// <gavp name="CC-Time"/>
		// <gavp name="CC-Total-Octets"/>
		// <gavp name="CC-Input-Octets"/>
		// <gavp name="CC-Output-Octets"/>
		// <gavp name="CC-Service-Specific-Units"/>
		// <gavp name="Event-Charging-TimeStamp"/>
		// </grouped>
		// </avp>
		List<DiameterAVP> variablePart = roMsgfactory.createVariablePartAVP(2L,
				2L, "play.wav");
		List<DiameterAVP>[] vparrray = new List[1];
		vparrray[0] = variablePart;
		List<DiameterAVP> AnnInfo = roMsgfactory
				.createAnnouncementInformationAVP(1L, vparrray, 1000L,
						"QUOTA_IS_NOT_USED_DURING_PLAYBACK", 1L,
						"SERVED_PARTY", "PRIVATE", "English");

		List<DiameterAVP>[] annarrray = new List[1];
		annarrray[0] = AnnInfo;
		List<DiameterAVP> multiSCCUnitAVP = roMsgfactory
				.createMultipleServicesCreditControlAVP(reqGrantedSVCUnitAVP,
						reqSvcUnitAVP, usuarrray, null, null, null, null, null,
						finalUIUnitAVP, null, null, null, null, null, null,
						null, null, null, null, null, null, null, null, null,
						annarrray, null, null);

		// java.util.List<DiameterAVP>
		// createCostInformationAVP(java.util.List<DiameterAVP> unitValue,
		// java.lang.Long currencyCode,
		// java.lang.String costUnit)
		List<DiameterAVP> costInfoAvp = roMsgfactory.createCostInformationAVP(
				uvAVP, 200L, "USD");

		DiameterAVPAttribute avp0 = new DiameterAVPAttribute(
				DiameterAVPNames.Cost_Information,
				DiameterConstants.VENDOR_NAME_BASE, DiameterAVPType.Grouped);
		avp0.setValue(costInfoAvp);

		//
		DiameterAVPAttribute avp1 = new DiameterAVPAttribute(
				DiameterAVPNames.Mutil_Service_Credit_Control,
				DiameterConstants.VENDOR_NAME_BASE, DiameterAVPType.Grouped);
		avp1.setValue(multiSCCUnitAVP);

		DiameterAVPAttribute avp2 = new DiameterAVPAttribute(
				DiameterAVPNames.Credit_Control_Failure_Handling,
				DiameterConstants.VENDOR_NAME_BASE, DiameterAVPType.OctetString);
		String abc = "TERMINATE";
		avp2.setValue(abc);

		avpAttriList.add(avp0);
		avpAttriList.add(avp1);
		avpAttriList.add(avp2);

		logger.debug(origLegCallId + " set attribute map.." + avpAttriList);
		leg1.set(LegDataAttributes.DIAMETER_RES_AVP_LIST, avpAttriList);

		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * This method creates and returns ACTION for mPH to send SIP 302 response
	 */
	public Action[] sendEnumRequest(String origLegCallId, CallData callData) {
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		logger.debug(origLegCallId + " sendEnumRequest..");
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_SEND_ENUM_QUERY);
		action.setProtocol(Protocol.ENUM);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData leg1 = (LegData) callData.get(CallDataAttribute.P_LEG1);
		// leg1.set(LegDataAttributes.DIAMETER_CCR_REQUESTED_ACTION,RequestedActionEnum.CHECK_BALANCE);

		leg1.set(LegDataAttributes.ENUM_NUMBER_QUERIED, "+91-817-112-0014");
		leg1.set(LegDataAttributes.ENUM_ZONE_QUERIED,
				"cz.TOROONXN08.NGN.uslcr.telus.com.");

		actionArr[0] = action;
		return actionArr;
	}

	/*
	 * When service want to connect orig and term for first time then -- 1.
	 * After receiving initial INVITE mPH will send EVENT_INITIAL to service 2.
	 * Service will return CONNECT_TERM 3. mPH will send initial INVITE to term
	 * leg. After receiving 200 OK from term, mPH will send EVENT_CONNECTED to
	 * service with legId of TERM Leg 4. Then service will return SYNC_WITH_ORIG
	 * ACTION to connect orig and term term leg
	 */
	public Action[] connectTerm(String origLegCallId, CallData callData,
			LegData legData) {
		logger.debug(origLegCallId
				+ " connectTerm() Send initial INVITE to term and connect mPH to term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		String destinationNumber = "123412";

		/*
		 * Create LegData for term leg and set destination details on term
		 * legData
		 */

		// if (MphTestAppSipServlet.getDestinationList() != null) {
		//
		// logger.debug(origLegCallId +
		// " connectTerm() Multiple destinations list is available !! ");
		// if (callData.get(DEST_INDEX) != null) {
		//
		// int lastDestIndex = (Integer) callData
		// .get(DEST_INDEX);
		//
		// logger.debug(origLegCallId +
		// " connectTerm() last destination index was !! "+ lastDestIndex);
		//
		// int currentDestIndex=lastDestIndex++;
		//
		// if (MphTestAppSipServlet.getDestinationList().size() >=
		// (currentDestIndex)) {
		// destinationNumber = MphTestAppSipServlet
		// .getDestinationList().get(currentDestIndex);
		//
		// logger.debug(origLegCallId +
		// " connectTerm()next  destination in the list is  !! "+
		// destinationNumber +" with index" +currentDestIndex);
		// callData.set(DEST_INDEX, currentDestIndex);
		// }else{
		//
		// logger.debug(origLegCallId
		// +
		// " connectTerm() destination list has got exhausted . not destinations available in list for trying more so returning");
		//
		// return actionArr;
		// }
		//
		// } else {
		//
		// logger.debug(origLegCallId +
		// " connectTerm() no destination list available tryiing on default destination number !! "+
		// destinationNumber);
		// destinationNumber = MphTestAppSipServlet.getDestinationList()
		// .get(0);
		// callData.set(DEST_INDEX, 0);
		//
		// }
		//
		// }

		PhoneNumber destPhNumber = new PhoneNumber();
		destPhNumber.setAddress(destinationNumber);
		Action action = new Action(Action.ActionType.ACTION_CONNECT);
		action.setConnectionMode(Action.CONNECTIONMODE.B2BUA);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		LegData termLegData = new LegData();
		termLegData.set(LegDataAttributes.P_DESTINATION_NUMBER, destPhNumber);
		termLegData.set(LegDataAttributes.P_CONNECTION_TYPE,
				ConnectionType.TERM_CONNECTION);
		callData.set(CallDataAttribute.P_LEG2, termLegData);

		actionArr[0] = action;

		/*
		 * Set Terminating IP and port. These will be set in 302 response
		 * contact header
		 */

		PhoneNumber dialledNumber = (PhoneNumber) legData
				.get(LegDataAttributes.P_DIALED_DIGITS);

		logger.debug(origLegCallId + " connectTerm()Number dialled is  !! "
				+ dialledNumber);

		if (dialledNumber.getAddress().equals("334")) {
			termLegData.set(LegDataAttributes.P_REMOTE_IP, "10.32.4.66");
			termLegData.set(LegDataAttributes.P_REMOTE_PORT, 6066);
		} else {

			logger.debug(origLegCallId + " connectTerm()Number dialled is  !! "
					+ dialledNumber);

			termLegData.set(LegDataAttributes.P_REMOTE_IP, "10.32.4.66");
			termLegData.set(LegDataAttributes.P_REMOTE_PORT, 6067);
		}
		return actionArr;
	}

	/**
	 * This method creates action to connect orig to media-server. Orig is
	 * connected in confirmed state or chargeable mode
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	public Action[] connectOrigToIvrInConfirmed(String origLegCallId,
			CallData callData) {
		logger.debug(origLegCallId
				+ " connectIvrInConfirmed() Connect orig to IVR in confirmed mode");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_CONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		origLegData.set(LegDataAttributes.NP_IS_CHARGEABLE_ANN, "TRUE");
		actionArr[0] = action;
		return actionArr;
	}

	/**
	 * This method creates action to connect term to media-server.
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	public Action[] connectTermToIvr(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId + " connectTermToIvr() Connect term to IVR");

		Action[] actionArr = null;
		Action action = new Action(Action.ActionType.ACTION_CONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());

		actionArr[0] = action;
		return actionArr;
	}

	/**
	 * This method creates action to connect orig to media-server. Orig is
	 * connected in early media state
	 * 
	 * @param origLegCallId
	 * @param callData
	 * @return
	 */
	public Action[] connectOrigToIvrInEarlyMedia(String origLegCallId,
			CallData callData) {
		logger.debug(origLegCallId
				+ " connectOrigToIvrInEarlyMedia() Connect orig to IVR in EarlyMedia mode");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_CONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		/*
		 * If service does not set IS_CHARGEABLE_ANN then default is early media
		 * TRUE
		 */
		origLegData.set(LegDataAttributes.NP_IS_CHARGEABLE_ANN, "FALSE");
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] playAnnToOrig(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId + " playAnnToOrig() play ann to orig");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = new Action[2];

		Action action = new Action(Action.ActionType.ACTION_PLAY);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		// Create and set annSpec in LegData
		AnnSpec annSpec = new AnnSpec();
		annSpec.setTerminationKey("Z");
		annSpec.setEsacpeKey("Z");
		annSpec.setAnnIteration(100);
		// annSpec.setAnnLength(3);
		annSpec.setAnnLanguage(AnnSpec.ANN_LANG_EN_US);
		annSpec.addMessage("CDIV/606060.wav", ANN_TYPE.ANN);
		origLegData.set(LegDataAttributes.NP_ANN_SPEC, annSpec);
		actionArr[0] = action;

		actionArr[1] = connectCRE(origLegCallId, callData, "1234");
		return actionArr;
	}

	public Action[] playAnnToTerm(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId + " playAnnToTerm() play ann to term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_PLAY);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		LegData termLegData = (LegData) callData.get(CallDataAttribute.P_LEG2);
		// Create and set annSpec in LegData
		AnnSpec annSpec = new AnnSpec();
		annSpec.setTerminationKey("Z");
		annSpec.setEsacpeKey("Z");
		annSpec.setAnnIteration(5);
		annSpec.setAnnLength(3);
		annSpec.setAnnLanguage(AnnSpec.ANN_LANG_EN_US);
		annSpec.addMessage("MyAnnFolder/myAnn.wav", ANN_TYPE.ANN);

		if (termLegData != null) {
			termLegData.set(LegDataAttributes.NP_ANN_SPEC, annSpec);
		} else {
			logger.debug(origLegCallId
					+ " could not playAnnToTerm() play ann to term termleg data is null");
		}
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] connectOrigAndTerm(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId
				+ " connectTerm() 200 OK received from term. Now connect orig and term");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_RESYNC_CALL);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] connectOrigAndTermAndDisconnectMs(String origLegCallId,
			CallData callData) {
		logger.debug(origLegCallId
				+ " connectOrigAndTermAndDisconnectMs ........");
		/*
		 * Dest Number which will be set in Contact header of 302 response
		 */
		Action[] actionArr = null;
		actionArr = new Action[2];

		Action action = new Action(Action.ActionType.ACTION_RESYNC_CALL);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.toString());
		actionArr[0] = action;

		action = new Action(Action.ActionType.ACTION_DISCONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG1.toString());
		actionArr[1] = action;
		return actionArr;
	}

	public Action[] disconnectOrigIvr(String origLegCallId, CallData callData,
			Protocol protocol) {
		logger.debug(origLegCallId + " disconnectOrigIvr()");

		MphTestAppServiceStateData serviceStateData = (MphTestAppServiceStateData) callData
				.get(CallDataAttribute.SSD_KEY);

		if (serviceStateData != null
				&& !serviceStateData.isIsorigconnectedToIvr()) {
			logger.debug(origLegCallId + " orig ivr not connected ...");
			return null;
		}

		Action[] actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_DISCONNECT_MS);
		action.setProtocol(protocol);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		actionArr[0] = action;
		// Action[] actionArr1 = null;
		// actionArr1 = new Action[2];
		//
		// actionArr1= dropCall(origLegCallId, callData, -1,Protocol.SIP);
		// actionArr[1]=actionArr1[0];
		return actionArr;
	}

	public Action[] disconnectOrigIvrAndStopRBTTimer(String origLegCallId,
			CallData callData, Protocol protocol) {
		logger.debug(origLegCallId + " disconnectOrigIvrAndStopRBTTimer()");

		MphTestAppServiceStateData serviceStateData = (MphTestAppServiceStateData) callData
				.get(CallDataAttribute.SSD_KEY);

		Action[] actionArr = null;

		boolean origIvrconnected = serviceStateData.isIsorigconnectedToIvr();
		boolean rbttimerstarted = serviceStateData.isRBTTimerStarted();

		if (!origIvrconnected && !rbttimerstarted) {
			return null;
		}

		if (origIvrconnected && rbttimerstarted) {
			actionArr = new Action[2];
		}

		if (!rbttimerstarted || !origIvrconnected) {
			actionArr = new Action[1];
		}

		if (serviceStateData != null
				&& serviceStateData.isIsorigconnectedToIvr()) {

			logger.debug(origLegCallId + "Need to disconnect ms");
			Action action = new Action(Action.ActionType.ACTION_DISCONNECT_MS);
			action.setProtocol(protocol);
			action.setLeg(CallDataAttribute.P_LEG1.name());
			actionArr[0] = action;
			origIvrconnected = true;
		}

		Action action = null;
		if (serviceStateData != null && serviceStateData.isRBTTimerStarted()) {

			logger.debug(origLegCallId + "Need to stop RBT timer");
			action = new Action(Action.ActionType.ACTION_STOP_TIMER);
			action.setProtocol(protocol);
			action.setTimerName(RBT_TIMER);
			action.setTimerTimeInMills(RBT_TIME_IN_MILLS);
			action.setLeg(CallDataAttribute.P_LEG1.name());
			if (actionArr[0] == null) {
				actionArr[0] = action;
			} else {
				actionArr[1] = action;
			}
		}

		return actionArr;
	}

	public Action[] startNoSDPTimerForRBT(String origLegCallId,
			CallData callData, Protocol protocol) {
		logger.debug(origLegCallId + " startNoSDPTimerForRBT()");

		MphTestAppServiceStateData serviceStateData = (MphTestAppServiceStateData) callData
				.get(CallDataAttribute.SSD_KEY);
		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_START_TIMER);
		action.setProtocol(protocol);
		action.setTimerName(RBT_TIMER);
		action.setTimerTimeInMills(RBT_TIME_IN_MILLS);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		actionArr[0] = action;

		if (serviceStateData != null) {
			serviceStateData.setRBTTimerStarted(true);
		}
		return actionArr;
	}

	public Action[] stopNoSDPTimerForRBT(String origLegCallId,
			CallData callData, Protocol protocol) {
		logger.debug(origLegCallId + " stopNoSDPTimerForRBT()");

		MphTestAppServiceStateData serviceStateData = (MphTestAppServiceStateData) callData
				.get(CallDataAttribute.SSD_KEY);

		if (serviceStateData != null && !serviceStateData.isRBTTimerStarted()) {

			logger.debug(origLegCallId + " RBT timer not running()");
			return null;
		}

		Action[] actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_STOP_TIMER);
		action.setLeg(CallDataAttribute.P_LEG1.name());
		actionArr[0] = action;

		return actionArr;
	}

	public Action[] disconnectTermIvr(String origLegCallId, CallData callData) {
		logger.debug(origLegCallId + " disconnectTermIvr()");

		Action[] actionArr = null;
		actionArr = new Action[1];

		Action action = new Action(Action.ActionType.ACTION_DISCONNECT_MS);
		action.setProtocol(Protocol.SIP);
		action.setLeg(CallDataAttribute.P_LEG2.name());
		actionArr[0] = action;
		return actionArr;
	}

	public Action[] dropCall(String origLegCallId, CallData callData,
			int causeCode, Protocol protocol) {
		logger.debug(origLegCallId + " dropCall()");

		Action[] actionArr = null;
		actionArr = new Action[1];

		LegData origLegData = (LegData) callData.get(CallDataAttribute.P_LEG1);
		origLegData.set(LegDataAttributes.P_CAUSE_CODE, causeCode);
		Action action = new Action(Action.ActionType.ACTION_END_CALL);
		action.setDropCallMode(Action.DROP_CALL_MODE.RELEASE_CALL);
		action.setProtocol(protocol);
		action.setLeg(null);// No need to set leg in action
		actionArr[0] = action;
		return actionArr;
	}

	@Override
	public String getServletName() {
		logger.debug("Inside getServletName");
		return "mphTestAppSipServlet";
	}

	@Override
	public String getApplicationName() {
		logger.debug("Inside getApplicationName");
		return "testapp_1";
	}

	@Override
	public String[] getServiceCdr(CallData callData) {
		logger.debug("Inside getServiceCdr");
		String transferedCallId = (String) callData
				.get(CallDataAttribute.P_TRANSFERED_CALL_ID);
		String origCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		Object transferedCallInd = callData
				.get(CallDataAttribute.P_TRANSFERED_CALL_IND);

		String[] cdr = new String[1];
		cdr[0] = "MphTestAppCDR : P_ORIG_LEG_CALL_ID: " + origCallId
				+ " P_TRANSFERED_CALL_ID :" + transferedCallId
				+ ", P_TRANSFERED_CALL_IND: " + transferedCallInd;
		return cdr;
	}

	private static void readHlrConfiguration() {
		String testAppProFile = System.getProperty("ase.home")
				+ "/conf/mphtestapp.properties";

		Properties p = new Properties();
		try {
			p.load(new FileInputStream(new File(testAppProFile)));
		} catch (FileNotFoundException e) {
			logger.error(testAppProFile + " Not found");
		} catch (IOException e) {
			logger.error(testAppProFile + " IOException");
		}

		String hlrSsn = "";
		String hlrSpc = "";

		if (p.getProperty(HLR_SPC) != null) {
			hlrSpc = p.getProperty(HLR_SPC);
		}

		if (p.getProperty(HLR_SSN) != null) {
			hlrSsn = p.getProperty(HLR_SSN);
		}

		if (!hlrSpc.isEmpty()) {
			String[] tmp = hlrSpc.split("-");
			SignalingPointCode spc = null;

			if (tmp.length == 3) {
				spc = new SignalingPointCode(Integer.parseInt(tmp[2]),
						Integer.parseInt(tmp[1]), Integer.parseInt(tmp[0]));
			}

			if (spc != null && !hlrSsn.isEmpty()) {
				hlrSccpAddr = new SccpUserAddress(new SubSystemAddress(spc,
						(short) Integer.parseInt(hlrSsn)));
				hlrSccpAddr.setProtocolVariant(1);
				logger.info("HLR Configuration = " + hlrSccpAddr);
				isHlrInteractionReqd = true;
			}
		}

	}

	public static final String DIAMETER_SUCCESS = "DIAMETER_SUCCESS";
	public static final int DIAMETER_AUTHENTICATION_REJECTED = 4001;
	public static final int DIAMETER_END_USER_SERVICE_DENIED = 4010;
	public static final int DIAMETER_CREDIT_CONTROL_NOT_APPLICABLE = 4011;
	public static final int DIAMETER_CREDIT_LIMIT_REACHED = 4012;
	public static final int DIAMETER_UNABLE_TO_COMPLY = 5012;
	public static final int DIAMETER_USER_UNKNOWN = 5030;
	public static final int DIAMETER_RATING_FAILED = 5031;

};
