/*******************************************************************************
 *   Copyright (c) 2020 Agnity, Inc. All rights reserved.
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
package com.agnity.ph.Enum;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import javax.servlet.sip.SipApplicationSession;

import org.apache.log4j.Logger;
import org.xbill.DNS.NAPTRRecord;
import org.xbill.DNS.RRset;
import org.xbill.DNS.Record;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.agnity.ph.sip.SipProtocolHelper;
import com.agnity.ph.sip.SipProtocolUtil;
import com.baypackets.ase.enumclient.EnumClient;
import com.baypackets.ase.enumclient.EnumException;
import com.baypackets.ase.ra.enumserver.EnumResourceFactory;
import com.baypackets.ase.ra.enumserver.message.EnumMessage;
import com.baypackets.ase.ra.enumserver.message.EnumRequest;
import com.baypackets.ase.ra.enumserver.message.EnumResponse;
import com.baypackets.ase.resource.ResourceException;
import com.baypackets.ase.sbb.CDR;

/**
 * This class is helper class for Enum ProtocolHandler
 * @author reeta
 *
 */
public class EnumProtocolHelper {

	private static Logger logger = Logger.getLogger(EnumProtocolHelper.class);

	/**
	 * This method is used to send Enum query
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 * @throws EnumException
	 */
	public static void sendEnumQuery(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::sendEnumQuery");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		String enumNumber = (String) legData
				.get(LegDataAttributes.ENUM_NUMBER_QUERIED);
		String zone = (String) legData
				.get(LegDataAttributes.ENUM_ZONE_QUERIED);

		if(enumNumber!=null && !enumNumber.startsWith("+")){
			enumNumber="+"+enumNumber;
		}
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId
					+ "::sendEnumQuery  ENUM_NUMBER_QUERIED " + enumNumber);
		}
		EnumClient enumClient = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getEnumClient();
		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();

		try {
			
			enumClient.resolveAsync(null, enumNumber, zone,new EnumResponseListener(callData));

//			Event event = null;
//			if (dnsList != null && !dnsList.isEmpty()) {
//
//				legData.set(LegDataAttributes.ENUM_RESULT_LIST, dnsList);
//				event = new Event(EventType.EVENT_ENUM_QUERY_SUCCESS,
//						Protocol.ENUM, action.getLeg());
//			} else {
//				event = new Event(EventType.EVENT_ENUM_QUERY_FAILURE,
//						Protocol.ENUM, action.getLeg());
//			}
//			ProtocolRouter.getInstance().execute(event, callData,
//					serviceHandler);
		} catch (EnumException e) {

			logger.error("EnumException received raising failuyre event " +e);
			Event event = new Event(EventType.EVENT_ENUM_QUERY_FAILURE,
					Protocol.ENUM, action.getLeg());

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);		}
	}

	/**
	 * This method is used to send Enum response out
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception 
	 */
	public static void sendResponse(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));

		@SuppressWarnings("unchecked")
		List<NAPTRRecord> result = (List<NAPTRRecord>) legData
				.get(LegDataAttributes.ENUM_RESULT_RECORDS);

		EnumRequest request = (EnumRequest) legData
				.get(LegDataAttributes.ENUM_REQUEST_RECEIVED);

		EnumResourceFactory enumResFactory = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getEnumResFactory();
		EnumResponse response;
		try {
			if (result != null) {
				response = enumResFactory.createResponse(appSession,
						((EnumMessage) request).getMessageId(),
						createNAPTRRRSet(result), request);
			} else {
				response = enumResFactory.createResponse(appSession,
						((EnumMessage) request).getMessageId(), null, request);
			}

			((EnumMessage) response).send();
			
			ServiceInterface serviceHandler = PhUtilityServices.getInstance(
					(String) callData.get(CallDataAttribute.SERVICE_ID))
					.getServiceHandler();
			
			Event event = new Event(EventType.EVENT_ENUM_RESPONSE_SENT,
					Protocol.ENUM, action.getLeg());

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);

		} catch (ResourceException e) {
			logger.error("ResourceException received " +e);
			e.printStackTrace();
			
		} catch (IOException e) {
			logger.error("IOException received " +e);
			
			e.printStackTrace();
		}

	}
	
	
	
	/**
	 * This method performs the call cleanup for intra-network call Based on
	 * different orig and term sip call state, appropriate messages are sent for
	 * cleanup the sip call legs.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 * @param action
	 *            represents the instance of Action
	 */
	public static void dropCall(SipApplicationSession appSession,
			CallData callData, Action action) {
		String origLegCallId = null;

		if (logger.isDebugEnabled()) {
			logger.debug(":: dropcall");
		}
		callData.set(CallDataAttribute.P_CALL_DISCONNECT_TIME, new Date());

		if (!PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId + ":: Write CDR");
			}
			SipProtocolHelper.writeServiceCdr(callData, action);
		} else {
     		if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Donot write CDR it is already written");
			}
		}

		/*
		 * Invalidate appsession at last after wr
		 */

		if (appSession.isValid()) {
			SipProtocolUtil.invalidateAppSession(appSession);
		}
	}
	
	
//	/**
//	 * This utility method is for writing service cdr with appSession.
//	 * 
//	 * @param appSession
//	 *            represents the instance of SipApplicationSession
//	 */
//	public static void writeServiceCdr(CallData callData,Action action) {
//		// CallData callData = null;
//		String origLegCallId = null;
//		try {
//			// callData = SipProtocolUtil.getCallData(appSession);
//			origLegCallId = (String) callData
//					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
//
//			if (logger.isDebugEnabled()) {
//				logger.debug(origLegCallId
//						+ ":: Inside writeServiceCdr with appSession");
//			}
//
//			CDR cdrRef = (CDR) callData
//					.get(CallDataAttribute.P_CDR_REF);
//			writeServiceCdr(cdrRef, callData ,action.getApplicationName());
//
//			// mark final cdr written in order to avoid writing 2 CDRS
//			// however it needs to disabled in case of service chaining 
//			// so that next applicaiton can write CDRs
//			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN,PhConstants.TRUE);
//
//		} catch (Exception ex) {
//			logger.error(origLegCallId + ":: Failed to write service cdr.", ex);
//		}
//	}


	/**
	 * get NAPTR RRSet from NAPTRrecords
	 * @param naptrRecords
	 * @return
	 */
	private static RRset[] createNAPTRRRSet(List<NAPTRRecord> naptrRecords) {

		/* Sample Record as below
		 * Name repl1 = new Name("7.7.7.7.7.0.0.9.5.1.6.e164.arpa.");
		 * Name repl2 = new Name(".");
		 * NAPTRRecord reco1 = new NAPTRRecord(repl1, 1, 12121, 100, 7, "u",
		 * "E2U+sip", "!^.*$!sip:57@aarnet.edu.au!", repl2)
		 */

		if (logger.isDebugEnabled())
			logger.debug(" Entering :createNAPTRRRSet ");

		RRset[] recordSet = new RRset[1];
		RRset rrset = new RRset();

		for (NAPTRRecord record : naptrRecords) {
			rrset.addRR((Record) record);
		}

		recordSet[0] = rrset;

		if (logger.isDebugEnabled())
			logger.debug(" Returning  :createNAPTRRRSet " + recordSet);
		return recordSet;

	}

}
