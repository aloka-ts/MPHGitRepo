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
package com.agnity.ph.smpp;

import javax.servlet.sip.SipApplicationSession;

import org.apache.log4j.Logger;

import com.agnity.mphdata.common.Action;
import com.agnity.mphdata.common.CallData;
import com.agnity.mphdata.common.Event;
import com.agnity.mphdata.common.Event.EventType;
import com.agnity.mphdata.common.LegData;
import com.agnity.mphdata.common.PhoneNumber;
import com.agnity.mphdata.common.Protocol;
import com.agnity.ph.common.PhConstants;
import com.agnity.ph.common.PhUtilityServices;
import com.agnity.ph.common.ProtocolRouter;
import com.agnity.ph.common.ServiceInterface;
import com.agnity.ph.common.enums.CallDataAttribute;
import com.agnity.ph.common.enums.LegDataAttributes;
import com.baypackets.ase.enumclient.EnumException;
import com.baypackets.ase.ra.smpp.Address;
import com.baypackets.ase.ra.smpp.SmppRequest;
import com.baypackets.ase.ra.smpp.SmppResourceFactory;
import com.baypackets.ase.sbb.CDR;
import com.baypackets.ase.sbb.CDRWriteFailedException;

/**
 * This class is helper class for Smpp ProtocolHandler
 * @author reeta
 *
 */
public class SmppProtocolHelper {

	private static Logger logger = Logger.getLogger(SmppProtocolHelper.class);

	/**
	 * This method is used to send Enum query
	 * 
	 * @param appSession
	 * @param callData
	 * @param action
	 * @throws Exception
	 * @throws EnumException
	 */
	public static void sendShortMessage(SipApplicationSession appSession,
			CallData callData, Action action) throws Exception {

		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		if (logger.isDebugEnabled()) {
			logger.debug(origLegCallId + "::sendShortMessage");
		}

		LegData legData = (LegData) callData.get(CallDataAttribute
				.valueOf(action.getLeg()));
		SmppResourceFactory resFactory = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getSmppResFactory();
		ServiceInterface serviceHandler = PhUtilityServices.getInstance(
				(String) callData.get(CallDataAttribute.SERVICE_ID))
				.getServiceHandler();

		try {
			//createAddress(byte ton, byte npi, String Addr)
			
			PhoneNumber srcPh=(PhoneNumber)legData.get(LegDataAttributes.SMPP_SRC_PHONE_NUMBER);
			PhoneNumber destPh=(PhoneNumber)legData.get(LegDataAttributes.SMPP_DEST_PHONE_NUMBER);
			
			String shortMsg=(String)legData.get(LegDataAttributes.SMPP_SHORT_MESSAGE);
			
			Address addrsrc=resFactory.createAddress((byte)srcPh.getNatureOfAddress(), (byte)srcPh.getNumberingPlan(), srcPh.getAddress());
			Address addrdest=resFactory.createAddress((byte)destPh.getNatureOfAddress(), (byte)destPh.getNumberingPlan(), destPh.getAddress());
			SmppRequest req=resFactory.createRequest(appSession, addrsrc, addrdest, shortMsg);
			req.send();

			Event event = new Event(EventType.EVENT_SHORT_MESSGE_SENT,
					Protocol.SMPP, CallDataAttribute.P_LEG1.name());
			try {
				logger.debug("Raising EVENT_SHORT_MESSGE_SENT ");
				ProtocolRouter.getInstance().execute(event, callData, serviceHandler);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} catch (Exception e) {

			logger.error("Smppreceived raising failure event " +e);
			Event event = new Event(EventType.EVENT_SEND_MESSAGE_FAILURE,
					Protocol.SMPP, action.getLeg());

			ProtocolRouter.getInstance().execute(event, callData,
					serviceHandler);		}
	}

	
	/**
	 * This utility method is for writing service cdr with appSession.
	 * 
	 * @param appSession
	 *            represents the instance of SipApplicationSession
	 */
	public static void writeServiceCdr(CallData callData,Action action) {
		// CallData callData = null;
		String origLegCallId = null;
		try {
			// callData = SipProtocolUtil.getCallData(appSession);
			origLegCallId = (String) callData
					.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ ":: Inside writeServiceCdr with appSession");
			}

			CDR cdrRef = (CDR) callData
					.get(CallDataAttribute.P_CDR_REF);
			
			if(action!=null){
				writeServiceCdr(cdrRef, callData ,action.getApplicationName());
			}else{
				writeServiceCdr(cdrRef, callData ,null);
			}
			

			// mark final cdr written in order to avoid writing 2 CDRS
			// however it needs to disabled in case of service chaining 
			// so that next applicaiton can write CDRs
			callData.set(CallDataAttribute.P_FINAL_CDR_WRITTEN,PhConstants.TRUE);

		} catch (Exception ex) {
			logger.error(origLegCallId + ":: Failed to write service cdr.", ex);
		}
	}

	/**
	 * This method writes the SN CDR if applicable
	 * 
	 * @param cdr
	 *            represents an instance of CDR
	 * @param callData
	 *            represents an instance of CallData
	 * @throws CDRWriteFailedException
	 */
	private static void writeServiceCdr(CDR cdr, CallData callData,String appName)
			throws CDRWriteFailedException {
		String origLegCallId = (String) callData
				.get(CallDataAttribute.P_ORIG_LEG_CALL_ID);
		
		String prevSvcId=(String) callData
				.get(CallDataAttribute.PREV_SERVICE_ID);
		if (PhConstants.TRUE.equals(callData
				.get(CallDataAttribute.P_FINAL_CDR_WRITTEN))) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: Final CDR already written. So returning without writing CDR");
			}

			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: writing final CDR");
			}
			
			if(prevSvcId==null){
			return;
			}
		}

		/**
		 * if appname no specified then take current app name
		 */
		if(appName==null){
			appName = (String) callData
					.get(CallDataAttribute.SERVICE_ID);
		}
		ServiceInterface serviceHandler = PhUtilityServices.getInstance(appName)
				.getServiceHandler();
		String[] cdrArr = serviceHandler.getServiceCdr(callData);

		if (cdrArr != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(origLegCallId
						+ " :: Invoking write method on CDR utility");
			}
			try {
				cdr.write(cdrArr);
				//				callData.set(
				//						CallDataAttribute.P_FINAL_CDR_WRITTEN, PhConstants.TRUE);
			} catch (Exception e) {

				if (logger.isDebugEnabled()) {
					logger.debug(origLegCallId + " :: could not write CDR ..."
							+ e.getMessage());
				}

			}
		}

	}

}
